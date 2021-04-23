module Lib
    ( bankSimulation
    ) where

import System.Random.MWC
import Data.List
import System.Random.MWC.Distributions
import Control.Monad
import System.Random.Stateful
import Text.Printf

bankSimulation:: Int -> IO ()
bankSimulation n = do
            gen <- getStdGen
            (arrivals, gen' ) <- calculateArrivals n 0 gen
            (yellowProcessingTimes, gen'') <- yellowProcessingTimeList n gen'
            (redProcessingTimes, gen''') <- redProcessingTimeList n gen''
            (blueProcessingTimes, gen4) <- blueProcessingTimeList n gen'''
            let  
               -- results for a query 
               (waitingTimeListYellow, departureTimeListYellow) = waitingAndDepartureLists  arrivals yellowProcessingTimes 0
               (max, avg) = resultWaitingTimes $ waitingTimeListYellow
               yellowDiff = max-avg

               -- results for b query 
               (waitingTimeListRed, departureTimeListRed) = waitingAndDepartureLists  arrivals redProcessingTimes 0
{-               queueList = findQueue  arrivals departureTimeListRed departureTimeListRed 
               (maxQueue, avgQueue) = resultWaitingTimes $ queueList
-}
               queueList' = findQueue' arrivals departureTimeListRed [] 
               (maxQueue', avgQueue') = resultWaitingTimes $ queueList'

               -- results for c query               
               (maxRed, avgRed) = resultWaitingTimes $ waitingTimeListRed
               redDiff = maxRed - avgRed
               
               (waitingTimeListBlue, departureTimeListBlue) = waitingAndDepartureLists  arrivals blueProcessingTimes 0
               (maxBlue, avgBlue) = resultWaitingTimes $ waitingTimeListBlue
               blueDiff = maxBlue - avgBlue
               
               diffs = [(redDiff, "Red"), (blueDiff, "Blue")]
               (closestValue, best) = foldl (\(diff1,str1 ) (diff2,str2) -> if diff1<diff2 then (diff1,str1) else (diff2,str2)) (yellowDiff,"Yellow") diffs
            printf "Answer to a: \nmax: %g, avg: %g\n"  max avg
            --printf "Answer to b: \nmaxQueue: %g, avgQueue: %g\n"  maxQueue avgQueue
            printf "Efficient Answer' to b: \nmaxQueue': %g, avgQueue': %g\n"  maxQueue' avgQueue'
            printf "Answer to c: \nclosest is: %s, with diff: %g\n"  best closestValue



randomArrivalTime :: RandomGen g => g -> IO (Double, g)
randomArrivalTime gen = do
            let
               (a  , gen') = randomR (0,1) gen 
               res = -100 *  (logBase (exp 1) a)
            --printf "Random arrival:%g \n" res      
            return (res, gen')


calculateArrivals:: (RandomGen g) => Int -> Double -> g-> IO ([Double], g)
calculateArrivals 0 _  gen= return ([], gen)
calculateArrivals n 0 gen = do                      
                     (arrival, gen') <-randomArrivalTime gen
                     (rest, gen'') <- calculateArrivals (n-1) arrival gen'
                    -- printf "will arrive:%g\n" arrival
                     return ( (arrival : rest), gen'')
calculateArrivals n previous gen = do
                            (x, gen') <-randomArrivalTime gen
                            let 
                               arrival = previous +x
                            (rest, gen'') <- calculateArrivals (n-1) arrival gen'
                            --printf "inter arr: %g will arrive:%g\n" x arrival
                            return ( (arrival : rest), gen'')

randomProcessingTime:: RandomGen g => Double -> Double -> g -> IO (Double, g)
randomProcessingTime a b gen = do
            let
               (x  , gen') = randomR (0,1) gen
               res = 200 * (x ** (a-1)) * ((1-x)**(b-1))  
            --print res
            return (res , gen')

yellowProcessingTimeList:: (RandomGen g) => Int -> g-> IO ([Double],g)  
yellowProcessingTimeList 0 gen = return ([], gen)
yellowProcessingTimeList n gen = do
                            (processingTime, gen') <-randomProcessingTime 2 5 gen
                            (rest, gen'') <- yellowProcessingTimeList (n-1) gen'
                            --printf "processingTime: %g\n" processingTime
                            return ( processingTime : rest , gen'')      

redProcessingTimeList:: (RandomGen g) => Int -> g-> IO ([Double],g) 
redProcessingTimeList 0 gen = return ([], gen)
redProcessingTimeList n gen = do
                            (processingTime, gen') <-randomProcessingTime 2 2 gen
                            (rest, gen'') <- redProcessingTimeList (n-1) gen'
                            --printf "processingTime: %g\n" processingTime
                            return ( processingTime : rest , gen'')      

blueProcessingTimeList:: (RandomGen g) => Int -> g-> IO ([Double],g)    
blueProcessingTimeList 0 gen = return ([], gen)
blueProcessingTimeList n gen = do
                            (processingTime, gen') <-randomProcessingTime 5 1 gen
                            (rest, gen'') <- blueProcessingTimeList (n-1) gen'
                            --printf "processingTime: %g\n" processingTime
                            return ( processingTime : rest , gen'')                                                        

resultWaitingTimes:: [Double] -> (Double, Double)
resultWaitingTimes l =  (maximum l , average l)


average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

customerWaitingTime:: Double ->  Double -> Double
customerWaitingTime ownArrival previousDeparture = if (ownArrival > previousDeparture) 
                                                        then  0
                                                        else  previousDeparture - ownArrival
                                                          
waitingAndDepartureLists:: [Double] ->  [Double] ->  Double -> ([Double],[Double])
waitingAndDepartureLists [] _ _ = ([],[])
waitingAndDepartureLists (a:arrivals) (p:processingTimes) previousDeparture  = 
    let 
       waitingTime = customerWaitingTime a previousDeparture
       departureTime = waitingTime + a + p
       (waitingList, departList) = waitingAndDepartureLists arrivals processingTimes departureTime
    in   
    (waitingTime : waitingList , departureTime:departList) 


findLasts:: Double -> [Double]
findLasts 0 = []
findLasts acc = acc : (findLasts $ acc-1) 


{- n^2 complexity
findQueue:: [Double] -> [Double] -> [Double] -> [Double]
findQueue [] _ _ = []
findQueue (a:arrivals) (d:departureTimes) departures =  
    (fromIntegral (length (filter (\x -> x < d && x > a ) departures))) : findQueue arrivals departureTimes departures 
-}

-- Efficient: ~ n complexity
findQueue':: [Double] -> [Double] -> [Double] -> [Double]
findQueue' [] _ _ = []
findQueue' (a:arrivals) (d:departureTimes) possibleQueue =  
    (fromIntegral (length customersInFrontOfMe)) : findQueue' arrivals departureTimes (d:customersInFrontOfMe)
    where    
        customersInFrontOfMe = (filter (\x -> x > a ) possibleQueue)