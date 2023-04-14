module Lista1.IO (main1, main2) where

import System.Environment (getArgs)
import qualified Data.HashMap.Lazy as Map
import Data.Vector (Vector, toList)
-- import Control.Monad (forM)
-- import Data.Maybe (fromMaybe)

import Lista1.Dijkstra (Graph, Arc, Vertex, aStarPath, HeuristicFunction, dijkstraPath)
import Lista1.Connections (readConnectionCsv, Connection(..), timeDifference, readCoordinates)
import Data.Time.Compat (TimeOfDay, parseTimeM, defaultTimeLocale)
import GHC.Float (int2Double)
import Data.List (find, groupBy, intersperse)
import Data.Function (on)
import Utils.TupleOperators ( off )
import Text.Printf (printf)
import System.IO (stderr, hPutStrLn, hPrint)
import System.CPUTime (getCPUTime)
import Data.HashMap.Lazy ((!))

infinity :: Double
infinity = read "Infinity" :: Double

connectionsToGraph :: Vector Connection -> Graph
connectionsToGraph connections = edges where
  edges = Map.fromListWith (++) $ toList $ connectionToKeyValues <$> connections
  connectionToKeyValues connection = (from connection, [connection])

lineChangeCost :: TimeOfDay -> Maybe Arc -> Arc -> Double
lineChangeCost startTime prevArc arc
  | departure arc < arrivalTime = infinity
  | maybe (line arc) line prevArc /= line arc = 1
  | otherwise = 0
  where
      arrivalTime = maybe startTime arrival prevArc

timeCost :: TimeOfDay -> Maybe Arc -> Arc -> Double
timeCost startTime prevArc arc = if departure arc < arrivalTime
  then infinity
  else int2Double $ fromInteger $ departure arc `timeDifference` arrivalTime where
    arrivalTime = maybe startTime arrival prevArc

manhattanHeuristic :: Map.HashMap String (Double, Double) -> HeuristicFunction
manhattanHeuristic coordinateMap station end = station `latDiff` end + station `lonDiff` end where
  coordinates stationName = coordinateMap ! stationName
  latDiff = abs `off` (-) `on` (fst . coordinates)
  lonDiff = abs `off` (-) `on` (snd . coordinates)

scheduleEntry :: [Connection] -> String
scheduleEntry connections =
  printf
    "linia %s\nodjazd: %s o %s\njedź %d przystanków\nprzyjazd: %s o %s"
    (line firstConn)
    (from firstConn)
    (show $ departure firstConn)
    (length connections)
    (to lastConn)
    (show $ arrival lastConn)
  where
    firstConn = head connections
    lastConn = last connections

travelSchedule :: [Connection] -> [String]
travelSchedule connections = intersperse "" $ map scheduleEntry $ groupBy ((==) `on` line) connections

readInputTime :: String -> IO TimeOfDay
readInputTime input = case parseTimeM True defaultTimeLocale "%H:%M:%S" input of
  Just time -> return time
  Nothing -> do
    putStrLn "błąd odczytu godziny, godzina powinna być podana w formacie hh:mm:ss"
    fail "time format error"

-- readStation :: [Vertex] -> String -> IO Station
-- readStation stations input = case find (\station -> name station == input) stations of
--     Just station -> return station
--     Nothing -> do
--       putStrLn $ "nie znaleziono przystanku " ++ input
--       fail "station not found"

main1 :: IO ()
main1 = do
  graph <- connectionsToGraph <$> readConnectionCsv "data/connection_graph.csv"
  stations <- readCoordinates "data/connection_graph.csv"

  print $ seq graph ""
  print $ seq stations ""

  inputFrom : inputTo : mode : restArgs <- getArgs
  costFunction <- case mode of
    "p" -> do 
      time <- readInputTime $ head restArgs
      return $ lineChangeCost time
    "t" -> do
      time <- readInputTime $ head restArgs
      return $ timeCost time
    _ -> do
      putStrLn "możliwe wartości trybu optymalizacji to 'p' dla przesiadek lub 't' dla czasu"
      fail "wrong mode"
  -- fromStation <- readStation stations inputFrom
  -- toStation <- readStation stations inputTo

  startTime <- getCPUTime
  -- print `mapM_` dijkstraPath costFunction graph fromStation toStation
  let result = aStarPath (manhattanHeuristic stations) costFunction graph inputFrom inputTo
  -- let result = dijkstraPath costFunction graph inputFrom inputTo

  print $ seq result ""
  endTime <- getCPUTime
  let timeDiff = fromIntegral (endTime - startTime) :: Double

  print `mapM_` result
  putStrLn ""
  putStrLn `mapM_` travelSchedule result
  putStrLn ""
  hPrint stderr timeDiff


main2 :: IO ()
main2 = do
  print ""