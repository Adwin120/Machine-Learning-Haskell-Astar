module Lista1.IO (main) where

import System.Environment (getArgs)
import qualified Data.HashMap.Lazy as Map
import Data.Vector (Vector, toList)
-- import Control.Monad (forM)
-- import Data.Maybe (fromMaybe)

import Lista1.Dijkstra (Graph, dijkstraPath, Arc, Vertex, aStarPath, HeuristicFunction)
import Lista1.Connections (readConnectionCsv, Connection(..), Station(..), timeDifference)
import Data.Time.Compat (TimeOfDay, parseTimeM, defaultTimeLocale)
import GHC.Float (int2Double)
import Data.List (find)
import Data.Function (on)

infinity :: Double
infinity = read "Infinity"::Double

connectionsToGraph :: Vector Connection -> Graph
connectionsToGraph connections = edges where
  edges = Map.fromListWith (++) $ toList $ connectionToKeyValues <$> connections
  connectionToKeyValues connection = (from connection, [connection])

lineChangeCost :: Maybe Arc -> Arc -> Double
lineChangeCost prevArc arc = if maybe (line arc) line prevArc /= line arc then 1 else 0

timeCost :: TimeOfDay -> Maybe Arc -> Arc -> Double
timeCost startTime prevArc arc = if departure arc < arrivalTime 
  then infinity 
  else int2Double $ fromInteger $ departure arc `timeDifference` arrivalTime where
    arrivalTime = maybe startTime arrival prevArc

readInputTime :: String -> IO TimeOfDay
readInputTime input = case parseTimeM True defaultTimeLocale "%H:%M:%S" input of
  Just time -> return time
  Nothing -> do 
    putStrLn "błąd odczytu godziny, godzina powinna być podana w formacie hh:mm:ss"
    fail "time format error"

readStation :: [Vertex] -> String -> IO Station
readStation stations input = case find (\station -> name station == input) stations of
    Just station -> return station
    Nothing -> do
      putStrLn $ "nie znaleziono przystanku " ++ input 
      fail "station not found"

heuristic :: HeuristicFunction
heuristic station end = abs (latDiff station end) + abs (lonDiff station end) where
  latDiff = (-) `on` (fst . coordinates)
  lonDiff = (-) `on` (snd . coordinates)

main :: IO ()
main = do
  graph <- connectionsToGraph <$> readConnectionCsv "data/connection_graph.csv"
  let stations = Map.keys graph

  inputFrom : inputTo : mode : restArgs <- getArgs
  costFunction <- case mode of
    "p" -> return lineChangeCost
    "t" -> do
      time <- readInputTime $ head restArgs
      return $ timeCost time
    _ -> do 
      putStrLn "możliwe wartości trybu optymalizacji to 'p' dla przesiadek lub 't' dla czasu"
      fail "wrong mode"
  fromStation <- readStation stations inputFrom
  toStation <- readStation stations inputTo
        
  -- print `mapM_` dijkstraPath costFunction graph fromStation toStation
  print `mapM_` aStarPath heuristic costFunction graph fromStation toStation


