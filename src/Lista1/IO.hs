module Lista1.IO (main) where

import System.Environment (getArgs)
import qualified Data.HashMap.Lazy as Map
import Data.Vector (Vector, toList)
import Control.Monad (forM)
import Data.Maybe (fromMaybe)

import Lista1.Dijkstra (Graph (Graphh), dijkstra, dijkstraPath, PrevMap, Arc)
import Lista1.Connections (readConnectionCsv, Connection(..), timeDifference)
import Data.HashMap.Lazy ((!?))
import Data.Time.Compat (TimeOfDay, midday)
import GHC.Float (int2Double)

infinity :: Double
infinity = read "Infinity"::Double

connectionsToGraph :: Vector Connection -> Graph
connectionsToGraph connections = Graphh edges where
  edges = Map.fromListWith (++) $ toList $ connectionToKeyValues <$> connections
  connectionToKeyValues connection = (from connection, [connection])

lineChangeCost :: Maybe Arc -> Arc -> Double
lineChangeCost prevArc arc = if maybe (line arc) line prevArc /= line arc then 1 else 0

timeCost :: TimeOfDay -> PrevMap -> Arc -> Double
timeCost startTime prevMap arc = if departure arc < arrivalTime 
  then infinity 
  else int2Double $ fromInteger $ departure arc `timeDifference` arrivalTime where
    arrivalTime = maybe startTime arrival (prevMap !? from arc)


main :: IO ()
main = do
  csvData <- readConnectionCsv "data/connection_graph.csv"
  [from, to, mode, startTime] <- getArgs
  -- let edgesMap = (\connection -> (from connection, to connection)) <$> csvData
  -- print `mapM_` Map.toList (snd (dijkstra (connectionsToGraph csvData) from to))
  print `mapM_` dijkstraPath lineChangeCost (connectionsToGraph csvData) from to
  
  -- print $ connectionsTo csvData to
  -- print `mapM_` dijkstraPath (connectionsToGraph csvData) from to
  -- let result = connectionDijkstraPath csvData connectionLineChangeCost from to
  -- print `mapM_` result

