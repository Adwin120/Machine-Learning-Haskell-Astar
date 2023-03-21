module Main (main) where

import System.Environment (getArgs)
import qualified Data.HashMap.Lazy as Map
import Data.Vector (Vector, toList)
import Control.Monad (forM)

import Dijkstra (Graph (Graphh), dijkstra, dijkstraPath)
import Connections (readConnectionCsv, Connection(..))

connectionsToGraph :: Vector Connection -> Graph
connectionsToGraph connections = Graphh edges (const 1) where
  edges = Map.fromListWith (++) $ toList $ connectionToKeyValues <$> connections
  connectionToKeyValues connection = (from connection, [connection])

main :: IO ()
main = do
  csvData <- readConnectionCsv "data/connection_graph.csv"
  [from, to, mode, startTime] <- getArgs
  -- let edgesMap = (\connection -> (from connection, to connection)) <$> csvData
  -- print `mapM_` Map.toList (snd (dijkstra (connectionsToGraph csvData) from to))
  print $ dijkstraPath (connectionsToGraph csvData) from to

