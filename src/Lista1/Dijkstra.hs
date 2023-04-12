module Lista1.Dijkstra
  ( dijkstra,
    travelMapToPath,
    dijkstraPath,
    Vertex,
    PrevMap,
    Arc,
    Graph (Graphh),
  )
where

import Data.Function (on)
import Data.HashMap.Lazy ((!), (!?))
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set
import qualified Data.Heap as Heap

import Lista1.Connections ( Connection (to, from) )
import Lista1.Graph
    ( Graph(..), PrevMap, WeightMap, Arc, Vertex, infinity, CostFunction )

import Utils.TupleOperators ( (<->) )
import Data.Maybe (fromJust)
import Data.List (minimumBy)

minBy :: Ord b => (a -> b) -> a -> a -> a
minBy f a b = minimumBy (compare `on` f) [a, b]

type HeuristicFunction = Vertex -> Vertex -> Double

aStar :: HeuristicFunction -> CostFunction -> Graph -> Vertex -> Vertex -> (WeightMap, PrevMap)
aStar heuristic travelCost (Graphh edges) start end =
  go initQueue (initWeightMap, initTravelMap)
  where
    initQueue = Heap.singleton (0, start)
    initWeightMap = Map.singleton start 0
    initTravelMap = Map.empty
    go :: Heap.MinPrioHeap Double Vertex -> (WeightMap, PrevMap) -> (WeightMap, PrevMap)
    go queue maps@(weightMap, travelMap)
      | null queue = maps
      | Map.member end weightMap = maps
      | otherwise = go nextQueue (nextWeightMap, nextPrevMap)
      where
        weight station = Map.findWithDefault infinity station weightMap + heuristic station end
        ((_, u), tailQueue) = fromJust $ Heap.view queue
        weightAfterTravelWith connection = weight u + travelCost (travelMap !? from connection) connection

        currentConnections = filter destinationNotVisited (edges ! u) where
          destinationNotVisited = not . (`Map.member` weightMap) . to

        altWeightMap = Map.fromListWith min $ map (to <-> weightAfterTravelWith) currentConnections
        altPrevMap = Map.fromListWith (minBy weightAfterTravelWith) $ map (to <-> id) currentConnections

        nextWeightMap = Map.unionWith min weightMap altWeightMap
        nextPrevMap = Map.unionWith (minBy weightAfterTravelWith) altPrevMap travelMap

        nextQueue = tailQueue `Heap.union` Heap.fromList (map (stationWithPriority . to) currentConnections) where
          stationWithPriority station = (nextWeightMap ! station, station)


travelMapToPath :: PrevMap -> Vertex -> [Arc]
travelMapToPath prevMap end = case prevMap !? end of
    Just previous -> previous : travelMapToPath prevMap (from previous)
    Nothing -> []

aStarPath :: HeuristicFunction -> CostFunction -> Graph -> Vertex -> Vertex -> [Arc]
aStarPath heuristic costF graph start end = reverse $ travelMapToPath travelMap end where
  travelMap = snd $ aStar heuristic costF graph start end

dijkstra :: CostFunction -> Graph -> Vertex -> Vertex -> (WeightMap, PrevMap)
dijkstra = aStar (\_connection -> const 1)

dijkstraPath :: CostFunction -> Graph -> Vertex -> Vertex -> [Arc]
dijkstraPath = aStarPath (\_connection -> const 1)