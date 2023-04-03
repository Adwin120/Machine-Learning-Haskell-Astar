module Lista1.Dijkstra
  ( dijkstra,
    prevMapToPath,
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
import Data.List (minimumBy)

import Lista1.Connections ( Connection (to, from) )
import Lista1.Graph
    ( Graph(..), PrevMap, WeightMap, Arc, Vertex, infinity )

import Utils.TupleOperators ( (<:-:>), (<:-), (-:>) )
import Data.Maybe (fromJust)

identity :: a -> a
identity = id



dijkstra :: Graph -> Vertex -> Vertex -> (WeightMap, PrevMap)
dijkstra (Graphh edges travelCost) start end =
  go initQueue (initWeight, initPrev)
  where
    initQueue = Heap.singleton (0, start)
    initWeight = Map.singleton start 0
    initPrev = Map.empty
    go :: Heap.MinPrioHeap Double Vertex -> (WeightMap, PrevMap) -> (WeightMap, PrevMap)
    go queue maps@(weightMap, prevMap)
      | null queue = maps
      | Map.member end weightMap = maps
      | otherwise = go nextQueue (nextWeightMap, nextPrevMap)
      where
        weight v = Map.findWithDefault infinity v weightMap
        ((_, u), tailQueue) = fromJust $ Heap.view queue

        currentConnections = filter (not . (`Map.member` weightMap) . to) (edges ! u)

        altWeightMap = Map.fromList $ map (to <:-:> altWeight) currentConnections where
          altWeight arc = weight u + travelCost (prevMap !? from arc) arc
        altPrevMap = Map.fromList $ map (to <:-) currentConnections

        nextWeightMap = Map.unionWith min weightMap altWeightMap
        nextPrevMap = Map.union altPrevMap prevMap

        nextQueue = tailQueue `Heap.union` Heap.fromList (map (((nextWeightMap !) <:-) . to) currentConnections)


prevMapToPath :: PrevMap -> Vertex -> [Arc]
prevMapToPath prevMap end = case prevMap !? end of
    Just previous -> previous : prevMapToPath prevMap (from previous)
    Nothing -> []

dijkstraPath :: Graph -> Vertex -> Vertex -> [Arc]
dijkstraPath  graph start end = reverse $ (prevMapToPath $ snd $ dijkstra graph start end) end