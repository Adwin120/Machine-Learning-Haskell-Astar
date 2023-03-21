module Dijkstra
  ( dijkstra,
    prevMapToPath,
    dijkstraPath,
    Vertex,
    Arc,
    Graph (Graphh)
  )
where

import Data.Function (on)
import Data.HashMap.Lazy ((!), (!?))
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set
import Data.List (minimumBy)
import GHC.Real (infinity)

import Connections ( Connection (to, from) )

type Vertex = String

type Arc = Connection

type WeightMap = Map.HashMap Vertex Rational

type PrevMap = Map.HashMap Vertex Arc

data Graph = Graphh (Map.HashMap Vertex [Arc]) (Arc -> Rational)

dijkstra :: Graph -> Vertex -> Vertex -> (WeightMap, PrevMap)
dijkstra (Graphh edges arcWeight) start end =
  go (Map.keysSet edges) (initWeight, initPrev)
  where
    initWeight = Map.singleton start 0
    initPrev = Map.empty
    go :: Set.HashSet Vertex -> (WeightMap, PrevMap) -> (WeightMap, PrevMap)
    go queue maps@(weightMap, prevMap)
      | null queue = maps
      | Map.member end weightMap = maps
      | otherwise = go nextQueue (nextWeightMap, nextPrevMap)
      where
        weight v = Map.findWithDefault infinity v weightMap
        u = minimumBy (compare `on` weight) (Set.toList queue)

        nextQueue = Set.delete u queue

        (altWeights, altPrevs) = unzip $ map mapsEntries $ filter destInQueue (edges ! u) where
          mapsEntries arc = ((to arc, altDestWeight arc), (to arc, arc))
          destInQueue connection = to connection `Set.member` nextQueue
          altDestWeight arc = weight u + arcWeight arc

        nextWeightMap = Map.unionWith min weightMap (Map.fromList altWeights)
        nextPrevMap = Map.union (Map.fromList altPrevs) prevMap


prevMapToPath :: PrevMap -> Vertex -> [Vertex]
-- prevMapToPath prevMap end = end : (prevMap ! end) : prevMapToPath prevMap (prevMap ! end)
prevMapToPath prevMap end = case prevMap !? end of
    Just previous -> end : prevMapToPath prevMap (from previous)
    Nothing -> [end]

dijkstraPath :: Graph -> Vertex -> Vertex -> [Vertex]
dijkstraPath  graph start end = reverse $ (prevMapToPath $ snd $ dijkstra graph start end) end