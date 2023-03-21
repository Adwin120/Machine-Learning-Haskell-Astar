module Lib
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
import Data.List (minimumBy, unfoldr)
import GHC.Real (infinity)

type Vertex = String

type Arc = (Vertex, Vertex)

type WeightMap = Map.HashMap Vertex Rational

type PrevMap = Map.HashMap Vertex Vertex

data Graph = Graphh (Map.HashMap Vertex [Vertex]) (Arc -> Rational)

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

        altWeight v = weight u + arcWeight (u, v)
        altWeightMap = Map.fromList $ map (\v -> (v, altWeight v)) $ filter (`Set.member` nextQueue) (edges ! u)

        nextWeightMap = Map.unionWith min weightMap altWeightMap
        nextPrevMap = Map.union (u <$ altWeightMap) prevMap


prevMapToPath :: PrevMap -> Vertex -> [Vertex]
-- prevMapToPath prevMap end = end : (prevMap ! end) : prevMapToPath prevMap (prevMap ! end)
prevMapToPath prevMap end = case prevMap !? end of
    Just previous -> end : prevMapToPath prevMap previous
    Nothing -> [end]

dijkstraPath :: Graph -> Vertex -> Vertex -> [Vertex]
dijkstraPath  graph start end = reverse $ (prevMapToPath $ snd $ dijkstra graph start end) end