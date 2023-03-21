module Lib
  ( dijkstra,
    prevMapToPath,
    dijkstraPath,
    Vertex,
    Arc,
    Graph,
  )
where

import Data.Function (on)
import Data.HashMap.Lazy ((!), (!?))
import Data.HashMap.Lazy qualified as Map
import Data.HashSet qualified as Set
import Data.List (minimumBy, unfoldr)
import GHC.Real (infinity)

type Vertex = String

type Arc = (Vertex, Vertex)

type WeightMap = Map.HashMap Vertex Rational

type PrevMap = Map.HashMap Vertex Vertex

data Graph = Graph (Map.HashMap Vertex [Vertex]) (Arc -> Rational)

dijkstra :: Graph -> Vertex -> Vertex -> (WeightMap, PrevMap)
dijkstra (Graph edges arcWeight) start end =
  go (Map.keysSet edges) (initWeight, initPrev)
  where
    initWeight = Map.singleton start 0
    initPrev = Map.empty
    go :: Set.HashSet Vertex -> (WeightMap, PrevMap) -> (WeightMap, PrevMap)
    go queue maps@(weightMap, prevMap)
      | null queue = maps
      | Map.member end weightMap = maps
      | otherwise = go (Set.delete u queue) (nextWeightMap, nextPrevMap)
      where
        weight v = Map.findWithDefault infinity v weightMap
        u = minimumBy (compare `on` weight) (Set.toList queue)

        altWeight v = weight u + arcWeight (u, v)
        altWeightMap = Map.fromList $ map (\v -> (v, altWeight v)) $ filter (`Set.member` queue) (edges ! u)

        nextWeightMap = Map.unionWith min weightMap altWeightMap
        nextPrevMap = Map.union (u <$ altWeightMap) prevMap

reflexivePair :: b -> (b, b)
reflexivePair x = (x,x)

prevMapToPath :: PrevMap -> Vertex -> [Vertex]
prevMapToPath prevMap = unfoldr (fmap reflexivePair . (prevMap !?))

dijkstraPath :: Graph -> Vertex -> Vertex -> [Vertex]
dijkstraPath  graph start end = (prevMapToPath $ snd $ dijkstra graph start end) end