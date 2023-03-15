module Lib
  ( dijkstra,
    Vertex,
    Path,
    Arc,
    Graph,
  )
where

import Data.Function (on)
import Data.HashMap.Lazy qualified as Map
import Data.HashSet qualified as Set
import Data.List (minimumBy)
import GHC.Real (infinity)
import Data.HashMap.Lazy ((!))

type Vertex = String

type Path = [Vertex]

type Arc = (Vertex, Vertex)

type WeightMap = Map.HashMap Vertex Rational

data Graph = Graph (Map.HashMap Vertex Vertex) (Arc -> Rational)

dijkstra :: Graph -> Vertex -> Vertex -> WeightMap
dijkstra (Graph edges arcWeight) start end =
  go (Map.keysSet edges) initWeight
  where
    -- initWeight vertex = if vertex == start then 0 else infinity
    initWeight = Map.singleton start 0
    go :: Set.HashSet Vertex -> WeightMap -> WeightMap
    -- go queue weight
    --   | null queue = weight
    --   | weight end /= infinity = weight
    --   | otherwise =
    --       let u = minimumBy (compare `on` weight) (Set.toList queue)
    --           nextWeight vertex =
    --             if not (Set.member vertex queue)
    --               then weight vertex
    --               else min (weight u + arcWeight (u, vertex)) (weight vertex)
    --        in go (Set.delete u queue) nextWeight
    go queue weightMap
      | null queue = weightMap
      | weightMap ! end /= infinity = weightMap
      | otherwise =
        let
          weight v = Map.findWithDefault infinity v weightMap 
          u = minimumBy (compare `on` weight) (Set.toList queue) 
          nextWeightMap = weightMap
        in go (Set.delete u queue) nextWeightMap