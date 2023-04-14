module Lista1.Graph (
    Vertex,
    Arc,
    WeightMap,
    PrevMap,
    Graph,
    infinity,
    CostFunction
) where

import qualified Data.HashMap.Lazy as Map
import Lista1.Connections ( Connection )

type Vertex = String

type Arc = Connection

type WeightMap = Map.HashMap Vertex Double

type PrevMap = Map.HashMap Vertex Arc

type CostFunction = (Maybe Arc -> Arc -> Double)

type Graph = Map.HashMap Vertex [Arc]

infinity :: Double
infinity = read "Infinity"::Double