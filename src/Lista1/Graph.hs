module Lista1.Graph (
    Vertex,
    Arc,
    WeightMap,
    PrevMap,
    Graph (..),
    infinity
) where

import qualified Data.HashMap.Lazy as Map
import Lista1.Connections ( Connection )

type Vertex = String

type Arc = Connection

type WeightMap = Map.HashMap Vertex Double

type PrevMap = Map.HashMap Vertex Arc

data Graph = Graphh (Map.HashMap Vertex [Arc]) (Maybe Arc -> Arc -> Double)

infinity :: Double
infinity = read "Infinity"::Double