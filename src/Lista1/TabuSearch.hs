module Lista1.TabuSearch () where
-- import Lista1.Graph ( Graph, CostFunction, Arc )
-- import qualified Data.HashSet as Set

-- type Tabu = Set.HashSet (Arc, Arc)

-- tabuSearch :: Int -> ([Arc] -> Double)  -> [Arc] -> Int -> ([Arc], Double)
-- tabuSearch tabuSize cost pathToVisit maxIterations = go maxIterations Set.empty (pathToVisit, cost pathToVisit) where
--     go :: Int -> Tabu -> ([Arc], Double) -> ([Arc], Double)
--     go 0 _ solution = solution
--     go iteration tabu (bestRoute, bestCost) = go (iteration - 1) newTabu (newBestRoute, newBestCost) where
