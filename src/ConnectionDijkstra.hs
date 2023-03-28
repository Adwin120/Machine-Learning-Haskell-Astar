module ConnectionDijkstra (connectionDijkstraPath, connectionDijkstra, connectionsTo) where
import Connections (Connection (..), timeDifference)
import Data.HashMap.Lazy ((!), (!?))
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set
import qualified Data.PSQueue as Queue
import Data.List (minimumBy, delete)
import Data.Function (on)
import Data.Vector (Vector, toList)

import Debug.Trace (traceShowId, traceShow)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.PSQueue (Binding((:->)))

infinity :: Double
infinity = read "Infinity" :: Double

hour :: Integer
hour = 36 * (10 ^ 14)

type Station = String
type WeightMap = Map.HashMap Connection Double
type TravelMap = Map.HashMap Connection Connection
type CostFunction = Maybe Connection -> Connection -> Double

data ConnectionGraph = Graph (Map.HashMap Station [Connection]) CostFunction

connectionGraph :: Vector Connection -> CostFunction -> ConnectionGraph
connectionGraph connections = Graph edges where
    edges = Map.fromListWith (++) $ toList $ connectionToKeyValues <$> connections
    connectionToKeyValues connection = (from connection, [connection])

connectionsTo :: Vector Connection -> String -> Set.HashSet Connection
connectionsTo connections station = Set.fromList $ filter (\conn -> to conn == station) $ toList connections

associate :: (t -> b) -> t -> (t, b)
associate f x = (x, f x)

connectionDijkstra :: ConnectionGraph -> Station -> Set.HashSet Connection -> (WeightMap, TravelMap)
connectionDijkstra (Graph connectionsFrom travelCost) start toEnd =
    go Set.empty initQueue (initWeight, initTravel)
    where
        startingConnections = connectionsFrom ! start
        initWeight = Map.fromList $ map (associate $ travelCost Nothing) startingConnections
        initTravel = Map.empty
        initQueue = Queue.fromList $ map (\conn -> conn :-> (initWeight ! conn)) startingConnections

        go :: Set.HashSet Connection -> Queue.PSQ Connection Double -> (WeightMap, TravelMap) -> (WeightMap, TravelMap)
        go visited queue maps@(weightMap, travelMap)
            | Queue.null queue = maps
            | not $ null $ Set.intersection visited toEnd = maps
            | otherwise = go nextVisited nextQueue (nextWeightMap, nextTravelMap)
            where
                weight conn = Map.findWithDefault infinity conn weightMap
                (u :-> _, queueNoU) = fromJust $ Queue.minView queue

                nextVisited = Set.insert u visited
                notVisited conn = not $ Set.member conn nextVisited

                currConnections = filter (\x -> notVisited x &&  (departure x `timeDifference` arrival u < hour ) && departure x > arrival u) (connectionsFrom ! to u)


                altWeightMap = Map.fromList $ map (associate altWeight) currConnections
                    where altWeight conn = weight u + travelCost (Just u) conn

                nextQueue = foldr (\conn -> Queue.insert conn (weight conn)) queueNoU currConnections


                nextWeightMap = Map.unionWith min weightMap altWeightMap
                nextTravelMap = Map.union (u <$ altWeightMap) travelMap


travelMapToPath :: Map.HashMap Connection Connection -> Set.HashSet Connection -> [[Connection]]
travelMapToPath travelMap toEnd = go <$> (take 1 . Set.toList) (Map.keysSet travelMap `Set.intersection` toEnd) where
    go :: Connection -> [Connection]
    go end = case travelMap !? end of
        Just previous -> previous : go previous
        Nothing -> []




-- connectionDijkstraPath :: Vector Connection -> CostFunction -> Station -> Station -> [[Connection]]
-- connectionDijkstraPath connections costF start end = reverse $ (travelMapToPath $ snd $ connectionDijkstra (connectionGraph connections costF) start toEnd) toEnd where
--     toEnd = connectionsTo connections end


connectionDijkstraPath connections costF start end = Map.toList $ snd $ connectionDijkstra (connectionGraph connections costF) start toEnd where
    toEnd = connectionsTo connections end