module Main (main) where

import System.Environment (getArgs)
import qualified Data.HashMap.Lazy as Map
import Data.ByteString (toStrict)
import qualified Data.ByteString.Lazy as BS
import Data.Csv (HasHeader (HasHeader), decode)
import Data.Either (fromRight)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time
  ( diffTimeToPicoseconds,
    timeOfDayToTime,
  )
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Data.Vector (Vector, empty, (!), toList)
import Lib (Graph (Graphh), dijkstra, dijkstraPath)
import Control.Monad (forM)
import qualified GHC.Arr as Map

data Connection = Connection
  { cId :: Integer,
    travelTimeSeconds :: Integer,
    from :: String,
    to :: String,
    line :: String
  }
  deriving (Show)

valuesToConnection :: Vector String -> Connection
valuesToConnection values =
  Connection
    { cId = read $ values ! 0,
      from = values ! 6,
      to = values ! 7,
      line = values ! 3,
      travelTimeSeconds = readCsvTime (values ! 5) - readCsvTime (values ! 4)
    }

readCsvTime :: String -> Integer
readCsvTime =
  diffTimeToPicoseconds
    . timeOfDayToTime
    . parseTimeOrError True defaultTimeLocale "%H:%M:%S"

readCsvFile :: FilePath -> IO (Vector (Vector BS.ByteString))
readCsvFile path = fromRight empty . decode HasHeader <$> BS.readFile path

readConnectionCsv :: FilePath -> IO (Vector Connection)
readConnectionCsv path = fmap (valuesToConnection . byteVectorToStringVector) <$> readCsvFile path
  where
    byteVectorToStringVector = fmap (unpack . decodeUtf8 . toStrict)

connectionsToGraph :: Vector Connection -> Graph
connectionsToGraph connections = Graphh edges (const 1) where
  edges = Map.fromListWith (++) $ toList $ connectionToKeyValues <$> connections
  connectionToKeyValues connection = (from connection, [to connection])

main :: IO ()
main = do
  csvData <- readConnectionCsv "data/connection_graph.csv"
  [from, to, mode, startTime] <- getArgs
  -- let edgesMap = (\connection -> (from connection, to connection)) <$> csvData
  -- print `mapM_` Map.toList (snd (dijkstra (connectionsToGraph csvData) from to))
  print $ dijkstraPath (connectionsToGraph csvData) from to

