{-# LANGUAGE DeriveGeneric #-}
module Lista1.Connections (readConnectionCsv, Connection (..), Station (..), timeDifference) where

import Data.ByteString (toStrict)
import qualified Data.ByteString.Lazy as BS
import Data.Csv (HasHeader (HasHeader), decode)
import Data.Either (fromRight)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Compat
  ( diffTimeToPicoseconds,
    timeOfDayToTime, TimeOfDay
  )
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Data.Vector (Vector, empty, (!))
import Data.Function (on)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

data Connection = Connection
  { cId :: Integer,
    departure :: TimeOfDay,
    arrival :: TimeOfDay,
    from :: Station,
    to :: Station,
    line :: String
  }
  deriving (Show, Eq, Ord, Generic)
instance Hashable Connection

data Station = Station {
  name :: String,
  coordinates :: (Double, Double)
} deriving (Show, Eq, Ord, Generic)
instance Hashable Station

valuesToConnection :: Vector String -> Connection
valuesToConnection values =
  Connection
    { cId = read $ values ! 0,
      from = Station {
        name = values ! 6,
        coordinates = (read $ values ! 8, read $ values ! 9)
      },
      to = Station {
        name = values ! 7,
        coordinates = (read $ values ! 10, read $ values ! 11)
      },
      line = values ! 3,
      departure = readCsvTime (values ! 4),
      arrival = readCsvTime (values ! 5)
    }

readCsvTime :: String -> TimeOfDay
readCsvTime = parseTimeOrError True defaultTimeLocale "%H:%M:%S"

timeDifference :: TimeOfDay -> TimeOfDay -> Integer
timeDifference = (-) `on` (`div` (10 ^ (12 :: Integer))) . diffTimeToPicoseconds . timeOfDayToTime

readCsvFile :: FilePath -> IO (Vector (Vector BS.ByteString))
readCsvFile path = fromRight empty . decode HasHeader <$> BS.readFile path

readConnectionCsv :: FilePath -> IO (Vector Connection)
readConnectionCsv path = fmap (valuesToConnection . byteVectorToStringVector) <$> readCsvFile path
  where
    byteVectorToStringVector = fmap (unpack . decodeUtf8 . toStrict)