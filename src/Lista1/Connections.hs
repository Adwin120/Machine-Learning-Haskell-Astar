{-# LANGUAGE DeriveGeneric #-}
module Lista1.Connections (readConnectionCsv, Connection (..), timeDifference) where

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
    travelTimeSeconds :: Integer,
    from :: String,
    to :: String,
    line :: String
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable Connection

valuesToConnection :: Vector String -> Connection
valuesToConnection values =
  Connection
    { cId = read $ values ! 0,
      from = values ! 6,
      to = values ! 7,
      line = values ! 3,
      departure = readCsvTime (values ! 4),
      arrival = readCsvTime (values ! 5),
      travelTimeSeconds = readCsvTime (values ! 5) `timeDifference` readCsvTime (values ! 4)
    }

readCsvTime :: String -> TimeOfDay
readCsvTime = parseTimeOrError True defaultTimeLocale "%H:%M:%S"

timeDifference :: TimeOfDay -> TimeOfDay -> Integer
timeDifference = (-) `on` (`div` (1 ^ 12)) . diffTimeToPicoseconds . timeOfDayToTime

readCsvFile :: FilePath -> IO (Vector (Vector BS.ByteString))
readCsvFile path = fromRight empty . decode HasHeader <$> BS.readFile path

readConnectionCsv :: FilePath -> IO (Vector Connection)
readConnectionCsv path = fmap (valuesToConnection . byteVectorToStringVector) <$> readCsvFile path
  where
    byteVectorToStringVector = fmap (unpack . decodeUtf8 . toStrict)