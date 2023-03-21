module Connections (readConnectionCsv, Connection (..)) where

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
import Data.Vector (Vector, empty, (!))

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