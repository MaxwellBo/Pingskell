{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}

import Web.Scotty as S 
import Database.Redis as R

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Control.Monad.Trans
import Control.Applicative

import Data.Function ((&))
import Data.Ix
import Data.Either

import Data.Time.Format
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Text.Read

import qualified Data.Map.Lazy as Map

{-- DEFINITIONS --}

type EpochTime = Integer

{-- CONVERSIONS --}

byteStringToText :: BC.ByteString -> TL.Text
byteStringToText = TLE.decodeUtf8 . BL.fromStrict

textToByteString :: TL.Text -> BC.ByteString
textToByteString = BL.toStrict . TLE.encodeUtf8

byteStringToString :: BC.ByteString -> String
byteStringToString = TL.unpack . byteStringToText

{-- TIME --}

--| Given a string possibly containing a ISO8601 date ("2016-12-10"), attempts
--| to convert that date into an Integer Unix timestamp
parseISO8601 :: TL.Text -> Maybe EpochTime
parseISO8601 string = (posixToEpoch . utcTimeToPOSIXSeconds) <$> utcTime
  where
    utcTime = parseTimeM False defaultTimeLocale formatString (TL.unpack string)
    formatString = (iso8601DateFormat Nothing)
    posixToEpoch = truncate . toRational

--| Given a string possibly containing a Unix timestamp ("1481287522"), attempts
--| to convert that date into an Integer Unix timestamp
parseEpochTime :: TL.Text -> Maybe EpochTime
parseEpochTime = readMaybe . TL.unpack

parseTime' :: TL.Text -> Maybe EpochTime
-- Pass the TL.Text argument to both functions, and return the first Just
parseTime' = (<|>) <$> parseISO8601 <*> parseEpochTime

{-- DB --}

--| Given a device ID, and a ping, produces a computation that 
--| "adds the value at the tail of the list stored at [the] key. 
--| If [the] key does not exist, [an] empty list [is created] before appending."
postPing :: TL.Text -> TL.Text -> Redis (Either Reply Integer)
postPing deviceID epochTime = do
  R.rpush (textToByteString deviceID) [(textToByteString epochTime)]

--| A computation that retrives all key-value pairs from a database,
--| and converts their types as neccessary
getKeyValuePairs :: Redis (Either Reply [(TL.Text, [EpochTime])])
getKeyValuePairs = do
  (Right keys) <- (R.keys "*")

  let getValue = \key -> R.lrange key 0 (-1) -- ran in the Redis context

  values <- rights <$> (mapM getValue keys)

  let devices = fmap byteStringToText keys
  let pings = (fmap (read . byteStringToString)) <$> values

  return $ return $ Prelude.zip devices pings

--| Given a connection, and a strategy that filters a list of pings into a
--| bounded time window, retrieves a mapping of devices and their respective
--| pings from the database.
getMap :: Connection 
       -> ([EpochTime] -> [EpochTime]) 
       -> IO (Map.Map TL.Text [EpochTime])
getMap conn sliceFunc = do 
  (Right pairs) <- liftIO $ (runRedis conn getKeyValuePairs)

  let pairs' = [ (device, sliceFunc pings) 
               | (device, pings) <- pairs
               , sliceFunc pings /= []
               ]

  return $ Map.fromList pairs'

{-- BUSINESS LOGIC --}

--| Given a possible start Unix timestamp, filters the list of pings so that
--| only pings up to a day later remain.
takeDaySlice :: Maybe EpochTime -> [EpochTime] -> [EpochTime]
takeDaySlice from = takeRangeSlice from to
  where
    to = (+86400) <$> from -- a day later

--| Given a possible start and end Unix timestamp, filters the list of pings so
--| that only pings, including the start timestamp, up until, but not including,
--| the end timestamp remain.
takeRangeSlice :: Maybe EpochTime -> Maybe EpochTime -> [EpochTime] -> [EpochTime]
takeRangeSlice (Just from) (Just to) pings = Prelude.filter predicate pings
  where
    predicate = inRange (from, to - 1) 
takeRangeSlice _ _ _ = []

{-- ENTRY POINT --}

main :: IO ()
main = do 
  conn <- connect defaultConnectInfo

  scotty 3000 $ do

    S.post "/:device_id/:epoch_time" $ do
      deviceID <- param "device_id"
      epochTime <- param "epoch_time"

      liftIO $ (runRedis conn (postPing deviceID epochTime))

      text $ ""

    S.get "/all/:date" $ do
      date <- param "date"

      let sliceFunc = (takeDaySlice (parseISO8601 date))
      map <- liftIO $ getMap conn sliceFunc

      json $ map

    S.get "/all/:from/:to" $ do
      from <- param "from"
      to <- param "to"

      let sliceFunc = takeRangeSlice (parseTime' from) (parseTime' to)
      map <- liftIO $ getMap conn sliceFunc

      json $ map


    S.get "/:deviceID/:date" $ do
      deviceID <- param "deviceID"
      date <- param "date"

      let sliceFunc = (takeDaySlice (parseISO8601 date))
      map <- liftIO $ getMap conn sliceFunc

      json $ (Map.findWithDefault [] deviceID map)


    S.get "/:deviceID/:from/:to" $ do
      deviceID <- param "deviceID"
      from <- param "from"
      to <- param "to"

      let sliceFunc = takeRangeSlice (parseTime' from) (parseTime' to)
      map <- liftIO $ getMap conn sliceFunc

      json $ (Map.findWithDefault [] deviceID map)


    S.get "/devices" $ do
      let getDevices = (fmap . fmap . fmap) byteStringToText (R.keys "*")
      (Right devices) <- liftIO $ (runRedis conn getDevices)
      json $ devices


    S.post "/clear_data" $ do
      liftIO $ runRedis conn R.flushall

      text $ ""
