{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}

import Web.Scotty as S 
import Database.Redis as R

import qualified Data.Text.Lazy as TL

import Control.Monad.Trans
import Control.Applicative

import Data.Function ((&))
import Data.Ix
import Data.Either

import Data.Time.Format
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Text.Read
import Data.String.Conversions (cs)

import qualified Data.Map.Lazy as Map

{-- DEFINITIONS --}

type EpochTime = Integer

{-- TIME --}

-- | Given a string possibly containing a ISO8601 date ("2016-12-10"), attempts
-- | to convert that date to an Integer Unix timestamp.
parseISO8601 :: TL.Text -> Maybe EpochTime
parseISO8601 string = (posixToEpoch . utcTimeToPOSIXSeconds) <$> utcTime
  where
    utcTime = parseTimeM False defaultTimeLocale formatString (TL.unpack string)
    formatString = (iso8601DateFormat Nothing)
    posixToEpoch = truncate . toRational

-- | Given a string possibly containing a Unix timestamp ("1481287522"), attempts
-- | to convert that date to an Integer Unix timestamp.
parseEpochTime :: TL.Text -> Maybe EpochTime
parseEpochTime = readMaybe . TL.unpack

-- | Given a string containing either a ISO8601 date or a Unix timestamp,
-- | attempts to convert the string to to an Integer Unix timestamp.
parseTimeStamp :: TL.Text -> Maybe EpochTime
-- Pass the TL.Text argument to both functions, and return the first Just
parseTimeStamp = (<|>) <$> parseISO8601 <*> parseEpochTime

{-- DB --}

-- | Given a device ID, and a ping, produces a computation that 
-- | "adds the value at the tail of the list stored at [the] key. 
-- | If [the] key does not exist, [an] empty list [is created] before appending."
postPing :: TL.Text -> TL.Text -> Redis (Either Reply Integer)
postPing deviceID epochTime = do
  R.rpush (cs deviceID) [(cs epochTime)]

-- | A computation that retrives all key-value pairs from a database,
-- | and converts their types as neccessary
getKeyValuePairs :: Redis (Either Reply [(TL.Text, [EpochTime])])
getKeyValuePairs = do
  (Right keys) <- (R.keys "*")

  let getValue = \key -> R.lrange key 0 (-1) -- ran in the Redis context

  values <- rights <$> (mapM getValue keys)

  let devices = fmap cs keys
  let pings = (fmap (read . cs)) <$> values

  return $ return $ Prelude.zip devices pings

-- | Given a connection, and a strategy that filters a list of pings into a
-- | bounded time window, retrieves a mapping of devices and their respective
-- | pings from the database.
getMap :: Connection 
       -> ([EpochTime] -> [EpochTime]) 
       -> IO (Map.Map TL.Text [EpochTime])
getMap conn sliceFunc = do 
  (Right pairs) <- liftIO $ (runRedis conn getKeyValuePairs)

  let pairs' = [ (device, sliceFunc pings) 
               | (device, pings) <- pairs
               , sliceFunc pings /= [] -- don't present keys with no values
               ]

  return $ Map.fromList pairs'

{-- BUSINESS LOGIC --}

-- | Given a possible start Unix timestamp, filters the list of pings so that
-- | only pings up to a day later remain.
takeDaySlice :: Maybe EpochTime -> [EpochTime] -> [EpochTime]
takeDaySlice from = takeRangeSlice from to
  where
    to = (+86400) <$> from -- a day later

-- | Given a possible start and end Unix timestamp, filters the list of pings so
-- | that only pings, including the start timestamp, up until, but not including,
-- | the end timestamp remain.
takeRangeSlice :: Maybe EpochTime -> Maybe EpochTime -> [EpochTime] -> [EpochTime]
takeRangeSlice (Just from) (Just to) pings = Prelude.filter predicate pings
  where
    predicate = inRange (from, to - 1) 
takeRangeSlice _ _ _ = []

{-- ENTRY POINT --}

-- Author: Max Bo (max@maxbo.me)
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

      let sliceFunc = takeRangeSlice (parseTimeStamp from) (parseTimeStamp to)
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

      let sliceFunc = takeRangeSlice (parseTimeStamp from) (parseTimeStamp to)
      map <- liftIO $ getMap conn sliceFunc

      json $ (Map.findWithDefault [] deviceID map)


    S.get "/devices" $ do
      let getDevices = (fmap . fmap . fmap) cs (R.keys "*")
      (Right devices) <- liftIO $ (runRedis conn getDevices)
      
      json $ (devices :: [TL.Text])


    S.post "/clear_data" $ do
      liftIO $ runRedis conn R.flushall

      text $ ""
