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

{-- UTILS --}

byteStringToText :: BC.ByteString -> TL.Text
byteStringToText = TLE.decodeUtf8 . BL.fromStrict

textToByteString :: TL.Text -> BC.ByteString
textToByteString = BL.toStrict . TLE.encodeUtf8

byteStringToString :: BC.ByteString -> String
byteStringToString = TL.unpack . byteStringToText

parseISO8601 :: TL.Text -> Maybe EpochTime
parseISO8601 string = (posixToEpoch . utcTimeToPOSIXSeconds) <$> utcTime
  where
    utcTime = parseTimeM False defaultTimeLocale formatString (TL.unpack string)
    formatString = (iso8601DateFormat Nothing)
    posixToEpoch = truncate . toRational

parseEpochTime :: TL.Text -> Maybe EpochTime
parseEpochTime string = readMaybe . TL.unpack $ string

parseTime' :: TL.Text -> Maybe EpochTime
-- Pass the TL.Text argument to both functions, and take the first Just argument
-- if possible
parseTime' = (<|>) <$> parseISO8601 <*> parseEpochTime

{-- DB --}

session :: Redis (Either Reply (Maybe BC.ByteString))
session = do
  R.set "hello" "haskell"
  R.get "hello"

postPing :: TL.Text -> TL.Text -> Redis (Either Reply Integer)
postPing deviceID epochTime = do
  R.rpush (textToByteString deviceID) [(textToByteString epochTime)]

getDevices :: Redis (Either Reply ([TL.Text]))
getDevices = fmap (fmap (fmap byteStringToText)) (R.keys "*")

getValue :: BC.ByteString -> Redis (Either Reply [BC.ByteString])
getValue key = R.lrange key 0 (-1)

getKeyValuePairs :: Redis (Either Reply [(TL.Text, [EpochTime])])
getKeyValuePairs = do
  (Right keys) <- (R.keys "*")
  values <- (mapM getValue keys)
  let devices = fmap byteStringToText keys
  let pings = (fmap (read . byteStringToString)) <$> rights values
  return $ return $ Prelude.zip devices pings

{-- BUSINESS LOGIC --}

takeDaySlice :: Maybe EpochTime -> Maybe [EpochTime] -> [EpochTime]
takeDaySlice from = takeRangeSlice from to
  where
    to = (+86400) <$> from -- a day later

takeRangeSlice :: Maybe EpochTime -> Maybe EpochTime -> Maybe [EpochTime] -> [EpochTime]
takeRangeSlice (Just from) (Just to) (Just pings) = Prelude.filter predicate pings
  where
    predicate = inRange (from, to - 1) 
takeRangeSlice _ _ _ = []

main :: IO ()
main = do 
  conn <- connect defaultConnectInfo

  scotty 3000 $ do

    S.post "/:device_id/:epoch_time" $ do
      deviceID <- param "device_id"
      epochTime <- param "epoch_time"

      liftIO $ (runRedis conn (postPing deviceID epochTime))

      text $ ""

    S.get "/:deviceID/:date" $ do
      deviceID <- param "deviceID"
      date <- param "date"

      (Right pairs) <- liftIO $ (runRedis conn getKeyValuePairs)
      let pings = Map.lookup deviceID (Map.fromList pairs)

      json $ takeDaySlice (parseISO8601 date) pings

    S.get "/:deviceID/:from/:to" $ do
      deviceID <- param "deviceID"
      from <- param "from"
      to <- param "to"

      (Right pairs) <- liftIO $ (runRedis conn getKeyValuePairs)
      let pings = Map.lookup deviceID (Map.fromList pairs)

      json $ takeRangeSlice (parseTime' from) (parseTime' to) pings

    S.get "/random_prefix/:date" $ do
      (date :: TL.Text) <- param "date"

      (Right pairs) <- liftIO $ (runRedis conn getKeyValuePairs)

      let pairs' = [ (device, takeDaySlice (parseISO8601 date) (Just pings)) 
                   | (device, pings) <- pairs
                   , takeDaySlice (parseISO8601 date) (Just pings) /= []
                   ]

      json $ Map.fromList pairs 

    -- S.get "/all/:from/:to" $ do
    --   from <- param "from"
    --   to <- param "to"

    S.get "/devices" $ do
      (Right devices) <- liftIO $ (runRedis conn getDevices)
      json $ devices

    S.post "/clear_data" $ do
      liftIO $ runRedis conn R.flushall

      text $ ""


