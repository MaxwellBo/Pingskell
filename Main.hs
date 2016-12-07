{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}

import Web.Scotty as S 
import Database.Redis as R

import Control.Monad.Trans
import Control.Applicative
import Data.Function ((&))

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.ByteString.Char8 as BE

import Data.Time.Format
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Text.Read
import Data.Ix

{-- UTILS --}

byteStringToText :: ByteString -> TL.Text
byteStringToText = TLE.decodeUtf8 . BL.fromStrict

textToByteString :: TL.Text -> ByteString
textToByteString = BL.toStrict . TLE.encodeUtf8

parseISO8601 :: TL.Text -> Maybe POSIXTime
parseISO8601 string = utcTimeToPOSIXSeconds <$> utcTime
  where
    utcTime = parseTimeM False defaultTimeLocale formatString (TL.unpack string)
    formatString = (iso8601DateFormat Nothing)

parseEpochTime :: TL.Text -> Maybe Integer
parseEpochTime string = readMaybe . TL.unpack $ string

filterTimeRange :: (Maybe POSIXTime) -> [Integer] -> [Integer]
filterTimeRange (Just posixTime) pings = pings
                                          & Prelude.filter predicate
  where
    posixTime' = (truncate . toRational $ posixTime) :: Integer
    predicate = (inRange (posixTime', posixTime' + 86400))

filterTimeRange _ _ = []

{-- DB --}

session :: Redis (Either Reply (Maybe ByteString))
session = do
  R.set "hello" "haskell"
  R.get "hello"

postPing :: TL.Text -> TL.Text -> Redis (Either Reply Integer)
postPing deviceID epochTime = do
  R.rpush (textToByteString deviceID) [(textToByteString epochTime)]

getDevices :: Redis (Either Reply ([ByteString]))
getDevices = do
  R.keys "*"

getDevicePings :: TL.Text -> Redis (Either Reply [ByteString])
getDevicePings deviceID = do
  R.lrange (textToByteString deviceID) 0 (-1)

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

      (Right devicePings) <- liftIO $ (runRedis conn (getDevicePings deviceID))
      let (devicePings' :: [Integer]) = (read . TL.unpack . byteStringToText) <$> devicePings

      json $ (filterTimeRange $ (parseISO8601 date)) $ devicePings'

    S.get "/devices" $ do
      (Right devices) <- liftIO $ (runRedis conn getDevices)
      json $ (fmap byteStringToText devices)


