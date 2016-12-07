{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}

import Web.Scotty as S 
import Database.Redis as R

import Control.Monad.Trans

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.ByteString.Char8 as BE

session :: Redis (Either Reply (Maybe ByteString))
session = do
  R.set "hello" "haskell"
  R.get "hello"

postPing :: TL.Text -> TL.Text -> Redis (Either Reply Integer)
postPing deviceID epochTime = do
  let (deviceID' :: ByteString) = BL.toStrict $ TLE.encodeUtf8 $ deviceID 
  let (epochTime' :: ByteString) = BL.toStrict $ TLE.encodeUtf8 $ epochTime
  R.append deviceID' epochTime'

getDevices :: Redis (Either Reply ([ByteString]))
getDevices = do
  R.keys "*"

main :: IO ()
main = do 
  conn <- connect defaultConnectInfo

  scotty 3000 $ do

    S.post "/:device_id/:epoch_time" $ do
      deviceID <- param "device_id"
      epochTime <- param "epoch_time"

      liftIO $ (runRedis conn (postPing deviceID epochTime))

      text $ "RESPONSE"

    S.get "/:deviceID/:date" $ do
      deviceID <- param "deviceID"
      date <- param "date"
      text $ mconcat [ deviceID, " ", date ]

    -- S.get "/devices" $ do

