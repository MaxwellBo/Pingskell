{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}

import Web.Scotty as S 
import Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE

import Database.Redis as R

import Data.ByteString.Char8 as BE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

session :: Redis (Either Reply (Maybe ByteString))
session = do
  R.set "hello" "haskell"
  R.get "hello"

postToDB :: ByteString -> ByteString -> Redis (Either Reply Integer)
postToDB deviceID epochTime = do
  R.append deviceID epochTime


main :: IO ()
main = do 
  conn <- connect defaultConnectInfo { connectHost = "127.0.0.1" }
  res <- runRedis conn session
  print res

  scotty 3000 $ do
    S.post "/:device_id/:epoch_time" $ do
      deviceID <- param "device_id"
      epochTime <- param "epoch_time"

      let (deviceID' :: ByteString) = BL.toStrict $ TLE.encodeUtf8 $ deviceID 
      let (epochTime' :: ByteString) = BL.toStrict $ TLE.encodeUtf8 $ epochTime

      -- liftIO $ (runRedis conn (postToDB deviceID' epochTime'))

      text $ mconcat [ deviceID, " ", epochTime ]
