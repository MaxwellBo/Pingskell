{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}

import Web.Scotty as S 
import Control.Monad.Trans
import qualified Data.Text as T
import Database.Redis as R
import Data.ByteString.Char8

session :: Redis (Either Reply (Maybe ByteString))
session = do
  R.set "hello" "haskell"
  R.get "hello"

main :: IO ()
main = do 
  conn <- connect defaultConnectInfo { connectHost = "127.0.0.1" }
  res <- runRedis conn session
  print res
  scotty 3000 $ do
    S.get "/:device_id/:epoch_time" $ do
      device_id <- param "device_id"
      epoch_time <- param "epoch_time"
      text $ mconcat [ device_id, " ", epoch_time ]
