{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}

import Web.Scotty
import Control.Monad.Trans
import qualified Data.Text as T


main :: IO ()
main = do 
  scotty 3000 $ do
    get "/:device_id/:epoch_time" $ do
      device_id <- param "device_id"
      epoch_time <- param "epoch_time"
      text $ mconcat [ device_id, " ", epoch_time ]
