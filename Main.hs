{-# LANGUAGE OverloadedStrings #-} 
-- I think this is so that strings can be coerced into any type of string
-- without explicit declaration (`String` bad - `Text` good)

import Web.Scotty

main = scotty 3000 $ do
  get "/" $ do
    text "Hello World!"

  get "/:device_id/:epoch_time" $ do
    device_id <- param "device_id"
    epoch_time <- param "epoch_time"

    text $ mconcat [ device_id, " ", epoch_time ]