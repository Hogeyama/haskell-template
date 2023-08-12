module Main (main) where

import RIO
import Prelude (putStrLn)

import MyLib qualified

import Data.Aeson ((.=))
import Data.Aeson.Types (object)
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/hello/:name" $ do
    name <- param "name"
    text $ fromString $ MyLib.hello name
  get "/healthcheck" $ do
    liftIO $ putStrLn "healthcheck"
    json . object $
      [ "status" ..= "ok"
      ]
  where
    k ..= (v :: Text) = k .= v
