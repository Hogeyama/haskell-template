module Main (main) where

import           Gauge.Main
import           Gauge.Main.Options
import           MyLib              ()
import           RIO


main :: IO ()
main = defaultMainWith config
    [ bench "const" $ whnf const ()
    --, bench "someFunc" $ whnfIO someFunc
    ]
  where
    config = defaultConfig
