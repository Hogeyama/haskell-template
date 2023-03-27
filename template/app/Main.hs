module Main (main) where

import MyLib qualified (someFunc)
import RIO

main :: IO ()
main = do
  hPutBuilder stdout "Hello, Haskell!"
  MyLib.someFunc
