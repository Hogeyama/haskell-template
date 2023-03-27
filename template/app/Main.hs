module Main (main) where

import RIO

import MyLib qualified (hello)

main :: IO ()
main = MyLib.hello
