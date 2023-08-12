module MyLib (hello) where

import RIO

-- $setup
-- >>> import System.IO.Silently

-- | Some function
--
-- >>> capture_ hello
-- "Hello World!\n"
hello :: IO ()
hello = hPutBuilder stdout "Hello World!\n"
