module MyLib (hello) where

import RIO

-- | Some function
--
-- >>> hello
-- Hello World!
hello :: IO ()
hello = hPutBuilder stdout "Hello World!\n"
