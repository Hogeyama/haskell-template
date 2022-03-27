module MyLib (someFunc) where

import RIO

-- | Some function
--
-- >>> someFunc
-- someFunc
someFunc :: IO ()
someFunc = hPutBuilder stdout "someFunc"
