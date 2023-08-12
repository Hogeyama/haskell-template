module MyLib (hello, sayHello) where

import RIO

-- $setup
-- >>> import System.IO.Silently

-- | Some function
--
-- >>> capture_ sayHello
-- "Hello World!"
sayHello :: IO ()
sayHello = hPutBuilder stdout . fromString $ hello "World"

hello :: String -> String
hello name = "Hello " <> name <> "!"
