--
-- Cat2
--

module Main (
  main
) where

import System.Environment

main :: IO ()
main = do
  xs <- getArgs
  map putFile xs

putFile :: String -> IO ()
putFile f = do
  cs <- readFile f
  putStr cs
