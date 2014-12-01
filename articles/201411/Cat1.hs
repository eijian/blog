--
-- Cat1
--

module Main (
  main
) where

import System.Environment

main :: IO ()
main = do
  (x:xs) <- getArgs
  putFile x

putFile :: String -> IO ()
putFile f = do
  cs <- readFile f
  putStr cs
