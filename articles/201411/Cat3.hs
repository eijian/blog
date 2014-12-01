--
-- Cat3
--

module Main (
  main
) where

import System.Environment

main :: IO ()
main = do
  xs <- getArgs
  putFiles xs

putFiles :: [String] -> IO ()
putFiles [] = do
  putStr ""
putFiles (x:xs) = do
  putFile x
  putFiles xs

putFile :: String -> IO ()
putFile f = do
  cs <- readFile f
  putStr cs
