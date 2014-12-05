-- 
-- Cat4
-- 

module Main (
  main
) where

import System.Environment

main :: IO ()
main = do
  xs <- getArgs
  putFiles $ checkArgs xs

checkArgs :: [String] -> (Bool, [String])
checkArgs [] = (False, [])
checkArgs (x:xs)
  | x == "-n" = (True, xs)
  | otherwise = (False, (x:xs))

putFiles :: (Bool, [String]) -> IO ()
putFiles (_, []) = putStr ""
putFiles (b, (x:xs)) = do
  cs <- readFile x
  let cs' = decorate b cs
  putStr cs'
  putFiles (b, xs)

decorate :: Bool -> String -> String
decorate False cs = cs
decorate True cs = unlines $ map tr (zip [1..] $ lines cs)

tr :: (Int, String) -> String
tr (n, l) = (show n) ++ "\t" ++ l
