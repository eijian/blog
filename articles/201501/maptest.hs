--
--
--

import Data.Map as Map
import Data.Maybe
import Control.Monad

type FilePrint = (String, FilePath)

src :: [FilePrint]
src = [("aaa", "file1.jpg"), ("abb", "file2.jpg"), ("abc", "file3.jpg"),
       ("aaa", "file4.jpg"), ("abc", "file5.jpg"), ("aaa", "file6.jpg")]

findSame :: [FilePrint] -> Map String [FilePath] -> Map String [FilePath]
findSame [] m = m
findSame (x:xs) m = findSame xs (addFile x m)

addFile :: FilePrint -> Map String [FilePath] -> Map String [FilePath]
addFile f m
  | e == Nothing = Map.insert k [(snd f)] m
  | otherwise    = Map.insert k f' m
  where
  k = fst f
  e = Map.lookup k m
  f' = (snd f):(fromJust e)

lenElem :: (String, [FilePath]) -> IO Bool
lenElem (s, xs) = return (length xs > 1)

main :: IO ()
main = do
  res <- filterM lenElem (toList (findSame src Map.empty))
  putStrLn (show res)
  
