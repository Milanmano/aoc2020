import Data.List.Split
import Data.List
main = do
  contents <- readFile "input"
  let list = map words $ splitOn "\n\n" contents
  print $ length $ concat $ map nub $ map concat list
 
