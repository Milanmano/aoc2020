import Data.List.Split
import Data.List
main = do
  contents <- readFile "input"
  let list = map words $ splitOn "\n\n" contents
  let chars = ['a'..'z']
  print $ sum $ map length $ zipWith filter (repeat (==True)) $ zipWith solve (repeat chars) list
 

solve :: [Char] -> [[Char]] -> [Bool]
solve [] xs = [False]
solve ys xs | all (\x -> elem (head ys) x) xs = (True:(solve (tail ys) xs))
            | otherwise = (False:(solve (tail ys) xs))




 
