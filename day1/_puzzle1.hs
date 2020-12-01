import Data.List
import Data.Maybe
main = do
  contents <- readFile "input"
  let list = map read $ words $ contents
  let result = product  $ zipWith findResult list (replicate (length list) (map findOther list))
  print result

findOther :: Integer -> Integer
findOther a = 2020 - a

findResult :: Integer -> [Integer] -> Integer
findResult x xs = fromMaybe 1 (find (== x) xs)


