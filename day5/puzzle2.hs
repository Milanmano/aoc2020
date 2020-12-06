import Data.List
main = do
  contents <- readFile "input"
  let list = words $ contents
  let rows = [0..127]
  let cols = [0..7]
  print $ solve2 $ sort $ puzzle1 rows cols list 

halve :: [Int] -> ([Int],[Int])
halve x = (take (div (length x) 2) x, drop (div (length x) 2) x)

solve :: [Int] -> [Int] -> [Char] -> [Int]
solve xs ys (s:ss) | s == 'F' = solve (fst $ halve xs) ys ss
                   | s == 'B' = solve (snd $ halve xs) ys ss
                   | s == 'R' = solve xs (snd $ halve ys) ss
                   | s == 'L' = solve xs (fst $ halve ys) ss
solve (x:xs) (y:ys) [] = [x, y]

seatid :: [Int] -> Int
seatid (x:y:xs) = x * 8 + y

puzzle1 :: [Int] -> [Int] -> [[Char]] -> [Int]
puzzle1 rows cols list = map seatid $ zipWith3 solve (repeat rows) (repeat cols) list

solve2 :: [Int] -> Int
solve2 (x:y:xs) | x+2 == y = x+1
                | otherwise = solve2 (y:xs)


