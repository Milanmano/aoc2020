main = do
  contents <- readFile "input"
  let list = words $ contents
  let rows = [0..127]
  let cols = [0..7]
  print $ maximum $ map seatid $ zipWith3 solve (repeat rows) (repeat cols) list

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

