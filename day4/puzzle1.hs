import Data.List.Split

main = do
  contents <- readFile "input"
  let list = map words $ splitOn "\n\n" contents
  print $ sum $ map check $ map (\x -> map (\y -> pair $ splitOn ":" y) x) list

pair :: [[Char]] -> ([Char], [Char])
pair (x:y:xs) = (x,y)

check :: [([Char], [Char])] -> Int
check xs | (length xs) >= 8 = 1
         | (length xs) == 7 && not (or $ map (\(x,_) -> "cid" == x) xs) = 1
         | otherwise = 0

