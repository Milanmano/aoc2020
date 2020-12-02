import Data.List.Split

main = do
  contents <- readFile "input"
  let list = splitOn "\n" contents 
  print $ length . filter (==1) $ check $ init $ concat $ zipWith splitOn (repeat " ") $ concat $ zipWith splitOn (repeat ":") $ concat $ zipWith splitOn (repeat "-") list

check :: [[Char]] -> [Int]
check [] = [0]
check (a:b:c:d:e:xs) | read b >= (length $ filter (== (head c)) e) && (read a <= (length $ filter (== (head c)) e)) = 1:check xs
                     | otherwise = 0:check xs
