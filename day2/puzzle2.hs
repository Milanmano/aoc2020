import Data.List.Split

main = do
  contents <- readFile "input"
  let list = splitOn "\n" contents 
  print $ length . filter (==1) $ check $ init $ concat $ zipWith splitOn (repeat " ") $ concat $ zipWith splitOn (repeat ":") $ concat $ zipWith splitOn (repeat "-") list

check :: [[Char]] -> [Int]
check [] = [0]
check (a:b:c:d:e:xs) | (length e >= read b) && (xor (head (drop ((read b)-1) e) == last c) (head (drop ((read a)-1) e) == last c)) = 1:check xs
                     | otherwise = 0:check xs

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a
