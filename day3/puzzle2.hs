main = do
  contents <- readFile "input"
  let list = words $ contents
  print $ product [(slope 1 $ second list), (slope 1 list), (slope 3 list), (slope 5 list), (slope 7 list)]

slope :: Int -> [[Char]] -> Int
slope x list = count '#' $ zipWith makeWay [0, x .. ] list

second (x:y:xs) = x : second xs;
second _ = []

makeWay :: Int -> [Char] -> Char
makeWay x ys = head (drop x $ concat (replicate 100 ys))

count x ys = length (filter (==x) ys)
