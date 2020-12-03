main = do
  contents <- readFile "input"
  let list = words $ contents
  print $ count '#' $ zipWith makeWay [0, 3 .. ] list


makeWay :: Int -> [Char] -> Char
makeWay x ys = head (drop x $ concat (replicate 100 ys))

count x ys = length (filter (==x) ys)
