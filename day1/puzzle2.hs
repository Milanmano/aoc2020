main = do
  contents <- readFile "input"
  let list = map read $ words $ contents
  print [(i*j*k) | i <- list, j <- list, k <- list, i+j+k == 2020]

