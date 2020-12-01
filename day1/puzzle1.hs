main = do
  contents <- readFile "input"
  let list = map read $ words $ contents 
  print [(i*j) | i <- list, j <- list, i+j == 2020]

