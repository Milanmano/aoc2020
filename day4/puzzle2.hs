import Data.List.Split
import Data.List
import Text.Regex.TDFA

main = do
  contents <- readFile "input"
  let list = map words $ splitOn "\n\n" contents
  print $ sum $ map check $ map (\x -> map (\y -> pair $ splitOn ":" y) x) list

pair :: [[Char]] -> ([Char], [Char])
pair (x:y:xs) = (x,y)

check :: [([Char], [Char])] -> Int
check xs | (length xs) >= 8 && (and $ validate xs) = 1
         | (length xs) == 7 && not (or $ map (\(x,_) -> "cid" == x) xs) && (and $ validate xs) = 1
         | otherwise = 0

validate :: [([Char], [Char])] -> [Bool]
validate ((x,y):xs) | x == "byr" && length y == 4 && (read y) >= 1920 && (read y) <= 2002 = True : validate xs
                    | x == "iyr" && length y == 4 && (read y) >= 2010 && (read y) <= 2020 = True : validate xs
                    | x == "eyr" && length y == 4 && (read y) >= 2020 && (read y) <= 2030 = True : validate xs
                    | x == "hgt" && isInfixOf "cm" y && (read $ init $ init y) >= 150 && (read $ init $ init y) <= 193 = True : validate xs
                    | x == "hgt" && isInfixOf "in" y && (read $ init $ init y) >= 59 && (read $ init $ init y) <= 76 = True : validate xs
                    | x == "hcl" && length y == 7 && y =~ "#[a-f0-9]{6}" = True : validate xs
                    | x == "ecl" && elem y ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] = True : validate xs
                    | x == "pid" && length y == 9 && y =~ "[0-9]{9}" = True : validate xs
                    | x == "cid" = True : validate xs
                    | otherwise = False : validate xs
validate [] = [True]

