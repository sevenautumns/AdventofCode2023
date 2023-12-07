import Data.Char (isDigit)

getNumber :: String -> Int
getNumber input =
  let numbers = filter isDigit input
   in read [head numbers, last numbers]

decrypt :: [String] -> Int
decrypt = foldr ((+) . getNumber) 0

main :: IO ()
main = readFile "day1-input" >>= print . decrypt . lines
