import Data.Char (isDigit)

getNumber :: String -> Int
getNumber input = do
  let numbers = filter isDigit input
  read [head numbers, last numbers] :: Int

decrypt :: [String] -> Int
decrypt = foldr ((+) . getNumber) 0

main :: IO ()
main = do
  input <- readFile "day1-input"
  print $ decrypt $ lines input
