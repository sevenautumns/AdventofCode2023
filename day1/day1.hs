import Data.Char (isDigit)

getNumber :: String -> Int
getNumber input = do
  let numbers = filter isDigit input
  read [head numbers, last numbers] :: Int

decrypt :: [String] -> Int
decrypt = foldr ((+) . getNumber) 0

main :: IO ()
main = readFile "day1-input" >>= print . decrypt . lines
