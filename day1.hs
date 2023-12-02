import Data.Char (isDigit)

getNumber :: String -> Int
getNumber input = do
  let numbers = filter isDigit  input
  read [head numbers, last numbers] :: Int

decrypt :: [String] -> Int
decrypt = foldr ((+) . getNumber) 0

input = [ "1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet" ]

main :: IO ()
main = print $ decrypt input
