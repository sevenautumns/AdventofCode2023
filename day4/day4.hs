import Data.Char (isDigit, isSpace)
import Data.List (groupBy)

splitOnFirst :: Char -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (== sep) str in (a, drop 1 b)

intersect :: [Int] -> [Int] -> [Int]
intersect left right = [x | x <- left, x `elem` right]

filterNumbers :: String -> [Int]
filterNumbers = map read . words

calcPayout :: [a] -> Int
calcPayout [] = 0
calcPayout x = 2 ^ (length x - 1)

solveGame :: String -> Int
solveGame input = do
  let numbers = snd $ splitOnFirst ':' input
  let (winning, have) = splitOnFirst '|' numbers
  let intersection = filterNumbers winning `intersect` filterNumbers have
  calcPayout intersection

solveGames :: [String] -> Int
solveGames = sum . map solveGame

main :: IO ()
main = readFile "day4-input" >>= print . solveGames . lines
