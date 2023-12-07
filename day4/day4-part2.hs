import Data.Char (isDigit, isSpace)
import Data.List (groupBy)

splitOnFirst :: Char -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (== sep) str in (a, drop 1 b)

intersect :: [Int] -> [Int] -> [Int]
intersect left = filter (`elem` left)

filterNumbers :: String -> [Int]
filterNumbers = map read . words

solveGame :: String -> Int
solveGame input = do
  let numbers = snd $ splitOnFirst ':' input
  let (winning, have) = splitOnFirst '|' numbers
  let intersection = filterNumbers winning `intersect` filterNumbers have
  length intersection

incCounts :: Int -> Int -> [Int] -> [Int]
incCounts 0 _ rest = rest
incCounts numbersToIncrease increaseBy (y : ys) = (y + increaseBy) : incCounts (numbersToIncrease - 1) increaseBy ys

walkOverGames :: [String] -> [Int] -> Int
walkOverGames [] [] = 0
walkOverGames (x : xs) (y : ys) = do
  let gain = solveGame x
  let counts = incCounts gain y ys
  walkOverGames xs counts + y

solveGames :: [String] -> Int
solveGames input = walkOverGames input (replicate (length input) 1)

main :: IO ()
main = readFile "day4-input" >>= print . solveGames . lines
