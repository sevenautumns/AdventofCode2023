import Data.Char (isDigit, isSpace)
import Data.List (groupBy)

splitOnFirst :: Char -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (== sep) str in (a, drop 1 b)

intersect :: [Int] -> [Int] -> [Int]
intersect [] _ = []
intersect (x : xs) compare
  | x `elem` compare = x : rest
  | otherwise = rest
  where
    rest = intersect xs compare

filterNumbers :: String -> [Int]
filterNumbers input = map read (filter (not . isSpace . head) (groupBy (\a b -> isDigit a && isDigit b) input))

solveGame :: String -> Int
solveGame input = do
  let (_, rest) = splitOnFirst ':' input
  let (winning, have) = splitOnFirst '|' rest
  let filtered_winning = filterNumbers winning
  let filtered_have = filterNumbers have
  let intersection = filtered_winning `intersect` filtered_have
  if null intersection then 0 else 2 ^ (length intersection - 1)

solveGames :: [String] -> Int
solveGames input = sum $ map solveGame input

main :: IO ()
main = do
  input <- readFile "day4-input"
  print $ solveGames $ lines input
