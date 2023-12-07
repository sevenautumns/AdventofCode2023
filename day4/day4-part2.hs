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
  length intersection

incCounts :: Int -> Int -> [Int] -> [Int]
incCounts 0 _ rest = rest
incCounts numbersToIncrease increaseBy (y : ys) = do
  (y + increaseBy) : incCounts (numbersToIncrease - 1) increaseBy ys

walkGames :: [String] -> [Int] -> Int
walkGames [] [] = 0
walkGames (x : xs) (y : ys) = do
  let gain = solveGame x
  let counts = incCounts gain y ys
  walkGames xs counts + y

solveGames :: [String] -> Int
solveGames input = do
  let counts = replicate (length input) 1
  walkGames input counts

main :: IO ()
main = readFile "day4-input" >>= print . solveGames . lines
