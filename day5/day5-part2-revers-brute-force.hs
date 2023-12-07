import Data.Char (isDigit)
import Data.List (groupBy)

inRange :: [Int] -> Int -> Bool
inRange (x : _ : y : _) input = diff < y && diff >= 0 where diff = input - x

getOutput :: [Int] -> Int -> Int
getOutput (x : y : _) input = (input - x) + y

applyMap :: Int -> [[Int]] -> Int
applyMap i [] = i
applyMap input (x : xs)
  | inRange x input = getOutput x input
  | otherwise = applyMap input xs

applyMaps :: Int -> [[[Int]]] -> Int
applyMaps = foldl applyMap

parseNumbers :: String -> [Int]
parseNumbers input = parseNumbersHelper input ""

parseNumbersHelper :: String -> String -> [Int]
parseNumbersHelper [] [] = []
parseNumbersHelper [] x = [read x]
parseNumbersHelper (x : xs) y
  | isDigit x = parseNumbersHelper xs (y ++ [x])
  | not (null y) = read y : parseNumbersHelper xs ""
  | otherwise = parseNumbersHelper xs ""

pruneNonNumberGroups :: [[String]] -> [[String]]
pruneNonNumberGroups [] = []
pruneNonNumberGroups (x : xs)
  | not $ containsNumber $ head x = pruneNonNumberGroups xs
  | otherwise = x : pruneNonNumberGroups xs

parseSeeds :: [Int] -> [Int]
parseSeeds [] = []
parseSeeds (x : y : xs) = enumFromTo x (x + y) ++ parseSeeds xs

inSeeds :: Int -> [Int] -> Bool
inSeeds _ [] = False
inSeeds i (x : y : xs)
  | diff < y && diff >= 0 = True
  | otherwise = inSeeds i xs
  where
    diff = i - x

containsNumber :: String -> Bool
containsNumber [] = False
containsNumber (x : xs)
  | isDigit x = True
  | otherwise = containsNumber xs

findFirstHit :: [String] -> Int
findFirstHit input = do
  let grouped = groupBy (\a b -> containsNumber a && containsNumber b) input
  let pruned = pruneNonNumberGroups grouped
  let seeds = parseNumbers $ head $ head pruned
  let maps = reverse (map (map parseNumbers) (tail pruned))
  findFirstHitHelper 0 seeds maps

findFirstHitHelper :: Int -> [Int] -> [[[Int]]] -> Int
findFirstHitHelper i seeds maps
  | inSeeds res seeds = i
  | otherwise = findFirstHitHelper (i + 1) seeds maps
  where
    res = applyMaps i maps

main :: IO ()
main = readFile "day5-input" >>= print . findFirstHit . lines
