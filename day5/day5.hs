import Data.Char (isDigit)
import Data.List (groupBy)

inRange :: [Int] -> Int -> Bool
inRange (_ : x : y : _) input = diff < y && diff >= 0 where diff = input - x

getOutput :: [Int] -> Int -> Int
getOutput (x : y : _) input = (input - y) + x

applyMap :: Int -> [[Int]] -> Int
applyMap i [] = i
applyMap input (x : xs)
  | inRange x input = getOutput x input
  | otherwise = applyMap input xs

applyMapsHelper :: [Int] -> [[[Int]]] -> [Int]
applyMapsHelper xs maps = map (\x -> foldl applyMap x maps) xs

parseNumbers :: String -> [Int]
parseNumbers = parseNumbersHelper ""

parseNumbersHelper :: String -> String -> [Int]
parseNumbersHelper [] [] = []
parseNumbersHelper x [] = [read x]
parseNumbersHelper x (y : ys)
  | isDigit y = parseNumbersHelper (x ++ [y]) ys
  | not (null x) = read x : parseNumbersHelper "" ys
  | otherwise = parseNumbersHelper "" ys

pruneNonNumberGroups :: [[String]] -> [[String]]
pruneNonNumberGroups [] = []
pruneNonNumberGroups (x : xs)
  | not $ containsNumber $ head x = pruneNonNumberGroups xs
  | otherwise = x : pruneNonNumberGroups xs

containsNumber :: String -> Bool
containsNumber = foldr ((||) . isDigit) False

applyMaps :: [String] -> [Int]
applyMaps input = do
  let grouped = groupBy (\a b -> containsNumber a && containsNumber b) input
  let pruned = pruneNonNumberGroups grouped
  let maps = map (map parseNumbers) (tail pruned)
  let seeds = parseNumbers $ head $ head pruned
  applyMapsHelper seeds maps

main :: IO ()
main = readFile "day5-input" >>= print . minimum . applyMaps . lines
