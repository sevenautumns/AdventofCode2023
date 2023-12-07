import Data.Char (isDigit)
import Data.List (groupBy)

type Range = (Int, Int)

addToRange :: Range -> Int -> Range
addToRange (x, y) i = (x + i, y + i)

applyRule :: [Int] -> Range -> ([Range], [Range])
applyRule (dest : start : delta : _) input
  | snd input < start || fst input > start + delta - 1 = ([input], [])
  | fst input >= start && snd input <= start + delta - 1 = ([], [addToRange input (dest - start)])
  | fst input < start && snd input > start + delta - 1 = ([(fst input, start - 1), (start + delta, snd input)], [(dest, dest + delta - 1)])
  | fst input < start = ([(fst input, start - 1)], [addToRange (start, snd input) (dest - start)])
  | snd input > start + delta - 1 = ([(start + delta, snd input)], [addToRange (fst input, start + delta - 1) (dest - start)])

applyMap :: Range -> [[Int]] -> [Range]
applyMap input [] = [input]
applyMap input (x : xs)
  | length rest == 2 = hits ++ applyMap (head rest) xs ++ applyMap (last rest) xs
  | length rest == 1 = hits ++ applyMap (head rest) xs
  | otherwise = hits
  where
    (rest, hits) = applyRule x input

applyMaps :: [Range] -> [[[Int]]] -> [Range]
applyMaps = foldl (\ranges maps -> concatMap (`applyMap` maps) ranges)

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

parseSeeds :: [Int] -> [Range]
parseSeeds [] = []
parseSeeds (x : y : xs) = (x, x + y - 1) : parseSeeds xs

containsNumber :: String -> Bool
containsNumber [] = False
containsNumber (x : xs)
  | isDigit x = True
  | otherwise = containsNumber xs

findFirstSeed :: [String] -> Int
findFirstSeed input = do
  let grouped = groupBy (\a b -> containsNumber a && containsNumber b) input
  let pruned = pruneNonNumberGroups grouped
  let seeds = parseSeeds $ parseNumbers $ head $ head pruned
  let maps = map (map parseNumbers) (tail pruned)
  minimum $ map fst (applyMaps seeds maps)

main :: IO ()
main = readFile "day5-input" >>= print . findFirstSeed . lines
