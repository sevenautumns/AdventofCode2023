import Data.List (group, intercalate)
import qualified Data.Map.Strict as Map

type Cache = Map.Map (String, [Int], Int) Int

splitOnFirst :: Char -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (== sep) str in (a, drop 1 b)

countValidCombinations :: Cache -> String -> [Int] -> Int -> (Int, Cache)
countValidCombinations cache [] [] 0 = (1, cache)
countValidCombinations cache [] _ _ = (0, cache)
countValidCombinations cache ('?' : xs) checks group
  | Just res <- cached = (res, cache)
  | otherwise = do
      let (left, c) = countValidCombinations cache ('#' : xs) checks group
      let (right, c2) = countValidCombinations c ('.' : xs) checks group
      let res = left + right
      (res, Map.insert ('?' : xs, checks, group) res c2)
  where
    cached = Map.lookup ('?' : xs, checks, group) cache
countValidCombinations cache ('#' : xs) checks group
  | Just res <- cached = (res, cache)
  | otherwise = do
      let (res, c) = countValidCombinations cache xs checks (group + 1)
      (res, Map.insert ('#' : xs, checks, group) res c)
  where
    cached = Map.lookup ('#' : xs, checks, group) cache
countValidCombinations cache ('.' : xs) checks 0
  | Just res <- cached = (res, cache)
  | otherwise = do
      let (res, c) = countValidCombinations cache xs checks 0
      (res, Map.insert ('.' : xs, checks, 0) res c)
  where
    cached = Map.lookup ('.' : xs, checks, 0) cache
countValidCombinations cache ('.' : xs) checks group
  | Just res <- cached = (res, cache)
  | null checks && group /= 0 = (0, cache)
  | null checks = do
      let (res, c) = countValidCombinations cache xs checks group
      (res, Map.insert ('.' : xs, checks, group) res c)
  | head checks == group = do
      let (res, c) = countValidCombinations cache xs (tail checks) 0
      (res, Map.insert ('.' : xs, checks, group) res c)
  | otherwise = (0, cache)
  where
    cached = Map.lookup ('.' : xs, checks, group) cache

isValid :: String -> [Int] -> Bool
isValid input = (== map length (filter (elem '#') $ group input))

parseNumbers :: String -> [Int]
parseNumbers [] = []
parseNumbers (',' : xs) = parseNumbers xs
parseNumbers input = let (x, xs) = break (== ',') input in read x : parseNumbers xs

countCombinations :: Cache -> String -> (Int, Cache)
countCombinations cache input = do
  let (record, rest) = splitOnFirst ' ' input
  let unfolded_record = intercalate "?" (replicate 5 record)
  let counts = parseNumbers rest
  let unfold_counts = concat $ replicate 5 counts
  let combinations = countValidCombinations cache (unfolded_record ++ ".") unfold_counts 0
  combinations

allCombinations :: Cache -> [String] -> Int
allCombinations _ [] = 0
allCombinations cache (x : xs) = let (count, c) = countCombinations cache x in count + allCombinations c xs

main :: IO ()
main = readFile "day12-input" >>= print . allCombinations Map.empty . lines
