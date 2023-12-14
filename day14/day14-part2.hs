import Data.List (groupBy, sort, transpose)
import qualified Data.Map.Strict as Map

type Cache = Map.Map [String] (Int, [String])

rotate = reverse . transpose

tilt :: String -> String
tilt input = concatMap (reverse . sort) $ groupBy (\a b -> a /= '#' && b /= '#') input

cachedCycle :: Int -> Cache -> [String] -> [String]
cachedCycle 0 _ input = input
cachedCycle i cache input
  | Just (oi, res) <- cached = cachedCycle ((i - (i `div` (oi - i)) * (oi - i)) - 1) cache res
  | otherwise = let res = iterate (rotate . map tilt) input !! 4 in cachedCycle (i - 1) (Map.insert input (i, res) cache) res
  where
    cached = Map.lookup input cache

load :: String -> Int
load [] = 0
load ('O' : xs) = length xs + 1 + load xs
load (_ : xs) = load xs

main :: IO ()
main = readFile "day14-input" >>= print . sum . map load . cachedCycle 1000000000 Map.empty . transpose . lines
