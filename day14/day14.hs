import Data.List (groupBy, sort, transpose)

tilt :: String -> String
tilt input = concatMap (reverse . sort) $ groupBy (\a b -> a /= '#' && b /= '#') input

load :: String -> Int
load [] = 0
load ('O' : xs) = length xs + 1 + load xs
load (_ : xs) = load xs

main :: IO ()
main = readFile "day14-input" >>= print . sum . map (load . tilt) . transpose . lines
