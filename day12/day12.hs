import Data.List (group)

splitOnFirst :: Char -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (== sep) str in (a, drop 1 b)

generateCombinations :: String -> [String]
generateCombinations [] = [[]]
generateCombinations ('?' : xs) = concatMap (\s -> ['.' : s, '#' : s]) $ generateCombinations xs
generateCombinations (s : xs) = map (s :) $ generateCombinations xs

isValid :: String -> [Int] -> Bool
isValid input = (== map length (filter (elem '#') $ group input))

parseNumbers :: String -> [Int]
parseNumbers [] = []
parseNumbers (',' : xs) = parseNumbers xs
parseNumbers input = let (x, xs) = break (== ',') input in read x : parseNumbers xs

countCombinations :: String -> Int
countCombinations input = do
  let (record, rest) = splitOnFirst ' ' input
  let counts = parseNumbers rest
  let combinations = generateCombinations record
  length $ filter (`isValid` counts) combinations

main :: IO ()
main = readFile "day12-input" >>= print . sum . map countCombinations . lines
