import Data.List (elemIndices, findIndices, tails)

transposeMatrix :: [[Char]] -> [[Char]]
transposeMatrix [] = []
transposeMatrix ([] : _) = []
transposeMatrix matrix = map head matrix : transposeMatrix (map tail matrix)

verticalExpansion :: [[Char]] -> [[Char]]
verticalExpansion [] = []
verticalExpansion (x : xs)
  | all (== '.') x = x : x : verticalExpansion xs
  | otherwise = x : verticalExpansion xs

findGalaxies :: Int -> [[Char]] -> [(Int, Int)]
findGalaxies _ [] = []
findGalaxies i (x : xs) = map (,i) (elemIndices '#' x) ++ findGalaxies (i + 1) xs

allPairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
allPairs xs = [(x, y) | (x : rest) <- tails xs, y <- rest]

distance :: ((Int, Int), (Int, Int)) -> Int
distance ((a, b), (x, y)) = abs (a - x) + abs (b - y)

main :: IO ()
main = readFile "day11-input" >>= print . sum . map distance . allPairs . findGalaxies 0 . transposeMatrix . verticalExpansion . transposeMatrix . verticalExpansion . lines
