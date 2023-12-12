import Data.List (elemIndices, findIndices, tails)

transposeMatrix :: [[Char]] -> [[Char]]
transposeMatrix [] = []
transposeMatrix ([] : _) = []
transposeMatrix matrix = map head matrix : transposeMatrix (map tail matrix)

addExpansion :: [Int] -> [Int] -> (Int, Int) -> (Int, Int)
addExpansion xpansion ypansion (x, y) = (x + length (filter (< x) xpansion) * 999999, y + length (filter (< y) ypansion) * 999999)

findExpansionLines :: [[Char]] -> [Int]
findExpansionLines = map fst . filter (all (== '.') . snd) . zip [0 ..]

findGalaxies :: Int -> [[Char]] -> [(Int, Int)]
findGalaxies _ [] = []
findGalaxies i (x : xs) = map (,i) (elemIndices '#' x) ++ findGalaxies (i + 1) xs

allPairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
allPairs xs = [(x, y) | (x : rest) <- tails xs, y <- rest]

distance :: ((Int, Int), (Int, Int)) -> Int
distance ((a, b), (x, y)) = abs (a - x) + abs (b - y)

main :: IO ()
main = do
  input <- lines <$> readFile "day11-input"
  let ypansion = findExpansionLines input
  let xpansion = findExpansionLines $ transposeMatrix input
  print $ sum $ map distance $ allPairs $ map (addExpansion xpansion ypansion) $ findGalaxies 0 input
