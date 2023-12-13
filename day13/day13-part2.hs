import Data.List (groupBy)

groupPatterns :: [String] -> [[String]]
groupPatterns input = filter (not . null . head) $ groupBy (\a b -> null a == null b) input

transposeMatrix :: [String] -> [String]
transposeMatrix [] = []
transposeMatrix ([] : _) = []
transposeMatrix matrix = map head matrix : transposeMatrix (map tail matrix)

isReflection :: Int -> [String] -> [String] -> Bool
isReflection 1 [] _ = True
isReflection 1 _ [] = True
isReflection i (x : left) (y : right) = let diffs = length $ filter (uncurry (/=)) $ zip x y in isReflection (i + diffs) left right
isReflection _ _ _ = False

findMirror :: [String] -> [String] -> Int
findMirror [] (x : right) = findMirror [x] right
findMirror left [] = 100 * findMirror [] (transposeMatrix left)
findMirror left right
  | isReflection 0 right $ reverse left = length left
  | otherwise = findMirror (left ++ [head right]) $ tail right

main :: IO ()
main = readFile "day13-input" >>= print . sum . map (findMirror [] . transposeMatrix) . groupPatterns . lines
