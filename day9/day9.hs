calculateDifferences :: [Int] -> [Int]
calculateDifferences (x : y : rest) = (y - x) : calculateDifferences (y : rest)
calculateDifferences _ = []

predictLast :: [Int] -> Int
predictLast input = if all (== 0) input then 0 else last input + predictLast (calculateDifferences input)

main :: IO () --                                   | predict    |  parse to [Int]   | to [String]
main = readFile "day9-input" >>= print . sum . map (predictLast . map read . words) . lines
