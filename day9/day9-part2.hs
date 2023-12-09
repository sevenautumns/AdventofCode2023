calculateDifferences :: [Int] -> [Int]
calculateDifferences (x : y : rest) = (y - x) : calculateDifferences (y : rest)
calculateDifferences _ = []

predictFirst :: [Int] -> Int
predictFirst input = if all (== 0) input then 0 else head input - predictFirst (calculateDifferences input)

main :: IO () --                                   | predict     |  parse to [Int]   | to [String]
main = readFile "day9-input" >>= print . sum . map (predictFirst . map read . words) . lines
