import Data.Char (ord)

splitAll :: Char -> String -> [String]
splitAll _ [] = []
splitAll c input = let (left, right) = break (== c) input in left : splitAll c (drop 1 right)

hash = foldl (\a b -> (17 * (a + ord b)) `mod` 256) 0

main :: IO ()
main = readFile "day15-input" >>= print . sum . map hash . splitAll ',' . head . lines
