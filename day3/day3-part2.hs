import Data.Char (isDigit, isSpace)
import Data.List (foldl', groupBy)

type Coordinate = (Int, Char)

extractNumbersInRange :: [[Coordinate]] -> (Int, Int) -> [Int]
extractNumbersInRange [] _ = []
extractNumbersInRange (line : lines) (minBound, maxBound)
  | containsNumberInRange slice = rest ++ [convertToNumber line]
  | otherwise = rest
  where
    slice = filter (\(i, _) -> minBound <= i && i <= maxBound) line
    rest = extractNumbersInRange lines (minBound, maxBound)

extractNumbersFromGroups :: [String] -> (Int, Int) -> [Int]
extractNumbersFromGroups [] _ = []
extractNumbersFromGroups (line : lines) (minBound, maxBound) = do
  let enumerated = zip [0 ..] line
  let grouped = groupBy (\(_, a) (_, b) -> isDigit a && isDigit b) enumerated
  let filteredNumbers = filter (isDigit . snd . (!! 0)) grouped
  extractNumbersInRange filteredNumbers (minBound, maxBound) ++ extractNumbersFromGroups lines (minBound, maxBound)

containsNumberInRange :: [Coordinate] -> Bool
containsNumberInRange = foldr (\(_, x) -> (||) (isDigit x)) False

convertToNumber :: [Coordinate] -> Int
convertToNumber coords = read $ filter isDigit ('0' : map snd coords)

checkCoordinate :: Coordinate -> [String] -> Int
checkCoordinate focus adjacent
  | length numbers == 2 = foldl' (*) 1 numbers
  | otherwise = 0
  where
    lowerBound = max 0 (fst focus - 1)
    upperBound = min (length $ head adjacent) (fst focus + 1)
    numbers = extractNumbersFromGroups adjacent (lowerBound, upperBound)

checkCoordinates :: [Coordinate] -> [String] -> [Int]
checkCoordinates [] _ = []
checkCoordinates (coord : coords) adjacentLines = checkCoordinate coord adjacentLines : checkCoordinates coords adjacentLines

scanCurrentLine :: String -> [String] -> [Int]
scanCurrentLine focus adjacent = do
  let enumerated = zip [0 ..] focus
  let filtered = filter ((== '*') . snd) enumerated
  checkCoordinates filtered adjacent

windowScanLines :: [String] -> [Int]
windowScanLines input@(line1 : line2 : line3 : _) = scanCurrentLine line2 [line1, line2, line3] ++ windowScanLines (tail input)
windowScanLines _ = []

findPartNumbers :: [String] -> [Int]
findPartNumbers input = windowScanLines $ [empty] ++ input ++ [empty]
  where
    empty = replicate (length $ head input) '.'

main :: IO ()
main = do
  input <- readFile "day3-input"
  print $ sum $ findPartNumbers $ lines input
