import Data.Char (isDigit, isSpace)
import Data.List (groupBy)

type Coordinate = (Int, Char)

containsSymbolInRange :: [String] -> (Int, Int) -> Bool
containsSymbolInRange [] _ = False
containsSymbolInRange (x : xs) (min, max) = do
  let slice = take (max - min + 1) (drop min x)
  any (\x -> not $ isDigit x && x /= '.') slice || containsSymbolInRange xs (min, max)

containsSymbol :: [Coordinate] -> Bool
containsSymbol = foldr (\(_, x) -> (||) (x /= '.' && not (isDigit x))) False

toNumber :: [Coordinate] -> Int
toNumber input = read $ filter isDigit ('0' : map snd input)

checkCandidate :: [Coordinate] -> [String] -> Int
checkCandidate focus adj
  | containsSymbolInRange adj (minimum, maximum) = number
  | otherwise = 0
  where
    number = toNumber focus
    minimum = max 0 (fst (head focus) - 1)
    maximum = min (length $ head adj) (fst (last focus) + 1)

checkCandidates :: [[Coordinate]] -> [String] -> [Int]
checkCandidates [] _ = []
checkCandidates (x : xs) adj = checkCandidate x adj : checkCandidates xs adj

scanLine :: String -> [String] -> [Int]
scanLine focus adj = do
  let enumerated = zip [0 ..] focus
  let grouped = groupBy (\(_, a) (_, b) -> isDigit a && isDigit b) enumerated
  let filteredNumbers = filter (isDigit . snd . (!! 0)) grouped
  checkCandidates filteredNumbers adj

scanLines :: [String] -> [Int]
scanLines input@(line1 : line2 : line3 : _) = scanLine line2 [line1, line2, line3] ++ scanLines (tail input)
scanLines _ = []

findPartNumbers :: [String] -> [Int]
findPartNumbers input = scanLines $ [empty] ++ input ++ [empty]
  where
    empty = replicate (length $ head input) '.'

main :: IO ()
main = readFile "day3-input" >>= print . sum . findPartNumbers . lines
