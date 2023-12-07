import Data.Char (isDigit)

--                   time   dist   press
findShortestPress :: Int -> Int -> Int
findShortestPress = findPressHelper 0 1

--                  time   dist   press
findLongestPress :: Int -> Int -> Int
findLongestPress time = findPressHelper time (-1) time

findPressHelper :: Int -> Int -> Int -> Int -> Int
findPressHelper press delta time distance
  | dist > distance = press
  | otherwise = findPressHelper (press + delta) delta time distance
  where
    dist = (time - press) * press

getWins :: Int -> Int -> Int
getWins time distance = max - min + 1
  where
    min = findShortestPress time distance
    max = findLongestPress time distance

countWins :: [Int] -> [Int] -> Int
countWins [] [] = 1
countWins (t : ts) (d : dx) = getWins t d * countWins ts dx

calculateMargin :: [[Int]] -> Int
calculateMargin (x : y : _) = countWins x y

parseNumbers :: String -> [Int]
parseNumbers input = parseNumbersHelper input ""

parseNumbersHelper :: String -> String -> [Int]
parseNumbersHelper [] [] = []
parseNumbersHelper [] x = [read x]
parseNumbersHelper (x : xs) y
  | isDigit x = parseNumbersHelper xs (y ++ [x])
  | not (null y) = read y : parseNumbersHelper xs ""
  | otherwise = parseNumbersHelper xs ""

main :: IO ()
main = readFile "day6-input" >>= print . calculateMargin . map parseNumbers . lines
