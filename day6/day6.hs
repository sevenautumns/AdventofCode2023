import Data.Char (isDigit)

--                   time   dist   press
findShortestPress :: Int -> Int -> Int
findShortestPress = findPressHelper 0 1

--                  time   dist   press
findLongestPress :: Int -> Int -> Int
findLongestPress time = findPressHelper time (-1) time

findPressHelper :: Int -> Int -> Int -> Int -> Int
findPressHelper press delta time distance
  | (time - press) * press > distance = press
  | otherwise = findPressHelper (press + delta) delta time distance

getWins :: Int -> Int -> Int
getWins time distance = findLongestPress time distance - findShortestPress time distance + 1

countWins :: [Int] -> [Int] -> Int
countWins [] [] = 1
countWins (t : ts) (d : dx) = getWins t d * countWins ts dx

calculateMargin :: [[Int]] -> Int
calculateMargin (x : y : _) = countWins x y

parseNumbers :: String -> [Int]
parseNumbers = parseNumbersHelper ""

parseNumbersHelper :: String -> String -> [Int]
parseNumbersHelper [] [] = []
parseNumbersHelper x [] = [read x]
parseNumbersHelper x (y : ys)
  | isDigit y = parseNumbersHelper (x ++ [y]) ys
  | not (null x) = read x : parseNumbersHelper "" ys
  | otherwise = parseNumbersHelper "" ys

main :: IO ()
main = readFile "day6-input" >>= print . calculateMargin . map parseNumbers . lines
