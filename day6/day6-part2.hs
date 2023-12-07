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

calculateMargin :: [Int] -> Int
calculateMargin (x : y : _) = getWins x y

parseNumbers :: String -> Int
parseNumbers input = read $ filter isDigit input

main :: IO ()
main = readFile "day6-input" >>= print . calculateMargin . map parseNumbers . lines
