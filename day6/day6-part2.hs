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

calculateMargin :: [Int] -> Int
calculateMargin (x : y : _) = getWins x y

parseNumbers :: String -> Int
parseNumbers = read . filter isDigit

main :: IO ()
main = readFile "day6-input" >>= print . calculateMargin . map parseNumbers . lines
