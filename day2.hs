import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe)
import Data.List (find)

splitOnFirst :: [Char] -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (`elem` sep) str in (a, drop 1 b)

isValidPick :: String -> Bool
isValidPick input = do
  let (number, color) = span isDigit input
  let number_parsed = read number :: Int
  let valid = maybe 0 fst $ find ((== color) . snd) allowed 
  valid >= number_parsed

validateGame :: String -> Bool
validateGame [] = True
validateGame input = do
  let (x, xs) = splitOnFirst [',',';'] input
  isValidPick (filter (not . isSpace) x) && validateGame xs

validateGames :: [String] -> Int
validateGames [] = 0
validateGames (x:xs) = do
  let (ident, game) = splitOnFirst [':'] x
  let value = if validateGame game then read $ filter isDigit ident else 0;
  validateGames xs + value

games = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
        \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
        \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
        \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
        \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

allowed = [(12,"red"),(13,"green"),(14,"blue")]

main :: IO ()
main = print $ validateGames $ lines games
