import Data.Char (isDigit, isSpace)
import Data.List (find)
import Data.Maybe (fromMaybe)

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
  let (x, xs) = splitOnFirst [',', ';'] input
  isValidPick (filter (not . isSpace) x) && validateGame xs

validateGames :: [String] -> Int
validateGames [] = 0
validateGames (x : xs) = do
  let (ident, game) = splitOnFirst [':'] x
  let value = if validateGame game then read $ filter isDigit ident else 0
  validateGames xs + value

allowed = [(12, "red"), (13, "green"), (14, "blue")]

main :: IO ()
main = do
  input <- readFile "day2-input"
  print $ validateGames $ lines input
