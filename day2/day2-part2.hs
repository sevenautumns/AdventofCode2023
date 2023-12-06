import Data.Char (isDigit, isSpace)

splitOnFirst :: [Char] -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (`elem` sep) str in (a, drop 1 b)

parseBalls :: String -> (String, Int)
parseBalls input = do
  let (n, color) = span isDigit (filter (not . isSpace) input)
  let number = read n :: Int
  (color, number)

--                       Red  Gre  Blu
leastBalls :: String -> (Int, Int, Int)
leastBalls [] = (0, 0, 0)
leastBalls input
  | color == "red" && n > r = (n, g, b)
  | color == "green" && n > g = (r, n, b)
  | color == "blue" && n > b = (r, g, n)
  | otherwise = rest
  where
    (x, xs) = splitOnFirst [',', ';'] input
    (color, n) = parseBalls x
    rest@(r, g, b) = leastBalls xs

validateGames :: [String] -> Int
validateGames [] = 0
validateGames (x : xs) = do
  let game = snd $ splitOnFirst [':'] x
  let (r, g, b) = leastBalls game
  validateGames xs + (r * g * b)

main :: IO ()
main = do
  input <- readFile "day2-input"
  print $ validateGames $ lines input
