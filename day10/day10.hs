import Data.List (elemIndex)

type Map = [[Char]]

type Point = (Int, Int)

filterPossible :: [Point] -> Point -> Map -> [Point]
filterPossible [] _ _ = []
filterPossible ((x, y) : xs) origin map
  | origin `elem` possible = (x, y) : rest
  | otherwise = rest
  where
    possible = conntectingPoints (map !! y !! x) (x, y)
    rest = filterPossible xs origin map

findStart :: Int -> Map -> Point
findStart y (x : xs) = case elemIndex 'S' x of
  Just x -> (x, y)
  Nothing -> findStart (y + 1) xs

allSame :: [Point] -> Bool
allSame (x : xs) = all (\(a, b) -> a == fst x && b == snd x) xs
allSame _ = True

conntectingPoints :: Char -> Point -> [Point]
conntectingPoints '|' (x, y) = [(x, y - 1), (x, y + 1)]
conntectingPoints '-' (x, y) = [(x - 1, y), (x + 1, y)]
conntectingPoints 'L' (x, y) = [(x, y - 1), (x + 1, y)]
conntectingPoints 'J' (x, y) = [(x, y - 1), (x - 1, y)]
conntectingPoints '7' (x, y) = [(x - 1, y), (x, y + 1)]
conntectingPoints 'F' (x, y) = [(x + 1, y), (x, y + 1)]
conntectingPoints _ _ = []

--                      Previous   Current
advancePoints :: Map -> [Point] -> [Point] -> [Point]
advancePoints _ _ [] = []
advancePoints map ((xo, yo) : xs) ((x, y) : ys) = do
  let connecting = conntectingPoints (map !! y !! x) (x, y)
  filter (\(x, y) -> x /= xo || y /= yo) connecting ++ advancePoints map xs ys

--                                Previous   Current           Previous Current
advanceTillMerge :: Map -> Int -> [Point] -> [Point] -> (Int, ([Point], [Point]))
advanceTillMerge map i origins points
  | allSame points = (i, ([], []))
  | otherwise = advanceTillMerge map (i + 1) points (advancePoints map origins points)

aPlaceFurtherThanTheUniverse :: Map -> Int
aPlaceFurtherThanTheUniverse map = do
  let (x, y) = findStart 0 map
  let nextPoints = filterPossible (filter (\(a, b) -> a >= 0 && b >= 0) [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]) (x, y) map
  fst $ advanceTillMerge map 1 (replicate (length nextPoints) (x, y)) nextPoints

main :: IO ()
main = readFile "day10-input" >>= print . aPlaceFurtherThanTheUniverse . lines
