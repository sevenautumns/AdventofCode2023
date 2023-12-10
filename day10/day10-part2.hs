import Data.List (elemIndex, group, groupBy, nub, sort, sortBy)

type Map = [[Char]]

type Point = (Int, Int)

filterPossible :: [Point] -> Point -> Map -> [Point]
filterPossible [] _ _ = []
filterPossible ((x, y) : xs) origin map
  | origin `elem` conntectingPoints (map !! y !! x) (x, y) = (x, y) : rest
  | otherwise = rest
  where
    rest = filterPossible xs origin map

findStart :: Int -> Map -> Point
findStart y (x : xs) = case elemIndex 'S' x of
  Just x -> (x, y)
  Nothing -> findStart (y + 1) xs

allSame :: [Point] -> Bool
allSame (x : xs) = all (\(a, b) -> a == fst x && b == snd x) xs

conntectingPoints :: Char -> Point -> [Point]
conntectingPoints '|' (x, y) = [(x, y - 1), (x, y + 1)]
conntectingPoints '-' (x, y) = [(x - 1, y), (x + 1, y)]
conntectingPoints 'L' (x, y) = [(x, y - 1), (x + 1, y)]
conntectingPoints 'J' (x, y) = [(x, y - 1), (x - 1, y)]
conntectingPoints '7' (x, y) = [(x - 1, y), (x, y + 1)]
conntectingPoints 'F' (x, y) = [(x + 1, y), (x, y + 1)]
conntectingPoints _ _ = []

whatIsStart :: Point -> [Point] -> Char
whatIsStart (x, y) next
  | (x, y - 1) `elem` next && (x, y + 1) `elem` next = '|'
  | (x - 1, y) `elem` next && (x + 1, y) `elem` next = '-'
  | (x, y - 1) `elem` next && (x + 1, y) `elem` next = 'L'
  | (x, y - 1) `elem` next && (x - 1, y) `elem` next = 'J'
  | (x - 1, y) `elem` next && (x, y + 1) `elem` next = '7'
  | (x + 1, y) `elem` next && (x, y + 1) `elem` next = 'F'

--                      Previous   Current
advancePoints :: Map -> [Point] -> [Point] -> [Point]
advancePoints _ _ [] = []
advancePoints map ((xo, yo) : xs) ((x, y) : ys) = do
  let connecting = conntectingPoints (map !! y !! x) (x, y)
  filter (\(x, y) -> x /= xo || y /= yo) connecting ++ advancePoints map xs ys

--                         Previous     Current     Previous Current
advanceTillMerge :: Map -> [[Point]] -> [Point] -> ([[Point]], [Point])
advanceTillMerge map origins points
  | allSame points = (points : origins, [])
  | otherwise = advanceTillMerge map (points : origins) (advancePoints map (head origins) points)

countEncased :: [(Int, Char)] -> Int
countEncased (a : b : c : rest)
  | snd a `elem` "L" && snd b `elem` "J" = countEncased (c : rest)
  | snd a `elem` "F" && snd b `elem` "7" = countEncased (c : rest)
  | snd a `elem` "FL" = countEncased $ b : c : rest
  | snd b `elem` "L" && snd c `elem` "7" = fst b - fst a - 1 + countEncased rest
  | snd b `elem` "F" && snd c `elem` "J" = fst b - fst a - 1 + countEncased rest
  | snd b `elem` "LF" = fst b - fst a - 1 + countEncased (c : rest)
  | otherwise = fst b - fst a - 1 + countEncased (c : rest)
countEncased (a : b : rest)
  | snd a `elem` "L" && snd b `elem` "J" = countEncased rest
  | snd a `elem` "F" && snd b `elem` "7" = countEncased rest
  | snd a `elem` "FL" = countEncased $ b : rest
  | snd b `elem` "LF" = fst b - fst a - 1 + countEncased rest
  | otherwise = fst b - fst a - 1 + countEncased rest
countEncased _ = 0

aPlaceFurtherThanTheUniverse :: Map -> Int
aPlaceFurtherThanTheUniverse map_input = do
  let (x, y) = findStart 0 map_input
  let nextPoints = filterPossible (filter (\(a, b) -> a >= 0 && b >= 0) [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]) (x, y) map_input
  let start_char = whatIsStart (x, y) nextPoints
  let loop = nub $ concat $ fst $ advanceTillMerge map_input [replicate (length nextPoints) (x, y)] nextPoints
  let groups = groupBy (\a b -> snd a == snd b) (sortBy (\a b -> compare (snd a) (snd b) <> compare (fst a) (fst b)) loop)
  sum $ map (countEncased . map (\(i, c) -> if c == 'S' then (i, start_char) else (i, c)) . filter (\(a, b) -> b /= '-') . map (\(x, y) -> (x, map_input !! y !! x))) groups

main :: IO ()
main = readFile "day10-input" >>= print . aPlaceFurtherThanTheUniverse . lines
