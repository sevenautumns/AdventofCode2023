import Data.List (find, nub, sort, sortBy)

type Point = (Int, Int)

addTuple (x, y) (a, b) = (x + a, y + b)

splitAll :: Char -> String -> [String]
splitAll _ [] = []
splitAll c input = let (left, right) = break (== c) input in left : splitAll c (drop 1 right)

pair :: [a] -> [(a, a)]
pair [] = []
pair (a : b : xs) = (a, b) : pair xs

toInstruction :: [String] -> (Char, Int)
toInstruction ((c : _) : i : _) = (c, read i)

convertInstruction :: Point -> [(Char, Int)] -> [(Point, (Int, Int))]
convertInstruction _ [] = []
convertInstruction cur (('R', i) : xs) = (cur, (i, 0)) : convertInstruction (addTuple (i, 0) cur) xs
convertInstruction cur (('L', i) : xs) = (cur, (-i, 0)) : convertInstruction (addTuple (-i, 0) cur) xs
convertInstruction cur (('D', i) : xs) = (cur, (0, i)) : convertInstruction (addTuple cur (0, i)) xs
convertInstruction cur (('U', i) : xs) = (cur, (0, -i)) : convertInstruction (addTuple cur (0, -i)) xs

nextY :: Int -> [(Point, (Int, Int))] -> Int
nextY cur points
  | head numbers - cur > 1 = head numbers - 1
  | head numbers == cur = cur + 1
  | otherwise = head numbers
  where
    numbers = sort $ nub $ concatMap (filter (>= cur) . (\((_, y), (_, l)) -> [y, y + l])) points

removeHigher :: Int -> [(Point, (Int, Int))] -> [(Point, (Int, Int))]
removeHigher cur = filter (\((_, y), (_, l)) -> cur <= y + l || cur <= y)

filterDoubles :: [(Point, (Int, Int))] -> [(Point, (Int, Int))]
filterDoubles [] = []
filterDoubles [x] = [x]
filterDoubles (a@((_, ay), (_, al)) : b@((_, by), (_, bl)) : xs)
  | al < 0 && bl < 0 = filterDoubles (a : xs)
  | al > 0 && bl > 0 = filterDoubles (b : xs)
  | otherwise = a : filterDoubles (b : xs)

merge :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
merge _ [] = []
merge _ [x] = [x]
merge horizontal (a@(ax, ay) : b@(bx, by) : xs)
  | Just p <- h = merge horizontal ((ax, by) : xs)
  | otherwise = a : merge horizontal (b : xs)
  where
    h = find (\(s, t) -> s == ay && t == bx) horizontal

fillX :: Int -> [(Point, (Int, Int))] -> Int
fillX curY points = do
  let vertical = filter (\(_, (_, y)) -> y /= 0) points
  let horizontal = filter (\((_, y), (xl, yl)) -> xl /= 0 && (y == curY || y + yl == curY)) points
  let pairedHorizontal = pair $ sort $ nub $ concatMap (\((x, _), (xl, _)) -> [x, x + xl]) horizontal
  let relevantVertical = filter (\((_, y), (_, l)) -> (curY >= y && curY <= y + l) || (curY >= y + l && curY <= y)) vertical
  let sortedRelevantVertical = sortBy (\((a, _), _) ((b, _), _) -> compare a b) relevantVertical
  let prunedDoubleVertical = map (\((a, _), _) -> a) $ filterDoubles sortedRelevantVertical
  let mergedVerticalHorizontal = merge pairedHorizontal (pair prunedDoubleVertical)
  sum $ map (\(a, b) -> b - a + 1) mergedVerticalHorizontal

fill :: Int -> [(Point, (Int, Int))] -> Int
fill _ [] = 0
fill y points = do
  let next = nextY y points
  ((next - y) * fillX y points) + fill next (removeHigher next points)

main :: IO ()
main = readFile "day18-input" >>= print . fill minBound . convertInstruction (0, 0) . map (toInstruction . splitAll ' ') . lines
