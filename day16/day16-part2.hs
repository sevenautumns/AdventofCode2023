import qualified Data.Set as Set

type Contraption = [String]

type Cache = Set.Set (Direction, Point)

type Energized = Set.Set Point

type Point = (Int, Int)

type Direction = (Int, Int)

addTuple (x, y) (a, b) = (x + a, y + b)

transTuple (x, y) = (y, x)

negTuple (x, y) = (-x, -y)

cacheLayer :: Cache -> Direction -> Point -> Contraption -> (Cache, Energized)
cacheLayer cache dir cur@(x, y) contr
  | Set.member (dir, cur) cache = (cache, Set.empty)
  | x < 0 || y < 0 || y >= length contr || x >= length (head contr) = (cache, Set.empty)
  | otherwise = advanceBeam (Set.insert (dir, cur) cache) dir (cur, contr !! y !! x) contr

advanceBeam :: Cache -> Direction -> (Point, Char) -> Contraption -> (Cache, Energized)
advanceBeam cache dir (cur, '.') contr = let (c, res) = cacheLayer cache dir (addTuple cur dir) contr in (c, Set.insert cur res)
advanceBeam cache dir@(_, 0) (cur, '-') contr = let (c, res) = cacheLayer cache dir (addTuple cur dir) contr in (c, Set.insert cur res)
advanceBeam cache dir@(0, _) (cur, '|') contr = let (c, res) = cacheLayer cache dir (addTuple cur dir) contr in (c, Set.insert cur res)
advanceBeam cache dir (cur, '\\') contr = let (c, res) = cacheLayer cache d (addTuple cur d) contr in (c, Set.insert cur res) where d = transTuple dir
advanceBeam cache dir (cur, '/') contr = let (c, res) = cacheLayer cache d (addTuple cur d) contr in (c, Set.insert cur res) where d = negTuple $ transTuple dir
advanceBeam cache dir (cur, _) contr = do
  -- This is the case where we either have a `|` or `-`
  let (c1, r1) = cacheLayer cache (transTuple dir) (addTuple cur (transTuple dir)) contr
  let (c2, r2) = cacheLayer c1 (negTuple $ transTuple dir) (addTuple cur (negTuple $ transTuple dir)) contr
  (c2, Set.insert cur $ Set.union r1 r2)

starts length height =
  [((0, 1), (x, 0)) | x <- [0 .. length - 1]]
    ++ [((0, -1), (x, height - 1)) | x <- [0 .. length - 1]]
    ++ [((1, 0), (0, y)) | y <- [0 .. height - 1]]
    ++ [((-1, 0), (length - 1, y)) | y <- [0 .. height - 1]]

main :: IO ()
main = do
  input <- readFile "day16-input"
  let inputLines = lines input
  print $ maximum $ map (\(d, p) -> length $ snd $ cacheLayer Set.empty d p inputLines) $ starts (length $ head inputLines) $ length inputLines
