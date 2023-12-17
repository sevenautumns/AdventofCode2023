import Data.List (singleton)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Direction = Int -- 0: Right, 1: Down, 2: Left, 3: Up

type Loss = Int

type Chart = [[Loss]]

type Point = (Int, Int)

type Seen = Set.Set (Point, Direction)

type Losses = Map.Map (Point, Direction) Loss

type Queue = [(Loss, Point, Direction)]

directions = [(1, 0), (0, 1), (-1, 0), (0, -1)] :: [Point]

addTuple (x, y) (a, b) = (x + a, y + b)

insertSorted :: [(Loss, Point, Direction)] -> [(Loss, Point, Direction)] -> [(Loss, Point, Direction)]
insertSorted x [] = x
insertSorted [] y = y
insertSorted (x@(c1, p1, d1) : xs) (y@(c2, p2, d2) : ys)
  | c1 <= c2 = x : insertSorted xs (y : ys)
  | otherwise = y : insertSorted (x : xs) ys

inRange :: Point -> Chart -> Bool
inRange (x, y) chart = x >= 0 && y >= 0 && x < length (head chart) && y < length chart

patchQueueAndLosses :: Loss -> Direction -> Point -> Losses -> (Queue, Losses)
patchQueueAndLosses loss dir cur losses
  | Just prevLoss <- Map.lookup (cur, dir) losses, loss >= prevLoss = ([], losses)
  | otherwise = ([(loss, cur, dir)], Map.insert (cur, dir) loss losses)

applyDistance :: Int -> Loss -> Direction -> Point -> Losses -> Chart -> (Queue, Losses)
applyDistance 0 _ _ _ losses _ = ([], losses)
applyDistance dist loss dir cur losses chart
  | not $ inRange newPos chart = ([], losses)
  | dist > 10 - 3 = applyDistance (dist - 1) newLoss dir newPos losses chart
  | otherwise = do
      let (q1, l1) = patchQueueAndLosses newLoss dir newPos losses
      let (q2, l2) = applyDistance (dist - 1) newLoss dir newPos l1 chart
      (insertSorted q1 q2, l2)
  where
    newPos@(x, y) = addTuple (directions !! dir) cur
    newLoss = loss + (chart !! y !! x)

applyDirection :: [Direction] -> Loss -> Point -> Losses -> Chart -> (Queue, Losses)
applyDirection [] _ _ losses _ = ([], losses)
applyDirection (dir : dirs) loss cur@(x, y) losses chart = do
  let (q1, l1) = applyDistance 10 loss dir cur losses chart
  let (q2, l2) = applyDirection dirs loss cur l1 chart
  (insertSorted q1 q2, l2)

dijkstra :: Queue -> Seen -> Losses -> Chart -> Losses
dijkstra [] seen losses chart = losses
dijkstra ((loss, cur@(x, y), dir) : queue) seen losses chart
  | Set.member (cur, dir) seen = dijkstra queue seen losses chart
  | otherwise = do
      let newSeen = Set.insert (cur, dir) seen
      let dirs = filter (\x -> x /= dir && ((x + 2) `mod` 4) /= dir) [0 .. 3]
      let (q, l) = applyDirection dirs loss cur losses chart
      dijkstra (insertSorted q queue) newSeen l chart

main :: IO ()
main = do
  input <- readFile "day17-input"
  let chart = map (map ((read :: (String -> Int)) . singleton)) $ lines input
  let maxX = length (head chart) - 1
  let maxY = length chart - 1
  let queue = [(0, (0, 0), -1)] :: Queue
  let res = Map.map id $ Map.filterWithKey (\((x, y), _) _ -> x == maxX && y == maxY) $ dijkstra queue Set.empty Map.empty chart
  print $ minimum res
