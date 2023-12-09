import Data.Char (isSpace)
import qualified Data.Map as Map

type Instructions = [Char]

type Maps = Map.Map Node (Node, Node)

type Node = String

splitOnFirst :: Char -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (== sep) str in (a, drop 1 b)

parseMap :: String -> (Node, (Node, Node))
parseMap input = do
  let (node, rest) = splitOnFirst '=' $ filter (not . isSpace) input
  let directions = splitOnFirst ',' $ filter (`notElem` "()") rest
  (node, directions)

parseMaps :: [String] -> Maps
parseMaps = Map.fromList . map parseMap

parse :: [String] -> (Instructions, Maps)
parse (inst : _ : maps) = (cycle inst, parseMaps maps)

advanceTillGoal :: Node -> Int -> Instructions -> Maps -> Int
advanceTillGoal "ZZZ" n _ _ = n
advanceTillGoal pos n ('L' : rest) maps = let node = fst $ maps Map.! pos in advanceTillGoal node (n + 1) rest maps
advanceTillGoal pos n ('R' : rest) maps = let node = snd $ maps Map.! pos in advanceTillGoal node (n + 1) rest maps

stepsTillGoal :: [String] -> Int
stepsTillGoal input = let (instructions, maps) = parse input in advanceTillGoal "AAA" 0 instructions maps

main :: IO ()
main = readFile "day8-input" >>= print . stepsTillGoal . lines
