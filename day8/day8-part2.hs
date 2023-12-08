import Data.Char (isSpace)
import qualified Data.Map as Map

type Instruction = Char

type Instructions = [Instruction]

type Maps = Map.Map Node (Node, Node)

type Node = String

splitOnFirst :: Char -> String -> (String, String)
splitOnFirst sep str = let (a, b) = break (== sep) str in (a, drop 1 b)

parseMap :: String -> (Node, (Node, Node))
parseMap input = do
  let (node, rest) = splitOnFirst '=' (filter (not . isSpace) input)
  let directions = splitOnFirst ',' (filter (\x -> x /= '(' && x /= ')') rest)
  (node, directions)

parseMaps :: [String] -> Maps
parseMaps = Map.fromList . map parseMap

parse :: [String] -> (Instructions, Maps)
parse (inst : _ : maps) = (inst, parseMaps maps)

advance :: Node -> Instruction -> Maps -> Node
advance pos 'L' maps = fst $ maps Map.! pos
advance pos 'R' maps = snd $ maps Map.! pos

advanceTillGoal :: Node -> Int -> Instructions -> Maps -> Int
advanceTillGoal pos n (i : rest) maps
  | 'Z' `elem` pos = n
  | otherwise = advanceTillGoal (advance pos i maps) (n + 1) (rest ++ [i]) maps

findStarts :: Maps -> [Node]
findStarts = filter (elem 'A') . map fst . Map.toList

findCommonGoal :: [Node] -> Instructions -> Maps -> Int
findCommonGoal [] _ _ = 1
findCommonGoal (x : xs) inst maps = advanceTillGoal x 0 inst maps `lcm` findCommonGoal xs inst maps

stepsTillGoal :: [String] -> Int
stepsTillGoal input = let (inst, maps) = parse input in findCommonGoal (findStarts maps) inst maps

main :: IO ()
main = readFile "day8-input" >>= print . stepsTillGoal . lines
