import Data.Char (ord)
import Data.List (findIndex)
import qualified Data.Map.Strict as Map

emptyBoxes = Map.fromList [(i, []) | i <- [0 .. 255]]

type Boxes = Map.Map Int [(String, Int)]

splitAll :: [Char] -> String -> [String]
splitAll _ [] = []
splitAll c input = let (left, right) = break (`elem` c) input in left : splitAll c (drop 1 right)

applyInstr :: Boxes -> [String] -> Boxes
applyInstr boxes [x] = Map.update (Just . filter ((/= x) . fst)) (hash x) boxes
applyInstr boxes (x : i : _)
  | Just li <- lens_index = Map.update (\lenses -> Just $ take li lenses ++ [(x, read i)] ++ drop (li + 1) lenses) index boxes
  | otherwise = Map.update (\lenses -> Just $ lenses ++ [(x, read i)]) (hash x) boxes
  where
    index = hash x
    lenses = boxes Map.! index
    lens_index = findIndex ((== x) . fst) lenses

hash = foldl (\a b -> (17 * (a + ord b)) `mod` 256) 0

focusPower = map (\(a, b) -> (a + 1) * sum (zipWith (*) (map snd b) [1 ..]))

main :: IO ()
main = readFile "day15-input" >>= print . sum . focusPower . Map.toList . foldl applyInstr emptyBoxes . map (splitAll "=-") . splitAll "," . head . lines
