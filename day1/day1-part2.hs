import Data.Char (isDigit)

patchLine :: String -> String
patchLine [] = ""
patchLine str@('o' : 'n' : 'e' : _) = "1" ++ patchLine (tail str)
patchLine str@('t' : 'w' : 'o' : _) = "2" ++ patchLine (tail str)
patchLine str@('t' : 'h' : 'r' : 'e' : 'e' : _) = "3" ++ patchLine (tail str)
patchLine str@('f' : 'o' : 'u' : 'r' : _) = "4" ++ patchLine (tail str)
patchLine str@('f' : 'i' : 'v' : 'e' : _) = "5" ++ patchLine (tail str)
patchLine str@('s' : 'i' : 'x' : _) = "6" ++ patchLine (tail str)
patchLine str@('s' : 'e' : 'v' : 'e' : 'n' : _) = "7" ++ patchLine (tail str)
patchLine str@('e' : 'i' : 'g' : 'h' : 't' : _) = "8" ++ patchLine (tail str)
patchLine str@('n' : 'i' : 'n' : 'e' : _) = "9" ++ patchLine (tail str)
patchLine (x : xs) = x : patchLine xs

getNumber :: String -> Int
getNumber input = do
  let numbers = filter isDigit (patchLine input)
  read [head numbers, last numbers] :: Int

decrypt :: [String] -> Int
decrypt = foldr ((+) . getNumber) 0

main :: IO ()
main = readFile "day1-input" >>= print . decrypt . lines
