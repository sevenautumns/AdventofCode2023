import Data.Char (isSpace)
import Data.List (group, sort, sortBy)
import Data.Ord (Down (Down), comparing)

splitOnSpace :: String -> (String, String)
splitOnSpace str = let (a, b) = break isSpace str in (a, drop 1 b)

countCards :: String -> [Int]
countCards input = sortBy (comparing Down) (map length $ (group . sort) input)

powerLevel :: [Int] -> Int
powerLevel (5 : _) = 6
powerLevel (4 : _) = 5
powerLevel (3 : 2 : _) = 4
powerLevel (3 : _) = 3
powerLevel (2 : 2 : _) = 2
powerLevel (2 : _) = 1
powerLevel _ = 0

charLevel :: Char -> Int
charLevel 'A' = 14
charLevel 'K' = 13
charLevel 'Q' = 12
charLevel 'J' = 11
charLevel 'T' = 10
charLevel c = read [c]

thrust :: String -> String -> Ordering
thrust [] [] = EQ
thrust (x : xs) (y : ys) = charLevel x `compare` charLevel y <> thrust xs ys

handLevel :: String -> Int
handLevel = powerLevel . countCards . fst . splitOnSpace

handLevelOrd :: String -> String -> Ordering
handLevelOrd x y = handLevel x `compare` handLevel y <> thrust x y

sumHands :: Int -> [String] -> Int
sumHands _ [] = 0
sumHands i (x : xs) = sumHands (i + 1) xs + i * read (snd $ splitOnSpace x)

calculatePayout :: [String] -> Int
calculatePayout = sumHands 1 . sortBy handLevelOrd

main :: IO ()
main = readFile "day7-input" >>= print . calculatePayout . lines
