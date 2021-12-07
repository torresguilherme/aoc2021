module AOC3 where

import Utils (readInputCharMat, toDec, invertBinary)
import Data.List (transpose)

numberOfChars :: Char -> String -> Int
numberOfChars c = length . filter (== c)

numberOfZeros ::String -> Int 
numberOfZeros = numberOfChars '0'

collectMostCommonBits :: String -> Char
collectMostCommonBits list = if numberOfZeros list > div (length list) 2 then '0'
    else '1'

findMostCommonBits :: [String] -> String
findMostCommonBits list = map collectMostCommonBits $ transpose list

sumPower :: [String] -> Int
sumPower s = toDec (findMostCommonBits s) * toDec (invertBinary $ findMostCommonBits s)

-- part 2

downToOne ::([String] -> String) -> Int -> [String] -> String
downToOne f _ [x] = x
downToOne f _  [] = "0"
downToOne f i xs = downToOne f (i + 1) $ filter (\s -> s !! i == f xs !! i) xs

filterMostCommonString :: [String] -> String
filterMostCommonString = downToOne findMostCommonBits 0

filterLeastCommonString :: [String] -> String
filterLeastCommonString = downToOne (invertBinary . findMostCommonBits) 0

findLifeSupply :: [String] -> Int 
findLifeSupply s = toDec (filterMostCommonString s) * toDec (filterLeastCommonString s)

aoc3 :: IO ()
aoc3 = do
    contents <- readFile "input"
    print $ findLifeSupply $ readInputCharMat contents