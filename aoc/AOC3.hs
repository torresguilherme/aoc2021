module AOC3 where

import Utils (readInputCharMat, toDec, invertBinary)
import Data.List (transpose)

numberOfZeros :: [Char] -> Int
numberOfZeros = length . filter (== '0')

collectMostCommonBits :: [Char] -> Char
collectMostCommonBits list = if numberOfZeros list > div (length list) 2 then '0'
    else '1'

findMostCommonBits :: [[Char]] -> [Char]
findMostCommonBits list = map collectMostCommonBits $ transpose list

sumPower :: [[Char]] -> Int
sumPower s = toDec (findMostCommonBits s) * toDec (invertBinary $ findMostCommonBits s)

aoc3 :: IO ()
aoc3 = do
    contents <- readFile "input"
    print $ sumPower $ readInputCharMat contents