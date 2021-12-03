module AOC1 where

import System.IO
import Control.Monad

readInt :: String -> Int
readInt = read

countChanges :: [Int] -> Int
countChanges xs = length . filter (< 0) $ zipWith (-) xs (tail xs)

diffWindow :: [Int] -> [Int] -> Int
diffWindow a b = sum a - sum b

makeWindows :: [Int] -> [[Int]]
makeWindows (a:b:c:ts) = [a, b, c] : makeWindows (b:c:ts)
makeWindows _ = []

countWindow :: [Int] -> Int
countWindow xs = countChanges . map sum $ makeWindows xs

aoc1 :: IO ()
aoc1 = do
    contents <- readFile "input"
    print $ countWindow $ map readInt $ lines contents
