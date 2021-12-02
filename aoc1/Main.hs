module Main where

import System.IO
import Control.Monad
import Data.List (transpose, tails)

readInt :: String -> Int
readInt = read

countChangesTail :: [Int] -> Int -> Int
countChangesTail [] x = x
countChangesTail [_] x = x
countChangesTail (h:t:ts) x = if t > h then countChangesTail (t:ts) x + 1
    else countChangesTail (t:ts) x

countChanges :: [Int] -> Int
countChanges s = countChangesTail s 0

sumL :: [Int] -> Int
sumL = sum

countWindowTail :: [Int] -> Int -> Int
countWindowTail [] x = x
countWindowTail s x = do
    let window = take 3 s
    let nextWindow = tail $ take 4 s
    if sumL nextWindow > sumL window then countWindowTail (tail s) (x + 1)
    else countWindowTail (tail s) x

countWindow :: [Int] -> Int
countWindow s = countWindowTail s 0

main :: IO ()
main = do
    contents <- readFile "input"
    print $ countWindow (map readInt $ lines contents)
