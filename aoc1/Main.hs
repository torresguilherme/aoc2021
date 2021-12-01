module Main where

import System.IO
import Control.Monad

readInt :: String -> Int
readInt = read

countChangesTail :: [Int] -> Int -> Int
countChangesTail [] x = x
countChangesTail [_] x = x
countChangesTail (h:t:ts) x = if t > h then countChangesTail (t:ts) x + 1
    else countChangesTail (t:ts) x

countChanges :: [Int] -> Int
countChanges s = countChangesTail s 0

main :: IO ()
main = do
    contents <- readFile "input"
    print $ countChanges (map readInt $ lines contents)
