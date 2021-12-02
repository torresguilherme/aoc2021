module Main where

import System.IO
import Control.Monad

data InputString = InputNumber Int | InputCommand String
    deriving Show

type Point = (Int, Int)

readInt :: String -> Int 
readInt = read

readInputTail :: [String] -> [InputString] -> [InputString]
readInputTail [] acc = acc
readInputTail [_] acc = acc
readInputTail (s:n:ts) acc = readInputTail ts (acc ++ [InputCommand s] ++ [InputNumber $ readInt n])

readInput :: [String] -> [InputString]
readInput s = readInputTail s []

getResultTail :: [InputString] -> Point -> Point
getResultTail [] x = x
getResultTail [_] x = x
getResultTail (InputNumber _:_:_) x = x
getResultTail (InputCommand _:InputCommand _:_) x = x
getResultTail (InputCommand s : InputNumber n : ts) x 
    | s == "forward" = getResultTail ts (fst x + n, snd x)
    | s == "up" = getResultTail ts (fst x, snd x - n)
    | s == "down" = getResultTail ts (fst x, snd x + n)
    | otherwise = getResultTail ts x

getResult :: [InputString] -> Point
getResult s = getResultTail s (0, 0)

multiplyResult :: Point -> Int
multiplyResult = uncurry (*)

main :: IO ()
main = do
    contents <- readFile "input"
    print $ multiplyResult $ getResult $ readInput $ words contents
