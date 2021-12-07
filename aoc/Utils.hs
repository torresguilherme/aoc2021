module Utils where

import System.IO ()
import Data.Char (digitToInt)
import Data.List (foldl')

data InputString = InputNumber Int | InputCommand String
    deriving Show

type Point = (Int, Int)
type Aim = Int
type Transform = (Point, Aim)

readInt :: String -> Int 
readInt = read

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

invertBinary :: String -> String
invertBinary = map (\c -> if c == '0' then '1' else '0')

readInputTail :: [String] -> [InputString] -> [InputString]
readInputTail [] acc = acc
readInputTail [_] acc = acc
readInputTail (s:n:ts) acc = readInputTail ts (acc ++ [InputCommand s] ++ [InputNumber $ readInt n])

readInput :: [String] -> [InputString]
readInput s = readInputTail s []

readInputCharMat :: String -> [String]
readInputCharMat = lines