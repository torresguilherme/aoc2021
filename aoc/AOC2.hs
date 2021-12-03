module AOC2 where

import Control.Monad ()
import Utils ( Transform, Point, InputString(..), readInput )

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

getResultAimedTail :: [InputString] -> Transform -> Transform
getResultAimedTail [] x = x
getResultAimedTail [_] x = x
getResultAimedTail (InputNumber _:_:_) x = x
getResultAimedTail (InputCommand _:InputCommand _:_) x = x
getResultAimedTail (InputCommand s : InputNumber n : ts) x 
    | s == "forward" = getResultAimedTail ts ((fst (fst x) + n, snd (fst x) + (snd x * n)), snd x)
    | s == "up" = getResultAimedTail ts (fst x, snd x - n)
    | s == "down" = getResultAimedTail ts (fst x, snd x + n)
    | otherwise = getResultAimedTail ts x

getResultAimed :: [InputString] -> Transform
getResultAimed s = getResultAimedTail s ((0, 0), 0)

multiplyResult :: Point -> Int
multiplyResult = uncurry (*)

aoc2 :: IO ()
aoc2 = do
    contents <- readFile "input"
    print $ multiplyResult $ fst $ getResultAimed $ readInput $ words contents
