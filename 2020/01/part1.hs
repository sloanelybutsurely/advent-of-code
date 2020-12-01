#!/usr/bin/env runghc

import Data.List
import Data.Maybe

main :: IO ()
main =
  do
    input <- getInput

    let answer = uncurry (*) $ fromJust $ find isSolutionPair $ pairs input

    putStrLn $ show answer

getInput :: IO [Int]
getInput = fmap (map read . lines) getContents

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x:xs) = [(x, y) | y <- xs] ++ pairs xs

isSolutionPair :: (Int, Int) -> Bool
isSolutionPair (a, b) = a + b == 2020
