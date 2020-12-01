#!/usr/bin/env runghc

import Data.List
import Data.Maybe

main :: IO ()
main =
  do
    input <- getInput

    let answer = product $ fromJust $ find isSolutionTriple $ triples input

    putStrLn $ show answer

getInput :: IO [Int]
getInput = fmap (map read . lines) getContents

triples :: [a] -> [[a]]
triples xs = sequence $ replicate 3 xs

isSolutionTriple :: [Int] -> Bool
isSolutionTriple triple = sum triple == 2020

multplyTriple (a, b, c) = a * b * c
