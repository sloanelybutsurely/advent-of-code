module Aoc.Day3 (solve, halve, shared, charToInt) where

import Data.Char
import qualified Data.Set as Set
import Data.List.Split (chunksOf)

halve :: String -> [String]
halve xs = [front, back]
  where
    midpoint = length xs `div` 2

    front = take midpoint xs
    back  = drop midpoint xs

shared :: [String] -> String
shared xs = Set.toList $ foldr1 Set.intersection $ map Set.fromList xs

charToInt :: Char -> Int
charToInt c
  | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
  | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27
  | otherwise            = -1

solve :: Integer -> String -> String
solve 1 input = show $ sum $ map (toInteger . charToInt . head . shared . halve) $ lines input

solve 2 input = show $ sum $ map (toInteger . charToInt . head . shared) $ chunksOf 3 $ lines input

solve _ _ = "?"
