{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day1 (solve) where

import Data.Ord 
import Data.List (sortBy)
import Data.Text (pack, splitOn, unpack)

parseInput :: String -> [[Integer]]
parseInput = (map (map read . words . unpack)) . splitOn "\n\n" . pack

inputSums :: String -> [Integer]
inputSums = map sum . parseInput

part1 :: String -> Integer
part1 = maximum . inputSums

part2 :: String -> Integer 
part2 = sum . take 3 . sortDesc . inputSums
  where
    sortDesc = sortBy (comparing Down)

solve :: Integer -> String -> Integer
solve 1 input = part1 input
solve 2 input = part2 input
