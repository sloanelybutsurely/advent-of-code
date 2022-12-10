module Aoc.Day6 (solve) where

import Data.List (nub, tails)

-- https://stackoverflow.com/questions/27726739/implementing-an-efficient-sliding-window-algorithm-in-haskell#comment43882101_27733778
windows :: Int -> [a] -> [[a]]
windows n = foldr (zipWith (:)) (repeat []) . take n . tails

sequenceOffset :: Eq a => Int -> [a] -> Int
sequenceOffset len xs = len + (length $ takeWhile dupes $ windows len xs)
  where
    dupes ys = length ys > (length $ nub ys)

solve :: Integer -> String -> String
solve 1 input = show $ sequenceOffset 4 input
solve 2 input = show $ sequenceOffset 14 input

solve _ _ = "?"
