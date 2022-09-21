module Day10 where

parse :: String -> [Int]
parse = map read . lines
