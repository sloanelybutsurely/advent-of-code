#!/usr/bin/env runghc

import Day10
import Data.List

main = interact (show . solve . parse)

solve xs = ones * threes
  where
    sorted = 0:(sort xs)
    differences = zipWith (flip (-)) sorted (tail sorted)
    ones = count 1 differences
    threes = count 3 differences + 1

count x = length . filter (==x)
