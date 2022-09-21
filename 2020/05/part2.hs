#!/usr/bin/env runghc

import Day5
import Data.List

main = interact solve

solve = show . mySeat . map (seatId . read) . lines

mySeat input = mySeat'
  where
    boardingPasses = sort input
    start = head boardingPasses
    mySeat' = fst . head . dropWhile (uncurry (==)) $ zip [start..] boardingPasses
