#!/usr/bin/env runghc
module Day11.Part1 where

import Day11
import Data.List (find)
import Data.Array (elems)

main = interact (show . solve . parse)

-- unlines $ map (render . snd) $ takeWhile (not . isUnchanged) ticks
solve start = length $ filter (== Occupied) $ elems stasis
  where
    simulation = iterate tick start
    ticks = zip simulation (tail simulation)
    isUnchanged = uncurry (==)
    (Just (stasis, _)) = find isUnchanged ticks
