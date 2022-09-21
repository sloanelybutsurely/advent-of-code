#!/usr/bin/env runghc

module Day13.Part1 where

ts = 1000434
bids = [17,41,983,29,19,23,397,37,13]

waitTime ts bid = (ts `div` bid) * bid + bid - ts

minimumBy fn (inp:inps) = minimumBy' fn inps inp
  where
    minimumBy' _ [] curr = curr
    minimumBy' fn (x:xs) curr
      | fn curr < fn x = minimumBy' fn xs curr
      | otherwise = minimumBy' fn xs x

main = putStrLn (show $ fst $ minimumBy snd $ zip bids $ map (waitTime ts) bids)
