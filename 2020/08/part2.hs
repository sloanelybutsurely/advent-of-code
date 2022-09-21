#!/usr/bin/env runghc
module Day8.Part2 where

import Data.Array
import Data.List (find)
import Data.Maybe (fromJust)
import Day8

main = interact (show . acc . fromJust . find (not . looped) . map run . flips . parse program)

flips :: Program -> [Program]
flips pgrm = (pgrm:(flips' pgrm 0))

flips' pgrm ix
  | outOfBounds pgrm ix = []
  | otherwise = (pgrm // [(ix, flipped)]):(flips' pgrm (ix + 1))
    where
      flipped = case (pgrm ! ix) of
        (Nop a) -> Jmp a
        (Jmp a) -> Nop a
        x -> x
