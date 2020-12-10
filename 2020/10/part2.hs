#!/usr/bin/env runghc

import Day10
import Data.Array
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

main = interact (show . solve . parse)

countPaths inp = pathsFrom start
  where
    start = IntSet.findMax inpSet + 3

    pathsFrom 0 = 1
    pathsFrom x
      | x < 0 = 0
      | IntSet.member x allowed =
          sum $ map ((!) memo) $ filter (>= 0) $ map ((-) x) [1, 2, 3]
      | otherwise = 0

    inpSet = IntSet.fromList inp
    allowed = IntSet.insert start inpSet

    memo = listArray bnds
      [ pathsFrom i | i <- range bnds ]
    bnds = (0, start)

solve = countPaths
