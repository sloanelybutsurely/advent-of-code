#!/usr/bin/env runghc

import qualified Data.Set as S
import Data.List (subsequences)
import Data.Either (fromLeft, lefts)
import Day9

main = interact (show . solve . parse)

solve = head . lefts . verify 25

verify :: Int -> [Int] -> [Either Int Int]
verify len xs
  | length xs < len + 1 = []
  | otherwise = verified:(verify len $ tail xs)
    where
      (preamble, [x]) = splitAt len $ take (len + 1) xs
      pairs = combinations 2 preamble
      sums = S.fromList $ map sum pairs
      verified = if S.member x sums then (Right x) else (Left x)

combinations n xs = filter ((n==) . length) $ subsequences xs
