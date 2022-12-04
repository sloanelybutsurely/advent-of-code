module AdventOfCode.Day6 (findFirstDuplicateAllocation, findSizeOfLoopOfDuplicateAllocation) where

import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set


type Memory = [Int]

set i y xs = set' i y xs
  where
    set' i y []     = []
    set' 0 y (_:xs) = y:xs
    set' i y (x:xs) = x:set' (i-1) y xs

over f i xs = set i (f (xs !! i)) xs
incrementIdx = over ((+) 1)


distribute :: Int -> Int -> Memory -> Memory
distribute 0 _ ms = ms
distribute x i ms = distribute (x-1) (cyclicIdx+1) (incrementIdx cyclicIdx ms)
  where cyclicIdx = i `mod` (length ms)

reallocate :: Memory -> Memory
reallocate ms = distribute max (maxIdx+1) zeroed
  where
    max = maximum ms
    maxIdx = fromJust . findIndex ((==) max) $ ms
    zeroed = set maxIdx 0 ms

findFirstDuplicateAllocation :: Memory -> Int
findFirstDuplicateAllocation ms = length $ takeWhile (uncurry (/=)) zipped
  where
    allocations = iterate reallocate ms
    sets = scanl (flip Set.insert) Set.empty allocations
    sizes = map Set.size sets
    zipped = zip sizes (tail sizes)


findSizeOfLoopOfDuplicateAllocation :: Memory -> Int
findSizeOfLoopOfDuplicateAllocation ms = (+) 1 $ length $ takeWhile ((/=) $ head allocationsFromFirstDuplication) $ tail allocationsFromFirstDuplication
  where
    allocations = iterate reallocate ms
    firstDuplicationIdx = findFirstDuplicateAllocation ms
    allocationsFromFirstDuplication = drop firstDuplicationIdx allocations

