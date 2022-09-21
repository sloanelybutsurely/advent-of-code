#!/usr/bin/env runghc

import Day6
import Data.Set (fromList, size)

main = interact (show . solve . parse)

solve = sum . map countEveryoneYes

parse = lines'

countEveryoneYes :: [String] -> Int
countEveryoneYes group =
    length $ filter everyoneYes questions
  where
    n = length group
    questions = ['a'..'z']
    everyoneYes q = all (elem q) group
