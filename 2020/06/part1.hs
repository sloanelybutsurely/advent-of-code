#!/usr/bin/env runghc

import Day6
import Data.Set (fromList, size)

main = interact (show . solve . parse)

solve = sum . map size

parse = map fromList . lines'
