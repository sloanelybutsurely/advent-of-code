#!/usr/bin/env runghc

import Data.List
import Data.Maybe

main = interact findSolution

findSolution = show . product . findJust isSolutionPair . pairs . parseInput

parseInput = map read . lines

pairs = sequence . replicate 2

isSolutionPair = (2020 ==) . sum

findJust f = fromJust . find f
