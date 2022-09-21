#!/usr/bin/env runghc

import Data.List
import Data.Maybe

main = interact findSolution

findSolution = show . product . findJust isSolutionTriple . triples . parseInput

parseInput = map read . lines 

triples = sequence . replicate 3

isSolutionTriple = (2020 ==) . sum

findJust f = fromJust . find f
