#!/usr/bin/env runghc
module Day12.Part1 where

import Day12

main = interact (show . solve . parseProgram)

toList (a, b) = [a, b]

solve = run
