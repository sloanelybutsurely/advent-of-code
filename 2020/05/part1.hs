#!/usr/bin/env runghc

import Day5

main = interact solve

solve = show . maxSeatId . map (read :: String -> BoardingPass) . lines

maxSeatId = maximum . map seatId
