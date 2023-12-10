#!/usr/bin/env runhaskell

import Control.Monad (ap)

parseInput :: String -> [[Integer]]
parseInput = map (map read . words) . lines

solve str = show (part1', part2')
  where
    input = parseInput str
    part1' = part1 input
    part2' = part2 input

part1 = sum . map next
part2 = sum . map prev

deltas = ap (zipWith subtract) tail
differentiate = takeWhile notAllZeros . iterate deltas
  where
    notAllZeros = not . all (0 ==)

next = sum . map last . differentiate

prev = foldl1 subtract . reverse . map head . differentiate

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ solve contents
