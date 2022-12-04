module Aoc.Day4 (solve) where

import Aoc (runReadP)
import Data.Char
import Text.ParserCombinators.ReadP

type Range = (Int, Int)
type Row = (Range, Range)
type Input = [Row]

parse :: String -> Input
parse = map (runReadP rowP) . lines
  where
    intP :: ReadP Int
    intP = do
      digits <- munch1 isDigit
      return $ read digits

    rangeP :: ReadP Range
    rangeP = do
      l <- intP
      _ <- char '-'
      r <- intP
      return (l, r)

    rowP :: ReadP Row
    rowP = do
      a <- rangeP
      _ <- char ','
      b <- rangeP
      return (a, b)

fullOverlap :: Row -> Bool
fullOverlap ((a, b), (c, d))
  | a <= c && b >= d = True
  | c <= a && d >= b = True
  | otherwise        = False

overlap :: Row -> Bool
overlap ((a, b), (c, d))
  | b < c     = False -- first is fully to the left of second
  | d < a     = False -- first is fully to the right of second
  | otherwise = True  -- there's some overlap

solve :: Integer -> String -> Integer
solve 1 input = fromIntegral $ length $ filter fullOverlap $ parse input

solve 2 input = fromIntegral $ length $ filter overlap $ parse input

solve _ _ = 0
