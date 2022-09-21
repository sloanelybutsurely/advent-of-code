#!/usr/bin/env runghc


import Day6
import Data.Bits
import Data.Char

main = interact (show . solve . lines')

solve = sum . map countEveryoneYes

charToBit c = bit $ ord c - ord 'a'

stringToBits :: String -> Int
stringToBits str = foldl1 (.|.) $ map charToBit str

countEveryoneYes group = popCount $ foldl1 (.&.) $ map stringToBits group
