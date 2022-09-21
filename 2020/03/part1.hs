#!/usr/bin/env runghc

import Data.Array

data Cell = Open | Tree
  deriving (Eq)

charToCell '.' = Open
charToCell '#' = Tree

isValidCell '.' = True
isValidCell '#' = True
isValidCell _ = False

instance Read Cell where
  readsPrec _ (c:rest) = [(charToCell c, rest)]
  readList input =
    let (valid, rest) = span isValidCell input
     in [(map charToCell valid, rest)]

cellToChar Open = '.'
cellToChar Tree = '#'

instance Show Cell where
  show cell = [cellToChar cell]
  showList = (++) . map cellToChar

main = interact (show . solve . map (read :: String -> [Cell]) . lines)

solve rows = count (==Tree) $ map at path
  where
    board = toArray $ map toArray rows
    (_, y) = fmap (+1) $ bounds board
    (_, x) = fmap (+1) $ bounds $ board ! 0
    xIdx i = mod i x
    yIdx i = mod i y
    deltaY = 1
    deltaX = 3
    at (x', y') = (board ! (yIdx y')) ! (xIdx x')
    path = [(i*3, i) | i <- [0..y]]

count predicate xs = length $ filter predicate xs

toArray l = listArray (0, length l - 1) l
