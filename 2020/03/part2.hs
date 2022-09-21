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

routes = [(1, 1)
         ,(3, 1)
         ,(5, 1)
         ,(7, 1)
         ,(1, 2)
         ]

solve rows = product $ map countTrees routes
  where
    board = toArray $ map toArray rows
    (_, y) = fmap (+1) $ bounds board
    (_, x) = fmap (+1) $ bounds $ board ! 0
    xIdx i = mod i x
    yIdx i = mod i y
    at (x', y') = (board ! (yIdx y')) ! (xIdx x')
    path (deltaX, deltaY) = zip [0,deltaX..] [0,deltaY..y]
    countTrees route = count (==Tree) $ map at $ path route

count predicate xs = length $ filter predicate xs

toArray l = listArray (0, length l - 1) l
