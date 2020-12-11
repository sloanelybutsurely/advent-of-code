module Day11 where

import Data.Array

data Cell = Empty | Occupied | Floor
  deriving (Eq, Show)

isCell 'L' = True
isCell '#' = True
isCell '.' = True
isCell _ = False

toCell 'L' = Empty
toCell '#' = Occupied
toCell '.' = Floor

instance Read Cell where
  readsPrec _ (c:rest) = [(toCell c, rest)]
  readList input = [(map toCell cells, rest)]
    where (cells, rest) = span isCell input

toChar Empty = 'L'
toChar Occupied = '#'
toChar Floor = '.'

type State = Array (Int, Int) Cell

parse :: String -> State
parse input = listArray ((1, 1), (width, height)) $ concat lists
  where
    lists :: [[Cell]]
    lists = map read $ lines input
    width = length $ head lists
    height = length lists

render :: State -> String
render state = unlines chunks
  where
    (_, (width, _)) = bounds state
    chars = map toChar $ elems state
    chunks = chunk width chars


chunk n [] = []
chunk n xs = curr:(chunk n rest)
  where
    (curr, rest) = splitAt n xs

n (x, y) = (x, y-1)
s (x, y) = (x, y+1)
w (x, y) = (x-1, y)
e (x, y) = (x+1, y)
ne = n . e
nw = n . w
se = s . e
sw = s . w

directions x = map ($ x) [n, ne, e, se, s, sw, w, nw]

(!?) arr ix =
  if inRange (bounds arr) ix
     then Just (arr ! ix)
     else Nothing

tick :: State -> State
tick curr = array (bounds curr) $ map nextCell $ assocs curr
  where
    occupiedAdjacent ix =
      length $ filter (== (Just Occupied)) $ map ((!?) curr) $ directions ix
    nextCell c@(ix, Empty)
      | occupiedAdjacent ix == 0 = (ix, Occupied)
    nextCell c@(ix, Occupied)
      | occupiedAdjacent ix >= 4 = (ix, Empty)
    nextCell x = x
