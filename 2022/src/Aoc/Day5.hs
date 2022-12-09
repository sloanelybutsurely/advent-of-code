module Aoc.Day5 (solve) where

import Aoc (runReadP)
import Data.Char (isDigit)
import Data.List (transpose)
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IntMap
import Text.ParserCombinators.ReadP

data Crate = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Read, Show)
type Stack = [Crate]
type State = IntMap Stack

data Move = Move { amount :: Int, from :: Int, to :: Int }
  deriving (Show)
type Procedure = [Move]

data Input = Input { state :: State, procedure :: Procedure }
  deriving (Show)

parseMove :: String -> Move
parseMove = runReadP moveP
  where
    intP :: ReadP Int
    intP = do
      digits <- munch1 isDigit
      return $ read digits

    moveP :: ReadP Move
    moveP = do
      _ <- string "move "
      amount' <- intP
      _ <- string " from "
      from' <- intP
      _ <- string " to "
      to' <- intP
      return $ Move { amount = amount', from = from', to = to' }

parseInput :: String -> Input
parseInput input = Input { state = state, procedure = procedure }
  where
    -- i hate this
    stateContents = concat $ transpose $ takeWhile (/= "") $ lines input
    parseState "" _ state = IntMap.map reverse state
    parseState (x:xs) acc state
      | x == '[' || x == ']' || x == ' ' = parseState xs acc state
      | isDigit x                        = parseState xs [] $ IntMap.insert (read [x]) acc state
      | otherwise                        = parseState xs ((read [x]):acc) state
    state = parseState stateContents [] IntMap.empty

    procedureContents = drop 1 $ dropWhile (/= "") $ lines input
    procedure = map parseMove procedureContents

doMove :: State -> Move -> State
doMove stacks (Move n x y) = IntMap.insert x newX (IntMap.insert y newY stacks)
  where
    (toPush, newX) = splitAt n (stacks ! x)
    newY = reverse toPush ++ stacks ! y

-- no reverse
doMove' :: State -> Move -> State
doMove' stacks (Move n x y) = IntMap.insert x newX (IntMap.insert y newY stacks)
  where
    (toPush, newX) = splitAt n (stacks ! x)
    newY = toPush ++ stacks ! y

stackLabel :: Stack -> String
stackLabel [] = ""
stackLabel (x:_) = show x

message :: State -> String
message s = concat $ map stackLabel $ IntMap.elems s

solve :: Integer -> String -> String
solve 1 input = message $ foldl doMove state' procedure'
  where
    input' = parseInput input
    state' = state input'
    procedure' = procedure input'

solve 2 input = message $ foldl doMove' state' procedure'
  where
    input' = parseInput input
    state' = state input'
    procedure' = procedure input'

solve _ _ = "?"
