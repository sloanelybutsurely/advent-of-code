module Day12 ( Action
             , Inst
             , action
             , value
             , Program
             , parseProgram
             , State
             , position
             , heading
             , start
             , run
             ) where

import Data.Char
import Text.ParserCombinators.ReadP

data Action = N | S | E | W | L | R | F
  deriving (Eq, Show)

data Inst = Inst { action :: Action, value :: Int }
  deriving (Eq, Show)

inst :: ReadP Inst
inst = do
  a <- action'
  v <- int
  return $ Inst a v
    where
      int :: ReadP Int
      int = do
        digits <- munch1 isDigit
        return $ read digits
      action' :: ReadP Action
      action' = do
        a <- choice $ map char "NSEWLRF"
        return $ case a of
          'N' -> N
          'S' -> S
          'E' -> E
          'W' -> W
          'L' -> L
          'R' -> R
          'F' -> F

type Program = [Inst]

program :: ReadP Program
program = inst `sepBy1` skipSpaces

parse p s = parsedResult $ readP_to_S p s
  where
    parsedResult [(a, _)] = a
    parsedResult (a:as)    = parsedResult as
    parsedResult _         = error "Parser error"

parseProgram = parse program

data State = State { position :: (Int, Int)
                   , heading :: Action -- This isn't "correct"
                   } deriving (Eq, Show)

start = State (0, 0) E

updatePosition :: State -> ((Int -> Int), (Int -> Int)) -> State
updatePosition s (deltaX, deltaY) = State np (heading s)
  where
    (x, y) = position s
    np = (deltaX x, deltaY y)


updateHeading :: State -> Action -> State
updateHeading s nh = State (position s) nh

travel :: State -> Inst -> State
travel s i = updatePosition s $ case i of
                                  (Inst N d) -> (id, (+) d)
                                  (Inst E d) -> ((+) d, id)
                                  (Inst S d) -> (id, (-) d)
                                  (Inst W d) -> ((-) d, id)

headings = cycle [N, E, S, W]
rotate h (Inst d v) = headings !! (v `div` 90)
  where
    headings = dropWhile (/= h) $ cycle $ case d of
                                            R -> [N, E, S, W]
                                            L -> [N, W, S, E]

tick :: State -> Inst -> State
tick s i@(Inst a v)
  | a == N || a == S || a == E || a == W = travel s i
  | a == L || a == R = updateHeading s $ rotate (heading s) i
  | a == F = travel s (Inst (heading s) v)

run :: Program -> State
run = foldl tick start
