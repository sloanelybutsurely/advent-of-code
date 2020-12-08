module Day8 where

import Data.Array
import Data.Char
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

signedInt :: ReadP Int
signedInt = do
  sign <- char '+' <++ char '-'
  digits <- munch1 isDigit
  return $ value sign digits
  where
    value '+' = read
    value '-' = negate . read

data Inst = Acc Int | Jmp Int | Nop Int
  deriving (Show)

inst :: ReadP Inst
inst = do
  op <- choice $ map string ["acc", "jmp", "nop"]
  _ <- skipSpaces
  arg <- signedInt
  return $ case op of
    "acc" -> Acc arg
    "jmp" -> Jmp arg
    "nop" -> Nop arg

type Program = Array Int Inst

program :: ReadP Program
program = do
  insts <- inst `sepBy` char '\n'
  _ <- skipSpaces
  _ <- eof
  let bnds = (0, length insts - 1)
  return $ listArray bnds insts

parse p s = unwrap $ readP_to_S p s
  where
    unwrap [(a, "")] = a
    unwrap [(_, rs)] = error "Parser did not consume entire stream."
    unwrap (a:as)    = unwrap as
    unwrap _         = error "Parser error"

data State = State { acc :: Int, sp :: Int, visits :: S.Set Int, looped :: Bool }
  deriving (Show)

start = State 0 0 S.empty False

outOfBounds arr idx = idx < l || idx > r
  where (l, r) = bounds arr

run pgrm = run' pgrm start

run' pgrm state
  | outOfBounds pgrm $ sp state = state
  | S.member (sp state) (visits state) = State (acc state) (sp state) (visits state) True
  | otherwise = run' pgrm state'
      where
        state' = State acc' sp' visits' False
        inst = pgrm ! (sp state)
        acc' = case inst of
                 (Acc arg) -> (acc state + arg)
                 _ -> acc state
        sp' = case inst of
                (Jmp arg) -> (sp state + arg)
                _ -> (sp state + 1)
        visits' = S.insert (sp state) (visits state)
