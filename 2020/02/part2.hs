#!/usr/bin/env runghc

import Data.Char

data PasswordEntry = PasswordEntry { pos1 :: Int, pos2 :: Int, char :: Char, password :: String }
  deriving (Show)

-- This could be a lot better...
instance Read PasswordEntry where
  readsPrec _ input =
    let (pos1', rest) = span isDigit input
        pos1 = read pos1' :: Int
        (_, rest') = splitAt 1 rest
        (pos2', rest'') = span isDigit rest'
        pos2 = read pos2' :: Int
        (_:char:_:_:password) = rest''
      in
      [(PasswordEntry pos1 pos2 char password, "")]


main = interact solve

solve = show . length . filter (isValidPasswordEntry . readPasswordEntry) . lines

readPasswordEntry :: String -> PasswordEntry
readPasswordEntry = read

xor :: Bool -> Bool -> Bool
xor l r = l /= r

isValidPasswordEntry (PasswordEntry pos1 pos2 char pass) =
    (charAtPos1 == char) `xor` (charAtPos2 == char)
  where
    charAtPos1 = pass !! (pos1 - 1)
    charAtPos2 = pass !! (pos2 - 1)

