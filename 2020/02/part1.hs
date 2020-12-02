#!/usr/bin/env runghc

import Data.Char

data PasswordEntry = PasswordEntry { min :: Int, max :: Int, character :: Char, password :: String }
  deriving (Show)

-- This could be a lot better...
instance Read PasswordEntry where
  readsPrec _ input =
    let (min', rest) = span isDigit input
        min = read min' :: Int
        (_, rest') = splitAt 1 rest
        (max', rest'') = span isDigit rest'
        max = read max' :: Int
        (_:char:_:_:password) = rest''
      in
      [(PasswordEntry min max char password, "")]


main = interact solve

solve = show . length . filter (isValidPasswordEntry . readPasswordEntry) . lines

readPasswordEntry :: String -> PasswordEntry
readPasswordEntry = read

isValidPasswordEntry (PasswordEntry min max char pass) =
  let countedElem = countElem char pass
   in min <= countedElem && max >= countedElem

countElem _ [] = 0
countElem a (x:xs)
  | a == x = 1 + countElem a xs
countElem a (_:xs) = countElem a xs
