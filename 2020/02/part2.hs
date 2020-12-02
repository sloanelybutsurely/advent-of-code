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

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
(x:xs) !!? 0 = Just x
(_:xs) !!? idx = xs !!? (idx - 1)

maybeToBool (Just a) = a
maybeToBool Nothing = False

xor :: Bool -> Bool -> Bool
xor l r = l /= r

isValidPasswordEntry (PasswordEntry pos1 pos2 char pass) =
    (maybeToBool $ fmap (==char) char1) `xor`
    (maybeToBool $ fmap (==char) char2)
  where
    char1 = pass !!? (pos1 - 1)
    char2 = pass !!? (pos2 - 1)

