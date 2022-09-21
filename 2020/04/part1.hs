#!/usr/bin/env runghc

  -- byr (Birth Year)
  -- iyr (Issue Year)
  -- eyr (Expiration Year)
  -- hgt (Height)
  -- hcl (Hair Color)
  -- ecl (Eye Color)
  -- pid (Passport ID)
  -- cid (Country ID)

import Data.Map.Strict (fromList, member)


isValidEntry ("byr", _) = True
isValidEntry ("iyr", _) = True
isValidEntry ("eyr", _) = True
isValidEntry ("hgt", _) = True
isValidEntry ("hcl", _) = True
isValidEntry ("ecl", _) = True
isValidEntry ("pid", _) = True
isValidEntry ("cid", _) = True
isValidEntry _ = False

toEntry = fmap tail . break (==':') 

main = interact (solve . getMaps)

solve = show . length . filter isValidDocument

isValidDocument document =
  member "byr" document &&
  member "iyr" document &&
  member "eyr" document &&
  member "hgt" document &&
  member "hcl" document &&
  member "ecl" document &&
  member "pid" document

getMaps = map (fromList . filter isValidEntry . map toEntry . words) . joinLines . lines

joinLines = foldl parse [""]
  where 
    parse [""] l = [l]
    parse [a] "" = ["", a]
    parse [a] l = [l ++ " " ++ a]
    parse (a:as) l = parse [a] l ++ as

