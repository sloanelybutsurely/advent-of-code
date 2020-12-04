#!/usr/bin/env runghc

  -- byr (Birth Year)
  -- iyr (Issue Year)
  -- eyr (Expiration Year)
  -- hgt (Height)
  -- hcl (Hair Color)
  -- ecl (Eye Color)
  -- pid (Passport ID)
  -- cid (Country ID)

import Data.Char
import Data.Map.Strict (fromList, member)


isValidEntry ("byr", byr)
  | byr >= "1920" && byr <= "2002" = True
  | otherwise = False
isValidEntry ("iyr", iyr)
  | iyr >= "2010" && iyr <= "2020" = True
  | otherwise = False
isValidEntry ("eyr", eyr)
  | eyr >= "2020" && eyr <= "2030" = True
  | otherwise = False
isValidEntry ("hgt", hgt@(a:b:c:"cm")) = hgt >= "150cm" && hgt <= "193cm"
isValidEntry ("hgt", hgt@(a:b:"in")) = hgt >= "59in" && hgt <= "76in"
isValidEntry ("hcl", '#':hcl) = length hcl == 6 && all isHexDigit hcl
isValidEntry ("ecl", "amb") = True
isValidEntry ("ecl", "blu") = True
isValidEntry ("ecl", "brn") = True
isValidEntry ("ecl", "gry") = True
isValidEntry ("ecl", "grn") = True
isValidEntry ("ecl", "hzl") = True
isValidEntry ("ecl", "oth") = True
isValidEntry ("pid", pid) = length pid == 9 && all isDigit pid
isValidEntry ("cid", _) = True -- ignored
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

