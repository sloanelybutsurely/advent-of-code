module Day6 (lines') where

lines' = joinLines . lines

joinLines = foldl parse [[]]
  where 
    parse [[]] s = [[s]]
    parse [a] "" = [[], a]
    parse [a] s = [a ++ [s]]
    parse (a:as) s = parse [a] s ++ as
