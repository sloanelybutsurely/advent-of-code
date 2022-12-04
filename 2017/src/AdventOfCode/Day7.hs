module AdventOfCode.Day7 (readInput) where


readInput = map readLine . lines
  where
    normalize = words . (filter $ flip notElem $ "()->,")
    parse :: [String] -> (String, Int, [String])
    parse [name, weightStr] = (name, read weightStr, [])
    parse (name:weightStr:children) = (name, read weightStr, children)
    readLine = parse . normalize
    

