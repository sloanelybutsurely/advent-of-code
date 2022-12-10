module Aoc.Day2 (solve) where

parseInput :: String -> [[String]]
parseInput = map words . lines

-- A: rock, B: paper, C: scissors
scoreGame :: [String] -> Integer
scoreGame ["A", "A"] = 1 + 3
scoreGame ["A", "B"] = 2 + 6
scoreGame ["A", "C"] = 3 + 0
scoreGame ["B", "A"] = 1 + 0
scoreGame ["B", "B"] = 2 + 3
scoreGame ["B", "C"] = 3 + 6
scoreGame ["C", "A"] = 1 + 6
scoreGame ["C", "B"] = 2 + 0
scoreGame ["C", "C"] = 3 + 3
scoreGame _          = 0

totalScore :: [[String]] -> Integer
totalScore = sum . map scoreGame

solve :: Integer -> String -> String
solve 1 input = show $ totalScore games
  where
    games = map convertGuide $ parseInput input

    convertGuide [p, "X"] = [p, "A"]
    convertGuide [p, "Y"] = [p, "B"]
    convertGuide [p, "Z"] = [p, "C"]
    convertGuide xs       = xs

solve 2 input = show $ totalScore games
  where
    games = map solveGuide $ parseInput input

    -- X: loss, Y: draw, Z: win
    solveGuide ["A", "X"] = ["A", "C"]
    solveGuide ["A", "Y"] = ["A", "A"]
    solveGuide ["A", "Z"] = ["A", "B"]
    solveGuide ["B", "X"] = ["B", "A"]
    solveGuide ["B", "Y"] = ["B", "B"]
    solveGuide ["B", "Z"] = ["B", "C"]
    solveGuide ["C", "X"] = ["C", "B"]
    solveGuide ["C", "Y"] = ["C", "C"]
    solveGuide ["C", "Z"] = ["C", "A"]
    solveGuide xs         = xs

solve _ _ = "?"
