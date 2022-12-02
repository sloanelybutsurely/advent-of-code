module Aoc.Day2 (solve) where

solve 1 input = sum $ map scoreGame guide
  where
    guide = map words . lines $ input

    loss = 0
    draw = 3
    win  = 6

    rock     = 1
    paper    = 2
    scissors = 3

    scoreGame ["A", "X"] = rock + draw
    scoreGame ["A", "Y"] = paper + win
    scoreGame ["A", "Z"] = scissors + loss
    scoreGame ["B", "X"] = rock + loss
    scoreGame ["B", "Y"] = paper + draw
    scoreGame ["B", "Z"] = scissors + win
    scoreGame ["C", "X"] = rock + win
    scoreGame ["C", "Y"] = paper + loss
    scoreGame ["C", "Z"] = scissors + draw
