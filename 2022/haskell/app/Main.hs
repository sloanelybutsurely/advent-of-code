{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment (getArgs)
import qualified Aoc.Day1
import qualified Aoc.Day2
import qualified Aoc.Day3
import qualified Aoc.Day4
import qualified Aoc.Day5
import qualified Aoc.Day6

main :: IO ()
main = do
  (dayS:partS:_) <- getArgs
  let day = read dayS :: Integer
  let part = read partS :: Integer

  contents <- readFile ("input/" ++ (show day) ++ ".txt")

  let f = case day of 1 -> Aoc.Day1.solve
                      2 -> Aoc.Day2.solve
                      3 -> Aoc.Day3.solve
                      4 -> Aoc.Day4.solve
                      5 -> Aoc.Day5.solve
                      6 -> Aoc.Day6.solve
                      _ -> error "unknown day"

  putStrLn $ f part contents

