{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment (getArgs)
import qualified Aoc.Day1
import qualified Aoc.Day2
import qualified Aoc.Day3

main :: IO ()
main = do
  (dayS:partS:_) <- getArgs
  let day = read dayS :: Integer
  let part = read partS :: Integer

  contents <- readFile ("input/" ++ (show day) ++ ".txt")

  let f = case day of 1 -> Aoc.Day1.solve
                      2 -> Aoc.Day2.solve
                      3 -> Aoc.Day3.solve
                      _ -> error "unknown day"

  putStrLn $ show $ f part contents

