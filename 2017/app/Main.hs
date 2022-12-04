module Main where

import System.Environment
import AdventOfCode

unknown args = \_ -> "No program for arguments: " ++ show args

selectProgram ["1", "1"] = captcha1
selectProgram ["1", "2"] = captcha2
selectProgram ["2", "1"] = checksum
selectProgram ["2", "2"] = sumEvenlyDivisibleValues
selectProgram ["3", "1"] = distanceToAddress
selectProgram ["4", "1"] = day41
selectProgram ["4", "2"] = day42
selectProgram ["5", "1"] = day51
selectProgram args = unknown args

main = do
  args <- getArgs
  interact $ selectProgram args
