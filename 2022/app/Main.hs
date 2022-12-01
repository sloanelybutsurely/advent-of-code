{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment (getArgs)
import Data.Ord 
import Data.List (sortBy)
import Data.Text (pack, splitOn, unpack)
    
parseInput :: String -> [[Integer]]
parseInput = (map (map read . words . unpack)) . splitOn "\n\n" . pack

inputSums :: String -> [Integer]
inputSums = map sum . parseInput

part1 :: String -> Integer
part1 = maximum . inputSums

part2 :: String -> Integer 
part2 = sum . take 3 . sortDesc . inputSums
  where
    sortDesc = sortBy (comparing Down)

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents

  let f = case args of ("1":_) -> part1
                       ("2":_) -> part2
                       _ -> error "unknown part"

  putStrLn $ show $ f contents

