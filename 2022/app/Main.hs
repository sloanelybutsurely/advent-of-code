{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.List (sort)
import Data.Text (Text, pack, splitOn, unpack)
    

part1 :: String -> Integer
part1 str = findMax lines' [] 0
  where
    lines' = lines str
    findMax [] acc cMax = max (sum acc) cMax
    findMax ("":xs) acc cMax = findMax xs [] $ max (sum acc) cMax
    findMax (x:xs) acc cMax = findMax xs ((read x):acc) cMax

chunkElves :: Text -> [Integer]
chunkElves t = integers
  where
    textChunks = splitOn "\n\n" t
    integerChunks = map (map read . words . unpack) textChunks
    integers = map sum integerChunks

part2 :: String -> Integer 
part2 str = sum $ take 3 $ reverse $ sort $ chunkElves $ pack str

main :: IO ()
main = do
  contents <- getContents

  putStrLn $ show $ part2 contents

