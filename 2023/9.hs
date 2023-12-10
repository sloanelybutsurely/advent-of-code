#!/usr/bin/env runhaskell

parseInput :: String -> [[Integer]]
parseInput = map (map read . words) . lines

solve str = show (part1', part2')
  where
    input = parseInput str
    part1' = part1 input
    part2' = part2 input

part1 xs = sum $ map next xs
part2 xs = sum $ map prev xs

deltas xs = zipWith (flip (-)) xs $ tail xs
differentiate xs = takeWhile notAllZeros $ iterate deltas xs
  where
    notAllZeros xs = not $ all (== 0) xs

next xs = sum $ map last $ differentiate xs

prev xs = foldl1 (flip (-)) $ reverse $ map head $ differentiate xs

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ solve contents
