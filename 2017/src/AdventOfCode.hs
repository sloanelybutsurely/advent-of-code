module AdventOfCode where

import Data.Char
import Data.List
import Data.List.Unique (allUnique)
import Data.Maybe
import Data.Array

-- Day 1
pairs j xs = take (length xs) $ zip (cycle xs) $ drop j (cycle xs)
hash = sum . map fst . filter (uncurry (==))
captcha j = show . hash . pairs j . map digitToInt
captcha1 = captcha 1
captcha2 ds = captcha (length ds `div` 2) ds

-- Day 2
readCells :: String -> [[Int]]
readCells = map (map read) . map words . lines

minmax xs = (minimum xs, maximum xs)
checksum buff = show $ sum $ map (uncurry . flip $ (-)) . map minmax $ readCells buff

-- Day 2
calculateRow row = result
  where
    sorted = reverse . sort $ row
    result = fromJust $
      fmap (uncurry div) $
      find ((==) 0 . uncurry rem) [ (i, j) | i <- sorted, j <- tail sorted, j < i ]
sumEvenlyDivisibleValues buff = show $ sum . map calculateRow $ readCells buff


-- Day 3
chunk (b, t) =
  let seq = [(t-1), (t-2)..(b)] ++ [(b+1)..(t+1)] in
    concat . replicate 4 $ take (length seq - 1) seq
memory = [0,0,1,2,1,2,1,2,1,2] ++ (concatMap chunk $ zip [2..] [4,6..])
distanceToAddress buff = show $ memory !! (read buff)

-- Day 4
readDay4 :: String -> [[String]]
readDay4 = map words . lines

both fn gn x = fn x && gn x

validPassPhrase1 = allUnique
boolToInt True = 1
boolToInt False = 0

validPassPhrase2 = both allUnique noPermutations

noPermutations :: [String] -> Bool
noPermutations phrase = all noPermutations' phrase
  where
    noPermutations' word =
      all ((flip notElem) (filter ((/=) word) phrase)) (permutations word)

day41 buff = show $ sum . map (boolToInt . validPassPhrase1) $ readDay4 buff
day42 buff = show $ sum . map (boolToInt . validPassPhrase2) $ readDay4 buff

-- Day 5

type Program = (Array Int Int, Int)
data Solution = Partial Program Int | Complete Int

inBounds (a, b) x = a <= x && x <= b

-- Part 1
-- runProgram (Complete jumps) = jumps
-- runProgram (Partial (instructions, pointer) jumps) =
--   if inBounds bounds' next
--   then runProgram $ Partial ((instructions // update), next) jumps'
--   else runProgram $ Complete jumps'
--   where
--     instruction = instructions ! pointer
--     next = pointer + instruction
--     bounds' = bounds instructions
--     jumps' = jumps + 1
--     update = [(pointer, instruction + 1)]

-- Part 2
runProgram (Complete jumps) = jumps
runProgram (Partial (instructions, pointer) jumps) =
  if inBounds bounds' next
  then runProgram $ Partial ((instructions // update), next) jumps'
  else runProgram $ Complete jumps'
  where
    instruction = instructions ! pointer
    next = pointer + instruction
    bounds' = bounds instructions
    jumps' = jumps + 1
    updatedInstruction = if instruction > 2 then instruction - 1 else instruction + 1
    update = [(pointer, updatedInstruction)]

makeProgram xs = Partial (listArray (0, length xs - 1) xs, 0) 0

day51 = show . runProgram . makeProgram . map read . lines

