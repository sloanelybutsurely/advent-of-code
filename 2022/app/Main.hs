module Main (main) where
    

part1 :: String -> Integer
part1 str = findMax lines' [] 0
  where
    lines' = lines str
    findMax [] acc cMax = max (sum acc) cMax
    findMax ("":xs) acc cMax = findMax xs [] $ max (sum acc) cMax
    findMax (x:xs) acc cMax = findMax xs ((read x):acc) cMax

main :: IO ()
main = do
  contents <- getContents

  putStrLn $ show $ part1 contents

