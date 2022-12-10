module Aoc (runReadP) where

import Text.ParserCombinators.ReadP

runReadP :: ReadP a -> String -> a
runReadP p s = unwrap $ readP_to_S p s
  where
    unwrap [(a, "")]  = a
    unwrap [(_, _rs)] = error "Parser did not consume entire stream."
    unwrap (_:as)     = unwrap as
    unwrap _          = error "Parser error."
