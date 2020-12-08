module Day7 where

import Data.Char
import Text.ParserCombinators.ReadP

letter = satisfy isLetter
space = satisfy isSpace
digit = satisfy isDigit

parse p s = parsedResult $ readP_to_S p s
  where
    parsedResult [(a, "")] = a
    parsedResult [(_, rs)] = error "Parser did not consume entire stream."
    parsedResult (a:as)    = parsedResult as
    parsedResult _         = error "Parser error"


data Bag = Bag { name :: String }
  deriving (Show)

bagName :: ReadP String
bagName = many1 (letter +++ space)

bag :: ReadP Bag
bag = do
  name' <- bagName
  _ <- space
  _ <- string "bags" <++ string "bag"
  return (Bag name')

int :: ReadP Int
int = do
  digits <- many1 digit
  return $ read digits

data Numbered a = Numbered { item :: a, value :: Int }
  deriving (Show)

numberedBag :: ReadP (Numbered Bag)
numberedBag = do
  value' <- int
  _ <- space
  bag' <- bag
  return (Numbered bag' value')

data Rule = Rule { container :: Bag, contents :: [Numbered Bag] }
  deriving (Show)

noOtherBags :: ReadP [Numbered Bag]
noOtherBags = do
  _ <- string "no other bags"
  return []

comma :: ReadP String
comma = string ", "

rule :: ReadP Rule
rule = do
  container' <- bag
  _ <- space
  _ <- string "contain"
  _ <- space
  contents' <- noOtherBags <++ (numberedBag `sepBy1` comma)
  return (Rule container' contents')

period = char '.'
endRule :: ReadP ()
endRule = do
  _ <- period
  skipSpaces
  return ()


rules :: ReadP [Rule]
rules = rule `endBy1` endRule
