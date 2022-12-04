module Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Data.Tuple (Tuple(..), uncurry)
import Parsing (runParser, Parser)
import Parsing.String (char)
import Parsing.String.Basic (number)
import Parsing.Combinators (optional)
import Data.Array (many, filter, length)
type Range = {start :: Number, end :: Number}

contains :: Range -> Range -> Boolean
contains a b | a.start <= b.start && a.end >= b.end = true
             | otherwise = false

pairContains :: Tuple Range Range -> Boolean
pairContains (Tuple a b) = contains a b || contains b a
lineParser :: Parser String (Tuple Range Range)
lineParser = do
  firstRange <- rangeParser
  _ <- char ','
  secondRange <- rangeParser
  _ <- optional $ char '\n'
  pure $ Tuple firstRange secondRange

rangeParser :: Parser String Range
rangeParser = do
  start <- number
  _ <- char '-'
  end <- number
  pure ({start: start, end: end})




main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "input.txt"
  case runParser input $ many lineParser of
    Right xs -> logShow $ length $ filter ((==) true) $ map pairContains xs
    Left msg -> logShow msg
