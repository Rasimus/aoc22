module Main where

import Prelude

import Data.Array (many, filter, length)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (runParser, Parser)
import Parsing.Combinators (optional)
import Parsing.String (char)
import Parsing.String.Basic (number)

type Range = {start :: Number, end :: Number}

contains :: Range -> Range -> Boolean
contains a b | a.start <= b.start && a.end >= b.end = true
             | otherwise = false
overlapping :: Range -> Range -> Boolean
overlapping a b | contains a {start: b.start, end: b.start} ||
                  contains a {start: b.end, end: b.end} ||
                  contains b {start: a.start, end: a.start} ||
                  contains b {start: a.end, end: a.end} = true
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
    Right xs -> do
      log "Part 1. Number of fully contained pairs:"
      logShow $ countTrue $ map pairContains xs
      log "Part 2. Number of overlapping pairs:"
      logShow $ countTrue $ map (uncurry overlapping) xs
    Left msg -> logShow msg
  where countTrue xs = length $ filter ((==) true) xs
