module Main where

import Prelude

import Data.Array (head, index, many, tail, (:))
import Data.Either (Either(..))
import Data.Foldable (oneOf, sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (optional)
import Parsing.String (char, string)
import Parsing.String.Basic (intDecimal)


-- Parses a list of newline separated "NOOP" or "ADDX" operations into a list of integers.
-- NOOP is parsed to 0
-- ADDX `x` is parsed to `x`
inputParser :: Parser String (Array Int)
inputParser = do
  let optEol = optional $ char '\n'
  ops <- many $ oneOf
          [0 <$ string "noop\n" <* optEol,
           string "addx " *> intDecimal <* optEol]
  pure (ops)

signalStrength :: Array Int -> Int -> Int
signalStrength xs i = (i+1) * (fromMaybe 0 $ index xs i)

elvenCpu :: Int -> Int -> Int -> Array Int -> Array Int
elvenCpu 0 op value ops =
  case head ops of
    Nothing   -> newValue : []
    Just 0    -> newValue : (elvenCpu 0 0 newValue tail')
    Just x    -> newValue : (elvenCpu 1 x newValue tail')
  where
    newValue = value + op
    tail' = fromMaybe [] $ tail ops
elvenCpu cyclesLeft op value ops  = value : (elvenCpu (cyclesLeft-1) op value ops)

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  case runParser input inputParser of
    Right ops -> do
      let
        -- Add a zero before every non-zero value to represent the first cycle of the operation
        cycleValues = elvenCpu 0 0 1 ops
        cyclesOfInterest = map (add (-1)) [20, 60, 100, 140, 180, 220]
        cycleStrengths = map (signalStrength cycleValues) cyclesOfInterest
        sumOfStrengths = sum $ cycleStrengths
      logShow sumOfStrengths
    Left msg -> logShow msg
