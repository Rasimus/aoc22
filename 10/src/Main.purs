module Main where

import Prelude

import Data.Array (concatMap, drop, foldl, groupBy, head, index, many, scanl, tail, take, (:))
import Data.Either (Either(..))
import Data.Foldable (oneOf, sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log, logShow)
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

shiftNonZero :: Array Int -> Array Int
shiftNonZero xs = case take 2 xs of
  [a, 0] -> [0, a] <> (shiftNonZero $ drop 2 xs)
  [0, _] -> 0:(shiftNonZero $ drop 1 xs)
  -- [x] -> [x, 0]
  _ -> []



main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  case runParser input inputParser of
    Right ops -> do
      let
        -- Add a zero before every non-zero value to represent the first cycle of the operation
        cycleOps = shiftNonZero $ concatMap (\x -> if x /= 0 then [0, x] else [x]) (ops <> [0])
        cycleValues = scanl (+) 1 cycleOps
        cyclesOfInterest = map (add (-1)) [20, 60, 100, 140, 180, 220]
        cycleStrengths = map (signalStrength cycleValues) cyclesOfInterest
        sumOfStrengths = sum $ cycleStrengths
      log "Part 1"
      log $ "Sum of signal strengths at interesting cycles: " <> show sumOfStrengths
    Left msg -> logShow msg
