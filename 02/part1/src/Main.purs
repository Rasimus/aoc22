module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array (many)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (runParser, Parser)
import Parsing.String (char)
import Parsing.String.Basic (skipSpaces)

data Choice = Rock | Paper | Scissors
derive instance eqChoice      :: Eq Choice
derive instance genericChoice :: Generic Choice _
instance showChoice :: Show Choice where
  show = genericShow

data Outcome = Win | Loss | Tie
derive instance eqOutcome       :: Eq Outcome
derive instance genericOutcome  :: Generic Outcome _
instance showOutcome  :: Show Outcome where
  show = genericShow

choiceScore :: Choice -> Int
choiceScore Rock      = 1
choiceScore Paper     = 2
choiceScore Scissors  = 3

outcomeScore :: Outcome -> Int
outcomeScore Win  = 6
outcomeScore Tie  = 3
outcomeScore Loss = 0

versus :: Choice -> Choice -> Outcome
versus opps yours
  | opps == yours     = Tie
versus Rock Paper     = Win
versus Paper Scissors = Win
versus Scissors Rock  = Win
versus _ _            = Loss

roundScore :: Choice -> Choice -> Int
roundScore opps yours = (choiceScore yours) + (outcomeScore outcome)
  where
    outcome = versus opps yours

rockParser :: Parser String Choice
rockParser = do
  _ <- char 'A' <|> char 'X'
  pure Rock
paperParser :: Parser String Choice
paperParser = do
  _ <- char 'B' <|> char 'Y'
  pure Paper
scissorsParser :: Parser String Choice
scissorsParser = do
  _ <- char 'C' <|> char 'Z'
  pure Scissors
choiceParser :: Parser String Choice
choiceParser = rockParser <|> paperParser <|> scissorsParser

inputParser1 :: Parser String (Tuple Choice Choice)
inputParser1 = do
  oppChoice <- choiceParser
  skipSpaces
  subjChoice <- choiceParser
  skipSpaces
  pure (Tuple oppChoice subjChoice)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "input.txt"
  case runParser input $ many inputParser1 of
    Right xs -> do
      log "Total score:"
      logShow $ sum $ map (uncurry roundScore) xs
    Left msg ->
      logShow msg
