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
versus opps yours | opps == yours = Tie
versus Rock     Paper     = Win
versus Paper    Scissors  = Win
versus Scissors Rock      = Win
versus _        _         = Loss

findChoice :: Choice -> Outcome -> Choice
findChoice Rock     Win       = Paper
findChoice Paper    Win       = Scissors
findChoice Scissors Win       = Rock
findChoice choice   Loss      = findChoice (findChoice choice Win) Win
findChoice choice   Tie       = choice

roundScore :: Choice -> Choice -> Int
roundScore opps yours =
  (choiceScore yours) + (outcomeScore outcome)
  where outcome = versus opps yours

choiceParser :: Parser String Choice
choiceParser =
  rockParser <|> paperParser <|> scissorsParser
  where
    rockParser = do
      _ <- char 'A'
      pure Rock
    paperParser = do
      _ <- char 'B'
      pure Paper
    scissorsParser = do
      _ <- char 'C'
      pure Scissors

outcomeParser :: Parser String Outcome
outcomeParser =
  winParser <|> lossParser <|> tieParser
  where
    winParser = do
      _ <- char 'X'
      pure Loss
    lossParser = do
      _ <- char 'Y'
      pure Tie
    tieParser = do
      _ <- char 'Z'
      pure Win

inputParser :: Parser String (Tuple Choice Outcome)
inputParser = do
  oppChoice <- choiceParser
  skipSpaces
  outcome <- outcomeParser
  skipSpaces
  pure (Tuple oppChoice outcome)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "input.txt"
  case runParser input $ many inputParser of
    Right xs -> do
      log "Total score:"
      let rounds = map (\(Tuple a b) -> (Tuple a (findChoice a b))) xs
      logShow $ sum $ map (uncurry roundScore) rounds
    Left msg ->
      logShow msg
