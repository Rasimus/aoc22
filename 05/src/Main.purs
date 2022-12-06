module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array (foldl, many, replicate, updateAtIndices, (!!))
import Data.Either (Either(..))
import Data.List (List(..), transpose)
import Data.List as L
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (between, lookAhead, sepEndBy)
import Parsing.String (anyTill, char, string)
import Parsing.String.Basic (intDecimal, upper)

type Move = { amount :: Int, from :: Int, to :: Int }
type CrateStack = List Char
type Input = { crates :: Array CrateStack, moves :: Array Move }

moveCrates :: Boolean -> Array CrateStack -> Move -> Array CrateStack
moveCrates multiMove crates {amount, from, to} =
  updateAtIndices [Tuple (from-1) newFrom, Tuple (to-1) newTo] crates
  where
    fromCrate :: CrateStack
    fromCrate = fromMaybe Nil (crates !! (from-1))
    toCrate :: CrateStack
    toCrate = fromMaybe Nil (crates !! (to-1))
    cratesToMove :: CrateStack
    cratesToMove = (if multiMove then identity else L.reverse) $ L.take amount fromCrate
    newFrom :: CrateStack
    newFrom = L.drop amount fromCrate
    newTo :: CrateStack
    newTo = cratesToMove <> toCrate

inputParser :: Parser String Input
inputParser = do
  crates <- cratesParser
  _ <- (char '\n')
  moves <- many moveParser
  pure ({crates: crates, moves: moves})

moveParser :: Parser String Move
moveParser = do
  _       <- string "move "
  amount  <- intDecimal
  _       <- string " from "
  from    <- intDecimal
  _       <- string " to "
  to      <- intDecimal
  _       <- char '\n'
  pure ({amount: amount, from: from, to: to})

padCrates :: CrateStack -> CrateStack
padCrates crates = crates <> L.fromFoldable (replicate paddingLength ' ')
  where paddingLength = 9 - (L.length crates)


cratesParser :: Parser String (Array CrateStack)
cratesParser = do
  rows <- many rowParser
  _ <- anyTill (char '\n')
  pure (map (L.dropWhile (eq ' ')) $ L.toUnfoldable $ transpose $ L.fromFoldable rows)
  where
    rowParser = do
      _ <- lookAhead crateParser
      crates <- sepEndBy crateParser (char ' ')
      _ <- char '\n'
      pure (padCrates crates)
    crateParser = do
      value <- between (string "[") (string "]") upper <|>
               between (string " ") (string " ") (char ' ')
      pure (value)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "input.txt"
  case runParser input inputParser of
    Right parsed -> do
      let finalCrates = foldl (moveCrates false) parsed.crates parsed.moves
      log "Part 1 Top Crates:"
      logShow $ fromCharArray $ getTopCrates finalCrates
      let finalCrates9001 = foldl (moveCrates true) parsed.crates parsed.moves
      log "Part 2 Top Crates:"
      logShow $ fromCharArray $ getTopCrates finalCrates9001
      where
        getTopCrates = map ((fromMaybe ' ') <<< L.head)
    Left msg -> logShow msg
