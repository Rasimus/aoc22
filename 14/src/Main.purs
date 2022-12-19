
module Main where

import Prelude

import Data.Array (concatMap, foldl, nub, (..))
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..), fromMaybe, optional)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (lookAhead, try)
import Parsing.Combinators.Array (many)
import Parsing.String (char, string)
import Parsing.String.Basic (intDecimal)
import QuadTree as QT


type Point = {x :: Int, y :: Int}
type Line = {start :: Point, end :: Point}

dropSand :: Int -> QT.QuadTree -> Point -> Maybe QT.QuadTree
dropSand lowest qt p@{x, y}
  | y >= lowest = Nothing
  | not $ QT.exists qt {x: x, y: y+1} = dropSand lowest qt {x: x, y: y+1}
  | not $ QT.exists qt {x: x-1, y: y+1} = dropSand lowest qt {x: x-1, y: y+1}
  | not $ QT.exists qt {x: x+1, y: y+1} = dropSand lowest qt {x: x+1, y: y+1}
  | otherwise = Just $ QT.insert qt p

dropSand' lowest qt p@{x, y}
  | y == lowest+2 = Just $ QT.insert qt {x: x, y: lowest+1}
  | not $ QT.exists qt {x: x, y: y+1} = dropSand' lowest qt {x: x, y: y+1}
  | not $ QT.exists qt {x: x-1, y: y+1} = dropSand' lowest qt {x: x-1, y: y+1}
  | not $ QT.exists qt {x: x+1, y: y+1} = dropSand' lowest qt {x: x+1, y: y+1}
  | p == {x: 500, y: 0} = Nothing
  | otherwise = Just $ QT.insert qt p

dropUntilNothing :: Int -> QT.QuadTree -> Int
dropUntilNothing lowest qt =
  case dropSand lowest qt {x: 500, y: 0} of
    Nothing -> 0
    Just qt' -> 1 + (dropUntilNothing lowest qt')

dropUntilNothing' :: Int -> QT.QuadTree -> Int
dropUntilNothing' lowest qt =
  case dropSand' lowest qt {x: 500, y: 0} of
    Nothing -> 1
    Just qt' -> 1 + (dropUntilNothing' lowest qt')

createPoint :: Int -> Int -> Point
createPoint x y = {x: x, y: y}

toLine :: Point -> Line
toLine p = {start: p, end: p}

toPoints :: Line -> Array Point
toPoints line = do
  x <- (line.start.x .. line.end.x)
  y <- (line.start.y .. line.end.y)
  pure {x: x, y: y}

parsePoint :: Parser String Point
parsePoint = do
  x <- intDecimal
  _ <- char ','
  y <- intDecimal
  pure {x: x, y: y}

parseLine :: Parser String Line
parseLine = do
  start <- parsePoint
  _ <- string " -> "
  end <- lookAhead $ parsePoint
  _ <- optional $ try (parsePoint <* string "\n")
  pure {start: min start end, end: max start end}

parseInput :: Parser String (Array Point)
parseInput = do
  lines <- many $ toPoints <$> parseLine
  pure $ concatMap identity lines

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  case runParser input parseInput of
    Right points ->
      do
        let uniquePoints = nub points
            lowest = fromMaybe 0 $ maximum $ map _.y uniquePoints
            qt = foldl QT.insert QT.Empty uniquePoints
        log "Part 1:"
        log $ "Number of sand units: " <> (show $ dropUntilNothing lowest qt)
        log "Part 2:"
        log $ "Number of sand units: " <> (show $ dropUntilNothing' lowest qt)
    Left msg -> logShow msg
