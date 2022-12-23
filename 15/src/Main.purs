module Main where

import Prelude

import Control.Apply (lift2)
import Data.Array (any, catMaybes, filter, find, nub, sortBy, uncons, (..), (:))
import Data.Array as A
import Data.BigInt (BigInt, fromInt)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Ord (abs)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators.Array (many)
import Parsing.String (char, string)
import Parsing.String.Basic (intDecimal)

type Point = {x :: Int, y :: Int}
type SensorBeaconPair = {sensor :: Point, beacon :: Point}

type Range = {start :: Int, end :: Int}
ordRange :: Range -> Range -> Ordering
ordRange a b = case compare a.start b.start of
  EQ -> compare a.end b.end
  ord -> ord

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  case runParser input inputParser of
    Right pairs -> do
      log "Part 1"
      let
        row = 2000000
        blockedRanges = blocked row pairs
        beaconsInRow = nub $ map _.beacon $ filter (eq row <<< _.beacon.y) pairs
        beaconsInRanges = filter (inRanges blockedRanges) beaconsInRow
        blockedPos = (sum $ map length blockedRanges) - (A.length beaconsInRanges)
      log $ "Number of positions that can not contain a beacon: " <> (show blockedPos)
      log "Part 2"
      let
        range = {start: 0, end: 4000000}
        rowOfInterest :: Maybe Int
        rowOfInterest = find (neq [range] <<< flip blocked' pairs) (range.start .. range.end)
        colOfInterest :: Maybe Int
        colOfInterest = findGap =<< (flip blocked' pairs <$> rowOfInterest)
        position = lift2 makePoint colOfInterest rowOfInterest
      logShow $ frequency <$> position
    Left msg -> logShow msg

frequency :: Point -> BigInt
frequency {x, y} = (fromInt x) * (fromInt 4000000) + (fromInt y)

makePoint :: Int -> Int -> Point
makePoint x y = {x, y}

findGap :: Array Range -> Maybe Int
findGap [r1, _] = Just $ r1.end + 1
findGap _ = Nothing

neq :: forall a. Eq a => a -> a -> Boolean
neq = not <<< eq

length :: Range -> Int
length {start, end} = abs(end - start) + 1

blocked :: Int -> Array SensorBeaconPair -> Array Range
blocked row pairs = mergeRanges $ sortBy ordRange $ catMaybes $ map (pointsReached row) pairs

blocked' :: Int -> Array SensorBeaconPair -> Array Range
blocked' row pairs = mergeRanges $ sortBy ordRange $ catMaybes $ map (pointsReached' row) pairs

inRanges :: Array Range -> Point -> Boolean
inRanges rs p = any (inRange p) rs

inRange :: Point -> Range ->  Boolean
inRange {x, y} {start, end}
  | x >= start && x <= end = true
  | otherwise = false

pointsReached :: Int -> SensorBeaconPair -> Maybe Range
pointsReached row {sensor, beacon} =
  let
    reach = abs(sensor.x - beacon.x) + abs(sensor.y - beacon.y)
    rowDistance = abs(sensor.y - row)
    remaining = reach - rowDistance
  in
  case remaining >= 0 of
       false -> Nothing
       true -> Just {start: sensor.x - remaining, end: sensor.x + remaining}

pointsReached' :: Int -> SensorBeaconPair -> Maybe Range
pointsReached' row {sensor, beacon} =
  let
    reach = abs(sensor.x - beacon.x) + abs(sensor.y - beacon.y)
    rowDistance = abs(sensor.y - row)
    remaining = reach - rowDistance
    rangeLimit = {start: 0, end: 4000000}
  in
  case remaining >= 0 of
       false -> Nothing
       true -> Just {start: max (sensor.x - remaining) rangeLimit.start,
                     end: min (sensor.x + remaining) rangeLimit.end}

mergeRanges :: Array Range -> Array Range
mergeRanges rs = case uncons rs of
  Nothing -> []
  Just {head: x, tail: xs} -> case uncons xs of
    Nothing -> [x]
    Just {head: y, tail: ys} -> case merge x y of
      Nothing -> x:(mergeRanges xs)
      Just r -> mergeRanges (r:ys)
  where
    merge :: Range -> Range -> Maybe Range
    merge a b | b.start > a.end = Nothing
              | b.end < a.end = Just a
              | otherwise = Just {start: a.start, end: b.end}


sensorBeacon :: Parser String SensorBeaconPair
sensorBeacon = do
  _ <- string "Sensor at "
  sensor <- point
  _ <- string ": closest beacon is at "
  beacon <- point
  _ <- char '\n'
  pure {sensor: sensor, beacon: beacon}

point :: Parser String Point
point = do
  x <- string "x=" *> intDecimal
  _ <- string ", "
  y <- string "y=" *> intDecimal
  pure {x: x, y: y}

inputParser :: Parser String (Array SensorBeaconPair)
inputParser = do
  pairs <- many sensorBeacon
  pure pairs
