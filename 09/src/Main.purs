module Main
  ( main
  )
  where

import Prelude

import Data.Array (concatMap, foldl, length, many, nub, replicate, scanl, takeEnd)
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (optional)
import Parsing.String (char)
import Parsing.String.Basic (intDecimal, skipSpaces)



data Direction = N | NE | E | SE | S | SW | W | NW
derive instance genericDirection :: Generic Direction _
instance showDirection :: Show Direction where
  show = genericShow

type Coords = {x :: Int, y :: Int}

type State = {headCoords :: Coords,
              tailCoords :: Array Coords,
              tailVisited :: Array Coords}

toCoords :: Direction -> Coords
toCoords N = {x: 0, y: 1}
toCoords NE = {x: 1, y: 1}
toCoords E = {x: 1, y: 0}
toCoords SE = {x: 1, y: -1}
toCoords S = {x: 0, y: -1}
toCoords SW = {x: -1, y: -1}
toCoords W = {x: -1, y: 0}
toCoords NW = {x: -1, y: 1}

fromCoords :: Coords -> Maybe Direction
fromCoords {x, y} | x > 0 &&
                    y < 0  = Just SE
fromCoords {x, y} | x < 0 &&
                    y < 0 = Just SW
fromCoords {x, y} | x < 0 &&
                    y > 0 = Just NW
fromCoords {x, y} | x > 0 &&
                    y > 0 = Just NE
fromCoords {x: 0, y} | y > 0 = Just N
fromCoords {x: 0, y} | y < 0 = Just S
fromCoords {x, y: 0} | x > 0 = Just E
fromCoords {x, y: 0} | x < 0 = Just W
fromCoords {x: _, y: _} = Nothing

zipCoords :: (Int -> Int -> Int) -> Coords -> Coords -> Coords
zipCoords f a b = {x: f a.x b.x, y: f a.y b.y}

addCoords :: Coords -> Coords -> Coords
addCoords = zipCoords (+)

subCoords :: Coords -> Coords -> Coords
subCoords = zipCoords (-)

isFarAway :: Coords -> Coords -> Boolean
isFarAway a b | (abs $ toNumber (b.x - a.x)) > 1.0 ||
                (abs $ toNumber (b.y - a.y)) > 1.0 = true
              | otherwise = false

getMoveDirection :: Coords -> Coords -> Maybe Direction
getMoveDirection from to | isFarAway from to = fromCoords $ subCoords to from
getMoveDirection _ _ = Nothing

moveHead :: Coords -> Direction -> Coords
moveHead headCoords dir = addCoords headCoords $ toCoords dir

moveTail :: Coords -> Coords -> Coords
moveTail towards from =  addCoords from $ fromMaybe noStep step
  where
    step :: Maybe Coords
    step = toCoords <$> getMoveDirection from towards
    noStep :: Coords
    noStep = {x: 0, y: 0}

move :: State -> Direction -> State
move state dir = {headCoords: newHead, tailCoords: newTail, tailVisited: state.tailVisited <> takeEnd 1 newTail}
  where
    newHead :: Coords
    newHead = moveHead state.headCoords dir
    newTail :: Array Coords
    newTail = scanl moveTail newHead state.tailCoords-- state.tailCoords

inputParser :: Parser String (Array Direction)
inputParser = do
  moves <- many do
    d <- oneOf $ [char 'U' $> N,
                  char 'R' $> E,
                  char 'D' $> S,
                  char 'L' $> W]
    _ <- skipSpaces
    n <- intDecimal
    _ <- optional $ char '\n'
    pure (replicate n d)
  pure (concatMap identity moves)

createStartState :: Int -> State
createStartState tailLength = {
  headCoords: {x: 0, y: 0},
  tailCoords: (replicate tailLength {x: 0, y: 0}),
  tailVisited: [] :: Array Coords
  }

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  case runParser input inputParser of
    Right moves -> do
      let part1 = length $ nub $ _.tailVisited $ foldl move (createStartState 1) moves
      log "Part 1"
      log $ "Number of positions visited by tail: " <> show part1
      log "Part 2"
      let part2 = length $ nub $ _.tailVisited $ foldl move (createStartState 9) moves
      log $ "Number of positions visited by tail: " <> show part2
    Left msg -> logShow msg
