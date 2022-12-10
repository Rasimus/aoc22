module Main
  ( Direction(..)
  , GridLocation
  , HeadLocation
  , State
  , TailPosition(..)
  , main
  , move
  , moveTail
  , rotate
  , toCoords
  )
  where

import Prelude

import Data.Array (concatMap, foldl, length, many, nub, replicate, (:))
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (optional)
import Parsing.String (char)
import Parsing.String.Basic (intDecimal, skipSpaces)



data TailPosition = C | N | NE | E | SE | S | SW | W | NW
data Direction = UP | RIGHT | DOWN | LEFT

derive instance genericDirection :: Generic Direction _
instance showDirection :: Show Direction where
  show = genericShow

type GridLocation = {x :: Int, y :: Int}
type HeadLocation = GridLocation

type State = {headLoc :: HeadLocation, tailPos :: TailPosition, tailHistory :: Array GridLocation }

-- ISOMORPHISMS, IS THERE A TYPECLASS TO USE?
-- input:   TailPosition
-- output:  TailPosition after rotating the grid 90 degrees
rotate :: TailPosition -> TailPosition
rotate C  = C
rotate N  = E
rotate NE = SE
rotate E  = S
rotate SE = SW
rotate S  = W
rotate SW = NW
rotate W  = N
rotate NW = NE
rotate2 :: TailPosition -> TailPosition
rotate2 = rotate <<< rotate
rotate3 :: TailPosition -> TailPosition
rotate3 = rotate <<< rotate <<< rotate
-- input:   TailPosition
-- output:  TailPosition as coords relative to head (0, 0)
toCoords :: TailPosition -> GridLocation
toCoords C  = {x:   0,  y:    0}
toCoords N  = {x:   0,  y:    1}
toCoords NE = {x:   1,  y:    1}
toCoords E  = {x:   1,  y:    0}
toCoords SE = {x:   1,  y:  - 1}
toCoords S  = {x:   0,  y:  - 1}
toCoords SW = {x: - 1,  y:  - 1}
toCoords W  = {x: - 1,  y:    0}
toCoords NW = {x: - 1,  y:    1}

addGL :: GridLocation -> GridLocation -> GridLocation
addGL {x: a, y: b} {x: c, y: d} = {x: a+c, y: b+d}

-- input:   TailPosition
-- output:  new TailPosition after moving head 1 step northward
moveTail :: TailPosition -> TailPosition
moveTail SW = S
moveTail S  = S
moveTail SE = S
moveTail C  = S
moveTail W  = SW
moveTail E  = SE
moveTail NE = E
moveTail NW = W
moveTail N  = C

-- input:
-- output: TailPosition
move :: State -> Direction -> State
move {headLoc: {x, y}, tailPos, tailHistory} dir =
  {headLoc:     newHeadPos dir,
   tailPos:     newTailPos dir,
   tailHistory: (newTailCoords:tailHistory)}
  where newHeadPos UP     = {x: x,    y: y+1}
        newHeadPos RIGHT  = {x: x+1,  y: y}
        newHeadPos DOWN   = {x: x,    y: y-1}
        newHeadPos LEFT   = {x: x-1,  y: y}

        newTailPos UP     = moveTail tailPos
        newTailPos RIGHT  = rotate $ moveTail $ rotate3 tailPos
        newTailPos DOWN   = rotate2 $ moveTail $ rotate2 tailPos
        newTailPos LEFT   = rotate3 $ moveTail $ rotate tailPos

        newTailCoords = addGL (newHeadPos dir) (toCoords $ newTailPos dir)

inputParser :: Parser String (Array Direction)
inputParser = do
  moves <- many do
    d <- oneOf $ [char 'U' $> UP,
                  char 'R' $> RIGHT,
                  char 'D' $> DOWN,
                  char 'L' $> LEFT]
    _ <- skipSpaces
    n <- intDecimal
    _ <- optional $ char '\n'
    pure (replicate n d)
  pure (concatMap identity moves)

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  case runParser input inputParser of
    Right moves -> do
      let startState = {headLoc: {x: 0, y: 0},
                        tailPos: C,
                        tailHistory: []}
          endState = foldl move startState moves
      log "Part 1"
      log $ "Number of positions visited by tail: " <> (show $ length $ nub $ endState.tailHistory)
    Left msg -> logShow msg
