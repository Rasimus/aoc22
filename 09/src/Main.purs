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

moveCoords :: Direction -> GridLocation -> GridLocation
moveCoords UP     coords  = addGL coords $ toCoords N
moveCoords RIGHT  coords  = addGL coords $ toCoords E
moveCoords DOWN   coords  = addGL coords $ toCoords S
moveCoords LEFT   coords  = addGL coords $ toCoords W

addGL :: GridLocation -> GridLocation -> GridLocation
addGL {x: a, y: b} {x: c, y: d} = {x: a+c, y: b+d}

-- input: direction that head moved
--        current tail position relative to head
-- output:  new TailPosition after moving head 1 step northward
moveTail :: Direction -> TailPosition -> TailPosition
moveTail UP W   = SW
moveTail UP E   = SE
moveTail UP NE  = E
moveTail UP NW  = W
moveTail UP N   = C
moveTail UP SW  = S
moveTail UP S   = S
moveTail UP SE  = S
moveTail UP C   = S
moveTail RIGHT  pos = rotate  $ moveTail UP $ rotate3 pos
moveTail DOWN   pos = rotate2 $ moveTail UP $ rotate2 pos
moveTail LEFT   pos = rotate3 $ moveTail UP $ rotate pos

-- input:   state, direction
-- output:  state after moving head 1 step in direction
move :: State -> Direction -> State
move {headLoc: {x, y}, tailPos, tailHistory} dir =
  {headLoc:     newHeadPos,
   tailPos:     newTailPos,
   tailHistory: (newTailCoords:tailHistory)}
  where newHeadPos = moveCoords dir {x, y}
        newTailPos = moveTail dir tailPos
        newTailCoords = addGL newHeadPos (toCoords $ newTailPos)


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
