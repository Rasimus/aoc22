module Main
  where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Cont (lift)
import Control.Monad.RWS (RWS, execRWS, tell)
import Control.Monad.State (State, StateT, execState, get, gets, modify_, put)
import Control.Monad.Trampoline (done)
import Data.Array (length)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List.Lazy (List)
import Data.List.Lazy as LL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (many1, sepBy1)
import Parsing.String (char, string)

-- True and false represents occupied and empty spaces, respectively
type Layer = Array Boolean
type Rock = Array Layer
type Chamber = Rock
data WindDirection = LeftWind | RightWind

type WindOrder = List WindDirection
type RockOrder = List Rock
type ChamberState = {chamber :: Chamber,
                     rockStream :: RockOrder,
                     windStream :: WindOrder}
-- type SimulationConfig = Reader WindOrder RockOrder
type SimulationState = RWS String (Array String) ChamberState
-- type RockState = StateT {rock :: Rock, offset :: Int} SimulationState

class Pretty a where
  pretty :: a -> String

instance prettyWindDirection :: Pretty WindDirection where
  pretty LeftWind = "<"
  pretty RightWind = ">"

instance prettyWindOrder :: Pretty WindOrder where
  pretty xs = foldl append "" $ map pretty xs

instance prettyLayer :: Pretty Layer where
  pretty layer = fromCharArray $ map (\x -> if x then '#' else '.') layer

instance prettyRock :: Pretty Rock where
  pretty rock = joinWith "\n" $ map pretty rock

-- instance prettyChamber :: Pretty Chamber where
--   pretty chamber = joinWith "\n" $ map pretty chamber

instance prettyRockOrder :: Pretty RockOrder where
  pretty rocks = joinWith "\n\n" $ A.fromFoldable $ map pretty rocks

collision :: Layer -> Layer -> Boolean
collision a b = A.any identity $ A.zipWith (&&) a b

collisionRocks :: Rock -> Rock -> Int -> Boolean
collisionRocks [] _ _ = true
collisionRocks _ [] _ = true
collisionRocks a b offset | offset < 0 = collisionRocks b a (-offset)
collisionRocks a b offset = A.any identity $ A.zipWith collision a' b'
  where Tuple a' b' = extendTo a (prextend offset b)

zipWithAll :: forall a b c. (a -> b -> c) -> (b -> c) -> Array a -> Array b -> Array c
zipWithAll f g a b = zipped <> rest
  where
  zipped = A.zipWith f a b
  rest = map g $ A.drop (length zipped) b

extendTo :: Rock -> Rock -> Tuple Rock Rock
extendTo a b
  | length a < length b = Tuple a' b
    where a' = extend (length b - length a) a
  | otherwise = Tuple a b'
    where b' = extend (length a - length b) b

isEmpty :: Layer -> Boolean
isEmpty layer = not $ A.any identity layer

mergeLayers :: Layer -> Layer -> Layer
mergeLayers a b = A.zipWith (||) a b

width :: Rock -> Int
width r = length $ A.transpose r

-- Takes the first n layers of the rock, or if the rock has less than n layers
--  extend the rock to n layers by appending empty layers.
takeOrExtend :: Int -> Rock -> Rock
takeOrExtend n rock | length rock >= n = A.take n rock
                    | otherwise = extend (n - length rock) rock

-- Extends a rock by appending n empty layers
extend :: Int -> Rock -> Rock
extend n rock = rock <> (A.replicate n $ A.replicate (width rock) false)

-- Extends a rock by prepending n empty layers
prextend :: Int -> Rock -> Rock
prextend n rock = (A.replicate n $ A.replicate (width rock) false) <> rock

mergeRocks :: Rock -> Rock -> Int -> Rock
mergeRocks [] b _ = b
mergeRocks a [] _ = a
mergeRocks a b offset | offset < 0 = mergeRocks b a (-offset)
mergeRocks a b offset = uncurry (A.zipWith mergeLayers) $ extendTo a (prextend offset b)

push :: Rock -> WindDirection -> Rock
push rock LeftWind = pushLeft rock
push rock RightWind = pushRight rock

pushLeft :: Rock -> Rock
pushLeft rock = case A.uncons $ A.transpose rock of
  Nothing -> []
  Just {head, tail} | isEmpty head -> A.transpose (A.snoc tail head)
                    | otherwise -> rock

pushRight :: Rock -> Rock
pushRight rock = case A.uncons $ A.reverse $ A.transpose rock of
  Nothing -> []
  Just {head, tail} | isEmpty head -> A.transpose $ (A.reverse (A.snoc tail head))
                    | otherwise -> rock

windOrder :: Parser String WindOrder
windOrder = do
  directions <- many1 $ left <|> right
  pure $ LL.cycle $ LL.fromFoldable directions
  where
    left = char '<' *> pure LeftWind
    right = char '>' *> pure RightWind

rockOrder :: Parser String RockOrder
rockOrder = do
  rocks <- sepBy1 rock $ string "\n"
  pure $ LL.cycle $ LL.fromFoldable rocks
  where
    rock :: Parser String (Array Layer)
    rock = padRock 7 2 <$> A.many do
      l <- layer
      _ <- char '\n'
      pure l

    layer :: Parser String (Array Boolean)
    layer = A.fromFoldable <$> many1 (char '.' *> pure false <|> char '#' *> pure true)

padRock :: Int -> Int -> Rock -> Rock
padRock targetWidth leftMargin rock = --
  map padLayer rock
  where
  -- rockHeight = A.length rock
  currentWidth = A.length $ A.transpose rock
  padLayer :: Layer -> Layer
  padLayer l = leftPadding <> l <> rightPadding
  leftPadding = A.replicate leftMargin false
  rightPadding =  A.replicate (targetWidth - leftMargin - currentWidth) false

nextWind :: SimulationState WindDirection
nextWind = do
  windStream <- _.windStream <$> get
  case LL.uncons windStream of
    Nothing -> pure LeftWind -- Should never happen, can I improve this?
    Just {head, tail} -> do
      modify_ (\state -> state {windStream = tail})
      pure head

peekWind :: SimulationState WindDirection
peekWind = do
  dir <- gets $ _.windStream >>> LL.head
  pure $ fromMaybe LeftWind dir

nextRock :: SimulationState Rock
nextRock = do
  rockStream <- _.rockStream <$> get
  case LL.uncons rockStream of
    Nothing -> pure [] -- Should never happen, can I improve this?
    Just {head, tail} -> do
      modify_ (\state -> state {rockStream = tail})
      pure head

dropRock :: Rock -> Int -> SimulationState Unit
dropRock r o =
  do
    chamber <- gets _.chamber
    -- tell ["Rock falls down\n" <> "Current offset " <> show o]
    -- tell [pretty $ mergeRocks chamber r o]
    wind <- nextWind
    -- tell ["Jet of gas pushes rock " <> pretty wind]
    let pushedRock = pushInChamber r o chamber wind
    -- tell [pretty $ mergeRocks chamber pushedRock o]
    if collisionRocks chamber pushedRock (o+1)
      then do
        -- pushedRock <- pushInChamber r o chamber <$> nextWind
        -- tell ["Collision below, merging rock into chamber"]
        modify_ (\state -> state { chamber = mergeRocks chamber pushedRock o })
        -- tell [pretty $ mergeRocks chamber pushedRock o]
        pure unit
      else do
        dropRock pushedRock (o+1)

dropRocks :: Int -> SimulationState Unit
dropRocks 0 = pure unit
dropRocks n = do
  -- chamber <- gets _.chamber
  rock <- nextRock
  -- pushedRock <- push rock <$> nextWind

  let startOffset = -(length rock + 3)

  dropRock rock startOffset
  dropRocks $ n-1

pushInChamber :: Rock -> Int -> Chamber -> WindDirection -> Rock
pushInChamber r o c d =
  if collisionRocks c pushedRock o
     then r
     else pushedRock
  where pushedRock = push r d

main :: Effect Unit
main = do
  rockInput <- readTextFile UTF8 "rocks.txt"
  windInput <- readTextFile UTF8 "wind.txt"
  case runParser rockInput rockOrder of
    Left msg -> logShow msg
    Right rockOrder' ->
      case runParser windInput windOrder of
        Left msg -> logShow msg
        Right windOrder' -> do
          let numberOfRocks = 2022
              initState = {chamber: [[true,true,true,true,true,true,true]],
                           rockStream: rockOrder',
                           windStream: windOrder'}
              Tuple {chamber, windStream} debugLog = execRWS (dropRocks numberOfRocks) "" initState
          -- log $ pretty $ LL.take 5 rockOrder'
          -- log "\n"
          -- log $ pretty chamber
          logShow $ length chamber
          -- log $ pretty $ LL.take 20 windOrder'
          -- log $ pretty $ LL.take 20 windStream
          -- log $ joinWith "\n\n" $ debugLog
