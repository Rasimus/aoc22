module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array (head, tail)
import Data.Either (Either(..))
import Data.Foldable (length, product, sum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List, filter, snoc, sort, toUnfoldable, unzip, zip, (..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (between, optional, sepBy)
import Parsing.String (char, string)
import Parsing.String.Basic (intDecimal)

data Packet = Nested (Array Packet) | Single Int

nest :: Packet -> Packet
nest x = Nested [x]

instance showPacket :: Show Packet where
  show (Nested xs) = "Nested " <> show xs
  show (Single x) = "Single " <> show x

instance eqPacket :: Eq Packet where
  eq :: Packet -> Packet -> Boolean
  eq (Single x) (Single y) = eq x y
  eq (Nested xs) (Nested ys) = eq xs ys
  eq x@(Single _) ys@(Nested _) = eq (nest x) ys
  eq xs@(Nested _) y@(Single _) = eq xs (nest y)

instance ordPacket :: Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (Single x) (Single y) = compare x y
  compare x@(Single _) ys@(Nested _) = compare (nest x) ys
  compare xs@(Nested _) y@(Single _) = compare xs (nest y)
  compare (Nested xs) (Nested ys) =
    case Tuple (head xs) (head ys) of
      Tuple (Just x) (Just y)
        | x == y -> compare (Nested $ fromMaybe [] $ tail xs) (Nested $ fromMaybe [] $ tail ys)
        | otherwise -> compare x y
      Tuple Nothing (Just _) -> LT
      Tuple Nothing Nothing -> EQ
      Tuple (Just _) Nothing -> GT

parsePacket :: Parser String Packet
parsePacket = do
  packet <- Single <$> intDecimal <|>
            Nested <<< toUnfoldable <$>
              defer \_ -> between
              (string "[") (string "]")
              (sepBy parsePacket $ string ",")
  pure packet

parsePair :: Parser String (Tuple Packet Packet)
parsePair = do
    p1 <- parsePacket
    _  <- char '\n'
    p2 <- parsePacket
    _  <- optional $ char '\n'
    pure (Tuple p1 p2)

parseInput :: Parser String (List (Tuple Packet Packet))
parseInput = do
  pairs <- sepBy parsePair $ char '\n'
  pure pairs

isDivider :: Packet -> Boolean
isDivider (Nested [Nested [Single 2]]) = true
isDivider (Nested [Nested [Single 6]]) = true
isDivider _ = false

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  case runParser input parseInput of
    Right packetPairs -> do

      log "Part 1:"
      let
        inOrder = map fst $ filter snd $ mapWithIndex (\idx (Tuple a b) -> Tuple (idx+1) (a <= b)) packetPairs
      log $ "Sum of indices of ordered pairs: " <> (show $ sum $ inOrder)

      log "Part 2:"
      let
        dividerPair = Tuple (nest $ nest $ Single 2) (nest $ nest $ Single 6)
        withDividers = snoc packetPairs dividerPair
        sortedPackets = sort $ uncurry append $ unzip withDividers
        nPackets = length sortedPackets
        dividerIndices = map fst $ filter (isDivider <<< snd) $ zip (1..nPackets) sortedPackets
      log $ "Product of divider indices: " <> (show $ product dividerIndices)
    Left msg -> logShow msg
