module Main
  ( findUniqueSeq
  , uniqueElems
  , main
  )
  where

import Prelude

import Data.Array (length, nub, tail, take)
import Data.Maybe (Maybe(..))
import Data.String (toCodePointArray)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

uniqueElems:: forall a. Ord a => Array a -> Boolean
uniqueElems xs = (nub xs) == xs

findUniqueSeq :: forall a. Ord a => Int -> Array a -> Maybe Int
findUniqueSeq n xs | (length $ take n xs) < n = Nothing
                   | uniqueElems $ take n xs = Just n
                   | otherwise = add 1 <$> (tail xs >>= findUniqueSeq n)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "input.txt"
  let inputArray = toCodePointArray input
  log "Part 1"
  case findUniqueSeq 4 inputArray of
    Just n -> log $ "First start-of-packet sequence detected at index: " <> show n
    Nothing -> log "No start-of-packet sequence detected."
  log "Part 2"
  case findUniqueSeq 14 $ inputArray of
    Just n -> log $ "First start-of-message sequence detected at index: " <> show n
    Nothing -> log "No start-of-message sequence detected."
