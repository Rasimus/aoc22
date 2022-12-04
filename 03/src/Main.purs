module Main where

import Prelude

import Data.Array as A
import Data.Array ((:))
import Data.Char (toCharCode)
import Data.CodePoint.Unicode (isLower)
import Data.Foldable (sum)
import Data.Set as S
import Data.String (Pattern(..), codePointFromChar, length, split, splitAt)
import Data.String.CodeUnits as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)


stringToSet :: String -> S.Set Char
stringToSet = S.fromFoldable <<< String.toCharArray

itemScore :: Char -> Int
itemScore c | isLower $ codePointFromChar c = 1 + (toCharCode c) - (toCharCode 'a')
            | otherwise = 27 + (toCharCode c) - (toCharCode 'A')

splitAtHalf :: String -> { before :: String, after :: String }
splitAtHalf xs = splitAt halfPoint xs
  where halfPoint = (length xs) `div` 2
findCommonItems :: { before :: String, after :: String } -> Array Char
findCommonItems {before, after} = S.toUnfoldable $ S.intersection (stringToSet before) (stringToSet after)

chunks :: forall a. Int -> Array a -> Array (Array a)
chunks _ [] = []
chunks n xs = (A.take n xs):(chunks n $ A.drop n xs)

badgeScore :: Array String -> Int
badgeScore [elf1, elf2, elf3] = sum $ map itemScore commonItems
  where
    commonItems :: Array Char
    commonItems = S.toUnfoldable $ S.intersection commonItems' (stringToSet elf3)
    commonItems' = S.intersection (stringToSet elf1) (stringToSet elf2)

badgeScore _ = 0

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "input.txt"
  let backpacks = split (Pattern "\n") input
  let splitBackpacks = map splitAtHalf backpacks
  let backpackItems = A.concatMap findCommonItems splitBackpacks
  log "Part 1 total item score:"
  logShow $ sum $ map itemScore backpackItems
  -- let groups =
  let groups = chunks 3 backpacks
  log "Part 2 total badge score:"
  logShow $ sum $ map badgeScore groups
