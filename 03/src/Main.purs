module Main where

import Prelude

import Data.Array as A
import Data.Char (toCharCode)
import Data.CodePoint.Unicode (isLower)
import Data.Set as S
import Data.String (Pattern(..), codePointFromChar, length, split, splitAt)
import Data.String.CodeUnits as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Data.Foldable (sum)

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

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "input.txt"
  let backpacks = split (Pattern "\n") input
  let splitBackpacks = map splitAtHalf backpacks
  let backpackItems = A.concatMap findCommonItems splitBackpacks
  logShow $ sum $ map itemScore backpackItems
