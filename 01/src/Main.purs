module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Data.String.Pattern (Pattern(..))
import Data.String (split)
import Data.Foldable (sum, foldr)
import Data.Int (fromString)
import Data.Array (catMaybes, sort, reverse, take)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "input1.txt"
  let splitElves = split (Pattern "\n\n") input
  let splitFood = map (split (Pattern "\n")) splitElves
  let foodAsInt = map (catMaybes <<< (map fromString)) splitFood
  let caloriesPerElf = map sum foodAsInt
  let mostCarriedBy1 = foldr max 0 caloriesPerElf
  log "Most calories carried by one elf:"
  logShow mostCarriedBy1
  let sortedCaloriesPerElf = reverse $ sort caloriesPerElf
  let mostCarriedBy3 = sum $ take 3 sortedCaloriesPerElf
  log "Most calories carried by three elves: "
  logShow mostCarriedBy3
