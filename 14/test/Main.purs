module Test.Main where

import Prelude

import Data.Array (index, scanl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as S
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main =  do
  log "Hello"
