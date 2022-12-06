module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (toCodePointArray)
import Effect (Effect)
import Main (findUniqueSeq, uniqueElems)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest do
    suite "Test uniqueElems" do
      test "Empty array is unique" do
        Assert.equal true (uniqueElems ([] :: Array Char))
      test "Duplicate elements returns false" do
        Assert.equal false (uniqueElems [1,2,3,2])
      test "Unique elements returns true" do
        Assert.equal true (uniqueElems [1,2,3,4])
    suite "Test findUniqueSeq" do
      test "Returns start index of unique sequence (1)" do
        let array = toCodePointArray "bvwbjplbgvbhsrlpgdmjqwftvncz"
        Assert.equal (findUniqueSeq 4 array) (Just $ 5)
      test "Returns start index of unique sequence (2)" do
        let array = toCodePointArray "nppdvjthqldpwncqszvftbrmjlhg"
        Assert.equal (findUniqueSeq 4 array) (Just $ 6)
      test "Returns start index of unique sequence (3)" do
        let array = toCodePointArray "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
        Assert.equal (findUniqueSeq 4 array) (Just $ 10)
      test "Returns start index of unique sequence (4)" do
        let array = toCodePointArray "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
        Assert.equal (findUniqueSeq 4 array) (Just $ 11)
      test "No unique sequence returns Nothing" do
        let array = toCodePointArray "zcfzfzzqf"
        Assert.equal Nothing (findUniqueSeq 4 array)
