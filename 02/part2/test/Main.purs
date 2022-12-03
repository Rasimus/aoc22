module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Main (versus, Outcome(..), Choice(..), findChoice)


main :: Effect Unit
main = do
  runTest do
    suite "Test versus" do
      test "Rock vs Rock" do
        Assert.equal Tie $ versus Rock Rock
      test "Paper vs Paper" do
        Assert.equal Tie $ versus Paper Paper
      test "Scissors vs Scissors" do
        Assert.equal Tie $ versus Scissors Scissors
      test "Rock vs Paper" do
        Assert.equal Win $ versus Rock Paper
      test "Paper vs Scissors" do
        Assert.equal Win $ versus Paper Scissors
      test "Scissors vs Rock" do
        Assert.equal Win $ versus Paper Scissors
      test "Rock vs Scissors" do
        Assert.equal Loss $ versus Rock Scissors
      test "Paper vs Rock" do
        Assert.equal Loss $ versus Paper Rock
      test "Scissors vs Paper" do
        Assert.equal Loss $ versus Scissors Paper
    suite "Test findChoice" do
      test "Tie vs Rock" do
        Assert.equal Rock $ findChoice Rock Tie
      test "Tie vs Paper" do
        Assert.equal Paper $ findChoice Paper Tie
      test "Tie vs Scissors" do
        Assert.equal Scissors $ findChoice Scissors Tie
      test "Win vs Rock" do
        Assert.equal Paper $ findChoice Rock Win
      test "Win vs Paper" do
        Assert.equal Scissors $ findChoice Paper Win
      test "Win vs Rock" do
        Assert.equal Rock $ findChoice Scissors Win
      test "Loss vs Rock" do
        Assert.equal Scissors $ findChoice Rock Loss
      test "Loss vs Paper" do
        Assert.equal Rock $ findChoice Paper Loss
      test "Loss vs Scissors" do
        Assert.equal Paper $ findChoice Scissors Loss
