module Test.Main where

import Prelude

import Control.Monad.Trampoline (done)
import Data.Array as A
import Data.Interpolate (i)
import Effect (Effect)
import Effect.Class.Console (log)
import Main (Rock, WindDirection(..), collision, collisionRocks, mergeLayers, mergeRocks, padRock, pushRight, takeOrExtend, zipWithAll)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

horizontal :: Rock
horizontal =
  [[true, true, true, true]]

cross :: Rock
cross =
  [[false,  true, false],
   [true,   true, true],
   [false,  true, false]]

backwardsL :: Rock
backwardsL =
  [[false, false,  true],
   [false, false,  true],
   [true,  true,   true]]

vertical :: Rock
vertical =
  [[true],
   [true],
   [true],
   [true]]

square :: Rock
square =
  [[true, true],
   [true, true]]

paddedSquare :: Rock
paddedSquare =
  [[false, false, true, true, false, false, false],
   [false, false, true, true, false, false, false]]

paddedVertical :: Rock
paddedVertical =
  [[false, false, true, false, false, false, false],
   [false, false, true, false, false, false, false],
   [false, false, true, false, false, false, false],
   [false, false, true, false, false, false, false]]

paddedHorizontal :: Rock
paddedHorizontal = [[false, false, true, true, true, true, false]]

paddedBackwardsL :: Rock
paddedBackwardsL =
  [[false, false, false, false,  true, false, false],
   [false, false, false, false,  true, false, false],
   [false, false, true,  true,   true, false, false]]

paddedCross :: Rock
paddedCross =
  [[false, false,  false, true, false,  false, false],
   [false, false,  true,  true, true,   false, false],
   [false, false,  false, true, false,  false, false]]

main :: Effect Unit
main = do
  runTest do
    suite "Test padRock" do
      let leftMargin = 2
          targetWidth = 7
          padRock' = padRock targetWidth leftMargin
      test "Pad vertical" do
        Assert.equal paddedVertical $ padRock' vertical
      test "Pad horizontal" do
        Assert.equal paddedHorizontal $ padRock' horizontal
      test "Pad square" do
        Assert.equal paddedSquare $ padRock' square
      test "Pad cross" do
        Assert.equal paddedCross $ padRock' cross
      test "Pad backwardsL" do
        Assert.equal paddedBackwardsL $ padRock' backwardsL
    suite "Test takeOrExtend" do
      test "Take all" do
        Assert.equal cross $ takeOrExtend (A.length cross) cross
      test "Take less" do
        let n = 1 - A.length cross
        Assert.equal (A.take n cross) (takeOrExtend n cross)
      test "Take more" do
        let extendedCross = cross <> [[false, false, false]]
            extendedCross2 = extendedCross <> [[false, false, false]]
        Assert.equal extendedCross $ takeOrExtend (1 + A.length cross) cross
        Assert.equal extendedCross2 $ takeOrExtend (2 + A.length cross) cross

    suite "Test zipWithAll" do
      test "Same length arrays is just zipWith" do
        let a = [1,2,3,4,5]
            b = [1,0,1,0,1]
        Assert.equal (zipWithAll (+) identity a b) (A.zipWith (+) a b)
      test "Second array is of greater length" do
        let a = [1,2,3]
            b = [1,0,1,0,1]
            c = [2,2,4,0,1]
        Assert.equal (zipWithAll (+) identity a b) c
      test "First array is of greater length" do
        let a = [1,2,3,4,5]
            b = [1,0,1]
            c = [2,2,4]
        Assert.equal (zipWithAll (+) identity a b) c

    suite "Test layerCollision" do
      let a = [false,true]
          b = [true, true]
          c = [false, true]
          d = [true, false]
          emptyLayer = []
      test "Collision" do
        Assert.assert "layers a b are NOT colliding" (collision a b)
        Assert.assert "layers a c are NOT colliding" (collision a c)
        Assert.assert "layers b d are NOT colliding" (collision b d)
      test "No collision" do
        Assert.assertFalse "layers a d are colliding" (collision a d)
        Assert.assertFalse "layers b c are colliding" (collision c d)
      test "No collision with empty" do
        Assert.assertFalse "layer a is colliding with empty layer" (collision a emptyLayer)

    suite "Test mergeLayers" do
      test "Merge" do
        let a = [false, false, true]
            b = [true, false, false]
        Assert.equal [true, false, true] $ mergeLayers a b

    suite "Test mergeRocks" do
      test "Merge without offset" do
        let merged =
              [[false, false, true, true,  true, true, false],
               [false, false, true, false, false, false, false],
               [false, false, true, false, false, false, false],
               [false, false, true, false, false, false, false]]
        Assert.equal merged $ mergeRocks paddedHorizontal paddedVertical 0
      test "Merge with offset" do
        let merged1 =
              [[false, false, true, true,  true, true, false],
               [false, false, true, false, false, false, false],
               [false, false, true, false, false, false, false],
               [false, false, true, false, false, false, false],
               [false, false, true, false, false, false, false]]
        Assert.equal merged1 $ mergeRocks paddedHorizontal paddedVertical 1
        let merged2 =
              [[false, false, true, false, false, false, false],
               [false, false, true, true,  true, true, false],
               [false, false, true, false, false, false, false],
               [false, false, true, false, false, false, false]]
        Assert.equal merged2 $ mergeRocks paddedVertical paddedHorizontal 1
      test "Merge with empty" do
        Assert.equal paddedVertical $ mergeRocks paddedVertical [] 0
        Assert.equal paddedVertical $ mergeRocks [] paddedVertical 0
    suite "Test collisionRocks" do
      test "No collision" do
        Assert.assertFalse
          "backwardsL and square collides"
          $ collisionRocks paddedBackwardsL paddedSquare 0
        Assert.assertFalse
          "vertical and right-shifted cross collides"
          $ collisionRocks paddedVertical (pushRight paddedCross) 0
      test "Collision" do
        Assert.assert "vertical and horizontal does not collide"
          $ collisionRocks paddedVertical paddedHorizontal 0
        Assert.assert
          "backwardsL and offset (1) square does notcollide"
          $ collisionRocks paddedBackwardsL paddedSquare 1
