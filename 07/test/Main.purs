module Test.Main where

import Prelude

import Effect (Effect)
import Main (FileObject(..), dirSizes, size)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest do
    suite "Test size" do
      test "Single file" do
          Assert.equal 45 (size $ File 45)
      test "Single dir" do
        Assert.equal 100 (size $ Dir [File 45, File 55])
      test "Nested dir" do
        Assert.equal 100 (size $ Dir [Dir [File 45], File 55])
    suite "Test dirSizes" do
      test "Single file" do
          Assert.equal [] (dirSizes $ File 45)
      test "Dir" do
          Assert.equal [45] (dirSizes $ Dir [File 45])
      test "Nested dirs" do
          Assert.equal [100, 45, 55] (dirSizes $ Dir [Dir [File 45], Dir [File 55]])
