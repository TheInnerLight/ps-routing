module Test.Paths where

import Prelude

import Data.Maybe (Maybe(..))
import Paths (constantPath, intPath, numberPath, runRoute)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)

constantPathTests :: ∀ e. Spec (RunnerEffects e) Unit
constantPathTests = 
  describe "constantPath" do
    it "returns Just of the supplied value when the supplied path matches" do
      let testConstantPath = constantPath "test"
      runRoute true "test" testConstantPath `shouldEqual` Just true
    it "returns Nothing when the supplied value does not match" do
      let testConstantPath = constantPath "cheese"
      runRoute true "biscuits" testConstantPath `shouldEqual` Nothing

intPathTests :: ∀ e. Spec (RunnerEffects e) Unit
intPathTests = 
  describe "intPath" do
    it "returns Just of the supplied value when the supplied path is an int" do
      let testPath = intPath
      runRoute id "2" testPath `shouldEqual` Just 2
    it "returns Nothing when the supplied value is a string" do
      let testPath = intPath
      runRoute id "a" testPath `shouldEqual` Nothing
    it "returns Nothing when the supplied value is a number" do
      let testPath = intPath
      runRoute id "1.5" testPath `shouldEqual` Nothing


numberPathTests :: ∀ e. Spec (RunnerEffects e) Unit
numberPathTests = 
  describe "intPath" do
    it "returns Just of the supplied value when the supplied path is an integer number" do
      let testPath = numberPath
      runRoute id "2" testPath `shouldEqual` Just 2.0
    it "returns Just of the supplied value when the supplied path is a real number" do
      let testPath = numberPath
      runRoute id "7.123214234" testPath `shouldEqual` Just 7.123214234
    it "returns Nothing when the supplied value does not match" do
      let testPath = numberPath
      runRoute id "a" testPath `shouldEqual` Nothing

pathTests :: ∀ e. Spec (RunnerEffects e) Unit
pathTests = do
  constantPathTests
  intPathTests
  numberPathTests