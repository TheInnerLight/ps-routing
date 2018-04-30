module Tests.Paths.Formatted.DateTime where

import Prelude

import Data.Date (Date, canonicalDate)
import Data.Date.Component (Day, Month(..), Year(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Paths (runRoute)
import Paths.Formatted.DateTime (yearMonthDayPath)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)

yearMonthDayPathTests :: ∀ e. Spec (RunnerEffects e) Unit
yearMonthDayPathTests = 
  describe "yearMonthDayPath" do
    it "returns Just of the supplied value when the supplied path is a yyyy-mm-dd format date" do
      let testConstantPath = yearMonthDayPath
      let date = unsafePartial $ fromJust $ canonicalDate <$> toEnum 2018 <*> toEnum 1 <*> toEnum 2
      runRoute id "2018-01-02" testConstantPath `shouldEqual` Just date
    it "returns Nothing when the supplied value does not match" do
      let testConstantPath = yearMonthDayPath
      runRoute id "phasers" testConstantPath `shouldEqual` Nothing

dateTimePathTests :: ∀ e. Spec (RunnerEffects e) Unit
dateTimePathTests = do
  yearMonthDayPathTests