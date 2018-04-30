module Paths.Formatted.DateTime where

import Prelude
import Data.DateTime (Date, DateTime, date)
import Data.Either (hush)
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime as FDT
import Data.List (fromFoldable)
import Paths (ParsePath(..))

yearMonthDayFormatter :: FDT.Formatter 
yearMonthDayFormatter = 
  fromFoldable 
    [ YearFull
    , Placeholder "-"
    , MonthTwoDigits
    , Placeholder "-"
    , DayOfMonthTwoDigits
    ]

-- | Accepts a route of the supplied DateTime.Formatter
formattedDateTimeR :: FDT.Formatter -> ParsePath DateTime
formattedDateTimeR formatter = 
  ParsePath (hush <<< FDT.unformat formatter)

-- | Accepts a route of the form YYYY-MM-DD
yearMonthDayR :: ParsePath Date 
yearMonthDayR =
  ParsePath (map (date) <<< hush <<< FDT.unformat yearMonthDayFormatter)