{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.TimeSeries
(
    TimeSeries
) where

import Data.Vector
import Data.Time.Clock
import Data.Word (Word64)

newtype TimeSeries = TimeSeries {
    unTimeStamp :: Vector (UTCTime, Double)
}

type Interpolator = (Double -> Double -> Double -> Double)

mkTimeSeries ::
    [(UTCTime, Double)] ->
    NominalDiffTime ->
    Interpolator ->
    TimeSeries
mkTimeSeries points period interp = TimeSeries $ fromList points

linear :: Interpolator
linear x y alpha =
    ((1.0 - alpha) * x) + (alpha * y)
