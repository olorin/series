{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.TimeSeries
(
    TimeSeries
) where

import qualified Data.Vector as V
import Data.Vector ((!),(!?))
import Data.Vector (Vector)
import Data.Time.Clock
import Data.Word (Word64)
import Data.Maybe

newtype TimeSeries = TimeSeries {
    unTimeSeries :: Vector (TimeStamp, Double)
}

newtype TimeStamp = TimeStamp {
    unTimeStamp :: Word64
} deriving (Num,Ord,Eq,Enum,Integral,Real)

type Interpolator = (Double -> Double -> Double -> Double)

type SampleInterval = TimeStamp

mkTimeSeries ::
    [(TimeStamp, Double)] ->
    SampleInterval ->
    Interpolator ->
    TimeSeries
mkTimeSeries points period interp = TimeSeries $ V.fromList points

align ::
    Vector (TimeStamp, Double) ->
    SampleInterval ->
    Interpolator ->
    Vector (TimeStamp, Double)
align s h interp = undefined
  where
    finalVector = let (start, end) = timeRange (TimeSeries s) in
                  V.fromList $ [(t, 0.0) | t <- [start + h, start + (2*h)..end] ]

    interpTime start i = (+)
                         ((*) h (ceiling (start / (fromIntegral h))))
                         (i * h)


timeParam :: TimeStamp -> TimeStamp -> SampleInterval -> Double
timeParam t1 t2 ti = fromIntegral (ti - t1) / fromIntegral (t2 - t1)

-- | lerp returns the linear interpolation of two points x and y with time
--   parameter alpha.
lerp :: Interpolator
lerp x y alpha =
    ((1.0 - alpha) * x) + (alpha * y)

minute :: SampleInterval
minute = 60 * nanoScale

hour :: SampleInterval
hour = 3600 * nanoScale

nanoScale :: TimeStamp
nanoScale = TimeStamp 1000000000

timeRange :: TimeSeries -> (TimeStamp, TimeStamp)
timeRange (TimeSeries s) = (fst (s ! 0), fst (s ! ((V.length s) - 1)))
