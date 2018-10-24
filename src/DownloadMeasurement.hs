module DownloadMeasurement
  -- (
  --   updateMeasurement
  -- , DownloadMeasurement (..)
  -- , MeasurementError (..)
  -- , emptyDownloadMeasurement
  -- )
where

import qualified System.Clock as CL
import Data.Int (Int64)

import Util

microSecPerSec :: Num a => a
microSecPerSec = 1000000

chunkTimeout :: Num a => a
chunkTimeout = 5 * microSecPerSec

seqTuple3 :: (a ,b, c) -> (a, b,c )
seqTuple3 (a, b, c) = a `seq` b `seq` c `seq` (a, b, c)

simpleLinRegression :: [(Double, Double)] -> (Double, Double)
simpleLinRegression xy = (yMean - m * xMean, m)
  where m = xySum / xxSum

        (xSum, ySum, n) =
          let f (x, y) (xSum', ySum', n') = seqTuple3 (xSum' + x, ySum' + y, n' + 1)
          in foldr f (0, 0, 0) xy

        xMean = xSum / n
        yMean = ySum / n

        (xySum, xxSum, _) =
          let f (x, y) (xySum', xxSum', yySum') = seqTuple3
                (xySum' + (x - xMean) * (y - yMean)
                , xxSum' + (x - xMean) ^^ (2 :: Int)
                , yySum' + (y - yMean) ^^ (2 :: Int))
          in foldr f (0, 0, 0) xy

numSamples :: Num a => a
numSamples = 45

timeoutThreshold :: Num a => a
timeoutThreshold = 120 * microSecPerSec

-- Returns absolute delta time in microseconds
deltaTime' :: CL.TimeSpec -> CL.TimeSpec -> Int64
deltaTime' t1 t2
  | t1 < t2   = deltaTime t2 t1
  -- 500 is added to round the nanoseconds to the nearest microsecond
  -- signum is done last, because div truncate towards negative infinity
  | otherwise = 1000000 * (CL.sec t1 - CL.sec t2) + signum dn * ((500 + abs dn) `div` 1000)
  where dn = CL.nsec t1 - CL.nsec t2

deltaTime :: CL.TimeSpec -> CL.TimeSpec -> Int64
deltaTime t1 t2
  | t1 < t2   = deltaTime t2 t1
  -- 500 is added to round the nanoseconds to the nearest microsecond
  -- signum is done last, because div truncate towards negative infinity
  | otherwise = 1000000 * (CL.sec t1 - CL.sec t2) + round (fromIntegral dn / 1000 :: Double)
  where dn = CL.nsec t1 - CL.nsec t2

data DownloadMeasurement = DownloadMeasurement
  {
    timeLastUpdate :: CL.TimeSpec
  , bytesDownloaded :: Int
  , etaEq :: (Double, Double)
  , samples :: [(Double, Double)]
  } deriving (Show)

data MeasurementError = NoUpdate | Timeout deriving (Eq, Show)

emptyDownloadMeasurement :: CL.TimeSpec -> DownloadMeasurement
emptyDownloadMeasurement t = DownloadMeasurement t 0 (0, 0) []

downloadSpeed :: DownloadMeasurement -> Double
downloadSpeed = snd . etaEq

updateMeasurement :: DownloadMeasurement -> CL.TimeSpec -> Int -> Either MeasurementError DownloadMeasurement
updateMeasurement dm newTime newBytesDownloaded
  | dt > chunkTimeout = Right $ (emptyDownloadMeasurement newTime)
    {
      timeLastUpdate    = newTime
      , bytesDownloaded = newBytesDownloaded
    }
  | newBytesDownloaded == bytesDownloaded dm = Left NoUpdate
  | otherwise = Right $ dm
    { timeLastUpdate  = newTime
    , bytesDownloaded = newBytesDownloaded
    , etaEq           = if enoughSamples ss
                        then simpleLinRegression ss
                        else (0, 0)
    , samples         = ss
    }
  where ss = (toSecs newTime, fromIntegral newBytesDownloaded) : take (numSamples - 1) (samples dm)

        dt = deltaTime newTime (timeLastUpdate dm)

        enoughSamples (_ : _ : _) = True
        enoughSamples _           = False
