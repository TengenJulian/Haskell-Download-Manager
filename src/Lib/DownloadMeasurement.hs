module Lib.DownloadMeasurement
  -- (
  --   updateMeasurement
  -- , DownloadMeasurement (..)
  -- , MeasurementError (..)
  -- , emptyDownloadMeasurement
  -- )
where

import qualified System.Clock as CL
import Data.Int (Int64)
import Data.List (sort, drop)

numSamples :: Num a => a
numSamples = 20

secInMs :: Num a => a
secInMs = 1000000

timeoutThreshold :: Num a => a
timeoutThreshold = 120 * secInMs

-- Returns absolute delta time in microseconds
deltaTime :: CL.TimeSpec -> CL.TimeSpec -> Int64
deltaTime t1 t2
  | t1 < t2   = deltaTime t2 t1
  -- 500 is added to round the nanoseconds to the nearest microsecond
  -- signum is done last, because div truncate towards negative infinity
  | otherwise = 1000000 * (CL.sec t1 - CL.sec t2) + signum dn * ((500 + abs dn) `div` 1000)
  where dn = CL.nsec t1 - CL.nsec t2

median :: [Double] -> Double
median xs
  | l == 0    = 0
  | odd l     = xs' !! (l `div` 2)
  | otherwise = let (x1 : x2: _) = drop ((l `div` 2) - 1) xs'
                in  (x1 + x2) / 2
  where l = length xs
        xs' = sort xs

-- | Simple Moving average
sma :: [Double] -> Double
sma xs = sum' / n
  where (n, sum') = foldr (\x (l, s) -> (l + 1, s + x)) (0, 0) xs

-- | Weighted Moving Average
wma :: [Double] -> Double
wma xs = sum' * 2 / ((n + 1) * n)
  where (n, sum') = foldr (\x (l, s) -> (l + 1, s + (l + 1) * x)) (0, 0) xs

data DownloadMeasurement = DownloadMeasurement
  {
    timeLastUpdate :: CL.TimeSpec
  , bytesDownloaded :: Int
  , downloadSpeed :: Double
  , duration :: Double
  , speedSamples :: [Double]
  , durationSamples :: [Double]
  } deriving (Show)

data MeasurementError = NoUpdate | Timeout deriving (Eq, Show)

emptyDownloadMeasurement :: CL.TimeSpec -> DownloadMeasurement
emptyDownloadMeasurement t = DownloadMeasurement t 0 0 0 [] []

updateMeasurement :: DownloadMeasurement -> CL.TimeSpec -> Int -> Either MeasurementError DownloadMeasurement
updateMeasurement dm newTime newBytesDownloaded
  | timeSinceUpdate > timeoutThreshold = Left Timeout
  | newBytes > 0 = Right $ dm
    {
        timeLastUpdate  = newTime
      , bytesDownloaded = newBytesDownloaded
      , downloadSpeed   = median ss
      , duration        = wma ds
      , speedSamples    = ss
      , durationSamples = ds
    }
  | newBytes == 0 && fromIntegral timeSinceUpdate > 1.3 * duration dm = Right $ dm
    {
      downloadSpeed = downloadSpeed dm * max 0 (1 - ((fromIntegral timeSinceUpdate - 1.3 * duration dm) / (5 * secInMs)))
    }
  | otherwise = Left NoUpdate
  where timeSinceUpdate = deltaTime newTime (timeLastUpdate dm)
        newBytes = newBytesDownloaded - bytesDownloaded dm

        ss = fromIntegral newBytes / (fromIntegral timeSinceUpdate / secInMs) : take (numSamples - 1) (speedSamples dm)
        ds = fromIntegral timeSinceUpdate : take (numSamples - 1) (durationSamples dm)
