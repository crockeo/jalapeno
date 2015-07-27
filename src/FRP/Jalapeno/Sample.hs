module FRP.Jalapeno.Sample where

-------------
-- Imports --
import Control.Concurrent
import Data.Time.Clock

import FRP.Jalapeno.Behavior

----------
-- Code --

-- | Sampling a @'Behavior'@ in real time at a maximum number of times per
--   second.
intermittentSample :: Show a => Behavior a -> Int -> IO ()
intermittentSample b rate = do
  ct <- getCurrentTime
  intermittentSample' ct 0 b rate
  where intermittentSample' :: Show a => UTCTime -> Double -> Behavior a -> Int -> IO ()
        intermittentSample' lt t b@(Behavior fn) rate = do
          fn t >>= print

          -- Non-reactive delay. Could fix later to make sure that if it's
          -- running slowly it doesn't apply the delay.
          threadDelay $ 1000000 `div` rate

          ct <- getCurrentTime
          intermittentSample' ct
                              (t + (fromRational $ toRational $ diffUTCTime ct lt))
                              b
                              rate

-- | Sampling a @'Behavior'@ at around 100 samples per second.
sample :: Show a => Behavior a -> IO ()
sample b = intermittentSample b 60
