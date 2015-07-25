module FRP.Jalapeno.Sample where

-------------
-- Imports --
import Data.Time.Clock

import FRP.Jalapeno.Behavior

----------
-- Code --

-- | Sampling a @'Behavior'@ in real time as fast as the computer can manage.
sample :: Show a => Behavior a -> IO ()
sample b = do
  ct <- getCurrentTime
  sample' ct 0 b
  where sample' :: Show a => UTCTime -> Double -> Behavior a -> IO ()
        sample' lt t b@(Behavior fn) = do
          print $ fn t

          ct <- getCurrentTime
          sample' ct (t + (fromRational $ toRational $ diffUTCTime ct lt)) b
