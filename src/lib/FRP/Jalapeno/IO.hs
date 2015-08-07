-- | This module consists of some messing around with the GLFW library along w/
--   FRP to try to do some interactive IO stuff.
module FRP.Jalapeno.IO where

-------------
-- Imports --
import Graphics.Rendering.OpenGL
import Control.Monad.IO.Class
import Control.Concurrent
import Graphics.UI.GLFW
import Data.Time.Clock
import Data.IORef

import FRP.Jalapeno.Behavior
import FRP.Jalapeno.Sample

----------
-- Code --

-- | This function runs when GLFW wants to be closed - writing true to the
--   closed @'IORef'@.
closeCallback :: IORef Bool -> WindowCloseCallback
closeCallback closedRef = do
  writeIORef closedRef True
  return True

-- | Running a given FRP network from some @'Behavior'@.
driveNetwork :: Show a => IORef Bool -> Behavior Double IO () a -> Int -> IO ()
driveNetwork closedRef b rate = do
  ct <- getCurrentTime
  driveNetwork' ct 0 closedRef b rate
  where driveNetwork' :: Show a => UTCTime -> Double -> IORef Bool -> Behavior Double IO () a -> Int -> IO ()
        driveNetwork' lt t closedRef b rate = do
          closed <- readIORef closedRef
          case closed of
            True  -> return ()
            False -> do
              clear [ColorBuffer, DepthBuffer]
              (v, next) <- runBehavior t () b
              print v
              swapBuffers

              pollEvents

              threadDelay $ 1000000 `div` rate

              ct <- getCurrentTime
              driveNetwork' ct
                          (t + (fromRational $ toRational $ diffUTCTime ct lt))
                          closedRef
                          next
                          rate

-- | Running a given behavior at a given rate (after having constructed a GLFW
--   instance).
runNetwork :: Show a => Behavior Double IO () a -> Int -> IO ()
runNetwork b rate = do
  succ <- initialize

  case succ of
    False -> putStrLn "Failed to initialize GLFW."
    True  -> do
      openWindow (Size 640 480)
                 [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24]
                 Window

      closedRef <- newIORef False

      windowTitle         $= "Testing Jalapeno"
      windowCloseCallback $= closeCallback closedRef

      driveNetwork closedRef b rate

      closeWindow
      terminate
