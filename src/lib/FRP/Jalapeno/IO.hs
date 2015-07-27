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

-- | Checking whether or not a key is pressed.
keyPressed :: Enum a => a -> Behavior Bool
keyPressed = liftIO . fmap (== Press) . getKey

-- | This function runs when GLFW wants to be closed - writing true to the
--   closed @'IORef'@.
closeCallback :: IORef Bool -> WindowCloseCallback
closeCallback closedRef = do
  writeIORef closedRef True
  return True

-- | Running a given FRP network from some @'Behavior'@.
runNetwork :: Show a => IORef Bool -> Behavior a -> Int -> IO ()
runNetwork closedRef b rate = do
  ct <- getCurrentTime
  runNetwork' ct 0 closedRef b rate
  where runNetwork' :: Show a => UTCTime -> Double -> IORef Bool -> Behavior a -> Int -> IO ()
        runNetwork' lt t closedRef b@(Behavior fn) rate = do
          closed <- readIORef closedRef
          case closed of
            True  -> return ()
            False -> do
              clear [ColorBuffer, DepthBuffer]
              fn t >>= print
              swapBuffers

              pollEvents

              threadDelay $ 1000000 `div` rate

              ct <- getCurrentTime
              runNetwork' ct
                          (t + (fromRational $ toRational $ diffUTCTime ct lt))
                          closedRef
                          b
                          rate

-- | Performing some tests around GLFW and Jalapeno's FRP.
testNetwork :: IO ()
testNetwork = do
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

      runNetwork closedRef (keyPressed $ CharKey 'W') 20

      closeWindow
      terminate
