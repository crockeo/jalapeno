-- | This module consists of some messing around with the GLFW library along w/
--   FRP to try to do some interactive IO stuff.
module FRP.Jalapeno.IO where

-------------
-- Imports --
import Control.Monad.IO.Class
import Graphics.UI.GLFW

import FRP.Jalapeno.Behavior
import FRP.Jalapeno.Sample

----------
-- Code --

-- | Checking whether or not a key is pressed.
keyPressed :: Enum a => a -> Behavior Bool
keyPressed = liftIO . fmap (== Press) . getKey

-- | Performing some tests around GLFW and Jalapeno's FRP.
testing :: IO ()
testing = do
  succ <- initialize

  case succ of
    False -> putStrLn "Failed to initialize GLFW."
    True  -> do
      sample $ keyPressed $ CharKey 'W'
      terminate
