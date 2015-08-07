-- | @'Behavior'@s to return user input from GLFW.
module FRP.Jalapeno.Input where

-------------
-- Imports --
import Control.Monad.IO.Class
import Graphics.UI.GLFW

import FRP.Jalapeno.Behavior

----------
-- Code --

-- | Determining whether if given key is pressed - wrapped in a @'Behavior'@.
keyPressed :: (MonadIO m, Enum a) => a -> Behavior t m i Bool
keyPressed = ioBehavior . fmap (== Press) . getKey
