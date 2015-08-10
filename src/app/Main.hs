-- | The entry point to testing this library.
{-# LANGUAGE Arrows #-}
module Main where

-------------
-- Imports --
import Prelude hiding ((.))
import Control.Applicative
import Graphics.UI.GLFW
import Control.Category
import FRP.Jalapeno

----------
-- Code --

-- | Checking if a tuple of keys to go positive and negative in a wire are
--   pressed.
keysPressed :: Enum a => a -> a -> Behavior Double IO () (Bool, Bool)
keysPressed posKey minKey =
  (,) <$> keyPressed posKey
      <*> keyPressed minKey

-- | A single axis of speed.
axisSpeed :: Float -> Behavior Double IO (Bool, Bool) Float
axisSpeed speed =
  BehaviorP $ \_ p ->
    case p of
      ( True, False) -> ( speed, axisSpeed speed)
      (False,  True) -> (-speed, axisSpeed speed)
      _              -> (     0, axisSpeed speed)

-- | The position on a given axis.
axisPosition :: Enum a => a -> a -> Float -> Behavior Double IO () Double
axisPosition posKey minKey speed =
  integral $ integral $ axisSpeed speed . keysPressed posKey minKey

-- | Finding the X & Y position of the user.
position :: Float -> Behavior Double IO () (Double, Double)
position speed =
  (,) <$> axisPosition (CharKey 'D') (CharKey 'A') speed
      <*> axisPosition (CharKey 'W') (CharKey 'S') speed

-- | The entry point to the application.
main :: IO ()
main = runNetwork (position 50) 20
