-- | The entry point to testing this library.
module Main where

-------------
-- Imports --
import Control.Applicative
import Graphics.UI.GLFW
import FRP.Jalapeno

----------
-- Code --

-- | A single axis of speed.
axisSpeed :: Enum a => a -> a -> Float -> Behavior Float
axisSpeed posKey minKey speed =
  axisSpeed' <$> keyPressed posKey
             <*> keyPressed minKey
             <*> pure speed
  where axisSpeed' :: Bool -> Bool -> Float -> Float
        axisSpeed'  True False speed =  speed
        axisSpeed' False  True speed = -speed
        axisSpeed'     _     _ speed =      0

-- | The current speed of the player.
speed :: Behavior (Float, Float)
speed = (,) <$> axisSpeed (CharKey 'W') (CharKey 'S') 30
            <*> axisSpeed (CharKey 'D') (CharKey 'A') 30

-- | The current acceleration of the player.
acceleration :: Behavior (Float, Float)
acceleration = undefined

-- | The current position of the player.
position :: Behavior (Float, Float)
position = undefined

-- | The entry point to the application.
main :: IO ()
main = runNetwork speed 20