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
axisSpeed :: Enum a => a -> a -> Float -> Behavior IO Float
axisSpeed posKey minKey speed =
  axisSpeed' <$> keyPressed posKey
             <*> keyPressed minKey
             <*> pure speed
  where axisSpeed' :: Bool -> Bool -> Float -> Float
        axisSpeed'  True False speed =  speed
        axisSpeed' False  True speed = -speed
        axisSpeed'     _     _ speed =      0

-- | Finding the position on a given axis by the current speed of acceleration
--   or deceleration.
axisPosition :: Enum a => a -> a -> Float -> Behavior IO Double
axisPosition posKey minKey speed =
  integral $
    integral $
      axisSpeed posKey minKey speed

-- | Finding the X & Y position of the user.
position :: Float -> Behavior IO (Double, Double)
position speed = do
  (,) <$> axisPosition (CharKey 'D') (CharKey 'A') speed
      <*> axisPosition (CharKey 'W') (CharKey 'S') speed
  {-x <- axisPosition (CharKey 'D') (CharKey 'A') speed-}
  {-y <- axisPosition (CharKey 'W') (CharKey 'S') speed-}

  {-return (x, y)-}

{-

axisPosition (CharKey 'D') (CharKey 'A') speed >>= (\x ->
axisPosition (CharKey 'W') (CharKey 'S') speed >>= (\y ->
return (x, y)))

-}

-- ... desugars to:
{-mx >>= (\x ->-}
{-my >>= (\y ->-}
{-z ))-}

-- | The entry point to the application.
main :: IO ()
main = runNetwork (position 50) 20
