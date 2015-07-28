-- | The module containing information on the @'Behavior'@ type.
module FRP.Jalapeno.Behavior where

-------------
-- Imports --
import Control.Monad.IO.Class
import Control.Applicative
import Data.Monoid

----------
-- Code --

-- | A placeholder for the continuous value of time later to come.
type Time = Double

-- | A continuous behavior along a stream of time.
data Behavior a = Behavior (Time -> IO a)

instance Functor Behavior where
  fmap fn (Behavior a) = Behavior $ \t -> fmap fn (a t)

instance Applicative Behavior where
  pure a = Behavior $ \_ -> return a

  (Behavior fn) <*> (Behavior a) =
    Behavior $ \t -> do
      fnv <- fn t
      av  <- a  t

      return $ fnv av

instance Monad Behavior where
  return = pure

  (Behavior a) >>= fn =
    Behavior $ \t -> do
      av <- a t
      case fn av of
        (Behavior b) -> b t

instance MonadIO Behavior where
  liftIO a = Behavior $ \t -> a

-- | The @'Num'@ instance is really just a bunch of the appropriate function
--   applications to the @'Applicative'@ instance.
instance Num a => Num (Behavior a) where
  ba + bb = (+) <$> ba <*> bb
  ba * bb = (*) <$> ba <*> bb
  abs b = abs <$> b
  signum b = signum <$> b
  fromInteger = pure . fromInteger
  negate b = negate <$> b

-- | The @'Monoid'@ instance - similar to the @'Num'@ instance is just a lot of
--   function applications to the @'Applicative'@ instance.
instance Monoid a => Monoid (Behavior a) where
  mempty = pure mempty

  mappend ba bb = mappend <$> ba <*> bb

-- | Constructing a @'Behavior'@ from a pure function.
behavior :: (Time -> a) -> Behavior a
behavior fn = Behavior $ return . fn

-- | Getting the amount of seconds that have passed.
seconds :: Behavior Int
seconds = behavior floor

-- | Taking the integral of a @'Fractional'@ value over time, contained inside
--   of a @'Behavior'@.
integral :: Real a => a -> Behavior Double
integral n =
  Behavior $ \t ->
    return (realToFrac n * t)
