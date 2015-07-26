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
data Behavior a = Behavior   (Time ->    a)
                | BehaviorIO (Time -> IO a)

instance Functor Behavior where
  fmap fn (Behavior   a) = Behavior   $ \t ->      fn (a t)
  fmap fn (BehaviorIO a) = BehaviorIO $ \t -> fmap fn (a t)

instance Applicative Behavior where
  pure a = Behavior $ \_ -> a

  -- TODO: More applicative instances.
  (Behavior fn) <*> (Behavior a) =
    Behavior $ \t ->
      fn t (a t)

instance Monad Behavior where
  return = pure

  -- Binding a Behavior that doesn't produce an IO value.
  (Behavior a) >>= fn =
    Behavior $ \t ->
      let (Behavior b) = fn (a t) in
        b t

  -- Binding a Behavior that produces an IO value.
  (BehaviorIO a) >>= fn =
    BehaviorIO $ \t -> do
      av <- a t
      case fn av of
        (Behavior   b) -> return $ b t
        (BehaviorIO b) ->          b t

instance MonadIO Behavior where
  liftIO a = BehaviorIO $ \t -> a

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

seconds :: Behavior Int
seconds =
  Behavior floor
