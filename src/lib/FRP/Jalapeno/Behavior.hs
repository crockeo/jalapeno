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
data Behavior m a = BehaviorM (Time -> m a)
                  | BehaviorP (Time ->   a)

instance Functor m => Functor (Behavior m) where
  fmap fn (BehaviorM a) = BehaviorM $ \t -> fmap fn (a t)
  fmap fn (BehaviorP a) = BehaviorP $ \t ->      fn (a t)

instance Applicative m => Applicative (Behavior m) where
  pure a = BehaviorP $ \_ -> a

  (BehaviorM fn) <*> (BehaviorM a) =
    BehaviorM $ \t ->
      fn t <*> a t

  (BehaviorP fn) <*> (BehaviorP a) =
    BehaviorP $ \t ->
      fn t (a t)

  (BehaviorM fn) <*> (BehaviorP a) =
    BehaviorM $ \t ->
      fn t <*> pure (a t)

  (BehaviorP fn) <*> (BehaviorM a) =
    BehaviorM $ \t ->
      fn t <$> a t

instance Monad m => Monad (Behavior m) where
  return a = BehaviorP $ \_ -> a

  (BehaviorM a) >>= fn =
    BehaviorM $ \t -> do
      av <- a t
      case fn av of
        (BehaviorM b) ->          b t
        (BehaviorP b) -> return $ b t

  (BehaviorP a) >>= fn =
    BehaviorM $ \t -> do
      case fn $ a t of
        (BehaviorM b) ->          b t
        (BehaviorP b) -> return $ b t


instance MonadIO m => MonadIO (Behavior m) where
  liftIO a = BehaviorM $ \t -> liftIO a

-- | The @'Num'@ instance is really just a bunch of the appropriate function
--   applications to the @'Applicative'@ instance.
instance (Applicative m, Num a) => Num (Behavior m a) where
  ba + bb     = (+) <$> ba <*> bb
  ba * bb     = (*) <$> ba <*> bb
  abs b       = abs <$> b
  signum b    = signum <$> b
  fromInteger = pure . fromInteger
  negate b    = negate <$> b

-- | The @'Monoid'@ instance - similar to the @'Num'@ instance is just a lot of
--   function applications to the @'Applicative'@ instance.
instance (Applicative m, Monoid a) => Monoid (Behavior m a) where
  mempty = pure mempty

  mappend ba bb = mappend <$> ba <*> bb

-- | Running a @'Behavior'@ at a given point in time.
runBehavior :: (Monad m) => Time -> Behavior m a -> m a
runBehavior t b =
  case b of
    (BehaviorM fn) ->          fn t
    (BehaviorP fn) -> return $ fn t

-- | Getting the amount of seconds that have passed.
seconds :: Behavior m Int
seconds = BehaviorP floor

-- | Taking the integral of a @'Fractional'@ value over time, contained inside
--   of a @'Behavior'@.
integral :: (Monad m, Real a) => a -> Behavior m Double
integral n =
  BehaviorP $ \t ->
    realToFrac n * t

-- | Trying to perform @'integral'@ in such a way that would perform stateful
--   integration.
integralM :: (Monad m, Real a) => Behavior m a -> Behavior m Double
integralM (BehaviorP a) = BehaviorP $ \t -> realToFrac (a t) * t
integralM (BehaviorM a) = undefined -- TODO
