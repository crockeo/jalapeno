-- | The module containing information on the @'Behavior'@ type.
module FRP.Jalapeno.Behavior ( Time
                             , Behavior (..)
                             , runBehavior
                             , integral
                             ) where

-------------
-- Imports --
import Control.Monad.IO.Class
import Control.Applicative
import Data.Monoid

----------
-- Code --

-- | Mapping a function onto a tuple of a value and a functor of that value.
--   Used in the @'Applicative'@ instance a number of times.
mapTuple :: Functor f => (a -> b) -> (a, f a) -> (b, f b)
mapTuple fn (a, fa) = (fn a, fmap fn fa)

-- | A placeholder for the continuous value of time later to come.
type Time = Double

-- | A continuous behavior along a stream of time.
data Behavior m a = BehaviorM (Time -> m (a, Behavior m a))
                  | BehaviorP (Time ->   (a, Behavior m a))

instance Functor m => Functor (Behavior m) where
  fmap fn (BehaviorM a) =
    BehaviorM $ \t ->
      fmap (\(v, b) -> (fn v, fmap fn b)) (a t)

  fmap fn (BehaviorP a) =
    BehaviorP $ \t ->
           (\(v, b) -> (fn v, fmap fn b)) (a t)

instance Applicative m => Applicative (Behavior m) where
  pure a = BehaviorP $ \_ -> (a, pure a)

  (BehaviorM fn) <*> (BehaviorM a) =
    BehaviorM $ \t ->
      mapTuple <$> fmap fst (fn t) <*> a t

  (BehaviorP fn) <*> (BehaviorP a) =
    BehaviorP $ \t ->
      mapTuple (fst $ fn t) (a t)

  (BehaviorM fn) <*> (BehaviorP a) =
    BehaviorM $ \t ->
      mapTuple <$> fmap fst (fn t) <*> pure (a t)

  (BehaviorP fn) <*> (BehaviorM a) =
    BehaviorM $ \t ->
      mapTuple <$> pure (fst $ fn t) <*> a t

instance Monad m => Monad (Behavior m) where
  return a = BehaviorP $ \_ -> (a, return a)

  (BehaviorM a) >>= fn =
    BehaviorM $ \t -> do
      (av, _) <- a t
      case fn av of
        (BehaviorM b) ->          b t
        (BehaviorP b) -> return $ b t

  (BehaviorP a) >>= fn =
    BehaviorM $ \t -> do
      let (av, _) = a t in
        case fn av of
          (BehaviorM b) ->          b t
          (BehaviorP b) -> return $ b t

instance MonadIO m => MonadIO (Behavior m) where
  liftIO a =
    BehaviorM $ \t ->
      liftIO a >>= return . (flip (,)) (liftIO a)

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
runBehavior :: Monad m => Time -> Behavior m a -> m (a, Behavior m a)
runBehavior t b =
  case b of
    (BehaviorM fn) ->          fn t
    (BehaviorP fn) -> return $ fn t

-- | Taking the integral of a given @'Behavior'@ over time.
integral :: (Monad m, Real a) => Behavior m a -> Behavior m Double
integral   (BehaviorP a) =
  BehaviorP $ \t ->
    case a t of
      (av, ab) -> (realToFrac av * t, integral ab)
integral b@(BehaviorM a) =
  integral' 0 0 b
  where integral' :: (Monad m, Real a) => Time -> Double -> Behavior m a -> Behavior m Double
        integral' lt lv (BehaviorM a) =
          BehaviorM $ \t -> do
            (av, ab) <- a t
            let nv = lv + realToFrac av * (t - lt) in
              return (nv, integral' t nv ab)
