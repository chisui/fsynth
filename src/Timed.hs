module Timed where

import "base" Control.Applicative


type Time = Double
data TimedT m a where
    TimedT :: m x -> (x -> Time -> m a) -> TimedT m a

instance Functor m => Functor (TimedT m) where
    fmap f (TimedT i g) = TimedT i (\x t -> f <$> g x t)
instance Applicative m => Applicative (TimedT m) where
    pure a = TimedT (pure ()) (\ _ _ -> pure a)
    liftA2 f (TimedT ia fa) (TimedT ib fb) = TimedT (liftA2 (,) ia ib) $ \(a, b) x -> liftA2 f (fa a x) (fb b x)

instance (Applicative f, Num a) => Num (TimedT f a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
    negate = fmap negate
instance (Applicative f, Fractional a) => Fractional (TimedT f a) where
    fromRational = pure . fromRational
    recip = fmap recip
    (/) = liftA2 (/)
instance (Applicative f, Floating a) => Floating (TimedT f a) where
    pi    = pure pi
    exp   = fmap exp
    log   = fmap log
    sin   = fmap sin
    cos   = fmap cos
    asin  = fmap asin
    acos  = fmap acos
    atan  = fmap atan
    sinh  = fmap sinh
    cosh  = fmap cosh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh

runTimedT :: (Traversable t, Monad f) => TimedT f a -> t Time -> f (t a)
runTimedT (TimedT i f) t = do
    a <- i
    traverse (f a) t

timedA :: Applicative f => (Time -> f a) -> TimedT f a
timedA = TimedT (pure ()) . const

timed :: Applicative f => (Time -> a) -> TimedT f a
timed f = timedA (pure . f)

now :: Applicative f => TimedT f Time
now = timed id

type (-$>) a b = forall f. Applicative f => TimedT f a -> TimedT f b
