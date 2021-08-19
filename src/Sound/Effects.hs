module Sound.Effects where

import "base" Control.Applicative
import "this" Sound.Internal
import "this" Timed


sinwave :: Double -$> Sample
sinwave freq = toSample <$> sin (2 * pi * freq * now)

level :: Applicative f => TimedT f Double -> TimedT f Sample -> TimedT f Sample
level = liftA2 $ \ scale -> Sample . round . (scale *) . fromIntegral . unSample
