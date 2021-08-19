module Sound.Effects where

import "this" Sound.Internal
import "this" Timed

sinwave :: Double -$> Sample
sinwave freq = toSample <$> sin (2 * pi * freq * now)
