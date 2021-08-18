module Main where

import Prelude hiding ((.), id)
import "base" Data.Foldable
import "base" Data.Int (Int32)
import "base" Control.Category
import "base" Control.Applicative
import "base" Control.Monad
import "base" Control.Concurrent
import "base" GHC.Stack
import "stm" Control.Concurrent.STM
import "dunai" Data.MonadicStreamFunction hiding (arrM)
import "sdl2" SDL qualified
import "vector" Data.Vector.Storable.Mutable qualified as V
import "vector" Data.Vector.Unboxed qualified as VU


type Time = Double
newtype (-$>) a b = Synth
    { runSynth :: Time -> a -> b
    }
  deriving stock Functor
instance Applicative ((-$>) a) where
    pure = Synth . pure . pure
    (<*>) = ap
instance Monad ((-$>) a) where
    Synth f >>= g = Synth (\ x a -> runSynth (g (f x a)) x a)
instance Category (-$>) where
    Synth f . Synth g = Synth (\ x -> f x . g x)
    id = arr id
instance Arrow (-$>) where
    arr = Synth . const
    Synth f *** Synth g = Synth (\ x (a, b) -> (f x a, g x b))

arrM :: (a -> () -$> b) -> a -$> b
arrM f = Synth $ \t a -> runSynth (f a) t ()

instance Num b => Num (a -$> b) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
    negate = fmap negate
instance Fractional b => Fractional (a -$> b) where
    fromRational = pure . fromRational
    recip = fmap recip
    (/) = liftA2 (/)
instance Floating b => Floating (a -$> b) where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh


newtype Sample = Sample
    { unSample :: Int32
    }
  deriving newtype (Eq, Ord, Show, Read, Num, Enum, Bounded, Real, Integral)


toSample :: Double ->  Sample
toSample = Sample . round . (sampleScale *)
  where sampleScale = fromIntegral (maxBound `div` 2 :: Int32)

sinwave :: Double -$> Sample
sinwave = Synth $ \ t freq -> toSample $ sin (2 * pi * freq * t)


main :: IO ()
main = do
   play (sinwave . pure 440)
   forever (threadDelay maxBound)

devFreq :: Num a => a
devFreq = 48000

devSamples :: Num a => a
devSamples = 4096 * 2

play :: () -$> Sample -> IO ()
play (Synth f) = do
    SDL.initializeAll
    queue <- newTBQueueIO 20
    forkIO $ pushBuffer queue stream
    device <- fst <$> SDL.openAudioDevice SDL.OpenDeviceSpec
        { SDL.openDeviceFreq     = SDL.Mandate devFreq
        , SDL.openDeviceFormat   = SDL.Mandate SDL.Signed32BitNativeAudio
        , SDL.openDeviceChannels = SDL.Mandate SDL.Mono
        , SDL.openDeviceSamples  = devSamples
        , SDL.openDeviceCallback = popBuffer queue
        , SDL.openDeviceUsage    = SDL.ForPlayback
        , SDL.openDeviceName     = Nothing
        }
    SDL.setAudioDevicePlaybackState device SDL.Play
    pure ()
  where
    popBuffer :: HasCallStack => TBQueue (VU.Vector Int32) -> SDL.AudioFormat a -> V.IOVector a -> IO ()
    popBuffer queue SDL.Signed32BitLEAudio out = do
        bytes <- atomically . readTBQueue $ queue
        for_ ([0 .. VU.length bytes - 1] :: [Int]) $ \i -> V.write out i (bytes VU.! i)
    popBuffer _ _ _ = error "Unsupported audio format"
    pushBuffer queue s = do
        let (chunk, s') = splitAt devSamples s
        atomically . writeTBQueue queue . VU.fromList $ chunk
        pushBuffer queue s'
    stream = [unSample $ f (fromIntegral t / devFreq) () | t <- [0 :: Int32 ..]]
