module Sound.Internal where

import Prelude hiding ((.), id)
import "base" Data.Foldable
import "base" Data.Int (Int32)
import "base" Control.Category
import "base" Control.Concurrent
import "base" GHC.Stack
import "stm" Control.Concurrent.STM
import "sdl2" SDL qualified
import "vector" Data.Vector.Storable.Mutable qualified as V
import "vector" Data.Vector.Unboxed qualified as VU

import "this" Timed


newtype Sample = Sample
    { unSample :: Int32
    }
  deriving newtype (Eq, Ord, Show, Read, Num, Enum, Bounded, Real, Integral)

toSample :: Double -> Sample
toSample = Sample . round . (sampleScale *)
  where sampleScale = fromIntegral (maxBound `div` 2 :: Int32)

devFreq :: Num a => a
devFreq = 48000

devSamples :: Num a => a
devSamples = 4096 * 2

play :: TimedT IO Sample -> IO ()
play (TimedT i f) = do
    SDL.initializeAll
    a <- i
    queue <- newTBQueueIO 20
    forkIO $ pushBuffer queue a ticks
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
        for_ ([0 .. VU.length bytes - 1] :: [Int]) $ \j -> V.write out j (bytes VU.! j)
    popBuffer _ fmt _ = error $ "Unsupported audio format: " ++ show fmt
    pushBuffer queue a ts = do
        let (chunk_ts, ts') = splitAt devSamples ts
        chunk <- fmap unSample <$> traverse (f a) chunk_ts
        atomically . writeTBQueue queue . VU.fromList $ chunk
        pushBuffer queue a ts'
    ticks = [fromIntegral t / devFreq | t <- [0 :: Int32 ..]]
