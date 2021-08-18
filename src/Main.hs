module Main where

import Prelude hiding ((.))
import "base" Data.Foldable
import "base" Data.Int (Int32)
import "base" Control.Category
import "base" Control.Monad
import "base" Data.IORef
import "base" Control.Concurrent
import "base" GHC.Stack
import "stm" Control.Concurrent.STM
import "dunai" Data.MonadicStreamFunction
import "dunai" Data.MonadicStreamFunction.Async
import "dunai" Data.MonadicStreamFunction.ReactHandle
import "sdl2" SDL qualified
import "vector" Data.Vector.Storable.Mutable qualified as V
import "vector" Data.Vector.Unboxed qualified as VU


type (-$>) = MSF IO
type Time = Double
newtype Sample = Sample
    { unSample :: Int32
    }
  deriving newtype (Eq, Ord, Show, Read, Num, Enum, Bounded, Real, Integral)


toSample :: Double ->  Sample
toSample = Sample . round . (sampleScale *)
  where sampleScale = fromIntegral (maxBound `div` 2 :: Int32)

sinwave :: Double -> Time -$> Sample
sinwave freq = arr (toSample . sin . (2 *) . (pi *) . (freq *))


audioCB :: IORef [Int32] -> SDL.AudioFormat sampleType -> V.IOVector sampleType -> IO ()
audioCB samples format buffer =
    case format of
       SDL.Signed32BitLEAudio -> do
                samples' <- readIORef samples
                let n = V.length buffer
                zipWithM_ (V.write buffer) [0 ..] (take n samples')
                writeIORef samples (drop n samples')
       _ -> error "Unsupported audio format"

main :: IO ()
main = do
   play (sinwave 440)
   forever (threadDelay maxBound)

devFreq :: Num a => a
devFreq = 48000

devSamples :: Num a => a
devSamples = 4096 * 2

play :: Time -$> Sample -> IO ()
play f = do
    SDL.initializeAll
    queue <- newTBQueueIO 20
    forkIO $ pushBuffer queue
    (device,_) <- SDL.openAudioDevice SDL.OpenDeviceSpec
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
    pushBuffer :: HasCallStack => TBQueue (VU.Vector Int32) -> IO ()
    pushBuffer queue = do
        buf <- newIORef VU.empty
        let push (Sample s) = modifyIORef' buf (<> VU.singleton s)
        handle <- reactInit (arrM push . f . time)
        forever $ do
            for_ ([0 .. devSamples - 1] :: [Int]) (const (react handle))
            bytes <- atomicModifyIORef' buf (VU.empty,)
            atomically . writeTBQueue queue $ bytes

    time = concatS . pure $ (/ devFreq) . fromIntegral <$> [0 :: Int32 ..]
