module Main where

import Prelude hiding ((.))
import "base" Data.Tuple
import "base" Data.Foldable
import "base" Control.Category
import "base" Data.IORef
import "base" Control.Monad
import "base" Control.Concurrent
import "base" Data.Int (Int32)
import "dunai" Data.MonadicStreamFunction
import "dunai" Data.MonadicStreamFunction.Async
import "sdl2" SDL qualified
import "vector" Data.Vector.Storable.Mutable qualified as V
import "vector" Data.Vector.Unboxed qualified as VU
import GHC.Stack

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
    buf <- newIORef VU.empty
    forkIO . reactimate $ (arrM (pushBuffer buf). f . time)
    (device,_) <- SDL.openAudioDevice SDL.OpenDeviceSpec
        { SDL.openDeviceFreq     = SDL.Mandate devFreq
        , SDL.openDeviceFormat   = SDL.Mandate SDL.Signed32BitNativeAudio
        , SDL.openDeviceChannels = SDL.Mandate SDL.Mono
        , SDL.openDeviceSamples  = devSamples
        , SDL.openDeviceCallback = popBuffer buf
        , SDL.openDeviceUsage    = SDL.ForPlayback
        , SDL.openDeviceName     = Nothing
        }
    SDL.setAudioDevicePlaybackState device SDL.Play
    pure ()
  where
    popBuffer :: HasCallStack => IORef (VU.Vector Int32) -> SDL.AudioFormat a -> V.IOVector a -> IO ()
    popBuffer buf SDL.Signed32BitLEAudio out = do
        let l = V.length out
        bytes <- atomicModifyIORef' buf (swap . VU.splitAt l)
        let l_bytes = VU.length bytes
        for_ ([0 .. min l l_bytes - 1] :: [Int]) $ \i -> V.write out i (bytes VU.! i)
    popBuffer _ _ _ = error "Unsupported audio format"
    pushBuffer :: HasCallStack => IORef (VU.Vector Int32) -> Sample -> IO ()
    pushBuffer buf (Sample s) = do
            let wait = do
                    l_buf <- VU.length <$> readIORef buf
                    if devFreq * 20 <= l_buf then do
                        threadDelay 50
                        wait
                    else pure ()
            wait
            modifyIORef buf . mappend . VU.singleton $ s
    time = concatS . pure $ (/ devFreq) . fromIntegral <$> [0 :: Int32 ..]
