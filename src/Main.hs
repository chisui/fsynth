module Main where

import "base" Control.Concurrent
import "base" Control.Monad
import "this" Sound


main :: IO ()
main = do
   play . level 0.1 $ (sinwave 440 + sinwave 880)
   forever (threadDelay maxBound)
