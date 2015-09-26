module IX.Reactive.Utils
   (timer
   ,mkRoll
   ,fromRight
   ,fromLeft)
   
   where

import DataStructures.Atomic
import IX.Universe.Utils (intToPInt)
import Control.Concurrent (threadDelay)
import System.Random

timer :: TimeOut -> IO ()
timer (PInt ms) = do
   threadDelay ms

mkRoll :: StdGen -> [PInt]
mkRoll gen = map intToPInt $ randomRs (1,100) gen

fromRight :: Either a b -> Maybe b
fromRight (Right b) = Just b
fromRight _         = Nothing

fromLeft :: Either a b -> Maybe a
fromLeft (Left a) = Just a
fromLeft _        = Nothing

