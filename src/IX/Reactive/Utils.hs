module IX.Reactive.Utils
   (timer
   ,mkRoll
   ,fromRight
   ,fromLeft
   ,asIS)
   
   where

import DataStructures.Atomic
import IX.Universe.Utils (intToPInt)
import Control.Concurrent (threadDelay)
import System.Random
import qualified Data.Map.Strict as M

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

asIS :: Ord a => M.Map a b -> M.Map a b -> M.Map a b
asIS m1 m2 = M.union m1 m2  
