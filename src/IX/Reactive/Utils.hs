module IX.Reactive.Utils
   (timer
   ,mkRoll
   ,fromRight
   ,fromLeft
   ,asIS
   ,asIS_M
   ,asIS_MM
   ,clearBuffer)
   
   where

import DataStructures.Atomic
import DataStructures.Composite
import IX.Universe.Utils (intToPInt)
import Control.Concurrent (threadDelay)
import Control.Applicative ((<$>),(<*>))
import Reactive.Banana
import System.Random
import Debug.Trace
import qualified Data.Map.Strict as M


clearBuffer :: VAC -> VAC -> VAC
clearBuffer validated clear = trace ("clearBuffer called") clear
timer :: TimeOut -> IO ()
timer ms = do
   trace ("timer called") threadDelay ms

mkRoll :: StdGen -> [PInt]
mkRoll gen = map intToPInt $ randomRs (1,100) gen

fromRight :: Either a b -> Maybe b
fromRight (Right b) = Just b
fromRight _         = Nothing

fromLeft :: Either a b -> Maybe a
fromLeft (Left a) = Just a
fromLeft _        = Nothing

asIS :: [a] -> [a] -> [a]
asIS m1 m2 = m1 ++ m2 

asIS_M :: Ord k => M.Map k v -> M.Map k v -> M.Map k v
asIS_M m1 m2 = m1 `M.union` m2 

asIS_MM :: Ord k => Maybe (M.Map k v) -> Maybe (M.Map k v) -> Maybe (M.Map k v)
asIS_MM m1 m2 = M.union <$> m1 <*> m2 
