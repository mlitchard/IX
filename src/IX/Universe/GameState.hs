module IX.GameState
   () where

import DataStructures.Composite
import DataStructures.Atomic

import Control.Monad (join)
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes,mapMaybe,fromJust)  
import Data.List (lookup,sortBy,foldl')
import Data.List.Utils (addToAL,delFromAL)
import qualified Data.List as DL
import Control.Applicative ((<$>))
import Safe (lookupJustNote)
import Prelude hiding (lookup)


intToPInt :: Int -> PInt
intToPInt int = PInt int

nextPlayerRoll :: [PInt] -> [PInt]
nextPlayerRoll rolls = drop 1 rolls



