import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assert, Assertion)

import           Data.IORef
import           Reactive.Banana.Combinators
import           Reactive.Banana.Frameworks
import           Control.Event.Handler
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           InputReactive

import           DataStructures.Composite
import           DataStructures.Atomic
import           IX.Reactive.Input
import           IX.Universe.Input
import           IX.Universe.Output


main :: IO ()
main = defaultMain
  [ testGroup "Input - Reactive"
      [ testBuffer "eBuffer" testData  ]
  ]


-- testRolls :: String -> [DieRoll] -> Test
-- testRolls name d_rolls = do
--  testCase name $ assert $ testRolls' d_rolls

-- testRolls' :: [DieRoll] -> IO (Bool)
-- testRolls' d_rolls = do
  


