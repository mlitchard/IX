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

import           DataStructures.Composite
import           DataStructures.Atomic
import           Emporos.Reactive.Input
import           Emporos.Reactive.Market
import           Emporos.Reactive.Hyperspace
import           Emporos.Universe.Input
import           Emporos.Universe.Output


main :: IO ()
main = defaultMain
  [ testGroup "Behaviors"
      [ testBuffer "bBuffer" testData  ]
  ]

testBuffer :: String -> [UAC] -> Test
testBuffer name test_data = do
  testCase name $ assert $ testBuffer' test_data

testBuffer' :: [UAC] -> IO (Bool)
testBuffer' uac_data = do
  (_,(((BufferMap res):_):_)) <- interpretFrameWorks $ toVAC <$> testData
  let res' = M.toList res
  return (expectedRes == res')

testRolls :: String -> [DieRoll] -> Test
testRolls name d_rolls = do
  testCase name $ assert $ testRolls' d_rolls

testRolls' :: [DieRoll] -> IO (Bool)
testRolls' d_rolls = do
  


testData = [(UAC (PlayerCommand (Move (ToPlanetName Mongo)) (AID (T.pack
 "100"))))]

expectedRes = map ((,) (AID (T.pack "100"))) (toVAC <$> testData)
eClear = Clear <$ (() <$ never)
