{-# LANGUAGE ScopedTypeVariables, RecursiveDo #-}
module IX.Reactive.EventNetwork
   (gameloop) where


import DataStructures.Composite
import DataStructures.Atomic
import IX.Reactive.Input
import IX.Reactive.Utils (timer,mkRoll,asIS,asIS_M,asIS_MM)
import IX.Universe.Utils (intToPInt,nextPlayerRoll)
import IX.Reactive.Output (writeOut)
import IX.Universe.Output (updateAMap)
import IX.Universe.Input
import IX.Universe.Market (nextMarketRolls,adjustMarket)
import IX.Universe.HyperSpace (manageTravel)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Monad (forever,join)
import Control.Monad.Fix 
import Control.Concurrent (forkIO)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan
import System.Random
import qualified Data.Map.Strict as M


gameloop :: TChan [UAC]       ->
            TChan GameState   ->
            InitMaps          ->
            IO ()
gameloop commandChannel gsChannel initMaps' = do
  (addCommandEvent,fireCommand) <- newAddHandler
  (tickHandler, tickSink)       <- newAddHandler
  gen1                          <- getStdGen
  gen2                          <- getStdGen
  let params = Parameters {
         input         = addCommandEvent
        ,output        = gsChannel
        ,initMaps      = initMaps'
        ,tick          = tickHandler
        ,playerRolls   = mkRoll gen1
        ,marketRolls   = mkRoll gen2
      }

  networkDescr <- compile $ makeNetworkDescription params
  appendFile "gameloop.txt" "gameloop called\n"
  actuate networkDescr
  _ <- forkIO $ forever (atomically (readTChan commandChannel) >>= fireCommand)
  _ <- forkIO $ forever $ (timer tickSize) >>= tickSink
  return ()

makeNetworkDescription :: Parameters -> MomentIO ()
makeNetworkDescription params = mdo
  eInput <- fromAddHandler (input params)
  eTick <- fromAddHandler (tick params)
  let 
      gsChannel = output params
      initPM    = pMap $ initMaps params
      initLMs   = lMap $ initMaps params
      initAM    = aMap  $ initMaps params
      playerR   = playerRolls params
      marketR   = marketRolls params


       -- UAC to VAC means the elimination of non-existent AIDs
      eValidated :: Event [VAC]
      eValidated = toVAC <$> filterApply (agentExists <$> bAgentMap) eInput

-- eClearBuffer happens when eBuffer happens
      eClearBuffer :: Event [VAC]
      eClearBuffer = [] <$ eBuffer

-- eBuffer happens when eTick happens 
      eBuffer ::Event [VAC]
      eBuffer = bBuffer <@ eTick

      eMove   :: Event (Maybe (M.Map AID ToPlanetName))
      eAInput :: Event [DAgentMap]
      (eAInput,eMove) = playerInput gMaps bRandom eGameState eBuffer


      eAgentMap :: Event (AgentMap -> AgentMap)
      eAgentMap = updateAMap <$> eAInput
-- bBuffer populated by eValidated and emptied by eClearOut
--      bBuffer :: Behavior [VAC]
  bBuffer         <- accumB []     $ 
                     manageBuffer <$>
                     unionWith (++) eValidated eClearBuffer

--      bRandom :: DieRolls 
  bRandom         <- accumB playerR $ nextPlayerRoll <$ eAInput

--      bAgentMap :: Behavior AgentMap
  bAgentMap       <- (accumB initAM eAgentMap)

  (eLocationMap,bLocationMap) <- mapAccum initLMs                             $
                                 (manageTravel <$> bPlanetMap <*> bAgentMap) <@>
                                 unionWith asIS_MM eHypTravel' eMove 
  let eHypTravel' = eHypTravel eGameState

  bMarketRolls <- accumB marketR   $ 
                  nextMarketRolls <$>
                  bResourceMap    <@
                  eTick

--bResourceMap :: Behavior ResourceMap
  bResourceMap <- accumB initRmap $  
                  adjustMarket   <$>
                  bMarketRolls   <@
                  eTick

  let eUpdatePmap' = eUpdatePmap (bLocationMap,eLocationMap) bAgentMap
--bPlanetMap :: Behavior PlanetMap
  bPlanetMap <- 
    accumB initPM eUpdatePmap'
  let
      gMaps = GameMaps {
                 bAMap  = bAgentMap
                ,beLMap = (bLocationMap,eLocationMap)
                ,bRMap  = bResourceMap
                ,bPMap  = bPlanetMap
              }
  --eGameState :: Event GameState
      eGameState = GameState  <$>
                   bAgentMap  <*>
                   bPlanetMap <@
                   eTick
  reactimate $ (writeOut gsChannel) <$> eGameState



tickSize :: PInt
tickSize = intToPInt 10
