{-# LANGUAGE ScopedTypeVariables #-}
module IX.Reactive.EventNetwork
   (gameloop) where


import DataStructures.Composite
import DataStructures.Atomic
import IX.Reactive.Input
import IX.Reactive.Utils (timer,mkRoll)
import IX.Universe.Utils (intToPInt,nextPlayerRoll)
import IX.Reactive.Output (writeOut)
import IX.Universe.Output (updateAMap)
import IX.Universe.Input
import IX.Universe.Market (nextMarketRolls,adjustMarket)
import IX.Universe.HyperSpace (manageTravel)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan
import System.Random


gameloop :: TChan [UAC]       ->
            TChan GameState   ->
            InitMaps          ->
            IO ()
gameloop commandChannel gsChannel initMaps' = do
   (addCommandEvent,fireCommand) <- newAddHandler
   (tickHandler, tickSink)       <- newAddHandler
   gen1                          <- getStdGen
   gen2                          <- getStdGen
   let params = 
          Parameters {
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

makeNetworkDescription :: forall t . Frameworks t => Parameters -> Moment t ()
makeNetworkDescription params = do
   eInput <- fromAddHandler (input params)
   eTick <- fromAddHandler (tick params)

   let gsChannel  = output params
       initAM     = aMap  $ initMaps params
       initPM     = pMap  $ initMaps params
       initLMs    = lMaps $ initMaps params
       playerR    = playerRolls params
       marketR    = marketRolls params
       gMaps      = GameMaps {
                       bAMap  = bAgentMap
                      ,beLMap = (bLocationMap,eLocationMap)
                      ,bRMap  = bResourceMap
                      ,bPMap  = bPlanetMap
                    }

       -- UAC to VAC means the elimination of non-existent AIDs
       eValidated :: Event t [VAC]
       eValidated = toVAC <$> filterApply (agentExists <$> bAgentMap) eInput

-- bBuffer populated by eValidated and emptied by eClearOut
       bBuffer :: Behavior t [VAC]
       bBuffer = accumB [] $ manageBuffer <$> eValidated `union` eClearBuffer

-- eClearBuffer happens when eBuffer happens
       eClearBuffer :: Event t [VAC]
       eClearBuffer = [] <$ eBuffer

-- eBuffer happens when eTick happens 
       eBuffer ::Event t [VAC]
       eBuffer = bBuffer <@ eTick


       bRandom :: DieRolls t
       bRandom = accumB playerR $ nextPlayerRoll <$ eAInput

       bMarketRolls :: DieRolls t
       bMarketRolls = accumB marketR   $
                      nextMarketRolls <$>
                      bResourceMap    <@
                      eTick

       bAgentMap :: Behavior t AgentMap
       bAgentMap = accumB initAM eAgentMap

       eAgentMap :: Event t (AgentMap -> AgentMap)
       eAgentMap = updateAMap <$> eAInput

       bPlanetMap :: Behavior t PlanetMap
       bPlanetMap = accumB initPM eUpdatePmap'
          where
             eUpdatePmap' = eUpdatePmap (bLocationMap,eLocationMap) bAgentMap

       bResourceMap :: Behavior t ResourceMap
       bResourceMap = accumB initRmap $
                      adjustMarket   <$>
                      bMarketRolls   <@
                      eTick

       bLocationMap :: Behavior t LocationMap
       eLocationMap :: Event t ()
       (eLocationMap,bLocationMap) =
          mapAccum initLMs                             $
          (manageTravel <$> bPlanetMap <*> bAgentMap) <@>
          eHypTravel' `union` eMove
          where
             eHypTravel' = eHypTravel eGameState

       eMove   :: Event t (Maybe (AID,ToPlanetName))
       eAInput :: Event t DAgentMap
       (eAInput,eMove) = playerInput gMaps bRandom eGameState eBuffer

       eGameState :: Event t GameState
       eGameState =  GameState  <$>
                     bAgentMap  <*>
                     bPlanetMap <@
                     eTick
   reactimate $ (writeOut gsChannel) <$> eGameState



tickSize :: PInt
tickSize = intToPInt 10
