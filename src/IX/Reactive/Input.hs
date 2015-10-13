module IX.Reactive.Input
   (playerInput
   ,eUpdatePmap
   ,eHypTravel
   ,timer
   ,toEPair)
   where

import DataStructures.Atomic
import DataStructures.Composite
import IX.Universe.Combat
import IX.Universe.HyperSpace
import IX.Universe.Input
import IX.Universe.Utils
import IX.Universe.Output
import IX.Reactive.Utils
import IX.Universe.Market

import Data.Either (isLeft,isRight)
import Reactive.Banana (apply
                       ,unionWith
                       ,Behavior
                       ,Event
                       ,filterJust
                       ,filterE
                       ,(<@)
                       ,(<@>)
                       ,(<$)
                       ,(<$>)
                       ,(<*>))
import Prelude hiding (unionWith)

playerInput :: GameMaps        ->
               DieRolls        ->
               Event GameState ->
               Buffer          ->
              (Event DAgentMap,  Event (Maybe (AID,ToPlanetName)))
playerInput gMaps bDieRolls eGameState eBuffer =
   
   let bAMap'          = bAMap gMaps
       (bLMap',eLMap') = beLMap gMaps

       eClearOut   = (Just $ ClearOut) <$ eGameState
       eLook       = apply (lookToAgt <$> bAMap') $
                     unionWith asIS eHypAction ePlanetAction

       eDamage     = apply (dmgToAgt <$> bAMap') $ ePlanetAction

       eTransition = (commTransitions <$> bLMap') <@ eLMap'

       eCError     = apply (cErrToAgt <$> bAMap') $ 
                     unionWith asIS eHypAction ePlanetAction

       eChangeShip = apply (changeShip <$> bAMap') $
                     unionWith asIS eHypAction ePlanetAction

       eLocalMarket = apply (marketToAgt <$> bAMap') $ ePlanetAction

       eCommerce = apply (commerceToAgt <$> bAMap') $ ePlanetAction

       eAInput     = filterJust ( eClearOut   `unionWith`
                                  eLook       `unionWith`
                                  eDamage     `unionWith`
                                  eTransition `unionWith`
                                  eChangeShip `unionWith`
                                  eCommerce   `unionWith`
                                  eLocalMarket `unionWith`
                                  eCError)

       ePlanetAction = psAction actions
       eHypAction    = hsAction actions
       eMove = moveAction actions

   in (eAInput,eMove)
   where
      actions = partitionedActions gMaps bDieRolls eBuffer

partitionedActions  :: GameMaps ->
                       DieRolls ->
                       Buffer   ->
                       ActionPartitions 
partitionedActions gMaps bDieRolls eBuffer =
   let eMove           = Just <$> filterRight eEvalPComm
       ePSaction       = filterLeft eEvalPComm
       eHSaction       = evalHypComm <$>
                         bLMap'      <*>
                         bAMap'      <@>
                         filterJust eHypComm
   in ActionPartitions eMove ePSaction eHSaction
   where
      bAMap'             = bAMap gMaps
      bLMap'             = fst $ beLMap gMaps
      bPMap'             = bPMap gMaps
      bRMap'             = bRMap gMaps

      (eUPComm,eHypComm) =
         toEPair $ apply (whichLoc <$> bLMap') eBuffer
      eEvalPComm         =
         (evalPComm <$> bPMap' <*> bAMap' <*> bRMap' <*> bDieRolls) `apply`
         ePComm
      ePComm             =
         apply (orderUPComm <$> bLMap') $ filterJust eUPComm

eUpdatePmap :: (Behavior LocationMap, Event ()) ->
               Behavior AgentMap                ->
               Event (PlanetMap -> PlanetMap)
eUpdatePmap (bLMap,eLMap) bAMap' =
   (updatePmap <$> bLMap <*> bAMap') <@ eLMap

eHypTravel :: Event GameState -> Event (Maybe (AID,ToPlanetName))
eHypTravel eGameState = Nothing <$ eGameState

-- much like split

toEPair :: Event (a, b) -> (Event a, Event b)
toEPair epair = (extractFst <$> epair, extractSnd <$> epair)
   where
      extractFst (x, _) = x
      extractSnd (_, y) = y

filterRight :: Event (Either a b) -> Event b
filterRight e = filterJust $ fromRight <$> filterE isRight e

filterLeft :: Event (Either a b) -> Event a
filterLeft e = filterJust $ fromLeft <$> filterE isLeft e


