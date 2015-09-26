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
import IX.Universe.Output
import IX.Reactive.Utils
import IX.Universe.Market

import Data.Either (isLeft,isRight)
import Reactive.Banana
import Prelude

playerInput :: GameMaps t        ->
               DieRolls t        ->
               Event t GameState ->
               Buffer t          ->
              (Event t DAgentMap,  Event t (Maybe (AID,ToPlanetName)))
playerInput gMaps bDieRolls eGameState eBuffer =
   
   let bAMap'          = bAMap gMaps
       (bLMap',eLMap') = beLMap gMaps

       eClearOut   = (Just $ ClearOut) <$ eGameState
       eLook       = apply (lookToAgt <$> bAMap') $
                     eHypAction `union` ePlanetAction

       eDamage     = apply (dmgToAgt <$> bAMap') $ ePlanetAction

       eTransition = (commTransitions <$> bLMap') <@ eLMap'

       eCError     = apply (cErrToAgt <$> bAMap') $
                     eHypAction `union` ePlanetAction

       eChangeShip = apply (changeShip <$> bAMap') $
                     eHypAction `union` ePlanetAction

       eLocalMarket = apply (marketToAgt <$> bAMap') $ ePlanetAction

       eCommerce = apply (commerceToAgt <$> bAMap') $ ePlanetAction

       eAInput     = filterJust ( eClearOut   `union`
                                  eLook       `union`
                                  eDamage     `union`
                                  eTransition `union`
                                  eChangeShip `union`
                                  eCommerce   `union`
                                  eLocalMarket `union`
                                  eCError)

       ePlanetAction = psAction actions
       eHypAction    = hsAction actions
       eMove = moveAction actions

   in (eAInput,eMove)
   where
      actions = partitionedActions gMaps bDieRolls eBuffer

partitionedActions  :: GameMaps t ->
                       DieRolls t ->
                       Buffer t   ->
                       ActionPartitions t
partitionedActions gMaps bDieRolls eBuffer =
   let eMove           = Just <$> filterRight eEvalPComm
       ePSaction       = filterLeft eEvalPComm
       eHSaction       = spill        $
                         evalHypComm <$>
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
         spill $ apply (orderUPComm <$> bLMap') $ filterJust eUPComm

eUpdatePmap :: (Behavior t LocationMap, Event t ()) ->
               Behavior t AgentMap                  ->
               Event t (PlanetMap -> PlanetMap)
eUpdatePmap (bLMap,eLMap) bAMap' =
   (updatePmap <$> bLMap <*> bAMap') <@ eLMap

eHypTravel :: Event t GameState -> Event t (Maybe (AID,ToPlanetName))
eHypTravel eGameState = Nothing <$ eGameState

-- much like split

toEPair :: Event t (a, b) -> (Event t a, Event t b)
toEPair epair = (extractFst <$> epair, extractSnd <$> epair)
   where
      extractFst (x, _) = x
      extractSnd (_, y) = y

filterRight :: Event t (Either a b) -> Event t b
filterRight e = filterJust $ fromRight <$> filterE isRight e

filterLeft :: Event t (Either a b) -> Event t a
filterLeft e = filterJust $ fromLeft <$> filterE isLeft e


