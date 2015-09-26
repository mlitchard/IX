module IX.Universe.Output
   (updateAMap
   ,updatePmap
   ,updateAgents
   ,lookToAgt
   ,cErrToAgt
   ) where

import DataStructures.Atomic
import DataStructures.Composite
import IX.Universe.Utils (setMessage,mkAgent,getPlanet)
import IX.Universe.HyperSpace (getName,setRepairField)

import Data.Maybe (mapMaybe)
import Data.Text (unpack)
import Data.List (foldl')
import Data.List.Utils (addToAL)
import Safe (lookupJustNote)


updateAMap :: DAgentMap -> AgentMap -> AgentMap
updateAMap (DAgentMap (SubAgentMap agts@(_:_))) (AgentMap aMap') =
   AgentMap $ foldl' updateAgents aMap' agts

updateAMap (DAgentMap (SubAgentMap [])) aMap' = aMap'

updateAMap (LocationUpdate messages) (AgentMap aMap') =
   AgentMap $ foldl' updateMessages aMap' messages

updateAMap ClearOut (AgentMap aMap') =
   AgentMap $ map (repairShip . markDead . removeMSG) aMap'

updateAgents :: [(AID,Agent)] -> (AID,Agent) -> [(AID,Agent)]
updateAgents aMap (aid,agt) = addToAL aMap aid agt

updateMessages :: [(AID,Agent)] -> (AID,Message) -> [(AID,Agent)]
updateMessages aMap (aid,msg') =
   addToAL aMap aid  $
   setMessage [msg']  $
   lookupJustNote upMessageFail aid aMap
   where
      upMessageFail = "updateMessage failed to find " ++
                       show aid                        ++
                      "in agent map\n"

removeMSG :: (AID,Agent) -> (AID,Agent)
removeMSG ap@(aid,agt) =
   case agt of
      (Dead _) -> ap
      _    -> let agt' = setMessage [] agt
              in (aid,agt')

markDead :: (AID,Agent) -> (AID,Agent)
markDead ap@(aid,agt) =
   case agt of
      (Dead _) -> ap -- no more game messages
      _        ->
         case isDead agt of
            True  -> (aid, Dead name)
               where
                  name = getName agt
            False -> ap


repairShip :: (AID,Agent) -> (AID,Agent)
repairShip ap@(aid,agt@(Player {ship = Ship shipParts shipStats})) =
   case repairing shipStats of
      True
         | hullStrength < maxStrength -> (aid,repairShip')
         | otherwise                  -> (aid,stopRepairing)
      False                           -> ap
   where
      (HullStrength hullStrength) = hull_strength shipStats
      maxStrength       = PInt $ (fromEnum $ hull shipParts) * 100
      repairShip'       =
         agt {ship = Ship shipParts setHealthField}
      setHealthField    =
         shipStats {hull_strength = HullStrength $ hullStrength + (PInt 20)}
      stopRepairing     = setRepairField False agt

repairShip ap = ap

updatePmap :: LocationMap -> AgentMap -> PlanetMap -> PlanetMap
updatePmap (LocationMap l_map) (AgentMap a_map) (PlanetMap p_map) =
   -- add recently landed
   let landed   = foldl' addLanded p_map l_map
   -- bring out yer dead
       deadGone = foldl' (removeDead a_map) landed $ findPlanetSide l_map
   -- then remove the ones that just left. 
   in PlanetMap $ foldl' removeDeparted deadGone l_map

lookToAgt :: AgentMap -> (AID,Result) -> Maybe DAgentMap
lookToAgt (AgentMap aMap) (aid@(AID aid'),Looked res ship) =
   let o_agent = lookupJustNote aAgentFail aid aMap
   in case res of
         Left pName ->
            Just $ DAgentMap $ mkAgent (aid,o_agent) (PlanetLoc pName)
         Right hyp  ->
            Just $ DAgentMap $ mkAgent (aid,o_agent) (InHyp hyp)
   where
      aAgentFail = "lookToAgt failed to match aid " ++ unpack aid'

lookToAgt _ _ = Nothing

cErrToAgt :: AgentMap -> (AID,Result) -> Maybe DAgentMap
cErrToAgt (AgentMap aMap') (aid, (CError cerr)) =
   let oAgent  = lookupJustNote cErrToAgtERR aid aMap'
       naAgent = setMessage [CommandErr cerr] oAgent
   in Just $ DAgentMap $ SubAgentMap $ [(aid,naAgent)]
    where
      cErrToAgtERR = "cErrToAgt failed to find "       ++
                     "the following agent in AgentMap" ++
                     (show aid)
cErrToAgt _ _ = Nothing

addLanded :: [(PlanetName,Planet)]      ->
             (AID,Location)             ->
             [(PlanetName,Planet)]
addLanded p_map
   (aid,(Location (Left (pName,Landed)))) =
      case (isAdded aid planet) of
            True  -> p_map
            False -> addToAL p_map pName $ addResident
      where
         addResident = setResidents uResidents $ planet
         uResidents  = (residents planet) ++ [aid]
         planet      = getPlanet pName (PlanetMap p_map)

addLanded pMap'' _ = pMap''

removeDeparted :: [(PlanetName,Planet)] ->
                  (AID,Location)        ->
                  [(PlanetName,Planet)]
removeDeparted pMap (aid, (Location (Right (hyp,Launched)))) =
   addToAL pMap pName $ removeResident aid planet
      where
         (FromPlanetName pName) = origin hyp
         planet = getPlanet pName (PlanetMap pMap)

removeDeparted pMap'' _ = pMap''

removeDead :: [(AID,Agent)]          ->
              [(PlanetName,Planet)]  ->
              (AID,PlanetName)       ->
              [(PlanetName,Planet)]
removeDead aMap pMap (aid,pName) =
   let agent = lookupJustNote aAgentFail aid aMap
   in case agent of
      (Dead _) -> let planet =
                         removeResident aid $
                         lookupJustNote planetFail pName pMap
                  in addToAL pMap pName planet
                  where
                     planetFail = "removeDead failed to find this " ++
                                  "planet from PlanetMap "          ++
                                  (show pName)                      ++
                                    "\n"
      _        -> pMap
   where
      aAgentFail = "removeDead failed to find this agent in AgentMap" ++
                   (show aid)                                         ++
                   "\n"


findPlanetSide :: [(AID,Location)] -> [(AID,PlanetName)]
findPlanetSide locs =
   mapMaybe findPlanetSide' locs
   where
     findPlanetSide' :: (AID,Location) -> Maybe (AID,PlanetName)
     findPlanetSide' (aid,Location (Left (pName,_))) = Just (aid,pName)
     findPlanetSide' _                               = Nothing

isAdded :: AID -> Planet -> Bool
isAdded aid (Planet {residents = aids}) = aid `elem` aids

--unwrap :: PlanetNameWrapper -> PlanetName
--unwrap (FP_W (FromPlanetName fpn)) = fpn
--unwrap (TP_W (ToPlanetName tpn))   = tpn
-----getters setters -----
setResidents :: [AID] -> Planet -> Planet
setResidents aids planet = planet { residents = aids }

removeResident :: AID -> Planet -> Planet
removeResident aid planet@(Planet {residents = aids}) =
   flip setResidents planet $ filter (/= aid) aids

