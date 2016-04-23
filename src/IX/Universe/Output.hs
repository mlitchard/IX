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

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Text (unpack)
import Data.List (foldl')
import Data.List.Utils (addToAL)
import Safe (fromJustNote)

import Debug.Trace

updateAMap :: [DAgentMap] -> AgentMap -> AgentMap
--updateAMap ((DAgentMap (SubAgentMap agts)) (AgentMap aMap) =
updateAMap [] agts =
  trace ("empty list updateAMap " ++ (show agts))  agts

updateAMap aMapList agts =
  trace ("updateAMap says " ++ (show agts)) foldl toAGT agts aMapList
    where
      toAGT :: AgentMap -> DAgentMap -> AgentMap
      toAGT (AgentMap a_map) (DAgentMap (SubAgentMap (a_map'))) =
        AgentMap $ M.union a_map' a_map
      toAGT (AgentMap a_map) (LocationUpdate messages) =
        AgentMap $ M.foldlWithKey messageUpdate a_map messages 
      toAGT (AgentMap a_map) ClearOut = 
        AgentMap $ M.map (repairShip . markDead . removeMSG) a_map

updateAgents :: M.Map AID Agent -> AID -> Agent -> M.Map AID Agent
updateAgents aMap aid agt = M.insert aid agt aMap

messageUpdate :: M.Map AID Agent -> AID -> Message -> M.Map AID Agent
messageUpdate a_map aid msg =
  let 
      o_agt  = fromJustNote agtFail (M.lookup aid a_map)
      n_agt  = setMessage [msg] o_agt
  in M.insert aid n_agt a_map
    where
      agtFail = "updateAMap failed to find " ++
                (show aid)                   ++
                " in agent map\n"

removeMSG :: Agent -> Agent
removeMSG agt =
  case agt of
    (Dead _) -> agt
    _        -> setMessage [] agt

markDead :: Agent -> Agent
markDead agt =
  case agt of
     (Dead _) -> agt -- no more game messages
     _        ->
        case isDead agt of
          True  -> Dead name
              where
                name = getName agt
          False -> agt


repairShip :: Agent -> Agent
repairShip agt@(Player {ship = Ship shipParts shipStats}) =
  case repairing shipStats of
    True
       | hullStrength < maxStrength -> repairShip'
       | otherwise                  -> stopRepairing
    False                           -> agt
  where
     (HullStrength hullStrength) = hull_strength shipStats
     maxStrength       = PInt $ (fromEnum $ hull shipParts) * 100
     repairShip'       =
        agt {ship = Ship shipParts setHealthField}
     setHealthField    =
        shipStats {hull_strength = HullStrength $ hullStrength + (PInt 20)}
     stopRepairing     = setRepairField False agt

repairShip agt = agt

updatePmap :: LocationMap -> AgentMap -> PlanetMap -> PlanetMap
updatePmap (LocationMap l_map) (AgentMap a_map) (PlanetMap p_map) =
   -- add recently landed
  let landed   = M.foldlWithKey addLanded p_map l_map
   -- bring out yer dead
      deadGone = M.foldlWithKey (removeDead a_map) landed $ findPlanetSide l_map
   -- then remove the ones that just left. 
  in PlanetMap $ M.foldlWithKey removeDeparted deadGone l_map

lookToAgt :: AgentMap -> M.Map AID Result -> [Maybe DAgentMap]
lookToAgt (AgentMap aMap) resList =
  trace (" lookToAGT says " ++ (show aMap)) snd `fmap` M.toList (M.mapWithKey processLook resList)
  where
    processLook aid (Looked res ship) =
      let o_agent = fromJustNote aAgentFail (M.lookup aid aMap)
      in case res of
           Left pName ->
             Just $ DAgentMap $ mkAgent (aid,o_agent) (PlanetLoc pName) 
           Right hyp  ->
             Just $ DAgentMap $ mkAgent (aid,o_agent) (InHyp hyp)
      where
        aAgentFail = "lookToAgt failed to match aid " ++ (show aid)
    processLook _ _ = Nothing


cErrToAgt :: AgentMap -> M.Map AID Result -> [Maybe DAgentMap]
cErrToAgt (AgentMap aMap') resList =
  snd `fmap` M.toList (M.mapWithKey processErr resList)
  where
    processErr aid (CError cerr) =
      let oAgent  = fromJustNote cErrToAgtERR (M.lookup aid aMap')
          naAgent = setMessage [CommandErr cerr] oAgent
      in Just $ DAgentMap $ SubAgentMap $ M.singleton aid naAgent
      where
        cErrToAgtERR = "cErrToAgt failed to find "       ++
                       "the following agent in AgentMap" ++
                     (show aid)
    processErr _  _ = Nothing

addLanded :: M.Map PlanetName Planet    ->
             AID                        ->
             Location                   ->
             M.Map PlanetName Planet
addLanded p_map aid (Location (Left (pName,Landed))) =
  case (isAdded aid planet) of
    True  -> p_map
    False -> M.insert pName addResident p_map  
   where
     addResident = setResidents uResidents $ planet
     uResidents  = (residents planet) ++ [aid]
     planet      = getPlanet pName (PlanetMap p_map)

addLanded pMap _ _  = pMap

removeDeparted :: M.Map PlanetName Planet ->
                  AID                     ->
                  Location                ->
                  M.Map PlanetName Planet
removeDeparted pMap aid (Location (Right (hyp,Launched))) =
  let planet' = removeResident aid planet
  in M.insert pName planet' pMap
  where
    (FromPlanetName pName) = origin hyp
    planet = getPlanet pName (PlanetMap pMap)

removeDeparted pMap _ _ = pMap

removeDead :: M.Map AID Agent         ->
              M.Map PlanetName Planet ->
              AID                     ->
              PlanetName              ->
              M.Map PlanetName Planet
removeDead aMap pMap aid pName =
   let agent = fromJustNote aAgentFail (M.lookup aid aMap)
   in case agent of
      (Dead _) -> let planet =
                         removeResident aid $
                         fromJustNote planetFail (M.lookup pName pMap)
                  in M.insert pName planet pMap
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


findPlanetSide :: M.Map AID Location -> M.Map AID PlanetName
findPlanetSide locs =
   M.mapMaybe findPlanetSide' locs
   where
     findPlanetSide' :: Location -> Maybe PlanetName
     findPlanetSide' (Location (Left (pName,_))) = Just pName
     findPlanetSide' _                           = Nothing

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

