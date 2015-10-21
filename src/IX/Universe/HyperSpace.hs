-- This module manages the movement through Hyperspace
-- and the transistion from a ship being in Hyperspace
-- to being landed on a destination planet
module IX.Universe.HyperSpace
   (manageTravel
   ,evalHyp
   ,evalSetSpeed
   ,evalHypComm
   ,evalMove
   ,commTransitions
   ,changeShip
   ,getName
   ,getPlanet
   ,setRepairField
   ) where


import DataStructures.Atomic
import DataStructures.Composite
import IX.Universe.Utils (intToPInt)

import Data.List (foldl')
import Safe (fromJustNote)
import Data.List.Utils (delFromAL)
import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import Debug.Trace

manageTravel :: PlanetMap                      ->
                AgentMap                       ->
                Maybe (M.Map AID ToPlanetName) ->
                LocationMap                    ->
                ((),LocationMap)
manageTravel (pMap)
             _
             (Just transit_map)
             (LocationMap l_map) =
  let up_lmap = LocationMap $ M.foldrWithKey upLMap l_map transit_map  
  in ((),up_lmap)
  where
    upLMap :: AID -> ToPlanetName -> M.Map AID Location -> M.Map AID Location
    upLMap aid tpn l_map =
      let ((Location (Left (pn, _)))) = 
            fromJustNote aidNotFound (M.lookup aid l_map)
          newLoc                      = Location $ Right $ (hSpace, Launched)
          hSpace                      = HyperSpace {
                                           destination       = tpn
                                          ,origin            = fpn
                                          ,totalDistance     = distanceTo
                                          ,distanceTraversed = 0 :: PInt
                                        }
          distanceTo                  = getDistanceTo tpn $ FromPlanet (fpn,fp)
          fp                          = getPlanet pn pMap
          fpn                         = FromPlanetName pn
          aidNotFound                 = "manageTravel failed to find " ++ 
                                        (show aid)                     ++
                                        "in LocationMap\n"
      in M.insert aid newLoc l_map
manageTravel _      -- No need for a PlanetMap 
             (AgentMap aMap)
             Nothing -- a tick must have happened if this matches
             (LocationMap lMap)  =
  (,) ()
  (LocationMap                          $
  M.mapWithKey (updateLocationMap aMap) $
  M.foldlWithKey removeDead lMap aMap)
  where
    removeDead lmap' aid (Dead _) = M.delete aid lmap'
    removeDead lmap' _ _          = lmap'

    updateLocationMap _ _ (Location (Left ( pName,Landed))) =
      (Location (Left (pName,PlanetSide)))
    updateLocationMap _ _ loc@(Location (Left (_,PlanetSide))) =
      loc
    updateLocationMap a_map aid (Location (Right (hSpace,tState))) =
      let (ToPlanetName dest) = destination hSpace
          tDist               = totalDistance hSpace
          traversed           = distanceTraversed hSpace
      in case tState of
           Launched     -> updatedHSpace
           InHyperSpace -> if (tDist == traversed)
                           then landed
                           else updatedHSpace
                           where
                             landed = (Location $ Left $ (dest,Landed))
           where
             ship_speed =
               case (findSpeed) of
                 Just speed' -> speed'
                 Nothing     -> Turtle -- how did this happen?

             updatedHSpace =
               Location $ Right (incDist hSpace ship_speed,InHyperSpace)
             findSpeed = join               $
                         warp_speed        <$>
                         ship_stats        <$>
                         ship              <$>
                         M.lookup aid a_map

evalHyp :: M.Map AID Location -> M.Map AID Agent -> HCommand -> (AID,Result)
evalHyp l_map a_map (HCommand (VAC (PlayerCommand comm aid))) =
   let hyp_data = fromJustNote locFail (M.lookup aid l_map)
       ship'    = ship (fromJustNote agtFail (M.lookup aid a_map))
       res = case comm of
                Move pName -> CError (CantMoveTo $ pName)
                Zap aid'   -> CError (CantZap aid')
                Look       -> Looked (Right hyp_data) ship'
                Repair     -> ChangeShip Repairing
                SetSpeed _ -> CError SpeedIsSet
                Buy _ _    -> CError NoBusinessInHyperSpace
                Sell _ _   -> CError NoBusinessInHyperSpace
                Market     -> CError NoBusinessInHyperSpace
   in (aid,res)
      where
         locFail = "evalHyp did not find " ++
                   show aid                    ++
                   "in LocationMap"
         agtFail = "evalHyp did not find " ++
                   show aid                    ++
                   "in AgentMap"


evalSetSpeed :: WarpSpeed -> Ship -> Result
evalSetSpeed warp_speed (Ship (ShipParts {engine = engine'}) _)
   | w_speed <= e_power = ChangeShip $ WSpeed warp_speed
   | otherwise          = CError EngineTooWimpy
   where
      w_speed = fromEnum warp_speed
      e_power = fromEnum engine'

evalHypComm :: LocationMap -> AgentMap -> HSpaceComm -> M.Map AID Result
evalHypComm (LocationMap l_map) (AgentMap a_map) (HSpaceComm hCommands) =
   M.fromList (map (evalHyp l_map a_map) hCommands)


evalMove aid agt ((ToPlanet (tpn@(ToPlanetName pn),_)), fpn) (PlanetMap pMap') =
   case (warp_speed $ ship_stats $ ship agt) of
      Just _  -> attemptDest
      Nothing -> Left $ (aid,resultErr)
   where
      resultErr = CError $ SetSpeedFirst
      attemptDest =
         let mDest =
                join       $
                lookup pn <$>
                neighbors <$>
                M.lookup fpn pMap'
         in case mDest of
                     Just _  -> Right $ (aid,tpn)
                     Nothing -> Left  $ (aid,resultErr')
          where
             resultErr' = CError $ CantMoveTo tpn

commTransitions :: LocationMap -> [Maybe DAgentMap]
commTransitions (LocationMap lMap) =
   eIsN $ LocationUpdate $ M.mapMaybe commTransitions' lMap
   where
     commTransitions' :: Location -> Maybe Message
     commTransitions' (Location (Left (tpn, Landed))) =
        Just (JustLandedOn tpn)

     commTransitions' (Location (Right (hSpace,Launched))) =
        let fpn    = origin hSpace
            tpn    = destination hSpace
            onward = Onward tpn fpn
        in Just onward

     commTransitions' _ = Nothing

     eIsN :: DAgentMap -> [Maybe DAgentMap]
--     eIsN (LocationUpdate []) = Nothing
     eIsN lu@(LocationUpdate _) = [Just lu]
     eIsN _ = [Nothing]

changeShip :: AgentMap         ->
              M.Map AID Result ->
              [Maybe DAgentMap]
changeShip (AgentMap a_map) resList =
  snd `fmap` M.toList (M.mapWithKey changeShip' resList)
  where
    changeShip' aid (ChangeShip change) =
      let agt = fromJustNote aAgentFail (M.lookup aid a_map)
          res = case change of
                  (WSpeed w_speed) ->
                    setWarpSpeed w_speed agt
                  Repairing        ->
                    setRepairField True agt
      in Just $ DAgentMap $ SubAgentMap $ M.singleton aid res
      where
        aAgentFail = "changeShip failed to match aid " ++ (show aid)
    changeShip' _ _ = Nothing



--removeDead :: [(AID,Location)] -> (AID,Agent) -> [(AID,Location)]
--removeDead lmap' (aid,(Dead _)) = delFromAL lmap' aid
--removeDead lmap' _              = lmap'
----------------------- Getters and Setters --------------------

getName :: Agent -> ClientName
getName (Player {aName = name}) = name
getName (Dead name)             = name

getDistanceTo :: ToPlanetName ->
                 FromPlanet   ->
                 Distance
getDistanceTo (ToPlanetName tpn) (FromPlanet ((FromPlanetName fpn),fp)) =
   fromJustNote neighborFail (lookup tpn (neighbors fp))
   where
     neighborFail = show tpn ++ " should have been a neighbor of " ++ show fpn

getPlanet :: PlanetName -> PlanetMap -> Planet
getPlanet p_name (PlanetMap p_map) =
   fromJustNote noPlanet (M.lookup p_name p_map)
      where
         noPlanet = "getPlanet failed to find "          ++
                    "the following planet in PlanetMap " ++
                    show p_name

setRepairField :: Bool -> Agent -> Agent
setRepairField bool agt@(Player {ship = Ship shipParts shipStats}) =
   agt {ship = set_repair_field}
   where
      set_repair_field = Ship shipParts shipStats {repairing = bool}

setWarpSpeed :: WarpSpeed -> Agent -> Agent
setWarpSpeed w_speed agt@(Player {ship = Ship shipParts shipStats}) =
   agt {ship = set_warp_speed}
   where
      set_warp_speed = Ship shipParts shipStats {warp_speed = Just w_speed}
setWarpSpeed _ agt = agt

incDist :: HyperSpace -> WarpSpeed -> HyperSpace
incDist hSpace w_speed = hSpace {distanceTraversed = incremented}
   where incremented = (distanceTraversed hSpace) + increment
         increment   = (intToPInt $ fromEnum w_speed) + 1
