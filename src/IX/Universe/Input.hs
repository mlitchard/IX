module IX.Universe.Input
 (toVAC
 ,agentExists
 ,manageBuffer
 ,whichLoc
 ,orderUPComm
 ,evalPComm
 ,initAmap
 ,initLmaps
 ,initPmap
 ,initRmap
 ) where

import DataStructures.Atomic
import DataStructures.Composite
import IX.Universe.HyperSpace (evalSetSpeed,evalMove)
import IX.Universe.Market (evalMarket,evalCommerce)
import IX.Universe.Combat (evalZap)
import IX.Universe.Utils (intToPInt,getPlanet)

import Safe (lookupJustNote)
import Data.Either (partitionEithers)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)


-- Not doing much right now. Will one day actually do something
-- The idea is that only legal commands are allowed, so probably needs AgentMap
-- to assess this

toVAC :: [UAC] -> [VAC]
toVAC [(UAC pCommand)] = [VAC pCommand]
toVAC _                = []


-- used to prevent dead players of sending commands to the game
agentExists :: AgentMap -> [UAC] -> Bool
agentExists (AgentMap aMap') [UAC (PlayerCommand _ aid)] =
   let mAGENT = lookup aid aMap'
   in case mAGENT of
      (Just (Dead _)) -> False
      (Just _)        -> True
      Nothing         -> False
agentExists (AgentMap _) [] = False
agentExists (AgentMap _) (UAC (PlayerCommand _ _) : (_ : _)) = False



-- quad time alert
manageBuffer :: [VAC] -> [VAC] -> [VAC]
manageBuffer [] _ = []
manageBuffer incoming acc = acc ++ incoming



whichLoc :: LocationMap -> [VAC] -> (Maybe UPlanetComm,Maybe HSpaceComm)
whichLoc (LocationMap lMap) vacs@(_:_) =
   let pComm :: [PCommand]
       hComm :: [HCommand]
       (pComm, hComm) = partitionEithers $ map (`sortLoc` lMap) vacs
   in (Just (UPlanetComm pComm), Just (HSpaceComm hComm))

whichLoc _ [] = (Nothing,Nothing)

sortLoc :: VAC -> [(AID,Location)] -> Either PCommand HCommand
sortLoc vac@(VAC (PlayerCommand  _ aid)) locs =
   case pLoc of
      Location (Left _)  -> Left $ PCommand vac
      Location (Right _) -> Right $ HCommand vac
   where
      pLoc    = lookupJustNote locFail aid locs
      locFail = "sortLoc couldn't find " ++
                show aid                 ++
                " in LocationMap\n"
evalPComm :: PlanetMap             ->
             AgentMap              ->
             ResourceMap           ->
             [PInt]                ->
             (PlanetName,PCommand) ->
             Either (AID,Result) (AID,ToPlanetName) -- This type needs 
                                                    -- consolodation
                                                    -- (AID,ToPlanetName) is 
                                                    -- also a Result
evalPComm p_map
          (AgentMap a_map)
          r_map
          (dRoll:_)
          (p_name,(PCommand (VAC (PlayerCommand comm aid)))) =
   let agt    = lookupJustNote agtFail aid a_map
   in case agt of
        (Dead _)                       ->
           Left $ (aid, CError $ ActionCancelledDueToBeingVeryDead)
        _                              ->
          case comm of
            Repair                     ->
               Left $ (aid,(CError MustBeInHyperSpaceToRepair))
            SetSpeed warp_speed        ->
               Left $ (aid,evalSetSpeed warp_speed ship')
               where
               ship' = ship agt

            Market                     ->
               Left $ (aid,evalMarket p_name planet' r_map)
            Buy r_name amt             ->
               Left $ (aid,evalBuy')
               where
                  evalBuy' =
                     evalCommerce BuyA agt (r_name,r_map) amt (p_name,planet')

            Sell r_name amt            ->
               Left $ (aid,evalSell')
               where
                  evalSell' =
                     evalCommerce SellA agt (r_name,r_map) amt (p_name,planet')

            Move tpn@(ToPlanetName pn) ->
               evalMove aid agt (toPlanet, p_name) p_map
               where
                  toPlanet = ToPlanet (tpn,plt)
                  plt      = getPlanet pn p_map   
            Look                       ->
               Left $ (aid,doLook p_name agt)

            Zap aidATK           ->
               let agtATK = lookupJustNote agtFail' aidATK a_map
                   res    = evalZap (aidATK,agtATK) agt p_name dRoll p_map
               in Left $ (aid,res)
               where
                  agtFail' = "evalPComm failed to find "    ++
                             "this agent in AgentMap "      ++
                             "while trying to execute Zap " ++
                             (show aidATK)                  ++
                             "\n"
   where
      planet'  = getPlanet p_name p_map
      planetFail = "evalPComm couldn't find planet " ++ (show p_name)
      agtFail =
        "evalPComm couldn't find the following Agent who is trying to" ++
        "do an action "                                                ++
        (show aid)                                                     ++
        "\n"
orderUPComm :: LocationMap -> UPlanetComm -> [(PlanetName,PCommand)]
orderUPComm (LocationMap lMap) (UPlanetComm pCommands) =
   sortBy sortComms $ map (planetTag pSide) pCommands
   where
     pSide = mapMaybe findPlanetBound lMap

sortComms :: (PlanetName,PCommand)  ->
             (PlanetName, PCommand) ->
             Ordering
sortComms (p1,_) (p2,_)
   | p1 < p2 = LT
   | p1 > p2 = GT
   | p1 == p2 = EQ
sortComms _ _ = EQ

planetTag :: [(AID,PlanetName)] -> 
             PCommand                  ->
             (PlanetName,PCommand)
planetTag pLocs pc@(PCommand (VAC (PlayerCommand _ aid))) =
   let pName = case lookup aid pLocs of
                  (Just p) -> p
                  Nothing  -> error $ orderUPCommERR pName
   in (pName,pc)
        where
           orderUPCommERR :: PlanetName -> String
           orderUPCommERR pName = "orderUPComm couldn't find planet " ++
                                  show pName


findPlanetBound :: (AID,Location) -> Maybe (AID,PlanetName)
findPlanetBound (aid, Location (Left (pName,_))) = Just (aid,pName)
findPlanetBound _                                = Nothing

doLook :: PlanetName -> Agent -> Result
doLook p_name agt = Looked (Left p_name) $ ship agt

-- the result of evalPComm is either the consequence of an action
-- -- on another player or a resource on the planet (or an impossible command), 
-- -- or a transition to hyperspace

-------------------------------



initAmap :: [(Name,AID)] -> AgentMap
initAmap naMap' = AgentMap $ map initAMap' naMap'
   where
     initAMap' :: (Name,AID) -> (AID,Agent)
     initAMap' (name,aid) = let agent = Player { aName    = name
                                               , msg      = [PlanetLoc Vulcan]
                                               , ship     = mkShip
                                               , credits  = 50000
                                               , debt     = 50000
                                               , isDead   = False}
                            in (aid,agent)
     mkShip = Ship shipParts shipStats
        where
           shipParts = ShipParts {
              engine    = ARNOLDPOWER
             ,weapons   = KILLMACHINE
             ,hull      = Rhinoceros
             ,cargoSize = AllTheThings
             ,gasTank   = NvrEmpty
             ,special   = Nothing
           }
           shipStats = ShipStats {
              hull_strength = HullStrength $ PInt 500
             ,cargo         = mkCargo
             ,fuel          = Fuel $ PInt 5000
             ,repairing     = False
             ,warp_speed    = Nothing
           }
     mkCargo = map (flip (,) 0) [FinestGreen .. InterzoneSpecial]

initLmaps :: [AID] -> LocationMap
initLmaps aids =
   LocationMap $ map mkLMap aids
   where
     mkLMap :: AID -> (AID,Location)
     mkLMap aid = (aid, Location $ Left (pn,PlanetSide))
     pn         = Vulcan

initPmap :: [AID] -> PlanetMap
initPmap aids = PlanetMap $ zip [Vulcan .. VoidlessVoid] planets
   where
      planets = [p0,p1,p2,p3,p4,p5]
      p0      = Planet {
         neighbors = [(Mongo,500),(Tatooine,5000)]
        ,refueling = False
        ,residents = aids
        ,resources = [FinestGreen,BabyBlue]
      }
      p1      = Planet {
         neighbors = [(Vulcan,500),(Arakis,5000)]
        ,refueling = False
        ,residents = []
        ,resources = [SubstanceD,Melange]
      }
      p2      = Planet {
         neighbors = [(Mongo,5000),(Dantooine,5000)]
        ,refueling = True
        ,residents = []
        ,resources = [BabyBlue,InterzoneSpecial]
      }
      p3      = Planet {
         neighbors = [(Arakis,5000),(Tatooine,5000)]
        ,refueling = False
        ,residents = []
        ,resources = [Melange,FinestGreen]
      }
      p4      = Planet {
         neighbors = [(Dantooine,5000),(Vulcan,5000)]
        ,refueling = True
        ,residents = []
        ,resources = [SubstanceD,FinestGreen]
      }
      p5      = Planet {
         neighbors = []
        ,residents = []
        ,resources = []
        ,refueling = True
      }

initRmap :: ResourceMap
initRmap = ResourceMap $ zip [FinestGreen .. InterzoneSpecial] resources'
   where
      resources' = [r0,r1,r2,r3,r4]
      r0         = Resource {
         highestPrice = 400
        ,lowestPrice  = 20
        ,currentPrice = 100
        ,stability    = Stable 5000
      }

      r1        = Resource {
         highestPrice = 100
        ,lowestPrice  = 1
        ,currentPrice = 20
        ,stability    = Stable 5000
      }

      r2        = Resource {
         highestPrice = 1000
        ,lowestPrice  = 200
        ,currentPrice = 500
        ,stability    = Stable 5000
      }

      r3        = Resource {
         highestPrice = 10000
        ,lowestPrice  = 1000
        ,currentPrice = 5000
        ,stability    = Stable 5000
      }

      r4        = Resource {
         highestPrice = 100
        ,lowestPrice  = 1
        ,currentPrice = 20
        ,stability    = Stable 5000
      }

      p5      = Planet {neighbors = []
                       ,residents = []
                       ,resources = []
                       ,refueling = True}
