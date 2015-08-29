module DataStructures.Composite
   () where

import ClassyPrelude
import Control.Concurrent.STM.TChan (TChan)
import System.IO (Handle)
import Control.Concurrent.STM.TMVar (TMVar)
import Reactive.Banana.Frameworks (AddHandler)
import Reactive.Banana.Combinators (Event,Behavior)

import DataStructures.Atomic

data Parameters = Parameters {
   input        :: AddHandler [UAC]
  ,output       :: TChan GameState
  ,initMaps     :: InitMaps
  ,tick         :: AddHandler ()
  ,playerRolls  :: [PInt]
  ,marketRolls  :: [PInt]
}

data InitMaps = InitMaps {
   aMap      :: AgentMap
  ,pMap      :: PlanetMap
  ,lMaps     :: LocationMap
}

data GameData = GameData {
   nhMap       :: TMVar [(Name,Handle)]
  ,naMap       :: TMVar [(Name,AID)]
  ,acMap       :: TMVar [(AID,Bool)]
  ,commandChan :: TChan [UAC]
  ,gameState   :: TChan GameState
}

data LocationMap = LocationMap ![(AID,Location)] deriving Show
data ResourceMap = ResourceMap ![(ResourceName,Resource)] deriving Show
data PlanetMap = PlanetMap ![(PlanetName,Planet)] deriving (Show)

data AgentMap = AgentMap ![(AID, Agent)] deriving Show
data SubAgentMap = SubAgentMap ![(AID,Agent)] deriving Show
-- DAgentMap describes what updateAMap uses to modify AgentMap
data DAgentMap = DAgentMap SubAgentMap
               | LocationUpdate ![(AID,Message)]
               | ClearOut -- used when eGameState happens to clear out message
                  deriving Show

data Location = Location (Either (PlanetName,PTransitionState)
                                 (HyperSpace,HTransitionState)) deriving Show

data Planet = Planet {
   neighbors :: ![(PlanetName,Distance)]
  ,refueling :: Bool
  ,residents :: ![AID]
  ,resources :: ![ResourceName]
} deriving Show

data HyperSpace = HyperSpace {
   destination       :: PlanetName
  ,origin            :: PlanetName
  ,totalDistance     :: PInt
  ,distanceTraversed :: PInt
} deriving Show -- Agents are not aware of other
                -- Agents in HyperSpace

data Resource = Resource {
   highestPrice :: PInt
  ,lowestPrice  :: PInt
  ,currentPrice :: PInt
  ,stability    :: Stability
} deriving (Show,Ord,Eq,Read)

data Agent = Player { aName    :: Name
                    , msg      :: ![Message]
                    , ship     :: Ship
                    , credits  :: PInt
                    , debt     :: PInt
                    , isDead   :: Bool }
           | Dead Name
              deriving Show

newtype Ship = Ship (ShipParts ,ShipStats) deriving Show

data ShipParts = ShipParts {
   engine     :: Engine
  ,weapons    :: Weapon
  ,hull       :: Hull
  ,cargoSize  :: CargoSize
  ,gasTank    :: TankSize
  ,special    :: Maybe Special
} deriving Show

data ShipStats = ShipStats {
   hull_strength :: HullStrength
  ,cargo         :: [(ResourceName,Int)]
  ,fuel          :: Fuel
  ,repairing     :: Bool
  ,warp_speed    :: Maybe WarpSpeed
} deriving Show

data Message = PlanetLoc PlanetName Ship
             | InHyp Location Ship
             | Onward ToPlanet FromPlanet
             | JustLandedOn ToPlanet
             | AttackMSG (Either AttackedMSG BeenAttackedMSG) AID DieRoll
             | LocalMarketData PlanetName [(ResourceName,Resource)]
             | YouDeadSon
             | GameOver
             | CommandErr CommandError
                deriving Show

data GameMaps t = GameMaps {
   bAMap  :: Behavior t AgentMap
  ,beLMap :: (Behavior t LocationMap, Event t ())
  ,bRMap  :: (Behavior t ResourceMap)
  ,bPMap  :: Behavior t PlanetMap
}

data GameState = GameState !AgentMap !PlanetMap deriving Show
newtype HCommand = HCommand VAC deriving Show
newtype PCommand = PCommand VAC deriving Show

-- plural 
data HSpaceComm = HSpaceComm ![HCommand] deriving Show
-- -- Unordered PlanetSide commands
data UPlanetComm = UPlanetComm ![PCommand] deriving Show
--
data PlanetComm = PlanetComm ![(PlanetName,PCommand)] deriving Show 

data PlayerCommand = PlayerCommand Command AID deriving (Eq,Ord,Show)

-- Validated Agent Command
data VAC = VAC PlayerCommand deriving (Eq,Ord,Show)
-- Unvalidated Agent Command
data UAC = UAC PlayerCommand deriving (Show)

data Command = Move PlanetName
             | Zap AID
             | Look
             | Market
             | SetSpeed WarpSpeed
             | Buy ResourceName PInt
             | Sell ResourceName PInt
             | Repair
                deriving (Ord,Eq,Show,Read)

data ShipChange = WSpeed WarpSpeed
                | Repairing
                   deriving Show

data Result = Looked (Either PlanetName Location) Ship
            | Damage AID DieRoll Attacked
            | ChangeShip ShipChange
            | Commerce CommerceResult (ResourceName,Resource) Amount
            | MarketData PlanetName [(ResourceName,Resource)]
            | CError CommandError

data ActionPartitions t = ActionPartitions {
   moveAction :: Event t (Maybe (AID,PlanetName))
  ,psAction   :: Event t (AID,Result)
  ,hsAction   :: Event t (AID,Result)
}

data CommandError = CantMoveTo ToPlanet
                  | SetSpeedFirst
                  | SpeedIsSet
                  | CantZap AID
                  | EngineTooWimpy
                  | NoMovingInHyperSpace
                  | NoBusinessInHyperSpace
                  | CantAfford ResourceName PInt PInt
                  | OffTheMarket ResourceName
                  | CantBuy ResourceName PlanetName
                  | NotInTheMarketFor ResourceName PlanetName
                  | DontHave ResourceName
                  | YouDontHave ResourceName PInt
                  | MustBeInHyperSpaceToRepair
                  | ActionCancelledDueToBeingVeryDead
                     deriving (Eq,Ord,Show)


