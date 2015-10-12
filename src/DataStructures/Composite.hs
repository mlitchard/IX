module DataStructures.Composite
   ( ResourceMap (..)
   , Resource (..)
   , AgentMap(..)
   , DAgentMap(..)
   , SubAgentMap(..)
   , GameMaps (..)
   , UAC(..)
   , PlayerCommand (..)
   , VAC(..)
   , Agent(..)
   , Message (..)
   , Ship (..)
   , ShipParts (..)
   , ShipStats (..)
   , Planet (..)
   , ToPlanet (..)
   , FromPlanet (..)
   , HyperSpace (..)
   , LocationMap (..)
   , Location (..)
   , PlanetMap (..)
   , UPlanetComm (..)
   , HSpaceComm (..)
   , PCommand (..)
   , HCommand (..)
   , Result (..)
   , Command (..)
   , CommandError (..)
   , ShipChange (..)
   , GameState (..)
   , InitMaps (..)
   , Parameters (..)
   , ActionPartitions (..)
   , Buffer
   , DieRolls
   , Server (..)
   , Client (..)
   , SMessage (..))
   where

import DataStructures.Atomic

import Reactive.Banana.Frameworks (AddHandler)
import Reactive.Banana.Combinators (Event,Behavior)
import ClassyPrelude
import Control.Concurrent.STM.TChan (TChan)
import System.IO (Handle)
import Control.Concurrent.STM.TMVar (TMVar)
import           Data.Conduit.TMChan

import qualified Data.ByteString.Char8 as BS
import           Data.Conduit.Network
import qualified Data.Map.Strict as Map

data SMessage                          -- Server Messages 
  = Notice BS.ByteString               -- To: Everyone From: Server
  | Tell ClientName BS.ByteString      -- Talk smack to other players, of course
  | Broadcast ClientName BS.ByteString -- To: Everyone From: Game
  | SCommand BS.ByteString             -- Server Command
  | GCommand ClientName BS.ByteString  -- Game Command Soon To Be JSOn
  deriving Show

data Server = Server
  { clients       :: TVar (Map.Map ClientName Client)
  , clientNames   :: TVar (Map.Map AID ClientName)
  , gameStateChan :: TChan GameState
  , commandChan   :: TChan [UAC]
  , gameon        :: TVar Bool
  }

data Client = Client
  { clientName     :: ClientName
  , clientChan     :: TMChan SMessage
  , clientApp      :: AppData
  }
data Parameters = Parameters 
  { input        :: AddHandler [UAC] -- All user input per tick
  , output       :: TChan GameState  -- 
  , initMaps     :: InitMaps         -- provides initial states
  , tick         :: AddHandler ()    -- provides heartbeat
  , playerRolls  :: [PInt]           -- infitnite lists for die rolls
  , marketRolls  :: [PInt]
  }

data InitMaps = InitMaps
  { aMap      :: AgentMap -- tracks all Agent records
  , pMap      :: PlanetMap -- tracks all Planet records
  , lMap     :: LocationMap -- tracks Agent locations
  }

--data GameData = GameData 
--  { nhMap       :: TMVar [(Name,Handle)] -- easy mapping name to handle
--  , naMap       :: TMVar [(Name,AID)]    -- players know names, game knows AIDs
--  , acMap       :: TMVar [(AID,Bool)]    
--  , commandChan :: TChan [UAC]     -- all valid player commands end up in here
--  , gameState   :: TChan GameState --       
--  }

type Buffer = Event [VAC] -- the list of commands to be
                                --  processed in a tick
type DieRolls = Behavior [PInt]

data LocationMap = LocationMap (Map.Map AID Location) deriving Show
data ResourceMap = ResourceMap (Map.Map ResourceName Resource) deriving Show
data PlanetMap = PlanetMap (Map.Map PlanetName Planet) deriving (Show)

data AgentMap = AgentMap (Map.Map AID Agent) deriving Show
data SubAgentMap = SubAgentMap (Map.Map AID Agent) deriving Show
-- DAgentMap describes what updateAMap uses to modify AgentMap
data DAgentMap 
  = DAgentMap SubAgentMap
  | LocationUpdate (Map.Map AID Message)
  | ClearOut -- used when eGameState happens to clear out message
  deriving Show

data Location 
  = Location (Either 
               (PlanetName,PTransitionState) (HyperSpace,HTransitionState))
               deriving Show

newtype ToPlanet   = ToPlanet (ToPlanetName, Planet) deriving Show
newtype FromPlanet = FromPlanet (FromPlanetName, Planet) deriving Show

data Planet = Planet 
  { neighbors :: ![(PlanetName,Distance)]
  , refueling :: Bool
  , residents :: ![AID]
  , resources :: ![ResourceName]
  } 
  deriving Show

data HyperSpace = HyperSpace
  { destination       :: ToPlanetName
  , origin            :: FromPlanetName
  , totalDistance     :: PInt
  , distanceTraversed :: PInt
  }
  deriving Show -- Agents are not aware of other
                -- Agents in HyperSpace

data Resource = Resource
  { highestPrice :: PInt
  , lowestPrice  :: PInt
  , currentPrice :: PInt
  , stability    :: Stability
  }
  deriving (Show,Ord,Eq,Read)

data Agent = Player
  { aName    :: ClientName
  , msg      :: ![Message]
  , ship     :: Ship
  , credits  :: PInt
  , debt     :: PInt
  , isDead   :: Bool 
  }
           | Dead ClientName
  deriving Show

data Ship = Ship
  { ship_parts :: ShipParts 
  , ship_stats :: ShipStats
  }
  deriving Show

data ShipParts = ShipParts
  { engine     :: Engine
  , weapons    :: Weapon
  , hull       :: Hull
  , cargoSize  :: CargoSize
  , gasTank    :: TankSize
  , special    :: Maybe Special
  } deriving Show

data ShipStats = ShipStats
  { hull_strength :: HullStrength
  ,cargo         :: [(ResourceName,PInt)]
  ,fuel          :: Fuel
  ,repairing     :: Bool
  ,warp_speed    :: Maybe WarpSpeed
  }
  deriving Show

data Message = PlanetLoc PlanetName 
             | InHyp Location
             | Onward ToPlanetName FromPlanetName
             | JustLandedOn PlanetName
             | AttackMSG (Either AttackedMSG BeenAttackedMSG) AID DieRoll
             | LocalMarketData PlanetName [(ResourceName,Resource)]
             | YouDeadSon
             | GameOver
             | CommandErr CommandError
                deriving Show

data GameMaps = GameMaps {
   bAMap  :: Behavior AgentMap
  ,beLMap :: (Behavior LocationMap, Event ())
  ,bRMap  :: Behavior ResourceMap
  ,bPMap  :: Behavior PlanetMap
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

data Command = Move ToPlanetName
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
            | MarketData PlanetName ![(ResourceName,Resource)]
            | CError CommandError

data ActionPartitions = ActionPartitions {
   moveAction :: Event (Maybe (AID,ToPlanetName))
  ,psAction   :: Event (AID,Result)
  ,hsAction   :: Event (AID,Result)
}

data CommandError = CantMoveTo ToPlanetName
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


