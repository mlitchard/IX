module DataStructures.Atomic
   (Name (..),
    TimeOut,
    DieRoll,
    ToPlanet (..),
    FromPlanet (..),
    HTransitionState,
    PTransitionState,
    PlanetName (..),
    ResourceName (..),
    WarpSpeed (..),
    Stability (..),
    CommerceAction (..),
    CommerceResult (..),
    PriceRoll (..),
    CreditAct (..),
    Multiplier,
    CountDown (..),
    Cost,
    Revenue,
    Amount,
    Weapon (..),
    Engine (..),
    Hull (..),
    TankSize (..),
    Attacked (..),
    AttackedMSG (..),
    BeenAttackedMSG (..),
    Distance (..),
    InHyperSpaceSon (..),
    PInt (..),
    HullStrength (..),
    Fuel (..),
    AID (..),
    CargoSize (..),
    Special (..) 
   ) where
import     Data.Text

newtype Name = Name Text deriving (Eq,Show)
type TimeOut = PInt
newtype PInt = PInt Int deriving (Read,Ord,Eq,Show) -- Wrapper for Integers that can never 
                                      -- be negative, for truncated subtraction
                                      -- operations
type DieRoll       = PInt
newtype ToPlanet   = ToPlanet PlanetName deriving (Ord,Eq,Show)
newtype FromPlanet = FromPlanet PlanetName deriving Show

data PlanetName = Vulcan
                | Mongo
                | Arakis
                | Dantooine
                | Tatooine
                | VoidlessVoid
                     deriving (Ord,Eq,Enum,Bounded,Show,Read)

data WarpSpeed = Turtle
               | Grandpa
               | Chuggin
               | SpeedBuggy
               | ZoomZoomZoom
                  deriving (Show,Enum,Eq,Ord,Read)

data ResourceName = FinestGreen
                  | SubstanceD
                  | BabyBlue
                  | Melange
                  | InterzoneSpecial
                     deriving (Ord,Eq,Show,Read,Enum,Bounded)

data Stability = Volatile
               | Stable Int
                  deriving (Show,Eq,Ord,Read)

type Cost    = PInt
type Revenue = PInt
type Amount  = PInt

data CommerceAction = Buy_A
                    | Sell_A

data CommerceResult = Buy_R Cost
                    | Sell_R Revenue

data PriceRoll = Increment
               | Decrement

data CreditAct = Subtract
               | Add

type Multiplier = PInt

newtype CountDown  = CountDown PInt deriving (Ord,Show,Eq)

data PriceDirection = Up
                    | Down
                       deriving Show

data HTransitionState = Launched | InOrbit | InHyperSpace deriving Show
data PTransitionState = Landed | PlanetSide deriving Show


data Weapon = PEASHOOTER
            | HITCHSLAP
            | OKAYCANNON
            | DIRTYHARRY
            | KILLMACHINE
               deriving (Enum,Show)

data Engine = HAMSTERPOWER
            | HERBIEPOWER
            | PINTOPOWER
            | BIGHONKINGENGINE
            | ARNOLDPOWER
               deriving (Enum,Show)

data Hull = PaperPlates
          | TinFoil
          | StayFast
          | Elephunt
          | Rhinoceros
             deriving (Show,Enum)

data ShieldSize = GlowWorm
                | FlashLight
                | NotBad
                | ShallNotPass
                | NoYieldShield
                   deriving (Enum,Show)

data CargoSize = ShoeBox
               | Closet
               | Trunk
               | Garage
               | AllTheThings
                  deriving (Enum,Show)

data TankSize = DixieCup
              | MudPuddle
              | DayTrip
              | Tanker
              | NvrEmpty
                 deriving (Enum,Show)

data Special = Cloak
             | SpyBots
                deriving (Enum, Show)

newtype AID = AID Text deriving (Eq,Ord,Show,Read)

data Attacked = Hit (Int -> Int)
              | Miss PInt PInt

data AttackedMSG = YouHit
                 | YouMiss PInt PInt
                    deriving Show

data BeenAttackedMSG = BeenHitBy
                     | FailedAttackBy PInt PInt
                        deriving Show

newtype Distance = Distance PInt deriving Show

data InHyperSpaceSon = InHyperSpaceSon deriving (Show,Eq,Ord)

newtype HullStrength = HullStrength PInt deriving Show

newtype Fuel = Fuel PInt deriving Show
