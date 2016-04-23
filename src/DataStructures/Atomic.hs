{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Provides the simple types describing the game
module DataStructures.Atomic
   (Name (..),
    TimeOut,
    DieRoll,
    ToPlanetName (..),
    FromPlanetName (..),
    HTransitionState (..),
    PTransitionState (..),
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
    Distance,
    InHyperSpaceSon (..),
    PInt (..),
    HullStrength (..),
    Fuel (..),
    AID (..),
    CargoSize (..),
    Special (..),
    ClientName)
    where
import     Data.Text
import qualified Data.ByteString.Char8 as BS

type ClientName = BS.ByteString

newtype Name = Name Text deriving (Eq,Show)
newtype PInt = PInt Int deriving (Read) -- Wrapper for Integers that can never 
                                      -- be negative, for truncated subtraction
                                      -- operations
type DieRoll = PInt
type TimeOut = Int
newtype ToPlanetName   = ToPlanetName PlanetName deriving (Ord,Eq,Show,Read)
newtype FromPlanetName = FromPlanetName PlanetName deriving (Ord,Eq,Show,Read)
data PlanetName
  = Vulcan
  | Mongo
  | Arakis
  | Dantooine
  | Tatooine
  | VoidlessVoid
  deriving (Ord,Eq,Enum,Bounded,Show,Read)

data WarpSpeed 
  = Turtle
  | Grandpa
  | Chuggin
  | SpeedBuggy
  | ZoomZoomZoom
  deriving (Show,Enum,Eq,Ord,Read)

data ResourceName 
  = FinestGreen
  | SubstanceD
  | BabyBlue
  | Melange
  | InterzoneSpecial
  deriving (Ord,Eq,Show,Read,Enum,Bounded)

data Stability = Volatile
               | Stable PInt
                  deriving (Show,Eq,Ord,Read)

type Cost    = PInt
type Revenue = PInt
type Amount  = PInt

data CommerceAction 
  = BuyA
  | SellA

data CommerceResult 
  = BuyR Cost
  | SellR Revenue

data PriceRoll 
  = Increment
  | Decrement

data CreditAct 
  = Subtract
  | Add

type Multiplier = PInt

newtype CountDown  = CountDown PInt deriving (Ord,Show,Eq)

data PriceDirection 
  = Up
  | Down
  deriving Show

data HTransitionState 
  = Launched
  | InOrbit
  | InHyperSpace
  deriving Show

data PTransitionState 
  = Landed 
  | PlanetSide 
  deriving Show


data Weapon 
  = PEASHOOTER
  | HITCHSLAP
  | OKAYCANNON
  | DIRTYHARRY
  | KILLMACHINE
  deriving (Enum,Show)

data Engine 
  = HAMSTERPOWER
  | HERBIEPOWER
  | PINTOPOWER
  | BIGHONKINGENGINE
  | ARNOLDPOWER
  deriving (Enum,Show)

data Hull 
  = PaperPlates
  | TinFoil
  | StayFast
  | Elephunt
  | Rhinoceros
  deriving (Show,Enum)

data ShieldSize 
  = GlowWorm
  | FlashLight
  | NotBad
  | ShallNotPass
  | NoYieldShield
  deriving (Enum,Show)

data CargoSize 
  = ShoeBox
  | Closet
  | Trunk
  | Garage
  | AllTheThings
  deriving (Enum,Show)

data TankSize 
  = DixieCup
  | MudPuddle
  | DayTrip
  | Tanker
  | NvrEmpty
  deriving (Enum,Show)

data Special 
  = Cloak
  | SpyBots
  deriving (Enum, Show)

newtype AID = AID Text deriving (Eq,Ord,Show,Read)

data Attacked 
  = Hit (HullStrength -> HullStrength)
  | Miss PInt PInt

data AttackedMSG 
  = YouHit
  | YouMiss PInt PInt
  deriving Show

data BeenAttackedMSG 
  = BeenHitBy
  | FailedAttackBy PInt PInt
  deriving Show

type Distance = PInt

data InHyperSpaceSon = InHyperSpaceSon deriving (Show,Eq,Ord)

newtype HullStrength = HullStrength PInt deriving (Show,Num,Eq)

newtype Fuel = Fuel PInt deriving Show


instance Num PInt where

  x - y = x `truncSub` y
            where
              truncSub (PInt x) (PInt y)
                | y > x     = PInt 0
                | otherwise = PInt (x - y)

  x + y = PInt (fromPInt x + fromPInt y)

  x * y = PInt (fromPInt x * fromPInt y)

  abs x = x

  signum x = 1

  fromInteger x = PInt (fromInteger x)

instance Eq PInt where
   x == y = fromPInt x == fromPInt y
   x /= y = fromPInt x /= fromPInt y

instance Ord PInt where
   x <= y = fromPInt x <= fromPInt y
   x < y         = fromPInt x < fromPInt y
   x > y         = fromPInt x > fromPInt y

instance Show PInt where
   show x = show $ fromPInt x

fromPInt :: PInt -> Int
fromPInt (PInt a) = a
