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

data HullStrength = PaperPlates
                  | TinFoil
                  | StayFast
                  | Elephunt
                  | Rhinoceros
                     deriving (Show,Enum)

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

-- the number is 68. can't go past
