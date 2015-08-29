import Prelude

newtype PInt = PInt Int

instance Show PInt where
   show (PInt a) = show a

instance Num PInt where
   x - y = x `truncSub` y
   x + y = PInt $ (fromPInt x) + (fromPInt y)
   x * y = PInt $ (fromPInt x) * (fromPInt y)
   abs x = x
   signum x = 1
   fromInteger x = PInt $ fromInteger x
 


fromPInt (PInt a) = a  


truncSub (PInt x) (PInt y)
   | y > x = PInt 0
   | otherwise = PInt $ x - y
