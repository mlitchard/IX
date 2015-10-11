-- This module manages the logic related to doing damage to others

module IX.Universe.Combat
   (dmgToAgt
   ,evalZap
   ) where

import DataStructures.Atomic
import DataStructures.Composite
import Control.Monad (join)
import Data.List (find)
import qualified Data.List as DL
import Control.Applicative ((<$>))
import IX.Universe.Utils (setMessage,intToPInt)
import Safe (fromJustNote)
import qualified Data.Map.Strict as Map

--dmgToAgt
-- Takes the result of a combat check
-- Makes am entry in DAgentMap 
dmgToAgt :: AgentMap     ->
            (AID,Result) ->
            Maybe DAgentMap
dmgToAgt (AgentMap aMap') (aid, (Damage zapped dRoll zapRes)) =
   case zapRes of
      Hit zapDamage    ->
         Just      $ 
         DAgentMap $ 
         SubAgentMap (Map.fromList [(aid,attackerAGT),(zapped,attackedAGT)])
         where
            attackerAGT = setMessage youHit aAgent
            youHit      = [AttackMSG (Left YouHit) zapped dRoll]
            hStrength   = hull_strength $ 
                          ship_stats    $
                          ship zAgent
            attackedAGT =
               setMessage beenHit      $ 
               setHullStrength postDMG $ 
               setDead isDead' zAgent

            postDMG     = zapDamage $ hStrength
            isDead'     = if (postDMG == 0) then True else False
            beenHit     = if isDead'
                          then
                            [AttackMSG (Right BeenHitBy) aid dRoll] ++
                            [YouDeadSon]
                          else
                            [AttackMSG (Right BeenHitBy) aid dRoll]

      Miss dRoll' toHit ->
         Just      $
         DAgentMap $
         SubAgentMap (Map.fromList [(aid,attackerAGT),(zapped,attackedAGT)])
            where
               attackerAGT =
                  setMessage doneMissed aAgent
               doneMissed  =
                  [AttackMSG (Left $ YouMiss dRoll' toHit) zapped dRoll']
               attackedAGT =
                  setMessage failedAttack zAgent
               failedAttack =
                  [AttackMSG (Right $ FailedAttackBy dRoll' toHit) aid dRoll']
   where
      aAgent = fromJustNote aAgentFail (Map.lookup aid aMap')
      zAgent  = fromJustNote zaFail (Map.lookup zapped aMap')
      aAgentFail = "dmgAgt failed to lookup acting agent in aMap" ++ (show aid)
      zaFail = "dmgToAgt failed to lookup " ++ (show zapped)
dmgToAgt _ _ = Nothing

setDead :: Bool -> Agent -> Agent
setDead False agt = agt
setDead True agt  = agt {isDead = True}

setHullStrength :: HullStrength -> Agent -> Agent
setHullStrength hs agt = agt { ship = new_ship }
   where
      (Ship ship_parts ship_stats) = ship agt
      new_ship = (Ship ship_parts ship_stats {hull_strength = hs})

-- evaluates an attack attempt

evalZap :: (AID,Agent) ->
           Agent       ->
           PlanetName  ->
           DieRoll     ->
           PlanetMap   ->
           Result
evalZap (aid,(Dead _)) _ _ _ _ = CError $ CantZap aid
evalZap (aidATKD,agtATKD) agtATK pName dRoll (PlanetMap pMap') =
   let mTarget = join                  $
                 DL.find (== aidATKD) <$>
                 residents            <$>
                 Map.lookup pName pMap'
   in case mTarget of
        Just _
           | dRoll > toHit -> Damage aidATKD dRoll $ Hit (subtract wDMG)
           | otherwise     -> Damage aidATKD dRoll $ Miss dRoll toHit
        Nothing            -> CError $ CantZap aidATKD
   where
      toHit           = intToPInt $ 
                        (80 + ((attackedEngine + 1 ) * 20)) -
                        ((attackerWeapons +1) * 20)

      attackedEngine  = fromEnum   $
                        engine     $ 
                        ship_parts $
                        ship agtATKD

      attackerWeapons = fromEnum   $
                        weapons    $
                        ship_parts $
                        ship agtATK

      wDMG            = HullStrength $
                        PInt         $
                        ((attackerWeapons + 1) * 4)

