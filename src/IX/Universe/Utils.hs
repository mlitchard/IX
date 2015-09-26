module IX.Universe.Utils
 (setMessage
 ,intToPInt
 ,nextPlayerRoll
 ,mkAgent
 ,getPlanet
 ,getMessage
 ) where

import DataStructures.Atomic
import DataStructures.Composite

import Safe (lookupJustNote)

intToPInt :: Int -> PInt
intToPInt int
   | int < 0   = PInt 0
   | otherwise = PInt int

nextPlayerRoll :: [PInt] -> [PInt]
nextPlayerRoll = drop 1

setMessage :: [Message] -> Agent -> Agent
setMessage message@(_:_) agt = agt { msg = msg agt ++ message}
setMessage [] agt = agt {msg = []}

getMessage :: Agent -> [Message]
getMessage (Player {msg = messages}) = messages
getMessage _                         = [YouDeadSon]

mkAgent :: (AID, Agent) -> Message -> SubAgentMap
mkAgent (aid'', o_agent ) message' = SubAgentMap [(aid'',n_agent)]
   where
      n_agent = setMessage [message'] o_agent

getPlanet :: PlanetName -> PlanetMap -> Planet
getPlanet p_name (PlanetMap p_map) =
   lookupJustNote noPlanet p_name p_map
      where
         noPlanet = 
           "getPlanet failed to find "          ++
           "the following planet in PlanetMap " ++
           show p_name
