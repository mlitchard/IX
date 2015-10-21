module IX.Universe.Market
 (nextMarketRolls
 ,adjustMarket
 ,evalMarket
 ,evalCommerce
 ,marketToAgt
 ,commerceToAgt)
 where

import DataStructures.Atomic
import DataStructures.Composite
import Data.Maybe (catMaybes)
import Safe (fromJustNote)
import qualified Data.List as DL
import Data.List (find)
import Data.List.Utils (addToAL)
import Data.Text (unpack)
import qualified Data.Map as M

import IX.Universe.Utils (setMessage)

nextMarketRolls :: ResourceMap -> [PInt] -> [PInt]
nextMarketRolls (ResourceMap rMap)  rolls = drop (M.size rMap) rolls

adjustMarket :: [PInt] -> ResourceMap -> ResourceMap
adjustMarket dRolls (ResourceMap rMap) =
   let currentRolls = take (M.size rMap) dRolls
       matchedRolls = zip rMap' currentRolls
   in ResourceMap $ M.fromList (map adjustLocalMarket matchedRolls)
   where
     rMap' = M.toList rMap

adjustLocalMarket ((rName,res),roll) =
   case stability res of
      Stable cDown -> manageStability cDown
      Volatile     -> (rName,priceADJ)
   where
      priceADJ
         | roll <= 50 = stableOrDec
         | otherwise  = stableOrInc
      stableOrDec
         | cPrice > lPrice = stability_check Decrement
         | otherwise       = res
      stableOrInc
         | cPrice < hPrice = stability_check Increment
         | otherwise         = res
      stability_check Decrement
         | roll <= 20 = stablize dec_price
         | otherwise  = dec_price
      stability_check Increment
         | roll >= 80 = stablize inc_price
         | otherwise    = inc_price
      manageStability (PInt cDown)
         | cDown == 0 = (rName,endStability)
         | otherwise  = (rName,decStability)
         where
            decStability  = res {stability  = Stable $ PInt (cDown - 1)}
      endStability  = res {stability  = Volatile}
      stablize res' = res' {stability = Stable $ PInt 500}
      cPrice        = currentPrice res
      lPrice        = lowestPrice res
      hPrice        = highestPrice res
      dec_price     = res {currentPrice = cPrice - pDec}
      inc_price     = res {currentPrice = cPrice + pDec}
      pDec          = PInt 1

evalMarket :: PlanetName ->
              Planet     ->
              ResourceMap ->
              Result
evalMarket p_name planet (ResourceMap r_map) =
   (MarketData p_name r_data)
   where
      r_map' = M.toList r_map
      r_list = resources planet
      r_data = zip r_list $ catMaybes $ map (flip lookup r_map') r_list
      
evalCommerce :: CommerceAction             ->
                Agent                      ->
                (ResourceName,ResourceMap) ->
                PInt                       ->
                (PlanetName,Planet)        ->
                Result
evalCommerce c_action agt (r_name,(ResourceMap r_map)) amt (p_name,planet') =
   case c_action of
      BuyA  -> buy_offer
      SellA -> sell_offer
   where
      buy_offer =
         case p_res of
            Just _  -> buy_attempt
               where
                  buy_attempt
                     | (credits agt) >= cost =
                          Commerce (BuyR cost) (r_name,res) amt
                     | otherwise             =
                          CError $ CantAfford r_name cost (credits agt)
                     where
                        cost = c_res
            Nothing -> not_for_sale

      sell_offer =
         case p_res of
            Just _  -> sell_attempt
            Nothing -> CError $ NotInTheMarketFor r_name p_name
         where
            sell_attempt
               | (held_comm >= amt) = Commerce (SellR revenue) (r_name,res) amt
               | otherwise          = CError $ YouDontHave r_name amt
            revenue                 = c_res

      p_res = DL.find (== r_name) $ resources planet'
      c_res = (currentPrice res) * amt
      not_for_sale = CError $ DontHave r_name
      held_comm =
         let m_amt = lookup r_name $
                     cargo         $
                     ship_stats    $
                     ship agt
         in case m_amt of
               Just amt_ -> amt_
               Nothing   -> PInt 0

      cant_afford  = CError $ CantAfford r_name c_res creds
      res = fromJustNote resFail (M.lookup r_name r_map)
      creds = credits agt
      resFail = "evalCOmmerce failed to find " ++
                show r_name                    ++
                "in resource map\n"


marketToAgt :: AgentMap -> M.Map AID Result -> [Maybe DAgentMap]
marketToAgt (AgentMap a_map) resList =
  snd `fmap` M.toList (M.mapWithKey marketToAgt' resList)
  where 
    marketToAgt' aid@(AID aid') (MarketData p_name local_res) =
      Just $ 
      DAgentMap (mkAgent (aid,o_agent) (LocalMarketData p_name local_res))
      where
        o_agent :: Agent
        o_agent = fromJustNote aAgentFail ((M.lookup aid a_map) :: Maybe Agent)
        aAgentFail = "lookToAgt failed to match aid " ++ (unpack aid')
        mkAgent :: (AID, Agent) -> Message -> SubAgentMap
        mkAgent (aid'', o_agent ) message' = 
          SubAgentMap (M.singleton aid'' n_agent)
          where
          n_agent = setMessage [message'] o_agent
    marketToAgt' _ _ = Nothing

commerceToAgt :: AgentMap          ->
                 M.Map AID Result  ->
                 [Maybe DAgentMap]
commerceToAgt (AgentMap a_map) resList =
  snd `fmap` M.toList (M.mapWithKey commerceToAgt' resList)
  where
    commerceToAgt' aid (Commerce c_action (r_name,res) amt) =
      let agt = fromJustNote aAgentFail (M.lookup aid a_map)
      in case c_action of
           BuyR cost     -> -- code smell
             Just $ DAgentMap (SubAgentMap (M.singleton aid (buy agt cost)))
           SellR revenue -> -- code smell
            Just $ DAgentMap (SubAgentMap (M.singleton aid (sell agt revenue)))
      where
        buy agt cost =
          let agtLessCost = setCredits Subtract agt cost
          in  set_agt_commodity c_action agtLessCost
        sell agt revenue =
          let agtAddRevenue = setCredits Add agt revenue
          in  set_agt_commodity c_action agtAddRevenue

        set_agt_commodity :: CommerceResult -> Agent -> Agent
        set_agt_commodity c_res
                          agt@(Player {ship = Ship shipParts shipStats}) =
          case c_res of
            (BuyR _)  -> agt {ship = up_ship (+)}
            (SellR _) -> agt {ship = up_ship (-)}
          where
            res_inv         = fromJustNote resourceFail (lookup r_name c_inv)
            c_inv           = cargo $ ship_stats'
            ship_stats'     = ship_stats (ship agt)
            up_inv  f       = addToAL c_inv r_name (res_inv `f` amt)
            set_inv up_inv' = ship_stats' {cargo = up_inv'}
            up_ship f       = Ship shipParts (set_inv (up_inv f))
        setCredits c_act agt t_creds =
          let creds = credits agt
          in case c_act of
               Subtract -> agt {credits = creds - t_creds}
               Add      -> agt {credits = creds + t_creds}
        aAgentFail      = "commerceToAgt failed to lookup" ++
                          "acting agent in aMap"           ++
                          (show aid)
        resourceFail    = "commerceAGgt failed to find " ++ 
                          (show r_name)                  ++
                          "in inventory of"              ++
                          (show aid)

    commerceToAgt' _ _ = Nothing
