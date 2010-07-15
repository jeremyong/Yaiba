{-# OPTIONS_GHC -fdph-par -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
{-# LANGUAGE PArr #-}
-- | An object of type SPoly ord is a map from a Sugar to a list of
-- all Polys with that sugar.
module Yaiba.SPoly where

import qualified Data.Map as DM
import qualified Data.IntSet as DI
import qualified Data.Vector as DV
import Yaiba.Sugar
import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Debug.Trace

-- | SPoly is a map of CritPairs keyed to the ideal provided in the second argument.
data SPoly ord = SP (DM.Map (Int, Int) (CritPair ord)) (Ideal ord)

-- | CritPair stores a critical pair as a tuple with the sugar and lcm as described
-- in the Sugar paper.
newtype CritPair ord = CP (Sugar ord, Mon ord) deriving Eq

empty = SP (DM.empty) (I $ DV.empty)

isEmpty (SP spmap _) = DM.null spmap

sizespMap (SP spmap _) = DM.size spmap

updateSPolys :: Ord (Mon ord) => SPoly ord -> (Poly ord, Sugar ord) -> SPoly ord
updateSPolys (SP cpMap oldGens) (newGen,sug) = let !k = numGens oldGens
                                                   --mPass = mTest (SP cpMap oldGens) newGen
                                                   pairs = pairing oldGens (newGen,sug)
                                                   fPass = fTest pairs
                                                   --bPass = bTest mPass fPass newGen k
                                                   bPass = bTest cpMap fPass newGen k
                                                   --fPass2 = fTest2 fPass
                                                   newcpMap = DM.union bPass fPass
                                                   --newcpMap = DM.union mPass fPass
                                                   --newcpMap = DM.union bPass mPass
                                                   --newcpMap = DM.union cpMap fPass
                                                   --newcpMap = DM.union mPass (DM.map (\(x,_) -> x) pairs)
                                                   --newcpMap = DM.union cpMap (DM.map (\(x,_) -> x) pairs)
                                               in --("spmap size:" ++ show (DM.size newcpMap)) `trace` 
                                                  SP newcpMap (snoc oldGens (newGen,sug))
{-
-- | Criterion M described in Gebauer-Moller 1988. Sloppy variant from "One sugar cube, please"
mTest (SP cpMap oldGens) newGen = DM.mapMaybeWithKey mTest' cpMap where
    ltk = monLT newGen
    mTest' (i,j) cp@(CP (_,tauij)) = let tauik = tau oldGens i ltk
                                         taujk = tau oldGens j ltk
                                     in if tauij `isFactor` tauik && tauij `isFactor` taujk && tauij /= tauik && tauij /= taujk then
                                            --("DeletedM"++ show i ++ show j) `trace` 
                                            Nothing
                                        else
                                            Just cp
-}
pairing oldGens (newGen,S sugk) = ifoldl' pairing' DM.empty oldGens where
  k = numGens oldGens
  pairing' acc index (poly,S sugi) = let taui = monLT poly
                                         tauk = monLT newGen
                                         tauik = lcmMon tauk taui
                                         g = gcdMon taui tauk
                                         coprime = g == Constant
                                         newsug = S $ (degree tauik) + max (sugi - (degree taui)) (sugk - (degree tauk))
                                     in DM.insert (index,k) (CP (newsug,tauik),coprime) acc

-- | Criterion F described in Gebauer-Moller 1988.
{-
fTest nMap = let (coprimes,notCoprimes) = DM.partition snd nMap
                 coprimes' = DM.map (\(x,_) -> x) coprimes
                 notCoprimes' = DM.map (\(x,_) -> x) notCoprimes
             in DM.fold fTest' notCoprimes' coprimes' where
                 fTest' (CP (_,tauik)) acc = DM.mapMaybe (\cp@(CP (_,taujk)) -> if tauik == taujk then 
                                                                                    --("DeletedF"++show tauik) `trace` 
                                                                                    Nothing 
                                                                                else Just cp) acc
-}
fTest nMap = reformat $ DM.foldrWithKey fTest' DM.empty nMap where
    fTest' (i,k) (cp@(CP (_,tauik)),coprime) acc = let filteredAcc = DM.filterWithKey (\taujk _ -> not $ tauik `isFactor` taujk) acc
                                                   in DM.insertWithKey (\k (nv,copr) (l,coprAcc) -> (nv++l,copr || coprAcc))
                                                             tauik ([((i,k),cp)],coprime) filteredAcc
    reformat minMap = let notCoprime = DM.filter (\(_,copr) -> not copr) minMap
                          reformat' _ ((k,v):_,_) acc = DM.insert k v acc
                      in DM.foldrWithKey reformat' DM.empty notCoprime

{-
fTest nMap = DM.foldrWithKey fTest' DM.empty nMap where
    fTest' (j,k) (cp@(CP (_,taujk)),coprime) acc = if coprime then
                                                       DM.insert (j,k) cp $ DM.filter (\(CP (_,tauij)) -> tauij /= taujk) acc
                                                   else
                                                       DM.insert (j,k) cp acc
-}
fTest2 nMap = DM.foldrWithKey bTest' nMap nMap where
    bTest' key@(j,_) (CP (_,taujk)) acc = if DM.member key acc then
                                              DM.filterWithKey (\(i,_) (CP (_,tauik)) -> i>=j || not (tauik `isFactor` taujk)) acc
                                          else 
                                              acc


bTest oldMap nMap newGen k = DM.mapMaybeWithKey bTest' oldMap where
  bTest' (i,j) (CP (sug,tauij)) = let lookupi = DM.lookup (i,k) nMap
                                      lookupj = DM.lookup (j,k) nMap
                                      tauk = monLT newGen
                                      taukDivides = tauk `isFactor` tauij
                                  in if taukDivides then case lookupi of
                                    Nothing -> case lookupj of
                                      Nothing -> --("DeletedB"++ show i ++ show j) `trace` 
                                                            Nothing
                                      Just (CP (_,taujk)) -> if taujk /= tauij then
                                                               --("DeletedB"++ show i ++ show j) `trace` 
                                                               Nothing
                                                             else Just (CP (sug,tauij))
                                    Just (CP (_,tauik)) -> case lookupj of
                                      Nothing -> if tauik /= tauij then
                                                   --("DeletedB"++ show i ++ show j) `trace` 
                                                     Nothing
                                                 else Just (CP (sug,tauij))
                                      Just (CP (_,taujk)) -> if tauij /= tauik && tauij /= taujk && tauik /= taujk then
                                                               --("DeletedB"++ show i ++ show j) `trace` 
                                                                 Nothing
                                                             else
                                                               Just (CP (sug,tauij))
                                     else
                                       Just (CP (sug,tauij))

delFindLowest (SP spMap ideal) = let sugSet = DM.fold (\(CP (S x,_)) acc -> DI.insert x acc) DI.empty spMap
                                     minSug = S $ DI.findMin sugSet
                                     (bottom,top) = DM.partition (\(CP (x,_)) -> x == minSug) spMap
                                     botelems = toSPolys (SP bottom ideal)
                                 in (botelems,top)

toSPolys (SP spMap ideal) = DM.foldlWithKey toSPolys' [] spMap where
    toSPolys' acc (i,j) (CP (sug,_)) = let (polyi,_) = ideal ! i
                                           (polyj,_) = ideal ! j
                                           (taui,ci) = leadTerm polyi
                                           (tauj,cj) = leadTerm polyj
                                           tauij = lcmMon taui tauj
                                           spoly = monMult (divide tauij taui) cj polyi -
                                                   monMult (divide tauij tauj) ci polyj
                                       in (spoly,sug):acc