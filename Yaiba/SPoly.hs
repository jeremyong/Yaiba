{-# OPTIONS_GHC -fdph-par -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
{-# LANGUAGE PArr #-}
-- | An object of type SPoly ord is a map from a Sugar to a list of
-- all Polys with that sugar.
module Yaiba.SPoly where

import qualified Data.Map as DM
import qualified Data.IntSet as DI
import qualified Data.Vector as DV
import qualified Data.List as DL
import Yaiba.Sugar
import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Data.Ord
import Control.Parallel.Strategies
import Control.DeepSeq

-- | SPoly is a map of CritPairs keyed to the ideal provided in the second argument.
data SPoly ord = SP (DM.Map (Int, Int) (CritPair ord)) (Ideal ord)

-- | CritPair stores a critical pair as a tuple with the sugar and lcm as described
-- in the Sugar paper.
newtype CritPair ord = CP (Sugar ord, Mon ord) deriving Eq

instance NFData (CritPair ord) where
    rnf (CP (sug,mon)) = rnf sug `seq` rnf mon

instance Ord (CritPair ord) where
    compare (CP (a,_)) (CP (b,_)) = compare a b

empty = SP (DM.empty) (I $ DV.empty)

isEmpty (SP spmap _) = DM.null spmap

sizespMap (SP spmap _) = DM.size spmap

updateSPolys :: Ord (Mon ord) => SPoly ord -> (Poly ord, Sugar ord) -> SPoly ord
updateSPolys (SP cpMap oldGens) (newGen,sug) = let !k = numGens oldGens
                                                   --mPass = mTest (SP cpMap oldGens) newGen
                                                   pairs = pairing oldGens (newGen,sug)
                                                   --(fPass,bPass) = (fTest pairs, bTest cpMap fPass newGen k) `using` parPair rdeepseq rdeepseq
                                                   (fPass,bPass) = (fTest pairs, bTest cpMap fPass newGen k)
                                                   newcpMap = DM.union bPass fPass
                                               in SP newcpMap (snoc oldGens (newGen,sug))
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
                                                   in DM.insertWith (\(nv,copr) (l,coprAcc) -> (l++nv,copr || coprAcc))
                                                             tauik ([((i,k),cp)],coprime) filteredAcc
    reformat minMap = let notCoprime = DM.filter (\(_,copr) -> not copr) minMap
                          reformat' _ ([],_) acc = acc
                          reformat' _ ((k,v):_,_) acc = DM.insert k v acc
                          --reformat' _ (kvs,_) acc = let (k,v) = DL.maximumBy (comparing fst) kvs
                          --                          in DM.insert k v acc
                      in DM.foldrWithKey reformat' DM.empty notCoprime

{-
bTest oldMap nMap newGen k = DM.mapMaybeWithKey bTest' oldMap where
  bTest' (i,j) (CP (sug,tauij)) = let lookupi = DM.lookup (i,k) nMap
                                      lookupj = DM.lookup (j,k) nMap
                                      tauk = monLT newGen
                                      taukDivides = tauk `isFactor` tauij
                                  in if taukDivides then case lookupi of
                                                           Nothing -> case lookupj of
                                                                        Nothing -> Nothing
                                                                        Just (CP (_,taujk)) -> if taujk /= tauij then
                                                                                                   Nothing
                                                                                               else Just (CP (sug,tauij))
                                                           Just (CP (_,tauik)) -> case lookupj of
                                                                                    Nothing -> if tauik /= tauij then
                                                                                                   Nothing
                                                                                               else Just (CP (sug,tauij))
                                                                                    Just (CP (_,taujk)) -> if tauij /= tauik && tauij /= taujk && tauik /= taujk then
                                                                                                               Nothing
                                                                                                           else
                                                                                                               Just (CP (sug,tauij))
                                     else
                                         Just (CP (sug,tauij))

-}
bTest oldMap nMap newGen k = DM.mapMaybeWithKey bTest' oldMap where
  bTest' (i,j) (CP (sug,tauij)) = let lookupi = DM.lookup (i,k) nMap
                                      lookupj = DM.lookup (j,k) nMap
                                      tauk = monLT newGen
                                      taukDivides = tauk `isFactor` tauij
                                  in if taukDivides then
                                         if lookupi == Nothing || lookupj == Nothing
                                         then Nothing
                                         else let Just (CP (_,tauik)) = lookupi
                                                  Just (CP (_,taujk)) = lookupj
                                              in if tauik /= taujk && tauik /= tauij && taujk /= tauij
                                                 then Nothing
                                                 else Just (CP (sug,tauij))
                                     else
                                       Just (CP (sug,tauij))

delFindLowest (SP spMap ideal) = let !sugSet = DM.fold (\(CP (S x,_)) acc -> DI.insert x acc) DI.empty spMap
                                     !minSug = S $ DI.findMin sugSet
                                     !(bottom,top) = DM.partition (\(CP (x,_)) -> x == minSug) spMap
                                     !botelems = toSPolys (SP bottom ideal)
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