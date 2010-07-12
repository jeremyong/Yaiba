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
import Control.Parallel

-- | SPoly is a map of CritPairs keyed to the ideal provided in the second argument.
data SPoly ord = SP (DM.Map (Int, Int) (CritPair ord)) (Ideal ord)

-- | CritPair stores a critical pair as a tuple with the sugar and lcm as described
-- in the Sugar paper.
newtype CritPair ord = CP (Sugar ord, Mon ord) deriving Eq

empty = SP (DM.empty) (I $ DV.empty)

isEmpty (SP spmap _) = DM.null spmap

sizespMap (SP spmap _) = DM.size spmap

updateSPolys :: Ord (Mon ord) => SPoly ord -> (Poly ord, Sugar ord) -> SPoly ord
updateSPolys (SP cpMap oldGens) (newGen,sug) = let mPass = mTest (SP cpMap oldGens) newGen
                                                   pairs = pairing oldGens (newGen,sug)
                                                   fPass = fTest pairs
                                                   !tupmap = mPass `par` (fPass `pseq` (mPass,fPass))
                                                   newcpMap = DM.union (fst tupmap) (snd tupmap)
                                                   --newcpMap = DM.union cpMap fPass
                                                   --newcpMap = DM.union mPass (DM.map (\(x,_) -> x) pairs)
                                                   --newcpMap = DM.union cpMap (DM.map (\(x,_) -> x) pairs)
                                               in --("spmap size:" ++ show (DM.size newcpMap)) `trace` 
                                                  SP newcpMap (snoc oldGens (newGen,sug))

-- | Criterion M described in Gebauer-Moller 1988. Sloppy variant from "One sugar cube, please"
mTest (SP cpMap oldGens) newGen = DM.mapMaybeWithKey mTest' cpMap where
    ltk = monLT newGen
    mTest' (i,j) (CP cpair) = let tauij = snd cpair
                                  tauik = tau oldGens i ltk
                                  taujk = tau oldGens j ltk
                              in if tauij `isFactor` tauik && tauij `isFactor` taujk then
                                     Nothing
                                 else
                                     Just $ CP cpair


pairing oldGens (newGen,S sugk) = ifoldl' pairing' DM.empty oldGens where
  k = numGens oldGens
  pairing' acc index (poly,S sugi) = let (taui,_) = leadTerm poly
                                         (tauk,_) = leadTerm newGen
                                         tauik = lcmMon tauk taui
                                         g = gcdMon taui tauk
                                         coprime = g == Constant
                                         newsug = S $ (degree tauik) + max (sugi - (degree taui)) (sugk - (degree tauk))
                                     in DM.insert (index,k) (CP (newsug,tauik),coprime) acc

-- | Criterion F described in Gebauer-Moller 1988.
fTest nMap = let (coprimes,notCoprimes) = DM.partition snd nMap
                 coprimes' = DM.map (\(x,_) -> x) coprimes
                 notCoprimes' = DM.map (\(x,_) -> x) notCoprimes
             in DM.union coprimes' (DM.fold fTest' notCoprimes' coprimes') where
                 fTest' cpair acc = DM.mapMaybe (\cpair' -> if cpair' == cpair then Nothing else Just cpair') acc

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