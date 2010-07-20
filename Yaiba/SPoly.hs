{-# OPTIONS_GHC -fdph-par -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
{-# LANGUAGE PArr #-}
-- | An object of type SPoly ord is a map from a Sugar to a list of
-- all Polys with that sugar.
module Yaiba.SPoly where

import qualified Data.List as DL
import Yaiba.Sugar
import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Data.Ord
import Control.Parallel.Strategies
import Control.DeepSeq
import Control.Parallel
import Debug.Trace

-- | SPoly is a map of CritPairs keyed to the generators of an ideal.
newtype SPoly ord = SP [ ((Int,Int),CritPair ord) ]

instance NFData (SPoly ord) where
    rnf (SP as) = DL.foldl' (\_ (a,b) -> rnf a `seq` rnf b) () as

-- | CritPair stores a critical pair as a tuple with the sugar and lcm as
-- in Giovini et al. 1991.
newtype CritPair ord = CP (Sugar ord, Mon ord) deriving Eq

instance NFData (CritPair ord) where
    rnf (CP (sug,mon)) = rnf sug `seq` rnf mon

-- | CritPairs are sorted first by sugar, then by lcm.
instance Ord (Mon ord) => Ord (CritPair ord) where
    compare (CP (a,mona)) (CP (b,monb)) = 
        case compare a b of
          LT -> LT
          GT -> GT
          EQ -> compare mona monb

instance Show (CritPair ord) where
    show (CP cp) = show cp 

empty :: SPoly ord
empty = SP []

isEmpty :: SPoly ord -> Bool
isEmpty (SP []) = True
isEmpty _       = False

sizeSP :: SPoly ord -> Int
sizeSP (SP pairs) = DL.length pairs

-- | Delete from the existing queue of CritPairs every pair (i,j) such that
-- [;\\tau_i;] and [;\\tau_j;] strictly divide [;\\tau_k;].
bTest :: SPoly ord -> Ideal ord -> Mon ord -> SPoly ord
bTest (SP ps) fs tauk = let test ((i,j),CP (_,tauij)) = tauk `isFactor` tauij &&
                                                        tau fs i tauk /= tauij &&
                                                        tau fs j tauk /= tauij
                                                        --(monLT $ fst $ fs!i) `strictDiv` tauk &&
                                                        --(monLT $ fst $ fs!j) `strictDiv` tauk
                        in SP $ DL.filter (not . test) ps

constructN :: Ideal ord -> (Poly ord, Sugar ord) -> Int -> [( ((Int,Int), CritPair ord) , Bool)]
constructN fs (fk,S sugk) k = let tauk = monLT fk
                              in [ (((i,k),CP (sug,tauik)),copr) |
                                   i <- [0..(k-1)]
                                 , let (fi,S sugi) = fs!i
                                 , let taui = monLT fi
                                 , let tauik = lcmMon taui tauk
                                 , let sug = S $ degree tauik + max (sugi - degree taui) (sugk - degree tauk)
                                 , let copr = gcdMon taui tauk == Constant ]

-- | Consider the set of new pairs nps. Discard all pairs (j,k) s.t. [;\\tau_j;] and [;\\tau_k;] are
-- comprime, along with any other pair (i,k) s.t. [;\\tau_{jk}\left|\\tau_{ik}\\right.;].
tTest :: [(((Int,Int),CritPair ord),Bool)] -> SPoly ord
tTest nps = SP $ [ ps | (ps,_) <- notcoprs, tTest' ps ] where
    (coprs,notcoprs) = DL.partition snd nps
    tTest' (_,CP (_,taujk)) = DL.null $ filter (\((_,CP (_,tauik)),_) -> tauik `isFactor` taujk) coprs

-- | Discards from a set of new pairs all pairs (j,k) s.t. [;\\tau_{ik}\left|\\tau_{jk}\\right.;] with (i,k)
-- before (j,k) when sorted according to sugar (Fussy).
mTest :: Ord (Mon ord) => SPoly ord -> SPoly ord
mTest (SP nps) = SP $ [ ps | ps <- nps, mTest' ps ] where
    mTest' ((j,_),jkcp@(CP (_,taujk))) = DL.null [ qs |
                                                   qs@((i,_),ikcp@(CP (_,tauik))) <- nps
                                                 , i /= j
                                                 , ikcp <= jkcp
                                                 , taujk `isFactor` tauik]

-- | Applies the b-test to the existing queue. Then applies the t-test and m-test to the new pairs
-- with the addition of an [;f_k;]. Finally, merges the results to produce a new queue.
updateSPolys :: Ord (Mon ord) => SPoly ord -> (Poly ord, Sugar ord) -> Ideal ord -> SPoly ord
updateSPolys oldsp (fk,sugk) ideal = let SP bPass = bTest oldsp ideal (monLT fk)
                                         nsp = constructN ideal (fk,sugk) (numGens ideal)
                                         tPass = tTest nsp
                                         SP mPass = mTest tPass
                                     in SP $ bPass ++ mPass


delFindSingleLowest :: Ord (Mon ord) => SPoly ord -> Ideal ord -> ((Poly ord, Sugar ord),SPoly ord)
delFindSingleLowest (SP []) _ = ((nullPoly,S 0),empty)
delFindSingleLowest (SP pairs) ideal = let minsug = findMinSug pairs
                                           (bottom':top',top) = DL.partition (\(_,CP (sug,_)) -> sug == minsug) pairs
                                           bottom = toSPoly ideal bottom'
                                       in (bottom, SP (top'++top))
{-
delFindSingleLowest (SP []) _ = ((nullPoly, S 0), empty)
delFindSingleLowest (SP pairs) ideal = let lowest:rest = DL.sortBy (comparing snd) pairs
                                           lowestSPoly = toSPoly ideal lowest
                                       in (lowestSPoly, SP rest)
-}

delFindLowest :: Ord (Mon ord) => SPoly ord -> Ideal ord -> ([(Poly ord, Sugar ord)],SPoly ord)
delFindLowest (SP []) _ = ([],empty)
delFindLowest (SP pairs) ideal = let minsug = findMinSug pairs
                                     (bottom',top) = DL.partition (\(_,CP (sug,_)) -> sug == minsug) pairs
                                     bottom = DL.map (toSPoly ideal) bottom'
                                 in --("Bin size: " ++ show (DL.length bottom) ++ " Sugar: " ++ show minsug) `trace` 
                                        (bottom, SP top)

findMinSug :: [(t, CritPair ord)] -> Sugar ord
findMinSug [] = S 0
findMinSug ((_,CP (initsug,_)):as) = DL.foldl' (\acc (_,CP (sug,_)) -> min sug acc) initsug as

toSPoly :: Ord (Mon ord) =>
           Ideal ord -> ((Int, Int), CritPair ord) -> (Poly ord, Sugar ord)
toSPoly ideal ((i,j),(CP (sug,_))) = let (polyi,_) = ideal ! i
                                         (polyj,_) = ideal ! j
                                         (taui,ci) = leadTerm polyi
                                         (tauj,cj) = leadTerm polyj
                                         tauij = lcmMon taui tauj
                                         spoly = monMult (divide tauij taui) cj polyi -
                                                 monMult (divide tauij tauj) ci polyj
                                     in (spoly,sug)