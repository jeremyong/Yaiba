{-# OPTIONS_GHC -fdph-par -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
{-# LANGUAGE PArr #-}
-- | An object of type SPoly ord is a map from a Sugar to a list of
-- all Polys with that sugar.
module Yaiba.SPoly where

import Yaiba.Map
import qualified Data.List as DL
import qualified Data.Set as DS
import Yaiba.Sugar
import Yaiba.Monomial
import Yaiba.Polynomial
import Control.Parallel
import Yaiba.Ideal

newtype SPoly ord = SP (Map (Sugar ord) (DS.Set (Poly ord)))

-- | Auxilliary function to syzygy
sPoly :: (Ord (Mon ord)) => (Poly ord,Sugar ord) -> 
         (Poly ord, Sugar ord) -> Maybe (Poly ord, Sugar ord, Mon ord)
sPoly (a,S a') (b,S b') = let (a1,a2) = leadTerm a
                              (b1,b2) = leadTerm b
                              !g = gcdMon a1 b1
                              l = lcmMon a1 b1
                              sp = monMult (divide l a1) b2 a - monMult (divide l b1) a2 b
                              (spLT,_) = leadTerm sp
                              spLTdeg = degree spLT
                              sug = spLTdeg + max (a'-spLTdeg) (b'-spLTdeg)
                          in if g == Constant then
                               Nothing
                             else
                               Just (sp,S sug,l)

-- | Convolves a supplied Poly with an ideal, generating S-Polys and
-- throwing away those for which the lead terms are relatively prime.
syzygy :: (Ord (Mon ord)) =>
          Ideal ord -> (Poly ord, Sugar ord) -> 
          Ideal ord -> [(Poly ord, Sugar ord)]
syzygy (I as) b (I cs) = DL.foldl' (\x y -> f (sPoly b y) x) [] as where
  f Nothing acc = acc
  f (Just res) acc = let reformat (poly,_) = (monLT poly,poly)
                         cs' = DL.map reformat cs
                     in if DL.null $ DL.filter (\(cLT,cPoly) -> cPoly /= fst b 
                                                && isFactor cLT (thd res)) cs' then
                            (fstTwo res):acc
                        else
                            acc

-- | Extracts the minimal generating set in the third component of the argument,
-- assumed to be the LCM of the LTs of the Polys used to generate the S-Poly in the
-- first component.
{-
minimize :: [(Poly ord, Sugar ord, Mon ord)] -> [(Poly ord, Sugar ord)]
minimize as = DL.map (\(!a,!b,_) -> (a,b)) $! DL.filter (\(_,_,!x) -> isMinimal x) as where
    isMinimal a = DL.null $! DL.filter (\(_,_,!x) -> a /= x && isFactor x a) as
-}
minimize :: [(Poly ord, Sugar ord, Mon ord)] -> [(Poly ord, Sugar ord)]
minimize as = DL.map (\(!a,!b,_) -> (a,b)) $! DL.filter (\(_,_,!x) -> isMinimal x) as where
    isMinimal a = DL.null $! DL.filter (\(_,_,!x) -> a /= x && isFactor x a) as
{-
minimize as = DL.map (\(!a,!b,_) -> (a,b)) $! minimize' as [] where
    minimize' [] res = res
    minimize' (x:xs) res = let !nubbed = DL.filter (\(_,_,!y) -> not $! isFactor (thd x) y) xs
                           in minimize' nubbed (x:res)
-}
-- | Convolves two lists, returning an SPoly map using syzygy and minimize.
getSPolys :: (Ord (Mon ord)) => Ideal ord -> Ideal ord -> SPoly ord
getSPolys a' b' = getSPolyt a' b' where
    getSPolyt a b = SP $! DL.foldl' (\(!acc) (!v,!k) -> insertWith DS.union k (DS.singleton v) acc) 
                    empty 
                    (getSPolys' a b) where
                                  getSPolys' _ (I []) = []
                                  getSPolys' x@(I xs) (I (y:ys)) = let !minS = syzygy x y a'
                                                                       nextS = getSPolys' (I (y:xs)) (I ys)
                                                                   in minS ++ nextS

thd :: (a,b,c) -> c
thd (_,_,x) = x

fstTwo :: (a,b,c) -> (a,b)
fstTwo (x,y,_) = (x,y)