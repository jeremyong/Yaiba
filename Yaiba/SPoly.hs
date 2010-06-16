{-# OPTIONS_GHC -fdph-par -fglasgow-exts -XUndecidableInstances #-}
{-# LANGUAGE PArr #-}
module Yaiba.SPoly where

import Data.Map
import Data.Array.Parallel.Prelude
import Yaiba.Sugar
import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal

newtype SPoly ord = SP (Map (Sugar ord) [:Poly ord:])

sPoly :: (Ord (Mon ord)) => (Poly ord,Sugar ord) -> 
         (Poly ord, Sugar ord) -> Maybe (Poly ord, Sugar ord)
sPoly (a,S (a1,a2,a3)) (b,S (b1,b2,b3)) = let l = lcmMon a1 b1
                                              sp = (monMult (l/a1) b2 a) - (monMult (l/b1) a2 b)
                                              (spLT,co) = leadTerm sp
                                              spLTdeg = degree spLT
                                              sug = spLTdeg + max (a3-spLTdeg) (b3-spLTdeg)
                                          in case a1 * b1 == l of
                                            False -> Just (sp,S (spLT,co,sug))
                                            True -> Nothing

syzygy :: (Ord (Mon ord)) =>
          Ideal ord -> (Poly ord, Sugar ord) -> Map (Sugar ord) [:Poly ord:]
syzygy (I as) b = foldl (\x y -> f (sPoly b y) x) empty as where
  f Nothing acc = acc
  f (Just (sp,sug)) acc = insertWith (+:+) sug [:sp:] acc
  
getSPolys :: (Ord (Mon ord)) => Ideal ord -> Ideal ord -> SPoly ord
getSPolys a b = minS $ SP (getSPolys' a b) where
  getSPolys' _ (I []) = empty
  getSPolys' x@(I xs) (I (y:ys)) = unionWith (+:+) (syzygy x y) (getSPolys' (I (y:xs)) (I ys))
  
fitsIn :: Sugar t -> Sugar t -> Bool
fitsIn (S (a1,_,_)) (S (b1,_,_)) = a1 `isFactor` b1
  
minS :: SPoly t -> SPoly t
minS (SP spMap) = SP $ foldlWithKey rmDivisible spMap spMap where
  rmDivisible acc sug _ = mapMaybeWithKey (\k v -> if sug `fitsIn` k then Just v else Nothing) acc