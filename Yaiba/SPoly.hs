{-# OPTIONS_GHC -fdph-par -fglasgow-exts -XUndecidableInstances #-}
{-# LANGUAGE PArr #-}
module Yaiba.SPoly where

import qualified Data.Map as DM
import Data.Array.Parallel.Prelude
import Yaiba.Sugar
import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal

newtype SPoly ord = SP (DM.Map (Sugar ord) [:Poly ord:])

sPoly :: (Ord (Mon ord)) => (Poly ord,Sugar ord) -> 
         (Poly ord, Sugar ord) -> Maybe (Poly ord, Sugar ord)
sPoly (a,S (a1,a2,a3)) (b,S (b1,b2,b3)) = let l = lcmMon a1 b1
                                              sp = (monMult (l/a1) (b2/a2) a) - (monMult (l/b1) 1 b)
                                              (spLT,co) = leadTerm sp
                                              spLTdeg = degree spLT
                                              sug = spLTdeg + max (a3-spLTdeg) (b3-spLTdeg)
                                          in case a1 * b1 == l of
                                            False -> Just (sp,S (spLT,co,sug))
                                            True -> Nothing

syzygy :: (Ord (Mon ord)) =>
          Ideal ord -> (Poly ord, Sugar ord) -> DM.Map (Sugar ord) [:Poly ord:]
syzygy (I as) b = foldl (\x y -> f (sPoly b y) x) DM.empty as where
  f Nothing acc = acc
  f (Just (sp,sug)) acc = DM.insertWith (+:+) sug [:sp:] acc
  
getSPolys :: (Ord (Mon t)) =>
             Ideal t -> Ideal t -> DM.Map (Sugar t) [:Poly t:]
getSPolys _ (I []) = DM.empty
getSPolys xs@(I xs') (I (y:ys)) = DM.unionWith (+:+) (syzygy xs y) (getSPolys (I (y:xs')) (I ys))