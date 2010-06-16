{-# OPTIONS_GHC -fdph-par -fglasgow-exts -XUndecidableInstances #-}
{-# LANGUAGE PArr #-}
module Yaiba.SPoly where

import Data.Map
import qualified Data.Set as Set
import Data.Array.Parallel.Prelude
import Yaiba.Sugar
import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal

newtype SPoly ord = SP (Map (Sugar ord) (Set.Set (Poly ord)))

sPoly :: (Ord (Mon ord)) => (Poly ord,Sugar ord) -> 
         (Poly ord, Sugar ord) -> Maybe (Poly ord, Sugar ord)
sPoly (a,S a') (b,S b') = let (a1,a2) = leadTerm a
                              (b1,b2) = leadTerm b
                              l = lcmMon a1 b1
                              sp = (monMult (l/a1) b2 a) - (monMult (l/b1) a2 b)
                              (spLT,co) = leadTerm sp
                              spLTdeg = degree spLT
                              sug = spLTdeg + max (a'-spLTdeg) (b'-spLTdeg)
                          in case a1 * b1 == l of
                            False -> Just (sp,S sug)
                            True -> Nothing

syzygy :: (Ord (Mon ord)) =>
          Ideal ord -> (Poly ord, Sugar ord) -> [(Poly ord, Sugar ord)]
syzygy (I as) b = foldl (\x y -> f (sPoly b y) x) [] as where
  f Nothing acc = acc
  f (Just res) acc = res:acc

minimize :: [(Poly ord, Sugar ord)] -> [(Poly ord, Sugar ord)]
minimize as = as

getSPolys :: (Ord (Mon ord)) => Ideal ord -> Ideal ord -> SPoly ord
getSPolys a b = SP $ foldl (\a (v,k) -> insertWith Set.union k (Set.singleton v) a) empty (getSPolys' a b) where
  getSPolys' _ (I []) = []
  getSPolys' x@(I xs) (I (y:ys)) = syzygy x y ++ getSPolys' (I (y:xs)) (I ys)