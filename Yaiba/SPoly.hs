{-# OPTIONS_GHC -fdph-par -fglasgow-exts -XUndecidableInstances #-}
{-# LANGUAGE PArr #-}
module Yaiba.SPoly where

import qualified Data.Map as DM
--import Data.Array.Parallel.Prelude
import Yaiba.Sugar
import Yaiba.Monomial
import Yaiba.Polynomial

newtype SPoly ord = SP (DM.Map (Sugar ord) [:Poly ord:])

sPoly :: (Ord (Mon ord)) => (Poly ord,Sugar ord) -> 
         (Poly ord, Sugar ord) -> (Poly ord, Sugar ord)
sPoly (a,S (a1,a2,a3)) (b,S (b1,b2,b3)) = let l = lcmMon a1 b1
                                              sp = (monMult (l/a1) (b2/a2) a) - (monMult (l/b1) 1 b)
                                              (spLT,co) = leadTerm sp
                                              spLTdeg = degree spLT
                                              sug = spLTdeg + max (a3-spLTdeg) (b3-spLTdeg)
                                          in (sp,S (spLT,co,sug))

syzygy as b = map (sPoly b) as