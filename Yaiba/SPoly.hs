{-# OPTIONS_GHC -fdph-par #-}
{-# LANGUAGE PArr #-}
module Yaiba.SPoly where

import Data.Map
--import Data.Array.Parallel.Prelude
import Yaiba.Sugar
import Yaiba.Polynomial

newtype SPoly ord = SP (Map (Sugar ord) [:Poly ord:])

sPoly (a,a') (b,b') = let (a1,a2) = leadTerm a
                          (b1,b2) = leadTerm b
                          l = lcmMon a1 b1
                      in (monMult (l/a1) (b2/a2) a) - (monMult (l/b1) 1 b)

syzygy [] _ = SP empty
syzygy as b = map (\x -> 