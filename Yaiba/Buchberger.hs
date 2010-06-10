
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}

module Yaiba.Buchberger where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Data.Map
import Control.Parallel
import Prelude hiding (rem)

-- Computes the Groebner basis of an ideal a
gB (Ideal a) = Ideal $ gB' a empty
gB' orig diff = let (new,changed) = gB'' (newPolys orig diff) orig
                    out = union old new --Hedge union algorithm is more efficient
                                        --on union (bigger) (smaller)
                in case changed of
                  True -> gB' out new --Repeat this until gB does nothing
                  False -> out

-- Accepts a list of polynomials and mutates it so that all
-- S pairs not zero upon reduction are appended.
gB'' sPolyMap oldMap = foldlWithKey reduce (empty,False) sPolyMap
  where reduce (newMap,changed) divisor lt = rem `par` case isNull rem of
          True -> (newMap,changed)
          False -> (insert rem (leadTerm rem) newMap,True)
          where rem = x /. (Ideal oldMap)
                
newPolys a empty = getSPolys a
newPolys a b = union (getSPolys a b) (getSPolys b b)

getSPolys batter mix = foldlWithKey pair empty mix
  where pair acc k lt = union (mapWithKeys (sPoly k lt) batter) acc

sPoly k lt

sPoly (a,b) = let aLT = leadTerm a 
                  bLT = leadTerm b
                  (a1,a2) = aLT
                  (b1,b2) = bLT
                  l = lcmMon a1 b1
              in (monMult (l/a1) (b2/a2) a) - (monMult (l/b1) 1 b)