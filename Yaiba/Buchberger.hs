
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}

module Yaiba.Buchberger where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Yaiba.Sugar
import Yaiba.SPoly
import Data.Map
import Control.Parallel
import Control.Parallel.Strategies
import Debug.Trace
import Prelude hiding (rem,null,map,filter)

-- Computes the Groebner basis of an ideal a
gB :: (Ord (Monomial ord)) =>
      Ideal ord -> Ideal ord
gB a = gB' a (getSPolys (I []) a) where
  gB' orig@(I a') diff@(I b') = let new@(I c') = gB'' (newPolys orig diff) orig
                                    out = I $ union a' c'
                                in case null c' of
                                  False -> gB' out new
                                  True -> out

-- Accepts a set of polynomials and mutates it so that all
-- S pairs not zero upon reduction are appended.
gB'' :: (Ord (Monomial ord)) => Ideal ord -> Ideal ord -> Ideal ord
gB'' (I sPolys) divisors = I $ foldlWithKey reduce empty sPolys where
  reduce acc poly _ = rem `par` case isNull rem of
    True -> acc
    False -> insert rem (computeSugar rem 0) acc
    where rem = poly /. divisors


