
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

gB :: (Ord (Mon ord)) => Ideal ord -> Ideal ord
gB a = gB' a (SP $ getSPolys (I []) a) where
  gB' orig@(I a') diff@(I b') = let new@(I c') = gB'' (newPolys orig diff) orig
                                    out = I $ union a' c'
                                in case null c' of
                                  False -> gB' out new
                                  True -> out

peelOnce 

gB'' :: (Ord (Mon ord)) => SPoly ord -> Ideal ord -> Ideal ord
gB'' (SP sPolys) divisors = I $ foldlWithKey reduce empty sPolys where
  reduce acc poly _ = rem `par` case isNull rem of
    True -> acc
    False -> insert rem (computeSugar rem 0) acc
    where rem = poly /. divisors


