
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}

module Yaiba.Buchberger where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Yaiba.Sugar
import Data.Map
import Control.Parallel
import Control.Parallel.Strategies
import Debug.Trace
import Prelude hiding (rem,null,map,filter)

-- Computes the Groebner basis of an ideal a
gB :: (Ord (Monomial ord)) =>
      Ideal ord -> Ideal ord
gB a = gB' a (I empty) where
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

newPolys :: (Ord (Monomial ord)) => Ideal ord -> Ideal ord -> Ideal ord
newPolys a@(I a') b@(I b') | null b' == True = I $ getSPolys a 
                           | otherwise = I $ union mixed (getSPolys b) where
                             mixed = mixSPolys a' b'

getSPolys :: (Ord (Monomial ord)) => Ideal ord -> Map (Polynomial ord)  (Sugar ord)
getSPolys (I as) | size as <= 1 = empty
                 | otherwise = fst $ mapAccumWithKey splitPair empty as where
                   splitPair acc mixMe _ = (union (mixSPolys (snd $ split mixMe as) (singleton mixMe (computeSugar mixMe 0))) acc,as)

--Takes two maps and pairs elements of the second with elements of the first.
mixSPolys :: (Ord (Monomial ord)) =>
             Map (Polynomial ord) (Sugar ord) ->
             Map (Polynomial ord) (Sugar ord) -> Map (Polynomial ord) (Sugar ord)
mixSPolys batter mix | size mix == 0 = empty 
                     | otherwise = let pair acc k v = union (foldlWithKey stitch empty batter) acc where
                                         stitch acc' k' v' = let poly = sPoly k k'
                                                                 polySugar = computeSugar poly 0
                                                             in insert poly polySugar acc'
                                   in foldlWithKey pair empty mix

--  where pair k _ acc = S.union (mapKeys (sPoly k) batter) acc

sPoly :: (Ord (Monomial ord)) =>
         Polynomial ord -> Polynomial ord -> Polynomial ord
sPoly k k' = let l = lcmMon a1 b1
                 (a1,a2) = leadTerm k
                 (b1,b2) = leadTerm k'
             in (monMult (l/a1) (b2/a2) k) - (monMult (l/b1) 1 k')
