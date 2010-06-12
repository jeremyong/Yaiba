
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}

module Yaiba.Buchberger where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Yaiba.Sugar
import Data.Map
import Control.Parallel
import Debug.Trace
import Prelude hiding (rem,
                       null)

-- Computes the Groebner basis of an ideal a
gB :: (Ord (Monomial ord)) =>
      Ideal ord -> Ideal ord
gB (Ideal a) = Ideal $ gB' a empty

gB' :: (Ord (Monomial ord)) =>
       Map (Polynomial ord) (Monomial ord) ->
       Map (Polynomial ord) (Monomial ord) ->
       Map (Polynomial ord) (Monomial ord)
gB' orig diff = let (new,changed) = gB'' (newPolys orig diff) orig
                    out = union orig new --Hedge union algorithm is more efficient
                                           --on union (bigger) (smaller)
                in case changed of
                  True -> gB' out new --Repeat this until gB does nothing
                  False -> out

-- Accepts a set of polynomials and mutates it so that all
-- S pairs not zero upon reduction are appended.

gB'' :: (Ord (Monomial ord)) =>
        Map (Sugar ord) (Polynomial ord) ->
        Map (Polynomial ord) (Monomial ord) ->
        (Map (Polynomial ord) (Monomial ord), Bool)
gB'' sPolyMap oldMap = fold reduce (empty,False) sPolyMap
  where reduce dividend (newMap,changed) = rem `par` case isNull rem of
          True -> (newMap,changed)
          False -> (insert rem (fst $ leadTerm rem) newMap,True)
          where rem =  dividend /. (Ideal oldMap)
                
newPolys :: (Ord (Monomial ord)) =>
            Map (Polynomial ord) (Monomial ord) ->
            Map (Polynomial ord) (Monomial ord) ->
            Map (Sugar ord) (Polynomial ord)
newPolys a b | null b == True = getSPolys a 
             | otherwise = union mixed (mapKeys (\k -> shiftIndex k (size mixed)) $ getSPolys b) where
  mixed = mixSPolys a b

getSPolys :: (Ord (Monomial ord)) =>
             Map (Polynomial ord) (Monomial ord) ->
             Map (Sugar ord) (Polynomial ord)
getSPolys as | size as <= 1 = empty
             | otherwise = fst $ mapAccumWithKey splitPair empty as where
               splitPair acc mixMe _ = (union (mixSPolys (snd $ split mixMe as) (singleton mixMe (fst $ leadTerm mixMe))) acc,as)

--Takes two maps and pairs elements of the second with elements of the first.
mixSPolys :: (Ord (Monomial ord)) =>
             Map (Polynomial ord) (Monomial ord) ->
             Map (Polynomial ord) (Monomial ord) ->
             Map (Sugar ord) (Polynomial ord)
mixSPolys batter mix | size mix == 0 = empty 
                     | otherwise = let pair acc k _ = union (foldlWithKey stitch empty batter) acc where
                                         stitch acc' k' _ = insert polySugar poly acc' where
                                           poly = sPoly k k'
                                           polySugar = computeSugar poly (size acc)
                                   in foldlWithKey pair empty mix

--  where pair k _ acc = S.union (mapKeys (sPoly k) batter) acc

sPoly :: (Ord (Monomial ord)) =>
         Polynomial ord -> Polynomial ord -> Polynomial ord
sPoly k k' = let l = lcmMon a1 b1
                 (a1,a2) = leadTerm k
                 (b1,b2) = leadTerm k'
             in (monMult (l/a1) (b2/a2) k) - (monMult (l/b1) 1 k')
                
-- Computes the Groebner basis of an ideal a
nPgB :: (Ord (Monomial ord)) =>
        Ideal ord -> Ideal ord
nPgB (Ideal a) = Ideal $ nPgB' a empty

nPgB' :: (Ord (Monomial ord)) =>
         Map (Polynomial ord) (Monomial ord) ->
         Map (Polynomial ord) (Monomial ord) ->
         Map (Polynomial ord) (Monomial ord)
nPgB' orig diff = let (new,changed) = nPgB'' (newPolys orig diff) orig
                      out = union orig new --Hedge union algorithm is more efficient
                                           --on union (bigger) (smaller)
                  in case changed of
                    True -> nPgB' out new --Repeat this until gB does nothing
                    False -> out

-- Accepts a set of polynomials and mutates it so that all
-- S pairs not zero upon reduction are appended.

nPgB'' :: (Ord (Monomial ord)) =>
        Map (Sugar ord) (Polynomial ord) ->
        Map (Polynomial ord) (Monomial ord) ->
        (Map (Polynomial ord) (Monomial ord), Bool)
nPgB'' sPolyMap oldMap = fold reduce (empty,False) sPolyMap
  where reduce dividend (newMap,changed) = rem `seq` case isNull rem of
          True -> (newMap,changed)
          False -> (insert rem (fst $ leadTerm rem) newMap,True)
          where rem =  dividend /. (Ideal oldMap)
