
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}

module Yaiba.Buchberger where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Data.List
import Control.Parallel
import Prelude hiding (rem)

-- Computes the Groebner basis of an ideal a

gB :: (Ord (Monomial ord)) => Ideal ord -> Ideal ord
gB (Ideal a) = let (out,same) = gB' (map sPoly (getPairs a)) a a False
               in case same of
                 True -> gB out
                 False -> out

-- Accepts a list of polynomials and mutates it so that all
-- S pairs not zero upon reduction are appended.

gB' :: (Ord (Monomial ord)) =>
       [Polynomial ord] -> [Polynomial ord] -> [Polynomial ord] -> Bool
       -> (Ideal ord, Bool)
gB' [] _ as changed = (Ideal as, changed)
gB' (x:xs) ys as changed = rem `par` case isNull rem of
  True -> gB' xs ys as changed
  False -> gB' xs ys (x:as) True
  where rem = x /. (Ideal ys)

getPairs :: [a] -> [(a, a)]
getPairs a = convToTups $ combinations 2 a 

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

convToTups :: [[a]] -> [(a, a)]
convToTups (a:as) = (head a, last a):(convToTups as)
convToTups _ = []

sPoly :: (Ord (Monomial ord)) =>
         (Polynomial ord, Polynomial ord) -> Polynomial ord
sPoly (a,b) = let aLT = leadTerm a 
                  bLT = leadTerm b
                  (a1,a2) = aLT
                  (b1,b2) = bLT
                  l = lcmMon a1 b1
              in (monMult (l/a1) (b2/a2) a) - (monMult (l/b1) 1 b)