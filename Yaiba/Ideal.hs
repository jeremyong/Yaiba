
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XPArr #-}

module Yaiba.Ideal where

--import Data.Array.Repa as R
--import Data.Array.Repa.Index
--import Data.Array.Repa.Shape
--import Data.Array.Parallel.Unlifted as U
import Yaiba.Monomial
import Yaiba.Polynomial
import Data.List

newtype Ideal ord = Ideal [Polynomial ord] deriving (Eq)
{-
getPairs (Ideal as) = comb 2 as where
  comb 0 _ = [[]]
  comb n xs = [ y:ys | y:xs' <- tails xs, ys <- getPairs (n-1) xs' ]
  -}

--Division algorithm (outputs remainder)

(/.) r (Ideal []) = r
(/.) d (Ideal ds) = let (a,b) = divIdeal d ds in
  case b of
    False -> a
    True -> (/.) a (Ideal ds)


divIdeal :: (Ord (Monomial ord)) =>
            Polynomial ord -> [Polynomial ord] -> (Polynomial ord, Bool)
divIdeal d ds = foldl' divIdeal' (d,False) ds where
  divIdeal' (b,divOcc) a = let !(x,y) = quoRem b a 
                           in if numTerms x == 0 then 
                                (b,divOcc)
                              else (y,True)
                                   
{-
divIdeal :: (Ord (Monomial ord)) =>
            Polynomial ord -> [Polynomial ord] -> Polynomial ord
divIdeal r [] = r
divIdeal q (d:ds) = let !(x,y) = quoRem q d 
                    in if numTerms x == 0 then
                         divIdeal q ds
                       else divIdeal y ds
-}