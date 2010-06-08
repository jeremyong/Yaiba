
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XPArr #-}

module Yaiba.Ideal where

--import Data.Array.Repa as R
--import Data.Array.Repa.Index
--import Data.Array.Repa.Shape
--import Data.Array.Parallel.Unlifted as U
import Yaiba.Monomial
import Yaiba.Polynomial
import Data.List
import Prelude hiding (rem)

newtype Ideal ord = Ideal [Polynomial ord] deriving (Eq)

{-
getPairs (Ideal as) = comb 2 as where
  comb 0 _ = [[]]
  comb n xs = [ y:ys | y:xs' <- tails xs, ys <- getPairs (n-1) xs' ]
  -}

--Division algorithm (outputs remainder)

(/.) :: (Ord (Monomial ord)) =>
        Polynomial ord -> Ideal ord -> Polynomial ord
(/.) p q = (/..) p q nullPoly

(/..) dend (Ideal ds) rem | numTerms dend == 0 = rem
                          | otherwise = let (a,b) = divIdeal dend ds 
                                        in case b of
                                          False -> let (lt,rest) = deleteFindLT dend
                                                   in (/..) rest (Ideal ds) (rem + lt)
                                          True -> (/..) a (Ideal ds) rem
 

--Takes a dividend and a list of polynomials and divides until the lead term
--of the remainder is not divisible by any of the divisors.
--Outputs a tuple that gives the pseudo-eremainder and whether or not a
--division occurred.
divIdeal :: (Ord (Monomial ord)) =>
            Polynomial ord -> [Polynomial ord] -> (Polynomial ord, Bool)
divIdeal d ds = foldl' divIdeal' (d,False) ds where
  divIdeal' (b,divOcc) a = let !(x,y) = quoRem b a 
                           in if numTerms x == 0 then 
                                (b,divOcc)
                              else (y,True)