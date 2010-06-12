
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances #-}

module Yaiba.Ideal where

import Yaiba.Monomial
import Yaiba.Polynomial
import Data.Map

newtype Ideal ord = Ideal (Map (Polynomial ord) (Monomial ord)) deriving (Eq)

getPolys :: Ideal t -> [Polynomial t]
getPolys (Ideal a) = keys a

getPolyMap :: Ideal t -> Map (Polynomial t) (Monomial t)
getPolyMap (Ideal a) = a

(/.) :: (Ord (Monomial ord)) =>
        Polynomial ord -> Ideal ord -> Polynomial ord
(/.) p i = (/..) p i nullPoly

(/..) :: (Ord (Monomial ord)) =>
         Polynomial ord -> Ideal ord -> Polynomial ord -> Polynomial ord
(/..) dividend d@(Ideal divisors) remainder = case isNull dividend of
  True -> remainder
  False -> let (new,divOcc) = divByIdeal dividend divisors
           in case divOcc of
             False -> let (lt,rest) = deleteFindLT dividend
                      in (/..) rest d (remainder + lt)
             True -> (/..) new d remainder
             
divByIdeal :: (Ord (Monomial ord)) =>
              Polynomial ord -> Map (Polynomial ord) (Monomial ord) -> 
              (Polynomial ord, Bool)
divByIdeal dividend divisors = foldlWithKey divByIdeal' (dividend, False) divisors where
  divByIdeal' (p,divOcc) divisor _ = let (quo,rem) = quoRem p divisor
                                     in case isNull quo of
                                       True -> (p,divOcc)
                                       False -> (rem,True)