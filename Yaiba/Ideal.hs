
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances #-}

module Yaiba.Ideal where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Sugar
import Data.Map
import Debug.Trace

newtype Ideal ord = I (Map (Polynomial ord) (Sugar ord)) deriving (Eq)

getPolys :: Ideal ord -> [Polynomial ord]
getPolys (I a) = keys a

getPolyMap :: Ideal ord -> Map (Polynomial ord) (Sugar ord)
getPolyMap (I a) = a

(/.) :: (Ord (Monomial ord)) =>
        Polynomial ord -> Ideal ord -> Polynomial ord
(/.) p i = (/..) p i nullPoly

(/..) :: (Ord (Monomial ord)) =>
         Polynomial ord -> Ideal ord -> Polynomial ord -> Polynomial ord
(/..) dividend d remainder = case isNull dividend of
  True -> remainder
  False -> let (new,divOcc) = divByIdeal dividend d
           in case divOcc of
             False -> let (lt,rest) = deleteFindLT dividend
                      in (/..) rest d (remainder + lt)
             True -> (/..) new d remainder
             
divByIdeal :: (Ord (Monomial ord)) =>
              Polynomial ord -> Ideal ord -> 
              (Polynomial ord, Bool)
divByIdeal dividend (I divisors) = foldrWithKey divByIdeal' (dividend, False) divisors where
  divByIdeal' divisor _ (p,divOcc) = case divOcc of
    False -> let (quo,rem) = quoRem p divisor
            in case isNull quo of
              True -> (p,divOcc)
              False -> (rem,True)
    True -> (p,divOcc)