
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances #-}

module Yaiba.Ideal where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Sugar
import Data.List
import Debug.Trace

newtype Ideal ord = I [Polynomial ord] deriving (Eq)

getPolys :: Ideal ord -> [Polynomial ord]
getPolys (I a) = a

(/.) :: (Ord (Monomial ord)) =>
        Polynomial ord -> Ideal ord -> Polynomial ord
(/.) p i = let (/..) a b r = case isNull a of
                 True -> r
                 False -> let (new,divOcc) = divByIdeal a b
                          in case divOcc of
                            False -> let (lt,rest) = deleteFindLT a
                                     in (/..) rest b (r + lt)
                            True -> (/..) new b r
           in (/..) p i nullPoly
             
divByIdeal :: (Ord (Monomial ord)) =>
              Polynomial ord -> Ideal ord -> 
              (Polynomial ord, Bool)
divByIdeal p (I ds) = foldl divByIdeal' (p, False) ds where
  divByIdeal' (p',divOcc) d = case divOcc of
    False -> let (quo,rem) = quoRem p' d
             in case isNull quo of
              True -> (p,divOcc)
              False -> (rem,True)
    True -> (p',divOcc)