{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances #-}

module Yaiba.Sugar where

import Yaiba.Monomial
import Yaiba.Polynomial

--The last parameter is the "index" used to render keys
--unique. Should be set to the size of the s-Poly map.
newtype Sugar ord = Sugar ((Monomial ord), Int)

instance Eq (Sugar ord) where --Dummy instance, use compare instead

instance (Ord (Monomial ord)) => Ord (Sugar ord) where
  compare (Sugar (a,_)) (Sugar (b,_)) = compare a b
  
instance (Ord (Monomial ord)) => Show (Sugar ord) where
  show (Sugar (a,_)) = show a
  
computeSugar a b = Sugar (fst $ leadTerm a, b)

shiftIndex (Sugar (a,b)) n = Sugar (a,b+n)