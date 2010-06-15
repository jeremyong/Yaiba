{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances #-}

module Yaiba.Sugar where

import Yaiba.Monomial
import Yaiba.Polynomial

--The last parameter is the "index" used to render keys
--unique. Should be set to the size of the s-Poly map.
newtype Sugar ord = S ((Monomial ord), Int)

instance Eq (Sugar ord) where --Dummy instance, use compare instead

instance (Ord (Monomial ord)) => Ord (Sugar ord) where
  compare (S (a,_)) (S (b,_)) = compare a b
  
instance (Ord (Monomial ord)) => Show (Sugar ord) where
  show (S (a,_)) = show a
  
computeSugar a b = S (fst $ leadTerm a, b)

shiftIndex (S (a,b)) n = S (a,b+n)