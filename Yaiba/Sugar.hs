{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances #-}

module Yaiba.Sugar where

import Yaiba.Monomial

--Sugar tuples consist of the LT and the sugar itself
newtype Sugar ord = S ((Mon ord), Int)

instance Eq (Sugar ord) where --Dummy instance, use compare instead

instance (Ord (Mon ord)) => Ord (Sugar ord) where
  compare (S (a,_)) (S (b,_)) = compare a b
  
instance (Ord (Mon ord)) => Show (Sugar ord) where
  show (S (a,_)) = show a