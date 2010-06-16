{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances #-}

module Yaiba.Sugar where

import Yaiba.Monomial
import Math.Algebra.Field.Base

--Sugar tuples consist of the LT and the sugar itself
newtype Sugar ord = S Int

instance Eq (Sugar ord) where --Dummy instance, use compare instead

instance Ord (Sugar ord) where
  compare (S a) (S b) = compare a b
  
instance Show (Sugar ord) where
  show (S a) = show a