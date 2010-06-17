{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances #-}
-- | Currently a placeholder structure for potentially useful
-- data for manipulating and pruning S-Pairs. Currently holds
-- the "sugar" of a polynomial.
module Yaiba.Sugar where

import Yaiba.Monomial
import Math.Algebra.Field.Base

newtype Sugar ord = S Int

instance Eq (Sugar ord) where

instance Ord (Sugar ord) where
  compare (S a) (S b) = compare a b
  
instance Show (Sugar ord) where
  show (S a) = show a