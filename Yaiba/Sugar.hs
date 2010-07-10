{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances #-}
-- | Currently a placeholder structure for potentially useful
-- data for manipulating and pruning S-Pairs. Currently holds
-- the "sugar" of a polynomial.
module Yaiba.Sugar where

newtype Sugar ord = S Int

instance Eq (Sugar ord) where
    S a == S b = a == b

instance Ord (Sugar ord) where
  compare (S a) (S b) = compare a b
  
instance Show (Sugar ord) where
  show (S a) = show a