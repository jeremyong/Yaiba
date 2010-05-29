
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XPArr #-}

module Yaiba.Ideal where

--import Data.Array.Repa as R
--import Data.Array.Repa.Index
--import Data.Array.Repa.Shape
--import Data.Array.Parallel.Unlifted as U
import Yaiba.Polynomial

newtype Ideal ord = Ideal [: Polynomial ord :]

instance Show (Ideal ord) where
  show (Ideal a) = show a

--printIdeal :: (Ord (Monomial ord)) => Ideal ord -> String
--printIdeal (Ideal a) = show $ fromP a