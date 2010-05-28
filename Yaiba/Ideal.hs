
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XPArr #-}

module Yaiba.Ideal where

import Data.Array.Repa as R
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Array.Parallel.Unlifted as U
import Yaiba.Monomial
import Yaiba.Polynomial

newtype Ideal ord = Ideal (R.Array DIM1 (Polynomial ord))

printIdeal :: (Show a, Shape sh, U.Elt a) =>
     R.Array sh a -> String
printIdeal a = Prelude.show $ R.toList a