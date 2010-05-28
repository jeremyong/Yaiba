
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XPArr #-}

module Yaiba.Ideal where

import Data.Array.Repa as R
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Yaiba.Monomial
import Yaiba.Polynomial

newtype Ideal ord = Ideal (R.Array DIM1 (Polynomial ord))

printIdeal :: (Show a, Shape sh,
      dph-prim-par-0.4.0:Data.Array.Parallel.Unlifted.Elt a) =>
     Array sh a -> String
printIdeal a = Prelude.show $ R.toList a