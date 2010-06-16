{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
{-# LANGUAGE PArr #-}

module Yaiba.Buchberger where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Yaiba.Sugar
import Yaiba.SPoly
import Data.Map hiding (null,filter,map)
import Data.List
import Data.Array.Parallel.Prelude hiding (not)
import Prelude hiding (rem,null,map,filter)

gB :: (Ord (Mon ord)) => Ideal ord -> Ideal ord
gB a = gB' a (getSPolys (I []) a) where
  gB' b (SP empty) = b
  gB' d@(I ds) (SP spolys) = let ((_,polys),rest) = deleteFindMin spolys
                                 redPolys = filter (not . isNull) $ map (\x -> x /. d) polys
                                 initRed = initSugars redPolys
                                 SP new = getSPolys d (I initRed)
                                 nextSMap = SP $ unionWith (++) spolys new
                             in gB' (I $ ds++initRed) nextSMap