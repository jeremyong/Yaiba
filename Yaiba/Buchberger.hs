{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
{-# LANGUAGE PArr #-}

module Yaiba.Buchberger where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Yaiba.SPoly
import Data.Map hiding (filter,map)
import Data.List hiding (null)
import Data.Set
import Prelude hiding (rem,null,map,filter)

gB :: (Ord (Mon ord)) => Ideal ord -> Ideal ord
gB a = gB' a (getSPolys (I []) a) where
  gB' d@(I ds) (SP spolys) | Data.Map.null spolys = d
                           | otherwise = let ((_,polys),rest) = Data.Map.deleteFindMin spolys
                                             redPolys = Data.Set.filter (not . isNull) $ Data.Set.map (\x -> x /. d) polys
                                             initRed = initSugars (Data.Set.toList redPolys)
                                             SP new = getSPolys d (I initRed)
                                             nextSMap = SP $ unionWith Data.Set.union rest new
                                         in gB' (I $ ds++initRed) nextSMap