{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
{-# LANGUAGE PArr #-}

module Yaiba.Buchberger where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Yaiba.SPoly
import Data.Map hiding (filter,map)
import qualified Data.List as DL hiding (null)
import Control.Parallel
import Data.Array.Parallel
import qualified Data.Set as DS
import Prelude hiding (rem,null,map,filter)

gB :: (Ord (Mon ord)) => Ideal ord -> Ideal ord
gB a = gB' a (getSPolys (I []) a) where
  gB' d@(I ds) (SP spolys) = if null spolys then 
                               d 
                             else let ((_,polys),rest) = deleteFindMin spolys
                                      redPolys = DS.filter (not.isNull) $ 
                                                 DS.map (\x -> x /. d) polys
                                      initRed = initSugars redPolys
                                      SP new = getSPolys d (I initRed)
                                      nextSMap = SP $ unionWith DS.union rest new
                                  in initRed `par` gB' (I $ ds++initRed) nextSMap
                                   
{-
parReduce DS.empty _ _ = DS.empty
parReduce as d acc = let (max,rest) = Data.Set.deleteFindMax as
                         red = max /. d
                     in red `par` parReduce as-}