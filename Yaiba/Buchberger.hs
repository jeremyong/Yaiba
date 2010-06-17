{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
-- | Generates a Groebner basis of a supplied ideal.
module Yaiba.Buchberger where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Yaiba.SPoly
import Data.Map hiding (filter,map)
import qualified Data.List as DL hiding (null)
import Control.Parallel
import qualified Data.Set as DS
import Prelude hiding (rem,null,map,filter)
{-
-- | Parallelizes to depth 2 in the SPoly map.
gB :: (Ord (Mon ord)) => Ideal ord -> Ideal ord
gB a = gB' a (getSPolys (I []) a) where
  gB' d@(I ds) (SP spolys) = if null spolys then 
                               d 
                             else let ((_,polys),rest) = deleteFindMin spolys
                                      ((_,polys'),rest') = deleteFindMin rest
                                      redPolys = initSugars $ DS.filter (not.isNull) $ 
                                                 DS.map (/. d) polys
                                      redPolys' = initSugars $ DS.filter (not.isNull) $ 
                                                  DS.map (/. d) polys'
                                      initRed = if null rest then
                                                  redPolys
                                                else
                                                  redPolys `par` (redPolys' `pseq` redPolys ++ redPolys')
                                      SP new = getSPolys d (I $ initRed)
                                      nextSMap = if null rest then
                                                   SP $ unionWith DS.union rest new
                                                 else
                                                   SP $ unionWith DS.union rest' new
                                  in gB' (I $ ds++initRed) nextSMap
-}
-- | Partitions each value in an SPoly map and executes in parallel if the
-- value list is sufficiently long.
gB :: (Ord (Mon ord)) => Ideal ord -> Ideal ord
gB a = gB' a (getSPolys (I []) a) where
  gB' d@(I ds) (SP spolys) = if null spolys then 
                               d 
                             else let ((_,polys),rest) = deleteFindMin spolys
                                      M doubPivot = monLT (DS.findMax polys)
                                      pivot = P $ singleton (M $ DL.map (`quot` 2) doubPivot) 1
                                      (top,bot) = let (t,occ,b) = DS.splitMember pivot polys
                                                  in if occ then
                                                       (DS.insert pivot t, b)
                                                     else (t, b)
                                      topPolys = initSugars $ DS.filter (not.isNull) 
                                                 $ DS.map (/. d) top
                                      botPolys = initSugars $ DS.filter (not.isNull) 
                                                 $ DS.map (/. d) bot
                                      allPolys = initSugars $ DS.filter (not.isNull) 
                                                 $ DS.map (/. d) polys
                                      initRed = if length allPolys > 1 then
                                                  topPolys `par` 
                                                  (botPolys `pseq` topPolys++botPolys)
                                                else
                                                  allPolys
                                      SP new = getSPolys d (I $ initRed)
                                      nextSMap = SP $ unionWith DS.union rest new
                                  in gB' (I $ ds ++ initRed) nextSMap

-- | Non-parallelized implementation.
nPgB :: (Ord (Mon ord)) => Ideal ord -> Ideal ord
nPgB a = gB' a (getSPolys (I []) a) where
  gB' d@(I ds) (SP spolys) = if null spolys then 
                               d 
                             else let ((_,polys),rest) = deleteFindMin spolys
                                      redPolys = DS.filter (not.isNull) $ 
                                                 DS.map (/. d) polys
                                      initRed = initSugars redPolys
                                      SP new = getSPolys d (I initRed)
                                      nextSMap = SP $ unionWith DS.union rest new
                                  in gB' (I $ ds++initRed) nextSMap