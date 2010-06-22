{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
-- | Generates a Groebner basis of a supplied ideal.
module Yaiba.Buchberger where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Yaiba.SPoly
import Yaiba.Map hiding (filter,map)
import Yaiba.Cluster
import qualified Data.List as DL hiding (null)
import Control.Parallel.Strategies
import qualified Data.Set as DS
import GHC.Conc (numCapabilities)
import Prelude hiding (rem,null,map,filter)

reducePolys :: (Ord (Mon ord)) => Ideal ord -> [Poly ord] -> [Poly ord]
reducePolys d = DL.map (/. d)
--reducePolys _ []     = []
--reducePolys d fs = DL.foldl' k [] fs where
--                     k !gs !f = let !h = f /. d
--                                in if isNull h then gs else h:gs

-- | Partitions each value in an SPoly map and executes in parallel if the
-- value list is sufficiently long.

gB :: (Ord (Mon ord)) => Ideal ord -> Ideal ord
gB a = gB' a (getSPolys (I []) a) where
    gB' d@(I ds) (SP sPolys) = let ((_,polys),rest) = deleteFindMin sPolys
                                   polyList = DS.toList polys
                                   --numBins = ceiling $
                                   --          (fromIntegral (DS.size polys) :: Float)
                                   --          / (fromIntegral numCapabilities :: Float)
                                   --numBins = 4*numCapabilities
                                   numBins = DS.size polys
                                   allPolys = decluster (lift worker2 (cluster numBins polyList) `using` parList rwhnf) where
                                       worker1 = initSugars . DL.filter (not.isNull)
                                       --worker1 = initSugars
                                       worker2 = worker1 . reducePolys (I ds)
                                   newDivisors = ds ++ allPolys
                                   SP newSMap = getSPolys d (I allPolys)
                                   updatedSMap = unionWith DS.union rest newSMap
                               in if null sPolys then
                                      d
                                  else
                                      gB' (I newDivisors) (SP updatedSMap)

gB'' :: (Ord (Mon ord)) => Ideal ord -> Int -> Ideal ord
gB'' a = gB' a (getSPolys (I []) a) where
  gB' d@(I ds) (SP sPolys) n = let ((_,polys),rest) = deleteFindMin sPolys
                                   polyList = DS.toList polys
                                   --numBins = ceiling $
                                   --          (fromIntegral (DS.size polys) :: Float)
                                   --          / (fromIntegral numCapabilities :: Float)
                                   --numBins = 4*numCapabilities
                                   numBins = DS.size polys
                                   allPolys = DL.concat (lift worker2 (cluster numBins polyList) `using` parList rwhnf) where
                                                  worker1 = initSugars . DL.filter (not.isNull)
                                                  --worker1 = initSugars
                                                  worker2 = worker1 . reducePolys (I ds)
                                   newDivisors = ds ++ allPolys
                                   SP newSMap = getSPolys d (I allPolys)
                                   updatedSMap = unionWith DS.union rest newSMap
                               in if n == 0 || null sPolys then
                                      d
                                  else 
                                      gB' (I newDivisors) (SP updatedSMap) (n-1)
{-
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
                                  in (show $ DS.size polys) `trace` gB' (I $ ds++initRed) nextSMap-}
{-
gB'' :: (Ord (Mon ord)) => Ideal ord -> Int -> Ideal ord
gB'' a = gB' a [] (getSPolys (I []) a) where
  gB' d@(I ds) new (SP sPolys) n = if n==0 then d else
                                     let new' = initSugars $ DL.filter (not.isNull) new
                                         newDivisors = ds ++ new'
                                         SP newSMap = getSPolys d (I new')
                                         updatedSMap = unionWith DS.union sPolys newSMap
                                         ((_,polys),rest) = deleteFindMin updatedSMap
                                         polyList = DS.toList polys
                                         binSize = ceiling $ 
                                                   (fromIntegral (DS.size polys) :: Float)
                                                   / (fromIntegral numCapabilities :: Float)
                                         allPolys = reducePolys (I newDivisors) polyList
                                                    `using` parListChunk binSize rwhnf
                                     in gB' (I newDivisors) allPolys (SP rest) (n-1)-}