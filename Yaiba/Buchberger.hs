{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
-- | Generates a Groebner basis of a supplied ideal.
module Yaiba.Buchberger where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Yaiba.Sugar
import Yaiba.SPoly
import Data.Map hiding (filter,map)
import qualified Data.List as DL hiding (null)
import Control.Parallel
import Control.Parallel.Strategies
import qualified Data.Set as DS
import Debug.Trace
import GHC.Conc (numCapabilities)
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

--divideToConquer :: (Ord (Mon ord)) => DS.Set (Poly ord) -> [DS.Set (Poly ord)]
--divideToConquer = DS.fold (\p (a:as) -> as++[DS.insert p a]) (DL.replicate numCapabilities (DS.empty))
{-
divideToConquer :: (Ord (Mon ord)) => [Poly ord] -> Int -> [[Poly ord]]
divideToConquer polys 1 = [polys]
divideToConquer polys numBins = let l = length polys
                                    binSize = quot l numBins
                                    (chunk,rest) = DL.splitAt binSize polys
                                in  chunk:(divideToConquer rest (numBins-1))
-}
reducePolys :: (Ord (Mon ord)) => Ideal ord -> [Poly ord] -> [Poly ord]
reducePolys d = DL.map (/. d)

-- | Partitions each value in an SPoly map and executes in parallel if the
-- value list is sufficiently long.
{-
gB :: (Ord (Mon ord)) => Ideal ord -> Ideal ord
gB a = gB' a [] (getSPolys (I []) a) where
  gB' d@(I ds) newStuff (SP sPolys) = let newDivisors = ds ++ newStuff
                                          SP newSMap = getSPolys d (I newStuff)
                                          updatedSMap = unionWith DS.union sPolys newSMap
                                      in if null updatedSMap then
                                           I $ ds ++ newStuff
                                         else let ((_,polys),rest) = deleteFindMin updatedSMap
                                                  binSize = ceiling $ 
                                                            (fromIntegral (DS.size polys) :: Float)
                                                            / (fromIntegral numCapabilities :: Float)
                                                  allPolys = reducePolys (I newDivisors) polys
                                              in gB' (I newDivisors) (allPolys `using` parListChunk binSize rwhnf) (SP rest)-}

gB'' :: (Ord (Mon ord)) => Ideal ord -> Int -> Ideal ord
gB'' a = gB' a [] (getSPolys (I []) a) where
  gB' d@(I ds) new (SP sPolys) n = let new' = initSugars $ DL.filter (not.isNull) new
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
                                   in if n == 0 then
                                        I newDivisors
                                      else 
                                        gB' (I newDivisors) allPolys (SP rest) (n-1)
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
                                        (top,bot,_) = DS.fold (\p (x,y,z) -> if z `mod` 2 == 0 then (DS.insert p x,y,z+1) else (x,DS.insert p y,z+1)) (DS.empty,DS.empty,0::Int) polys
                                        topPolys = initSugars $ DS.filter (not.isNull)
                                                   $ DS.map (/. d) top
                                        botPolys = initSugars $ DS.filter (not.isNull) 
                                                   $ DS.map (/. d) bot
                                        allPolys = initSugars $ DS.filter (not.isNull) 
                                                   $ DS.map (/. d) polys
                                        initRed = if DS.size polys > 10 then
                                                     topPolys++botPolys
                                                   else
                                                     allPolys
                                        -}
