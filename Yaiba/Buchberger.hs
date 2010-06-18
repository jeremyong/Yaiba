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
  gB' d@(I ds) new (SP sPolys) n = if n==0 then I newDivisors else
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
                                     in gB' (I newDivisors) allPolys (SP rest) (n-1)