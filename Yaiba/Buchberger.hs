{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
-- | Generates a Groebner basis of a supplied ideal.
module Yaiba.Buchberger where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Yaiba.Sugar
import Yaiba.SPoly
import Yaiba.Map hiding (filter,map)
import qualified Data.List as DL hiding (null)
import Control.Parallel.Strategies
import qualified Data.Set as DS
import GHC.Conc (numCapabilities)
import Prelude hiding (rem,null,map,filter)
<<<<<<< HEAD

class Cluster c where
  singleton :: c a -> c (c a)
  cluster :: Int -> c a -> c (c a)
  decluster :: c (c a) -> c a
  lift :: (c a -> b) -> (c (c a) -> c b)

instance Cluster [] where
<<<<<<< HEAD
  singleton list = [list]
  cluster _ [] = []
  cluster n list = elems $ fst $ DL.foldl' f (empty,0) list where
    f (acc,z) a = (insertWith (\v vs -> v ++ vs) (z `mod` n) [a] acc, z+1)
  decluster = DL.concat
  lift = DL.map 
=======
  singleton list       = [list]
  cluster   _ []       = []
  cluster   n list     = elems $ fst $ DL.foldl' f (empty,0) list where
                             f = (\(!acc,!z) !a -> (insertWith (\v vs -> v ++ vs) (z `mod` n) [a] acc, z+1))
  decluster            = concat
  lift                 = DL.map 
>>>>>>> 06765081f6a79ffd7eff59e9ed3f2cce88d0f733

<<<<<<< .merge_file_J76uxg
=======
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
=======
>>>>>>> 84616b6a7d42d40d4021be969ec904cc8b6984ed

class Cluster c where
  singleton :: c a -> c (c a)
  cluster :: Int -> c a -> c (c a)
  decluster :: c (c a) -> c a
  lift :: (c a -> b) -> (c (c a) -> c b)

instance Cluster [] where
  singleton list = [list]
  cluster _ [] = []
  cluster n list = elems $ fst $ foldl f (empty,0) list where
    f (acc,z) a = (insertWith (\v vs -> v ++ vs) (z `mod` n) [a] acc, z+1)
  decluster = DL.concat
  lift = DL.map 

>>>>>>> .merge_file_RwOg5f
reducePolys :: (Ord (Mon ord)) => Ideal ord -> [Poly ord] -> [Poly ord]
reducePolys d = DL.map (/. d)

-- | Partitions each value in an SPoly map and executes in parallel if the
-- value list is sufficiently long.

gB :: (Ord (Mon ord)) => Ideal ord -> Ideal ord
gB a = gB' a (getSPolys (I []) a) where
<<<<<<< HEAD
    gB' d@(I ds) (SP sPolys) = let ((_,polys),rest) = deleteFindMin sPolys
                                   polyList = DS.toList polys
                                   binSize = ceiling $ 
                                             (fromIntegral (DS.size polys) :: Float)
                                             / (fromIntegral numCapabilities :: Float)
                                   allPolys = decluster ((lift worker2) (cluster binSize polyList) `using` parList rwhnf) where
                                       worker1 = (\x -> initSugars $ DL.filter (not.isNull) x)
                                       worker2 = worker1 . reducePolys (I ds)
                                   newDivisors = ds ++ allPolys
                                   SP newSMap = getSPolys d (I allPolys)
                                   updatedSMap = unionWith DS.union rest newSMap
                               in if null sPolys then
                                      d
                                  else 
=======
      gB' d@(I ds) (SP sPolys) = let ((_,polys),rest) = deleteFindMin sPolys
                                     polyList = DS.toList polys
                                     binSize = ceiling $
                                               (fromIntegral (DS.size polys) :: Float)
                                               / (fromIntegral numCapabilities :: Float)
                                     allPolys = decluster ((lift worker2) (cluster binSize polyList) `using` parList rwhnf) where
                                       worker1 = (\x -> initSugars $ DL.filter (not.isNull) x)
                                       worker2 = worker1 . reducePolys (I ds)
                                     newDivisors = ds ++ allPolys
                                     SP newSMap = getSPolys d (I allPolys)
                                     updatedSMap = unionWith DS.union rest newSMap
                                 in if null sPolys then
                                      d
                                    else
>>>>>>> 84616b6a7d42d40d4021be969ec904cc8b6984ed
                                      gB' (I newDivisors) (SP updatedSMap)

gB'' :: (Ord (Mon ord)) => Ideal ord -> Int -> Ideal ord
gB'' a = gB' a (getSPolys (I []) a) where
  gB' d@(I ds) (SP sPolys) n = let ((_,polys),rest) = deleteFindMin sPolys
                                   polyList = DS.toList polys
<<<<<<< HEAD
                                   binSize = ceiling $ 
=======
                                   binSize = ceiling $
>>>>>>> 84616b6a7d42d40d4021be969ec904cc8b6984ed
                                             (fromIntegral (DS.size polys) :: Float)
                                             / (fromIntegral numCapabilities :: Float)
                                   allPolys = DL.concat ((lift worker2) (cluster binSize polyList) `using` parList rwhnf) where
                                                  worker1 = (\x -> initSugars $ DL.filter (not.isNull) x)
                                                  worker2 = worker1 . reducePolys (I ds)
                                   newDivisors = ds ++ allPolys
                                   SP newSMap = getSPolys d (I allPolys)
                                   updatedSMap = unionWith DS.union rest newSMap
                               in if n == 0 || null sPolys then
                                      d
<<<<<<< HEAD
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
<<<<<<< .merge_file_J76uxg
=======
=======
                                  else
                                      gB' (I newDivisors) (SP updatedSMap) (n-1)
>>>>>>> 84616b6a7d42d40d4021be969ec904cc8b6984ed

>>>>>>> .merge_file_RwOg5f
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