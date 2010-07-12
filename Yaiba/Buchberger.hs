{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
-- | Generates a Groebner basis of a supplied ideal.
module Yaiba.Buchberger where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Yaiba.Sugar
import Yaiba.SPoly
import Yaiba.Cluster
import qualified Data.List as DL
import Control.Parallel.Strategies
import qualified Data.Set as DS
import qualified Data.Vector as DV
import qualified Data.Map as DM
import Debug.Trace
import Prelude hiding (rem,null,map,filter)

gB :: Ord (Mon ord) => DS.Set (PolySug ord) -> Ideal ord
gB seed = let (initial,restSeed) = deleteFindMin seed
          in gB' (I $ DV.singleton initial) restSeed DM.empty where
              gB' res oneByOne spMap | DS.null oneByOne && DM.null spMap = res
                                     | DS.null oneByOne = let (lowSugPolys, higherSugPolys) = delFindLowest (SP spMap res)
                                                              numBins = DL.length lowSugPolys
                                                              redPolys = decluster (lift worker (cluster numBins lowSugPolys)
                                                                                    `using` parList rwhnf)
                                                              worker = DL.filter (\(poly,_) -> not $ isNull poly) . reducePolys res
                                                              newOneByOne = DL.foldl' (\acc x -> DS.insert (PS x) acc) oneByOne redPolys
                                                          in gB' res newOneByOne higherSugPolys
                                     | otherwise = let (gen,newGens) = deleteFindMin oneByOne
                                                       SP newspMap newres = updateSPolys (SP spMap res) gen
                                                       (lowSugPolys, higherSugPolys) = delFindLowest (SP newspMap newres)
                                                       numBins = DL.length lowSugPolys
                                                       redPolys = decluster (lift worker (cluster numBins lowSugPolys)
                                                                             `using` parList rwhnf)
                                                       worker = DL.filter (\(poly,_) -> not $ isNull poly) . reducePolys newres
                                                       newOneByOne = DL.foldl' (\acc x -> DS.insert (PS x) acc) newGens redPolys
                                                   in if isNull $ fst (gen /. res) then
                                                          gB' res newGens spMap
                                                      else ("Queue size: "++(show $ DS.size newOneByOne)) `trace`
                                                           gB' newres newOneByOne higherSugPolys



gB'' :: Ord (Mon ord) => DS.Set (PolySug ord) -> Int -> Ideal ord
gB'' seed c = let (initial,restSeed) = deleteFindMin seed
              in gB' (I $ DV.singleton initial) restSeed DM.empty c where
                gB' res _ _ 0 = res
                gB' res oneByOne spMap n | DS.null oneByOne && DM.null spMap = res
                                         | DS.null oneByOne = let (lowSugPolys, higherSugPolys) = delFindLowest (SP spMap res)
                                                                  numBins = DL.length lowSugPolys
                                                                  redPolys = decluster (lift worker (cluster numBins lowSugPolys)
                                                                                        `using` parList rwhnf)
                                                                  worker = DL.filter (\(poly,_) -> not $ isNull poly) . reducePolys res
                                                                  newOneByOne = DL.foldl' (\acc x -> DS.insert (PS x) acc) oneByOne redPolys
                                                              in gB' res newOneByOne higherSugPolys (n-1)
                                         | otherwise = let (gen,newGens) = deleteFindMin oneByOne
                                                           SP newspMap newres = updateSPolys (SP spMap res) gen
                                                           (lowSugPolys, higherSugPolys) = delFindLowest (SP newspMap newres)
                                                           numBins = DL.length lowSugPolys
                                                           redPolys = decluster (lift worker (cluster numBins lowSugPolys)
                                                                                 `using` parList rwhnf)
                                                           worker = DL.filter (\(poly,_) -> not $ isNull poly) . reducePolys newres
                                                           newOneByOne = DL.foldl' (\acc x -> DS.insert (PS x) acc) newGens redPolys
                                                       in if isNull $ fst (gen /. res) then
                                                              gB' res newGens spMap n
                                                          else ("Queue size: "++(show $ DS.size newOneByOne)) `trace`
                                                               gB' newres newOneByOne higherSugPolys (n-1)

{-
-- | Partitions each value in an SPoly map and executes in parallel if the
-- value list is sufficiently long.

gB :: (Ord (Mon ord)) => Ideal ord -> Ideal ord
gB a = gB' a (getSPolys (I []) a) where
    gB' d@(I ds) (SP sPolys) = if null sPolys then
                                   d
                               else let ((_,polys),rest) = deleteFindMin sPolys
                                        polyList = DS.toList polys
                                        numBins = DS.size polys
                                        !allPolys = decluster (lift worker2 (cluster numBins polyList) `using` parList rwhnf) where
                                            worker1 = initSugars . DL.filter (not.isNull)
                                            worker2 = worker1 . reducePolys (I ds)
                                        newDivisors = ds ++ allPolys
                                        SP newSMap = getSPolys d (I allPolys)
                                        updatedSMap = unionWith DS.union rest newSMap
                                    in gB' (I newDivisors) (SP updatedSMap)

gB'' :: (Ord (Mon ord)) => Ideal ord -> Int -> Ideal ord
gB'' a = gB' a (getSPolys (I []) a) where
  gB' d@(I ds) (SP sPolys) n = if n==0 || size sPolys ==0 then
                                   d
                               else let ((_,polys),rest) = deleteFindMin sPolys
                                        polyList = DS.toList polys
                                        numBins = DS.size polys
                                        !allPolys = decluster (lift worker2 (cluster numBins polyList) `using` parList rwhnf)
                                        worker1 = initSugars . DL.filter (not.isNull)
                                        --worker1 = initSugars
                                        worker2 = worker1 . reducePolys (I ds)
                                        newDivisors = ds ++ allPolys
                                        SP newSMap = getSPolys d (I allPolys)
                                        updatedSMap = unionWith DS.union rest newSMap
                                        --newDivisorsPruned = minDivs newDivisors
                                    --in gB' (I newDivisorsPruned) (SP updatedSMap) (n-1)
                                    in gB' (I newDivisors) (SP updatedSMap) (n-1)

minDivs :: Ord (Mon ord) => [(Poly ord, Sugar ord)] -> [(Poly ord, Sugar ord)]
minDivs as = DL.filter isMinimal as where
  isMinimal (a,_) = DL.null $ DL.filter (\(!x,_) -> a /= x && isFactor (monLT x) (monLT a)) as
-}

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