{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
-- | Generates a Groebner basis of a supplied ideal.
module Yaiba.Buchberger where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Yaiba.SPoly
import Yaiba.Cluster
import qualified Data.List as DL
import Control.Parallel.Strategies
import qualified Data.Set as DS
import qualified Data.Vector as DV
import qualified Data.Map as DM
import Prelude hiding (rem,null,map,filter)
import Debug.Trace

-- | This implementation of gB does not insert the supplied generators first. Instead, generators are
-- inserted by sugar degree.
modgB :: Ord (Mon ord) => DS.Set (PolySug ord) -> Ideal ord
modgB seed = let (initial,restSeed) = deleteFindMin seed
             in gB' (I $ DV.singleton initial) restSeed DM.empty where
                 gB' res oneByOne spMap | DS.null oneByOne && DM.null spMap = res
                                        | DS.null oneByOne = let (lowSugPolys, higherSugPolys) = delFindLowest (SP spMap res)
                                                                 !redPolys' = worker $ parMap rdeepseq (/. res) lowSugPolys
                                                                 !worker = DL.filter (\(poly,_) -> not $ isNull poly)
                                                                 !redPolys = parMap rdeepseq makeMonic redPolys'
                                                                 newOneByOne = DL.foldl' (\acc x -> DS.insert (PS x) acc) oneByOne redPolys
                                                             in gB' res newOneByOne higherSugPolys
                                        | otherwise = let (gen,newGens) = deleteFindMin oneByOne
                                                          reducedGen = makeMonic $ gen /. res
                                                          SP newspMap newres = updateSPolys (SP spMap res) reducedGen
                                                          (lowSugPolys, higherSugPolys) = delFindLowest (SP newspMap newres)
                                                          !redPolys' = worker $ parMap rdeepseq (/. res) lowSugPolys
                                                          !worker = DL.filter (\(poly,_) -> not $ isNull poly)
                                                          !redPolys = parMap rdeepseq makeMonic redPolys'
                                                          newOneByOne = DL.foldl' (\acc x -> DS.insert (PS x) acc) newGens redPolys
                                                      in if isNull $ fst reducedGen then
                                                             gB' res newGens spMap
                                                         else gB' newres newOneByOne higherSugPolys

-- | Strategy accurate implementation of gB
accgB :: Ord (Mon ord) => DS.Set (PolySug ord) -> Ideal ord
accgB seed = let (initial,restSeed) = deleteFindMin seed
             in gB' (I $ DV.singleton initial) restSeed DM.empty where
                 gB' res oneByOne spMap | DS.null oneByOne && DM.null spMap = res
                                        | DS.null oneByOne = let (lowSugPoly, higherSugPolys) = delFindSingleLowest (SP spMap res)
                                                                 !redPoly = (show res ++ "\n" ++ DM.showTree spMap) `trace`
                                                                            makeMonic $ lowSugPoly /. res
                                                                 newOneByOne = if not $ isNull $ fst redPoly then
                                                                                   DS.insert (PS redPoly) oneByOne
                                                                               else
                                                                                   oneByOne
                                                             in gB' res newOneByOne higherSugPolys
                                        | otherwise = let (gen,newGens) = deleteFindMin oneByOne
                                                          reducedGen = makeMonic $ gen /. res
                                                          SP newspMap newres = updateSPolys (SP spMap res) reducedGen
                                                          (lowSugPoly, higherSugPolys) = delFindSingleLowest (SP newspMap newres)
                                                          !redPoly = (show res ++ "\n" ++"Adding: " ++ show reducedGen++"\n"++ DM.showTree newspMap) `trace`
                                                                     makeMonic $ lowSugPoly /. res
                                                          newOneByOne = if not $ isNull $ fst redPoly then
                                                                            DS.insert (PS redPoly) newGens
                                                                        else
                                                                            newGens
                                                      in if isNull $ fst reducedGen then
                                                             gB' res newGens spMap
                                                         else gB' newres newOneByOne higherSugPolys


gB :: Ord (Mon ord) => DS.Set (PolySug ord) -> Ideal ord
gB seed = let (initial,restSeed) = deleteFindMin seed
          in gB1 (I $ DV.singleton initial) restSeed DM.empty DS.empty where
              gB1 res tierOne spMap tierTwo | DS.null tierOne = gB2 res tierTwo spMap
                                            | otherwise = let (gen,newGens) = deleteFindMin tierOne
                                                              redGen = makeMonic $ gen /. res
                                                              SP newSpMap newRes = updateSPolys (SP spMap res) redGen
                                                              (lowSugPolys, highSugPolys) = delFindLowest (SP newSpMap newRes)
                                                              numBins = DL.length lowSugPolys
                                                              redPolys = decluster (lift worker (cluster numBins lowSugPolys) `using` parList rwhnf)
                                                              worker = DL.map makeMonic . DL.filter (\(poly,_) -> not $ isNull poly) . reducePolys newRes
                                                              newTierTwo = DL.foldl' (\acc x -> DS.insert (PS x) acc) tierTwo redPolys
                                                          in if isNull $ fst redGen then
                                                                 gB1 res newGens spMap tierTwo
                                                             else
                                                                 gB1 newRes newGens highSugPolys newTierTwo
              gB2 res tierTwo spMap | DS.null tierTwo && DM.null spMap = res
                                    | DS.null tierTwo = let (lowSugPolys, highSugPolys) = delFindLowest (SP spMap res)
                                                            numBins = DL.length lowSugPolys
                                                            redPolys = decluster (lift worker (cluster numBins lowSugPolys)
                                                                                  `using` parList rwhnf)
                                                            worker = DL.map makeMonic . DL.filter (\(poly,_) -> not $ isNull poly) . reducePolys res
                                                            newTierTwo = DL.foldl' (\acc x -> DS.insert (PS x) acc) tierTwo redPolys
                                                        in gB2 res newTierTwo highSugPolys
                                     | otherwise = let (gen,newGens) = deleteFindMin tierTwo
                                                       redGen = makeMonic $ gen /. res
                                                       SP newSpMap newRes = updateSPolys (SP spMap res) redGen
                                                       (lowSugPolys, highSugPolys) = delFindLowest (SP newSpMap newRes)
                                                       numBins = DL.length lowSugPolys
                                                       redPolys = decluster (lift worker (cluster numBins lowSugPolys)
                                                                             `using` parList rwhnf)
                                                       worker = DL.map makeMonic . DL.filter (\(poly,_) -> not $ isNull poly) . reducePolys newRes
                                                       newTierTwo = DL.foldl' (\acc x -> DS.insert (PS x) acc) newGens redPolys
                                                   in if isNull $ fst redGen then
                                                          gB2 res newGens spMap
                                                      else --("Queue size: "++(show $ DS.size newOneByOne)) `trace`
                                                          gB2 newRes newTierTwo highSugPolys

{-
oldgB :: Ord (Mon ord) => DS.Set (PolySug ord) -> Ideal ord
oldgB seed = let (initial,restSeed) = deleteFindMin seed
             in gB' (I $ DV.singleton initial) restSeed DM.empty where
                 gB' res oneByOne spMap | DS.null oneByOne && DM.null spMap = res
                                        | DS.null oneByOne = let (lowSugPolys, higherSugPolys) = delFindLowest (SP spMap res)
                                                                 numBins = DL.length lowSugPolys
                                                                 redPolys = decluster (lift worker (cluster numBins lowSugPolys)
                                                                                       `using` parList rwhnf)
                                                                 worker = DL.map (\(poly,sug) -> (scalePoly (recip $ coefLT poly) poly,sug)) .
                                                                          DL.filter (\(poly,_) -> not $ isNull poly) . reducePolys res
                                                                 newOneByOne = DL.foldl' (\acc x -> DS.insert (PS x) acc) oneByOne redPolys
                                                             in gB' res newOneByOne higherSugPolys
                                        | otherwise = let (gen,newGens) = deleteFindMin oneByOne
                                                          SP newspMap newres = updateSPolys (SP spMap res) gen
                                                          (lowSugPolys, higherSugPolys) = delFindLowest (SP newspMap newres)
                                                          numBins = DL.length lowSugPolys
                                                          redPolys = decluster (lift worker (cluster numBins lowSugPolys)
                                                                                `using` parList rwhnf)
                                                          worker = DL.map (\(poly,sug) -> (scalePoly (recip $ coefLT poly) poly,sug)) .
                                                                   DL.filter (\(poly,_) -> not $ isNull poly) . reducePolys newres
                                                          newOneByOne = DL.foldl' (\acc x -> DS.insert (PS x) acc) newGens redPolys
                                                      in if isNull $ fst (gen /. res) then
                                                             gB' res newGens spMap
                                                         else --("Queue size: "++(show $ DS.size newOneByOne)) `trace`
                                                             gB' newres newOneByOne higherSugPolys

modgB' :: Ord (Mon ord) => DS.Set (PolySug ord) -> Ideal ord
modgB' seed = let (initial,restSeed) = deleteFindMin seed
             in gB' (I $ DV.singleton initial) restSeed DM.empty where
                 gB' res oneByOne spMap | DS.null oneByOne && DM.null spMap = res
                                        | DS.null oneByOne = let (lowSugPolys, higherSugPolys) = delFindLowest (SP spMap res)
                                                                 numBins = DL.length lowSugPolys
                                                                 redPolys = decluster (lift worker (cluster numBins lowSugPolys)
                                                                                       `using` parList rwhnf)
                                                                 worker = DL.map (\(poly,sug) -> (scalePoly (recip $ coefLT poly) poly,sug)) .
                                                                          DL.filter (\(poly,_) -> not $ isNull poly) . reducePolys res
                                                                 newOneByOne = DL.foldl' (\acc x -> DS.insert (PS x) acc) oneByOne redPolys
                                                             in gB' res newOneByOne higherSugPolys
                                        | otherwise = let (gen,newGens) = deleteFindMin oneByOne
                                                          reducedGenNotMonic = gen /. res
                                                          reducedGen = (scalePoly (recip $ coefLT (fst reducedGenNotMonic)) (fst reducedGenNotMonic),
                                                                        snd reducedGenNotMonic)
                                                          SP newspMap newres = updateSPolys (SP spMap res) reducedGen
                                                          (lowSugPolys, higherSugPolys) = delFindLowest (SP newspMap newres)
                                                          numBins = DL.length lowSugPolys
                                                          redPolys = decluster (lift worker (cluster numBins lowSugPolys)
                                                                                `using` parList rwhnf)
                                                          worker = DL.map (\(poly,sug) -> (scalePoly (recip $ coefLT poly) poly,sug)) .                                                        
                                                                   DL.filter (\(poly,_) -> not $ isNull poly) . reducePolys newres
                                                          newOneByOne = DL.foldl' (\acc x -> DS.insert (PS x) acc) newGens redPolys
                                                      in if isNull $ fst reducedGen then
                                                             gB' res newGens spMap
                                                         else --("Queue size: "++(show $ DS.size newOneByOne)) `trace`
                                                             gB' newres newOneByOne higherSugPolys
-}

gB'' :: Ord (Mon ord) => DS.Set (PolySug ord) -> Int -> Ideal ord
gB'' seed c = let (initial,restSeed) = deleteFindMin seed
              in gB' (I $ DV.singleton initial) restSeed DM.empty c where
                gB' res _ _ 0 = res
                gB' res oneByOne spMap n | DS.null oneByOne && DM.null spMap = res
                                         | DS.null oneByOne = let (lowSugPolys, higherSugPolys) = delFindLowest (SP spMap res)
                                                                  numBins = DL.length lowSugPolys
                                                                  redPolys = decluster (lift worker (cluster numBins lowSugPolys)
                                                                                        `using` parList rwhnf)
                                                                  worker = DL.map (\(poly,sug) -> (scalePoly (recip $ coefLT poly) poly,sug)) . 
                                                                           DL.filter (\(poly,_) -> not $ isNull poly) . reducePolys res
                                                                  newOneByOne = DL.foldl' (\acc x -> DS.insert (PS x) acc) oneByOne redPolys
                                                              in gB' res newOneByOne higherSugPolys (n-1)
                                         | otherwise = let (gen,newGens) = deleteFindMin oneByOne
                                                           SP newspMap newres = updateSPolys (SP spMap res) gen
                                                           (lowSugPolys, higherSugPolys) = delFindLowest (SP newspMap newres)
                                                           numBins = DL.length lowSugPolys
                                                           redPolys = decluster (lift worker (cluster numBins lowSugPolys)
                                                                                 `using` parList rwhnf)
                                                           worker = DL.map (\(poly,sug) -> (scalePoly (recip $ coefLT poly) poly,sug)) .
                                                                    DL.filter (\(poly,_) -> not $ isNull poly) . reducePolys newres
                                                           newOneByOne = DL.foldl' (\acc x -> DS.insert (PS x) acc) newGens redPolys
                                                       in if isNull $ fst (gen /. res) then
                                                              gB' res newGens spMap n
                                                          else --("Queue size: "++(show $ DS.size newOneByOne)) `trace`
                                                               --("Sug deg: "++(show $ snd gen)++" Poly: " ++ (show $ fst (gen /. res))) `trace` 
                                                               gB' newres newOneByOne higherSugPolys (n-1)