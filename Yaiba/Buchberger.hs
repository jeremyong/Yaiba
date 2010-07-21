{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
-- | Generates a Groebner basis of a supplied ideal.
module Yaiba.Buchberger where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Ideal
import Yaiba.SPoly
import qualified Data.List as DL
import Control.Parallel.Strategies
import qualified Data.Set as DS
import qualified Data.Vector as DV
import Prelude hiding (rem,null,map,filter)
import GHC.Conc (numCapabilities)
import Debug.Trace

ngB :: Ord (Mon ord) => DS.Set (PolySug ord) -> Ideal ord
ngB seed = let (initial,restSeed) = deleteFindMin seed
           in gB' (I $ DV.singleton initial) restSeed empty where
               gB' res fks queue | DS.null fks && isEmpty queue = res
                                 | DS.null fks = let (lowSugPolys, higherSugPolys) = delFindNLowest queue (numCapabilities-1) res
                                                     -- !redPolys' = worker $ parMap rdeepseq (\f -> (totalRed f res)) lowSugPolys
                                                     !redPolys' = worker $ parMap rdeepseq (\f -> (lppRed f res)) lowSugPolys
                                                     !worker = DL.filter (\(poly,_) -> not $ isNull poly)
                                                     !redPolys = parMap rdeepseq makeMonic redPolys'
                                                     newfks = DL.foldl' (\acc x -> DS.insert (PS x) acc) fks redPolys
                                                 in gB' res newfks higherSugPolys
                                 | otherwise = let (gen,newGens) = deleteFindMin fks
                                                   reducedGen = makeMonic $ totalRed gen res
                                                   --reducedGen = makeMonic $ gen /. res
                                                   newQueue = updateSPolys queue reducedGen res
                                                   newres = res `snoc` reducedGen
                                                   (lowSugPolys, higherSugPolys) = delFindNLowest newQueue (numCapabilities-1) newres
                                                   -- !redPolys' = worker $ parMap rdeepseq (\f -> (totalRed f res)) lowSugPolys
                                                   !redPolys' = worker $ parMap rdeepseq (\f -> (lppRed f res)) lowSugPolys
                                                   !worker = DL.filter (\(poly,_) -> not $ isNull poly)
                                                   !redPolys = parMap rdeepseq makeMonic redPolys'
                                                   newfks = DL.foldl' (\acc x -> DS.insert (PS x) acc) newGens redPolys
                                               in if isNull $ fst reducedGen then
                                                      gB' res newGens queue
                                                  else --show newres `trace` 
                                                      gB' newres newfks higherSugPolys

-- | This implementation of gB does not insert the supplied generators first. Instead, generators are
-- inserted by sugar degree.
modgB :: Ord (Mon ord) => DS.Set (PolySug ord) -> Ideal ord
modgB seed = let (initial,restSeed) = deleteFindMin seed
             in gB' (I $ DV.singleton initial) restSeed empty where
                 gB' res fks queue | DS.null fks && isEmpty queue = res
                                   | DS.null fks = let (lowSugPolys, higherSugPolys) = delFindLowest queue res
                                                       -- !redPolys' = worker $ parMap rdeepseq (\f -> (totalRed f res)) lowSugPolys
                                                       !redPolys' = worker $ parMap rdeepseq (\f -> (lppRed f res)) lowSugPolys
                                                       !worker = DL.filter (\(poly,_) -> not $ isNull poly)
                                                       !redPolys = parMap rdeepseq makeMonic redPolys'
                                                       newfks = DL.foldl' (\acc x -> DS.insert (PS x) acc) fks redPolys
                                                   in gB' res newfks higherSugPolys
                                   | otherwise = let (gen,newGens) = deleteFindMin fks
                                                     reducedGen = makeMonic $ totalRed gen res
                                                     --reducedGen = makeMonic $ gen /. res
                                                     newQueue = updateSPolys queue reducedGen res
                                                     newres = res `snoc` reducedGen
                                                     (lowSugPolys, higherSugPolys) = delFindLowest newQueue newres
                                                     -- !redPolys' = worker $ parMap rdeepseq (\f -> (totalRed f res)) lowSugPolys
                                                     !redPolys' = worker $ parMap rdeepseq (\f -> (lppRed f res)) lowSugPolys
                                                     !worker = DL.filter (\(poly,_) -> not $ isNull poly)
                                                     !redPolys = parMap rdeepseq makeMonic redPolys'
                                                     newfks = DL.foldl' (\acc x -> DS.insert (PS x) acc) newGens redPolys
                                                 in if isNull $ fst reducedGen then
                                                        gB' res newGens queue
                                                    else --show newres `trace` 
                                                         gB' newres newfks higherSugPolys


-- | Strategy accurate implementation of gB
accgB :: Ord (Mon ord) => DS.Set (PolySug ord) -> Ideal ord
accgB seed = let (initial,restSeed) = deleteFindMin seed
             in gB' (I $ DV.singleton initial) restSeed (SP []) where
                 gB' res fks queue | DS.null fks && isEmpty queue = res
                                   | DS.null fks = let (lowSugPoly, higherSugPolys) = delFindSingleLowest queue res
                                                       !redPoly = makeMonic $ totalRed lowSugPoly res
                                                       newfks = if not $ isNull $ fst redPoly then
                                                                         DS.insert (PS redPoly) fks
                                                                     else
                                                                         fks
                                                   in gB' res newfks higherSugPolys
                                   | otherwise = let (gen,newGens) = deleteFindMin fks
                                                     reducedGen = makeMonic $ totalRed gen res
                                                     newQueue = updateSPolys queue reducedGen res
                                                     newres = res `snoc` reducedGen
                                                     (lowSugPoly, higherSugPolys) = delFindSingleLowest newQueue newres
                                                     !redPoly = makeMonic $ totalRed lowSugPoly res
                                                     newfks = if not $ isNull $ fst redPoly then
                                                                  DS.insert (PS redPoly) newGens
                                                              else
                                                                  newGens
                                                 in if isNull $ fst reducedGen then
                                                        gB' res newGens queue
                                                    else gB' newres newfks higherSugPolys


gB :: Ord (Mon ord) => DS.Set (PolySug ord) -> Ideal ord
gB seed = let (initial,restSeed) = deleteFindMin seed
          in gB1 (I $ DV.singleton initial) restSeed (SP []) DS.empty where
              gB1 res tierOne queue tierTwo | DS.null tierOne = gB2 res tierTwo queue
                                            | otherwise = let (gen,newGens) = deleteFindMin tierOne
                                                              !redGen = makeMonic $ totalRed gen res
                                                              newQueue = updateSPolys queue redGen res
                                                              newRes = res `snoc` redGen
                                                              (lowSugPoly, highSugPolys) = delFindSingleLowest newQueue newRes
                                                              redPoly = makeMonic $ totalRed lowSugPoly res
                                                              newTierTwo = if not $ isNull $ fst redPoly then
                                                                               DS.insert (PS redPoly) tierTwo
                                                                           else tierTwo
                                                          in if isNull $ fst redGen then
                                                                 gB1 res newGens queue tierTwo
                                                             else
                                                                 gB1 newRes newGens highSugPolys newTierTwo
              gB2 res tierTwo queue | DS.null tierTwo && isEmpty queue = res
                                    | DS.null tierTwo = let (lowSugPoly, highSugPolys) = delFindSingleLowest queue res
                                                            redPoly = makeMonic $ totalRed lowSugPoly res
                                                            newTierTwo = if not $ isNull $ fst redPoly then
                                                                             DS.insert (PS redPoly) tierTwo
                                                                         else tierTwo
                                                        in gB2 res newTierTwo highSugPolys
                                     | otherwise = let (gen,newGens) = deleteFindMin tierTwo
                                                       redGen = makeMonic $ totalRed gen res
                                                       newQueue = updateSPolys queue redGen res
                                                       newRes = res `snoc` redGen
                                                       (lowSugPoly, highSugPolys) = delFindSingleLowest newQueue newRes
                                                       redPoly = makeMonic $ totalRed lowSugPoly res
                                                       newTierTwo = if not $ isNull $ fst redPoly then
                                                                        DS.insert (PS redPoly) tierTwo
                                                                    else tierTwo
                                                   in if isNull $ fst redGen then
                                                          gB2 res newGens queue
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
{-
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
-}