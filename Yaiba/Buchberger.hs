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

gB :: Ord (Mon ord) => DS.Set (PolySug ord) -> Ideal ord
gB seed = let (initial,restSeed) = deleteFindMin seed
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