{-# OPTIONS_GHC -XBangPatterns -fglasgow-exts -XUndecidableInstances #-}
-- | Ideals are represented as lists of tuples consisting of
-- a Poly and a Sugar.
module Yaiba.Ideal where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Sugar
import qualified Data.List as DL
import qualified Data.Vector as DV
import Control.DeepSeq
import Data.Ord
import Prelude hiding (rem)

newtype Ideal ord = I (DV.Vector (Poly ord,Sugar ord))

instance NFData (Ideal ord) where
    rnf (I a) = DV.foldl' (\_ x -> rnf x) () a

instance Ord (Mon ord) => Show (Ideal ord) where
    show = show . getPolys

getPolys :: Ideal ord -> [Poly ord]
getPolys (I a) = DL.map fst (DV.toList a)

initSugars :: Ord (Mon ord) => [Poly ord] -> [(Poly ord, Sugar ord)]
initSugars as = DL.map initSugar (DL.sort as)
initPolySugars :: Ord (Mon ord) => [Poly ord] -> [PolySug ord]
initPolySugars as = DL.map (\a -> PS (initSugar a)) (DL.sort as)

initSugarsIdeal :: Ord (Mon ord) => [Poly ord] -> DV.Vector (Poly ord, Sugar ord)
initSugarsIdeal as = DV.fromList $! DL.map initSugar (DL.sort as)

initSugar :: Ord (Mon ord) => Poly ord -> (Poly ord, Sugar ord)
initSugar a = (a, S $! totalDeg a)

tau :: Ideal ord -> Int -> Mon ord -> Mon ord
tau (I as) index mon = lcmMon (monLT $ fst $ as DV.! index) mon

ifoldl' :: (a -> Int -> (Poly t, Sugar t) -> a) -> a -> Ideal t -> a
ifoldl' f acc (I as) = DV.ifoldl' f acc as

foldl' :: (a -> (Poly t, Sugar t) -> a) -> a -> Ideal t -> a
foldl' f acc (I as) = DV.foldl' f acc as

snoc :: Ideal ord -> (Poly ord, Sugar ord) -> Ideal ord
snoc (I as) a = I $! DV.snoc as a

numGens :: Ideal t -> Int
numGens (I as) = DV.length as

null :: Ideal ord -> Bool
null (I as) = DV.null as

(!) :: Ideal t -> Int -> (Poly t, Sugar t)
(!) (I as) index = (DV.!) as index

find tauk fs = [ gi | i <- [0..(numGens fs -1)], let gi@(polyi,_) = fs!i, let taui = monLT polyi, taui `isFactor` tauk ]

totalRed p (I fs) = totalRed' p nullPoly where
    totalRed' polysug rem = let !(rem', poly', sug, divOcc) = lppRedDivOcc polysug rem False
                            in if divOcc then
                                   totalRed' (poly',sug) rem'
                               else
                                   (rem'+poly',sug)
    lppRedDivOcc (poly,S psug) rem divOcc = if isNull poly then (rem,poly, S psug, divOcc) else
                                                let !((tauk,ck),newpoly) = deleteFindLT poly
                                                    !fi = DV.find (\(f,_) -> monLT f `isFactor` tauk) fs
                                                in case fi of
                                                     Just (polyf,S sugf) -> let (taui,ci) = leadTerm polyf
                                                                                tauki = divide tauk taui
                                                                            in --(scalePoly ci rem, scalePoly ci poly - monMult (divide tauk taui) ck polyf,
                                                                              (rem, poly - monMult tauki (ck/ci) polyf,
                                                                                  S $ max psug (degree tauki * sugf), True)
                                                     Nothing -> lppRedDivOcc (newpoly,S psug) (monAdd tauk ck rem) divOcc


{-
totalSaccRed p fs = totalRed' p nullPoly where
    totalRed' polysug rem = let !(rem', poly', sug, divOcc) = lppRedDivOcc polysug rem False
                            in if divOcc then
                                   totalRed' (poly',sug) rem'
                                   --let (red,redsug) = (poly',sug) /. fs
                                   --in (rem'+red,redsug)
                               else
                                   (rem'+poly',sug)
    lppRedDivOcc (poly,S psug) rem divOcc = if isNull poly then (rem,poly, S psug, divOcc) else
                                                let !((tauk,ck),newpoly) = deleteFindLT poly
                                                    -- !fi = DV.find (\(f,_) -> monLT f `isFactor` tauk) fs
                                                    !fi = find tauk fs
                                                in if fi == [] then
                                                       lppRedDivOcc (newpoly, S psug) (monAdd tauk ck rem) divOcc
                                                   else 
                                                       let gs = DL.minimumBy (\a b -> comparing 

                                                                              case fi of
                                                     Just (polyf,S sugf) -> let (taui,ci) = leadTerm polyf
                                                                            in --(scalePoly ci rem, scalePoly ci poly - monMult (divide tauk taui) ck polyf,
                                                                              (rem, poly - monMult (divide tauk taui) (ck/ci) polyf,
                                                                                  S $ max psug (degree tauk * sugf), True)
                                                     Nothing -> lppRedDivOcc (newpoly,S psug) (monAdd tauk ck rem) divOcc
-}