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
import Prelude hiding (rem)

newtype Ideal ord = I (DV.Vector (Poly ord,Sugar ord))

instance NFData (Ideal ord) where
    rnf (I a) = DV.foldl' (\_ x -> rnf x) () a

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

ifoldl' f acc (I as) = DV.ifoldl' f acc as
foldl' f acc (I as) = DV.foldl' f acc as

snoc (I as) a = I $! DV.snoc as a

--union (I as) (I bs) = I $! DV.++ as bs

numGens (I as) = DV.length as

null (I as) = DV.null as

(!) (I as) index = (DV.!) as index

reduceIdeal ideal@(I as) = I $ DV.imap (\i a -> a /. (allExcept i ideal)) as 

allExcept index (I as) | index == 0 = I back
                       | index == len = I front
                       | otherwise = I (front DV.++ back) 
    where
      len = DV.length as -1
      front = DV.take (index-1) as
      back = DV.drop index as

-- | Reduces a polynomial by an ideal into something irreducible. Records
-- its "history" in the Sugar
(/.) :: Ord (Mon ord) => (Poly ord, Sugar ord) -> 
        Ideal ord -> (Poly ord, Sugar ord)
(/.) p ideal = (/..) p nullPoly where
    (/..) (poly,psug) rem = if isNull poly then (rem,psug) else
                                let !(new,newsug,divOcc) = divByIdeal (poly,psug) ideal
                                in if divOcc then (/..) (new,newsug) rem
                                   else let !((m,q),rest) = deleteFindLT poly
                                            !newrem = rem + monPoly m q
                                        in (/..) (rest,newsug) newrem

{-
(/.) :: Ord (Mon ord) => (Poly ord, Sugar ord) -> 
         Ideal ord -> (Poly ord, Sugar ord)
(/.) p ideal = (/..) p nullPoly where
    (/..) (poly,psug) rem = if isNull poly then (rem,psug) else
                                let !(new,newsug,divOcc) = divByIdeal (poly,psug) ideal
                                in if divOcc then (/..) (new,newsug) rem
                                   else (/..) (nullPoly,psug) poly
-}
-- | Auxilliary function to /.
divByIdeal :: Ord (Mon ord) => (Poly ord, Sugar ord) -> 
              Ideal ord -> (Poly ord, Sugar ord, Bool)
divByIdeal (poly,sug) ideal = foldl' divByIdeal' (poly, sug, False) ideal where
  divByIdeal' (p,s,divOcc) d | isNull $ fst d = (p,s,divOcc) 
                             | otherwise = if divOcc then (p,s,divOcc) else
                                               let (quo,rem,remsug) = quoRem (p,s) d
                                               in if isNull quo then (p,s,divOcc) 
                                                  else (rem,remsug,True)

reducePolys :: Ord (Mon ord) => Ideal ord ->
               [(Poly ord,Sugar ord)] -> [(Poly ord,Sugar ord)]
reducePolys d = DL.map (/. d)