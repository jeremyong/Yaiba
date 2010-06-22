{-# OPTIONS_GHC -XBangPatterns -fglasgow-exts -XUndecidableInstances #-}
-- | Ideals are represented as lists of tuples consisting of
-- a Poly and a Sugar.
module Yaiba.Ideal where

import Yaiba.Monomial
import Yaiba.Polynomial
import Yaiba.Sugar
import Data.List
--import qualified Data.Set as Set
import Prelude hiding (rem)

newtype Ideal ord = I [(Poly ord,Sugar ord)]

getPolys :: Ideal ord -> [Poly ord]
getPolys (I a) = map fst a

initSugars = map (\a -> (a,S $ deg a))

-- | Reduces a polynomial by an ideal completely.
(/.) :: (Ord (Mon ord)) =>
        Poly ord -> Ideal ord -> Poly ord
(/.) p i = let (/..) a b r = if isNull a then r else
                                let !(new,divOcc) = divByIdeal a b
                                in if divOcc then (/..) new b r else
                                     let !(lt,rest) = deleteFindLT a
                                         !newR = r + lt
                                     in (/..) rest b newR
           in (/..) p i nullPoly
             
-- | Auxilliary function to /.
divByIdeal :: (Ord (Mon ord)) =>
              Poly ord -> Ideal ord -> 
              (Poly ord, Bool)
divByIdeal p (I ds) = foldl' divByIdeal' (p, False) ds where
  divByIdeal' = \(!p',!divOcc) !d -> if divOcc then (p',divOcc) else
                                         let !(quo,rem) = quoRem p' d
                                         in if isNull quo then (p,divOcc) else (rem,True)
