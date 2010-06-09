
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XPArr #-}

module Yaiba.Ideal where

import Yaiba.Monomial
import Yaiba.Polynomial
import Data.List
import Prelude hiding (rem)

newtype Ideal ord = Ideal [Polynomial ord] deriving (Eq)

printIdeal :: Ideal Lex -> [Char]
printIdeal (Ideal a) = concatMap ((++", ") . pLp) a

getPolys :: Ideal ord -> [Polynomial ord]
getPolys (Ideal a) = a

--Division algorithm (outputs remainder)

(/.) :: (Ord (Monomial ord)) =>
        Polynomial ord -> Ideal ord -> Polynomial ord
(/.) p q = (/..) p q nullPoly

(/..) dend (Ideal ds) rem = case isNull dend of 
  True -> rem
  False -> let (a,b) = divIdeal dend ds 
           in case b of
             False -> let (lt,rest) = deleteFindLT dend
                      in (/..) rest (Ideal ds) (rem + lt)
             True -> (/..) a (Ideal ds) rem

--Takes a dividend and a list of polynomials and divides until the lead term
--of the remainder is not divisible by any of the divisors.
--Outputs a tuple that gives the pseudo-eremainder and whether or not a
--division occurred.

divIdeal p q = divIdeal' (p,False) q where
  divIdeal' (b,divOcc) [] = (b,divOcc)
  divIdeal' (b,divOcc) (a:as) = let !(x,y) = quoRem b a
                                in case isNull x of
                                    True -> divIdeal' (b,divOcc) as
                                    False -> (y,True)                                    
                                                                              
{- This is probably slower
divIdeal :: (Ord (Monomial ord)) =>
            Polynomial ord -> [Polynomial ord] -> (Polynomial ord, Bool)
divIdeal d ds = foldl' divIdeal' (d,False) ds

divIdeal'
  :: (Ord (Monomial ord)) =>
     (Polynomial ord, Bool) -> Polynomial ord -> (Polynomial ord, Bool)
divIdeal' (b,True) _ = (b,True)
divIdeal' (b,divOcc) a = let !(x,y) = quoRem b a 
                         in if numTerms x == 0 then 
                              (b,divOcc)
                            else (y,True)
-}
