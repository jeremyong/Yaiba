
{-# OPTIONS_GHC -fglasgow-exts #-}

module Yaiba.Polynomial where

import Data.Map
import Yaiba.Monomial
import Math.Algebra.Field.Base
import Prelude hiding (null)

newtype Polynomial ord = Polynomial (Map (Monomial ord) Q)

instance Show (Polynomial Lex) where
  show (Polynomial a) = showTerm $ reverse (toAscList (a::Map (Monomial Lex) Q))
    where showTerm [] = ""
          showTerm (a:[]) = show a 
          showTerm (a:as) = (show a) ++ " + " ++ (showTerm as)
          
--An empty polynomial.
nullPoly :: Polynomial ord
nullPoly = Polynomial empty
          
--Dummy instance. Don't use, use "compare" instead
instance Eq (Polynomial ord) where
  
--Implements Lex comparison for Polynomials. Empty polynomials are equivalent.
instance Ord (Polynomial Lex) where
  compare (Polynomial a) (Polynomial b) | null a && null b = EQ
                                        | null a = LT
                                        | null b = GT
                                        | top == EQ = compare taila tailb
                                        | otherwise = top where
    top = compare (maxView a) (maxView b) --Will always return "Just ..."
    taila = Polynomial (deleteMax a)::Polynomial Lex
    tailb = Polynomial (deleteMax b)::Polynomial Lex

insertTerm :: (Ord (Monomial ord)) => Polynomial ord -> Monomial ord -> Q -> Polynomial ord
insertTerm (Polynomial a) b c = Polynomial (insertWith (+) b c a)

instance (Ord (Monomial ord)) => Num (Polynomial ord) where
  (Polynomial a) + (Polynomial b) = Polynomial $ unionWith (+) a b

--addCoefficients :: Set (Term ord) -> Q
--addCoefficients a = fold f 0 a where
  --f = (+) . coefficient