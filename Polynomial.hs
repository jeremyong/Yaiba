
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances #-}

module Yaiba.Polynomial where

import Data.Map
import Yaiba.Monomial
import Math.Algebra.Field.Base
import Prelude hiding (null)

newtype Polynomial ord = Polynomial (Map (Monomial ord) Q)

instance Show (Polynomial ord) where
  show (Polynomial a) = showTerm $ toList a where
    showTerm [] = ""                    
    showTerm (a:[]) = show a
    showTerm (a:as) = (show a) ++ " + " ++ (showTerm as)

-- A shortened prettyLexPrint
pLp :: Polynomial Lex -> [Char]
pLp = prettyLexPrint

prettyLexPrint :: Polynomial Lex -> [Char]
prettyLexPrint (Polynomial a) = showTerm $ reverse (toAscList (a::Map (Monomial Lex) Q)) where 
  showTerm [] = ""
  showTerm (a:[]) = show a 
  showTerm (a:as) = (show a) ++ " + " ++ (showTerm as)                        

--An empty polynomial.
nullPoly :: Polynomial ord
nullPoly = Polynomial empty
          
monPoly a b = Polynomial (singleton a b)
           
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

prune :: Polynomial t -> Map (Monomial t) Q
prune (Polynomial a) = a

getMap :: Polynomial ord -> Map (Monomial ord) Q
getMap (Polynomial a) = a

instance (Ord (Monomial ord)) => Num (Polynomial ord) where
  Polynomial a + Polynomial b = Polynomial $ unionWith (+) a b
  a * Polynomial b = Polynomial $ foldWithKey (\k v -> unionWith (+) (getMap (monMult k v a))) empty b
    
monMult :: (Ord (Monomial ord)) => Monomial ord -> Q -> Polynomial ord -> Polynomial ord
monMult a b (Polynomial c) = Polynomial $ foldWithKey (f a b) empty c where
  f a b k v acc = unionWith (+) (singleton (a*k) (b*v)) acc
