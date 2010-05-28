
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances #-}

module Yaiba.Polynomial where

import Data.Map
import Yaiba.Monomial
--import qualified Data.Array.Parallel.Unlifted as U
import Math.Algebra.Field.Base
import Data.Maybe
import Prelude hiding (null,
                       filter,
                       map)

newtype Polynomial ord = Polynomial (Map (Monomial ord) Q)

--instance (Ord (Monomial ord)) => U.Elt (Polynomial ord)

instance Show (Polynomial ord) where
  show (Polynomial a) = showTerm $ toList a

-- A shortened prettyLexPrint
pLp :: Polynomial Lex -> [Char]
pLp = prettyLexPrint

prettyLexPrint :: Polynomial Lex -> [Char]
prettyLexPrint (Polynomial a) = showTerm $ reverse (toAscList a)
  
showTerm :: (Num t1, Show t) => [(t, t1)] -> [Char]
showTerm [] = ""
showTerm ((a,b):[]) | b==0 = "" 
                    | otherwise = (show a) ++ (show b)
showTerm ((a,b):as) | b==0 = showTerm as
                    | otherwise = (show a) ++ (show b) ++ " + " ++ showTerm as

--An empty polynomial.
nullPoly :: Polynomial ord
nullPoly = Polynomial empty

isNull :: Polynomial t -> Bool
isNull (Polynomial a) = null a
          
monPoly a b | b==0 = nullPoly 
            | otherwise = Polynomial (singleton a b)
           
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

prune :: (Ord (Monomial ord)) => Polynomial ord -> Polynomial ord
prune (Polynomial a) = Polynomial $ filter (/=0) a

getMap :: Polynomial ord -> Map (Monomial ord) Q
getMap (Polynomial a) = a

leadTerm (Polynomial a) = findMax a

instance (Ord (Monomial ord)) => Num (Polynomial ord) where
  Polynomial a + Polynomial b = prune $ Polynomial (unionWith (+) a b)
  a * Polynomial b = prune $ Polynomial (foldWithKey (\k v -> unionWith (+) (getMap (monMult k v a))) empty b)
  negate (Polynomial a) = Polynomial $ map negate a
    
monMult :: (Ord (Monomial ord)) => Monomial ord -> Q -> Polynomial ord -> Polynomial ord
monMult a b (Polynomial c) = Polynomial $ foldWithKey (f a b) empty c where
  f a b k v acc = unionWith (+) (singleton (a*k) (b*v)) acc

--Divides the first polynomial by the second.

{-
quoRem :: (Ord (Monomial ord)) => 
          Polynomial ord -> [Polynomial ord] -> ([Polynomial ord], Polynomial ord)
quoRem a b = div a (sort b) [] 0 where
  div 0 _ as r = (as, r)
  div f fs as r = foldl -}