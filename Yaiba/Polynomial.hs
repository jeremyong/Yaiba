
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}

module Yaiba.Polynomial where

import Data.Map hiding (fromList)
import qualified Data.Map as M
import Yaiba.Monomial
--import qualified Data.Array.Parallel.Unlifted as U
import Math.Algebra.Field.Base
import Prelude hiding (null,
                       filter,
                       map,
                       rem)

newtype Polynomial ord = Polynomial (Map (Monomial ord) Q)

--instance (Ord (Monomial ord)) => U.Elt (Polynomial ord)

instance (Ord (Monomial ord)) => Show (Polynomial ord) where
  show (Polynomial a) = showTerm $ toAscList a

-- A shortened prettyLexPrint
pLp :: Polynomial Lex -> [Char]
pLp = prettyLexPrint

prettyLexPrint :: Polynomial Lex -> [Char]
prettyLexPrint (Polynomial a) = showTerm $ reverse (toAscList a)
  

showTerm [] = ""
showTerm ((a,b):[]) | show a == " " = show b
                    | b==0 = ""
                    | otherwise = if b/=1 then (show b) ++ (show a) else tail (show a)
showTerm ((a,b):as) | show a == " " = (show b) ++ showTerm as
                    | b==0 = showTerm as
                    | otherwise = if b/=1 then (show b) ++ (show a) ++ " + " ++ showTerm as else (tail (show a)) ++ " + " ++ showTerm as

--Constructors

--An empty polynomial.
nullPoly :: Polynomial ord
nullPoly = Polynomial empty

isNull :: Polynomial ord -> Bool
isNull (Polynomial a) = null a
          
monPoly :: (Monomial ord, Q) -> Polynomial ord
monPoly (a,b) | b==0 = nullPoly 
              | otherwise = Polynomial (singleton a b)
                          
fromList :: (Ord (Monomial ord)) => [(Monomial ord, Q)] -> Polynomial ord
fromList a = prune $ Polynomial (M.fromList a)
           
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

--Removes elements that point to the zero element of the field.
prune :: (Ord (Monomial ord)) => Polynomial ord -> Polynomial ord
prune (Polynomial a) = Polynomial $ filter (/=0) a

getMap :: Polynomial ord -> Map (Monomial ord) Q
getMap (Polynomial a) = a

--leadTerm nullPoly = (Monomial [],0)
leadTerm :: Polynomial ord -> (Monomial ord, Q)
leadTerm a = findMax $ getMap a

deleteFindLT :: Polynomial ord -> (Polynomial ord, Polynomial ord)
deleteFindLT a = let (x,y) = deleteFindMax $ getMap a
                 in ((monPoly x),Polynomial y)

numTerms :: Polynomial ord -> Int
numTerms a = size $ getMap a

instance (Ord (Monomial ord)) => Num (Polynomial ord) where
  Polynomial a + Polynomial b = prune $ Polynomial (unionWith (+) a b)
  --Polynomial a + Polynomial b = Polynomial $ differenceWith (\x y -> if x+y==0 then Nothing else Just (x+y)) a b
  a * Polynomial b = prune $ Polynomial (foldWithKey (\k v -> unionWith (+) (getMap (monMult k v a))) empty b)
  negate (Polynomial a) = Polynomial $ map negate a
    
monMult :: (Ord (Monomial ord)) => Monomial ord -> Q -> Polynomial ord -> Polynomial ord
monMult a b (Polynomial c) = Polynomial $ foldWithKey (f a b) empty c where
  f a' b' k v acc = unionWith (+) (singleton (a' * k) (b' * v)) acc

--Divides the first polynomial by the second once
quoRem :: (Ord (Monomial ord)) =>
          Polynomial ord -> Polynomial ord -> (Polynomial ord, Polynomial ord)
quoRem a b = quoRem' a b nullPoly where
  quoRem' rem d quo | numTerms rem == 0 = (quo, nullPoly)
                    | otherwise = let 
    !remLT = leadTerm rem
    !dLT = leadTerm d
    !remOd = (fst remLT) / (fst dLT)
    !remOdco = (snd remLT) / (snd dLT) in
    case (signs remOd) of
      -1 -> (quo, rem)
      1 -> (quo + (Polynomial (singleton remOd remOdco)), rem - (monMult remOd remOdco d))

--Divides the first polynomial by the second the entire way through
quoRem'' :: (Ord (Monomial ord)) =>
           Polynomial ord -> Polynomial ord -> (Polynomial ord, Polynomial ord)
quoRem'' a b = quoRem''' a b nullPoly where
  quoRem''' rem d quo | numTerms rem == 0 = (quo, nullPoly)
                      | otherwise = let 
    !remLT = leadTerm rem
    !dLT = leadTerm d
    !remOd = (fst remLT) / (fst dLT)
    !remOdco = (snd remLT) / (snd dLT) in
    case (signs remOd) of
      -1 -> (quo, rem)
      1 -> quoRem''' (rem - (monMult remOd remOdco d)) d (quo + (Polynomial (singleton remOd remOdco)))