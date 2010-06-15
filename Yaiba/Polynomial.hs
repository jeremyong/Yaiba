
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}

module Yaiba.Polynomial where

import Data.Map hiding (fromList)
import qualified Data.Map as DM
import Yaiba.Monomial
import Math.Algebra.Field.Base
import Prelude hiding (null,
                       filter,
                       map,
                       rem)

newtype Polynomial ord = P (Map (Monomial ord) Q)

--instance (Ord (Monomial ord)) => U.Elt (Polynomial ord)

instance (Ord (Monomial ord)) => Show (Polynomial ord) where
  show a | numTerms a == 0 = "0"
         | otherwise = showTerm $ toAscList (getMap a)

-- A shortened prettyLexPrint
pLp :: Polynomial Lex -> [Char]
pLp = prettyLexPrint

prettyLexPrint :: Polynomial Lex -> [Char]
prettyLexPrint b@(P a) | numTerms b == 0 = "0" 
                       | otherwise = showTerm $ reverse (toAscList a)
  
showTerm :: [(Monomial ord, Q)] -> [Char]
showTerm [] = ""
showTerm ((a,b):[]) | show a == " " = show b
                    | b==0 = ""
                    | otherwise = if b/=1 then
                                    (show b) ++ (show a) 
                                  else
                                    tail (show a)
showTerm ((a,b):as) | show a == " " = (show b) ++ showTerm as
                    | b==0 = showTerm as
                    | otherwise = if b/=1 then
                                    (show b) ++ (show a) ++ " + " ++ showTerm as 
                                  else
                                    (tail (show a)) ++ " + " ++ showTerm as

--Constructors

--An empty polynomial.
nullPoly :: Polynomial ord
nullPoly = P empty

isNull :: Polynomial ord -> Bool
isNull (P a) = null a

monPoly :: (Monomial ord, Q) -> Polynomial ord
monPoly (a,b) | b==0 = nullPoly 
              | otherwise = P (singleton a b)
                          
fromList :: (Ord (Monomial ord)) => [(Monomial ord, Q)] -> Polynomial ord
fromList a = prune $ P (DM.fromList a)
           
--Dummy instance. Don't use, use "compare" instead
instance Eq (Polynomial ord) where

instance (Ord (Monomial ord)) => Ord (Polynomial ord) where
  compare (P a) (P b) | null a && null b = EQ
                      | null a = LT
                      | null b = GT
                      | top == EQ = compare taila tailb
                      | otherwise = top where
    top = compare (findMax a) (findMax b)
    taila = P (deleteMax a)
    tailb = P (deleteMax b)

insertTerm :: (Ord (Monomial ord)) => Polynomial ord -> Monomial ord -> Q -> Polynomial ord
insertTerm (P a) b c = P (insertWith (+) b c a)

--Removes elements that point to the zero element of the field.
prune :: (Ord (Monomial ord)) => Polynomial ord -> Polynomial ord
prune (P a) = P $ filter (/=0) a

getMap :: Polynomial ord -> Map (Monomial ord) Q
getMap (P a) = a

--leadTerm nullPoly = (Monomial [],0)
leadTerm :: Polynomial ord -> (Monomial ord, Q)
leadTerm (P a) | null a == True = (M [],0)
               | otherwise = findMax a

deleteFindLT :: Polynomial ord -> (Polynomial ord, Polynomial ord)
deleteFindLT a = let (x,y) = deleteFindMax $ getMap a
                 in ((monPoly x),P y)

numTerms :: Polynomial ord -> Int
numTerms a = size $ getMap a

instance (Ord (Monomial ord)) => Num (Polynomial ord) where
  P a + P b = prune $ P (unionWith (+) a b)
  a * P b = prune $ P (foldWithKey (\k v -> unionWith (+) (getMap (monMult k v a))) empty b)
  negate (P a) = P $ map negate a

monMult :: (Ord (Monomial ord)) => Monomial ord -> Q -> Polynomial ord -> Polynomial ord
monMult a b (P c) = P $ foldWithKey (f a b) empty c where
  f a' b' k v acc = unionWith (+) (singleton (a' * k) (b' * v)) acc

--Divides the first polynomial by the second once
quoRem :: (Ord (Monomial ord)) =>
          Polynomial ord -> Polynomial ord -> (Polynomial ord, Polynomial ord)
quoRem a b = quoRem' a b nullPoly where
  quoRem' rem d quo | isNull rem == True = (quo, nullPoly)
                    | otherwise = let remLT = leadTerm rem
                                      dLT = leadTerm d
                                      remOd = (fst remLT) / (fst dLT)
                                      remOdco = (snd remLT) / (snd dLT) 
                                  in case (signs remOd) of
                                    -1 -> (quo, rem)
                                    1 -> (quo + (P (singleton remOd remOdco)), rem - (monMult remOd remOdco d))

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
      1 -> quoRem''' (rem - (monMult remOd remOdco d)) d (quo + (P (singleton remOd remOdco)))