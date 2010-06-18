{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
-- | A Poly is synonymous to a map from Mon lists to a rational number
module Yaiba.Polynomial where

import Data.Map hiding (fromList,(!))
import qualified Data.Map as DM
import Yaiba.Monomial
import Yaiba.Sugar
import Math.Algebra.Field.Base
import Data.Array.Unboxed
import qualified Data.List as DL
import Prelude hiding (null,filter,map,rem)

newtype Poly ord = P (Map (Mon ord) Q)

instance (Ord (Mon ord)) => Show (Poly ord) where
  show a | numTerms a == 0 = "0"
         | otherwise = showTerm $ toAscList (getMap a)

pLp :: Poly Lex -> String
pLp = prettyLexPrint

prettyLexPrint :: Poly Lex -> String
prettyLexPrint b@(P a) | numTerms b == 0 = "0" 
                       | otherwise = showTerm $ reverse (toAscList a)
  
showTerm :: [(Mon ord, Q)] -> String
showTerm [] = ""
showTerm ((a,b):[]) | show a == " " = show b
                    | b==0 = ""
                    | otherwise = if b/=1 then
                                    show b ++ show a 
                                  else
                                    tail $ show a
showTerm ((a,b):as) | show a == " " = show b ++ showTerm as
                    | b==0 = showTerm as
                    | otherwise = if b/=1 then
                                    show b ++ show a ++ " + " ++ showTerm as 
                                  else
                                    tail (show a) ++ " + " ++ showTerm as

-- | Constructors

-- | Creates an empty polynomial.
nullPoly :: Poly ord
nullPoly = P empty

-- | Creates a single termed polynomial.
monPoly :: (Mon ord, Q) -> Poly ord
monPoly (a,b) | b==0 = nullPoly 
              | otherwise = P $ singleton a b
                          
-- | Creates a polynomial from a list.
fromList :: (Ord (Mon ord)) => [([Int], Q)] -> Poly ord
fromList a = prune $ P $ DM.fromList $ DL.map (\(x,b) -> (M $ listArray (0,DL.length x-1) x,b)) a

instance (Ord (Mon ord)) => Eq (Poly ord) where
  a == b = isNull $ a-b
  
instance (Ord (Mon ord)) => Ord (Poly ord) where
  compare (P a) (P b) | null a && null b = EQ
                      | null a = LT
                      | null b = GT
                      | top == EQ = compare taila tailb
                      | otherwise = top where
    top = compare (findMax a) (findMax b)
    taila = P (deleteMax a)
    tailb = P (deleteMax b)

insertTerm :: (Ord (Mon ord)) => Poly ord -> Mon ord -> Q -> Poly ord
insertTerm (P a) b c = P (insertWith (+) b c a)

isNull :: Poly ord -> Bool
isNull (P a) = null a

-- | Removes elements that point to the zero element of the field.
prune :: (Ord (Mon ord)) => Poly ord -> Poly ord
prune (P a) = P $ filter (/=0) a

getMap :: Poly ord -> Map (Mon ord) Q
getMap (P a) = a

-- | Returns a tuple of the lead term Mon list and its coefficient.
leadTerm :: Poly ord -> (Mon ord, Q)
leadTerm (P a) | null a = (Constant,0)
               | otherwise = findMax a
                             
-- | Just returns the lead term Mon list.
monLT (P a) | null a = Constant
            | otherwise = fst $ findMax a

-- | The degree of the poly.
deg a = degree $ monLT a

-- | Returns a tuple of the lead term as Poly and the rest of the supplied Poly.
deleteFindLT :: Poly ord -> (Poly ord, Poly ord)
deleteFindLT a = let (x,y) = deleteFindMax $ getMap a
                 in (monPoly x,P y)

-- | The length of the poly
numTerms :: Poly ord -> Int
numTerms a = size $ getMap a

instance (Ord (Mon ord)) => Num (Poly ord) where
  P a + P b = prune $ P (unionWith (+) a b)
  a * P b = prune $ P (foldWithKey (\k v -> unionWith (+) (getMap (monMult k v a))) empty b)
  negate (P a) = P $ map negate a

-- | Scales every term of a Polynomial by a Mon list and rational number.
monMult :: (Ord (Mon ord)) => Mon ord -> Q -> Poly ord -> Poly ord
monMult a b (P c) = P $ foldWithKey (f a b) empty c where
  f a' b' k v = unionWith (+) (singleton (a' * k) (b' * v))

--Divides the first polynomial by the second once
{-
quoRem :: (Ord (Mon ord)) =>
            Poly ord -> (Poly ord,Sugar ord) -> (Poly ord, Poly ord)
quoRem rem (d,_) | isNull rem = (nullPoly, nullPoly)
                 | otherwise = let (a1,a2) = leadTerm rem
                                   (b1,b2) = leadTerm d
                                   remOd = a1/b1
                                   remOdco = a2/b2 
                               in case isFactor b1 a1 of
                                 False -> (nullPoly, rem)
                                 True -> (P (singleton remOd remOdco), rem - (monMult remOd remOdco d))
-}

-- | Divides the first polynomial by the second repeatedly until it fails.
quoRem :: (Ord (Mon ord)) =>
           Poly ord -> (Poly ord,Sugar ord) -> (Poly ord, Poly ord)
quoRem a (b,_) = quoRem' a b nullPoly where
  quoRem' rem d quo | numTerms rem == 0 = (quo, nullPoly)
                    | otherwise = let !(a1,a2) = leadTerm rem
                                      !(b1,b2) = leadTerm d
                                      !remOd = a1/b1
                                      !remOdco = a2/b2
                                  in if isFactor b1 a1 then
                                       quoRem' (rem - monMult remOd remOdco d) d (quo + P (singleton remOd remOdco))
                                     else
                                       (quo, rem)
                                      
