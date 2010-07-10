{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
-- | A Poly is synonymous to a map from Mon lists to a rational number
module Yaiba.PolynomialList where

import qualified Data.List as DL
import qualified Data.Ord as DO
import Yaiba.Monomial
import Yaiba.Sugar
import Yaiba.Base
import Data.Maybe
import Prelude hiding (null,filter,map,rem,sum)

-- | Polynomials are represented as lists of monomial/coefficient tuples.
newtype Poly ord = P [(Mon ord,Q)]

instance (Ord (Mon ord)) => Show (Poly ord) where
  show a | numTerms a == 0 = "0"
         | otherwise = showTerm $ getList a

instance (Ord (Mon ord)) => Eq (Poly ord) where
  a == b = if numTerms a /= numTerms b then False
           else isNull $! a-b
  
instance (Ord (Mon ord)) => Ord (Poly ord) where
    compare a b | top == EQ = compare taila tailb
                | otherwise = top where
                top = compare lta ltb
                (lta,taila) = deleteFindLT a
                (ltb,tailb) = deleteFindLT b

showTerm :: [(Mon ord,Q)] -> String
showTerm [] = ""
showTerm ((a,b):[]) | b==0 = ""
                    | show a == " " = show b
                    | otherwise = if b/=1 then
                                      show b ++ "*" ++ show a 
                                  else
                                      show a
showTerm ((a,b):as) | show a == " " = show b ++ showTerm as
                    | b==0 = showTerm as
                    | otherwise = if b/=1 then
                                      show b ++ "*" ++ show a ++ " + " ++ showTerm as 
                                  else
                                      show a ++ " + " ++ showTerm as

nullPoly :: Poly ord
nullPoly = P []

monPoly :: Mon ord -> Q -> Poly ord
monPoly m q = P $! [(m,q)]

fromList :: (Ord (Mon ord)) => [(Mon ord, Q)] -> Poly ord
fromList a = prune $ P $ DL.reverse $ DL.sort a

isNull :: Poly ord -> Bool
isNull (P []) = True
isNull _ = False

-- | Removes elements that point to the zero element of the field.
prune :: (Ord (Mon ord)) => Poly ord -> Poly ord
prune (P a) = P $! DL.filter (\(_,q)-> q/=0) a

getList :: Poly ord -> [(Mon ord,Q)]
getList (P as) = as

-- | Returns a tuple of the lead term Mon list and its coefficient.
leadTerm :: Poly ord -> (Mon ord, Q)
leadTerm (P []) = (Constant,0)
leadTerm (P (a:_)) = a
                             
-- | Just returns the lead term Mon list.
monLT :: Poly ord -> Mon ord
monLT = fst . leadTerm

-- | The degree of the poly.
deg :: Poly ord -> Int
deg = degree . monLT

-- | Returns a tuple of the lead term as Poly and the rest of the supplied Poly.
deleteFindLT :: Poly ord -> ((Mon ord, Q), Poly ord)
deleteFindLT (P []) = ((Constant,0), nullPoly)
deleteFindLT (P (a:as)) = (a,P as)

-- | The length of the poly
numTerms :: Poly ord -> Int
numTerms = DL.length . getList

maybeAdd :: Q -> Maybe Q -> Maybe Q
maybeAdd a Nothing = Just a
maybeAdd a (Just b) = let !sum = a+b
                      in if sum==0 then Nothing else Just sum

placeHead :: (Mon ord,Q) -> Poly ord -> Poly ord
placeHead a (P b) = P (a:b) 

instance (Ord (Mon ord)) => Num (Poly ord) where
    a + b | isNull a = b
          | isNull b = a
          | otherwise = let !((lta,qa),taila) = deleteFindLT a
                            !((ltb,qb),tailb) = deleteFindLT b
                        in case compare lta ltb of
                             EQ -> if qa+qb == 0 then
                                       taila+tailb
                                   else 
                                       placeHead (lta,qa+qb) (taila + tailb)
                             LT -> placeHead (ltb,qb) (a + tailb)
                             GT -> placeHead (lta,qa) (b + taila)
    a * b | isNull a || isNull b = nullPoly
          | otherwise = let !((lta,qa),taila) = deleteFindLT a
                            !innerFoil = monMult lta qa b
                        in innerFoil + (taila * b)
    negate (P a) = P $! fmap (\(m,q) -> (m,-q)) a
    fromInteger 0 = nullPoly
    fromInteger 1 = monPoly Constant 1

-- | Just adds a monomial without turning it into a polynomial first
monAdd :: Ord (Mon ord) => Mon ord -> Q -> Poly ord -> Poly ord
monAdd mon coef poly = monPoly mon coef + poly

-- | Scales every term of a Polynomial by a Mon list and rational number.
monMult :: Mon ord -> Q -> Poly ord -> Poly ord
monMult mon coef (P poly) = P $! DL.map (\(m,q) -> (multiply mon m, q*coef)) poly

scalePoly :: Q -> Poly ord -> Poly ord
scalePoly 0 _ = nullPoly
scalePoly coef (P poly) = P $! DL.map (\(m,q) -> (m,q*coef)) poly 

-- | Divides the first polynomial by the second repeatedly until it fails.
quoRem :: (Ord (Mon ord)) =>
           Poly ord -> (Poly ord,Sugar ord) -> (Poly ord, Poly ord)
quoRem a (P [],_) = (a,nullPoly)
quoRem a (b,_) = quoRem' a b nullPoly where
  quoRem' rem d quo | isNull rem = (quo, nullPoly)
                    | otherwise = let !(lta,qa) = leadTerm rem
                                      !(ltb,qb) = leadTerm d
                                      !aOb = divide lta ltb
                                      aObco = qa/qb
                                      newRem = rem - monMult aOb aObco d
                                      newQuo = monAdd aOb (-aObco) quo
                                  in if isMon aOb then
                                         quoRem' newRem d newQuo
                                     else
                                         (quo, rem)