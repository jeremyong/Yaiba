{-# OPTIONS_GHC -fglasgow-exts #-}

module Yaiba.Monomial where

import Math.Algebra.Field.Base

newtype Monomial ord = Monomial (Q, [Int])
                                               
unMon (Monomial (a,b)) = b

instance Eq (Monomial ord) where
  Monomial a == Monomial b = a==b

instance Show (Monomial ord) where
  show (Monomial (a,b)) = show a ++ showVar 1 b
    where showVar _ [] = ""
          showVar k (y:[]) | y<0 = "x_" ++ (show k) ++ "^(" ++ (show y) ++ ")"
                           | y==0 = ""
                           | y==1 = "x_" ++ (show k)
                           | otherwise = "x_" ++ (show k) ++ "^" ++ (show y)
          showVar k (y:ys) | y<0 = "x_" ++ (show k) ++ "^(" ++ (show y) ++ ")" ++" * "++ showVar (k+1) ys
                           | y==0 = showVar (k+1) ys
                           | y==1 = "x_"++(show k)++" * "++showVar (k+1) ys
                           | otherwise = "x_"++(show k)++"^"++(show y)++" * "++showVar (k+1) ys
          
data Lex
data Grlex
data Grevlex
data Revlex

--Monomials behave like numbers. You can add them as long as they are like; addition
-- returns 0 if they are not. They multiply as expected. Constants are represented as
-- a tuple with the constant and an empty list. The abs function returns the degree
-- of the monomial as a constant monomial. Signum returns one if all powers are at
-- least zero and negative one otherwise.
instance Num (Monomial ord) where
  Monomial (a,as) + Monomial (b,bs) = if as==bs then Monomial (a+b, as) else 0
  Monomial (a,as) * Monomial (b,bs) = Monomial (a*b, zipWith (+) as bs)
  fromInteger 1 = Monomial (1, [])
  abs (Monomial (a,as)) = Monomial (Q (toRational (sum as)), [])
  signum (Monomial (_,[])) = 1
  signum (Monomial (q,(a:as))) | a>=0 = signum (Monomial (q,as))
                               | otherwise = -1

instance Fractional (Monomial ord) where
  recip (Monomial (a,as)) = Monomial (a, map negate as)
  fromRational 1 = fromInteger 1

instance Ord (Monomial Lex) where
  compare x@(Monomial (a,as)) y@(Monomial (b,bs)) = headCompare (unMon (x/y)) where
    headCompare (a:[]) | a>0 = GT
                       | a<0 = LT
                       | a==0 = EQ
    headCompare (a:as) | a>0 = GT
                       | a<0 = LT
                       | a==0 = headCompare as