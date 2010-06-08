{-# OPTIONS_GHC -fglasgow-exts #-}

module Yaiba.Monomial where

import Math.Algebra.Field.Base

newtype Monomial ord = Monomial [Int]

instance Eq (Monomial ord) where
  Monomial as == Monomial bs = as==bs

instance Show (Monomial ord) where
  show (Monomial a) | filter (/=0) a == [] = " "
                    | otherwise = showVar (1::Int) a
                        where showVar _ [] = ""
                              showVar k (y:[]) | y<0 = "*x_" ++ (show k) ++ "^(" ++ (show y) ++ ")"
                                               | y==0 = ""
                                               | y==1 = "*x_" ++ (show k)
                                               | otherwise = "*x_" ++ (show k) ++ "^(" ++ (show y) ++ ")"
                              showVar k (y:ys) | y<0 = "*x_" ++ (show k) ++ "^(" ++ (show y) ++ ")" ++ showVar (k+1) ys
                                               | y==0 = showVar (k+1) ys
                                               | y==1 = "*x_"++(show k)++showVar (k+1) ys
                                               | otherwise = "*x_"++(show k)++"^("++(show y)++")"++showVar (k+1) ys

--Dummy phantom ord types. Requires -fglasgow-exts enabled.
data Lex
data Grlex
data Grevlex
data Revlex

-- Signum returns one if all powers are at least zero and negative one otherwise.
instance Num (Monomial ord) where
  a + b = Monomial [0]
  Monomial as * Monomial bs = Monomial $ zipWith (+) as bs
  fromInteger 1 = Monomial [0]
  signum (Monomial []) = Monomial []
  
signs :: Monomial ord -> Int
signs (Monomial []) = 1
signs (Monomial (a:as)) | a>=0 = signs (Monomial as)
                        | otherwise = -1

instance Fractional (Monomial ord) where
  recip (Monomial as) = Monomial $ map negate as
  fromRational 1 = fromInteger 1
  
instance Ord (Monomial Lex) where
  compare x y = headCompare (powerList (x/y)) where
    headCompare [] = EQ
    headCompare (a:[]) | a>0 = GT
                       | a<0 = LT
                       | a==0 = EQ
    headCompare (a:as) | a>0 = GT
                       | a<0 = LT
                       | a==0 = headCompare as
                                
powerList :: Monomial t -> [Int]
powerList (Monomial b) = b

isFactor :: Monomial ord -> Monomial ord -> Bool
isFactor a b = signum (b/a) == 1