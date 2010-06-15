{-# OPTIONS_GHC -fglasgow-exts #-}

module Yaiba.Monomial where

import Math.Algebra.Field.Base

newtype Monomial ord = M [Int]

instance Eq (Monomial ord) where
  M as == M bs = as==bs

instance Show (Monomial ord) where
  show (M a) | filter (/=0) a == [] = " "
             | otherwise = showVar (1::Int) a where 
                 showVar _ [] = ""
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
  a + b = M [0]
  M as * M bs = M $ zipWith (+) as bs
  fromInteger _ = M [0] --Don't use
  signum _ = M [0] --Don't use
  abs _ = M [0] --Don't use
  
degree :: Monomial ord -> Int
degree (M a) = sum a
  
signs :: Monomial ord -> Int
signs (M as) = case any (<0) as of
  True -> -1
  False -> 1

instance Fractional (Monomial ord) where
  recip (M as) = M $ map negate as
  fromRational 1 = fromInteger 1
  
instance Ord (Monomial Lex) where
  compare x y = headCompare (powerList (x/y))

headCompare :: (Num t, Ord t) => [t] -> Ordering
headCompare [] = EQ
headCompare (a:[]) | a>0 = GT
                   | a<0 = LT
                   | a==0 = EQ
headCompare (a:as) | a>0 = GT
                   | a<0 = LT
                   | a==0 = headCompare as
                                
instance Ord (Monomial Grlex) where
  compare x y = case compare (degree x) (degree y) of
    GT -> GT
    LT -> LT
    EQ -> headCompare (powerList (x/y))
                     
powerList :: Monomial t -> [Int]
powerList (M b) = b

isFactor :: Monomial ord -> Monomial ord -> Bool
isFactor a b = signs (b/a) == 1

lcmMon :: Monomial t -> Monomial t1 -> Monomial ord
lcmMon (M a) (M b) = M $ zipWith (max) a b