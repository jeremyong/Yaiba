{-# OPTIONS_GHC -fglasgow-exts #-}
-- | At present, Mon ord is a list of integers (the multidegree of a monomial).
-- It is not checked that the lists are of the same length when initialized.
-- After initialization, all Mon lists are mostly-guaranteed to have the same length.
module Yaiba.Monomial where

import Math.Algebra.Field.Base

newtype Mon ord = M [Int]

instance Eq (Mon ord) where
  M as == M bs = as==bs

instance Show (Mon ord) where
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

instance Num (Mon ord) where
  M as * M bs = M $ zipWith (+) as bs
  
degree :: Mon ord -> Int
degree (M a) = sum a
  
signs :: Mon ord -> Bool
signs (M as) = all (>=0) as

instance Fractional (Mon ord) where
  recip (M as) = M $ map negate as
  
instance Ord (Mon Lex) where
  compare x y = headCompare (powerList (x/y))

headCompare :: (Num t, Ord t) => [t] -> Ordering
headCompare [] = EQ
headCompare (a:[]) | a>0 = GT
                   | a<0 = LT
                   | a==0 = EQ
headCompare (a:as) | a>0 = GT
                   | a<0 = LT
                   | a==0 = headCompare as
                                
instance Ord (Mon Grlex) where
  compare x y = case compare (degree x) (degree y) of
    GT -> GT
    LT -> LT
    EQ -> headCompare (powerList (x/y))
                     
powerList :: Mon t -> [Int]
powerList (M b) = b

isFactor :: Mon ord -> Mon ord -> Bool
isFactor a b = signs (b/a)

lcmMon :: Mon t -> Mon t1 -> Mon ord
lcmMon (M a) (M b) = M $ zipWith (max) a b