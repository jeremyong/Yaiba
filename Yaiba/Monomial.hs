{-# OPTIONS_GHC -fglasgow-exts #-}
-- | At present, Mon ord is a list of integers (the multidegree of a monomial).
-- It is not checked that the lists are of the same length when initialized.
-- After initialization, all Mon lists are mostly-guaranteed to have the same length.
module Yaiba.Monomial where

import Math.Algebra.Field.Base
import Data.List
import Data.Array.Unboxed

data Mon ord = M (UArray Int Int) | Constant

instance Eq (Mon ord) where
  a == b = all (==0) (powerList $ a/b)

instance Show (Mon ord) where
  show (M a) | filter (/=0) (elems a) == [] = " "
             | otherwise = showVar (1::Int) (elems a) where 
                 showVar _ [] = ""
                 showVar k (y:[]) | y<0 = "*x_" ++ show k ++ "^(" ++ show y ++ ")"
                                  | y==0 = ""
                                  | y==1 = "*x_" ++ show k
                                  | otherwise = "*x_" ++ show k ++ "^(" ++ show y ++ ")"
                 showVar k (y:ys) | y<0 = "*x_" ++ show k ++ "^(" ++ show y ++ ")" ++ showVar (k+1) ys
                                  | y==0 = showVar (k+1) ys
                                  | y==1 = "*x_"++ show k ++showVar (k+1) ys
                                  | otherwise = "*x_"++ show k ++"^("++ show y ++")"++showVar (k+1) ys

--Dummy phantom ord types. Requires -fglasgow-exts enabled.
data Lex
data Grlex
data Grevlex
data Revlex

instance Num (Mon ord) where
  a * Constant = a
  Constant * a = a
  M as * M bs = let numVars = rangeSize (bounds as) - 1
                in M $ array (0,numVars) [(z,as!z + bs!z) | z <- [0..(numVars)]]
  fromInteger a = Constant

degree :: Mon ord -> Int
degree Constant = 0
degree (M a) = sum [ a!z | z <- indices a ]
  
signs :: Mon ord -> Bool
signs (M as) = all (>=0) [ as!z | z <- indices as ]

instance Fractional (Mon ord) where
  recip (M as) = M $ array (bounds as) [ (z,negate (as!z)) | z <- indices as ]
  fromRational a = Constant
  
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
powerList (M b) = [b!z|z<-indices b]

isFactor :: Mon ord -> Mon ord -> Bool
isFactor a b = signs (b/a)

lcmMon :: Mon t -> Mon t1 -> Mon ord
lcmMon (M a) (M b) = M $ array (bounds a) [(z,max (a!z) (b!z)) | z <- indices a ]