{-# OPTIONS_GHC -fglasgow-exts #-}
-- | At present, Mon ord is a list of integers (the multidegree of a monomial).
-- It is not checked that the lists are of the same length when initialized.
-- After initialization, all Mon lists are mostly-guaranteed to have the same length.
module Yaiba.Monomial where

import Math.Algebra.Field.Base
import Data.List

data Mon ord = M [Int] | Constant

instance Eq (Mon ord) where
  a == b = all (==0) (powerList $ a/b)

instance Show (Mon ord) where
  show (M a) | filter (/=0) a == [] = " "
             | otherwise = showVar (1::Int) a where 
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
  Constant * Constant = Constant
  Constant * M as = (M as)
  M as * Constant = (M as)
  M as * M bs = M $ zipWith (+) as bs
  
degree :: Mon ord -> Int
degree Constant = 0
degree (M a) = sum a
  
instance Fractional (Mon ord) where
  recip Constant = Constant
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
isFactor Constant Constant = True
isFactor Constant (M as)   = True
isFactor (M as) Constant   = False
isFactor a b               = all (>=0) cs where
                                (M cs) = b/a

lcmMon :: Mon t -> Mon t1 -> Mon ord
lcmMon Constant Constant = Constant
lcmMon Constant (M a)    = (M a)
lcmMon (M a) Constant    = (M a)
lcmMon (M a) (M b)       = M $ zipWith max a b

gcdMon :: Mon t -> Mon t1 -> Mon ord
gcdMon Constant Constant = Constant
gcdMon Constant (M a)    = Constant
gcdMon (M a) Constant    = Constant
gcdMon (M a) (M b)       = M $ zipWith max a b
