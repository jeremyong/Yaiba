{-# OPTIONS_GHC -fglasgow-exts #-}
-- | At present, Mon ord is a list of integers (the multidegree of a monomial).
-- It is not checked that the lists are of the same length when initialized.
-- After initialization, all Mon lists are mostly-guaranteed to have the same length.
module Yaiba.Monomial where

import Math.Algebra.Field.Base
import qualified Data.List as DL
import qualified Data.Vector.Unboxed as DVU

data Mon ord = M (DVU.Vector Int) | Constant

--Dummy phantom ord types. Requires -fglasgow-exts enabled.
data Lex
data Grlex
data Grevlex
data Revlex

instance Eq (Mon ord) where
  a == b = DVU.all (==0) (powerList $ divide a b)

instance Show (Mon ord) where
  show Constant = " "
  show (M a) | DVU.filter (/=0) a == DVU.empty = " "
             | otherwise = let multVars = fst $ DVU.foldl (\acc@(str,n) a -> (str ++ (showVar n a),n+1)) ("",1) a
                           in take (length multVars - 1) multVars

--instance Num (Mon ord) where
--  Constant * Constant = Constant
--  Constant * M as = (M as)
--  M as * Constant = (M as)
--  M as * M bs = M $ DVU.zipWith (+) as bs
  
--instance Fractional (Mon ord) where
--  recip Constant = Constant
--  recip (M as) = M $ DVU.map negate as

instance Ord (Mon Lex) where
  compare x y = lexCompare (powerList (divide x y))

instance Ord (Mon Grlex) where
  compare x y = case compare (degree x) (degree y) of
    GT -> GT
    LT -> LT
    EQ -> lexCompare (powerList (divide x y))

--instance Num (Mon ord) where
--  Constant * Constant = Constant
--  Constant * M as = (M as)
--  M as * Constant = (M as)
--  M as * M bs = M $ DVU.zipWith (+) as bs
  
--instance Fractional (Mon ord) where
--  recip Constant = Constant
--  recip (M as) = M $ DVU.map negate as

multiply :: (Mon ord) -> (Mon ord) -> (Mon ord)
multiply Constant Constant = Constant
multiply Constant (M as) = M as
multiply (M as) Constant = M as
multiply (M as) (M bs) = M $ DVU.zipWith (+) as bs

divide :: (Mon ord) -> (Mon ord) -> (Mon ord)
divide Constant Constant = Constant
divide Constant (M as) = M $ DVU.map negate as
divide (M as) Constant = M as
divide (M as) (M bs) = M $ DVU.zipWith (-) as bs

showVar :: Int -> Int -> String
showVar n a | a==0      = ""
            | a>0&&a<10 = "x_" ++ show n ++ "^" ++ show a ++ "*"
            | otherwise = "x_" ++ show n ++ "^(" ++ show a ++ ")*"

lexCompare :: (DVU.Vector Int) -> Ordering
lexCompare a = let h = DVU.head a
                   t = DVU.tail a
               in if a == DVU.empty then
                      EQ
                  else case compare h 0 of 
                         GT -> GT
                         LT -> LT
                         EQ -> lexCompare t
                                
degree :: Mon ord -> Int
degree Constant = 0
degree (M a) = DVU.sum a
  
powerList :: Mon ord -> (DVU.Vector Int)
powerList (M b) = b

isFactor :: Mon ord -> Mon ord -> Bool
isFactor Constant Constant = True
isFactor Constant (M _)    = True
isFactor (M _) Constant    = False
isFactor a b               = DVU.all (>=0) cs where
                                (M cs) = divide b a

lcmMon :: Mon ord -> Mon ord -> Mon ord
lcmMon Constant Constant = Constant
lcmMon Constant (M a)    = (M a)
lcmMon (M a) Constant    = (M a)
lcmMon (M a) (M b)       = M $ DVU.zipWith max a b

gcdMon :: Mon ord -> Mon ord -> Mon ord
gcdMon Constant Constant = Constant
gcdMon Constant (M _)    = Constant
gcdMon (M _) Constant    = Constant
gcdMon (M a) (M b)       = M $ DVU.zipWith min a b
