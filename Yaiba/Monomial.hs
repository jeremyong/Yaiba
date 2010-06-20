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
  Constant == Constant = True
  Constant == (M as)   = False
  (M as) == Constant   = False  
  (M as) == (M bs)     = DVU.and (DVU.zipWith (==) as bs)

instance Show (Mon ord) where
  show Constant = " "
  show (M a) | DVU.filter (/=0) a == DVU.empty = " "
             | otherwise = let multVars = fst $ DVU.foldl (\(str,n) a -> (str ++ (showVar n a),n+1)) ("",1) a
                           in take (length multVars - 1) multVars

instance Ord (Mon Lex) where
  compare x y = lexCompare x y

instance Ord (Mon Grlex) where
  compare x y = case compare (degree x) (degree y) of
    GT -> GT
    LT -> LT
    EQ -> lexCompare x y

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
            | a==1      = "x_" ++ show n ++ "*"
            | a>0&&a<10 = "x_" ++ show n ++ "^" ++ show a ++ "*"
            | otherwise = "x_" ++ show n ++ "^(" ++ show a ++ ")*"

lexCompare :: (Mon ord) -> (Mon ord) -> Ordering
lexCompare (M as) (M bs) = let ha = DVU.unsafeHead as
                               ta = DVU.tail as
                               hb = DVU.unsafeHead bs
                               tb = DVU.tail bs
                           in if as == DVU.empty then
                                  EQ
                              else case compare ha hb of 
                                     GT -> GT
                                     LT -> LT
                                     EQ -> lexCompare (M ta) (M tb)
                                
degree :: Mon ord -> Int
degree Constant = 0
degree (M a) = DVU.sum a
  
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
