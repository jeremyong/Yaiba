{-# OPTIONS_GHC -fglasgow-exts -XBangPatterns #-}
-- | At present, Mon ord is a list of integers (the multidegree of a monomial).
-- It is not checked that the lists are of the same length when initialized.
-- After initialization, all Mon lists are mostly-guaranteed to have the same length.
module Yaiba.Monomial where

import Math.Algebra.Field.Base
<<<<<<< HEAD
import qualified Data.List as DL
import qualified Data.Vector.Unboxed as DVU

data Mon ord = M (DVU.Vector Int) | Constant

-- | Dummy phantom ord types. Requires -fglasgow-exts enabled.
=======
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
>>>>>>> 84616b6a7d42d40d4021be969ec904cc8b6984ed
data Lex
data Grlex
data Grevlex

<<<<<<< HEAD
instance Eq (Mon ord) where
  Constant == Constant = True
  Constant == (M as)   = False
  (M as) == Constant   = False  
  (M as) == (M bs)     = DVU.and (DVU.zipWith (==) as bs)

instance Show (Mon ord) where
  show Constant = " "
  show (M a) | DVU.filter (/=0) a == DVU.empty = " "
             | otherwise = let multVars = fst $ DVU.foldl (\(str,n) b -> (str ++ (showVar n b),n+1)) ("",1) a
                           in take (length multVars - 1) multVars

=======
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
  
>>>>>>> 84616b6a7d42d40d4021be969ec904cc8b6984ed
instance Ord (Mon Lex) where
  compare x y = lexCompare x y

instance Ord (Mon Grlex) where
  compare x y = case compare (degree x) (degree y) of
    GT -> GT
    LT -> LT
<<<<<<< HEAD
    EQ -> lexCompare x y

instance Ord (Mon Grevlex) where
  compare x y = case compare (degree x) (degree y) of
    GT -> GT
    LT -> LT
    EQ -> grevlexCompare x y

fromList :: [Int] -> Mon ord
fromList ds = M (DVU.fromList ds :: DVU.Vector Int)

multiply :: (Mon ord) -> (Mon ord) -> (Mon ord)
multiply Constant Constant = Constant
multiply Constant (M as)   = M as
multiply (M as) Constant   = M as
multiply (M as) (M bs)     = M $ DVU.zipWith (+) as bs
-- definition to make Constant work right - much slower
--multiply (M as) (M bs)     = let cs = DVU.zipWith (+) as bs
--                             in if DVU.any (/=0) cs then
--                                    M cs
--                                else
--                                    Constant

divide :: (Mon ord) -> (Mon ord) -> (Mon ord)
divide Constant Constant = Constant
divide Constant (M as)   = M $ DVU.map negate as
divide (M as) Constant   = M as
divide (M as) (M bs)     = M $ DVU.zipWith (-) as bs
-- definition to make Constant work right - much slower
--divide (M as) (M bs)     = let cs = DVU.zipWith (-) as bs
--                             in if DVU.any (/=0) cs then
--                                    M cs
--                                else
--                                    Constant
=======
    EQ -> headCompare (powerList (x/y))
                     
powerList :: Mon t -> [Int]
powerList (M b) = [b!z|z<-indices b]
>>>>>>> 84616b6a7d42d40d4021be969ec904cc8b6984ed

showVar :: Int -> Int -> String
showVar n a | a==0      = ""
            | a==1      = "x_" ++ show n ++ "*"
            | a>0&&a<10 = "x_" ++ show n ++ "^" ++ show a ++ "*"
            | otherwise = "x_" ++ show n ++ "^(" ++ show a ++ ")*"

maybeHead :: (Eq a, DVU.Unbox a) => (DVU.Vector a) -> Maybe a
maybeHead as = if as == DVU.empty then Nothing else Just $ DVU.unsafeHead as

maybeLast :: (Eq a, DVU.Unbox a) => (DVU.Vector a) -> Maybe a
maybeLast as = if as == DVU.empty then Nothing else Just $ DVU.unsafeLast as

lexCompare :: (Mon ord) -> (Mon ord) -> Ordering
lexCompare (M as) (M bs) = let !a = maybeHead $ DVU.filter (/=0) (DVU.zipWith (-) as bs)
                           in case a of
                                Nothing -> EQ
                                (Just a) -> if a > 0 then GT else LT
                           
grevlexCompare :: (Mon ord) -> (Mon ord) -> Ordering
grevlexCompare (M as) (M bs) = let !a = maybeLast $ DVU.filter (/=0) (DVU.zipWith (-) as bs)
                               in case a of
                                    Nothing -> EQ
                                    (Just a) -> if a < 0 then GT else LT
                           
degree :: Mon ord -> Int
degree Constant = 0
degree (M a) = DVU.sum a
  
isFactor :: Mon ord -> Mon ord -> Bool
isFactor Constant Constant = True
isFactor Constant (M _)    = True
isFactor (M _) Constant    = False
isFactor a b               = let !mon = divide b a
                             in case mon of
                                  Constant -> True
                                  (M cs) -> DVU.all (>=0) cs

lcmMon :: Mon ord -> Mon ord -> Mon ord
lcmMon Constant Constant = Constant
lcmMon Constant (M as)   = (M as)
lcmMon (M as) Constant   = (M as)
lcmMon (M as) (M bs)     = M $ DVU.zipWith max as bs

<<<<<<< HEAD
gcdMon :: Mon ord -> Mon ord -> Mon ord
gcdMon Constant Constant = Constant
gcdMon Constant (M _)    = Constant
gcdMon (M _) Constant    = Constant
gcdMon (M as) (M bs)     = let !cs = DVU.zipWith min as bs
                           in case DVU.all (==0) cs of
                                True -> Constant
                                False -> M cs
=======
lcmMon :: Mon t -> Mon t1 -> Mon ord
lcmMon (M a) (M b) = M $ array (bounds a) [(z,max (a!z) (b!z)) | z <- indices a ]
>>>>>>> 84616b6a7d42d40d4021be969ec904cc8b6984ed
