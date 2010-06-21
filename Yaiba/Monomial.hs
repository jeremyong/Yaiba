{-# OPTIONS_GHC -fglasgow-exts -XBangPatterns #-}
-- | At present, Mon ord is a list of integers (the multidegree of a monomial).
-- It is not checked that the lists are of the same length when initialized.
-- After initialization, all Mon lists are mostly-guaranteed to have the same length.
module Yaiba.Monomial where

import Yaiba.Base
import qualified Data.List as DL
import qualified Data.Vector.Unboxed as DVU

data Mon ord = M (DVU.Vector Int) | Constant

-- | Dummy phantom ord types. Requires -fglasgow-exts enabled.
data Lex
data Grlex
data Grevlex

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

instance Ord (Mon Lex) where
  compare x y = lexCompare x y

instance Ord (Mon Grlex) where
  compare x y = case compare (degree x) (degree y) of
    GT -> GT
    LT -> LT
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

gcdMon :: Mon ord -> Mon ord -> Mon ord
gcdMon Constant Constant = Constant
gcdMon Constant (M _)    = Constant
gcdMon (M _) Constant    = Constant
gcdMon (M as) (M bs)     = let !cs = DVU.zipWith min as bs
                           in case DVU.all (==0) cs of
                                True -> Constant
                                False -> M cs
