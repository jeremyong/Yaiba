{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}

import Control.Parallel
import Control.Parallel.Strategies
import Data.List
import Prelude hiding (gcd)
import System.IO
import GHC.Conc (numCapabilities)

class Cluster c where
  singleton :: c a -> c (c a)
  cluster   :: Int -> c a -> c (c a)
  decluster :: c (c a) -> c a
  lift      :: (c a -> a) -> (c (c a) -> c a)

instance Cluster [] where
  singleton list       = [list]
  cluster   n []       = []
  cluster   n list     = foldl split (replicate n []) list where
                           split (b:bs) a = bs ++ a:b
  decluster buckets    = concat buckets
  lift      f          = map f

mkList :: Int -> [Int]
mkList n = [1..(n-1)]

gcd:: Int -> Int -> Int
gcd x 0 = x
gcd x y = gcd y (rem x y)

relprime :: Int -> Int -> Bool
relprime x y = gcd x y == 1

euler :: Int -> Int
euler n = length (filter (relprime n) (mkList n))

sumEuler :: Int -> Int -> Int
sumEuler z n = sum ((lift worker) (cluster z (mkList n)) `using` parList rwhnf) where
                  worker = sum . map euler

sumEuler2 :: Int -> Int -> Int
sumEuler2 c n = sum (map euler (mkList n) `using` parListChunk c rwhnf )

main = do
   let a = 20000
--   let se = show $ sumEuler2 (quot a numCapabilities) a
   let se = show $ sumEuler (quot a numCapabilities) a
   putStrLn(se)