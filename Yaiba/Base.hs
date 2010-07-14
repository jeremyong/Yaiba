-- Copyright (c) David Amos, 2008. All rights reserved.

{-# OPTIONS_GHC -fglasgow-exts #-}

module Yaiba.Base where

import Data.Ratio
--import Math.Common.IntegerAsType


-- RATIONALS

-- |Q is just the rationals, but with a better show function than the Prelude version
newtype Q = Q Rational deriving (Eq,Ord,Num,Fractional)

instance Show Q where
    show (Q x) | b == 1    = show a
               | otherwise = show a ++ "/" ++ show b
               where a = numerator x
                     b = denominator x

numeratorQ (Q x) = Data.Ratio.numerator x
denominatorQ (Q x) = Data.Ratio.denominator x

-- Finite Fields

{-

newtype F101 = F Int deriving (Eq,Show)

instance Num F101 where
  F a + F b = F $ let c = a + b in
                  case compare c 101 of
                    LT -> c
                    GT -> c - 101
                    EQ -> 0
  F a * F b = F $ a * b `mod` 101
  negate F a = F $ 101 - a

instance 

inject :: (Num a) => a -> F101
inject n = F $ n `mod` 101

-- returns (u,v,d) where u*p+v*q = d
extendedEuclid :: (Integral a) => a -> a -> (a,a,a)
extendedEuclid a b | a >= 0 && b >= 0 = extendedEuclid' a b [] where
    extendedEuclid' d 0 qs = let (u,v) = unwind 1 0 qs in (u,v,d)
    extendedEuclid' a b qs = let (q,r) = quotRem a b in extendedEuclid' b r (q:qs)
    unwind u v [] = (u,v)
    unwind u v (q:qs) = unwind v (u-v*q) qs




newtype M s a = M a deriving (Eq, Show)

unM :: M s a -> a
unM (M a) = a

class Modular s a | s -> a where modulus :: s -> a

normalize :: forall s a. (Modular s a, Integral a) => a -> M s a
normalize a = (M $ a `mod` (modulus (undefined :: s)))

instance (Modular s a, Integral a) => Num (M s a) where
  M a + M b     = normalize (a+b)
  M a - M b     = normalize (a-b)
  M a * M b     = normalize (a*b)
  negate (M a)  = normalize (negate a)
  fromInteger i = normalize (fromInteger i)  
  signum        = error "Modular numbers are not signed"
  abs           = error "Modular numbers are not signed"

data Zero; data Twice s; data Succ s

class ReflectNum s where reflectNum :: Num a => s -> a
instance ReflectNum Zero where reflectNum _ = 0
instance ReflectNum s => ReflectNum (Twice s) where reflectNum _ = reflectNum (undefined :: s) * 2
instance ReflectNum s => ReflectNum (Succ s)  where reflectNum _ = reflectNum (undefined :: s) + 1

reifyIntegral :: Integral a => a -> (forall s. ReflectNum s => s -> w) -> w
reifyIntegral i k = case quotRem i 2 of
  (0, 0) -> k (undefined :: Zero)
  (j, 0) -> reifyIntegral j (\(_::s) -> k (undefined :: Twice s))
  (j, 1) -> reifyIntegral j (\(_::s) -> k (undefined :: Succ (Twice s)))

data ModulusNum t a
instance (ReflectNum t, Num a) => Modular (ModulusNum t a) a where modulus _ = reflectNum (undefined :: t)

withIntegralModulus :: Integral a => a -> (forall s. Modular s a => s -> w) -> w
withIntegralModulus i k = reifyIntegral i (\(_::t) -> k (undefined :: ModulusNum t a))

inject :: forall s a. (Integral a) => a -> M s a
inject a = M a :: M s a

test3' :: (Modular s a, Integral a) => M s a -> M s a -> s -> M s a
test3' x y _ = x*x + y*y

tester :: (Modular s a, Integral a) => s -> M s a
tester _ = 5

junk = withIntegralModulus 101 (unM . tester)
--junk = withIntegralModulus 101 tester

myX = inject 3
myY = inject 10
myPrime1 = 101
myPrime2 = 17
--data FF101
--instance Modular FF101 Int where modulus _ = myPrime

test3a = withIntegralModulus myPrime1 (unM . (test3' myX myY))
test3b = withIntegralModulus myPrime2 (unM . (test3' myX myY))
--test3 = withIntegralModulus 101 (test3' 3 10)

-- extendedEuclid a b returns (u,v,d) such that u*a + v*b = d
extendedEuclid a b | a >= 0 && b >= 0 = extendedEuclid' a b [] where
    extendedEuclid' d 0 qs = let (u,v) = unwind 1 0 qs in (u,v,d)
    extendedEuclid' a b qs = let (q,r) = quotRem a b in extendedEuclid' b r (q:qs)
    unwind u v [] = (u,v)
    unwind u v (q:qs) = unwind v (u-v*q) qs

newtype Fp n = Fp Integer deriving (Eq,Ord)

instance Show (Fp n) where
    show (Fp x) = show x

instance IntegerAsType n => Num (Fp n) where
    Fp x + Fp y = Fp $ (x+y) `mod` p where p = value (undefined :: n)
    negate (Fp 0) = Fp 0
    negate (Fp x) = Fp $ p - x where p = value (undefined :: n)
    Fp x * Fp y = Fp $ (x*y) `mod` p where p = value (undefined :: n)
    fromInteger m = Fp $ m `mod` p where p = value (undefined :: n)

-- n must be prime - could perhaps use a type to guarantee this
instance IntegerAsType n => Fractional (Fp n) where
    recip 0 = error "Fp.recip 0"
    recip (Fp x) = let (u,v,1) = extendedEuclid x p -- so ux+vp = 1. (We know the gcd is 1 as p prime)
                   in Fp $ u `mod` p
                   where p = value (undefined :: n)

class Fractional fq => FiniteField fq where
    eltsFq :: fq -> [fq]  -- return all elts of the field
    basisFq :: fq -> [fq] -- return an additive basis for the field (as Z-module)

instance IntegerAsType p => FiniteField (Fp p) where
    eltsFq _ = map fromInteger [0..p'-1] where p' = value (undefined :: p)
    basisFq _ = [fromInteger 1]

primitiveElt fq = head [x | x <- tail fq, length (powers x) == q-1] where
    q = length fq

powers x | x /= 0 = 1 : takeWhile (/=1) (iterate (*x) x)


-- characteristic of a finite field
char fq = head [p | p <- [2..], length fq `mod` p == 0]


-- |F2 is a type for the finite field with 2 elements
type F2 = Fp T2

-- |f2 lists the elements of F2
f2 :: [F2]
f2 = map fromInteger [0..1] -- :: [F2]

-- |F3 is a type for the finite field with 3 elements
type F3 = Fp T3

-- |f3 lists the elements of F3
f3 :: [F3]
f3 = map fromInteger [0..2] -- :: [F3]

-- |F5 is a type for the finite field with 5 elements
type F5 = Fp T5

-- |f5 lists the elements of F5
f5 :: [F5]
f5 = map fromInteger [0..4] -- :: [F5]

-- |F7 is a type for the finite field with 7 elements
type F7 = Fp T7

-- |f7 lists the elements of F7
f7 :: [F7]
f7 = map fromInteger [0..6] -- :: [F7]

type F11 = Fp T11
f11 = map fromInteger [0..10] :: [F11]

type F13 = Fp T13
f13 = map fromInteger [0..12] :: [F13]

type F17 = Fp T17
f17 = map fromInteger [0..16] :: [F17]

type F19 = Fp T19
f19 = map fromInteger [0..18] :: [F19]

type F23 = Fp T23
f23 = map fromInteger [0..22] :: [F23]

type F29 = Fp T29
f29 = map fromInteger [0..28] :: [F29]

type F31 = Fp T31
f31 = map fromInteger [0..30] :: [F31]

type F37 = Fp T37
f37 = map fromInteger [0..36] :: [F37]

type F41 = Fp T41
f41 = map fromInteger [0..40] :: [F41]

type F43 = Fp T43
f43 = map fromInteger [0..42] :: [F43]

type F47 = Fp T47
f47 = map fromInteger [0..46] :: [F47]

type F53 = Fp T53
f53 = map fromInteger [0..52] :: [F53]

type F59 = Fp T59
f59 = map fromInteger [0..58] :: [F59]

type F61 = Fp T61
f61 = map fromInteger [0..60] :: [F61]

type F67 = Fp T67
f67 = map fromInteger [0..66] :: [F67]

type F71 = Fp T71
f71 = map fromInteger [0..70] :: [F71]

type F73 = Fp T73
f73 = map fromInteger [0..72] :: [F73]

type F79 = Fp T79
f79 = map fromInteger [0..78] :: [F79]

type F83 = Fp T83
f83 = map fromInteger [0..82] :: [F83]

type F89 = Fp T89
f89 = map fromInteger [0..88] :: [F89]

type F97 = Fp T97
f97 = map fromInteger [0..96] :: [F97]

-}