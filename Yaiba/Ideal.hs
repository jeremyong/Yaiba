
{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XPArr #-}

module Yaiba.Ideal where

--import Data.Array.Repa as R
--import Data.Array.Repa.Index
--import Data.Array.Repa.Shape
--import Data.Array.Parallel.Unlifted as U
import Control.Concurrent
import Yaiba.Polynomial
import Data.List

newtype Ideal ord = Ideal [Polynomial ord] deriving (Eq, Show)
{-
getPairs (Ideal as) = comb 2 as where
  comb 0 _ = [[]]
  comb n xs = [ y:ys | y:xs' <- tails xs, ys <- getPairs (n-1) xs' ]
  -}
--Division algorithm (outputs remainder)
(/.) r (Ideal []) = r
(/.) d (Ideal ds) = divIdeal d ds False where
  divIdeal a [] divOcc = if divOcc == True then (/.) a (Ideal ds) else a
  divIdeal a (b:bs) divOcc = if x == nullPoly then 
                               divIdeal a bs divOcc
                             else divIdeal x bs True where
                               x = snd (quoRem a b)