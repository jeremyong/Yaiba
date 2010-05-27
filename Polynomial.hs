{-# OPTIONS_GHC -fglasgow-exts #-}

module Yaiba.Polynomial where

import qualified Data.Heap as H; import Data.Heap
import Yaiba.Monomial

newtype Polynomial ord = Polynomial (MaxHeap (Monomial ord))

instance Show (Polynomial Lex) where
  show (Polynomial a) = showTerm $ H.toAscList (a::MaxHeap (Monomial Lex))
    where showTerm [] = ""
          showTerm (a:[]) = show a 
          showTerm (a:as) = (show a) ++ " + " ++ (showTerm as)
