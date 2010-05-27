{-# OPTIONS_GHC -fglasgow-exts #-}

module Yaiba.Polynomial where

import Data.Heap
import Yaiba.Monomial

newtype Polynomial ord = Polynomial (MaxHeap (Monomial ord))

instance Show (Polynomial Lex) where
  show (Polynomial a) = showTerm $ toAscList (a::MaxHeap (Monomial Lex))
    where showTerm [] = ""
          showTerm (a:[]) = show a 
          showTerm (a:as) = (show a) ++ " + " ++ (showTerm as)
          
nullPoly = Polynomial empty
          
--Don't use, use "compare" instead
instance Eq (Polynomial ord) where
           
instance Ord (Polynomial Lex) where
  compare (Polynomial a) (Polynomial b) | isEmpty a && isEmpty b = EQ
                                        | isEmpty a = LT
                                        | isEmpty b = GT
                                        | top == EQ = compare ((Polynomial (tail a))::Polynomial Lex) ((Polynomial (tail b))::Polynomial Lex)
                                        | otherwise = top where
    top = compare (viewHead a) (viewHead b)
    tail a = if viewTail a == Just b then b else empty