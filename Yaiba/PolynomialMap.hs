{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}
-- | A Poly is synonymous to a map from Mon lists to a rational number
module Yaiba.PolynomialMap where

import qualified Yaiba.Map as YM
import qualified Data.Ord as DO
import Yaiba.Monomial
import Yaiba.Sugar
import Yaiba.Base
import Data.Maybe
import Prelude hiding (null,filter,map,rem,sum)

-- | 
data Poly ord = P (YM.Map (Mon ord) Q)

instance (Ord (Mon ord)) => Show (Poly ord) where
  show a | numTerms a == 0 = "0"
         | otherwise = showTerm $ YM.toDescList (getMap a)

instance (Ord (Mon ord)) => Eq (Poly ord) where
  a == b = if numTerms a == numTerms b then False
           else isNull $! a-b
  
instance (Ord (Mon ord)) => Ord (Poly ord) where
    compare (P a) (P b) | YM.null a && YM.null b = EQ
                        | YM.null a = LT
                        | YM.null b = GT
                        | top == EQ = compare taila tailb
                        | otherwise = top where
                        top = DO.comparing YM.findMax a b
                        taila = P $! YM.deleteMax a
                        tailb = P $! YM.deleteMax b

showTerm :: [(Mon ord, Q)] -> String
showTerm [] = ""
showTerm ((a,b):[]) | show a == " " = show b
                    | b==0 = ""
                    | otherwise = if b/=1 then
                                    show b ++ "*" ++ show a 
                                  else
                                    show a
showTerm ((a,b):as) | show a == " " = show b ++ showTerm as
                    | b==0 = showTerm as
                    | otherwise = if b/=1 then
                                    show b ++ "*" ++ show a ++ " + " ++ showTerm as 
                                  else
                                    show a ++ " + " ++ showTerm as

nullPoly = P YM.empty

monPoly m q = P $ YM.singleton m q

fromList :: (Ord (Mon ord)) => [(Mon ord, Q)] -> Poly ord
fromList a = prune $ P $ YM.fromList a

isNull :: Poly ord -> Bool
isNull (P a) = YM.null a

-- | Removes elements that point to the zero element of the field.
prune :: (Ord (Mon ord)) => Poly ord -> Poly ord
prune (P a) = P $ YM.filter (/=0) a

getMap :: Poly ord -> YM.Map (Mon ord) Q
getMap (P a) = a

-- | Returns a tuple of the lead term Mon list and its coefficient.
leadTerm :: Poly ord -> (Mon ord, Q)
leadTerm (P a) = if YM.null a then (Constant,0)
                 else YM.findMax a
                             
-- | Just returns the lead term Mon list.
monLT = fst . leadTerm

-- | The degree of the poly.
deg = degree . monLT

-- | Returns a tuple of the lead term as Poly and the rest of the supplied Poly.
deleteFindLT :: Poly ord -> ((Mon ord, Q), Poly ord)
deleteFindLT a@(P a') = if YM.null a' then ((Constant,0), nullPoly)
                        else let ((m,q),y) = YM.deleteFindMax $! getMap a
                             in ((m,q),P y)

-- | The length of the poly
numTerms :: Poly ord -> Int
numTerms a = YM.size $ getMap a

maybeAdd a Nothing = Just a
maybeAdd a (Just b) = let !sum = a+b
                      in if sum==0 then Nothing else Just sum

instance (Ord (Mon ord)) => Num (Poly ord) where
    P a + P b | YM.null a = P b
              | YM.null b = P a
              | otherwise = P $! YM.foldWithKey addPrune a b where
                                           addPrune mon coef polyMap = YM.alter (maybeAdd coef) mon polyMap
--              | otherwise = P $ fst (YM.mapAccumWithKey addPrune a b) where
--                                           addPrune acc mon coef = (YM.alter (maybeAdd coef) mon acc,True)
    a * P b             = YM.foldWithKey (polyFoil a) nullPoly b where
                              polyFoil p mon coef acc = acc + monMult mon coef p
--    a * P b               = fst (YM.mapAccumWithKey polyFoil nullPoly b) where
--                                polyFoil acc mon coef = (acc + monMult mon coef a,True)
    negate (P a) = P $! YM.map negate a
    fromInteger 0 = nullPoly
    fromInteger 1 = monPoly Constant 1

-- | Just adds a monomial without turning it into a polynomial first
monAdd mon coef (P poly) = P $ YM.alter (maybeAdd coef) mon poly
monAdd' mon coef (P poly) = YM.alter (maybeAdd coef) mon poly

-- | Scales every term of a Polynomial by a Mon list and rational number.
monMult mon coef (P poly) = P $! YM.mapKeysValuesMonotonic (\(!k,!v) -> (multiply mon k, v*coef)) poly

-- | Divides the first polynomial by the second once
{-
quoRem' :: (Ord (Mon ord)) =>
            Poly ord -> (Poly ord,Sugar ord) -> (Poly ord, Poly ord)
quoRem' rem (d,_) | isNull rem = (nullPoly, nullPoly)
| otherwise  = let (a1,a2) = leadTerm rem
                                     (b1,b2) = leadTerm d
                                     remOd = divide a1 b1
                                     remOdco = a2/b2 
                                 in case isFactor b1 a1 of
                                      False -> (nullPoly, rem)
                                      True -> (P (singleton remOd remOdco), rem - (monMult remOd remOdco d))
-}
-- | Divides the first polynomial by the second repeatedly until it fails.
quoRem :: (Ord (Mon ord)) =>
           Poly ord -> (Poly ord,Sugar ord) -> (Poly ord, Poly ord)
quoRem a (b,_) = quoRem' a b nullPoly where
  quoRem' rem d quo | isNull rem = (quo, nullPoly)
                    | otherwise = let !(a1,a2) = leadTerm rem
                                      !(b1,b2) = leadTerm d
                                  in if isFactor b1 a1 then
                                         let !remOd = divide a1 b1
                                             !remOdco = a2/b2
                                             !newRem = rem - monMult remOd remOdco d
                                             !newQuo = monAdd remOd (-remOdco) quo 
--                                             !newQuo = quo + monPoly remOd remOdco
                                         in quoRem' newRem d newQuo
                                     else
                                         (quo, rem)
                                      
