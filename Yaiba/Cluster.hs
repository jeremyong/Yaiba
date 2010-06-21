{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances -XBangPatterns #-}

module Yaiba.Cluster where
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Vector.Generic as DVG

class Cluster c where
    singleton :: c a -> c (c a)
    cluster :: Int -> c a -> c (c a)
    decluster :: c (c a) -> c a
    lift :: (c a -> b) -> c (c a) -> c b

instance Cluster [] where
    singleton list = [list]
    cluster _ [] = []
    cluster n list = DM.elems $ fst $ DL.foldl' f (DM.empty,0) list where
                    f = \(!acc,!z) !a -> (DM.insertWith (++) (z `mod` n) [a] acc, z+1)
    decluster = DL.concat
    lift = DL.map

--instance Cluster DVG.Vector where
--    singleton = DVG.singleton
--    cluster n vec | vec == DVG.empty = DVG.empty
--                  | otherwise = 
--                      DM.elems $ fst $ DL.foldl' f (DM.empty,0) list where
--                            f = \(!acc,!z) !a -> (DM.insertWith (++) (z `mod` n) [a] acc, z+1)
--    decluster = DL.concat
--    lift = DL.map