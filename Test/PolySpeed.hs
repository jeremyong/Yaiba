{-# OPTIONS_GHC -XBangPatterns #-}
module Main where

import System.Random
import Yaiba.Base
import qualified Yaiba.Monomial as M
import qualified Yaiba.PolynomialMap as PM
import qualified Yaiba.PolynomialList as PL
import System.CPUTime
import Text.Printf
import Control.Monad
import Control.Parallel.Strategies
import Microbench
import qualified Data.List as DL

lim :: Int
lim = 10^6

numVars :: Int
numVars = 3

randoList :: IO [[(M.Mon M.Lex,Q)]]
randoList = do
  gen <- newStdGen
  gen' <- newStdGen
  let ns = randomRs (0,10) gen :: [Int]
  let ms = randomRs (0,30) gen' :: [Int]
  return $ makePolys 7 ns ms

makePolys :: Int -> [Int] -> [Int] -> [[(M.Mon M.Lex,Q)]]
makePolys 0 _ _ = []
makePolys n (a:ns) ms = let (superList,ms') = splitter a ms
                        in superList:(makePolys (n-1) ns ms')

splitter :: Int -> [Int] -> ([(M.Mon M.Lex,Q)],[Int])
splitter i ms = f ([],ms) i where
    f ans 0 = ans
    f (xs,ys) i = let (q:front,back) = splitAt (numVars+1) ys
                  in f ((M.fromList front :: M.Mon M.Lex,fromIntegral q::Q):xs,back) (i-1)

convPMap ns = map PM.fromList ns
convPList ns = map PL.fromList ns
--pmapMul ns = foldl (+) (PM.monPoly M.Constant 1) ns
pmapMul ns = DL.product ns
--plistMul ns = foldl (+) (PL.monPoly M.Constant 1) ns
plistMul ns = DL.product ns

time :: IO t -> IO t
time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "Computation time: %0.5f sec\n" (diff :: Double)
  return v

main = do
  a <- randoList
  b <- randoList
  let !pmap = convPMap a
  let !plist = convPList a
  
  --putStrLn $ "Multiplying same polys: " ++ show (show pmap == show plist)

  let m = pmapMul pmap
  let v = plistMul plist

  --putStrLn "Map implementation"
  --time $ m `seq` return ()
  putStrLn "List implementation"
  time $ v `seq` return ()
--  putStrLn "Map implementation"
--  time $ m `seq` return ()
  --putStrLn $ "Results agree: " ++ show (show m == show v)

  putStrLn "Done."
  --putStrLn $ show b