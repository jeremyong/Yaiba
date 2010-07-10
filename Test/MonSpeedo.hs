{-# OPTIONS_GHC -XBangPatterns #-}
module Main where

import System.Random
import qualified Yaiba.Monomial as MMap
import qualified Yaiba.MonomialVec as MVec
import System.CPUTime
import Text.Printf
import Control.Monad
import Control.Parallel.Strategies
import Microbench

lim :: Int
lim = 10^6

randoList :: IO [([Int],[Int])]
randoList = do
  gen <- newStdGen
  let ns = randomRs (0,300) gen :: [Int]
  return $ makePairs 10000 ns

makePairs 0 _ = []
makePairs n ns = let fst8 = take 8 ns
                     snd8 = take 8 (drop 8 ns)
                     rest = drop 16 ns
                 in (fst8,snd8):(makePairs (n-1) rest)

convMMap ns = map (\(x,y) -> (MMap.fromList x,MMap.fromList y)) ns
convMVec ns = map (\(x,y) -> (MVec.fromList x,MVec.fromList y)) ns
mmapMul ns = map (\(x,y) -> MMap.multiply x y) ns
mvecMul ns = map (\(x,y) -> MVec.multiply x y) ns

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
  let !mmapa = convMMap a
  let !mveca = convMVec b
  
  let m = mmapMul mmapa
  let v = mvecMul mveca

  time $ m `seq` return ()
  time $ v `seq` return ()

  putStrLn "Done."
  --putStrLn $ show b