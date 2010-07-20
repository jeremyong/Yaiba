--Testing program.
--CLO refers to the text "Ideals, Varieties, and Algorithms"
--by David Cox, John Little, and Donal O'Shea

import Yaiba.Buchberger
import Yaiba.Ideal
import Yaiba.Sugar
import qualified Yaiba.Polynomial as P
import qualified Yaiba.Monomial as M
import Yaiba.SPoly
import Control.Parallel
import GHC.Conc (numCapabilities)
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Vector as DV
import qualified Data.List as DL
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.IO
import System.CPUTime
import Text.Printf
import Criterion.Main

main = do 
  let gb = modgB y
  putStrLn "8"
  putStrLn "Lex"
  putStrLn (show $ getPolys yideal)
  start <- getCurrentTime
  gb `seq` return ()
  end <- getCurrentTime
  let diff = show $ end `diffUTCTime` start
  printf "Time to compute gb in Yaiba with %i cores: %s sec\n" numCapabilities diff
  putStrLn (show $ gb) 
{-
time :: IO t -> IO t
time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "Time to compute gb in Yaiba (%i cores): %0.5f sec\n" numCapabilities (diff :: Double)
  return v
-}
j_1 = P.fromList [(M.fromList [2,3,4,3,0,0,0,0],-1),(M.fromList [0,0,0,0,1,0,0,0],1)]
j_2 = P.fromList [(M.fromList [4,3,2,3,0,0,0,0],-1),(M.fromList [0,0,0,0,0,1,0,0],1)]
j_3 = P.fromList [(M.fromList [3,5,3,5,0,0,0,0],-1),(M.fromList [0,0,0,0,0,0,1,0],1)]
j_4 = P.fromList [(M.fromList [2,3,4,3,0,0,0,0],-1),(M.fromList [0,0,0,0,0,0,0,1],1)]
j' = j_1:j_2:j_3:j_4:[]
j = DS.fromList (initPolySugars j')
jideal = I $ DV.fromList (initSugars j') :: Ideal M.Lex

y_1 = P.fromList [(M.fromList [0,0,8,1,0],1),(M.fromList [0,0,8,0,0],8),(M.fromList [0,0,6,1,0],4),(M.fromList [0,0,6,0,0],-2),(M.fromList [0,0,4,1,0],6),(M.fromList [0,0,4,0,0],90),(M.fromList [0,0,2,1,0],4),(M.fromList [0,0,2,0,0],-38),(M.fromList [0,0,0,1,0],1),(M.fromList [0,0,0,0,0],-10)]
y_2 = P.fromList [(M.fromList [0,0,8,0,1],1),(M.fromList [0,0,7,0,0],-34),(M.fromList [0,0,6,0,1],4),(M.fromList [0,0,5,0,0],-6),(M.fromList [0,0,4,0,1],6),(M.fromList [0,0,3,0,0],-102),(M.fromList [0,0,2,0,1],4),(M.fromList [0,0,1,0,0],-2),(M.fromList [0,0,0,0,1],1)]
y' = [y_1,y_2]
y = DS.fromList (initPolySugars y')
yideal = I $ DV.fromList (initSugars y') :: Ideal M.Lex

x_1 = P.fromList [(M.fromList [1,0,0,0,0],1),(M.fromList [0,1,0,0,0],1),(M.fromList [0,0,1,0,0],1),(M.fromList [0,0,0,1,0],1),(M.fromList [0,0,0,0,1],1)]
x_2 = P.fromList [(M.fromList [1,1,0,0,0],1),(M.fromList [1,0,0,0,1],1),(M.fromList [0,1,1,0,0],1),(M.fromList [0,0,1,1,0],1),(M.fromList [0,0,0,1,1],1)]
x_3 = P.fromList [(M.fromList [1,1,1,0,0],1),(M.fromList [1,1,0,0,1],1),(M.fromList [1,0,0,1,1],1),(M.fromList [0,1,1,1,0],1),(M.fromList [0,0,1,1,1],1)]
x_4 = P.fromList [(M.fromList [1,1,1,1,0],1),(M.fromList [1,1,1,0,1],1),(M.fromList [1,1,0,1,1],1),(M.fromList [1,0,1,1,1],1),(M.fromList [0,1,1,1,1],1)]
x_5 = P.fromList [(M.fromList [1,1,1,1,1],1),(M.fromList [0,0,0,0,0],-1)]
x' = [x_1,x_2,x_3,x_4,x_5]
x = DS.fromList (initPolySugars x')
xideal = I $ DV.fromList (initSugars x') :: Ideal M.Lex

{-
p_1 = P.fromList [(M.fromList [4,0,0,0],1),(M.fromList [0,1,0,0],-1)]
p_2 = P.fromList [(M.fromList [3,0,0,0],1),(M.fromList [0,0,1,0],-1)]
p_3 = P.fromList [(M.fromList [2,0,0,0],1),(M.fromList [0,0,0,1],-1)]
p' = p_1:p_2:p_3:[]
p = DS.fromList (DL.map (\x-> P.PS x) (initSugars p'))
pideal = I $ DV.fromList (initSugars p') :: Ideal M.Lex

p_1 = P.fromList [(M.fromList [0,0,1,0,0],1),(M.fromList [1,0,0,0,0],-1),(M.fromList [0,1,0,0,0],-1)]
p_2 = P.fromList [(M.fromList [0,0,0,1,0],1),(M.fromList [2,0,0,0,0],-1),(M.fromList [1,1,0,0,0],-2)]
p_3 = P.fromList [(M.fromList [0,0,0,0,1],1),(M.fromList [3,0,0,0,0],-1),(M.fromList [2,1,0,0,0],-3)]
p' = p_1:p_2:p_3:[]
p = DS.fromList (initPolySugars p')
pideal = I $ DV.fromList (initSugars p') :: Ideal M.Lex -}

p_1 = P.fromList [(M.fromList [4,0,0,0],2),(M.fromList [2,2,0,0],3),(M.fromList [1,3,0,0],1),(M.fromList [0,4,0,0],3),(M.fromList [3,0,1,0],3),(M.fromList [2,1,1,0],-3),(M.fromList [1,2,1,0],1),(M.fromList [0,3,1,0],2),(M.fromList [2,0,2,0],2),(M.fromList [1,1,2,0],2),(M.fromList [0,2,2,0],-1),(M.fromList [1,0,3,0],-1),(M.fromList [0,1,3,0],1),(M.fromList [0,0,4,0],-2),(M.fromList [3,0,0,1],2),(M.fromList [2,1,0,1],3),(M.fromList [1,2,0,1],3),(M.fromList [1,1,1,1],1),(M.fromList [1,0,2,1],2),(M.fromList [0,1,2,1],3),(M.fromList [2,0,0,2],-3),(M.fromList [1,1,0,2],-1),(M.fromList [0,1,1,2],-2),(M.fromList [0,0,2,2],1),(M.fromList [1,0,0,3],3),(M.fromList [0,1,0,3],2),(M.fromList [0,0,1,3],-2),(M.fromList [0,0,0,4],3)]
p_2 = P.fromList [(M.fromList [4,0,0,0],-1),(M.fromList [3,1,0,0],-1),(M.fromList [2,2,0,0],2),(M.fromList [1,3,0,0],-2),(M.fromList [0,4,0,0],1),(M.fromList [3,0,1,0],-3),(M.fromList [2,1,1,0],-2),(M.fromList [1,2,1,0],-2),(M.fromList [0,3,1,0],3),(M.fromList [2,0,2,0],-2),(M.fromList [1,1,2,0],3),(M.fromList [1,0,3,0],2),(M.fromList [0,1,3,0],-3),(M.fromList [0,0,4,0],2),(M.fromList [3,0,0,1],1),(M.fromList [2,1,0,1],3),(M.fromList [1,2,0,1],-1),(M.fromList [0,3,0,1],1),(M.fromList [2,0,1,1],3),(M.fromList [1,1,1,1],2),(M.fromList [0,2,1,1],-2),(M.fromList [1,0,2,1],-3),(M.fromList [0,1,2,1],3),(M.fromList [1,1,0,2],-1),(M.fromList [0,2,0,2],-2),(M.fromList [1,0,1,2],-2),(M.fromList [0,1,1,2],-1),(M.fromList [0,0,2,2],1),(M.fromList [0,0,1,3],-3),(M.fromList [0,0,0,4],1)]
p_3 = P.fromList [(M.fromList [3,1,0,0],2),(M.fromList [1,3,0,0],3),(M.fromList [0,4,0,0],3),(M.fromList [3,0,1,0],-2),(M.fromList [2,1,1,0],-2),(M.fromList [1,2,1,0],3),(M.fromList [0,3,1,0],-3),(M.fromList [2,0,2,0],3),(M.fromList [0,2,2,0],-2),(M.fromList [1,0,3,0],2),(M.fromList [0,1,3,0],2),(M.fromList [0,0,4,0],-3),(M.fromList [3,0,0,1],2),(M.fromList [0,3,0,1],-1),(M.fromList [2,0,1,1],-2),(M.fromList [1,1,1,1],-2),(M.fromList [0,2,1,1],2),(M.fromList [0,0,3,1],-2),(M.fromList [2,0,0,2],2),(M.fromList [1,1,0,2],2),(M.fromList [0,2,0,2],-2),(M.fromList [0,1,1,2],2),(M.fromList [0,0,2,2],-2),(M.fromList [1,0,0,3],1),(M.fromList [0,1,0,3],1),(M.fromList [0,0,1,3],1),(M.fromList [0,0,0,4],-3)]
p_4 = P.fromList [(M.fromList [4,0,0,0],-3),(M.fromList [3,1,0,0],3),(M.fromList [2,2,0,0],1),(M.fromList [1,3,0,0],-3),(M.fromList [0,4,0,0],-2),(M.fromList [3,0,1,0],-1),(M.fromList [2,1,1,0],-3),(M.fromList [1,2,1,0],2),(M.fromList [0,3,1,0],-3),(M.fromList [2,0,2,0],-1),(M.fromList [1,1,2,0],-2),(M.fromList [0,2,2,0],3),(M.fromList [1,0,3,0],1),(M.fromList [0,1,3,0],2),(M.fromList [0,0,4,0],2),(M.fromList [3,0,0,1],-2),(M.fromList [1,2,0,1],1),(M.fromList [2,0,1,1],-3),(M.fromList [1,1,1,1],-2),(M.fromList [0,2,1,1],-2),(M.fromList [1,0,2,1],3),(M.fromList [0,1,2,1],-1),(M.fromList [0,0,3,1],-3),(M.fromList [2,0,0,2],1),(M.fromList [1,1,0,2],-3),(M.fromList [0,2,0,2],-2),(M.fromList [1,0,1,2],-3),(M.fromList [0,1,1,2],-1),(M.fromList [0,0,2,2],2),(M.fromList [1,0,0,3],2),(M.fromList [0,1,0,3],3),(M.fromList [0,0,1,3],1),(M.fromList [0,0,0,4],1)]
p' = [p_1,p_2,p_3,p_4]
p = DS.fromList (initPolySugars p')
pideal = I $ DV.fromList (initSugars p') :: Ideal M.Lex

{-p_1 = P.fromList [(M.fromList [2,1,0,0],-1),(M.fromList [1,2,0,0],-3),(M.fromList [2,0,1,0],1),(M.fromList [1,1,1,0],2),(M.fromList [0,2,1,0],-3),(M.fromList [1,0,2,0],2),(M.fromList [0,1,2,0],1),(M.fromList [0,0,3,0],-2),(M.fromList [1,1,0,1],-1),(M.fromList [0,2,0,1],-1),(M.fromList [1,0,1,1],3),(M.fromList [0,1,1,1],2),(M.fromList [0,0,2,1],-3),(M.fromList [0,0,1,2],2)]
p_2 = P.fromList [(M.fromList [3,0,0,0],-3),(M.fromList [2,1,0,0],3),(M.fromList [1,2,0,0],-3),(M.fromList [0,3,0,0],3),(M.fromList [2,0,1,0],-1),(M.fromList [1,1,1,0],1),(M.fromList [0,2,1,0],2),(M.fromList [1,0,2,0],-2),(M.fromList [0,1,2,0],2),(M.fromList [2,0,0,1],-2),(M.fromList [1,1,0,1],2),(M.fromList [0,2,0,1],2),(M.fromList [1,0,1,1],3),(M.fromList [0,1,1,1],2),(M.fromList [0,0,2,1],2),(M.fromList [1,0,0,2],-1),(M.fromList [0,1,0,2],1),(M.fromList [0,0,1,2],2),(M.fromList [0,0,0,3],3)]
p_3 = P.fromList [(M.fromList [3,0,0,0],-3),(M.fromList [2,1,0,0],-2),(M.fromList [1,2,0,0],2),(M.fromList [0,3,0,0],-2),(M.fromList [2,0,1,0],-1),(M.fromList [1,1,1,0],3),(M.fromList [0,2,1,0],1),(M.fromList [1,0,2,0],1),(M.fromList [0,1,2,0],1),(M.fromList [0,0,3,0],-1),(M.fromList [2,0,0,1],-1),(M.fromList [1,1,0,1],-2),(M.fromList [0,2,0,1],2),(M.fromList [1,0,1,1],-1),(M.fromList [0,0,2,1],-2),(M.fromList [1,0,0,2],-3),(M.fromList [0,1,0,2],1),(M.fromList [0,0,1,2],-1)]
p_4 = P.fromList [(M.fromList [3,0,0,0],2),(M.fromList [2,1,0,0],-2),(M.fromList [1,2,0,0],-1),(M.fromList [0,3,0,0],2),(M.fromList [2,0,1,0],3),(M.fromList [1,1,1,0],-2),(M.fromList [0,2,1,0],-1),(M.fromList [0,1,2,0],-2),(M.fromList [0,0,3,0],-2),(M.fromList [2,0,0,1],-3),(M.fromList [1,1,0,1],1),(M.fromList [0,2,0,1],2),(M.fromList [0,1,1,1],2),(M.fromList [0,1,0,2],2),(M.fromList [0,0,1,2],-3)]
p_5 = P.fromList [(M.fromList [3,0,0,0],2),(M.fromList [2,1,0,0],-1),(M.fromList [1,2,0,0],2),(M.fromList [0,3,0,0],2),(M.fromList [2,0,1,0],-2),(M.fromList [1,1,1,0],1),(M.fromList [0,2,1,0],3),(M.fromList [1,0,2,0],3),(M.fromList [0,1,2,0],3),(M.fromList [0,0,3,0],1),(M.fromList [2,0,0,1],-1),(M.fromList [1,1,0,1],-2),(M.fromList [0,2,0,1],-3),(M.fromList [1,0,1,1],-2),(M.fromList [0,1,1,1],-1),(M.fromList [0,0,2,1],3),(M.fromList [1,0,0,2],1),(M.fromList [0,1,0,2],-1),(M.fromList [0,0,1,2],-1),(M.fromList [0,0,0,3],-2)]
p_6 = P.fromList [(M.fromList [3,0,0,0],3),(M.fromList [2,1,0,0],-3),(M.fromList [1,2,0,0],-1),(M.fromList [0,3,0,0],1),(M.fromList [2,0,1,0],3),(M.fromList [1,1,1,0],-3),(M.fromList [1,0,2,0],1),(M.fromList [0,1,2,0],-2),(M.fromList [2,0,0,1],-2),(M.fromList [1,1,0,1],-1),(M.fromList [0,2,0,1],3),(M.fromList [1,0,1,1],3),(M.fromList [0,1,1,1],2),(M.fromList [0,0,0,3],3)]
p' = [p_1,p_2,p_3,p_4,p_5,p_6]
p = DS.fromList (initPolySugars p')
pideal = I $ DV.fromList (initSugars p') :: Ideal M.Lex

p_1 = P.fromList [(M.fromList [0,32,0,23,0,0,0,0,0],1),(M.fromList [0,0,82,0,1,0,0,0,0],-1)]
p_2 = P.fromList [(M.fromList [0,45,0,0,0,0,0,0,0],1),(M.fromList [0,0,13,21,0,1,0,0,0],-1)]
p_3 = P.fromList [(M.fromList [0,41,0,0,0,0,1,0,0],-1),(M.fromList [0,0,33,12,0,0,0,0,0],1)]
p_4 = P.fromList [(M.fromList [0,22,0,0,0,0,0,0,0],1),(M.fromList [0,0,33,12,0,0,0,1,0],-1)]
p_5 = P.fromList [(M.fromList [0,5,17,22,0,0,0,0,1],1),(M.fromList [0,0,0,0,0,0,0,0,0],-1)]
p_6 = P.fromList [(M.fromList [1,1,1,1,0,0,0,0,0],1),(M.fromList [0,0,0,0,0,0,0,0,0],-1)]
p' = [p_1,p_2,p_3,p_4,p_5,p_6]
p = DS.fromList (initPolySugars p')
pideal = I $ DV.fromList (initSugars p') :: Ideal M.Lex -}

