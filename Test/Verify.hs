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

j_1 = P.fromList [(M.fromList [2,3,4,3,0,0,0,0],-1),(M.fromList [0,0,0,0,1,0,0,0],1)]
j_2 = P.fromList [(M.fromList [4,3,2,3,0,0,0,0],-1),(M.fromList [0,0,0,0,0,1,0,0],1)]
j_3 = P.fromList [(M.fromList [3,5,3,5,0,0,0,0],-1),(M.fromList [0,0,0,0,0,0,1,0],1)]
j_4 = P.fromList [(M.fromList [2,3,4,3,0,0,0,0],-1),(M.fromList [0,0,0,0,0,0,0,1],1)]
j' = j_1:j_2:j_3:j_4:[]
j = DS.fromList (initPolySugars j')
jideal = I $ DV.fromList (initSugars j') :: Ideal M.Lex

{-p_1 = P.fromList [(M.fromList [4,0,0,0],1),(M.fromList [0,1,0,0],-1)]
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
pideal = I $ DV.fromList (initSugars p') :: Ideal M.Lex

p_1 = P.fromList [(M.fromList [4,0,0,0],2),(M.fromList [2,2,0,0],3),(M.fromList [1,3,0,0],1),(M.fromList [0,4,0,0],3),(M.fromList [3,0,1,0],3),(M.fromList [2,1,1,0],-3),(M.fromList [1,2,1,0],1),(M.fromList [0,3,1,0],2),(M.fromList [2,0,2,0],2),(M.fromList [1,1,2,0],2),(M.fromList [0,2,2,0],-1),(M.fromList [1,0,3,0],-1),(M.fromList [0,1,3,0],1),(M.fromList [0,0,4,0],-2),(M.fromList [3,0,0,1],2),(M.fromList [2,1,0,1],3),(M.fromList [1,2,0,1],3),(M.fromList [1,1,1,1],1),(M.fromList [1,0,2,1],2),(M.fromList [0,1,2,1],3),(M.fromList [2,0,0,2],-3),(M.fromList [1,1,0,2],-1),(M.fromList [0,1,1,2],-2),(M.fromList [0,0,2,2],1),(M.fromList [1,0,0,3],3),(M.fromList [0,1,0,3],2),(M.fromList [0,0,1,3],-2),(M.fromList [0,0,0,4],3)]
p_2 = P.fromList [(M.fromList [4,0,0,0],-1),(M.fromList [3,1,0,0],-1),(M.fromList [2,2,0,0],2),(M.fromList [1,3,0,0],-2),(M.fromList [0,4,0,0],1),(M.fromList [3,0,1,0],-3),(M.fromList [2,1,1,0],-2),(M.fromList [1,2,1,0],-2),(M.fromList [0,3,1,0],3),(M.fromList [2,0,2,0],-2),(M.fromList [1,1,2,0],3),(M.fromList [1,0,3,0],2),(M.fromList [0,1,3,0],-3),(M.fromList [0,0,4,0],2),(M.fromList [3,0,0,1],1),(M.fromList [2,1,0,1],3),(M.fromList [1,2,0,1],-1),(M.fromList [0,3,0,1],1),(M.fromList [2,0,1,1],3),(M.fromList [1,1,1,1],2),(M.fromList [0,2,1,1],-2),(M.fromList [1,0,2,1],-3),(M.fromList [0,1,2,1],3),(M.fromList [1,1,0,2],-1),(M.fromList [0,2,0,2],-2),(M.fromList [1,0,1,2],-2),(M.fromList [0,1,1,2],-1),(M.fromList [0,0,2,2],1),(M.fromList [0,0,1,3],-3),(M.fromList [0,0,0,4],1)]
p_3 = P.fromList [(M.fromList [3,1,0,0],2),(M.fromList [1,3,0,0],3),(M.fromList [0,4,0,0],3),(M.fromList [3,0,1,0],-2),(M.fromList [2,1,1,0],-2),(M.fromList [1,2,1,0],3),(M.fromList [0,3,1,0],-3),(M.fromList [2,0,2,0],3),(M.fromList [0,2,2,0],-2),(M.fromList [1,0,3,0],2),(M.fromList [0,1,3,0],2),(M.fromList [0,0,4,0],-3),(M.fromList [3,0,0,1],2),(M.fromList [0,3,0,1],-1),(M.fromList [2,0,1,1],-2),(M.fromList [1,1,1,1],-2),(M.fromList [0,2,1,1],2),(M.fromList [0,0,3,1],-2),(M.fromList [2,0,0,2],2),(M.fromList [1,1,0,2],2),(M.fromList [0,2,0,2],-2),(M.fromList [0,1,1,2],2),(M.fromList [0,0,2,2],-2),(M.fromList [1,0,0,3],1),(M.fromList [0,1,0,3],1),(M.fromList [0,0,1,3],1),(M.fromList [0,0,0,4],-3)]
p_4 = P.fromList [(M.fromList [4,0,0,0],-3),(M.fromList [3,1,0,0],3),(M.fromList [2,2,0,0],1),(M.fromList [1,3,0,0],-3),(M.fromList [0,4,0,0],-2),(M.fromList [3,0,1,0],-1),(M.fromList [2,1,1,0],-3),(M.fromList [1,2,1,0],2),(M.fromList [0,3,1,0],-3),(M.fromList [2,0,2,0],-1),(M.fromList [1,1,2,0],-2),(M.fromList [0,2,2,0],3),(M.fromList [1,0,3,0],1),(M.fromList [0,1,3,0],2),(M.fromList [0,0,4,0],2),(M.fromList [3,0,0,1],-2),(M.fromList [1,2,0,1],1),(M.fromList [2,0,1,1],-3),(M.fromList [1,1,1,1],-2),(M.fromList [0,2,1,1],-2),(M.fromList [1,0,2,1],3),(M.fromList [0,1,2,1],-1),(M.fromList [0,0,3,1],-3),(M.fromList [2,0,0,2],1),(M.fromList [1,1,0,2],-3),(M.fromList [0,2,0,2],-2),(M.fromList [1,0,1,2],-3),(M.fromList [0,1,1,2],-1),(M.fromList [0,0,2,2],2),(M.fromList [1,0,0,3],2),(M.fromList [0,1,0,3],3),(M.fromList [0,0,1,3],1),(M.fromList [0,0,0,4],1)]
p' = [p_1,p_2,p_3,p_4]
p = DS.fromList (initPolySugars p')
pideal = I $ DV.fromList (initSugars p') :: Ideal M.Grevlex

p_1 = P.fromList [(M.fromList [2,1,0,0],-1),(M.fromList [1,2,0,0],-3),(M.fromList [2,0,1,0],1),(M.fromList [1,1,1,0],2),(M.fromList [0,2,1,0],-3),(M.fromList [1,0,2,0],2),(M.fromList [0,1,2,0],1),(M.fromList [0,0,3,0],-2),(M.fromList [1,1,0,1],-1),(M.fromList [0,2,0,1],-1),(M.fromList [1,0,1,1],3),(M.fromList [0,1,1,1],2),(M.fromList [0,0,2,1],-3),(M.fromList [0,0,1,2],2)]
p_2 = P.fromList [(M.fromList [3,0,0,0],-3),(M.fromList [2,1,0,0],3),(M.fromList [1,2,0,0],-3),(M.fromList [0,3,0,0],3),(M.fromList [2,0,1,0],-1),(M.fromList [1,1,1,0],1),(M.fromList [0,2,1,0],2),(M.fromList [1,0,2,0],-2),(M.fromList [0,1,2,0],2),(M.fromList [2,0,0,1],-2),(M.fromList [1,1,0,1],2),(M.fromList [0,2,0,1],2),(M.fromList [1,0,1,1],3),(M.fromList [0,1,1,1],2),(M.fromList [0,0,2,1],2),(M.fromList [1,0,0,2],-1),(M.fromList [0,1,0,2],1),(M.fromList [0,0,1,2],2),(M.fromList [0,0,0,3],3)]
p_3 = P.fromList [(M.fromList [3,0,0,0],-3),(M.fromList [2,1,0,0],-2),(M.fromList [1,2,0,0],2),(M.fromList [0,3,0,0],-2),(M.fromList [2,0,1,0],-1),(M.fromList [1,1,1,0],3),(M.fromList [0,2,1,0],1),(M.fromList [1,0,2,0],1),(M.fromList [0,1,2,0],1),(M.fromList [0,0,3,0],-1),(M.fromList [2,0,0,1],-1),(M.fromList [1,1,0,1],-2),(M.fromList [0,2,0,1],2),(M.fromList [1,0,1,1],-1),(M.fromList [0,0,2,1],-2),(M.fromList [1,0,0,2],-3),(M.fromList [0,1,0,2],1),(M.fromList [0,0,1,2],-1)]
p_4 = P.fromList [(M.fromList [3,0,0,0],2),(M.fromList [2,1,0,0],-2),(M.fromList [1,2,0,0],-1),(M.fromList [0,3,0,0],2),(M.fromList [2,0,1,0],3),(M.fromList [1,1,1,0],-2),(M.fromList [0,2,1,0],-1),(M.fromList [0,1,2,0],-2),(M.fromList [0,0,3,0],-2),(M.fromList [2,0,0,1],-3),(M.fromList [1,1,0,1],1),(M.fromList [0,2,0,1],2),(M.fromList [0,1,1,1],2),(M.fromList [0,1,0,2],2),(M.fromList [0,0,1,2],-3)]
p_5 = P.fromList [(M.fromList [3,0,0,0],2),(M.fromList [2,1,0,0],-1),(M.fromList [1,2,0,0],2),(M.fromList [0,3,0,0],2),(M.fromList [2,0,1,0],-2),(M.fromList [1,1,1,0],1),(M.fromList [0,2,1,0],3),(M.fromList [1,0,2,0],3),(M.fromList [0,1,2,0],3),(M.fromList [0,0,3,0],1),(M.fromList [2,0,0,1],-1),(M.fromList [1,1,0,1],-2),(M.fromList [0,2,0,1],-3),(M.fromList [1,0,1,1],-2),(M.fromList [0,1,1,1],-1),(M.fromList [0,0,2,1],3),(M.fromList [1,0,0,2],1),(M.fromList [0,1,0,2],-1),(M.fromList [0,0,1,2],-1),(M.fromList [0,0,0,3],-2)]
p_6 = P.fromList [(M.fromList [3,0,0,0],3),(M.fromList [2,1,0,0],-3),(M.fromList [1,2,0,0],-1),(M.fromList [0,3,0,0],1),(M.fromList [2,0,1,0],3),(M.fromList [1,1,1,0],-3),(M.fromList [1,0,2,0],1),(M.fromList [0,1,2,0],-2),(M.fromList [2,0,0,1],-2),(M.fromList [1,1,0,1],-1),(M.fromList [0,2,0,1],3),(M.fromList [1,0,1,1],3),(M.fromList [0,1,1,1],2),(M.fromList [0,0,0,3],3)]
p' = [p_1,p_2,p_3,p_4,p_5,p_6]
p = DS.fromList (initPolySugars p')
pideal = I $ DV.fromList (initSugars p') :: Ideal M.Lex -}

p_1 = P.fromList [(M.fromList [0,32,0,23,0,0,0,0,0],1),(M.fromList [0,0,82,0,1,0,0,0,0],-1)]
p_2 = P.fromList [(M.fromList [0,45,0,0,0,0,0,0,0],1),(M.fromList [0,0,13,21,0,1,0,0,0],-1)]
p_3 = P.fromList [(M.fromList [0,41,0,0,0,0,1,0,0],-1),(M.fromList [0,0,33,12,0,0,0,0,0],1)]
p_4 = P.fromList [(M.fromList [0,22,0,0,0,0,0,0,0],1),(M.fromList [0,0,33,12,0,0,0,1,0],-1)]
p_5 = P.fromList [(M.fromList [0,5,17,22,0,0,0,0,1],1),(M.fromList [0,0,0,0,0,0,0,0,0],-1)]
p_6 = P.fromList [(M.fromList [1,1,1,1,0,0,0,0,0],1),(M.fromList [0,0,0,0,0,0,0,0,0],-1)]
p' = [p_1,p_2,p_3,p_4,p_5,p_6]
p = DS.fromList (initPolySugars p')
pideal = I $ DV.fromList (initSugars p') :: Ideal M.Lex

main = do 
  let gb = show $ (getPolys $ gB p)
  putStrLn "8"
  putStrLn ("Lex")
  putStrLn (show (getPolys pideal))
  start <- getCurrentTime
  putStrLn (gb) 
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."