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

j_1 = P.fromList [(M.fromList [3,9,2,4,0,0,0,0],-1),(M.fromList [0,0,0,0,1,0,0,0],1)]
j_2 = P.fromList [(M.fromList [5,5,3,7,0,0,0,0],-1),(M.fromList [0,0,0,0,0,1,0,0],1)]
j_3 = P.fromList [(M.fromList [4,5,5,6,0,0,0,0],-1),(M.fromList [0,0,0,0,0,0,1,0],1)]
j_4 = P.fromList [(M.fromList [7,8,4,5,0,0,0,0],-1),(M.fromList [0,0,0,0,0,0,0,1],1)]
j' = j_1:j_2:j_3:j_4:[]
j = DS.fromList (DL.map (\x-> P.PS x) (initSugars j'))
jideal = I $ DV.fromList (initSugars j') :: Ideal M.Lex

main = do 
  let gb = show $ (getPolys $ gB j)
  putStrLn "8"
  putStrLn ("Lex")
  putStrLn (show (getPolys jideal))
  start <- getCurrentTime
  putStrLn (gb) 
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."