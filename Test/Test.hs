--Testing program.
--CLO refers to the text "Ideals, Varieties, and Algorithms"
--by David Cox, John Little, and Donal O'Shea

import Yaiba.Buchberger
import Yaiba.Ideal
import Yaiba.Sugar
import Yaiba.Polynomial
import Yaiba.Monomial
import Yaiba.SPoly
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.IO

j_1 = fromList [(M [3,42,10,4,0,0,0,0],-1),(M [0,0,0,0,1,0,0,0],1)]
j_2 = fromList [(M [21,5,3,12,0,0,0,0],-1),(M [0,0,0,0,0,1,0,0],1)]
j_3 = fromList [(M [4,13,5,41,0,0,0,0],-1),(M [0,0,0,0,0,0,1,0],1)]
j_4 = fromList [(M [7,22,13,5,0,0,0,0],-1),(M [0,0,0,0,0,0,0,1],1)]::Poly Lex
j' = j_1:j_2:j_3:j_4:[]
j = I $ initSugars (DS.fromList j') :: Ideal Lex

f = "output"

main = do 
  let gb = show (getPolys $ gB j)
  putStrLn "8"
  putStrLn ("Lex")
  putStrLn (show (getPolys j))
  start <- getCurrentTime
  putStrLn (gb) 
  end <- getCurrentTime
  putStrLn (show (end `diffUTCTime` start)) {-
  start <- getCurrentTime
  putStrLn $ "A (parallel) non-reduced GB of j is " ++ gb
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."-}