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
import Data.Time.Clock (diffUTCTime, getCurrentTime)

j_1 = fromList [(M [3,4,2,4,0,0,0,0],-1),(M [0,0,0,0,1,0,0,0],1)]
j_2 = fromList [(M [2,3,1,1,0,0,0,0],-1),(M [0,0,0,0,0,1,0,0],1)]
j_3 = fromList [(M [1,2,2,1,0,0,0,0],-1),(M [0,0,0,0,0,0,1,0],1)]
j_4 = fromList [(M [3,1,3,2,0,0,0,0],-1),(M [0,0,0,0,0,0,0,1],1)]::Poly Lex
j' = j_1:j_2:j_3:j_3:[]
j = I $ initSugars j' :: Ideal Lex

main = do 
  start <- getCurrentTime
  putStrLn $ "A (parallel) non-reduced GB of j is " ++ (show (getPolys $ gB j))
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."