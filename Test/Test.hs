
--Testing program.
--CLO refers to the text "Ideals, Varieties, and Algorithms"
--by David Cox, John Little, and Donal O'Shea

import Yaiba.Buchberger
import Yaiba.Ideal
import Yaiba.Polynomial
import Yaiba.Monomial
import qualified Data.Map as M
import Data.Time.Clock (diffUTCTime, getCurrentTime)
{-
-- f = xy^2 + 1
f = fromList [(Monomial [1,2],1),
              (Monomial [0,0],1)]
-- f_1 = xy + 1
f_1 = fromList [(Monomial [1,1],1),
                (Monomial [0,0],1)]
-- f_2 = y + 1
f_2 = fromList [(Monomial [0,1],1),
                (Monomial [0,0],1)]
-- f' = x^2y+xy^2+y^2
f' = fromList [(Monomial [2,1],1),
               (Monomial [1,2],1),
               (Monomial [0,2],1)]
-- f'_1 = xy-1
f'_1 = fromList [(Monomial [1,1],1),
                 (Monomial [0,0],-1)]
-- f'_2 = y^2-1
f'_2 = fromList [(Monomial [0,2],1),
                 (Monomial [0,0],-1)]
-- g = xy^2 - x
g = fromList [(Monomial [1,2],1),
              (Monomial [1,0],-1)]                                   
-- g_1 = xy+1
g_1 = f_1

-- g_2 = y^2-1
g_2 = f'_2
-}
h_1 = fromList [(Monomial [3,0],1),
                (Monomial [1,1],-2)]
h_2 = fromList [(Monomial [2,1],1),
                (Monomial [0,2],-2),
                (Monomial [1,0],1)]
{-
h'_1 = fromList [(Monomial [3,0],1),
                (Monomial [1,1],-2)]
h'_2 = fromList [(Monomial [2,1],1),
                (Monomial [0,2],-2),
                (Monomial [1,0],1)]
-}
h = Ideal $ M.fromList [(h_1,fst (leadTerm h_1)),(h_2,fst (leadTerm h_2))] :: Ideal Grlex
--h' = Ideal [h'_1,h'_2] :: Ideal Lex
{-
i = Ideal [f_1,f,f_2,f',f'_1,g,g_1,g_2,h_1,h_2] :: Ideal Grlex
-}
j_1 = fromList [(Monomial [3,4,2,1,0,0,0,0],-1),(Monomial [0,0,1,0,0,0],1)]
j_2 = fromList [(Monomial [2,3,1,1,0,0,0,0],-1),(Monomial [0,0,0,1,0,0],1)]
j_3 = fromList [(Monomial [1,2,2,1,0,0,0,0],-1),(Monomial [0,0,0,0,1,0],1)]
j_4 = fromList [(Monomial [3,1,3,2,0,0,0,0],-1),(Monomial [0,0,0,0,0,1],1)]
j = Ideal $ M.fromList [(j_1,fst (leadTerm j_1)),(j_2,fst (leadTerm j_2)),(j_3,fst (leadTerm j_3)),(j_4, fst (leadTerm j_4))] :: Ideal Lex

main = do 
{-  -- Example 1 from CLO \S2.3
  putStrLn $ "f/[f_1,f_2] produces remainder " ++ (pLp $ f /. (Ideal [f_1,f_2]))
  -- Example 2 from CLO \S2.3
  putStrLn $ "f'/[f'_1,f'_2] produces remainder " ++ (pLp $ f' /. (Ideal [f'_1,f'_2]))
  -- Example 4 from CLO \S2.3
  putStrLn $ "f'/[f'_2,f'_1] produces remainder " ++ (pLp $ f' /. (Ideal [f'_2,f'_1]))
  -- Example 5 from CLO \S2.3
  putStrLn $ "g/[g_1,g_2] produces remainder " ++ (pLp $ g /. (Ideal [g_1,g_2]))
  putStrLn $ "g/[g_2,g_1] prodcues remainder " ++ (pLp $ g /. (Ideal [g_2,g_1]))-}
  -- Example 1 from CLO \S2.7
  start <- getCurrentTime
  putStrLn $ "A (parallel) non-reduced GB of j is " ++ (show (getPolys $ gB j))
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
  start' <- getCurrentTime
  putStrLn $ "A non-reduced GB of j is " ++ (show (getPolys $ nPgB j))
  end' <- getCurrentTime
  putStrLn $ show (end' `diffUTCTime` start') ++ " elapsed."