
--Testing program.
--CLO refers to the text "Ideals, Varieties, and Algorithms"
--by David Cox, John Little, and Donal O'Shea

import Yaiba.Buchberger
import Yaiba.Ideal
import Yaiba.Sugar
import Yaiba.Polynomial
import Yaiba.Monomial
import qualified Data.Map as DM
import Data.Time.Clock (diffUTCTime, getCurrentTime)
{-
-- f = xy^2 + 1
f = fromList [(M [1,2],1),
              (M [0,0],1)]
-- f_1 = xy + 1
f_1 = fromList [(M [1,1],1),
                (M [0,0],1)]
-- f_2 = y + 1
f_2 = fromList [(M [0,1],1),
                (M [0,0],1)]
-- f' = x^2y+xy^2+y^2
f' = fromList [(M [2,1],1),
               (M [1,2],1),
               (M [0,2],1)]
-- f'_1 = xy-1
f'_1 = fromList [(M [1,1],1),
                 (M [0,0],-1)]
-- f'_2 = y^2-1
f'_2 = fromList [(M [0,2],1),
                 (M [0,0],-1)]
-- g = xy^2 - x
g = fromList [(Monomial [1,2],1),
              (Monomial [1,0],-1)]                                   
-- g_1 = xy+1
g_1 = f_1

-- g_2 = y^2-1
g_2 = f'_2
-}{-
h_1 = fromList [(M [3,0],1),
                (M [1,1],-2)]
h_2 = fromList [(M [2,1],1),
                (M [0,2],-2),
                (M [1,0],1)]
-}
h'_1 = fromList [(M [3,0],1),
                 (M [1,1],-2)]
h'_2 = fromList [(M [2,1],1),
                 (M [0,2],-2),
                 (M [1,0],1)]

h = I $ DM.fromList [(h'_1,computeSugar h'_1 0),(h'_2,computeSugar h'_2 0)] :: Ideal Grlex
--h' = Ideal [h'_1,h'_2] :: Ideal Lex
{-
i = Ideal [f_1,f,f_2,f',f'_1,g,g_1,g_2,h_1,h_2] :: Ideal Grlex
-}
j_1 = fromList [(M [3,4,2,4,0,0,0,0],-1),(M [0,0,0,0,1,0,0,0],1)]
j_2 = fromList [(M [2,3,4,1,0,0,0,0],-1),(M [0,0,0,0,0,1,0,0],1)]
j_3 = fromList [(M [4,2,2,1,0,0,0,0],-1),(M [0,0,0,0,0,0,1,0],1)]
j_4 = fromList [(M [3,2,3,4,0,0,0,0],-1),(M [0,0,0,0,0,0,0,1],1)]
j = I $ DM.fromList [(j_1,computeSugar j_1 0),(j_2,computeSugar j_2 1),(j_3,computeSugar j_3 2),(j_4,computeSugar j_4 3)] :: Ideal Lex

main = do {-
  -- Example 1 from CLO \S2.3
  putStrLn $ "f/[f_1,f_2] produces remainder " ++ (show $ f /. (I $ DM.fromList [(f_1,computeSugar f_1 0),(f_2,computeSugar f_2 0)] :: Ideal Lex))
  -- Example 2 from CLO \S2.3
  putStrLn $ "f'/[f'_1,f'_2] produces remainder " ++ (show $ f' /. (I $ DM.fromList [(f'_1,computeSugar f'_1 0),(f'_2,computeSugar f'_2 0)] :: Ideal Grlex))
  -- Example 4 from CLO \S2.3
  putStrLn $ "f'/[f'_2,f'_1] produces remainder " ++ (show $ f' /. (I $ DM.fromList [(f'_2,computeSugar f'_2 0),(f'_1,computeSugar f'_1 0)] :: Ideal Grlex))
  -- Example 5 from CLO \S2.3
  putStrLn $ "g/[g_1,g_2] produces remainder " ++ (pLp $ g /. (Ideal [g_1,g_2]))
  putStrLn $ "g/[g_2,g_1] prodcues remainder " ++ (pLp $ g /. (Ideal [g_2,g_1]))-}
  -- Example 1 from CLO \S2.7
  start <- getCurrentTime
  putStrLn $ "A (parallel) non-reduced GB of j is " ++ (show (getPolys $ gB j))
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
  {-start' <- getCurrentTime
  putStrLn $ "A non-reduced GB of j is " ++ (show (getPolys $ nPgB j))
  end' <- getCurrentTime
  putStrLn $ show (end' `diffUTCTime` start') ++ " elapsed."-}