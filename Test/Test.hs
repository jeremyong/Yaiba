
--Testing program.
--CLO refers to the text "Ideals, Varieties, and Algorithms"
--by David Cox, John Little, and Donal O'Shea

import Yaiba.Buchberger
import Yaiba.Ideal
import Yaiba.Polynomial
import Yaiba.Monomial

-- f = xy^2 + 1
f = fromList [(Monomial [1,2],1),
              (Monomial [0,0],1)] :: Polynomial Lex
-- f_1 = xy + 1
f_1 = fromList [(Monomial [1,1],1),
                (Monomial [0,0],1)] :: Polynomial Lex
-- f_2 = y + 1
f_2 = fromList [(Monomial [0,1],1),
                (Monomial [0,0],1)] :: Polynomial Lex
-- f' = x^2y+xy^2+y^2
f' = fromList [(Monomial [2,1],1),
               (Monomial [1,2],1),
               (Monomial [0,2],1)] :: Polynomial Lex
-- f'_1 = xy-1
f'_1 = fromList [(Monomial [1,1],1),
                 (Monomial [0,0],-1)] :: Polynomial Lex
-- f'_2 = y^2-1
f'_2 = fromList [(Monomial [0,2],1),
                 (Monomial [0,0],-1)] :: Polynomial Lex
-- g = xy^2 - x
g = fromList [(Monomial [1,2],1),
              (Monomial [1,0],-1)] :: Polynomial Lex                                   
-- g_1 = xy+1
g_1 = f_1

-- g_2 = y^2-1
g_2 = f'_2

h_1 = fromList [(Monomial [3,0],1),
                (Monomial [1,1],-2)]
h_2 = fromList [(Monomial [2,1],1),
                (Monomial [0,2],-2),
                (Monomial [1,0],1)]
h = Ideal [h_1,h_2] :: Ideal Grlex

main = do 
  -- Example 1 from CLO \S2.3
  putStrLn $ "f/[f_1,f_2] produces remainder " ++ (pLp $ f /. (Ideal [f_1,f_2]))
  -- Example 2 from CLO \S2.3
  putStrLn $ "f'/[f'_1,f'_2] produces remainder " ++ (pLp $ f' /. (Ideal [f'_1,f'_2]))
  -- Example 4 from CLO \S2.3
  putStrLn $ "f'/[f'_2,f'_1] produces remainder " ++ (pLp $ f' /. (Ideal [f'_2,f'_1]))
  -- Example 5 from CLO \S2.3
  putStrLn $ "g/[g_1,g_2] produces remainder " ++ (pLp $ g /. (Ideal [g_1,g_2]))
  putStrLn $ "g/[g_2,g_1] prodcues remainder " ++ (pLp $ g /. (Ideal [g_2,g_1]))
  -- Example 1 from CLO \S2.7
  putStrLn $ "A non-reduced GB of h is " ++ (show (getPolys $ gB h))