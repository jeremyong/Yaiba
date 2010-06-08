
--Testing program.

import Yaiba.Ideal
import Yaiba.Polynomial
import Yaiba.Monomial

a = fromList [(Monomial [3],3),
              (Monomial [2],-2),
              (Monomial [1],4),
              (Monomial [0],-3)] :: Polynomial Lex

b = fromList [(Monomial [2],1),
              (Monomial [1],3),
              (Monomial [0],3)] :: Polynomial Lex

c = fromList [(Monomial [2,0],1),
              (Monomial [1,1],2),
              (Monomial [0,2],1)] :: Polynomial Lex
                                    
d = fromList [(Monomial [1,0],1),
              (Monomial [0,1],1)] :: Polynomial Lex

main = do --putStrLn $ "a = " ++ (pLp a)
          --putStrLn $ "b = " ++ (pLp b)
          --putStrLn $ "a+b = " ++ (pLp (a+b))
          --putStrLn $ "a*b = " ++ (pLp (a*b))
          --putStrLn $ "leadTerm a" ++ (show (leadTerm a))
          --putStrLn $ "leadTerm b" ++ (show (leadTerm b))
          --putStrLn $ "lTa/lTb" ++ (show ((fst (leadTerm a)) * (recip (fst (leadTerm b)))))
          --let x = fst (quoRem a b)
          --let y = snd (quoRem a b)
          --putStrLn $ "c/d = " ++ "("++pLp x++"," ++ pLp y++")"
          --putStrLn $ "quo * b = " ++ (pLp (x*b+y))
          putStrLn $ "also = " ++ (pLp ((divIdeal c [d])))
          --putStrLn $ "quorem " ++ (pLp (fst (quoRem c d)))