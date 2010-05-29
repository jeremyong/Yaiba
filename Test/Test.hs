
--Testing program.

import Yaiba.Ideal
import Yaiba.Polynomial
import Yaiba.Monomial
import Data.Map
a = Polynomial $ fromList [(Monomial [3,2,55,0,1],-2/5),
                           (Monomial [2,0,0,3,23],34),
                           (Monomial [0,0,32,1,0],14.4),
                           (Monomial [0,3,4,0,9],22)] :: Polynomial Lex

b = Polynomial $ fromList [(Monomial [0,0,35,92,1],13/11),
                           (Monomial [1,40,4,3,23],-4.2),
                           (Monomial [0,0,0,1,3],44),
                           (Monomial [2,55,0,23,4],30),
                           (Monomial [23,0,4,545,55],2)] :: Polynomial Lex
                                                            
main = do putStrLn $ "a = " ++ (show a)
          putStrLn $ "b = " ++ (show b)
          putStrLn $ "a+b = " ++ (show (a+b))
          putStrLn $ "a*b = " ++ (show (a*b))