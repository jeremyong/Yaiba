makePolynomial = method()
makePolynomial(RingElement) := (f) -> (
   (mons,coeffs) := coefficients(f, Variables=>gens R);
   mons = flatten entries mons;
   coeffs = flatten entries coeffs;
   tempList := apply(#mons, i -> (toExternalString flatten exponents mons#i, coeffs#i));
   tempList = apply(tempList, p -> ("[" | substring((1,length p#0 - 2), p#0) | "]", toExternalString p#1));
   tempStr := "fromList [" | fold(apply(tempList, p -> "(M " | p#0 | "," | p#1 | "),"), (i,j) -> i | j);
   substring((0,length tempStr - 1), tempStr) | "]"
)

makeExample = method()
makeExample(Ideal) := (I) -> (
   gensList := I_*;
   scan(#gensList, i -> << "p_" << i+1 << " = " << makePolynomial(gensList#i) << endl);
   << "I $ initSugars $ DS.fromList [";
   scan(#gensList-1, i -> << "p_" << i+1 << ",");
   << "p_" << #gensList << "] :: Ideal Grlex" << endl;
)

end

restart
load "M2MakeTest.m2"
matrixSize = 4
R = QQ[x_1..x_(2*(matrixSize^2)),MonomialOrder=>{Weights=>toList((2*matrixSize^2):1),Lex}]
R = QQ[x_1..x_(2*(matrixSize^2))]
varsA = take(gens R,matrixSize^2)
varsB = drop(gens R,matrixSize^2)
A = matrix pack(varsA,matrixSize)
B = matrix pack(varsB,matrixSize)
I = ideal flatten entries (A*B - B*A)
gbTrace = 3
gbI = gb I

makeExample(I)
makePolynomial(first I_*)
