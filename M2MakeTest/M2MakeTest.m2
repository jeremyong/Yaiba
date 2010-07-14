makePolynomial = method()
makePolynomial(RingElement) := (f) -> (
   (mons,coeffs) := coefficients(f, Variables=>gens ring f);
   mons = flatten entries mons;
   coeffs = flatten entries coeffs;
   tempList := apply(#mons, i -> (toExternalString flatten exponents mons#i, coeffs#i));
   tempList = apply(tempList, p -> ("[" | substring((1,length p#0 - 2), p#0) | "]", toExternalString p#1));
   tempStr := "P.fromList [" | fold(apply(tempList, p -> "(M.fromList " | p#0 | "," | p#1 | "),"), (i,j) -> i | j);
   substring((0,length tempStr - 1), tempStr) | "]"
)

makeExample = method()
makeExample(Ideal) := (I) -> (
   gensList := I_*;
   scan(#gensList, i -> << "p_" << i+1 << " = " << makePolynomial(gensList#i) << endl);
   << "p' = [";
   scan(#gensList-1, i -> << "p_" << i+1 << ",");
   << "p_" << #gensList << "]" << endl;
   << "p = DS.fromList (initPolySugars p')" << endl;
   << "pideal = I $ DV.fromList (initSugars p') :: Ideal M.Lex" << endl;
)

makeRandomExample = (n,m,k) -> (
  R := ZZ/7[x_1..x_n];
  S := QQ[x_1..x_n];
  substitute(ideal apply(k, i -> random(m,R)),S)
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
--gbI = gb I
makeExample(I)
makePolynomial(first I_*)

restart
load "../M2MakeTest/M2MakeTest.m2"
I = makeRandomExample(4,3,6)
time Igb = gb I
gens gb I
makeExample I

restart
load "../M2MakeTest/M2MakeTest.m2"
R = QQ[t,u,x,y,z,MonomialOrder=>Lex]
I = ideal {x+y+z+t+u, x*y+y*z+z*t+t*u+u*x, x*y*z+y*z*t+z*t*u+t*u*x+u*x*y, x*y*z*t+y*z*t*u+z*t*u*x+t*u*x*y+u*x*y*z, x*y*z*t*u-1}
makeExample I

restart
load "../M2MakeTest/M2MakeTest.m2"
R = QQ[t,u,x,y,z,MonomialOrder=>Lex]
I = ideal {y*(1+x^2)^4 - 2*(5+19*x^2-45*x^4+x^6-4*x^8), z*(1+x^2)^4-2*(x+51*x^3+3*x^5+17*x^7)}
makeExample I

restart
load "../M2MakeTest/M2MakeTest.m2"
R = QQ[T,x,y,z,a,b,c,d,e,MonomialOrder=>Lex]
I = ideal {-y^82*a+x^32*z^23,x^45-y^13*z^21*b,y^33*z^12-x^41*c,-y^33*z^12*d+x^22,x^5*y^17*z^22*e-1,x*y*z*T-1}
gbTrace = 3
time Igb = gb I
makeExample I

restart
load "../M2MakeTest/M2MakeTest.m2"
R = QQ[x,y,z,u,t]
I = ideal {2*x^2+2y^2+2z^2+2t^2+u^2-u,
           x*y+2*y*z+2*z*t+2*t*u-t,
	   2*x*z+2*y*t+t^2+2*z*u-z,
	   2*x*t+2*z*t+2*y*u-y,
	   2*x+2*y+2*z+2*t+u-1}
gbTrace = 3
time Igb = gb I