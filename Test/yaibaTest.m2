restart
infile = openIn "output"
infileString = lines read(infile,10000000)
numVars = poly(infileString#0)
monOrder = infileString#1
cputime = infileString#3

R = QQ[x_1..x_numVars,MonomialOrder=>{Lex}] --Weights=>toList(numVars:1),
--R = QQ[x_1..x_numVars,MonomialOrder=>{GRevLex}] --Weights=>toList(numVars:1),
origIdeal = ideal value ("{" | substring((1,#infileString#2 - 2), infileString#2) | "}")
gbList = value ("{" | substring((1,#infileString#4 - 2), infileString#4) | "}")
-- are they the same ideal?
assert(origIdeal == ideal gbList)
-- is it a GB?
ltList = apply(gbList, f -> leadTerm f)
myTime = timing (ltOrigIdeal = leadTerm origIdeal)
<< "Time to compute gb in M2 : " << first myTime << endl;
assert(ideal leadTerm origIdeal == ideal ltList)

I = ideal mingens ideal ltList
J = ideal leadTerm origIdeal
numgens I
numgens J

mingens (ideal leadTerm origIdeal / ideal ltList)
