restart
infile = openIn "output"
infileString = lines read(infile,10000000)
numVars = poly(infileString#0)
monOrder = infileString#1
<<<<<<< .merge_file_78g5Qf
R = QQ[x_1..x_numVars,MonomialOrder=>{Lex}] --Weights=>toList(numVars:1),
=======
<<<<<<< HEAD
R = QQ[x_1..x_numVars,MonomialOrder=>Lex]
=======
R = QQ[x_1..x_numVars,MonomialOrder=>{Lex}] --Weights=>toList(numVars:1),
>>>>>>> 84616b6a7d42d40d4021be969ec904cc8b6984ed
>>>>>>> .merge_file_sOR1pf
origIdeal = ideal value ("{" | substring((1,#infileString#2 - 2), infileString#2) | "}")
gbList = value ("{" | substring((1,#infileString#3 - 2), infileString#3) | "}")
-- are they the same ideal?
assert(origIdeal == ideal gbList)
-- is it a GB?
ltList = apply(gbList, f -> leadTerm f)
assert(ideal leadTerm origIdeal == ideal ltList)

I = ideal mingens ideal ltList
J = ideal leadTerm origIdeal
numgens I
numgens J

mingens (ideal leadTerm origIdeal / ideal ltList)
