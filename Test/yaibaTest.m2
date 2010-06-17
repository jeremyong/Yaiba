restart
infile = openIn "output"
infileString = lines read(infile,10000000)
numVars = poly(infileString#0)
monOrder = infileString#1
R = QQ[x_1..x_numVars,MonomialOrder=>Lex]
origIdeal = ideal value ("{" | substring((1,#infileString#2 - 2), infileString#2) | "}")
gbList = value ("{" | substring((1,#infileString#3 - 2), infileString#3) | "}")
-- are they the same ideal?
assert(origIdeal == ideal gbList)
-- is it a GB?
ltList = apply(gbList, f -> leadTerm f)
assert(ideal leadTerm origIdeal == ideal ltList)