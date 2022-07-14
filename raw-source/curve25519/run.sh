rm smt2formulas/*.smt2
racket ast_printer.rkt
rm results 
time for file in (ls smt2formulas/); 
	echo $file >> results; 
	boolector --smt2 -m smt2formulas/$file >> results; 
end