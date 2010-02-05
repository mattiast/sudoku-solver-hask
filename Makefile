sudoku: src/sudoku.hs src/heuristic.hs blength.o 
	ghc --make -O2 -XForeignFunctionInterface blength.o src/sudoku.hs src/heuristic.hs  -o sudoku
	strip sudoku
deduction: src/sudoku.hs src/deduction.hs blength.o 
	ghc --make -O2 -XForeignFunctionInterface blength.o src/sudoku.hs src/deduction.hs  -o sudoku
	strip sudoku
profile: src/sudoku.hs src/heuristic.hs blength.o 
	ghc --make -O2 -XForeignFunctionInterface -prof -auto-all blength.o src/sudoku.hs src/heuristic.hs -o prof_sud
	./prof_sud +RTS -p < samples/al_escargot
blength.o: src/blength.c
	gcc -c src/blength.c -o blength.o
clean:
	rm -f sudoku.hi sudoku.o blength.o heuristic.o heuristic.hi sudoku prof_sud*
test: sudoku
	echo time > times.dat
	@for i in samples/garden/* ;\
	do /usr/bin/time -o times.dat -a -f "%U" ./sudoku < $$i > /dev/null ;\
	   echo $$i ;\
	done
