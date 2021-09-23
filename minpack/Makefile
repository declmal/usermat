test: hybrd1.f hybrd.f fdjac1.f
	mkdir -p build
	gcc -c -Wall f77split.c -o build/f77split
	chmod u+x build/f77split
	gfortran -c minpack.f 
	ar qc libminpack.a build/*.o
