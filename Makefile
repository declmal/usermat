#
# Makefile for building lsdyna
# on 'Xeon64 System                     '
# for 'Linux 2.6.18                      '
# using 'Intel Fortran Compiler 13.1 SSE2  '
#
LS_LINK=~/lsdyna_dp2.e
all: lsdyna
	mkdir -p build
	mv init_dyn21.o build
	mv dyn21.o build
	mv umat41.o huild
	mv dyn21b.o build
	mv umat41c.o build
	mv couple2other_user.o build
	mv dynrfn_user.o build
	mv lsdyna build
	rm -f $(LS_LINK)
	ln -s $(shell pwd)/build/lsdyna $(LS_LINK)
lsdyna: init_dyn21.o dyn21.o umat41.o dyn21b.o couple2other_user.o dynrfn_user.o 
	ifort -o lsdyna init_once.o init_dyn21.o dynm.o dyn21.o umat41.o dyn21b.o couple2other_user.o dynrfn_user.o adummy_graph.o orderByMetis.o    adummy_msc.o mscapi.o libdyna.a libbcsext4.a liblsda.a liblssecurity.a  liblcpack.a libspooles.a libcparse.a libmf2.a liblsm.a liblscrypt.a libresurf.a libsfg.a libmetis.a libim_rotor_dynamics.a libarpack.a  libfemster_wrap.a libfemster_wrap2d.a libfemster_wrap1d.a libfemster.a libfemster2d.a libfemster1d.a libpfem.a libmetis.a libblas.a liblapack.a libfftw3.a  intel64_131_libansysdp.a  libdyna.a libblas.a liblapack.a libmetis.a  -i-static -L/usr/X11R6/lib64 -lX11 -openmp  -lstdc++ -lrt -lstdc++ 
#
init_dyn21.o: init_dyn21.f nhisparm.inc 
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. init_dyn21.f
dyn21.o: dyn21.f nhisparm.inc 
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. dyn21.f
umat41.o: umat41.f
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. umat41.f
dyn21b.o: dyn21b.f nhisparm.inc 
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. dyn21b.f
couple2other_user.o: couple2other_user.f 
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. couple2other_user.f
dynrfn_user.o: dynrfn_user.f 
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. dynrfn_user.f
clean:
	rm -rf build
