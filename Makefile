#
# Makefile for building lsdyna
# on 'Xeon64 System                     '
# for 'Linux 2.6.18                      '
# using 'Intel Fortran Compiler 13.1 SSE2  '
#
LS_LINK=~/lsdyna_dp2.e
all: lsdyna
	rm -f $(LS_LINK)
	ln -s $(shell pwd)/lsdyna $(LS_LINK)
lsdyna: init_dyn21 dyn21 umat41 dyn21b umat41c couple2other_user dynrfn_user minpack murcia
	ifort -o lsdyna init_once.o init_dyn21.o dynm.o dyn21.o umat41.o dyn21b.o umat41c.o couple2other_user.o dynrfn_user.o adummy_graph.o orderByMetis.o    adummy_msc.o mscapi.o libminpack.a libmurcia.a libdyna.a libbcsext4.a liblsda.a liblssecurity.a  liblcpack.a libspooles.a libcparse.a libmf2.a liblsm.a liblscrypt.a libresurf.a libsfg.a libmetis.a libim_rotor_dynamics.a libarpack.a  libfemster_wrap.a libfemster_wrap2d.a libfemster_wrap1d.a libfemster.a libfemster2d.a libfemster1d.a libpfem.a libmetis.a libblas.a liblapack.a libfftw3.a  intel64_131_libansysdp.a  libdyna.a libblas.a liblapack.a libmetis.a  -i-static -L/usr/X11R6/lib64 -lX11 -openmp  -lstdc++ -lrt -lstdc++ 
#
init_dyn21: init_dyn21.f nhisparm.inc 
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. init_dyn21.f
dyn21: dyn21.f nhisparm.inc 
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. dyn21.f
umat41: umat41.f
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. umat41.f
dyn21b: dyn21b.f nhisparm.inc 
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. dyn21b.f
umat41c: umat41c.f nhisparm.inc
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. umat41c.f
murcia: murcia.cc
	g++ -std=c++2a -c murcia.cc -o murcia.o
	ar qc libmurcia.a murcia.o
couple2other_user: couple2other_user.f 
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. couple2other_user.f
dynrfn_user: dynrfn_user.f 
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. dynrfn_user.f
minpack: minpack.f fdjac1ext.f hybrdext1.f hybrdext.f
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. minpack.f
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. fdjac1ext.f
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. hybrdext.f
	ifort -c -safe_cray_ptr -assume byterecl,buffered_io,protect_parens -warn nousage -zero -ftz -fp-model precise -mP2OPT_hpo_dist_factor=21 -diag-disable 10212,10010 -xSSE3 -align array16byte -nopad -openmp -i8 -r8  -DINTEL -DAdd_ -O2 -I. hybrdext1.f
	ar qc libminpack.a hybrdext.o hybrdext1.o fdjac1ext.o minpack.o
clean:
	rm -f init_dyn21.o
	rm -f dyn21.o
	rm -f umat41.o
	rm -f dyn21b.o
	rm -f umat41c.o
	rm -f couple2other_user.o
	rm -f dynrfn_user.o
	rm -f hybrdext.o
	rm -f hybrdext1.o
	rm -f fdjac1ext.o
	rm -f minpack.o
	rm -f libminpack.a
	rm -f murcia.o
	rm -f libmurcia.a
	rm -f $(LS_LINK)
	rm -f lsdyna
	rm -rf build
