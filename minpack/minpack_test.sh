#! /bin/bash
#
gfortran -c -Wall minpack_test.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran minpack_test.o -L$HOME/libf77 -lminpack
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm minpack_test.o
#
mv a.out minpack_test
./minpack_test > minpack_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm minpack_test
#
echo "Normal end of execution."
