#!/bin/bash
#
mkdir temp
cd temp
../f77split ../minpack.f
#
for FILE in `ls -1 *.f`;
do
  gfortran -c $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
gfortran -c ../hybrdext.f
gfortran -c ../hybrd1ext.f
gfortran -c ../fdjac1ext.f
rm *.f
#
ar qc libminpack.a *.o
rm *.o
#
mv libminpack.a ../
cd ..
rmdir temp
#
echo "Normal end of execution."
