#!/bin/bash
#
mkdir temp
cd temp
../f77split ../minpack.f
cp ../hybrdext.f hybrdext.f
cp ../hybrd1ext.f hybrd1ext.f
cp ../fdjac1ext.f fdjac1ext.f
#
for FILE in `ls -1 *.f`;
do
  gfortran -c $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
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
