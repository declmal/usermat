#!/bin/bash
#
mkdir temp
cd temp
~/binc/f77split ../minpack.f
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
mv libminpack.a ~/libf77
cd ..
rmdir temp
#
echo "Normal end of execution."
