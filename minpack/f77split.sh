#! /bin/bash
#
gcc -c -Wall f77split.c
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gcc f77split.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm f77split.o
#
chmod u+x a.out
mv a.out ~/binc/f77split
#
echo "Normal end of execution."
