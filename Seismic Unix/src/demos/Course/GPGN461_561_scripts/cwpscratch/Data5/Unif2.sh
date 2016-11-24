#! /bin/sh

#set -x

# set parameters here
method=mono                     # interpolation method
r1=10
r2=10
unifpar=unif2.par


unif2 < unif2.ascii par=$unifpar method=$method |
 smooth2 par=$unifpar r1=$r1 r2=$r2 > junk.bin

cp junk.bin unewvelzx.bin

transp par=$unifpar < unewvelzx.bin > unewvelxz.bin

echo "Writing out uniformly sampled v(z,x) file:  unewvelzx.bin"
echo "Writing out uniformly sampled v(x,z) file:  unewvelxz.bin"
echo
echo "You may hand edit the files $unifpar and unif2.ascii"
echo "and run the shell script Unif2.sh to make updated"
echo "v(z,x) and v(x,z) files."

exit 0
