#!/bin/bash
#<license>
#-------------------------------------------------------------------------------
# Copyright (c) 2007 ConocoPhillips Company
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#-------------------------------------------------------------------------------
#</license>
# build lam-7.0.6 mpi with single underscore on symbols, include shared libs.
temp=$(pwd)
trap "cd ${temp}; echo 'Aborted lam-mpi build'; exit 1" 1 2 3 6 9 11 12 15
PRODUCT=lam-7.0.6
BASE_DIR=${THIRDPARTY_DIR}/${PRODUCT}
BUILD_DIR=${BASE_DIR}/platforms/${CPSEIS_ARCH}
LINKS=$(echo $CPSEIS_ARCH | awk 'BEGIN{FS="_"}{
	for (i=1;i<NF-1;i++)printf("%s_",$i); 
	printf("%s",$(NF-1));}')
LINK_DIR=${BASE_DIR}/platforms/${LINKS}

cd ${THIRDPARTY_DIR}

if [ ! -e ${PRODUCT}.tar.gz ] ; then
        # get lam-7.0.6
	wget http://lam.fries.net/download/files/${PRODUCT}.tar.gz
        tar xvfz ${PRODUCT}.tar.gz
fi

if [ ! -d ${BASE_DIR} ] ; then
        tar xvfz ${PRODUCT}.tar.gz
fi
if [ "$CPSEIS_COMPILER" = "gfortran" ] ; then
	F90=gfortran
	F77=gfortran
	CC=gcc
elif [ "$CPSEIS_COMPILER" = "intel91" ] ; then
	F90=ifort
	F77=ifort
	CC=icc
fi

cd ${BASE_DIR}

CFLAGS="-fPIC -O"
FFLAGS="-fPIC -fno-second-underscore -O"
export F90 CC CFLAGS FFLAGS F77
cd ${BASE_DIR}
mkdir -p ${BUILD_DIR}
if [ "${BUILD_DIR}" != "${LINK_DIR}" ] ; then
        ln -s ${BUILD_DIR} ${LINK_DIR}
fi
FLAGS="--prefix=${BUILD_DIR} --enable-threads --enable-shared libs=gcc \
	--enable-f77 --enable-f90 --enable-cxx"
./configure ${FLAGS} && \
	make clean && \
	make -k
# there are errors in the lam install.  Use -k to bypass errors killing make.
	make -k install
cd $temp
echo "Build lam-mpi complete. Add ${BUILD_DIR}/bin to your path."
