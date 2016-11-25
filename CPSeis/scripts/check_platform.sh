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
# check the build of cpseis for a particular platform.
ERROR=0
if [ -z $1 ] ; then
	echo "$(basename $0): builds a cpseis installation for a particular platform."
	echo "	usage:	$(basename $0) platform (platform = arch_compiler_mpi)"
	echo "		where platform is one of the supported builds."
	echo "		and..."
	echo "			[arch] is one of [i686 or x86_64]"
	echo "			[compiler] is one of [gfortran or intel91]"
	echo "			[mpi] is one of [none or lam or mpich2]"
	echo "		Be sure you have prebuilt LAM or MPICH2 (or none) using the"
	echo "		scripts in the .../cpseis/scripts directory and "
	echo "		set up the proper environment for your compiler"
	echo "		and mpi in your .bashrc file"
	echo ""
	if [ ! -z $CPSEIS_ARCH ] ; then
		TEMP=$CPSEIS_ARCH
		echo "Checking the $CPSEIS_ARCH build of CPSeis..."
		sleep 1
	else
		echo "Either set up a CPSEIS_ARCH environment variable or"
		echo "specify the platform you want to check."
		exit 1
	fi
else
	TEMP=$1
	echo "Checking the $TEMP build of CPSeis..."
	sleep 1
fi
TEMP=$(echo $TEMP | sed 's/x86_64/x86+64/g')
ARCHC=$(echo $TEMP | awk 'BEGIN{FS="_"}{print $1}' | sed 's/+/_/g')
COMPC=$(echo $TEMP | awk 'BEGIN{FS="_"}{print $2}')
MPICC=$(echo $TEMP | awk 'BEGIN{FS="_"}{print $3}')
TEMP=${ARCHC}_${COMPC}_${MPICC}
if [ "$MPICC" = "mpich2" ] ; then
	if ! (which mpd &>/dev/null ) ; then
		echo "mpich2 not set up and in your path"
		ERROR=$((ERROR+1))
	else
		echo "mpich2 installed at: $(dirname $(which mpd))"
		if ! (echo $(dirname $(which mpd)) | grep thirdparty ) ; then
			echo "mpich2 is not coming from your thirdparty directory."
			ERROR=$((ERROR+1))
		fi
	fi
elif [ "$MPICC" = "lam" ] ; then
	if ! (which lamboot &>/dev/null ) ; then
		echo "lam not set up and in your path"
		ERROR=$((ERROR+1))
	else
		echo "lam mpi installed at: $(dirname $(which lamboot))"
		if ! (echo $(dirname $(which lamboot)) | grep thirdparty ) ; then
			echo "lam mpi is not coming from your thirdparty directory."
			ERROR=$((ERROR+1))
		fi
	fi
elif [ "$MPICC" = "none" ] ; then
	echo "no compiler setup within MPI."
			ERROR=$((ERROR+1))
else
	echo "Not a supported mpi"
			ERROR=$((ERROR+1))
fi

if [ "$COMPC" = "gfortran" ] ; then
	if ! (which gfortran &>/dev/null ) ; then
		echo "gfortran not set up and in your path"
			ERROR=$((ERROR+1))
	else
		echo "gfortran installed at: $(dirname $(which gfortran))"
	fi
elif [ "$COMPC" = "intel91" ] ; then
	if ! (which ifort &>/dev/null ) ; then
		echo "intel fortran not set up and in your path"
			ERROR=$((ERROR+1))
	else
		echo "intel installed at: $(dirname $(which ifort))"
	fi
else
	echo "Not supported compiler"
			ERROR=$((ERROR+1))
fi

if [ "$ARCHC" = "i686" ] ; then
	echo "A 32 bit installation will be required." 
elif [ "$ARCHC" = "x86_64" ] ; then
	echo "A 64 bit installation will be required."
else
	echo "This is not a supported architecture"
fi

if ! (which javac &> /dev/null ) ; then
	echo "Make sure you install the JAVA DEVELOPMENT KIT (jdk) in your thirdparty directory."
			ERROR=$((ERROR+1))
else
	echo "java compiler installed at: $(dirname $(which javac))"
	if ! (echo $(dirname $(which javac)) | grep thirdparty ) ; then
		echo "JAVA compiler is not coming from your thirdparty directory."
			ERROR=$((ERROR+1))
	fi
fi

if ! (which cfe &> /dev/null ) ; then
	echo "PATH is not set up to find your installation of CPSeis' front end, cfe."
			ERROR=$((ERROR+1))
else
	echo "cfe installed at: $(dirname $(which cfe))"
	if ! (echo $(dirname $(which cfe)) | grep cpseis) ; then
		echo "cfe is not coming from your cpseis directory."
			ERROR=$((ERROR+1))
	fi
fi

if ! (which icps &> /dev/null ) ; then
	echo "PATH is not set up to find your installation of CPSeis' icps."
			ERROR=$((ERROR+1))
else
	echo "icps installed at: $(dirname $(which icps))"
	if ! (echo $(dirname $(which icps)) | grep cpseis) ; then
		echo "icps is not coming from your cpseis directory."
			ERROR=$((ERROR+1))
	fi
fi

if [ "$CPSEIS_ARCH" != "$TEMP" ] ; then
			ERROR=$((ERROR+1))
	echo "You need to set up your .bashrc with:"
	echo "export CPSEIS_ARCH=$TEMP"
fi
echo Architecture=$ARCHC Compiler_choice=$COMPC MPI_choice=$MPICC
echo "Checking the make environment variables needed:"
echo "MPI=$MPI"
echo "CPSEIS_COMPILER_BASE=$CPSEIS_COMPILER_BASE"
[ "$CPSEIS_COMPILER_BASE" = "intel" ] && echo "ifort=$(which ifort)"


if [ "$((ERROR > 0 ))" = "1" ] ; then
	echo "There are $ERROR error(s) in your setup."
else
	echo "There are $ERROR errors in your setup. All looks good."
fi
