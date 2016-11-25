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
# This will do the actual installation
HERE=$(pwd)
if [ -z $CPSEIS_INSTALLATION_CALLED_ME ] ; then
	echo "Run the install_cpseis script --- it will call this one."
	echo "Otherwise, a log will not be kept of your installation for troubleshooting."
	exit 1
fi
exitnow() {
	if [ "$EXIT" == "0" ] ; then
		echo "Normal exit.  Hit return to exit"
		read prompt
		exit $EXIT
	fi
	echo "Error detected.  Hit return to exit"
	read prompt
	exit $EXIT
}

temp=$(pwd)
ERROR=0

if [ -z ${CPSEIS_INSTALL_DIR} ] ; then
	echo "Define a CPSEIS_INSTALL_DIR first"
	echo "example: export CPSEIS_INSTALL_DIR=/yourdir/cpseis"
	echo "This cannot be the root directory, and do not create it."
	echo "The installation will create this directory for you."
	ERROR=1
fi

if [ -z ${THIRDPARTY_DIR} ] ; then
	echo "Define a directory for thirdparty applications such as"
	echo "fftw, mpich2, and java jdk.  Save this environment variable"
	echo "as THIRDPARTY_DIR.  ex: export THIRDPARTY_DIR=/yourdir/thirdparty."
	ERROR=$((ERROR+1))
else
	mkdir -p ${THIRDPARTY_DIR}
fi

[ "$((ERROR > 0 ))" = "1" ] && EXIT=1 && exitnow


if [ ! -d ${CPSEIS_INSTALL_DIR} ] ; then
	BASE=$(dirname $CPSEIS_INSTALL_DIR)
	cd $BASE
	if [ ! -e cpseis.tgz ] ; then
		# get cpseis.tgz
		ERROR=$((ERROR+1))
		echo "Get cpseis.tgz tar file from: wget https://wush.net/trac/cpseis/browser/trunk/cpseis.tgz"
		echo "and put it in the directory above $CPSEIS_INSTALL_DIR"
	fi
	tar xvfz cpseis.tgz
fi

[ "$((ERROR > 0 ))" = "1" ] && EXIT=1 && exitnow

cd $CPSEIS_INSTALL_DIR
cp ${HERE}/cpseis_bashrc.sh ${CPSEIS_INSTALL_DIR}/etc/cpseis_bashrc.sh
chmod 755 ${CPSEIS_INSTALL_DIR}/etc/cpseis_bashrc.sh
chmod 755 ${CPSEIS_INSTALL_DIR}/etc/setup_cpseis_environment.sh

. ${HERE}/cpseis_bashrc.sh

chmod 755 ${CPSEIS_INSTALL_DIR}/scripts/*

echo "Default architecture will install.  If you want to change it, go to "
echo "${CPSEIS_INSTALL_DIR}/scripts/cpseis_bashrc.sh and change CPSEIS_ARCH."
echo "CPSEIS_ARCH=$CPSEIS_ARCH"
TEMP=${CPSEIS_ARCH}
TEMP=$(echo $TEMP | sed 's/x86_64/x86+64/g')
ARCHC=$(echo $TEMP | awk 'BEGIN{FS="_"}{print $1}' | sed 's/+/_/g')
COMPC=$(echo $TEMP | awk 'BEGIN{FS="_"}{print $2}')
MPICC=$(echo $TEMP | awk 'BEGIN{FS="_"}{print $3}')
TEMP=${ARCHC}_${COMPC}_${MPICC}
echo "Architecture is: $ARCHC"
echo "Compiler is    : $COMPC"
echo "MPI is         : $MPICC"
echo "Third party dir: $THIRDPARTY_DIR"
echo "Hit Enter to continue..."
read prompt
ERROR=0

if ! (which javac &> /dev/null ) ; then
	echo "Make sure you install the JAVA DEVELOPMENT KIT (jdk) in your thirdparty directory."
	ERROR=$((ERROR+1))
	if [ "$((ERROR > 0 ))" = "1" ] ; then
		if ( ${CPSEIS_INSTALL_DIR}/scripts/build_java.sh ) ; then
			ERROR=0
		else
			echo "Make sure you install the JAVA DEVELOPMENT KIT (jdk) in your thirdparty directory."
			ERROR=$((ERROR+1))
		fi
	fi
else
	echo "java compiler installed at: $(dirname $(which javac))"
	if ! (echo $(dirname $(which javac)) | grep thirdparty ) ; then
		echo "JAVA compiler is not coming from your thirdparty directory."
		ERROR=$((ERROR+1))
	fi
	if [ "$((ERROR > 0 ))" = "1" ] ; then
		if ( ${CPSEIS_INSTALL_DIR}/scripts/build_java.sh ) ; then
			ERROR=0
		else
			echo "Error with java development kit installation"
			ERROR=$((ERROR+1))
		fi
	fi
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
elif [ "$COMPC" = "intel" ] ; then
	if ! (which ifort &>/dev/null ) ; then
		echo "intel fortran not set up and in your path"
		ERROR=$((ERROR+1))
	else
		echo "intel installed at: $(dirname $(which ifort))"
	fi
else
	echo "No supported Fortran 9x compiler"
	ERROR=$((ERROR+1))
fi

[ "$((ERROR > 0 ))" = "1" ] && EXIT=1 && exitnow

if [ "$MPICC" = "mpich2" ] ; then
	if ! (which mpd &>/dev/null ) ; then
		echo "mpich2 not set up and in your path"
		ERROR=$((ERROR+1))
		if [ "$((ERROR > 0 ))" = "1" ] ; then
			if ( ${CPSEIS_INSTALL_DIR}/scripts/build_mpich2.sh ) ; then
				ERROR=0
			else
				echo "Error running build_mpich2.sh script"
				ERROR=$((ERROR+1))
			fi
		fi
	else
		echo "mpich2 installed at: $(dirname $(which mpd))"
		if ! (echo $(dirname $(which mpd)) | grep thirdparty ) ; then
			echo "mpich2 is not coming from your thirdparty directory."
			ERROR=$((ERROR+1))
		fi
	fi
	if [ "$((ERROR > 0 ))" = "1" ] ; then
		if ( ${CPSEIS_INSTALL_DIR}/scripts/build_mpich2.sh ) ; then
			ERROR=0
		else
			echo "Error running build_mpich2.sh script"
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
	if [ "$((ERROR > 0 ))" = "1" ] ; then
		if ( ${CPSEIS_INSTALL_DIR}/scripts/build_lam.sh ) ; then
			ERROR=0
		else
			echo "Error running build_lam.sh script"
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

[ "$((ERROR > 0 ))" = "1" ] && EXIT=1 && exitnow

if [ ! -f ${THIRDPARTY_DIR}/.built_fftw ] ; then
	echo "fftw not set up and in your path"
	ERROR=$((ERROR+1))
	if [ "$((ERROR > 0 ))" = "1" ] ; then
		if ( ${CPSEIS_INSTALL_DIR}/scripts/build_fftw.sh ) ; then
			ERROR=0
		else
			echo "Error running build_fftw.sh script."
			ERROR=$((ERROR+1))
		fi
	fi
fi

if [ "$((ERROR > 0 ))" = "1" ] ; then
		if ( ${CPSEIS_INSTALL_DIR}/scripts/build_fftw.sh ) ; then
			ERROR=0
		else
			echo "Error running build_fftw.sh script."
			ERROR=$((ERROR+1))
		fi
fi

[ "$((ERROR > 0 ))" = "1" ] && EXIT=1 && exitnow

cd ${CPSEIS_INSTALL_DIR}
${CPSEIS_INSTALL_DIR}/scripts/create_directories ${CPSEIS_ARCH}
cd platforms/${CPSEIS_ARCH}

make depend all cfe install

if ! (which cfe &> /dev/null ) ; then
	echo "PATH is not set up to find your installation of CPSeis front end, cfe."
	ERROR=$((ERROR+1))
else
	echo "cfe installed at: $(dirname $(which cfe))"
	if ! (echo $(dirname $(which cfe)) | grep cpseis) ; then
		echo "cfe is not coming from your cpseis directory."
		ERROR=$((ERROR+1))
	fi
fi

if ! (which icps &> /dev/null ) ; then
	echo "PATH is not set up to find your installation of CPSeis icps."
	ERROR=$((ERROR+1))
else
	echo "icps installed at: $(dirname $(which icps))"
	if ! (echo $(dirname $(which icps)) | grep cpseis) ; then
		echo "icps is not coming from your cpseis directory."
		ERROR=$((ERROR+1))
	fi
fi

if [ "$((ERROR > 0 ))" = "1" ] ; then
	echo "There are $ERROR error(s) in your setup."
	EXIT=1
else
	echo "There are $ERROR errors in your setup. So far, so good."
	EXIT=0
fi
exitnow
