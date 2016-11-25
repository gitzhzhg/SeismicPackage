#!/bin/bash
# Set these environment variables up in your .bashrc file
export CPSEIS_HOME=$CPSEIS_INSTALL_DIR
export CPSEIS_WRAPPERS=$CPSEIS_HOME/wrappers
export MACHINE_ARCH=$(uname -m)
export ARCHITECTURE=$MACHINE_ARCH
export HOSTNAME=$(uname -n)
export HOST=$HOSTNAME
export MPI=mpich2
#export MPI=lam
export CPSEIS_COMPILER=gfortran
#export CPSEIS_COMPILER=intel91
export CPSEIS_ARCH=${MACHINE_ARCH}_${CPSEIS_COMPILER}_${MPI}
# set up area for third-party software package installation
#export THIRDPARTY_DIR=${HOME}/thirdparty
export THIRDPARTY_DIR=/apps/fusiongeo/thirdparty

if [ -z $LD_LIBRARY_PATH ] ; then
        export LD_LIBRARY_PATH=${CPSEIS_INSTALL_DIR}/platforms/${CPSEIS_ARCH}/lib
else
        export LD_LIBRARY_PATH=${CPSEIS_INSTALL_DIR}/platforms/${CPSEIS_ARCH}/lib:$LD_LIBRARY_PATH
fi
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/${CPSEIS_INSTALL_DIR}/wrappers/platforms/${CPSEIS_ARCH}/lib

# set up fftw environment
export FFTW_DIR=${THIRDPARTY_DIR}/fftw-2.1.5/platforms/${MACHINE_ARCH}_${CPSEIS_COMPILER}
	#---------- no binaries in fftw package --- export PATH=$PATH:${FFTW_DIR}/bin
        export LD_LIBRARY_PATH=${FFTW_DIR}/lib:$LD_LIBRARY_PATH

#set up compiler environment
if [ "${CPSEIS_COMPILER}" = "intel91" ] ; then
        export LD_LIBRARY_PATH=${CPSEIS_INSTALL_DIR}/intel_${MACHINE_ARCH}:${LD_LIBRARY_PATH}
        . $HOME/bin/intel.sh
fi
# set up mpi environment
if [ -z ${MPI} ] ; then
	echo "NO MPI"
fi
if [ "${MPI}" = "mpich2" ] ; then
	# set up mpich2 environment
        export MPICH_DIR=${THIRDPARTY_DIR}/mpich2-1.1/platforms/${MACHINE_ARCH}_${CPSEIS_COMPILER}
        export PATH=${MPICH_DIR}/bin:$PATH
        export LD_LIBRARY_PATH=${MPICH_DIR}/lib:$LD_LIBRARY_PATH
        export CPSEIS_C_COMPILER=mpicc
        export CPSEIS_F90_COMPILER=mpif90
elif [ "${MPI}" = "lam" ] ; then
	# set up lam-mpi environment
        export LAM_DIR=${THIRDPARTY_DIR}/lam-7.0.6/platforms/${MACHINE_ARCH}_${CPSEIS_COMPILER}
        export PATH=${LAM_DIR}/bin:$PATH
        export LD_LIBRARY_PATH=${LAM_DIR}/lib:$LD_LIBRARY_PATH
        if [ "${CPSEIS_COMPILER}" = "intel91" ] ; then
                export CPSEIS_C_COMPILER=icc
                export CPSEIS_F90_COMPILER=ifort
        else
                export CPSEIS_C_COMPILER=gcc
                export CPSEIS_F90_COMPILER=gfortran
        fi
fi

# set up java environment
export JAVA_DIRECTORY=${THIRDPARTY_DIR}/jdk/platforms/${CPSEIS_ARCH}
export PATH=${JAVA_DIRECTORY}/bin:$PATH
export JAVA_HOME=${JAVA_DIRECTORY}

#export CPSEIS_INSTALL_DIR=${HOME}/cpseis
export CPSEIS_PLATFORM=${CPSEIS_INSTALL_DIR}/platforms/${CPSEIS_ARCH}

if [ -e ${CPSEIS_INSTALL_DIR}/etc/setup_cpseis_environment.sh ] ; then
	. ${CPSEIS_INSTALL_DIR}/etc/setup_cpseis_environment.sh
fi
