#!/bin/bash
FFTW_ROOT=/apps/fusiongeo/thirdparty
echo "fftw-root=$FFTW_ROOT"
if [ -z $1 ] ; then
        version=2
else
        version=$1
fi
if [ "$version" = "2" ] ; then
        FFTW_DIR=$FFTW_ROOT/fftw-2.1.5/platforms/${CPSEIS_ARCH}
elif [ "$version" = "3" ] ; then
        FFTW_DIR=$FFTW_ROOT/fftw-3.2.2/platforms/${CPSEIS_ARCH}
else
        echo "Not a valid version for fftw (use 2 or 3)"
        return 1
fi
export FFTW_DIR
export LD_LIBRARY_PATH=$FFTW_DIR/lib:$LD_LIBRARY_PATH
echo "fftw-dir=$FFTW_DIR"
