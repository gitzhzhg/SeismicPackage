#!/bin/bash
# set up environment for geocraft
# 
if [ -z $THIRDPARTY_DIR ] ; then
        export THIRDPARTY_DIR=/apps/fusiongeo/thirdparty
fi
export GC_INSTALL_DIR=/apps/fusiongeo/GeoCraft
export GC_ARCH=$(uname -m)
export GC_JAVA=$THIRDPARTY_DIR/jdk/platforms/$GC_ARCH/bin
GC_DIR=$GC_INSTALL_DIR/platforms/$GC_ARCH/Geocraft
export PATH=$GC_JAVA:$GC_DIR:$PATH
