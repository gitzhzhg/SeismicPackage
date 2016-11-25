export INTEL_ROOT=/opt/intel
if [ "$(uname -p)" = "i686" ] ; then
        version=10.0
        FTN_SCRIPT=${INTEL_ROOT}/fc/$version/bin/ifortvars.sh
        CPP_SCRIPT=${INTEL_ROOT}/cc/$version/bin/iccvars.sh 
        IDB_SCRIPT=${INTEL_ROOT}/idb/$version/bin/idbvars.sh 
        MKL_SCRIPT=${INTEL_ROOT}/mkl/$version/tools/environment/mklvarsem64t.sh
else
        version=10.0
        FTN_SCRIPT=${INTEL_ROOT}/fce/$version/bin/ifortvars.sh
        CPP_SCRIPT=${INTEL_ROOT}/cce/$version/bin/iccvars.sh 
        IDB_SCRIPT=${INTEL_ROOT}/idbe/$version/bin/idbvars.sh 
        MKL_SCRIPT=${INTEL_ROOT}/mkl/$version/tools/environment/mklvarsem64t.sh
fi
[ -f $FTN_SCRIPT ] && . $FTN_SCRIPT 
[ -f $CPP_SCRIPT ] && . $CPP_SCRIPT
[ -f $IDB_SCRIPT ] && . $IDB_SCRIPT
[ -f $MKL_SCRIPT ] && . $MKL_SCRIPT
