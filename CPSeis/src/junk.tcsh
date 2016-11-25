setenv CPSEIS_INSTALL_DIR /home/mengewm/workspace/cpseis
setenv CPSEIS_ARCH linux64a
unsetenv SPS_ENVIRONMENT
if($?SPS_ENVIRONMENT == 0) then
        echo "No SPS_ENVIRONMENT set up"
        source ${CPSEIS_INSTALL_DIR}/etc/setup_sps_environment
else
        echo "SPS_ENVIRONMENT already set up -- quitting here to let you check to make sure it is correct."
        exit 1
endif
# ----------------- Now to business.  Point to our client software and shared libraries.
setenv CLIENT_HOME ${CPSEIS_INSTALL_DIR}/opencfe
setenv CFESHARED ${CPSEIS_INSTALL_DIR}/lib/${CPSEIS_ARCH}/libcps.so

# ----------------- The GUI uses the xerces XML parser.  Apache licensed this for open source use.
#                   It also uses the jsdk http libraries in the build process.
setenv CLASSPATH ${CLIENT_HOME}/jars/opencfe.jar:${CLIENT_HOME}/jars/xercesImpl.jar:${CLIENT_HOME}/jars/resolver.jar:${CLIENT_HOME}/jars/serializer.jar
setenv JAVASEIS_PATH ${CPSEIS_INSTALL_DIR}/lib/javaseis64
setenv CLASSPATH ${CLASSPATH}:${JAVASEIS_PATH}/cop_ngs.jar:${JAVASEIS_PATH}/edu_mines_jtk.jar:${JAVASEIS_PATH}/org_javaseis.jar:${JAVASEIS_PATH}/cop_seiswrap.jar:${JAVASEIS_PATH}/mpijava.jar


#----------------- I currently have this set to use a 64 bit java 1.6.011 for AMD
#                   Point this to your java installation         
setenv JAVA_HOME /apps/geophysics/jdk1.6.0_11_amd64
# ----------------- Did not check on these parameters.  Historically used.
setenv JAVA_OPTS "-ms64m -mx64m -ss256k -Djava.util.prefs.syncInterval=2000000"
setenv JAVA $JAVA_HOME/bin/java

# ----------------- This points to the fortran link libraries (included with permission).
setenv LINK_DIR /d/geoprog/u/mengewm/fftw/linux64a/lib:/d/geoprog/u/mengewm/mpich2/linux64a/lib:${JAVA_HOME}/jre/lib/amd64:${JAVA_HOME}/jre/lib/amd64/server:${CPSEIS_INSTALL_DIR}/lib/absoft64
setenv LD_LIBRARY_PATH ${CLIENT_HOME}/lib/${CPSEIS_ARCH}:${LINK_DIR}
echo "LD_LIBRARY_PATH = "$LD_LIBRARY_PATH

# ----------------- Set up the path for executables.
setenv PATH ${CPSEIS_INSTALL_DIR}/bin/${CPSEIS_ARCH}:/${CPSEIS_INSTALL_DIR}/bin:${PATH}
#echo "PATH            = "$PATH

