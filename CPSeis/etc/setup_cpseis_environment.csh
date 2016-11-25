#!/bin/csh
#<CPS_v1 type="PROGRAM" pretag="#"/>
#<license>
#-------------------------------------------------------------------------------
# Copyright (c) 2011 Weinman Geoscience
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
#<brief_doc>
#-------------------------------------------------------------------------------
#                         C P S   D O C U M E N T A T I O N
#
# Name       : setup_cpseis_environment.sh
# Category   : stand-alone
# Written    : 2009-08-03   by: Bill Menger
# Revised    : 
# Maturity   : scripts
# Purpose    : Setup cpseis environment from bash or sh
# Portability: No known limitations.
#
#-------------------------------------------------------------------------------
#</brief_doc>
#<descript_doc>
#-------------------------------------------------------------------------------
#                          GENERAL DESCRIPTION
#
# This script should be called from individual user .bashrc files to
# set up the appropriate environment variables and aliases which will
# provide access to compilers and allow easy development work for cpseis
# programmers, developers, and users.
#
# To call this script, simply put the following into your .bashrc file:
#
#                     . setup_cpseis_environment.sh
#
# If this script is not reachable through the PATH variable (before it is
# added to by this script), you must specify the complete path, which can be
# provided to you by the cpseis system administrator.  Currently this will work:
#
#       . ${CPSEIS_INSTALL_DIR}/etc/setup_cpseis_environment.sh
#
# If you need to set some environment variables, paths, or aliases
# differently, you can simply redefine them in your .bashrc file after
# sourcing setup_cpseis_environment.sh.
#
#-------------------------------------------------------------------------------
#</descript_doc>
#<history_doc>
#-------------------------------------------------------------------------------
#                           REVISION HISTORY
#
#     Date        Author     Description
#     ----        ------     -----------
#  2. 2009-08-03  Menger     Added more mods to separate out thirdparty paths, etc.
#  1. 2009-06-03  Menger     Modified from setup_sps_environment (csh script)
#----------------------------------------------------------------------------------
# 18. 2007-04-10  Goodger    In testing whether SPS_ENVIRONMENT variable exists, 
#                            remove the == 1 portion of the if statment.  This
#                            syntax does not work in rhel 4.  Kruger Corn.
# 17. 2007-02-27  Glover     Do not set LD_ASSUME_KERNEL for nodes in cps_no_assume.dat list
# 16. 2007-02-02  Goodger    Add an spws path without the architecture directory.
# 15. 2006-08-22  Goodger    Added another path to JAVA_DIR.
# 14. 2006-08-14  Goodger    Added env var JAVA_DIR and included LD_LIBRARY_PATH.
# 13. 2005-10-28  Goodger    Change USAGE_HOST to cop_usage from cps_usage.
# 12. 2005-10-25  Goodger    Set LD_ASSUME_KERNEL to 2.4.1.  This gets rid
#                            if incorrect build binary message on worker
#                            nodes.
# 11  2005-08-29  Goodger    Add env variable USAGE_HOST.
# 10. 2004-10-21  Wardrop    Change XFILESEARCHPATH to include default system
#                            on Linux systems.
#  9. 2004-06-07  Goodger    Remove /apps/ow from LD_LIBRARY_PATH. This
#                            should be needed only for landmark jobs and it
#                            is requiring that the file system be mounted
#                            for all jobs.
#  8. 2004-02-16  Stoeckley  Use /apps/ow for all architectures (remove
#                             /appl/ow for solaris).
#  7. 2003-11-06  Goodger    Remove manpath command and replace with a
#                            specified path.
#  6. 2003-10-22  Goodger    Change SPWSINSTALLDIR from /appl/SPWS to
#                             /usr/app/vendors/spws
#  5. 2003-10-10  Stoeckley  Change to conditionally return just before adding
#                             to path variables instead of right away; add
#                             access to the Portland Group compiler and to
#                             Landmark libraries; fix MANPATH for linux; set
#                             OWHOME, add to LD_LIBRARY_PATH.
#  4. 2003-06-16  Goodger    Remove echos to screen.
#  3. 2003-06-16  Stoeckley  Add SGI64 and ALTIX architectures; provide access
#                             to intel compilers; move solaris compiler setups
#                             to new scripts; add gmake alias for SGI; add
#                             SPWSMAKEDIR; add HOST if missing; add additions
#                             to MANPATH and XFILESEARCHPATH; add printout.
#  2. 2003-04-04  Stoeckley  Add condition to exit immediately if previously
#                             called.
#  1. 2003-03-18  Stoeckley  Initial version.
#
#-------------------------------------------------------------------------------
#</history_doc>


##------------------------- set ARCHITECTURE -------------------------------##
##------------------------- set ARCHITECTURE -------------------------------##
##------------------------- set ARCHITECTURE -------------------------------##


##----------------- return if this script was already called ---------------##
if ( `which icps 2>/dev/null` ) then
	echo "inside "which icps is true"
	unsetenv CPSEIS_ENVIRONMENT
endif
if ( $?CPSEIS_ENVIRONMENT ) then
  	#echo "CPSEIS ENVIRONMENT already set up."
	exit 0
else
  	#echo "NEW CPSEIS ENVIRONMENT established."
	setenv CPSEIS_ENVIRONMENT "true"
endif

##---------------- Now perform stuff that happens on each login (as in the .bashrc)
# set up cpseis environment
# Add the following lines to use cpseis

if ( -e /usr/share/X11/XKeysymDB ) then
	setenv XKEYSYMDB /usr/share/X11/XKeysymDB
endif
if ( -e /usr/X11R6/lib/X11/XKeysymDB ) then
	setenv XKEYSYMDB /usr/X11R6/lib/X11/XKeysymDB
endif

if (! $?HOSTNAME ) then
	setenv HOSTNAME `uname -n`
  	setenv HOST=$HOSTNAME
endif
#echo "host=$HOST hostname=$HOSTNAME"
if (! $?HOME ) then
	set temp `pwd`
	cd
	setenv HOME `pwd`
	cd $temp
endif
setenv HOMELOC `dirname ${HOME}`
# see pfio.c for where HOMEMOUNT is used.
setenv HOMEMOUNT `\df ${HOMELOC} | awk 'END{print $NF}'`
#echo "homemount=$HOMEMOUNT homeloc=$HOMELOC"
if (! $?USERNAME ) then
        setenv USERNAME `whoami`
endif
#echo "username=$USERNAME"
if (! $?CPSEIS_COMPILER ) then
	setenv CPSEIS_COMPILER ""
	setenv CPSEIS_COMPILER_VER ""
	if ( \which ifort 2>/dev/null ) then
		set IFORT `basename ` \which ifort``
	else
		set IFORT "none"
	endif
	if ( \which gfortran 2>/dev/null )  then
		set GFORT `basename ` \which gfortran``
	else
		set GFORT "none"
	endif
	if ( $IFORT == ifort ) then
		set env CPSEIS_COMPILER intel
		set env CPSEIS_COMPILER_VER `ifort -v 2>&1 | awk '{print $1}' `
		if ( ${CPSEIS_COMPILER_VER} == 9.0 ) then
			setenv CPSEIS_COMPILER_VER 91
		endif
		if ( ${CPSEIS_COMPILER_VER} == 10.0 ) then
			setenv CPSEIS_COMPILER_VER 10
		endif
		if ( ${CPSEIS_COMPILER_VER} == 10.1 ) then
			setenv CPSEIS_COMPILER_VER 10
		endif
		if ( ${CPSEIS_COMPILER_VER} == 11.0 ) then
			setenv CPSEIS_COMPILER_VER 11
		endif
	else
		if ( $GFORT == gfortran ) then
			setenv CPSEIS_COMPILER gfortran
			setenv CPSEIS_COMPILER_VER ""
		endif
	endif
	setenv CPSEIS_COMPILER CPSEIS_COMPILER_VER
endif
if ( ! $?CPSEIS_COMPILER_VER ) setenv CPSEIS_COMPILER_VER ""
if ( $?CPSEIS_COMPILER_BASE ) then
	setenv CPSEIS_COMPILER_BASE `basename $CPSEIS_COMPILER_BASE $CPSEIS_COMPILER_VER `
else
	setenv CPSEIS_COMPILER_BASE $CPSEIS_COMPILER
endif

if (! $?MACHINE_ARCH ) then
	setenv MACHINE_ARCH `uname -m`
endif
if (! $?MPI ) then
	set MPI none
	set MPIM `which mpiexec | grep mpich2`
	set MPIL `which mpiexec | grep lam`
	if ( $MPIL != "" ) then
		set MPI lam
	else
		if ( $MPIM != "" ) then
			set MPI mpich2
		endif
	endif
	setenv MPI
endif


if (! $?CPSEIS_ARCH ) then
	setenv CPSEIS_ARCH ${MACHINE_ARCH}"_"${CPSEIS_COMPILER}${CPSEIS_COMPILER_VER}
endif
if (! $?THIRDPARTY_DIR ) then
	echo "Must define thirdparty installation directory.  ex: /home/your_user_name/thirdparty"
        echo "Example: Add this to your .bashrc: 'setenv THIRDPARTY_DIR=/your_installation_directory/thirdparty' "
	exit 1
endif
if (! $?CPSEIS_INSTALL_DIR ) then
	echo "Must define cpseis installation directory.  ex: /home/your_user_name/cpseis"
        echo "Example: Add this to your .bashrc: 'setenv CPSEIS_INSTALL_DIR=/your_installation_directory/cpseis' "
	exit 1
endif
##------------------- set general environment variables --------------------##

if ( ! -d ${CPSEIS_INSTALL_DIR}/etc/${CPSEIS_ARCH} ) then
	mkdir -p ${CPSEIS_INSTALL_DIR}/etc/${CPSEIS_ARCH}
endif
# create custom cps_config.dat for CPSEIS_INSTALL_DIR
if ( ! -f ${CPSEIS_INSTALL_DIR}/etc/${CPSEIS_ARCH}/cps_config.dat ) then
	cat ${CPSEIS_INSTALL_DIR}/etc/cpseis_config.dat |\
	sed "s:INSTALLDIR:${CPSEIS_INSTALL_DIR}:g;s:CPSEIS_ARCH:${CPSEIS_ARCH}:g" >${CPSEIS_INSTALL_DIR}/etc/${CPSEIS_ARCH}/cps_config.dat
endif

# set up cpsdata_nodes.dat and cpsdata_temp.dat if not set up yet.  Use the Template files to create.
if ( ! -f ${CPSEIS_INSTALL_DIR}/etc/cpsdata_nodes.dat ) then
	cat ${CPSEIS_INSTALL_DIR}/etc/cpsdata_nodes.dat.TEMPLATE |
		sed "s/^localhost/${HOSTNAME}/g;s:HOME:${HOMELOC}/${USERNAME}:g" \
		>${CPSEIS_INSTALL_DIR}/etc/cpsdata_nodes.dat
endif

if ( ! -f ${CPSEIS_INSTALL_DIR}/etc/cpstemp_nodes.dat ) then
	cat ${CPSEIS_INSTALL_DIR}/etc/cpstemp_nodes.dat.TEMPLATE |
		sed "s/^localhost/${HOSTNAME}/g;s/HOSTNAME/${HOSTNAME}/g" \
		>${CPSEIS_INSTALL_DIR}/etc/cpstemp_nodes.dat
endif

# template files just created require the following directories to be in place.  Create them if not present.
if ( ! -d $HOMELOC/$USERNAME/data ) then
	mkdir $HOMELOC/$USERNAME/data
endif
if ( ! -d /tmp/$HOSTNAME ) then
	mkdir /tmp/$HOSTNAME
	chmod 777 /tmp/$HOSTNAME
endif

if ( ! -d /tmp/$HOSTNAME/scratch ) then
	mkdir /tmp/$HOSTNAME/scratch
	chmod 777 /tmp/$HOSTNAME/scratch
endif

# --- for debugging ------------------------------
#echo "CPSEIS_INSTALL_DIR = ${CPSEIS_INSTALL_DIR}"
#cat ${CPSEIS_INSTALL_DIR}/etc/cps_config.dat
# --- ---- ---------------------------------------
setenv SPWSDIR $CPSEIS_INSTALL_DIR/spws_home
setenv SPSINSTALLDIR $CPSEIS_INSTALL_DIR
setenv SPSDIR $SPWSDIR
setenv SPWSINSTALLDIR $CPSEIS_INSTALL_DIR
#setenv XAPPLRESDIR $HOME/app-defaults
setenv XAPPLRESDIR $SPWSDIR/app-defaults
setenv SPWSMAKEDIR $SPWSDIR/making
setenv CFECUSTOMDIR $CPSEIS_INSTALL_DIR/etc
setenv CFECUSTOMSCRIPTS $SPSINSTALLDIR/scripts
setenv CFECUSTOMPROGRAMS $SPSINSTALLDIR/platforms/${CPSEIS_ARCH}/bin
setenv CPS_CONFIG_FILE $CPSEIS_INSTALL_DIR/etc/${CPSEIS_ARCH}/cps_config.dat
#you must have this next line for cnfg_crou.c.  Without it, you always look at the default values.
setenv cps_config_file $CPSEIS_INSTALL_DIR/etc/${CPSEIS_ARCH}/cps_config.dat 
setenv USAGE_HOST $HOSTNAME
setenv JAVA_HOME $JAVA_DIRECTORY
setenv JAVA_DIR_ROOT $JAVA_HOME/jre/lib
if ( $MACHINE_ARCH == x86_64 ) then
        setenv JAVA_DIR $JAVA_DIR_ROOT/amd64:$JAVA_DIR_ROOT/amd64/server
endif
if ( $MACHINE_ARCH == i686 ) then
        setenv JAVA_DIR $JAVA_DIR_ROOT/i386:$JAVA_DIR_ROOT/i386/client
endif
#echo "-------------------------------- READING NEW CONFIG FILE ---------------------"
#echo "cps_config_file="$cps_config_file
#echo "CPS_CONFIG_FILE="$CPS_CONFIG_FILE
##------------------ add to path environment variable ---------------------##
setenv PATH $SPSINSTALLDIR/platforms/${CPSEIS_ARCH}/bin:${CPSEIS_WRAPPERS}/scripts:${PATH}:.:
#echo "PATH $PATH"

##------------- add to xfilesearchpath environment variable ----------------##
if (! $?XFILESEARCHPATH ) then
  if ( $MACHINE_ARCH == x86_64 ) then
    setenv XFILESEARCHPATH /usr/lib/X11/%T/%N%S:$SPWSINSTALLDIR/spws/app-defaults/$ARCHITECTURE/%N
  else
    setenv XFILESEARCHPATH $SPWSINSTALLDIR/spws/app-defaults/$ARCHITECTURE/%N
  endif
else
  setenv XFILESEARCHPATH $SPWSINSTALLDIR/spws/app-defaults/$ARCHITECTURE/%N:$XFILESEARCHPATH
endif
##------------- add to ld_library_path environment variable ----------------##
setenv LD_LIBRARY_PATH ${JAVA_DIR}:${LD_LIBRARY_PATH}
#echo LD_LIBRARY_PATH $LD_LIBRARY_PATH
#-------------------------------- end -------------------------------------##
#which icps
#which cfe
