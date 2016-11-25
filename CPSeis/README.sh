#!/bin/bash
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
# -- This provides instructions for installing CPSeis(tm) on a Linux system.
cat <<EOF
Instructions for installing CPSeis(tm) on your Linux computer.
1) Read the License agreement in license.txt, included with this distribution.
EOF
echo "hit enter to continue..."
read prompt
echo ""
echo "   -------------------------------------"
echo ""
more ./license.txt
echo ""
echo "   -------------------------------------"
echo ""
echo "   hit enter to continue..."
read prompt
less -h 10 <<EOF

2) If you agree to the terms and conditions on the license, you may continue.

3) Decide on an installation directory for cpseis and for the thirdparty
   packages that are with it.  Put cpseis.tgz into the directory above where you
   will install cpseis. (for example, if your installation will be in
   /apps/cpseis, put cpseis.tgz into the /apps directory.)

3-OPTIONAL) download cpseis from sourceforge using SVN checkout. (see sourceforge
  for help on this.)  You will not be downloading a tar file, but all of the 
  source code + some prebuilt programs, scripts, help, etc.  This is the best way to
  ensure a clean build on your target platform, but is more work.

4) To ensure no license issues arise, no thirdparty packages have been included,
   but some are required.
   DEFINE a thirdparty directory and put it into your .cshrc or .bashrc.  ALSO
   DEFINE your installation directory location:
   (example for .bashrc: 
       export THIRDPARTY_DIR=/apps/thirdparty
       export CPSEIS_INSTALL_DIR=/apps/cpseis
   )
   Packages you will need:
   Java jdk 1.6 (see www.sun.com)
   mpich2.1 (This is available via a script file: .../cpseis/scripts/build_mpich2)
   fftw2.15 (This is available via a script file: .../cpseis/scripts/build_fftw)
   I use Centos4.8 (64 bit) on VMWare player using an iso downloaded from CentOS.
   You will need to add some packages to CentOS 4.8:
   rpms to download (rpm.pbone.net):
     gcc4-4.1.2-44.EL4.x86_64.rpm
     libgomp-4.1.2-44.EL4_8.1.x86_64.rpm
     gcc4-gfortran-4.1.2-44.EL4_8.1.x86_64.rpm
   rpm -U  gcc4-4.1.2-44.EL4.x86_64.rpm
   rpm -U  libgomp-4.1.2-44.EL4_8.1.x86_64.rpm
   rpm -U  gcc4-gfortran-4.1.2-44.EL4_8.1.x86_64.rpm
   yum install --enablerepo=centosplus xorg-x11-devel
   yum install --enablerepo=centosplus openmotif-devel
   yum install --enablerepo=centosplus Xaw3d-devel

5) You have now created a cpseis directory somewhere 
     (
       either 
             via "tar -xvfz cpseis.tgz"
       or 
             via "svn co https://cpseis.svn.sourceforge.net/svnroot/cpseis"
     )

   If you have the directory for thirdparty created, copy all of the tarballs
   from the distribution into that directory.  This will save time since the
   scripts will attempt to download the tarballs if they don't exist.

6) run the install_cpseis script.

7) build the banner program separately (Tarball included with this
   distribution.)

8) If you don't have a queueing system for your cluster or workstations, you
   can try out torque. (not included with this distribution.)
9) Make sure the environment variables are in your .cshrc or .bashrc files
        .cshrc: setenv CPSEIS_INSTALL_DIR /apps/cpseis (from example above)
        .bashrc: export CPSEIS_INSTALL_DIR=/apps/cpseis
        Then add something in your "." file to source the script:
        # bash example
        EXE=$CPSEIS_INSTALL_DIR/etc/cpseis_bashrc.sh
        [ -e $EXE ] && . $EXE
        # ----------------     
10) Testing:  There are some test jobs in the sample_work_files directory you should try:
        $ cd $CPSEIS_INSTALL_DIR/sample_work_files
        $ icps testfunction.wrk
11) Test the gui by running "$ cfe"
    It should run, starting up a lock file manager prior to starting the GUI.
	click on "Select Current Workfile...."
	select #3 testfunction.wrk
	click on "OK"
    You should see a Process List in the center section with the following:
	PROJECT_DATA
	JOB_DATA
	RNSYN
	HEADSUM
	INTEGRATE
    Now try running the single-cpu runtime, ICPS by clicking on "ICPS (local)".
    You should see a separate window appear showing the current hostname, then some
    messages about trying rsh...
    If rsh is not enabled, the xongp script will not correctly work, so you will 
    need to enable rsh or modify the scripts. (see ...cpseis/scripts).  I recommend
    enabling rsh for this and later for MPI.
    The job runs in about 1 second.  It should end with the words:
	Finished running ...testfunction.wrk"
	========================================

	Press ENTER to continue...	

    A log file is created for the run, in the current directory, ending in .icps.

    If this does not work please email the cpseis-users user group for help.
    Bill Menger (bill.menger@gmail.com) 06/14/2011
EOF
echo "Hit enter to exit (oxymoronic, ain't it?)"
read prompt
exit
