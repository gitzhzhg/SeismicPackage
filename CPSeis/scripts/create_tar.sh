#!/bin/bash
#<license>
#-------------------------------------------------------------------------------
# Copyright (c) 2009 Fusion Petroleum Technologies Inc
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
# create an archive of all files in the ~/workspace/cpseis tree excluding files as shown.
cat <<EOF >~/.Exclude
HOW_TO_MAKE_GEOPRESS
Geopress_help
Geopress
geopress
gp
gp_*.cc
gp_*.hh
gp_*.h
gpgui*
gpbox*
gppop*
Geopress.h
geopress.*
programs/geopress
oop/gpgui
oop/gpplots
oop/gp
obsolete_or_reference
geopress_application.*
include/geopress_application.hh
old_programs
kruger
.Exclude
runcfe*
platforms
cpsdata_nodes.dat
cpstemp_nodes.dat
cps_lockfile.dat*
cpseis.tgz
*.o
*.so
*.so.*
*.svn
*.svn-base
*.class
*.tar
*.gz
*.mod
*.tgz
*.submitted
*.started
*.aborted
*.icps*
*.rpt*
*.log
.nfs*
*.cps
*.local
*.completed
%HIST*
%trin_file%
time_stamp
*.job
.*license_splash
*.a
*.mod
*.class
EOF

cd $(dirname $CPSEIS_INSTALL_DIR )
echo "Starting from $(pwd) directory and doing tar on installed cpseis directory..."
echo "Hit return to continue"
read prompt
trap "rm -f ~/.Exclude;exit 1" 1 2 3 6 11 12 15
tar zcv --exclude-from ~/.Exclude --file ~/cpseis.tgz cpseis/
#tar zcv --file ~/intel.tgz cpseis/intel*
rm -f ~/.Exclude
echo "---DONE---"
