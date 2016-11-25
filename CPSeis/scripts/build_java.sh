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
# Get and unpack java jdk 
temp=$(pwd)
trap "cd ${temp}; echo 'Aborted java jdk installation.'; exit 1" 1 2 3 6 9 11 12 15
if [ "$(uname -m)" = "x86_64" ] ; then
	PRODUCT=jdk-6u14-linux-x64.bin
elif [ "$(uname -m)" = "i686" ] ; then
	PRODUCT=jdk-6u14-linux-i586.bin
else
	echo "Not a supported architecture"
	exit 1
fi
JDK_DIR=${THIRDPARTY_DIR}/jdk
JNAME=jdk1.6.0_14
RAW_DIR=${THIRDPARTY_DIR}/jdk/$JNAME
BASE_DIR=${THIRDPARTY_DIR}/jdk/$(basename ${PRODUCT} .bin)_$(uname -m)
LINK1_DIR=${THIRDPARTY_DIR}/jdk/platforms/${CPSEIS_ARCH}
LINKS=$(echo $CPSEIS_ARCH | awk 'BEGIN{FS="_"}{
        for (i=1;i<NF-2;i++)printf("%s_",$i);
        printf("%s",$(NF-1));}')
LINK2_DIR=${THIRDPARTY_DIR}/jdk/platforms/${LINKS}

cd ${THIRDPARTY_DIR}
echo "Thirdparty directory = "$THIRDPARTY_DIR
ls -l 
if [ ! -f ${PRODUCT} ] ; then
        # get binary 
        wget http://dev.iki-project.org/pub/java/${PRODUCT}
fi
ls -l 
echo $BASE_DIR
echo $JDK_DIR
if [ ! -d ${BASE_DIR} ] ; then
	mkdir -p ${JDK_DIR}
	cd ${JDK_DIR}
	if [ ! -f .built ] ; then
        	${THIRDPARTY_DIR}/${PRODUCT}
		touch .built
	fi
	mv ${RAW_DIR} ${BASE_DIR}
	mkdir -p platforms
	ln -s ${BASE_DIR} ${LINK1_DIR}
	ln -s ${BASE_DIR} ${LINK2_DIR}
fi
cd $temp
echo "Build java jdk complete. Add ${LINK2_DIR}/bin to your path."
exit 0
