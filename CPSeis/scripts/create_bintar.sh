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
# create an archive of all binary files in the ~/workspace/cpseis/platforms/*/bin tree excluding files as shown.

quit () {
        cd ..
        rm -rf .tmp$$
        exit 0
}
set -x
mkdir .tmp$$
cd .tmp$$

cat <<EOF >Exclude
geopress
Exclude
EOF

trap "cd ..;rm -rf .tmp$$;exit 1" 1 2 3 6 11 12 15

for dir in ${CPSEIS_INSTALL_DIR}/platforms/${CPSEIS_ARCH} ; do
        rsync -urlv --exclude-from Exclude $dir .
        tar zc --exclude-from Exclude --file ../${CPSEIS_ARCH}.tgz .
done

cd ..
rm -rf .tmp$$
ls -lrta ${CPSEIS_ARCH}.tgz
echo "---DONE---"
