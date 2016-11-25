#!/usr/bin/python
# -- obviously, you need to put your own python path above.  Needs to be 2.4 or better.
# -- This only adds the license to source files, which (in this repository) has already
# -- been done.  It requires that each source file have the <copyright> ... </copyright>
# -- tags in them.

import glob,re,os,os.path,shutil,sys

flicense = """C<license>
C-------------------------------------------------------------------------------
C Copyright (c) 2007 ConocoPhillips Company
C
C Permission is hereby granted, free of charge, to any person obtaining a copy
C of this software and associated documentation files (the "Software"), to deal
C in the Software without restriction, including without limitation the rights
C to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
C copies of the Software, and to permit persons to whom the Software is
C furnished to do so, subject to the following conditions:
C
C The above copyright notice and this permission notice shall be included in all
C copies or substantial portions of the Software.
C
C THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
C IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
C FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
C AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
C LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
C OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
C SOFTWARE.
C-------------------------------------------------------------------------------
C</license>
"""
clicense="""/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>
*/
"""
slicense="""#<license>
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
"""

### Replaces text between the <copyright>...</copyright>
### tags with ConocoPhillips' license between tags called
### <license>...</license>
def prependf(src, dst):
    if (src.endswith('.f90') or
        src.endswith('.F90'  ) or
        src.endswith('.f'  )):
	print 'Copying fortran file: ' +src
        lines = open(src).readlines()
        out = open(dst,'w')
        i = 0
        out.writelines(flicense)
        out.write(lines)

def prependc(src,dst):
        if (src.endswith('.c') or
            src.endswith('.h') or
            src.endswith('.cc') or
            src.endswith('.hh'):
	    print 'Copying c/h file: ' +src
            lines = open(src).readlines()
            out = open(dst,'w')
            out.writelines(clicense)
            out.writelines(lines)

def prepends(src,dst):
        if (src.endswith('.sh') or
            src.endswith('.csh'):
	    print 'Copying script file: ' +src
            lines = open(src).readlines()
            out = open(dst,'w')
            out.writelines(lines[0])
            out.writelines(slicense)
            i=1
            while i < len(lines):
                out.writelines(lines[i])
                i += 1

### Global variables
### List of files to be copied into the distribution.
tocopy   = []

### This is the name of the directory into which files are copied

for filename in glob.glob('*'):
    tocopy += [filename.strip()]
    
tocopy.sort()

########################################################################## 
###               Copy source files with license                      ####
########################################################################## 
for m in tocopy:
	print 'Evaluating file: '+m

for m in tocopy:
	prependf(m,m+'.new')
	prependc(m,m+'.new')
	prepends(m,m+'.new')
