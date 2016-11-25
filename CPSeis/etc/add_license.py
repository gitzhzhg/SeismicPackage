#!/usr/bin/python
# -- obviously, you need to put your own python path above.  Needs to be 2.4 or better.
# -- This only adds the license to source files, which (in this repository) has already
# -- been done.  It requires that each source file have the <copyright> ... </copyright>
# -- tags in them.

import glob,re,os,os.path,shutil,sys

license = """!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>
"""

### Replaces text between the <copyright>...</copyright>
### tags with ConocoPhillips' license between tags called
### <license>...</license>
def copyprependfile(fsrc, ddst):
    if (fsrc.endswith('.f90') or
        fsrc.endswith('.c'  ) or
        fsrc.endswith('.cc'  ) or
        fsrc.endswith('.hh'  ) or
        fsrc.endswith('.F90'  ) or
        fsrc.endswith('.h'  )):
        lines = open(fsrc).readlines()
        fdst = open(ddst+'/'+fsrc, 'w')
        i = 0;
        while not 'copyright' in lines[i]:
            fdst.write(lines[i])
            i+=1
        fdst.writelines(license)
        while not '/copyright' in lines[i]:
            i+=1
        i+=1
        while i < len(lines):
            fdst.write(lines[i])
            i+=1

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
prodname = 'cpseis'
#if os.path.isdir(prodname+'/src'): shutil.rmtree(prodname+'/src')
#os.makedirs(prodname+'/src')
#os.makedirs(prodname+'/lib')
#os.makedirs(prodname+'/bin')
for m in tocopy:
	print 'Evaluating file: '+m

for m in tocopy:
	print 'Copying file: ' +m
	#copyprependfile(m,prodname+'/src')
