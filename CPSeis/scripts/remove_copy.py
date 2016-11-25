#!/usr/bin/python
# -- obviously, you need to put your own python path above.  Needs to be 2.4 or better.
# -- This only adds the license to source files, which (in this repository) has already
# -- been done.  It requires that each source file have the <copyright> ... </copyright>
# -- tags in them.

import glob,re,os,os.path,shutil,sys

def rmcopy(src,dst):
    if (src.endswith('.f90') or
        src.endswith('.c'  ) or
        src.endswith('.cc'  ) or
        src.endswith('.hh'  ) or
        src.endswith('.F90'  ) or
        src.endswith('.sh'  ) or
        src.endswith('.csh'  ) or
        src.endswith('.h'  )):
        lines = open(src).readlines()
        out   = open(dst,'w')
        i     = 0
        while not '<copyright>' in lines[i]:
                out.write(lines[i])
                i += 1
        while not '</copyright>' in lines[i]:
                i += 1
        i += 1
        while i < len(lines):
                out.write(lines[i])
                i += 1

args=[]
args=sys.argv
file=args[1]
print 'Working on file '+file
rmcopy(file,file+'.new')

