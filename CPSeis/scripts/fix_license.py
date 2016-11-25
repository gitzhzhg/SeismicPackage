#!/usr/bin/python
# -- obviously, you need to put your own python path above.  Needs to be 2.4 or better.
# -- This only adds the license to source files, which (in this repository) has already
# -- been done.  It requires that each source file have the <copyright> ... </copyright>
# -- tags in them.

import glob,re,os,os.path,shutil,sys

def fixcopy(src,dst):
    if (src.endswith('.c'  ) or
        src.endswith('.cc'  ) or
        src.endswith('.hh'  ) or
        src.endswith('.h'  )):
        lines = open(src).readlines()
        out   = open(dst,'w')
        i     = 0
        j     = 0
        while not '<license>' in lines[i]:
                j += 1
                i += 1

        if (j > 1):
                print 'Fixing file '+file

        while not '</license>' in lines[i]:
                out.write(lines[i])
                i += 1
        out.write(lines[i]);
        i += 1
        k = 0
        while k < j:
                out.write(lines[k])
                k += 1
        while i < len(lines):
                out.write(lines[i])
                i += 1

args=[]
args=sys.argv
file=args[1]
print 'Fixing file '+file
fixcopy(file,file+'.new')

