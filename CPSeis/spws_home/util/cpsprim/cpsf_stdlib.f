C\USER DOC
C<license>
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
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process Name: CPSF_...
C        Author: R.S. DAY
C       Written: 97/10/24
C  Last Revised: 98/12/01  Day
C
C  PURPOSE: Some useful general utilities for CPS
C     subroutine cpsf_getenv(name, value)
C     subroutine cpsf_system(cmd)
C     integer function cpsf_malloc(n)
C     subroutine cpsf_free(pointer)
C           - wrappers around some c library routines
C             such as getenv, malloc and system. name, 
C             cmd and value are character. n is an integer
C           - pointer is the integer returned by cpsf_malloc
C     subroutine cpsf_name_inc(name,newname,n)
C           - puts extension xx on name to give newname.
C           - xx is a function of the integer 1<=n<26*26
C     subroutine cpsf_name_incd(name,newname,n)
C           - puts a 1 to 3 digit extension on name to give newname.
C           - n must be in the range 1-999
C NOTES: 
C 1. This file uses functions from the file cps_stdlib.c
C 2. Meant to replace non portable CRAY system or function
C    calls such as ISHELL, GETENV, HPALLOC
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date     Author     Description
C     ----     ------     -----------
C 3. 98/12/01 Day         cpsf_name_incd added
C 2. 98/11/05 Rozell     CFT to FIX conversion
C 1.  97/12/16 Day        Placed into newlib
C-----------------------------------------------------------------------
C
C Externals : convert_cc2hh  sizeof_real cps_system cps_getenv
C             cps_malloc     cps_free
C***************************************************************
C\END DOC

C Calls a wrapper around the c getenv call
      integer function cpsf_getenv(name, value)
      implicit none
      character*(*) name,value
      integer       n,ivalue(25),iname(25)
      integer sizeof_real,cps_getenv
      cpsf_getenv=0
      value=' '
      n = len(name)/sizeof_real()
      call convert_cc2hh(name,0,iname,-25)
      cpsf_getenv = cps_getenv(iname,ivalue)
      call convert_hh2cc(ivalue,0,value,0)
      return
      end

C Calls a wrapper around the c system call
      subroutine cpsf_system(cmd)
      implicit none
      character*(*) cmd
      integer       n,icmd(40)
      integer       sizeof_real
      n = 40  !len(cmd)/sizeof_real()
      call convert_cc2hh(cmd,0,icmd,-n)
      call cps_system(icmd)
      return
      end

C An interface to the c malloc function
C n is the number of bytes one wants allocated.
C Make your request a multiple of 8 bytes
      integer function cpsf_malloc(n)
      integer n,cps_malloc
      cpsf_malloc =  cps_malloc(n)
      return
      end
      subroutine cpsf_free(n)
      integer n
      call cps_free(n)
      return
      end

C Provides a standard way for a CPS routine to increment
C a file name in a way that is not compiler specific
      subroutine cpsf_name_inc(name,newname,n)
      implicit none
      integer i,n
      character*(*) name,newname
      character ext*2
      do i=1,len(name)
       if(name(i:i).eq.' ') goto 10
      enddo
 10   continue
      ext(1:1)=char(97+(n-1)/26)
      ext(2:2)=char(97+mod(n-1,26))
      newname=name(1:i-1)//ext
      return
      end

C Provides a standard way for a CPS routine to increment
C a file name in a way that is not compiler specific
      subroutine cpsf_name_incd(name,newname,n)
      implicit none
      integer i,n
      character*(*) name,newname
      character ext*4
      do i=1,len(name)
       if(name(i:i).eq.' ') goto 10
      enddo
 10   continue
      ext=' '
      if(n.le.9) then
        write(ext(1:1),'(i1)') n
      elseif(n.le. 99) then
        write(ext(1:2),'(i2)') n
      else
        write(ext(1:3),'(i3)') min(n,999)
      endif
      newname=name(1:i-1)//ext
      return
      end
