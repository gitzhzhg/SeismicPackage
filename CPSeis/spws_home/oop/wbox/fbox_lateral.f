
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

CCC                     fbox_lateral.f


ccc   this file contains routines which can be used to facilitate
ccc   lateral scrolling of a set of linked arrays when pointers
ccc   are used to update the arrays.

ccc   the routines in this file are by no means required to have
ccc   this functionality.  and in particular, if update functions
ccc   are used instead of pointers to update the arrays, it is
ccc   easy to scroll laterally without using any special routines.


c-----------------fbox lateral scroll start.
c-----------------fbox lateral scroll start.
c-----------------fbox lateral scroll start.

ccc        call this to register the variables.

      subroutine fbox_lateral_scroll_start (box,n,nmax,irow,nrow,
     $   nchar,ndec,kmax,ncol,prompt,jsw,var,isw,kstart)

      implicit none
      integer box,n,nmax,irow,nrow
      integer nchar,ndec,kmax,ncol,jsw(kmax),isw(kmax),kstart
      integer k,icol
      integer m44
      character*(*) prompt(kmax)
      character*1 nothing
      real var(nmax,kmax)
      save m44           ! needed on Cray.
      save nothing
      data nothing/' '/
      data m44/-44/

      call fbox_rega (n,nmax,irow,nrow)
      k=2+alog10(float(nmax))
c     call nbox_xrega (0,' ',m44,var,m44,1,k,0)
      call nbox_xrega (0,nothing,m44,var,m44,1,k,0)
      do k=1,kmax
           if (k.le.ncol) then
                isw(k)=1
                jsw(k)=0
                icol=0
           else
                isw(k)=-99
                jsw(k)=-99
                icol=1
           end if
           call nbox_frega (k,prompt,jsw(k),var,isw(k),icol,nchar,ndec)
      end do
      kstart=1
      call lateral_scroll_reregister
     $              (box,ncol,kstart,kmax,prompt,var,nmax)
      return
      end



c------------------fbox lateral scroll.
c------------------fbox lateral scroll.
c------------------fbox lateral scroll.

ccc          call this from the trap.

      subroutine fbox_lateral_scroll (box,n,nmax,irow,nrow,
     $   nchar,ndec,kmax,ncol,prompt,jsw,var,isw,kstart,
     $   ident,index,endkey)

      implicit none
      integer box,n,nmax,irow,nrow
      integer nchar,ndec,kmax,ncol,jsw(kmax),isw(kmax),kstart
      integer ident,index
      character*(*) prompt(kmax),endkey
      real var(nmax,kmax)

      if (endkey.eq.'RIGHT'.and.ident.eq.ncol) then
           if (kstart+ncol-1.lt.kmax) then
                kstart=kstart+1
                endkey=' '
                go to 50
           else if (index.lt.min(n+1,nmax)) then
                kstart=1
                go to 50
           end if
      else if (endkey.eq.'LEFT'.and.ident.eq.1) then
           if (kstart.gt.1) then
                kstart=kstart-1
                endkey=' '
                go to 50
           else if (index.gt.1) then
                kstart=kmax-ncol+1
                go to 50
           end if
      else if (endkey.eq.'@RIGHT') then
           kstart=kmax-ncol+1
           ident=ncol
           endkey=' '
           go to 50
      else if (endkey.eq.'@LEFT') then
           kstart=1
           ident=1
           endkey=' '
           go to 50
      end if
      return

50    call lateral_scroll_reregister
     $              (box,ncol,kstart,kmax,prompt,var,nmax)
      return
      end



c------------------lateral scroll reregister.
c------------------lateral scroll reregister.
c------------------lateral scroll reregister.

ccc      private.
ccc      call this from fbox_lateral_scroll_start.
ccc      call this from fbox_lateral_scroll.

      subroutine lateral_scroll_reregister
     $              (box,ncol,kstart,kmax,prompt,var,nmax)
      implicit none
      integer box,ncol,kstart,kmax,nmax
      character*(*) prompt(kmax)
      real var(nmax,kmax)
      integer k,k2

      do k=1,ncol
           k2=k+kstart-1
           call fbox_cnewreg (box,-k,prompt(k2))
           call fbox_fnewreg (box, k,var(1,k2))
      end do
      k2=ncol
      do k=1,kmax
      if (k.lt.kstart.or.k.gt.kstart+ncol-1) then
           k2=k2+1
           call fbox_cnewreg (box,-k2,prompt(k))
           call fbox_fnewreg (box, k2,var(1,k))
      end if
      end do
      return
      end



c------------------------end.
c------------------------end.
c------------------------end.

