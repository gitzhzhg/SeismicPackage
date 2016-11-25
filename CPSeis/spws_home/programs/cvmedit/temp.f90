
!<license>
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
      subroutine modgrid_dfile_c(ifile,stdo,iftype, idfile)
      use modgrid_module
      use string_module
      implicit none
      integer,intent(in)    :: ifile(*)
      integer,intent(out)   :: idfile(*)
      integer,intent(in)    :: stdo
      integer,intent(inout) :: iftype(*)

      type(modgrid_struct),pointer    :: obj
      character(len=120)    :: file
      character(len=120)    :: dfile
      character(len=8)      :: ftype
      character(len=8)      :: wtype
      integer     :: status
      integer     :: xhdr=17   !scan header for trace files
      integer     :: yhdr=18   !scan header for trace files
     
      call string_hh2cc(ifile,file)
      call string_hh2cc(idfile,dfile)
      status =  modgrid_rddesc(obj,file,stdo,dfile,wtype, ftype,xhdr,yhdr)
      call string_cc2hh(ftype(1:8),iftype)
      call string_cc2hh(dfile,idfile)
      call modgrid_delete(obj)
      return
      end subroutine modgrid_dfile_c

