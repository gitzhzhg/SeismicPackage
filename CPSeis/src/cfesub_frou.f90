!<CPS_v1 type="AUXILIARY_FILE"/>
!!----------------------------- cfesub_frou.f90 -----------------------------!!
!!----------------------------- cfesub_frou.f90 -----------------------------!!
!!----------------------------- cfesub_frou.f90 -----------------------------!!

        ! other files are  cfesub.c  
 
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


!<brief_doc>
!-------------------------------------------------------------------------------

!                   C P S   P R O G R A M
!
! Name       : CFESUB_FROU
! Category   : stand-alone
! Written    : 2003-01-05   by: Karen Goodger
! Revised    : 2004-01-07   by: Karen Goodger
! Maturity   : beta
! Purpose    : Submit CPS Job Files
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY 
!
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2004-01-07  Goodger    Add routine cfesub_remotename.
!  2. 2004-01-05  Goodger    Add routine cfesub_jobdatainfo.
!  1. 2004-01-05  Goodger    Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>



!!-------------------------- start of coding ------------------------------!!
!!-------------------------- start of coding ------------------------------!!
!!-------------------------- start of coding ------------------------------!!


      module cfesub_frou_module
      use cardset_module
      use putsys_module
      use string_module
      implicit none

      character(len=100),public,save :: CFESUB_FROU_IDENT = &
'$Id: cfesub_frou.f90,v 1.3 2004/01/07 19:17:43 Goodger beta sps $'

      end module cfesub_frou_module

      subroutine cfesub_timestamp(holcfilename)

      use cfesub_frou_module


      integer :: holcfilename(*),lun1=1

      integer :: istat,j,k
      character(len=180) :: card,cfilename,cmd,tmpfile


      call string_hh2cc(holcfilename,cfilename)

      tmpfile=trim(cfilename) // '_submitinfo'

      open(lun1,file=tmpfile,iostat=istat,status='old')
      if(istat.ne.0)return
      read(lun1,'(A)',iostat=istat)card


      k=index(cfilename,'.')
      j=index(card,'.')
      cmd='touch ' // cfilename(1:k-1) // '_' // card(1:j-1) // '.submitted'
      call putsys_cmd(trim(cmd))
      close(lun1,status='delete')      


      end subroutine cfesub_timestamp

      subroutine cfesub_jobdatainfo(hcfilename,hrlocation)

      use cfesub_frou_module

      integer :: hcfilename(*),hrlocation(*),istat,lun1=1
      type(cardset_struct),pointer :: cobj
      character(len=181) :: cfilename,card,msg,rlocation
      logical :: found=.false.

      call string_hh2cc(hcfilename,cfilename)

      call cardset_create(cobj)
      write(6,'(A)')cfilename
      open(lun1,file=cfilename,iostat=istat,status='old')
      if(istat.ne.0)return
      DO
        read(lun1,'(A)',iostat=istat)card
        if(istat.lt.0)return
        if(found)then
          k=index(card,'</PROCESS>')
          if(k.ne.0)exit
          call cardset_add_card(cobj,card)
        else
          k=index(card,'<PROCESS name="JOB_DATA">')
          if(k.ne.0)found=.true.
        endif
      ENDDO
      call cardset_get_scalar(cobj,'RLOCATION',rlocation,msg)

      call string_cc2hh(rlocation,hrlocation)

      close(lun1)
      


      end subroutine cfesub_jobdatainfo

      subroutine cfesub_remotename(hcfilename,hrname)
      use cfesub_frou_module

      integer :: i1,i2,hcfilename(*),hrname(*),k
      character(len=180) :: cfilename,ctemp,rname

      call string_hh2cc(hcfilename,cfilename)
      k=index(cfilename,'.job')
      i2=k-1
      k=index(cfilename,'/',.true.)
      i1=k+1
      ctemp=cfilename(i1:i2);
      rname=trim(ctemp) // '_remote'
      call string_cc2hh(rname,hrname)

      end subroutine cfesub_remotename


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

