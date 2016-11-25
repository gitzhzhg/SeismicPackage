!<CPS_v1 type="PROGRAM"/>
!!------------------------------ segyfix.f90 --------------------------------!!
!!------------------------------ segyfix.f90 --------------------------------!!
!!------------------------------ segyfix.f90 --------------------------------!!


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
!                       C P S   P R O G R A M
!
! Name       : segyfix
! Category   : stand-alone
! Written    : 2003-04-08   by: R.S.Day
! Revised    : 2003-04-08   by: R.S.Day
! Maturity   : production 
! Purpose    : Change the a segy sample rate to a user specified value
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc> 

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!  This program opens a segy file for r/w , prints the current sample rate
!  to stdout, and prompts the user for a new sample rate. The new sample
!  rate is written in place to the existing file. 
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2003-04-08  R.S.Day    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


      program segyfix
      implicit none
      character(len=120) :: fname
      character(len=12)  :: wtype
      integer  :: i_err
      integer  :: fsize
      integer  :: wrdsz
      integer  :: stdo
      integer  :: segyfix_sr
      character(len=100),save :: SEGYFIX_IDENT = &
'$Id: segyfix.f90,v 1.4 2008/02/15 19:52:15 mengewm Exp $'

      fname = ' '
      stdo = 6
      write(stdo,*) 'enter the  segy file name'
      read(*,'(a)') fname
       i_err = segyfix_sr(fname,&
       stdo,wtype,fsize,wrdsz)

      stop
      endprogram

      integer function segyfix_sr(fname,&
       stdo,wtype,fsize,wrdsz) result(status)
      use segy_module
      use modgrid_module
      use cio_module
      implicit none
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),intent(inout)  :: wtype
      integer,intent(inout)           :: fsize
      integer,intent(out)             :: wrdsz

      double precision                :: fsize_dbl
      integer    :: i_err
      integer    :: ntord
      integer    :: ndidrd
      integer    :: ibuff(900)
      integer    :: bytes_per_trace
      integer    :: ntraces
      integer    :: nscan
      real       :: rtraces
      character(len=4) :: mode
      character(len=8) :: ftype
      character(len=3200) :: obuff
      type(segy_bin_hdr)   :: binhdr

      integer    :: lun
      real       :: dt
      integer    :: idt
      integer    :: n(3)
      real       :: o(3),d(3)

      status = -1
      wtype=' '
      wrdsz = 4
      n = 1
      o = 0.0
      d = 1.0
      fsize_dbl = fsize
      ftype = modgrid_ftype(fname,stdo,fsize_dbl)
      fsize     = fsize_dbl
      if(ftype /= 'SEGY') then
      endif
      if(fsize <=0) then
         write(stdo,*) 'segyfix_sr: error in segy fsize=',fsize
         write(stdo,*) 'segyfix_sr: error fname=',fname
         return
      endif
      ftype= 'SEGY'
      wtype= 'IBM'
      mode = 'r+'
      lun = cio_fopen(fname,mode)
      if(lun <= 0) then
        write(stdo,*) 'segyfix_sr: open error:',trim(fname)
        return
      endif
      ! read in 3600 bytes of file header info
      ntord = 900
      ndidrd= cio_fread(ibuff,4,ntord,lun)
      if(ndidrd < ntord) then
        print *,'segyfix_sr: ndidrd=',ndidrd,' < ntord=',ntord
        i_err = cio_fclose(lun)
        return
      endif
      i_err = segy_buf2binh(ibuff(801:900),binhdr)
      if(i_err <0) then
        write(stdo,*) 'segyfix_sr: error in segy_buf2binh'
        return
      endif
      
      wrdsz = 4
      if(binhdr%format==3) wrdsz=2
      if(binhdr%format==5) wrdsz=1
      bytes_per_trace = 240 + wrdsz*binhdr%hns
      rtraces = (fsize-3600)/bytes_per_trace
      ntraces = nint(rtraces)

      ! save the segy ndpt and dt
      n(1) = binhdr%hns
      n(1) = max(1,n(1))
      o(1) = 0.0
      d(1) = binhdr%hdt*.000001
      idt  = binhdr%hdt
      write(stdo,*) 'segyfix_sr: sample rate idt=',binhdr%hdt,' usec'
      write(stdo,'(a)',advance='no') 'segyfix_sr: enter new idt:'
      read(*,'(i6)') idt
      write(stdo,*) 'segyfix_sr: you enterd idt=',idt,' usec'
      if(idt<0) then
        write(stdo,*) 'segyfix_sr: error - idt<1 is invalid'
        idt = binhdr%hdt
      endif
      if(idt>32767) then
        write(stdo,*) 'segyfix_sr: error - idt>32767 is invalid'
        idt= binhdr%hdt
      endif
      binhdr%hdt = idt

      i_err = cio_fseek(lun,3200,0)
      i_err = segy_write_binhdr(lun,binhdr)
      i_err = cio_fclose(lun)

! save the ascii version of the 3200 byte ebcdic header
!     obuff(1:3200) = transfer(ibuff(1:800),obuff(1:3200))
!     if(obuff(1:1) /= 'c' .and. obuff(1:1)/='C') &
!       call wrdc_ebc_asc(obuff(1:3200))
      status = 0
      return
      end function segyfix_sr
