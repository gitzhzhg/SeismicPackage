!<CPS_v1 type="PROGRAM"/>
!!------------------------------ segyview.f90 --------------------------------!!
!!------------------------------ segyview.f90 --------------------------------!!
!!------------------------------ segyview.f90 --------------------------------!!


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
! Name       : segyview
! Category   : stand-alone
! Written    : 2001-01-03   by: Bill Menger
! Revised    : 2001-09-07   by: Bill Menger
! Maturity   : beta
! Purpose    : View SEGY files using TEXT-only quick viewer
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
! SEGYVIEW uses the base functions of CPS to open, read, and display the 
! contents from SEGY files.  One may view the EBCDIC header, BINARY header, and
! any trace header or trace values (using a tab-plot).  If a SEGY file cannot be
! opened with SEGYVIEW then it cannot be read by CPS.
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
!  2. 2001-09-07  Bill Menger Modified to use the put module,added ident.
!  1. 2001-01-03  Bill Menger Initial version.
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>

!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!

program segyview
use put_module
use named_constants_module
use wrdc_module
use swap_module
use cio_module
use segy_module
use trcio_module
implicit none

character(len=100) :: segyview_ident = &
'$Id: segyview.f90,v 1.2 2008/02/15 20:18:51 mengewm Exp $'
type(trcio_struct),pointer    :: file
type(segy_ebcdic_hdr) :: ebcdic
type(segy_bin_hdr)    :: binhdr
type(segy_trc_hdr)    :: segyhd
double precision      :: cpshd(64)
real,allocatable      :: trace(:)
integer               :: h(60) , err
character(len=8)      :: key
character(len=128)    :: filename
logical               :: swap,ibmswap,newtrace
double precision      :: dt,tstart
integer               :: ns,trcnum
character(len=1)      :: go,cont
character(len=32)     :: tracenumber,values
character(len=132)    :: str

integer :: i,j,unit,status,ifirst,ilast,irange

100 continue
if(associated(file)) status = trcio_close(file)
status=put('Segy File (q to stop): ')
read(5,'(A)')filename
status=put('filename: %s'//char(10),trim(filename))

if(trim(filename) == 'q' .or. trim(filename) == 'Q') stop
if(len_trim(filename) <= 0 ) go to 100

file => trcio_open(filename,'r')
if(.not. associated(file)) then
  status=put('Error opening '//trim(filename)//'.  Read another?(y/n): ')
  read*,cont
  if(cont == 'y' .or. cont == 'Y') then
    go to 100
  else
    stop
  endif
endif

unit = file%lun
call cio_frewind(unit)

if(segy_is_file_segy(unit) == 1)  then
  status=put(trim(filename)//' is not SEGY.  Read another?(y/n): ')
  read*,cont
  if(cont == 'y' .or. cont == 'Y') then
    go to 100
  else
    stop
  endif
endif

status = segy_read_ebchdr(unit,ebcdic)
if(status /= 0 ) then
  status=&
  put('Error reading ebcdic header.  Status= %d Keep going?(y/n): ',status)
  read*,cont
  if(cont /= 'y' .and. cont /= 'Y') go to 100
endif
do i = 1, 40
  status=put(ebcdic%h(i)//char(10))
end do

status=put('Hit return to continue')
read(5,'(a)')cont

status = segy_read_binhdr(unit,binhdr)
if(status /= 0 ) then
  status=&
  put('Error reading binary header.  Status= %d Keep going?(y/n): ',status)
  read*,cont
  if(cont /= 'y' .and. cont /= 'Y') go to 100
endif
call segy_print_binhdr(binhdr)
!if (swap_endian() /= binhdr%endian ) then 
!  swap = .true.
!else
!  swap = .false.
!endif

allocate(trace(int(binhdr%hns)))
trcnum = 0
do
  trcnum = trcnum + 1
  status=put('Trace number to read[%d]:',trcnum)
  read(5,'(a)')tracenumber
  newtrace=.true.
  if(len_trim(tracenumber) == 0 ) then 
  else
    if(trim(tracenumber) == 'q' .or. trim(tracenumber)=='Q') exit
    read(tracenumber,*)trcnum
  endif
  status = trcio_read_trace(file,cpshd,trace,trcnum)
  if(status /= 0 ) then
    status=&
    put('Error reading trace.  Status= %d Keep going?(y/n): ',status)
    read*,cont
    if(cont /= 'y' .and. cont /= 'Y') exit
  endif
  tstart = file%tmin
  dt     = file%dt
  ns     = file%num_values
  call segy_cpshd_to_segyhd(cpshd,segyhd,tstart,dt,ns)
  call segy_print_trace_hd(segyhd)
  irange = 20
  ifirst = 1          
  ilast  = min(irange,nint(cpshd(HDR_BOTTOM_MUTE))-1)
200 continue
    ifirst    = min(int(segyhd%ns),max(1,ifirst))
    if(newtrace) then
      newtrace=.false.
    else
      ilast     = max(ifirst,min(ilast,int(segyhd%ns)))
    endif
    status=put('Samples to plot[%d] [',ifirst)
    status=put('%d]:',ilast)
    read(5,'(a)')values
    if(len_trim(values) /= 0 ) then 
      if(trim(values)=='q' .or. trim(values)=='Q' .or. trim(values) =='0')cycle
      read(values,*,err=200)ifirst,ilast
    endif
  ifirst    = min(int(segyhd%ns),max(1,ifirst))
  ilast     = max(ifirst,min(ilast,int(segyhd%ns)))
  call segy_tabplot(trace(1),ifirst,ilast)
  irange = ilast - ifirst + 1
  ifirst = ilast + 1
  ilast  = ifirst + irange - 1
  go to 200
end do
deallocate (trace)
go to 100

end program segyview

!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
