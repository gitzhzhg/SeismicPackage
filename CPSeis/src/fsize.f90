!<CPS_v1 type="PROGRAM"/>
!!------------------------------ fsize.f90 --------------------------------!!
!!------------------------------ fsize.f90 --------------------------------!!
!!------------------------------ fsize.f90 --------------------------------!!


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
! Name       : fsize (File Size Estimator)
! Category   : stand-alone
! Written    : 2001-01-11   by: Bill Menger
! Revised    : 2001-01-11   by: Bill Menger
! Maturity   : production
! Purpose    : Estimate file sizes for trace data.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
! fsize estimates file sizes (for trace data) based on user-input trace length,
! sample rate, etc.
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
!  1. 2001-01-11  Bill Menger Converted from old system.
!</history_doc>
!-------------------------------------------------------------------------------


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! Solaris' f90 compiler has run-time errors when using successive "advance=no"
! in a write statement, so extra write statements were put between them.
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!

module fsize_module
  implicit none
  private

  public :: fsize_read

  interface fsize_read
    module procedure fsize_read_double
    module procedure fsize_read_real
    module procedure fsize_read_integer
    module procedure fsize_read_character
    module procedure fsize_read_logical
  end interface

  contains

  function fsize_read_double(default,fsize) result (response)
    double precision,intent(in)  :: default
    character(len=*),intent(in)  :: fsize
    double precision             :: response
    !------------------------------------------    
    character(len=32)            :: a                       
    character(len=64)            :: chr_default
    integer                      :: cntr


    cntr = 0
    write(chr_default,'(a,E12.5,a)')' Hit <Enter> for default[',default,']:'
10  continue
    write(6,'(a,1x,a)',advance="no")trim(fsize),trim(chr_default)//' '
    read(5,'(a)',err=20 ,end=20 )a
    write(6,'(a)')" "
    if(len_trim(a) == 0 ) go to 999
    read(a,*,err=20 ,end=20 )response
    return

20  write(6,'(a)')"Error reading response.  Please re-enter"
    cntr = cntr + 1
    if(cntr > 10 ) stop 'Error on parameter input. Stopping job.'
    go to 10

999 continue
    response = default
    
  end function fsize_read_double

  function fsize_read_real(default,fsize) result (response)
    real,intent(in)              :: default
    character(len=*),intent(in)  :: fsize
    real                         :: response
    !------------------------------------------    
    character(len=32)            :: a                       
    character(len=64)            :: chr_default
    integer                      :: cntr


    cntr = 0
    write(chr_default,'(a,E12.5,a)')' Hit <Enter> for default[',default,']:'
10  continue
    write(6,'(a,1x,a)',advance="no")trim(fsize),trim(chr_default)//' '
    read(5,'(a)',err=20 ,end=20 )a
    write(6,'(a)')" "
    if(len_trim(a) == 0 ) go to 999
    read(a,*,err=20 ,end=20 )response
    return

20  write(6,'(a)')"Error reading response.  Please re-enter"
    cntr = cntr + 1
    if(cntr > 10 ) stop 'Error on parameter input. Stopping job.'
    go to 10

999 continue
    response = default
    
  end function fsize_read_real

  function fsize_read_integer(default,fsize) result (response)
    integer,intent(in)           :: default
    character(len=*),intent(in)  :: fsize
    integer                      :: response
    !------------------------------------------    
    character(len=32)            :: a                       
    character(len=64)            :: chr_default
    integer                      :: cntr


    cntr = 0
    write(chr_default,'(a,i11,a)')' Hit <Enter> for default[',default,']:'
10  continue
    write(6,'(a,1x,a)',advance="no")trim(fsize),trim(chr_default)//' '
    read(5,'(a)',err=20 ,end=20 )a
    write(6,'(a)')" "
    if(len_trim(a) == 0 ) go to 999
    read(a,*,err=20 ,end=20 )response
    return

20  write(6,'(a)')"Error reading response.  Please re-enter"
    cntr = cntr + 1
    if(cntr > 10 ) stop 'Error on parameter input. Stopping job.'
    go to 10

999 continue
    response = default
    
  end function fsize_read_integer

  function fsize_read_logical(default,fsize) result (response)
    logical,intent(in)           :: default
    character(len=*),intent(in)  :: fsize
    logical                      :: response
    !------------------------------------------    
    character(len=32)            :: a                       
    character(len=64)            :: chr_default
    integer                      :: cntr


    cntr = 0
    write(chr_default,'(a,L7,a)')' Hit <Enter> for default[',default,']:'
10  continue
    write(6,'(a,1x,a)',advance="no")trim(fsize),trim(chr_default)//' '
    read(5,'(a)',err=20 ,end=20 )a
    write(6,'(a)')" "
    if(len_trim(a) == 0 ) go to 999
    read(a,*,err=20 ,end=20 )response
    return

20  write(6,'(a)')"Error reading response.  Please re-enter"
    cntr = cntr + 1
    if(cntr > 10 ) stop 'Error on parameter input. Stopping job.'
    go to 10

999 continue
    response = default
    
  end function fsize_read_logical

  function fsize_read_character(default,fsize) result (response)
    character(len=*),intent(in)  :: default
    character(len=*),intent(in)  :: fsize
    character(len=64)            :: response
    !------------------------------------------    
    character(len=64)            :: a                       
    character(len=64)            :: chr_default
    integer                      :: cntr


    cntr = 0
    write(chr_default,'(3a)')' Hit <Enter> for default[',default,']:'
10  continue
    write(6,'(a,1x,a)',advance="no")trim(fsize),trim(chr_default)//' '
    read(5,'(a)',err=20 ,end=20 )a
    write(6,'(a)')" "
    if(len_trim(a) == 0 ) go to 999
    read(a,*,err=20 ,end=20 )response
    return

20  write(6,'(a)')"Error reading response.  Please re-enter"
    cntr = cntr + 1
    if(cntr > 10 ) stop 'Error on parameter input. Stopping job.'
    go to 10

999 continue
    response = default
    
  end function fsize_read_character

end module fsize_module

program fsize

  use fsize_module
  implicit none

  double precision,parameter ::  giga         = 1024.*1024.*1024.
  double precision,parameter ::  mega         = 1024.*1024.
  double precision           ::  dt,tlen,fsizm,fsizg,nbyt,ntrace,nwih
  double precision           ::  nbyt_sm,nbyt_tr,nbyt_hd,nbyt_rec
  integer                    ::  ndpt,nbit_sm=0

!================ user input parameters ====================
  dt     = fsize_read(4d-3,'Sample Rate (sec)')
  tlen   = fsize_read(4d0,'Trace Length(sec)')      
  ntrace = 1d0*fsize_read(200000,'number of traces')
  nwih   = 1d0*fsize_read(64,'Number of words in trc header')
  do while (nbit_sm == 0 )  
    nbit_sm= fsize_read(32,'Word size (bits) of samples')
    if(nbit_sm < 8 .or. modulo(nbit_sm,8) /= 0 .or. nbit_sm > 32 ) then
      nbit_sm = 0
      write(6,'(a)') 'Nbits must be 8,16,or 32.'
    end if
  end do
  nbyt_sm = 1d0*(nbit_sm/8)

  NDPT = NINT(tlen/DT) + 1
  tlen = (NDPT-1)*DT

  write(6,'(a,F5.3,a,F9.3,a,I9,a,i11)')&
  'DT = ',dt,' Time = ',tlen,' NDPT = ',ndpt,' NTRC = ',nint(ntrace)

  nbyt_tr = nbyt_sm*ndpt
  nbyt_hd = 8*nwih
  nbyt_rec = nbyt_tr + nbyt_hd

  write(6,'(a)')&
  'File sizes do not include ASCII header for CPS files or any HISTORY'
  write(6,'(a)')&
  'records (80 bytes/card).'
  write(6,'(a)')&
  '-------------------------------------------------------------------'

!     CPS `-------------------------------------------------------

  nbyt  = ntrace*nbyt_rec
  fsizm = nbyt/mega
  fsizg = nbyt/giga
  write(6,'(a)',advance="no")'  CPS: '
  write(6,'(2(a,F14.2,1x))')   'File Size(mb): ',fsizm,&
                               'File Size(gb): ',fsizg

!     SEGY -------------------------------------------------------
  
  nbyt_hd = 60*4
  nbyt_rec = nbyt_hd + nbyt_tr
  nbyt     = 3600 + nbyt_rec*ntrace
  fsizm = nbyt/mega
  fsizg = nbyt/giga
  write(6,'(a)',advance="no")' SEGY: '
  write(6,'(2(a,F14.2,1x))')   'File Size(mb): ',fsizm,&
                               'File Size(gb): ',fsizg
!     QTROT ------------------------------------------------------

  nbyt_hd = nwih*4
  if(modulo(ndpt,2) /= 0 ) ndpt = ndpt + 1
  nbyt_tr  = ndpt * 4
  nbyt_rec = nbyt_hd + nbyt_tr
  nbyt     = 4096 + nbyt_rec*ntrace
  fsizm = nbyt/mega
  fsizg = nbyt/giga
  write(6,'(a)',advance="no")'QTROT: '
  write(6,'(2(a,F14.2,1x))')   'File Size(mb): ',fsizm,&
                               'File Size(gb): ',fsizg

  write(6,'(a)')&
  '-------------------------------------------------------------------'

END program fsize
