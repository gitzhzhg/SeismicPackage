!<CPS_v1 type="PRIMITIVE"/>
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
!                        C P S   P R I M I T I V E                        
!
! Name       : getlun
! Category   : io
! Written    : 1986-01-01 by: Doug Hanson
! Revised    : 2001-03-23 by: Bill Menger
! Maturity   : production     2001-04-26
! Purpose    : Get a logical unit number
! Portability: No known problems
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                            
!
!    GETLUN searches all unit numbers and returns the first free one 
!    it finds. It starts the search just after the last unit number
!    it returned.
!
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS                         
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!                       INPUT AND OUTPUT ARGUMENTS                        
!
! For each subroutine or function documented here, each argument is flagged as 
! follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!    opt = optional argument.
!
!
!                     o    o(opt)
!        call getlun (lun, status )                                         
!        integer   lun     NEXT AVAILABLE LOGICAL UNIT NUMBER (-1=failed)
!        integer   status  status code 0=ok, -1 = failed. (optional)
!
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                              
!     Date     Author       Description                                 
!     ----     ------       -----------                                 
!11.  2001-04-26 Menger     Added ident string,fixed doc.
!10.  1999-08-13 Menger     Removed doc for getluns.
! 9.  1999-08-12 Menger     Removed getlun_free and getlun_s
! 8.  1999-08-11 Menger     Added getlun_free function.
! 7.  1999-08-10 Menger     Fix bug testing for optional argument.
! 6.  1999-08-05 Menger     Modified to use more units and changed test for open
! 5.  1999-06-09 Goodger    Converted from old system.
! 4.  1999-01-25 Goodger    Begin using the fortran90 compiler.         
! 3.  1992-05-22 Stoeckley  Entire routine replaced.  See note 1 above. 
! 2.  1992-04-06 Day        Changes made to run under Unix.             
! 1.  unknown    Hanson     Original version                            
!-------------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS        
! No known limitations.
!</portability_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
module getlun_module

  implicit none
  private
  public :: getlun
  

  character(len=100),public,parameter :: getlun_ident = &
  '$Id: getlun.f90,v 1.11 2001/04/25 18:38:20 sps prod sps $'
  logical                           :: opened
  integer,parameter                 :: GETLUN_ERROR = -1
  integer,parameter                 :: GETLUN_OK    =  0
  integer,parameter                 :: lstart=1, lstop=99
  integer,save                      :: last=0
  
  contains
  
  subroutine getlun (lun,ierr)
    integer,intent(out)               :: lun
    integer,intent(out),optional      :: ierr
    !----------- Local variables -------------------
    integer                           :: i,local_ierr
  
    do i=lstart,lstop
      last=last+1
      if (last.gt.lstop) last=lstart
      inquire (last,opened=opened,iostat=local_ierr)
      if(local_ierr /= 0)then
        lun = GETLUN_ERROR
        if(present(ierr)) ierr = GETLUN_ERROR
        return
      endif
      if (opened) then
        cycle
      else
        lun=last
        if(present(ierr)) ierr = GETLUN_OK
        return
      end if
    end do
    lun = GETLUN_ERROR 
    if(present(ierr)) ierr = GETLUN_ERROR
    return
  end subroutine getlun

end module getlun_module
