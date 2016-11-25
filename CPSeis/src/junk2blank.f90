!<CPS_v1 type="PROGRAM"/>
!!------------------------------ junk2blank.f90 ------------------------------!!
!!------------------------------ junk2blank.f90 ------------------------------!!
!!------------------------------ junk2blank.f90 ------------------------------!!


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
! Name       : junk2blank
! Category   : stand-alone
! Written    : 2001-05-24   by: Karen Goodger
! Revised    : 2001-05-24   by: Karen Goodger
! Maturity   : beta
! Purpose    : Change any character outside the ascii character set to a blank.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
! 
!  This program was designed to prepare some files, which may erroneously
!  contain some non-ascii characters, for permsave.  It will replace non-ascii
!  characters with a blank and save it a file with the expension .ascii.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!  Input to this program is the permsave.skip file. junk2blank will operate
!  only on the .rpt. names in this file.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author         Description
!     ----        ------         -----------
!  1. 2001-05-24  Karen Goodger  Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!


      program junk2blank
!!!   use abcdef_module   ! for each module to be used.
      implicit none

      character (len=160) :: filename,infile
      integer             :: istat,k,lun=2,lun_infile=1,lunnew=3

      print*,' Input file containing file names to be converted'

      read*,infile

      open(lun_infile,file=infile,status='old',iostat=istat)
      if(istat.ne.0)then
        print*,' Unable to open file ...'
        print*,infile
        stop
      endif

      DO
        read(lun_infile,9001,iostat=istat)filename
        if(istat.lt.0)exit
        k=index(filename,'.rpt')
        if(k.eq.0)cycle
        call junk2blank_testtext(lun,lunnew,filename)
        
      ENDDO


 9001 format(A)

      end program junk2blank
      subroutine junk2blank_testtext(lun,lunnew,filename)

!         Read a file as if it is character data
!         If there are any characters out of the range of the ascii character
!           set - replace it with a blank 

      implicit none
      integer, intent(in) :: lun,lunnew
      character(len=*),intent(in) :: filename

!             local variables
      integer :: istat,i,j
      character(len=120) :: record
      character(len=160) :: filenew

      open(lun,file=filename,status='old',iostat=istat)
      if(istat.ne.0)then
        go to 500
      endif
      filenew=trim(filename) // '.ascii'
      open(lunnew,file=filenew,status='replace',iostat=istat)
      if(istat.ne.0)go to 500

      DO
        read(lun,'(A)',iostat=istat)record
        if(istat.lt.0)exit
        do i=1,120
          j=ichar(record(i:i))
          if(j.lt.0.or.j.gt.127)then
            record(i:i)=' '
          endif
        enddo
        write(lunnew,'(A)',iostat=istat)trim(record)
      ENDDO
 500  continue
      close(lun)
      close(lunnew,status='keep')
      return
      end subroutine junk2blank_testtext


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!




