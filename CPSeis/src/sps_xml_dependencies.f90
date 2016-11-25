
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
!<CPS_v1 type="PROGRAM"/>
!!----------------------- sps_xml_dependencies.f90 --------------------!!
!!----------------------- sps_xml_dependencies.f90 --------------------!!
!!----------------------- sps_xml_dependencies.f90 --------------------!!




!<brief_doc>
!-------------------------------------------------------------------------------
!                         C P S   P R O G R A M 
!
! Name       : SPS_XML_DEPENDENCIES
! Category   : stand-alone
! Written    : 2003-03-18   by: Tom Stoeckley
! Revised    : 2003-08-08   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Program to generate XML dependencies for SPS.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This program is to be used in the following scripts for generating
! dependencies for the makefile used to manage the updates of process and
! primitive modules in the CPS processing system:
!                   depend   sps_depend
!
! This program creates XML dependencies.
! A program called sps_fortran_dependencies creates Fortran-90 dependencies.
! A script called sps_c_dependencies creates C and C++ dependencies.
!
! This program reads a list of files from standard in and writes the
! dependencies to standard out.  It scans each file looking gui_def tags
! and INCLUDE statements between the tags.  It assumes that the gui_def
! tags and the INCLUDE word are lower case.  It also assumes that each
! line starts with an exclamation point, and that there are no spaces
! before or within the gui_def tags or the INCLUDE word.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  2. 2003-08-08  Stoeckley    Increase array sizes.
!  1. 2003-03-18  Stoeckley    Initial version.
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


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!
! To compile this program, it is necessary only to run a command something
! like the following in a platform-specific directory under the source code
! directory, because this file is completely self-contained and does not
! need any libraries:
!
!         f90 -o sps_xml_dependencies ../sps_xml_dependencies.f90
!
! For example, the following scripts can be executed:
!
!       #! /bin/csh
!       # for platform linuxab75 or linuxab75_debug
!       ab75_f90 -en -cpu:p6 -YDEALLOC=MINE -o \
!                       sps_xml_dependencies   \
!                    ../sps_xml_dependencies.f90
!
!       #! /bin/csh
!       # for platform sol_new
!       f90 -dalign -o sps_xml_dependencies \
!                   ../sps_xml_dependencies.f90
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!------------------- sps xml dependencies module -----------------------!!
!!------------------- sps xml dependencies module -----------------------!!
!!------------------- sps xml dependencies module -----------------------!!


module sps_xml_dependencies_module
implicit none

character(len=100),public,save :: SPS_XML_DEPENDENCIES_IDENT = &
'$Id: sps_xml_dependencies.f90,v 1.2 2003/08/08 18:34:41 Stoeckley prod sps $'

contains


!!-------------------- sps write xml dependencies -------------------------!!
!!-------------------- sps write xml dependencies -------------------------!!
!!-------------------- sps write xml dependencies -------------------------!!

   ! backslash is set to '  \\' (four characters).
   ! then only the first three characters are used.
   ! this procedure makes pg90 happy and still keeps other compilers happy.
   ! pgf90 needs the double backslash to mean one backslash.


subroutine sps_write_xml_dependencies (filename5,root,lun2,num,hello)
implicit none
character(len=*),intent(in) :: filename5,root
integer         ,intent(in) :: lun2
integer         ,intent(in) :: num
character(len=*),intent(in) :: hello(:)
character(len=111)          :: line
character(len=111)          :: include_name
character(len=111)          :: deps(2000)
integer                     :: lun5,kount,i,stat,ndeps,indx
logical                     :: found,doit
character(len=8)            :: backslash

                    integer,save :: xxxx = 0

!---------- get list of dependencies:

lun5      = 50
kount     = 0
ndeps     = 0
doit      = .false.

open(lun5, file = filename5, action = 'READ', status = 'OLD', iostat = stat)
if (stat /= 0) then
     print *, 'open error on unit ',lun5,' for file ',trim(filename5)
     return
end if

do
     read (lun5,'(A)',iostat=stat) line
     if (stat /= 0) exit
     if (line == '!<gui_def>') then
          doit = .true.
     else if (line == '!</gui_def>') then
          exit
     else if (.not.doit) then
          cycle
     else if (line(1:9) == '!<include') then
          include_name = line(10:)
          include_name = adjustl(include_name)
          indx = scan(trim(include_name), '>')
          if (indx > 0) include_name(indx:) = ' '  ! remove '>' from name.
          found = .false.
          do i = 1,num
               if (include_name == trim(hello(i))//'.f90') then
                    found = .true.
                    exit
               end if
          end do
          if (found) then
               ndeps = ndeps + 1
               deps(ndeps) = include_name
          end if
     end if
end do
close(lun5, status = 'KEEP')

!---------- write dependencies to file:

if (.not.doit) return

line(1:22)  = trim(root)//'.xml'
line(23:25) = ' : '
line(26:)   = trim(root)//'.f90'
backslash   = '  \\'                          
if (ndeps > 0) line(61:63) = backslash(1:3)
write (lun2,*) trim(line)
do i = 1,ndeps
       line(1:25) = ' '
       line(26:)  = deps(i)
       if (i < ndeps) line(61:63) = backslash(1:3)
       write (lun2,*) trim(line)
end do
write (lun2,*) ' '
return
end subroutine sps_write_xml_dependencies

end module sps_xml_dependencies_module


!!-------------------------- main program --------------------------------!!
!!-------------------------- main program --------------------------------!!
!!-------------------------- main program --------------------------------!!


program sps_xml_dependencies
use sps_xml_dependencies_module
implicit none
character(len=111) :: line,full(2000),root
character(len=111) :: ext,hello(2000)
integer            :: lun1,lun2,stat,indx,num,i,length

lun1 = 5
lun2 = 6

!!!!!!!!!! get list of potential dependencies:

num = 0
do
     read (lun1,'(A)',iostat=stat) line
     if (stat /= 0) exit
     indx = scan(trim(line),'.',.true.)
     if (indx == 0) cycle
     root  = line(1:indx-1)
     ext = line(indx:)
     if (ext /= '.f90') cycle
     indx = scan(trim(root),'/',.true.)
     if (indx > 0) root = root(indx+1:)
     length = len_trim(root)
     num = num + 1
     full (num) = line
     hello(num) = root
end do

!!!!!!!!!! write xml dependencies:

rewind lun1
do i=1,num
     root = hello(i)
     call sps_write_xml_dependencies (full(i),root,lun2,num,hello)
end do
end program sps_xml_dependencies


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!

