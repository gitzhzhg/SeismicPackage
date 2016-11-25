
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
!!----------------------- sps_fortran_dependencies.f90 --------------------!!
!!----------------------- sps_fortran_dependencies.f90 --------------------!!
!!----------------------- sps_fortran_dependencies.f90 --------------------!!




!<brief_doc>
!-------------------------------------------------------------------------------
!                         C P S   P R O G R A M 
!
! Name       : SPS_FORTRAN_DEPENDENCIES
! Category   : stand-alone
! Written    : 1999-12-02   by: Tom Stoeckley
! Revised    : 2003-08-11   by: Karen Goodger
! Maturity   : production
! Purpose    : Program to generate Fortran dependencies for SPS.
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
! This program creates Fortran-90 dependencies.
! A script called sps_c_dependencies creates C and C++ dependencies.
!
! This program reads a list of files from standard in and writes the
! dependencies to standard out.  It scans each file looking for information
! on USE and MODULE statements.  It works for upper and lower and mixed case
! code.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
! Limitations:
!
! This program should work correctly if a USE statement contains qualifiers
! such as ONLY after the module name, but it will not pick up more than
! one module name on the same USE statement.  There may be occasional
! additional misbehavior which should be easy to fix in the Fortran-90
! source code, such as using the word MODULE as the name of a variable.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  8. 2003-08-11  Goodger    Add special handling for swap.  swap_module should
!                            be swap_frou_module.
!  7. 2003-08-08  Stoeckley  Increase array sizes.
!  6. 2003-03-18  Stoeckley  Improve the output formatting for easier reading.
!  5. 2002-12-02  Stoeckley  Remove requirement to link to the STRING module.
!  4. 2002-04-09  Stoeckley  Remove all references to 'frou'.
!  3. 2000-08-07  Stoeckley  Doubled the backslash in a character variable
!                             to make the portland group compiler happy.
!  2. 1999-12-14  Stoeckley  Fix bug which had been mistreating _frou files.
!  1. 1999-12-02  Stoeckley  Initial version.
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
!      f90 -o sps_fortran_dependencies ../sps_fortran_dependencies.f90
!
! For example, the following scripts can be executed:
!
!       #! /bin/csh
!       # for platform linuxab75 or linuxab75_debug
!       ab75_f90 -en -cpu:p6 -YDEALLOC=MINE -o \
!                   sps_fortran_dependencies   \
!                ../sps_fortran_dependencies.f90
!
!       #! /bin/csh
!       # for platform sol_new
!       f90 -dalign -o sps_fortran_dependencies \
!                   ../sps_fortran_dependencies.f90
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!------------------- sps fortran dependencies module -----------------------!!
!!------------------- sps fortran dependencies module -----------------------!!
!!------------------- sps fortran dependencies module -----------------------!!


module sps_fortran_dependencies_module
implicit none

character(len=100),public,save :: SPS_FORTRAN_DEPENDENCIES_IDENT = &
'$Id: sps_fortran_dependencies.f90,v 1.8 2003/08/11 19:30:46 Goodger prod sps $'

contains


!!------------------------- string to lower ------------------------!!
!!------------------------- string to lower ------------------------!!
!!------------------------- string to lower ------------------------!!

   ! this routine is copied from the STRING module so that this program
   ! will not have to be linked with an SPS library.

      subroutine string_to_lower (string)
      implicit none
      character(len=*),intent(inout) :: string                     ! argument
      integer                        :: length,i,i1,i3,i4,idiff,k  ! local

      i1     = ichar('a')
      i3     = ichar('A')
      i4     = ichar('Z')
      idiff  = i3 - i1
      length = len_trim(string)     ! faster than len(string)
      do i=1,length
           k = ichar(string(i:i))
           if (k >= i3 .and. k <= i4) string(i:i) = char(k-idiff)
      end do
      return
      end subroutine string_to_lower


!!----------------------- sps write dependencies -------------------------!!
!!----------------------- sps write dependencies -------------------------!!
!!----------------------- sps write dependencies -------------------------!!

   ! backslash is set to '  \\' (four characters).
   ! then only the first three characters are used.
   ! this procedure makes pg90 happy and still keeps other compilers happy.
   ! pgf90 needs the double backslash to mean one backslash.


subroutine sps_write_dependencies (filename5,root,lun2,num,hello)
implicit none
character(len=*),intent(in) :: filename5,root
integer         ,intent(in) :: lun2
integer         ,intent(in) :: num
character(len=*),intent(in) :: hello(:)
character(len=111)          :: line
character(len=111)          :: module_word
character(len=111)          :: module_word2
character(len=111)          :: module_name
character(len=111)          :: module_names(2000),deps(2000)
integer                     :: lun5,kount,i,stat,ndeps,indx
logical                     :: found
character(len=8)            :: backslash

!---------- get list of dependencies:

lun5      = 50
kount     = 0
ndeps     = 0

open(lun5, file = filename5, action = 'READ', status = 'OLD', iostat = stat)
if (stat /= 0) then
     print *, 'open error on unit ',lun5,' for file ',trim(filename5)
     return
end if

outer_loop: do
     read (lun5,'(A)',iostat=stat) line
     if (stat /= 0) exit
     call string_to_lower (line)
     line         = adjustl(line)
     module_word  = line(1:7)
     module_word2 = line(1:4)
     if (module_word == 'module') then
          module_name = line(8:)
          module_name = adjustl(module_name)
          if (module_name(1:9) == 'procedure') cycle
          if(module_name(1:11).eq.'swap_module')module_name='swap_frou_module'
          kount = kount + 1
          module_names(kount) = module_name
     else if (module_word2 == 'use') then
          module_name = line(5:)
          module_name = adjustl(module_name)
          if(module_name(1:11).eq.'swap_module')module_name='swap_frou_module'
          indx = scan(trim(module_name), ' ,')
          if (indx > 0) module_name(indx:) = ' '  ! remove ',xxxx' from name.
          do i = 1,kount
               if (module_name == module_names(i)) cycle outer_loop
          end do
          indx = len_trim(module_name)
          module_name = module_name(1:indx-7)   ! remove '_module' from name.
          found = .false.
          do i = 1,num
               if (module_name == hello(i)) then
                    found = .true.
                    exit
               end if
          end do
          if (found) then
               ndeps = ndeps + 1
               deps(ndeps) = trim(module_name)//'.o'
          end if
     end if
end do outer_loop
close(lun5, status = 'KEEP')

!---------- write dependencies to file:

line(1:22)  = trim(root)//'.o'
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
end subroutine sps_write_dependencies

end module sps_fortran_dependencies_module


!!-------------------------- main program --------------------------------!!
!!-------------------------- main program --------------------------------!!
!!-------------------------- main program --------------------------------!!


program sps_fortran_dependencies
use sps_fortran_dependencies_module
implicit none
character(len=99) :: line,full(2000),root
character(len=99) :: ext,hello(2000)
integer           :: lun1,lun2,stat,indx,num,i,length

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

!!!!!!!!!! write fortran object file dependencies:

rewind lun1
do i=1,num
     root = hello(i)
     call sps_write_dependencies (full(i),root,lun2,num,hello)
end do
end program sps_fortran_dependencies


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!

