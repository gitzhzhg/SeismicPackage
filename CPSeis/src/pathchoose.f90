
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- pathchoose.f90 -------------------------------!!
!!---------------------------- pathchoose.f90 -------------------------------!!
!!---------------------------- pathchoose.f90 -------------------------------!!


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
! Name       : PATHCHOOSE
! Category   : io
! Written    : 2001-06-11   by: Tom Stoeckley
! Revised    : 2008-12-11   by: Bill Menger
! Maturity   : beta
! Purpose    : Support for choosing a pathname from a file selection box.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This is a convenience routine to facilitate using the FILEBOX module
! to choose a pathname in the GUI associated with a process module.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS        
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                           CALLING SEQUENCE
!
!                                                                opt
!                                       o      i         i        i
!              call pathchoose_create (obj, keyword, extension, path)
!
! Place selected file path in scalar parameter:
!                                                     opt
!                                       b      b       i
!        doreturn = pathchoose_update (obj, pathname,trap)
!
! Append selected file path to array parameter:
!                                                                 opt
!                                       b       b         b        i
!        doreturn = pathchoose_update (obj, pathnames,npathnames,etrap)
!
!                                       b
!              call pathchoose_delete (obj)
!
! type(pathchoose_struct)           obj = pointer to the PATHCHOOSE object.
! character(len=*)              keyword = keyword for the pathname.
! character(len=*)            extension = default filter extension
!                                           (with or without .).
! character(len=*)                 path = default filter path.
! character(len=*)             pathname = name of selected file path in scalar.
! character(len=*),pointer pathnames(:) = array parameter values with name of
!                                           selected file path in last element.
! integer                    npathnames = number of array elements.
! external        ,optional        trap = scalar trap to call.
! external        ,optional       etrap = array element trap to call.
! logical                      doreturn = true if the process update routine
!                                           should return immediately.
!
!-------------------------------------------------------------------------------
!                                TRAPS
!
! The traps called by pathchoose_update must look like parameter cache traps.
! The keyword passed to the trap will be the keyword of the pushbutton
! (i.e. SELECT_PATHNAME or SELECT_PATHNAMES in the HOW TO USE example).
!
!   | subroutine trap (keyword)                ! scalar trap.
!   | implicit none
!   | character(len=*),intent(in) :: keyword   ! scalar parameter keyword.
!   | .................
!   | return
!   | end subroutine
!
!
!   | subroutine etrap (keyword,indx,action)   ! array element trap.
!   | implicit none
!   | character(len=*),intent(in) :: keyword   ! array parameter keyword.
!   | integer         ,intent(in) :: indx      ! index where change occurred.
!   | integer         ,intent(in) :: action    ! one of the named constants
!   |                                          !      PC_INSERT
!   |                                          !   or PC_REMOVE
!   |                                          !   or PC_MODIFY.
!   |  ! INDX is a Fortran-style index (=1 for the first array element).
!   |  ! INDX refers to the array element inserted or removed or modified.
!   | .................
!   | return
!   | end subroutine
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                     HOW TO USE THIS PRIMITIVE
!
! In the gui_def of the process module (if scalar parameter):
!
!      Select PATHNAME [PATHNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! -- or -- (if array parameter):
!
!      Select PATHNAME [SELECT_PATHNAMES]
!      PATHNAMES
!      `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!      `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!      `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!      `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! In the data structure of the process module:
!
!   type(pathchoose_struct),pointer :: pathname_obj
!
!
! In the process module create routine:
!
!   call pathchoose_create (obj%pathname_obj,'PATHNAME',extension)
!
!
! In the process module update routine
!  (before any other parameter cache calls):
!
!   if (pathchoose_update(obj%pathname_obj,obj%pathname)) return
!   -- or --
!   if (pathchoose_update(obj%pathname_obj,obj%pathnames,obj%npathnames)) return
!
!
! In the process module delete routine:
!
!   call pathchoose_delete (pathname_obj)
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  6, 2008-12-11  Bill Menger Added nullify.
!  5. 2004-08-23  SMCook     Incorporated mfilebox (multi-file selection).
!  4. 2002-09-09  Vunderink  Vary the default filter path based on the file
!                              extention. 
!  3. 2002-05-10  Vunderink  Added optional trap and appending selection to
!                              array parameter
!  2. 2001-06-26  Vunderink  Fixed bug in pathchoose_update.
!  1. 2001-06-11  Stoeckley  Initial version.
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



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module pathchoose_module
      use getsys_module
      use exptilde_module
      use filebox_module
      use mfilebox_module
      use named_constants_module
      use pc_module
      use string_module
      implicit none
      public

      character(len=100),public,save :: PATHCHOOSE_IDENT = &
'$Id: pathchoose.f90,v 1.5 2004/08/23 13:15:31 SMCook prod sps $'

      interface pathchoose_update
        module procedure pathchoose_update_scalar
        module procedure pathchoose_update_array
      end interface

      type,public :: pathchoose_struct
         private
         type(filebox_struct),pointer   :: dialog
         type(mfilebox_struct),pointer  :: mdialog
         character(len=50)              :: keyword
         character(len=20)              :: extension
         character(len=FILENAME_LENGTH) :: path
      end type pathchoose_struct

      contains


!!--------------------------- create --------------------------------------!!
!!--------------------------- create --------------------------------------!!
!!--------------------------- create --------------------------------------!!


      subroutine pathchoose_create (obj,keyword,extension,path)
      implicit none
      type(pathchoose_struct),pointer       :: obj            ! arguments
      character(len=*),intent(in)           :: keyword        ! arguments
      character(len=*),intent(in)           :: extension      ! arguments
      character(len=*),intent(in),optional  :: path           ! arguments

      integer                               :: npath          ! local

      nullify(obj)
      allocate (obj)
      nullify (obj%dialog)
      nullify (obj%mdialog)
      obj%keyword = 'SELECT_'//string_2_upper(keyword)
      if (extension(1:1) == '.') then
           obj%extension = '*'//extension
      else
           obj%extension = '*.'//extension
      end if

      if (present(path)) then
        obj%path = path
      else
        obj%path = ' '
        select case (trim(obj%extension))
          case ('*.trc','*.trc*','*.cmpr','*.segy','*.sgy')
            call pc_get_jdata ('PATHNAME_TRCIO', obj%path)
          case ('*.lst')
            call pc_get_jdata ('FRONTEND_PATH' , obj%path)
          case default
            call pc_get_jdata ('PATHNAME_DIR'  , obj%path)
        end select
        if (len_trim(obj%path) == 0) call getsys_current_dir (obj%path)
      endif
      npath = len_trim(obj%path)
      if (npath > 0 .and. obj%path(1:1) == '~') call exptilde (obj%path)
      npath = len_trim(obj%path)
      if (npath > 0 .and. obj%path(npath:npath) /= '/')   &
                                                obj%path(npath+1:npath+1) = '/'

      return
      end subroutine pathchoose_create


!!--------------------------- delete --------------------------------------!!
!!--------------------------- delete --------------------------------------!!
!!--------------------------- delete --------------------------------------!!


      subroutine pathchoose_delete (obj)
      implicit none
      type(pathchoose_struct),pointer       :: obj            ! arguments

      call filebox_delete (obj%dialog)
      call mfilebox_delete (obj%mdialog)
      deallocate (obj)

      return
      end subroutine pathchoose_delete


!!--------------------------- update --------------------------------------!!
!!--------------------------- update --------------------------------------!!
!!--------------------------- update --------------------------------------!!


      function pathchoose_update_scalar (obj,pathname,trap) result (doreturn)
      implicit none
      type(pathchoose_struct),intent(inout) :: obj            ! arguments
      character(len=*)       ,intent(inout) :: pathname       ! arguments
      external                                 trap           ! argument
      optional                                 trap           ! argument
      logical                               :: doreturn       ! result

      if (pc_pressed(obj%keyword)) then
             call filebox_put_gui (obj%keyword)
             if (.not.associated(obj%dialog)) then
                  call filebox_create (obj%dialog,trim(obj%path)//  &
                                       obj%extension)
             else
                  call filebox_restore (obj%dialog)
             end if
             doreturn = .true.
      else if (filebox_window(obj%keyword)) then
             if (pc_pressed('OK')) then
                  call filebox_selection (obj%dialog,pathname)
                  doreturn = .false.
                  if (present(trap)) then
                    call trap (obj%keyword)
                  end if
             else
                  call filebox_update    (obj%dialog)
                  doreturn = .true.
             end if
      else
        doreturn = .false.
      end if

      return
      end function pathchoose_update_scalar


      function pathchoose_update_array (obj,pathnames,npathnames,etrap)  &
                                                               result (doreturn)
      implicit none
      type(pathchoose_struct),intent(inout) :: obj            ! arguments
      character(len=*)       ,pointer       :: pathnames(:)   ! arguments
      integer                ,intent(inout) :: npathnames     ! arguments
      external                                 etrap          ! argument
      optional                                 etrap          ! argument
      logical                               :: doreturn       ! result

      character(len=FILENAME_LENGTH),pointer :: selections(:)    ! local
      integer                                :: i, nselections   ! local


      if (pc_pressed(obj%keyword)) then
             call mfilebox_put_gui (obj%keyword)
             if (.not.associated(obj%mdialog)) then
                  call mfilebox_create (obj%mdialog,trim(obj%path)//  &
                                       obj%extension)
             else
                  call mfilebox_restore (obj%mdialog)
             end if
             doreturn = .true.
      else if (mfilebox_window(obj%keyword)) then
             if (pc_pressed('OK')) then
                  nullify(selections)
                  call mfilebox_selections (obj%mdialog,selections,nselections)
                  do i=1,nselections
                    call pathchoose_append_array_element( &
                             pathnames,npathnames,selections(i))
                  end do
                  doreturn = .false.
                  if (present(etrap)) then
                    call etrap (obj%keyword,npathnames,PC_INSERT)
                  end if
             else
                  call mfilebox_update (obj%mdialog)
                  doreturn = .true.
             end if
      else
        doreturn = .false.
      end if

      return
      end function pathchoose_update_array


!!------------------------ append_array_element ----------------------------!!
!!------------------------ append_array_element ----------------------------!!
!!------------------------ append_array_element ----------------------------!!


      subroutine pathchoose_append_array_element (array,narray,value)
      implicit none
      character(len=*),pointer                 :: array(:)             !argument
      integer         ,intent(inout)           :: narray               !argument
      character(len=*),intent(in)              :: value                !argument

      character(len=PC_LENGTH),allocatable     :: temp_array(:)        !local

      if (.not. associated(array)) then
        narray = 1
        allocate(array(narray))
        array(1) = value
      else
        if (narray .gt. 0) then
          allocate(temp_array(narray))
          temp_array(1:narray) = array(1:narray)
          deallocate(array)
          allocate(array(narray+1))
          array(1:narray) = temp_array(1:narray)
          array(narray+1:narray+1) = value
          narray = narray + 1
          deallocate(temp_array)
        else
          deallocate(array)
          narray = 1
          allocate(array(narray))
          array(1) = value
        endif
      endif

      return
      end subroutine pathchoose_append_array_element


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module pathchoose_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

