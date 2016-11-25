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
! Name       : filebox 
! Category   : io
! Written    : 1999-10-01   by: Donna K. Vunderink
! Revised    : 2008-12-11   by: Bill Menger
! Maturity   : beta
! Purpose    : File Selection Box Popup.
! Portability: No known limitations, but see note below.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This is routine used to display a file selection GUI  for choosing a pathname.
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
!                          CALLING SEQUENCE
!
!                                           b     i
!              call filebox_create        (obj, filter)
!              call filebox_delete        (obj)
!              call filebox_update        (obj)
!              call filebox_restore       (obj)
!              call filebox_change_filter (obj, filter)
!
!                                           b       o
!              call filebox_selection     (obj, selection)
!
!                                             i
!              call filebox_put_gui       (keyword)
!
!                   o                         i
!              from_popup = filebox_window(keyword)
!
!   type(filebox_struct)  obj        =  the filebox structure
!   character(len=*)      filter     =  the file filter
!   character(len=*)      selection  =  the selected file
!   character(len=*)      keyword    =  keyword of button activating the filebox
!   logical               from_popup =  true if keywords are from the filebox
!
! FILEBOX_CREATE:
!    (1) allocates obj (PASSED AS A POINTER).
!    (2) initializes the structure.
! 
! FILEBOX_DELETE:
!    (1) deallocates obj (PASSED AS A POINTER).
!
! FILEBOX_UPDATE:
!    (1) get user input from the parameter cache.
!    (2) update the structure.
!
! FILEBOX_RESTORE:
!    (1) put current structure values to gui
!
! FILEBOX_CHANGE_FILTER:
!    (1) change the filter element within the structure.
!
! FILEBOX_SELECTION:
!    (1) retrieve the user's file selection from the structure.
!
! FILEBOX_PUT_GUI:
!    (1) display the filebox gui.
!
! FILEBOX_WINDOW:
!    (1) returns true if the parameter cache hold keywords from the filebox
!        popup
!
!-------------------------------------------------------------------------------
!</calling_doc>


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
! 17. 2008-12-11  Bill Menger  Nullified pointers.
! 16. 2004-01-29  SMCook       Call new filebox_readdir_lstat instead of old
!                               filebox_readdir.  The 'lstat' version uses
!                               'lstat' instead of 'stat' in the underlying C
!                               code.  It appears to fix at least some TRIN
!                               filebox freeze-ups, and seems fast also.
! 15. 2003-11-18  Stoeckley    Change '' to ' ' for Portland Group compiler;
!                               replace INDEX intrinsic functions with
!                               STRING_INDEX to make the Portland Group
!                               compiler happy.
! 14. 2002-09-23  Vunderink    Set array max/min sizes to insure no extra blank
!                                row and clear old selection in filebox_restore.
! 13. 2002-01-08  Vunderink    Make sure object pointer is current in 
!                                filebox_restore and fix Intel compiler 
!                                warnings.
! 12. 2001-09-28  Vunderink    Maded change required by Intel compiler.
! 11. 2001-06-11  Vunderink    Moved from cfe into testlib.
! 10. 2001-01-30  Vunderink    Added change_filter routine
!  9. 2000-10-11  Vunderink    Added calls to exptilde module
!  8. 2000-09-14  Vunderink    Make sure htemp array is large enough for NULL
!                                terminator and make sure all variables are
!                                initialized.
!  7. 2000-09-04  Vunderink    Added call to closedir to eliminate a memory leak
!  6. 2000-08-15  Vunderink    Removed (cfe) from category
!  5. 2000-05-23  Vunderink    Added parameter cache calls to set minsize and
!                                maxsize of arrays.
!  4. 2000-02-27  Vunderink    Added filebox_restore
!  3. 2000-02-23  Vunderink    Changed to use ItemSelected action
!  2. 2000-02-08  Vunderink    Fixed bug in filebox_load_files when filter
!                                directory is invalid.
!  1. 1999-10-01  Vunderink    Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! The Portland Group compiler required modifications in the code as follows:
!
!         aaaa(i:i) = ''    ! is a no-op with the Portland Group compiler.
!         aaaa(i:i) = ' '   ! does work with the Portland Group compiler.
!
! The Portland Group compiler required modifications in the code as follows:
!
!         Replaced reverse INDEX intrinsic function calls with STRING_INDEX.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module filebox_module

      use pc_module
      use getsys_module
      use exptilde_module
      use string_module

      implicit none

      private
      public :: filebox_create
      public :: filebox_update
      public :: filebox_restore
      public :: filebox_change_filter
      public :: filebox_delete
      public :: filebox_selection
      public :: filebox_append_array_element
      public :: filebox_put_gui
      public :: filebox_window

      character(len=100),public,save :: filebox_ident = &
       '$Id: filebox.f90,v 1.16 2004/01/29 12:28:50 SMCook prod sps $'

!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,public :: filebox_struct              
        private
        character(len=PC_LENGTH)                :: root
        character(len=PC_LENGTH)                :: wildcard
        character(len=PC_LENGTH)                :: filter
        integer                                 :: ndirs
        character(len=PC_LENGTH),pointer        :: dirs(:)
        integer                                 :: nfiles
        character(len=PC_LENGTH),pointer        :: files(:)
        character(len=PC_LENGTH)                :: selection
      end type filebox_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      type(filebox_struct),pointer,save :: object             ! needed for traps

      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine filebox_create (obj,default_filter)
      implicit none
      type(filebox_struct),pointer :: obj                     ! arguments
      character(len=*),intent(in)  :: default_filter          ! arguments

      nullify(obj)
      nullify(object)
      allocate (obj)

      object => obj                                           ! needed for traps

      obj%root      = ' '
      obj%wildcard  = ' '
      obj%ndirs     = 0
      obj%nfiles    = 0
      obj%filter    = trim(default_filter)
      obj%selection = ' '

      nullify(obj%dirs)
      nullify(obj%files)

      call filebox_load_files 

      call filebox_update (obj)

      return
      end subroutine filebox_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine filebox_delete (obj)
      implicit none
      type(filebox_struct),pointer :: obj       ! arguments

      if (.not. associated(obj)) return

      if (associated(obj%dirs))  deallocate(obj%dirs)
      if (associated(obj%files)) deallocate(obj%files)

      deallocate(obj)

      return
      end subroutine filebox_delete


!!-------------------------------- update ---------------------------------!!
!!-------------------------------- update ---------------------------------!!
!!-------------------------------- update ---------------------------------!!


      subroutine filebox_update (obj)
      implicit none
      type(filebox_struct),pointer :: obj             ! arguments
      integer                      :: indx            ! local
      integer,pointer              :: itmp_array(:)   ! local
      integer                      :: j               ! local
      character(len=3*PC_LENGTH+2) :: ctmp            ! local

      if (.not. associated(obj)) return

      object => obj                                          ! needed for traps

      call pc_get   ('FILEBOX_FILTER'     , obj%filter   , filebox_filter_trap)
      call pc_get   ('FILEBOX_SELECTION'  , obj%selection)
      call exptilde (obj%selection)

      if (pc_gui_action_present('FILEBOX_DIRECTORIES','ItemSelected')) then
        nullify(itmp_array)
        allocate(itmp_array(1))
        indx = 0
        call pc_alloc_gui ('FILEBOX_DIRECTORIES','ItemSelected',itmp_array,indx)
        if (indx .eq. 1) then
          indx = itmp_array(1)
          if (associated(itmp_array)) deallocate(itmp_array)
        else if (indx .gt. 1) then
          call pc_error ('Only one element may be selected')
          if (associated(itmp_array)) deallocate(itmp_array)
          return
        else
          if (associated(itmp_array)) deallocate(itmp_array)
          return
        endif
        if (trim(obj%dirs(indx)) .eq. '.') then
          return
        else if (trim(obj%dirs(indx)) .eq. '..') then
!!!       j = index        (trim(obj%root),'/',.true.)
          j = string_index (trim(obj%root),'/',.true.)
          if (j .gt. 0) then
            obj%filter = trim(obj%root(1:j)) // trim(obj%wildcard)
          else
            return
          endif
        else
          ctmp       = trim(obj%root) // '/' // trim(obj%dirs(indx)) //  '/' //&
                       trim(obj%wildcard)
          obj%filter = trim(ctmp(1:PC_LENGTH))
        endif
        call filebox_load_files
      endif

      if (pc_gui_action_present('FILEBOX_FILES','ItemSelected')) then
        nullify(itmp_array)
        indx = 0
        allocate(itmp_array(1))
        call pc_alloc_gui ('FILEBOX_FILES','ItemSelected',itmp_array,indx)
        if (indx .eq. 1) then
          indx = itmp_array(1)
          if (associated(itmp_array)) deallocate(itmp_array)
        else if (indx .gt. 1) then
          call pc_error ('Only one element may be selected')
          if (associated(itmp_array)) deallocate(itmp_array)
          return
        else
          if (associated(itmp_array)) deallocate(itmp_array)
          return
        endif
        ctmp          = trim(obj%root) // '/' // trim(obj%files(indx))
        obj%selection = trim(ctmp(1:PC_LENGTH))
      endif

      if (pc_gui_action_present('FILEBOX_FILTERBUTTON','ButtonPress'))    &
        call filebox_load_files

      call pc_put_minsize_array ('FILEBOX_DIRECTORIES' ,obj%ndirs )
      call pc_put_maxsize_array ('FILEBOX_DIRECTORIES' ,obj%ndirs )
      call pc_put_minsize_array ('FILEBOX_FILES'       ,obj%nfiles)
      call pc_put_maxsize_array ('FILEBOX_FILES'       ,obj%nfiles)

      call pc_put ('FILEBOX_FILTER'     , obj%filter               )
      call pc_put ('FILEBOX_DIRECTORIES', obj%dirs     , obj%ndirs )
      call pc_put ('FILEBOX_FILES'      , obj%files    , obj%nfiles)
      call pc_put ('FILEBOX_SELECTION'  , obj%selection            )

      return
      end subroutine filebox_update


!!-------------------------------- restore --------------------------------!!
!!-------------------------------- restore --------------------------------!!
!!-------------------------------- restore --------------------------------!!


      subroutine filebox_restore (obj)
      implicit none
      type(filebox_struct),pointer :: obj             ! arguments

      object => obj                                          ! needed for traps

      call filebox_load_files 
      obj%selection = ' '

      call pc_put_minsize_array ('FILEBOX_DIRECTORIES' ,obj%ndirs )
      call pc_put_maxsize_array ('FILEBOX_DIRECTORIES' ,obj%ndirs )
      call pc_put_minsize_array ('FILEBOX_FILES'       ,obj%nfiles)
      call pc_put_maxsize_array ('FILEBOX_FILES'       ,obj%nfiles)

      call pc_put ('FILEBOX_FILTER'     , obj%filter            )
      call pc_put ('FILEBOX_DIRECTORIES', obj%dirs  , obj%ndirs )
      call pc_put ('FILEBOX_FILES      ', obj%files , obj%nfiles)
      call pc_put ('FILEBOX_SELECTION'  , obj%selection         )

      return
      end subroutine filebox_restore


!!-------------------------- change_filter --------------------------------!!
!!-------------------------- change_filter --------------------------------!!
!!-------------------------- change_filter --------------------------------!!


      subroutine filebox_change_filter (obj,default_filter)
      implicit none
      type(filebox_struct),pointer :: obj                     ! arguments
      character(len=*),intent(in)  :: default_filter          ! arguments

      obj%filter = trim(default_filter)

      return
      end subroutine filebox_change_filter


!!----------------------------- filter_trap -------------------------------!!
!!----------------------------- filter_trap -------------------------------!!
!!----------------------------- filter_trap -------------------------------!!


      subroutine filebox_filter_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      call filebox_load_files 

      return
      end subroutine filebox_filter_trap


!!------------------------------ selection --------------------------------!!
!!------------------------------ selection --------------------------------!!
!!------------------------------ selection --------------------------------!!


      subroutine filebox_selection (obj,selection)
      implicit none
      type(filebox_struct),pointer :: obj                           ! arguments
      character(len=*)             :: selection                     ! arguments

      if (.not. associated(obj)) return

      selection = trim(obj%selection)

      return
      end subroutine filebox_selection


!!----------------------------- load_files --------------------------------!!
!!----------------------------- load_files --------------------------------!!
!!----------------------------- load_files --------------------------------!!


      subroutine filebox_load_files
      implicit none

      character(len=PC_LENGTH)    :: ctemp
      integer,allocatable         :: htemp(:)
      integer                     :: nhtemp
      integer                     :: i
      integer                     :: i1
      integer                     :: i2
      integer                     :: j1
      integer                     :: j2
      integer                     :: cpi
      integer                     :: itype
      integer,external            :: filebox_opendir
      integer,external            :: filebox_filter
      integer,external            :: filebox_readdir_lstat
      integer,external            :: filebox_closedir
      integer,external            :: filebox_sort

      cpi    = string_chars_per_integer()
      nhtemp = (PC_LENGTH+1)/cpi + 1
      allocate(htemp(nhtemp))
      htemp  = 0

      call exptilde (object%filter)
!!!   j1 = index        (trim(object%filter),'/',.true.)
      j1 = string_index (trim(object%filter),'/',.true.)
      j2 = len_trim(object%filter)

      if (j1 .gt. 1) then
        object%root = object%filter(1:j1-1)
        if (j1 .lt. j2) object%wildcard = object%filter(j1+1:j2)
      else
        call getsys_current_dir(object%root)
        call pc_get_jdata ('FRONTEND_PATH',object%root)
        j1 = len_trim(object%root)
  !!!   if (j1 .gt. 0) object%root(j1:j1) = ''   ! bad Portland Group compiler.
        if (j1 .gt. 0) object%root(j1:j1) = ' '  ! for Portland Group compiler.
        object%wildcard = '*'
      endif

      call string_cc2hh(trim(object%root),htemp)
      i = filebox_opendir(htemp)
      if (i .ne. 0) then
         deallocate(htemp)
         call pc_error ('Invalid directory',object%root)
         return                         !Error opening directory
      endif

      call string_cc2hh(trim(object%filter),htemp)
      i = filebox_filter(htemp)

      if (associated(object%files)) deallocate(object%files)
      object%nfiles = 0
      if (associated(object%dirs))  deallocate(object%dirs)
      object%ndirs  = 0

      do
        ! SMCook -- using lstat instead of stat fixed freeze-ups
        i = filebox_readdir_lstat(htemp,itype)
        if (htemp(1) .eq. 0) exit
        call string_hh2cc(htemp,ctemp)
        if (itype .eq. 0) then                  !normal file
          if (object%nfiles .eq. 0) then
            allocate(object%files(1))
            object%files(1) = trim(ctemp)
            object%nfiles = 1
          else
            call filebox_append_array_element (object%files,object%nfiles,  &
                                               trim(ctemp))
          endif
        else if (itype .eq. 1) then             !directory
          if (object%ndirs .eq. 0) then
            allocate(object%dirs(1))
            object%dirs(1) = trim(ctemp)
            object%ndirs = 1
          else
            call filebox_append_array_element (object%dirs,object%ndirs,  &
                                               trim(ctemp))
          endif
        endif
      enddo
      deallocate(htemp)
      i = filebox_closedir()

      i1 = nhtemp*cpi
      i2 = object%ndirs
      allocate(htemp(nhtemp*i2))
      htemp  = 0
      do i=1,i2
        j1 = 1 + (i-1)*nhtemp
        j2 = j1 + nhtemp - 1
        call string_cc2hh (object%dirs(i),htemp(j1:j2))
      enddo
      i = filebox_sort (htemp, i1, i2) 
      do i=1,i2
        j1 = 1 + (i-1)*nhtemp
        j2 = j1 + nhtemp - 1
        call string_hh2cc (htemp(j1:j2),object%dirs(i))
      enddo
      deallocate(htemp)

      i1 = nhtemp*cpi
      i2 = object%nfiles
      allocate(htemp(nhtemp*i2))
      htemp  = 0
      do i=1,i2
        j1 = 1 + (i-1)*nhtemp
        j2 = j1 + nhtemp - 1
        call string_cc2hh (object%files(i),htemp(j1:j2))
      enddo
      i = filebox_sort (htemp, i1, i2) 
      do i=1,i2
        j1 = 1 + (i-1)*nhtemp
        j2 = j1 + nhtemp - 1
        call string_hh2cc (htemp(j1:j2),object%files(i))
      enddo
      deallocate(htemp)

      return
      end subroutine filebox_load_files


!!------------------------ append_array_element ----------------------------!!
!!------------------------ append_array_element ----------------------------!!
!!------------------------ append_array_element ----------------------------!!


      subroutine filebox_append_array_element (array,narray,value)
      implicit none
      character(len=*),pointer                 :: array(:)             !argument
      integer         ,intent(inout)           :: narray               !argument
      character(len=*),intent(in)              :: value                !argument

      character(len=PC_LENGTH),allocatable     :: temp_array(:)        !local

      if (.not. associated(array)) return

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
        allocate(array(1))
        array(1) = value
        narray = 1
      endif

      return
      end subroutine filebox_append_array_element


!!------------------------------- put_gui ----------------------------------!!
!!------------------------------- put_gui ----------------------------------!!
!!------------------------------- put_gui ----------------------------------!!


      subroutine filebox_put_gui (keyword)
      implicit none
      character(len=*),intent(in)     ::  keyword                      !argument

      character(len=40)               :: filebox_keys(7)               !local
      integer                         :: nfilebox_keys                 !local

      nfilebox_keys = 7
      filebox_keys(1) = 'FILEBOX_FILTER'
      filebox_keys(2) = 'FILEBOX_DIRECTORIES'
      filebox_keys(3) = 'FILEBOX_FILES'
      filebox_keys(4) = 'FILEBOX_SELECTION'
      filebox_keys(5) = 'FILEBOX_FILTERBUTTON'
      filebox_keys(6) = 'OK'
      filebox_keys(7) = 'CANCEL'

      call pc_put_gui (keyword,'NewWindow','filebox.xml')
      call pc_put_gui (keyword,'WindowType','FILEBOX')
      call pc_put_gui (keyword,'Windowkeys',filebox_keys,nfilebox_keys)

      end subroutine filebox_put_gui




!!------------------------------- window -----------------------------------!!
!!------------------------------- window -----------------------------------!!
!!------------------------------- window -----------------------------------!!


      function filebox_window (keyword) result (from_filebox_popup)
      implicit none
      character(len=*),intent(in)     ::  keyword                      !argument

      logical                         :: from_filebox_popup            !result

      from_filebox_popup = pc_gui_action_present(keyword,'WINDOWTYPE')

      end function filebox_window


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module filebox_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

