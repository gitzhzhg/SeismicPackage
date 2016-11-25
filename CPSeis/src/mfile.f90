!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- mfile.f90 --------------------------------!!
!!------------------------------- mfile.f90 --------------------------------!!
!!------------------------------- mfile.f90 --------------------------------!!

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
! Name       : MFILE 
! Category   : io
! Written    : 2002-05-21   by: Ed Schmauch
! Revised    : 2007-03-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Specify multiple files.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! Provides a common gui and backend routines for processes requiring the
! specification of multiple files.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
! None.
!
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
! Not used.
!
!-------------------------------------------------------------------------------
!</global_doc>

 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
! Not used.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


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
!                          CALLING SEQUENCES
!
!
!                       b
! call mfile_create (obj)
!
!                     b
! call mfile_delete (obj)
!
!                     b
! call mfile_update (obj)
!
!                       b    i
! call mfile_set_type (obj, type)
!
!                                b         i
! call mfile_set_pathnames_ext (obj, pathnames_ext)
!
!                                i       o          o                 o
! status = mfile_get_filenames (obj, filenames, num_filenames, from_ascii_file)
!
! type(mfile_struct)                          , pointer :: obj
! type = MFILE_READ_TRC_FILE, MFILE_READ_ANY_FILE, or MFILE_WRITE_ANY_FILE.
! integer          , intent(in )                        :: type
! character(len=*) , intent(in )                        :: pathnames_ext
! logical                                               :: status
! character(len=FILENAME_LENGTH), dimension(:), pointer :: filenames
! integer          , intent(out)                        :: num_filenames
! logical, optional, intent(out)                        :: from_ascii_file
!
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
!                             REVISION HISTORY              
!
!     Date        Author        Description
!     ----        ------        -----------
! 15. 2007-03-29  Stoeckley     Fix minor bug whereby the pathname displayed in
!                                the scalar field might be the last deleted name
!                                in the array field if the array field is empty.
!                                Change default extension to * for latest path.
! 14. 2007-03-27  Stoeckley     Change GUI to allow typing in a single pathname
!                                in a scalar field, which then is copied to the
!                                pathnames array.  This change allows SeisSpace
!                                multi-job builder to work with a pathname.  The
!                                SeisSpace multi-job builder does not work with
!                                arrays.
! 13. 2006-11-14  D. Glover     Added NULLIFY statements for Intel compiler.
! 12. 2006-06-01  Stoeckley     Add pc_register_array_names for SeisSpace.
! 11. 2006-01-10  B. Menger     Removed Unused Variables.
! 10. 2004-08-23  SMCook        Incorporated mfilebox (multi-file selection).
!  9. 2004-06-15  SMCook        Replaced CHECK_FILE_STATUS approach with new
!                               TEST_BEYOND_SYMLINKS approach.  MFILE does not
!                               pursue STATUS of files that symlinks point to
!                               unless the user specifically requests it by
!                               pushing this button.  Purpose is to keep NFS
!                               and/or disk problems from affecting the user
!                               interface so much.  "Typos" are still caught.
!  8. 2004-01-30  SMCook        Added CHECK_FILE_STATUS button that allows user
!                               to bypass file status checking, which can lock
!                               up the GUI when there are NFS/disk problems.
!  7. 2003-01-23  Ed Schmauch   Changed finquire_output to finquire_file in
!                               mfile_file_status.  Moved eliminating blank
!                               pathnames to end session trap.  Added CE and WB
!                               status fields.  Improved WE check to also check
!                               for directory access.  mfile_update calls
!                               pc_error in backend so processing will not
!                               start if some file(s) are bad.  This is also
!                               necessary because otherwise trot would replace
!                               a write protected file if it had write
!                               permission on the directory since trot deletes
!                               then creates a file instead of overwriting it.
!  6. 2002-09-09  Vunderink     Modifed to use jdata parameter PATHNAME_TRCIO.
!  5. 2002-08-28  K. Goodger    Move test for duplicate file names to end
!                               session trap.
!  4. 2002-08-26  K. Goodger    Clear PATHNAMES when PATH_LIST selected.
!                               Clear PATH_LIST when PATHNAMES selected.
!  3. 2002-07-01  Ed Schmauch   Added subroutine mfile_set_pathnames_ext and
!                               optional argument from_ascii_file to
!                               mfile_get_filenames.
!  2. 2002-05-28  Ed Schmauch   Made downward compatible.
!  1. 2002-05-21  Ed Schmauch   Initial version.
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
! Allows users of a process to specify multiple files for input or output.
! The programmer must specify if the files are for input or output.
! The files may be either entered into an array or specified with a single
! ASCII file which contains the names of all the files.  The array may be
! populated manually or through the use of pathchoose.  Blank lines and
! duplicates are removed from the array gui as the user enters (misenters)
! them.  File status is checked with finquire, but bad files are only a
! warning not an error.  This philosophy is adapted because the user may
! be building a job for later use and wishes to specify input files that
! have not yet been created.  Blanks lines and duplicates in the ASCII file
! are a warning, not an error in the gui.  When the ASCII file is accessed at
! processing time, blank lines and duplicates are an error.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!
! Test code is included for the private routines of the module.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!          OPT_FILE=`CCCCCCCC                    CHECKFILE=`CC
!
!          Test Beyond Symbolic Links[TEST_BEYOND_SYMLINKS]`P
!
!  Select LATEST_PATHNAME[LATEST_PATHNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                         [LATEST_PATHNAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!          STATUS  PATHNAMES
!          `SSSSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!          `SSSSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!          `SSSSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!          `SSSSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!          `SSSSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!          `SSSSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!          `SSSSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!          `SSSSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!          `SSSSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!          `SSSSSSS`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!
!  Select PATH_LIST[PATH_LIST]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                   [PATH_LIST_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!<PARMS TEST_BEYOND_SYMLINKS[/EN]>
!<PARMS SELECT_LATEST_PATHNAME[/EN]>
!<PARMS STATUS[/EN]>
!<PARMS PATHNAMES[/ML=128]>
!<PARMS PATH_LIST[/ML=128/XST]>
!<PARMS STATUS_ARRAYSET[/XST/YST]>
!</gui_def>
!
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!
!
!<HelpSection>
!
!<Help KEYWORD="OPT_FILE">
!<Tip> Source of input files. </Tip>
! Default = PATHNAMES
! Allowed = PATHNAMES, PATH_LIST
! If OPT_FILE = PATHNAMES, file(s) listed in PATHNAMES array.
! If OPT_FILE = PATH_LIST, file(s) listed in file indicated by PATH_LIST.
! When using PATHNAMES, if no file suffix is specified an appropriate file
! suffix will be added.
! When using PATH_LIST, no file suffix is added.  If the user wants file
! suffices, he/she must specify them with each entry in PATH_LIST file.
!</Help>
!
!<Help KEYWORD="TEST_BEYOND_SYMLINKS">
!<Tip> Forces more thorough file STATUS checking. </Tip>
! Default behavior is to not pursue file STATUS checking beyond the symbolic
! links themselves.  Pushing this button causes more thorough file STATUS
! information to be provided.  Refrain from pushing this button during periods
! of NFS or disk problems.
!</Help>
!
!<Help KEYWORD="CHECKFILE">
!<Tip> Check that the file is valid trace file with consistent globals.  </Tip>
! Default = NO
! Allowed = YES, NO
! CFE can check the file for consistency and existence, and in the process will
! load special globals (scalar ones) for subsequent job processes to use.
! These are so that GVS, some Migrations, etc. will have proper globals set
! by the input data.
!</Help>
!
!<Help KEYWORD="SELECT_LATEST_PATHNAME">
!<Tip> Choose LATEST_PATHNAME using a file selection dialog box. </Tip>
! Pathname of trace file to add to PATHNAMES (the array of trace files).
!</Help>
!
!<Help KEYWORD="LATEST_PATHNAME">
!<Tip> Pathname of trace file to add to PATHNAMES. </Tip>
! Default = blank
! Allowed = char
! Pathname of trace file to add to PATHNAMES (the array of trace files).
! If no file suffix is specified an appropriate file suffix will be added.
!</Help>
!
!<Help KEYWORD="LATEST_PATHNAME_INFO">
!<Tip> File status for LATEST_PATHNAME. </Tip>
!</Help>
!
!<Help KEYWORD="STATUS">
!<Tip> File status (if CHECKSTATUS=YES). </Tip>
!  R  -- File found and readable.
! RE  -- File not found or not readable.
!  W  -- File found and writable.
!  C  -- File not found, but creatable.
! CE  -- File not found and not creatable.
! WE  -- File exists and can not be overwritten.  Checks both file and
!        directory permissions.
! WB  -- Blank filename.
! NTF -- File not a trc, cmpr, or segy file (only if CHECKFILE == YES).
! BG  -- File globals disagree with job (only if CHECKFILE == YES).
!</Help>
!
!<Help KEYWORD="PATHNAMES">
!<Tip> Pathname(s) for the file(s) to be read. </Tip>
! Default = blank
! Allowed = char
! Pathname of file(s), including optional node, userid, and dir.
! When using PATHNAMES, if no file suffix is specified an appropriate file
! suffix will be added.
!</Help>
!
!<Help KEYWORD="SELECT_PATH_LIST">
!<Tip> Choose PATH_LIST with a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="PATH_LIST">
!<Tip> ASCII file containing list of file names. </Tip>
! Default = blank
! Allowed = char
! When using PATH_LIST, no file suffix is added.  If the user wants file
! suffices, he/she must specify them with each entry in PATH_LIST file.
!</Help>
!
!<Help KEYWORD="PATH_LIST_INFO">
!<Tip> File status for PATH_LIST. </Tip>
! Pathname of ASCII list file.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


module mfile_module

  use finquire_module
  use pc_module
  use pathchoose_module
  use path_module
  use pathcheck_module
  use mem_module
  use alphasort_module
  use onesort_module
  use string_module
  use cio_module
  use trcio_module
  use cardset_module
  use tempname_module

  implicit none

  private

  public :: mfile_create
  public :: mfile_delete
! public :: mfile_clear   Only I need mfile_clear.
  public :: mfile_update
  public :: mfile_set_type
  public :: mfile_set_pathnames_ext
  public :: mfile_get_filenames

  !MAKE_PUBLIC_FOR_TEST :: mfile_remove_blank_lines
  !MAKE_PUBLIC_FOR_TEST :: mfile_add_line
  !MAKE_PUBLIC_FOR_TEST :: mfile_blank_duplicates
  !MAKE_PUBLIC_FOR_TEST :: mfile_create_new_lines_struct
  !MAKE_PUBLIC_FOR_TEST :: mfile_delete_new_lines_struct
  !MAKE_PUBLIC_FOR_TEST :: mfile_new_lines
  !MAKE_PUBLIC_FOR_TEST :: mfile_new_lines_struct

  character(len=100),public,save :: MFILE_IDENT = &
    '$Id: mfile.f90,v 1.15 2007/03/29 13:51:17 Stoeckley beta sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


  type :: mfile_new_lines_struct
    character(len=FILENAME_LENGTH), dimension(:), pointer :: old_lines
    integer                       , dimension(:), pointer :: old_sort
    integer                                               :: num_old_lines
    integer                       , dimension(:), pointer :: new_indices
  end type mfile_new_lines_struct

  integer, parameter         ::      STATUS_LENGTH   =  8
  integer, parameter         :: LONG_STATUS_LENGTH   = 80
  integer, parameter, public :: MFILE_READ_TRC_FILE  =  0
  integer, parameter, public :: MFILE_READ_ANY_FILE  =  1
  integer, parameter, public :: MFILE_WRITE_ANY_FILE =  2

  type, public :: mfile_struct              
    private

    character(len=9)                                      :: opt_file
    logical                                               :: quick
    character(len=3)                                      :: checkfile
    character(len=3)                                      :: old_checkfile
    logical                                               :: checkfile_changed
    type(pathchoose_struct)                     , pointer :: pathnames_choose
    character(len=STATUS_LENGTH)  , dimension(:), pointer :: status
    integer                                               :: num_status
    character(len=STATUS_LENGTH)  , dimension(:), pointer :: old_status
    character(len=FILENAME_LENGTH), dimension(:), pointer :: pathnames
    integer                                               :: num_pathnames
    type(mfile_new_lines_struct)                , pointer :: new_lines
    type(pathchoose_struct)                     , pointer :: path_list_choose
    character(len=FILENAME_LENGTH)                        :: path_list
    character(len=FILENAME_LENGTH)                        :: old_path_list
    integer                                               :: type
    character(len=FILENAME_LENGTH)                        ::     pathnames_ext
    character(len=FILENAME_LENGTH)                        :: old_pathnames_ext

  end type mfile_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


  type(mfile_struct), pointer, save :: trp ! need this for traps

  integer, parameter                :: num_opt_opt_file = 2
  character(len=9), parameter       :: opt_opt_file (num_opt_opt_file) &
                                         = (/'PATHNAMES','PATH_LIST'/)

  integer, parameter                :: num_opt_checkfile = 2
  character(len=3), parameter       :: opt_checkfile (num_opt_checkfile) &
                                         = (/'YES','NO '/)

  contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


  subroutine mfile_create (obj)
    type(mfile_struct), pointer :: obj

    allocate (obj)

    nullify (obj%pathnames_choose) ! jpa
    nullify(obj%status    )
    nullify(obj%old_status)
    nullify(obj%pathnames )
    nullify (obj%new_lines) ! jpa
    nullify (obj%path_list_choose) ! jpa

    call pathchoose_create(obj%pathnames_choose, 'latest_pathname', '*')
    call pathchoose_create(obj%path_list_choose, 'path_list'      , '*')

    if (mfile_create_new_lines_struct(obj%new_lines) /= 0) then
      nullify(obj)
      return
    endif

    call mfile_clear (obj)   ! initialize the data structure variables.

  end subroutine mfile_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


  subroutine mfile_delete (obj)
    type(mfile_struct), pointer :: obj

    call pathchoose_delete (obj%pathnames_choose)
    call pathchoose_delete (obj%path_list_choose)

    call mfile_delete_new_lines_struct(obj%new_lines)

    if (associated(obj%pathnames)) then
      deallocate(obj%pathnames)
    endif

    if (associated(obj%status)) then
      deallocate(obj%status)
    endif

    if (associated(obj%old_status)) then
      deallocate(obj%old_status)
    endif

    deallocate(obj)

  end subroutine mfile_delete


!!----------------------------- clear -------------------------------------!!
!!----------------------------- clear -------------------------------------!!
!!----------------------------- clear -------------------------------------!!


  subroutine mfile_clear (obj)
    type(mfile_struct), pointer :: obj

    obj%opt_file          = 'PATHNAMES'
    obj%quick             = .true.
    obj%checkfile         = 'NO'
    obj%old_checkfile     = 'UNK'
    obj%checkfile_changed = .false.
    obj%num_pathnames     = 0
    obj%num_status        = 0
    obj%path_list         = PATH_EMPTY
    obj%old_path_list     = PATH_EMPTY
    obj%type              = MFILE_READ_TRC_FILE
    obj%pathnames_ext     = ' '
    obj%old_pathnames_ext = ' '

  end subroutine mfile_clear


!!----------------------------- update ------------------------------------!!
!!----------------------------- update ------------------------------------!!
!!----------------------------- update ------------------------------------!!


  subroutine mfile_update(obj)
    type(mfile_struct), pointer :: obj

    character(len=FILENAME_LENGTH),pointer   :: latest(:)
    integer                                  :: nlatest
    character(len=FILENAME_LENGTH)           :: pathname_trcio
    character(len=FILENAME_LENGTH)           :: latest_pathname
    character(len=FILENAME_LENGTH)           :: latest_pathname_keep
    character(len=FILENAME_LENGTH),pointer   :: old_pathnames(:)
    integer                                  :: num_pathnames_keep

    integer                                  :: status
    integer                                  :: ext_len
    integer                                  :: pnm_len
    integer                                  :: i

    !--- point trp for trap routines.
    trp => obj
    if(.not. associated(trp) ) return

    latest_pathname = PATHCHECK_EMPTY
    if (obj%num_pathnames >= 1) latest_pathname = obj%pathnames(obj%num_pathnames)
    latest_pathname_keep = latest_pathname
    num_pathnames_keep   = obj%num_pathnames

    nullify(latest)
    nlatest = 0
 !  if (pathchoose_update(obj%pathnames_choose, latest, nlatest)) return
    if (pathchoose_update(obj%pathnames_choose, latest_pathname)) return

    if (pathchoose_update(obj%path_list_choose, obj%path_list  )) return

    call pc_register_array_names ('status_arrayset', (/'status   ', &
                                                       'pathnames'/))

    call pc_get   ('opt_file'   , obj%opt_file , mfile_opt_file_trap )
    call pc_get   ('checkfile'  , obj%checkfile, mfile_checkfile_trap)

    call pc_get   ('latest_pathname', latest_pathname)

    if (obj%type == MFILE_WRITE_ANY_FILE) then
      call pathcheck('latest_pathname', latest_pathname, show=PATHCHECK_INFO_OUTPUT)
    else
      call pathcheck('latest_pathname', latest_pathname, show=PATHCHECK_INFO_INPUT )
    endif

    call pc_alloc ('status   ', obj%status   , obj%num_status   )
    call pc_alloc ('pathnames', obj%pathnames, obj%num_pathnames)

    if (latest_pathname /= latest_pathname_keep .and. &
        latest_pathname /= PATHCHECK_EMPTY      .and. &
        obj%num_pathnames == num_pathnames_keep) then
      allocate (old_pathnames(obj%num_pathnames))
      old_pathnames(:) = obj%pathnames(:)
      deallocate (obj%pathnames)
      allocate (obj%pathnames(obj%num_pathnames+1))
      obj%pathnames(:obj%num_pathnames) = old_pathnames(:obj%num_pathnames)
      obj%pathnames(obj%num_pathnames+1) = latest_pathname
      obj%num_pathnames = obj%num_pathnames+1
      deallocate (old_pathnames)
    endif

    call pc_get   ('path_list', obj%path_list)
    call pathcheck('path_list', obj%path_list, show=PATHCHECK_INFO_INPUT )

    ! Call yourself because pathchoose_update could change obj%path_list.
    ! Must be called after mfile_checkfile_trap.
    !
    call mfile_path_list_trap('path_list')

    call mfile_make_downward_compatible(obj)

    if(pc_pressed('TEST_BEYOND_SYMLINKS')) then
      obj%quick = .false.
      status = mfile_update_files_found(obj)
      obj%quick = .true.
      if (status /= 0) then
        call pc_error('MFILE:  out of memory.')
        return
      endif
    endif

    call pc_put_options_field('OPT_FILE' , opt_opt_file , num_opt_opt_file )
    call pc_put_options_field('CHECKFILE', opt_checkfile, num_opt_checkfile)

    select case(obj%type)
      case(MFILE_READ_TRC_FILE )
        call pc_put_visible_flag('CHECKFILE', .true. )
      case(MFILE_READ_ANY_FILE )
        call pc_put_visible_flag('CHECKFILE', .false.)
      case(MFILE_WRITE_ANY_FILE)
        call pc_put_visible_flag('CHECKFILE', .false.)
      case default
        call pc_error('MFILE:  type must be  MFILE_READ_TRC_FILE, ' &
                       // 'MFILE_READ_ANY_FILE, or MFILE_WRITE_ANY_FILE')
    end select

    ! This happens first time through if pathnames are stored in the work file.
    ! The user typing into the last row of pathnames, ^D (^R), and ^I are
    ! handled by pc_alloc.
    ! Note num_status is not set to num_pathnames until after
    ! mfile_add_line is called.
    !
    if (obj%num_pathnames > obj%num_status) then
      ! mem_realloc works like mem_alloc if array is not associated.
      !
      call mem_realloc(obj%status, obj%num_pathnames, status)
      if (status /= 0) then
        call pc_error('MFILE:  out of memory.')
        return
      endif
      do i = obj%num_status + 1, obj%num_pathnames
        obj%status(i) = ' '
      enddo
    endif

    if (nlatest > 0) then
      do i=1, nlatest
        status = mfile_add_line(obj%pathnames, latest(i), &
                             obj%num_pathnames, obj%status, ' ')
        if (status /= 0) then
          call pc_error('MFILE:  out of memory.')
          return
        endif
      enddo
    endif

    obj%num_status = obj%num_pathnames

    pathname_trcio = ' '
    call pc_get_jdata('PATHNAME_TRCIO', pathname_trcio)


    do i = 1, obj%num_pathnames

      ! If pathnames_ext is changing, remove the old pathnames_ext from
      ! pathname so path_fixup_filename will replace it with the
      ! new pathnames_ext.
      !
      if ((obj%old_pathnames_ext /= obj%pathnames_ext) &
    .and. (obj%old_pathnames_ext /= ' '              )) then

        ext_len = len_trim(obj%old_pathnames_ext)
        pnm_len = len_trim(obj%pathnames(i))

        if (obj%pathnames(i)(pnm_len-ext_len:pnm_len) == &
            '.' // obj%old_pathnames_ext) then

          obj%pathnames(i)(pnm_len-ext_len:pnm_len) = ' '

        endif

      endif

      if (obj%pathnames_ext /= ' ') then
        call path_fixup_filename(obj%pathnames(i), pathname_trcio, &
                                 obj%pathnames_ext)
      else
        call path_fixup_filename(obj%pathnames(i), pathname_trcio)
      endif

      if (obj%pathnames(i) == PATH_EMPTY) then
          obj%pathnames(i) = ' '
      endif

    enddo

    obj%old_pathnames_ext = obj%pathnames_ext

    ! If in frontend, update status array even if using PATH_LIST so gui
    ! is always accurate.  If in backend, don't update status array if using
    ! PATH_LIST because trcio_open errors for unused PATHNAMES files are
    ! confusing in report file.
    !
    if ((pc_get_update_state() /= PC_BACKEND ) &
   .or. (obj%opt_file          == 'PATHNAMES')) then
      obj%quick = .true.
      status = mfile_update_files_found(obj)
      if (status /= 0) then
        call pc_error('MFILE:  out of memory.')
        return
      endif
    endif

    call pc_call_end_trap(mfile_end_trap)

    latest_pathname = PATHCHECK_EMPTY
    if (obj%num_pathnames >= 1) latest_pathname = obj%pathnames(obj%num_pathnames)

    call pc_put          ('opt_file'   , obj%opt_file )
    call pc_put          ('checkfile'  , obj%checkfile)
    call pc_put_gui_only ('status'     , obj%status   , obj%num_status   )
    call pc_put          ('pathnames'  , obj%pathnames, obj%num_pathnames)
    call pc_put          ('path_list'  , obj%path_list)
    call pc_put          ('latest_pathname', latest_pathname)

  end subroutine mfile_update


!!----------------------------- public subroutines ------------------------!!
!!----------------------------- public subroutines ------------------------!!
!!----------------------------- public subroutines ------------------------!!


  ! MFILE_READ_TRC_FILE, MFILE_READ_ANY_FILE, or MFILE_WRITE_ANY_FILE.
  !
  subroutine mfile_set_type(obj, type)
    type (mfile_struct), pointer    :: obj
    integer            , intent(in) :: type

    obj%type = type

  end subroutine mfile_set_type

  ! Don't include the period with the extension.
  !
  subroutine mfile_set_pathnames_ext(obj, pathnames_ext)
    type (mfile_struct), pointer    :: obj
    character(len=*)   , intent(in) :: pathnames_ext

    obj%pathnames_ext = pathnames_ext

  end subroutine mfile_set_pathnames_ext

  ! Returns false if list is empty, has blanks or duplicates, or out of memory.
  ! mfile_get_filenames allocates filenames, it is up to calling code to free.
  ! If false is returned, filenames is not allocated and shouldn't be freed.
  ! from_ascii_file is set even if false is returned, unless opt_file contains
  ! an illegal value.
  !
  function mfile_get_filenames(obj, filenames, num_filenames, from_ascii_file) &
    result(status)
    type (mfile_struct)                         , pointer :: obj
    character(len=FILENAME_LENGTH), dimension(:), pointer :: filenames
    integer          , intent(out)                        :: num_filenames
    logical, optional, intent(out)                        :: from_ascii_file
    logical                                               :: status

    character(len=FILENAME_LENGTH)                        :: pathname_dir
    character(len=FILENAME_LENGTH)                        :: pathname_trcio
    character(len=FILENAME_LENGTH)                        :: path_list
    integer                                               :: memstat
    integer                                               :: num_dups
    integer                                               :: i

    pathname_dir   = ' '
    pathname_trcio = ' '
    call pc_get_jdata('PATHNAME_DIR'  , pathname_dir)
    call pc_get_jdata('PATHNAME_TRCIO', pathname_trcio)

    select case(obj%opt_file)
      case ('PATHNAMES')
        if (present(from_ascii_file)) then
          from_ascii_file = .false.
        endif
        if (obj%num_pathnames > 0) then
          nullify(filenames)
          call mem_alloc(filenames, obj%num_pathnames, memstat)
          if (memstat == 0) then
            filenames(1:obj%num_pathnames) = obj%pathnames(1:obj%num_pathnames)
            num_filenames                  = obj%num_pathnames
          else
            call pc_error('MFILE:  out of memory')
            status = .false.
            return
          endif
        else
          status = .false.
          return
        endif
      case ('PATH_LIST')
        if (present(from_ascii_file)) then
          from_ascii_file = .true.
        endif
        path_list = obj%path_list
        call path_fixup_filename(path_list, pathname_dir)
        if (path_list /= PATH_EMPTY) then
          if (mfile_read_path_list(path_list, filenames, num_filenames)) then
            if (num_filenames == 0) then
              call mem_free(filenames)
              status = .false.
              return
            endif
          else
            status = .false.
            return
          endif
        else
          status = .false.
          return
        endif
      case default
        call pc_error('MFILE:  illegal value for opt_file')
        status = .false.
        return
    end select

    ! If opt_file == PATHNAMES, mfile_update will have already done the
    ! path_fixup_filename.  We need if here for opt_file == PATH_LIST.
    ! Doesn't hurt to repeat it for If opt_file == PATHNAMES.
    ! Also, for opt_file == PATHNAMES, mfile_update will have eliminated
    ! blanks.
    !
    do i = 1, num_filenames
      call path_fixup_filename(filenames(i), pathname_trcio)
      if (filenames(i) == PATH_EMPTY) then
        call mem_free(filenames)
        status = .false.
        return
      endif
    enddo

    ! If opt_file == PATHNAMES, duplicates were eliminated in mfile_update.
    !
    if (mfile_blank_duplicates(filenames, num_filenames, num_dups) /= 0) then
      call pc_error('MFILE:  out of memory')
      call mem_free(filenames)
      status = .false.
      return
    endif

    if (num_dups > 0) then
      call pc_error('MFILE: duplicate filenames')
      call mem_free(filenames)
      status = .false.
      return
    endif

    status = .true.

  end function mfile_get_filenames

!!----------------------------- private subroutines -----------------------!!
!!----------------------------- private subroutines -----------------------!!
!!----------------------------- private subroutines -----------------------!!


  ! Returns 0 if ok.
  ! 1 if fails, memory problem.
  !
  function mfile_update_files_found(obj) result (status)
    type (mfile_struct), intent(inout) :: obj
    integer                            :: status

    integer, dimension(:), pointer     :: new_indices
    integer                            :: i
    character(len=STATUS_LENGTH)       :: msg
    character(len=LONG_STATUS_LENGTH)  :: error_msg

    nullify (new_indices) ! jpa

    status = mfile_new_lines(obj%new_lines, obj%pathnames, obj%num_pathnames, &
                               new_indices)

    if (status /= 0) then
      return
    endif

    do i = 1, obj%num_pathnames

      if( (new_indices(i) == -1 ) .or. &
          (obj%checkfile_changed) ) then
        if (.not. mfile_file_status(obj, obj%pathnames(i), &
                                 short=msg, long=error_msg)) then
          if (pc_get_update_state() == PC_BACKEND) then
            call pc_error(trim(obj%pathnames(i)) // " " // trim(error_msg))
          endif
        endif
        obj%status(i) = msg
      else
        obj%status(i) = obj%old_status(new_indices(i))
      endif

    enddo

    ! Reset checkfile_changed, if necessary.
    !
    if (obj%checkfile_changed) then
      obj%checkfile_changed = .false.
    endif

    call mem_alloc(obj%old_status, obj%num_pathnames, status)
    if (status == 0) then
      obj%old_status(1:obj%num_pathnames) = obj%status(1:obj%num_pathnames)
    endif

  end function mfile_update_files_found

  subroutine mfile_opt_file_trap(opt_file)
    character(len=*),intent(in) :: opt_file

    call string_to_upper(trp%opt_file)
    select case(trp%opt_file)
      case('PATHNAMES')
        !call pc_put_sensitive_field_flag('SELECT_LATEST_PATHNAME',.true. )
        call pc_put_sensitive_field_flag('SELECT_LATEST_PATHNAME',.false. )
        call pc_put_sensitive_field_flag('LATEST_PATHNAME'       ,.true. )
        call pc_put_sensitive_field_flag('STATUS_ARRAYSET'       ,.true. )
        call pc_put_sensitive_field_flag('PATHNAMES'             ,.true. )
        call pc_put_sensitive_field_flag('PATH_LIST'             ,.false.)
        call pc_put_sensitive_field_flag('SELECT_PATH_LIST'      ,.false.)
        trp%path_list=PATH_EMPTY
      case('PATH_LIST')
        call pc_put_sensitive_field_flag('SELECT_LATEST_PATHNAME',.false.)
        call pc_put_sensitive_field_flag('LATEST_PATHNAME'       ,.false.)
        call pc_put_sensitive_field_flag('STATUS_ARRAYSET'       ,.false.)
        call pc_put_sensitive_field_flag('PATHNAMES'             ,.false.)
        call pc_put_sensitive_field_flag('PATH_LIST'             ,.true. )
        call pc_put_sensitive_field_flag('SELECT_PATH_LIST'      ,.true. )
        trp%num_pathnames=0
        trp%num_status   =0
      case default
        call pc_error('MFILE: opt_file must be PATHNAMES or PATH_LIST.')
        call pc_jump_field(opt_file)
    end select

  end subroutine mfile_opt_file_trap

  subroutine mfile_checkfile_trap(checkfile)
    character(len=*),intent(in) :: checkfile

    ! Force rechecking of path_list if getting stricter.
    ! mfile_checkfile_trap must be called before mfile_path_list_trap.
    !
    if ((trp%old_checkfile == 'NO') .and. (trp%checkfile == 'YES')) then
      trp%old_path_list = PATH_EMPTY
    endif

    ! Force rechecking of pathnames if changing.
    ! mfile_checkfile_trap must be called before mfile_update_files_found.
    !
    if ((trp%old_checkfile /= 'UNK'        ) &
  .and. (trp%old_checkfile /= trp%checkfile)) then
      trp%checkfile_changed = .true.
    endif

    trp%old_checkfile = trp%checkfile

  end subroutine mfile_checkfile_trap

  subroutine mfile_path_list_trap(path_list)
    character(len=*),intent(in)                           :: path_list

    character(len=FILENAME_LENGTH), dimension(:), pointer :: pathname
    integer                                               :: num_pathnames
    character(len=FILENAME_LENGTH)                        :: pathname_trcio
    integer                                               :: num_blanks
    integer                                               :: num_dups
    character(len=LONG_STATUS_LENGTH)                     :: error_msg
    integer                                               :: i

    nullify (pathname) ! jpa

    if ((trp%opt_file  == 'PATH_LIST'      ) &
  .and. (trp%path_list /= trp%old_path_list)) then

      if (trp%path_list /= PATH_EMPTY) then

        if (mfile_read_path_list(trp%path_list, pathname, num_pathnames)) then

          pathname_trcio = ' '
          call pc_get_jdata('PATHNAME_TRCIO', pathname_trcio)

          num_blanks = 0
          do i = 1, num_pathnames
            call path_fixup_filename(pathname(i), pathname_trcio)
            if (pathname(i) == PATH_EMPTY) then
              pathname(i) = ' '
              num_blanks = num_blanks + 1
            endif
          enddo

          if      (num_blanks == num_pathnames) then
            call pc_warning(trim(trp%path_list) &
                   // " contains no non-blank lines")
          else if (num_blanks >  0            ) then
            call pc_warning(trim(trp%path_list) &
                   // " contains blank lines")
          endif

          if (mfile_blank_duplicates(pathname, num_pathnames, num_dups) /= 0) &
           then
            call pc_error('MFILE:  out of memory.')
            call mem_free(pathname)
            return
          endif

          if (num_dups > 0) then
            call pc_warning(trim(trp%path_list) &
                   // " contains duplicate pathnames")
          endif

          do i = 1, num_pathnames
            if (pathname(i) /= ' ') then
              if (.not. mfile_file_status(trp, pathname(i), long=error_msg)) &
               then
                if (pc_get_update_state() == PC_BACKEND) then
                  call pc_error  (trim(pathname(i)) // " " // trim(error_msg))
                else
                  call pc_warning(trim(pathname(i)) // " " // trim(error_msg))
                endif
              endif
            endif
          enddo

          call mem_free(pathname)

        endif

      endif

      trp%old_path_list = trp%path_list

    endif

  end subroutine mfile_path_list_trap



  subroutine mfile_end_trap

    integer :: num_dups,status

    select case(trp%opt_file)
      case ('PATHNAMES')
        if (trp%num_pathnames == 0) then
          call pc_error('Need to specify file')
        endif
      case ('PATH_LIST')
        if (trp%path_list == PATH_EMPTY) then
          call pc_error('Need to specify file')
        endif
      case default
        call pc_error('MFILE:  illegal value for opt_file')
        return
    end select

    status = mfile_blank_duplicates(trp%pathnames, trp%num_pathnames, num_dups)
    if (status /= 0) then
      call pc_error('MFILE:  out of memory.')
      return
    endif

    if (num_dups > 0) then
      call pc_warning('Removed duplicate from pathnames.')
    endif

    status = mfile_remove_blank_lines(trp%pathnames, &
                                     trp%num_pathnames, trp%status, 'none')
    if (status /= 0) then
      call pc_error('MFILE:  out of memory.')
      return
    endif

    ! In case duplicates/blanks were removed.
    !
    trp%num_status = trp%num_pathnames

  end subroutine mfile_end_trap

  ! Returns false if error.
  ! Allocates pathname, calling code must free.  If num_pathnames is 0,
  ! pathname will be nullified so the calling code can still call mem_free.
  ! If error, pathname will not be allocated.
  !
  function mfile_read_path_list(path_list, pathname, num_pathnames) &
    result(status)
    character(len=FILENAME_LENGTH), intent(in )           :: path_list
    character(len=FILENAME_LENGTH), dimension(:), pointer :: pathname
    integer                       , intent(out)           :: num_pathnames
    logical                                               :: status

    integer                                               :: unit
    integer                                               :: len
    integer                                               :: num_pathnames_alloc
    integer                                               :: iostat

    unit = cio_fopen(path_list, "r")

    if (unit > 0) then

      num_pathnames       = 0
      num_pathnames_alloc = 0
      nullify(pathname)
      do

        num_pathnames = num_pathnames + 1
        if (num_pathnames > num_pathnames_alloc) then
          num_pathnames_alloc = num_pathnames_alloc + 10
          call mem_realloc(pathname, num_pathnames_alloc, iostat)
          if (iostat /= 0) then
            call pc_error('MFILE:  out of memory.')
            iostat = cio_fclose(unit)
            status = .false.
            return
          endif
        endif

        len = cio_fgetline(pathname(num_pathnames), FILENAME_LENGTH, unit)

        if      (len == CIO_EOF  ) then
          num_pathnames = num_pathnames - 1
          exit
        else if (len == CIO_ERROR) then
          call pc_error("MFILE -- Error reading:  " // trim(path_list))
          call mem_free(pathname)
          iostat = cio_fclose(unit)
          status = .false.
          return
        endif

      enddo

      if (cio_fclose(unit) == CIO_ERROR) then
        call pc_error("MFILE -- Error closing:  " // trim(path_list))
        call mem_free(pathname)
        status = .false.
        return
      endif

      status = .true.

    else

      status = .false.

    endif

  end function mfile_read_path_list

  ! Returns true if file ok, false if not.
  !
  function mfile_file_status(obj, pathname, short, long) result (retval)
    type (mfile_struct)              , intent(in )           :: obj
    character(len=   FILENAME_LENGTH), intent(in )           :: pathname
    character(len=     STATUS_LENGTH), intent(out), optional :: short
    character(len=LONG_STATUS_LENGTH), intent(out), optional :: long
    logical                                                  :: retval

    character(len=     STATUS_LENGTH)                        :: local_short
    character(len=LONG_STATUS_LENGTH)                        :: local_long
    character(len=   FILENAME_LENGTH)                        :: userid_junk
    character(len=   FILENAME_LENGTH)                        :: node_junk
    character(len=   FILENAME_LENGTH)                        :: dir
    character(len=   FILENAME_LENGTH)                        :: file_junk
    character(len=   200            )                        :: temp

    select case(obj%type)
      case(MFILE_READ_TRC_FILE)
        select case(finquire_input(pathname, quickly=obj%quick))
          case(FINQUIRE_FOUND)
            local_short = '       R'
            local_long  = 'File found and readable'
            if (obj%checkfile == 'YES') then
              retval = mfile_checkfile(obj, pathname, local_short, local_long)
            else
              retval = .true.
            endif
          case(FINQUIRE_ERROR)
            local_short = '      RE'
            local_long  = 'File not found or not readable'
            retval      = .false.
          case default
            call pc_error('MFILE:  finquire_input error')
            retval      = .false.
        end select
      case(MFILE_READ_ANY_FILE)
        select case(finquire_input(pathname, quickly=obj%quick))
          case(FINQUIRE_FOUND)
            local_short = '       R'
            local_long  = 'File found and readable'
            retval      = .true.
          case(FINQUIRE_ERROR)
            local_short = '      RE'
            local_long  = 'File not found or not readable'
            retval      = .false.
          case default
            call pc_error('MFILE:  finquire_input error')
        end select
      case(MFILE_WRITE_ANY_FILE)
        select case(finquire_file(pathname))
          case(FINQUIRE_FOUND, FINQUIRE_NOT_READABLE)
            ! Make sure you have write access to directory, since trot
            ! deletes/creates instead of overwriting.
            !
            call path_parse(pathname, userid_junk, node_junk, dir, file_junk)
            temp = tempname(' ', dir)
            if (temp == ' ') then
              local_short = '      WE'
              local_long  = 'File exists and can not be overwritten'
              retval      = .false.
            else
              local_short = '       W'
              local_long  = 'File found and writable'
              retval      = .true.
            endif
          case(FINQUIRE_NOT_FOUND)
            local_short = '       C'
            local_long  = 'File not found, but creatable'
            retval      = .true.
          case(FINQUIRE_NOT_CREATEABLE)
            local_short = '      CE'
            local_long  = 'File not found and not creatable'
            retval      = .false.
          case(FINQUIRE_NOT_WRITEABLE, FINQUIRE_NOT_READ_WRITE)
            local_short = '      WE'
            local_long  = 'File exists and can not be overwritten'
            retval      = .false.
          case(FINQUIRE_BLANK)
            local_short = '      WB'
            local_long  = 'Blank filename'
            retval      = .false.
          case default
            call pc_error('MFILE:  finquire_file error')
            retval      = .false.
        end select
      case default
        call pc_error('MFILE:  type must be  MFILE_READ_TRC_FILE, ' &
                       // 'MFILE_READ_ANY_FILE, or MFILE_WRITE_ANY_FILE')
    end select


    if (present(short)) then
      short = local_short
    endif

    if (present(long )) then
      long  = local_long
    endif

  end function mfile_file_status

  ! Returns true if file ok, false if not.
  !
  function mfile_checkfile(obj, pathname, short, long) result (retval)
    type (mfile_struct)              , intent(in )       :: obj
    character(len=   FILENAME_LENGTH), intent(in )       :: pathname
    character(len=     STATUS_LENGTH), intent(out)       :: short
    character(len=LONG_STATUS_LENGTH), intent(out)       :: long
    logical                                              :: retval

    type(trcio_struct), pointer                          :: file
    real                                                 :: tstrt
    real                                                 :: dt
    integer                                              :: ndpt
    integer                                              :: num_global_cards
    integer                                              :: nkeys
    integer                                              :: status
    integer                                              :: i
    character(len=cardset_length), pointer, dimension(:) :: global_cards
    character(len=cardset_length), pointer, dimension(:) :: global_keywords
    integer                                              :: idummy
    real                                                 :: rdummy
    character(len=cardset_length)                        :: cdummy
    character(len=cardset_length)                        :: errmsg
    type(cardset_struct), pointer                        :: jobglobals

    nullify (jobglobals) ! jpa

    file => trcio_open(pathname, 'r')
    if(.not. associated (file)) then
      short  = '     NTF'
      long   = 'File not a trc, cmpr, or segy file'
      retval = .false.
      return
    endif

    retval = .true.

    call pc_get_global('tstrt', tstrt)
    call pc_get_global('dt'   , dt   )
    call pc_get_global('ndpt' , ndpt )

    if ((tstrt /= file%tmin      ) &
   .or. (dt    /= file%dt        ) &
   .or. (ndpt  /= file%num_values)) then
      short  = '      BG'
      long   = 'File globals disagree with job'
      retval = .false.
    endif

    ! Read jobglobals from trace file if present ---
    ! Code copied from trin, tests for tstrt, dt, and ndpt are redundant
    ! with above.  All that is gained is the pc_put_global for special
    ! globals.
    !
    select case (trim(file%ftype))
      case('TRCIO', 'CMPR')
        num_global_cards = trcio_num_global_cards(file)
        if(num_global_cards > 0 ) then
         allocate(global_cards(num_global_cards))
         status = trcio_read_globals(file,num_global_cards,global_cards)
         if(status /= trcio_ok) then
           call pc_error('MFILE:  error reading TRCIO job globals.')
           status = trcio_close(file)
           return
         endif
         call cardset_create(jobglobals)
         call cardset_set_name(jobglobals,'jobglobals')
         call cardset_put_cards(jobglobals,global_cards,num_global_cards)
         deallocate (global_cards)
 
         nkeys = cardset_num_keywords(jobglobals)
         allocate(global_keywords(nkeys))
         !--- we will load job global keyword names from file into array.
         !--- we will only look at scalars (nelem == 1)
         do i = 1, nkeys
           global_keywords(i) = cardset_get_keyword(jobglobals,i)
           if(cardset_nature(jobglobals,global_keywords(i))==CARDSET_SCALAR) &
           then
             !--- compare with this job ---
             select case(trim(global_keywords(i)))
               case('NDPT')
                 call cardset_get_scalar(jobglobals,'NDPT',idummy,errmsg)
                 if(idummy /= file%num_values) then
                   call pc_error('MFILE:  ndpt from ' &
                              // 'cardset_get_scalar disagrees with that ' &
                              // 'from trcio_struct for ' // trim(pathname))
                 endif
               case('DT')
                 call cardset_get_scalar(jobglobals,'DT',rdummy,errmsg)
                 if(rdummy /= file%dt) then
                   call pc_error('MFILE:  dt from ' &
                              // 'cardset_get_scalar disagrees with that ' &
                              // 'from trcio_struct for ' // trim(pathname))
                 endif
               case('TSTRT')
                 call cardset_get_scalar(jobglobals,'TSTRT',rdummy,errmsg)
                 if(rdummy /= file%tmin) then
                   call pc_error('MFILE:  tstrt from ' &
                              // 'cardset_get_scalar disagrees with that ' &
                              // 'from trcio_struct for ' // trim(pathname))
                 endif
               case('NUMTR','NWIH','GATHERED')
               case default
                 !--- add the global ---
                 call cardset_get_scalar(jobglobals,global_keywords(i),&
                   cdummy,errmsg)
                 call pc_put_global(global_keywords(i),cdummy)
             end select
           endif
         end do
         deallocate(global_keywords)
         call cardset_delete(jobglobals)
        endif
      case ('SEGY')
      case default
        call pc_error('MFILE -- bad file type:  ' // trim(pathname))

    end select

    if (trcio_close(file) /= TRCIO_OK) then
      call pc_error('MFILE -- trcio_close error:  ' // trim(pathname))
    endif

  end function mfile_checkfile

  ! Returns 0 if ok.
  ! 1 if fails, memory problem.
  ! Removes line from both key and extra arrays if key is blank unless
  ! extra == extra_exception.
  !
  function mfile_remove_blank_lines(key, num, extra, extra_exception) &
    result (status)
    character(len=*), dimension(:), pointer :: key
    integer         , intent(inout)         :: num
    character(len=*), dimension(:), pointer :: extra
    character(len=*)                        :: extra_exception
    integer                                 :: status

    integer                                 :: i, j

    if ((num > size(key)) .or. (num > size(extra))) then
      call pc_error('MFILE:  bad input parameters.')
      status = 1
      return
    endif

    i = 1
    do
      if (i > num) then
        exit
      endif

      if ((len_trim(key(i)) /= 0) &
     .or. (       extra(i)  == extra_exception)) then
        i = i + 1
      else
        do j = i, num - 1
          key  (j) = key  (j+1)
          extra(j) = extra(j+1)
        enddo

        num = num - 1
      endif
    enddo

    if (size(key) > num) then
      call mem_realloc(key, num, status)
      if (status /= 0) then
        return
      endif
    endif

    if (size(extra) > num) then
      call mem_realloc(extra, num, status)
    else
      status = 0
    endif

  end function mfile_remove_blank_lines

  ! Returns 0 if ok.
  ! 1 if fails, memory problem.
  !
  function mfile_add_line(key, line, num, extra, extra_line) result (status)
    character(len=*), dimension(:), pointer :: key
    character(len=*), intent(in   )         :: line
    integer         , intent(inout)         :: num
    character(len=*), dimension(:), pointer :: extra
    character(len=*), intent(in   )         :: extra_line
    integer                                 :: status

    if ((num > size(key)) .or. (num > size(extra))) then
      call pc_error('MFILE:  bad input parameters.')
      status = 1
      return
    endif

    num = num + 1

    call mem_realloc(key, num, status)
    if (status /= 0) then
      return
    endif

    call mem_realloc(extra, num, status)
    if (status == 0) then
      key  (num) =       line
      extra(num) = extra_line
    endif

  end function mfile_add_line

  ! Returns 0 if ok.
  ! 1 if fails, memory problem.
  !
  function mfile_blank_duplicates(array, num, num_dups) result (status)
    character(len=*), dimension(:), intent(inout) :: array
    integer                       , intent(in   ) :: num
    integer                       , intent(out  ) :: num_dups
    integer                                       :: status

    integer         , dimension(:), pointer       :: sort_indices
    integer                                       :: ikeep
    integer                                       :: i

    if (num < 2) then
      num_dups = 0
      status   = 0
      return
    endif

    nullify(sort_indices)
    call mem_alloc(sort_indices, num, status)
    if (status /= 0) then
      return
    endif

    do i = 1, num
      sort_indices(i) = i
    enddo

    call alphasort_sort(sort_indices, array, num)

    num_dups = 0
    ikeep = sort_indices(1)

    do i = 2, num

      ! Don't blank what is already blank.
      !
      if ((array(sort_indices(i)) == array(ikeep)) &
    .and. (array(sort_indices(i)) /= ' '         )) then

        if (ikeep < sort_indices(i)) then
          array(sort_indices(i)) = ' '
        else
          array(ikeep) = ' '
          ikeep = sort_indices(i)
        endif

        num_dups = num_dups + 1

      else

        ikeep = sort_indices(i)

      endif

    enddo

    call mem_free(sort_indices)

  end function mfile_blank_duplicates

  ! Returns 0 if ok.
  ! 1 if fails, memory problem.
  !
  function mfile_create_new_lines_struct(new_lines_struct) result (status)
    type (mfile_new_lines_struct), pointer :: new_lines_struct
    integer                               :: status

    allocate(new_lines_struct, stat=status)
    if (status /= 0) then
      return
    endif

    nullify       (new_lines_struct%old_lines)
    call mem_alloc(new_lines_struct%old_lines, 1, status)
    if (status /= 0) then
      deallocate(new_lines_struct)
      return
    endif

    nullify       (new_lines_struct%old_sort)
    call mem_alloc(new_lines_struct%old_sort, 1, status)
    if (status /= 0) then
      call mem_free(new_lines_struct%old_lines)
      deallocate   (new_lines_struct)
      return
    endif

    nullify       (new_lines_struct%new_indices)
    call mem_alloc(new_lines_struct%new_indices, 1, status)
    if (status /= 0) then
      call mem_free(new_lines_struct%old_lines)
      call mem_free(new_lines_struct%old_sort )
      deallocate   (new_lines_struct)
      return
    endif

    new_lines_struct%num_old_lines = 0

  end function mfile_create_new_lines_struct

  subroutine mfile_delete_new_lines_struct(new_lines_struct)
    type (mfile_new_lines_struct), pointer :: new_lines_struct

    call mem_free(new_lines_struct%old_lines  )
    call mem_free(new_lines_struct%old_sort   )
    call mem_free(new_lines_struct%new_indices)
    deallocate   (new_lines_struct)

  end subroutine mfile_delete_new_lines_struct

  ! Returns 0 if ok.
  ! 1 if fails, memory problem.
  ! Upon return, new_indices contains the index of each array member in
  ! the old array.  If an array member is new, -1 is in its location in
  ! new_indices.  The pointer new_indices points to
  ! new_lines_struct%new_indices.  So calling mfile_new_lines a second
  ! time or calling mfile_delete_new_lines_struct will invalid the result.
  !
  function mfile_new_lines(new_lines_struct, array, num, new_indices) &
    result (status)
    type (mfile_new_lines_struct) , pointer     :: new_lines_struct
    character(len=*), dimension(:), intent(in ) :: array
    integer                       , intent(in ) :: num
    integer         , dimension(:), pointer     :: new_indices
    integer                                     :: status

    integer         , dimension(:), pointer     :: new_lines_sort_indices
    integer                                     :: oldi
    integer                                     :: newi
    logical                                     :: changed
    integer                                     :: i

    ! nullify so mem_alloc doesn't try to try new_lines_sort_indices from
    ! last call.
    !
    nullify(new_lines_sort_indices)
    call mem_alloc(new_lines_sort_indices, num, status)
    if (status /= 0) then
      return
    endif

    do i = 1, num
      new_lines_sort_indices(i) = i
    enddo

    if (num > 0) then
      call alphasort_sort(new_lines_sort_indices, array, num)
    endif

    if (num > size(new_lines_struct%new_indices)) then
      call mem_alloc(new_lines_struct%new_indices, num, status)
      if (status /= 0) then
        return
      endif
    endif

    oldi = 1
    newi = 1
    do
      if ((oldi > new_lines_struct%num_old_lines) .or. (newi > num)) then
        exit
      endif

      if     (array(new_lines_sort_indices(newi)) < &
        new_lines_struct%old_lines(new_lines_struct%old_sort(oldi))) then

        new_lines_struct%new_indices(new_lines_sort_indices(newi)) = -1
        newi = newi + 1

      elseif (array(new_lines_sort_indices(newi)) > & 
        new_lines_struct%old_lines(new_lines_struct%old_sort(oldi))) then

        oldi = oldi + 1

      else   ! They're equal.

        new_lines_struct%new_indices(new_lines_sort_indices(newi)) = &
          new_lines_struct%old_sort(oldi)

        oldi = oldi + 1
        newi = newi + 1

      endif

    enddo

    do i = newi, num   ! Fortran must be able to execute loop 0 times.

      new_lines_struct%new_indices(new_lines_sort_indices(i)) = -1

    enddo

    if (num /= new_lines_struct%num_old_lines) then

      changed = .true.

    else

      changed = .false.
      do i = 1, num

        if (new_lines_struct%new_indices(i) /= i) then
          changed = .true.
          exit
        endif

      enddo

    endif

    if (changed) then

      if (num > size(new_lines_struct%old_lines)) then
        call mem_alloc(new_lines_struct%old_lines, num, status)
        if (status /= 0) then
          return
        endif
      endif

      call mem_free(new_lines_struct%old_sort)

      new_lines_struct%old_lines(1:num) =  array(1:num)
      new_lines_struct%old_sort         => new_lines_sort_indices
      new_lines_struct%num_old_lines    =  num

    else

      call mem_free(new_lines_sort_indices)

    endif

    new_indices => new_lines_struct%new_indices

  end function mfile_new_lines

  subroutine mfile_make_downward_compatible(obj)
    type (mfile_struct), intent(inout) :: obj

    character(len=FILENAME_LENGTH)     :: pathname
    integer                            :: status

    ! Check for old keyword.
    !
    if (pc_process_keyword_present('pathname')) then

      ! Shouldn't have both old and new.
      !
      if (pc_process_keyword_present('opt_file' ) &
     .or. pc_process_keyword_present('pathnames') &
     .or. pc_process_keyword_present('path_list')) then

        call pc_error('MFILE:  error converting old file')

      else

        call mem_realloc(obj%pathnames, 1, status)
        if (status /= 0) then
          call pc_error('MFILE:  out of memory.')
          return
        endif

        pathname = ' '
        call pc_get('pathname', pathname)
        if ((pathname == ' ') .or. (pathname == 'NONE')) then

          call pc_error('MFILE:  error converting old file')

        else

          obj%opt_file      = 'PATHNAMES'
          obj%num_pathnames = 1
          obj%pathnames(1)  = pathname
          call pc_remove_process_keyword('pathname')

          call pc_warning('MFILE:  converting old pathname')

        endif

      endif

    endif

  end subroutine mfile_make_downward_compatible

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


end module mfile_module


!!----------------------------- test code --------------------------------!!
!!----------------------------- test code ---------------------------------!!
!!----------------------------- test code ---------------------------------!!


!<TEST_CODE>
!
!  program test
!
!    use mfile_module
!    use mem_module
!
!    character(len=24)             , dimension(5)          :: start1 = &
!      (/'hello world             ', &
!        '/u/poepsn61/schmaeh/junk', &
!        'now is the time         ', &
!        'goodbye_world           ', &
!        '/u/poepsn61/schmaeh/junk'/)
!
!    character(len=24)             , dimension(5)          :: start2 = &
!      (/'good                    ', &
!        'bad                     ', &
!        'ugly                    ', &
!        'conoco/phillips         ', &
!        'hello world             '/)
!
!    character(len=FILENAME_LENGTH), dimension(:), pointer :: array
!    character(len=FILENAME_LENGTH), dimension(:), pointer :: extra
!    integer                                               :: num
!    integer                                               :: status
!    integer                                               :: num_dups
!    type (mfile_new_lines_struct)               , pointer :: new_lines_struct
!    integer                       , dimension(:), pointer :: new_indices
!    character(len=FILENAME_LENGTH)                        :: temp
!
!    nullify(array)
!    nullify(extra)

!    call mem_alloc(array, 5, status)
!    if (status /= 0) then
!      stop 'mem_alloc'
!    endif
!
!    call mem_alloc(extra, 5, status)
!    if (status /= 0) then
!      stop 'mem_alloc'
!    endif
!
!    array = start1
!    extra = start2
!    num   = 5
!    if (mfile_remove_blank_lines(array, num, extra, ' ') /= 0) then
!      stop 'mfile_remove_blank_lines status error'
!    endif
!
!    if (num /= 5 &
!   .or. size(array) /= 5 &
!   .or. size(extra) /= 5 &
!   .or. array(1) /= start1(1) &
!   .or. array(2) /= start1(2) &
!   .or. array(3) /= start1(3) &
!   .or. array(4) /= start1(4) &
!   .or. array(5) /= start1(5) &
!   .or. extra(1) /= start2(1) &
!   .or. extra(2) /= start2(2) &
!   .or. extra(3) /= start2(3) &
!   .or. extra(4) /= start2(4) &
!   .or. extra(5) /= start2(5)) then
!      stop 'mfile_remove_blank_lines result error'
!    endif
!
!    num = 4
!    if (mfile_remove_blank_lines(array, num, extra, ' ') /= 0) then
!      stop 'mfile_remove_blank_lines status error'
!    endif
!
!    if (num /= 4 &
!   .or. size(array) /= 4 &
!   .or. size(extra) /= 4 &
!   .or. array(1) /= start1(1) &
!   .or. array(2) /= start1(2) &
!   .or. array(3) /= start1(3) &
!   .or. array(4) /= start1(4) &
!   .or. extra(1) /= start2(1) &
!   .or. extra(2) /= start2(2) &
!   .or. extra(3) /= start2(3) &
!   .or. extra(4) /= start2(4)) then
!      stop 'mfile_remove_blank_lines result error'
!    endif
!
!    array(3) = ' '
!    num      = 4
!    if (mfile_remove_blank_lines(array, num, extra, ' ') /= 0) then
!      stop 'mfile_remove_blank_lines status error'
!    endif
!
!    if (num /= 3 &
!   .or. size(array) /= 3 &
!   .or. size(extra) /= 3 &
!   .or. array(1) /= start1(1) &
!   .or. array(2) /= start1(2) &
!   .or. array(3) /= start1(4) &
!   .or. extra(1) /= start2(1) &
!   .or. extra(2) /= start2(2) &
!   .or. extra(3) /= start2(4)) then
!      stop 'mfile_remove_blank_lines result error'
!    endif
!
!    call mem_free(array)
!    call mem_free(extra)
!    nullify(array)
!    nullify(extra)
!
!    call mem_alloc(array, 5, status)
!    if (status /= 0) then
!      stop 'mem_alloc'
!    endif
!
!    call mem_alloc(extra, 5, status)
!    if (status /= 0) then
!      stop 'mem_alloc'
!    endif
!
!    array = start1
!    extra = start2
!    num   = 5
!    if (mfile_remove_blank_lines(array, num, extra, ' ') /= 0) then
!      stop 'mfile_remove_blank_lines status error'
!    endif
!
!    if (num /= 5 &
!   .or. size(array) /= 5 &
!   .or. size(extra) /= 5 &
!   .or. array(1) /= start1(1) &
!   .or. array(2) /= start1(2) &
!   .or. array(3) /= start1(3) &
!   .or. array(4) /= start1(4) &
!   .or. array(5) /= start1(5) &
!   .or. extra(1) /= start2(1) &
!   .or. extra(2) /= start2(2) &
!   .or. extra(3) /= start2(3) &
!   .or. extra(4) /= start2(4) &
!   .or. extra(5) /= start2(5)) then
!      stop 'mfile_remove_blank_lines result error'
!    endif
!
!    array(5) = ' '
!    if (mfile_remove_blank_lines(array, num, extra, ' ') /= 0) then
!      stop 'mfile_remove_blank_lines status error'
!    endif
!
!    if (num /= 4 &
!   .or. size(array) /= 4 &
!   .or. size(extra) /= 4 &
!   .or. array(1) /= start1(1) &
!   .or. array(2) /= start1(2) &
!   .or. array(3) /= start1(3) &
!   .or. array(4) /= start1(4) &
!   .or. extra(1) /= start2(1) &
!   .or. extra(2) /= start2(2) &
!   .or. extra(3) /= start2(3) &
!   .or. extra(4) /= start2(4)) then
!      stop 'mfile_remove_blank_lines result error'
!    endif
!
!    array(1) = ' '
!    if (mfile_remove_blank_lines(array, num, extra, ' ') /= 0) then
!      stop 'mfile_remove_blank_lines status error'
!    endif
!
!    if (num /= 3 &
!   .or. size(array) /= 3 &
!   .or. size(extra) /= 3 &
!   .or. array(1) /= start1(2) &
!   .or. array(2) /= start1(3) &
!   .or. array(3) /= start1(4) &
!   .or. extra(1) /= start2(2) &
!   .or. extra(2) /= start2(3) &
!   .or. extra(3) /= start2(4)) then
!      stop 'mfile_remove_blank_lines result error'
!    endif
!
!    array(2) = ' '
!    if (mfile_remove_blank_lines(array, num, extra, ' ') /= 0) then
!      stop 'mfile_remove_blank_lines status error'
!    endif
!
!    if (num /= 2 &
!   .or. size(array) /= 2 &
!   .or. size(extra) /= 2 &
!   .or. array(1) /= start1(2) &
!   .or. array(2) /= start1(4) &
!   .or. extra(1) /= start2(2) &
!   .or. extra(2) /= start2(4)) then
!      stop 'mfile_remove_blank_lines result error'
!    endif
!
!    if (mfile_add_line(array, start1(3), num, extra, start2(3)) /= 0) then
!      stop 'mfile_add_line status error'
!    endif
!
!    if (num /= 3 &
!   .or. size(array) /= 3 &
!   .or. size(extra) /= 3 &
!   .or. array(1) /= start1(2) &
!   .or. array(2) /= start1(4) &
!   .or. array(3) /= start1(3) &
!   .or. extra(1) /= start2(2) &
!   .or. extra(2) /= start2(4) &
!   .or. extra(3) /= start2(3)) then
!      stop 'mfile_add_line result error'
!    endif
!
!    if (mfile_add_line(array, start1(3), num, extra, start2(3)) /= 0) then
!      stop 'mfile_add_line status error'
!    endif
!
!    if (num /= 4 &
!   .or. size(array) /= 4 &
!   .or. size(extra) /= 4 &
!   .or. array(1) /= start1(2) &
!   .or. array(2) /= start1(4) &
!   .or. array(3) /= start1(3) &
!   .or. array(4) /= start1(3) &
!   .or. extra(1) /= start2(2) &
!   .or. extra(2) /= start2(4) &
!   .or. extra(3) /= start2(3) &
!   .or. extra(4) /= start2(3)) then
!      stop 'mfile_add_line result error'
!    endif
!
!    array(1) = ' '
!    array(2) = ' '
!    array(3) = ' '
!    array(4) = ' '
!    if (mfile_remove_blank_lines(array, num, extra, ' ') /= 0) then
!      stop 'mfile_remove_blank_lines status error'
!    endif
!
!    if (num /= 0 &
!   .or. size(array) /= 1 &
!   .or. size(extra) /= 1) then
!      stop 'mfile_remove_blank_lines result error'
!    endif
!
!    if (mfile_remove_blank_lines(array, num, extra, ' ') /= 0) then
!      stop 'mfile_remove_blank_lines status error'
!    endif
!
!    if (num /= 0 &
!   .or. size(array) /= 1 &
!   .or. size(extra) /= 1) then
!      stop 'mfile_remove_blank_lines result error'
!    endif
!
!    if (mfile_add_line(array, start1(3), num, extra, start2(3)) /= 0) then
!      stop 'mfile_add_line status error'
!    endif
!
!    if (num /= 1 &
!   .or. size(array) /= 1 &
!   .or. size(extra) /= 1 &
!   .or. array(1) /= start1(3) &
!   .or. extra(1) /= start2(3)) then
!      stop 'mfile_add_line result error'
!    endif
!
!    if (mfile_add_line(array, start1(3), num, extra, start2(3)) /= 0) then
!      stop 'mfile_add_line status error'
!    endif
!
!    if (num /= 2 &
!   .or. size(array) /= 2 &
!   .or. size(extra) /= 2 &
!   .or. array(1) /= start1(3) &
!   .or. extra(1) /= start2(3) &
!   .or. array(2) /= start1(3) &
!   .or. extra(2) /= start2(3)) then
!      stop 'mfile_add_line result error'
!    endif
!
!    if (mfile_add_line(array, start1(1), num, extra, start2(1)) /= 0) then
!      stop 'mfile_add_line status error'
!    endif
!
!    if (num /= 3 &
!   .or. size(array) /= 3 &
!   .or. size(extra) /= 3 &
!   .or. array(1) /= start1(3) &
!   .or. extra(1) /= start2(3) &
!   .or. array(2) /= start1(3) &
!   .or. extra(2) /= start2(3) &
!   .or. array(3) /= start1(1) &
!   .or. extra(3) /= start2(1)) then
!      stop 'mfile_add_line result error'
!    endif
!
!    call mem_free(array)
!    call mem_free(extra)
!    nullify(array)
!    nullify(extra)
!
!    call mem_alloc(array, 5, status)
!    if (status /= 0) then
!      stop 'mem_alloc'
!    endif
!
!    call mem_alloc(extra, 5, status)
!    if (status /= 0) then
!      stop 'mem_alloc'
!    endif
!
!    array = start1
!    extra = start2
!    num   = 5
!    if (mfile_blank_duplicates(array, num, num_dups) /= 0) then
!      stop 'mfile_blank_duplicates status error'
!    endif
!
!    if (num_dups /= 1 &
!   .or. array(1) /= start1(1) &
!   .or. array(2) /= start1(2) &
!   .or. array(3) /= start1(3) &
!   .or. array(4) /= start1(4) &
!   .or. array(5) /= ' '     ) then
!      stop 'mfile_blank_duplicates result error'
!    endif
!
!    array(2) = array(1)
!    array(3) = array(1)
!    array(4) = array(1)
!    array(5) = array(1)
!    num   = 5
!    if (mfile_blank_duplicates(array, num, num_dups) /= 0) then
!      stop 'mfile_blank_duplicates status error'
!    endif
!
!    if (num_dups /= 4 &
!   .or. array(1) /= start1(1) &
!   .or. array(2) /= ' '      &
!   .or. array(3) /= ' '      &
!   .or. array(4) /= ' '      &
!   .or. array(5) /= ' '     ) then
!      stop 'mfile_blank_duplicates result error'
!    endif
!
!    if (mfile_blank_duplicates(array, num, num_dups) /= 0) then
!      stop 'mfile_blank_duplicates status error'
!    endif
!
!    if (num_dups /= 0 &
!   .or. array(1) /= start1(1) &
!   .or. array(2) /= ' '      &
!   .or. array(3) /= ' '      &
!   .or. array(4) /= ' '      &
!   .or. array(5) /= ' '     ) then
!      stop 'mfile_blank_duplicates result error'
!    endif
!
!    array(1) = start1(1)
!    array(2) = start1(2)
!    array(3) = start1(1)
!    array(4) = start1(2)
!    array(5) = start1(1)
!    num   = 5
!    if (mfile_blank_duplicates(array, num, num_dups) /= 0) then
!      stop 'mfile_blank_duplicates status error'
!    endif
!
!    if (num_dups /= 3 &
!   .or. array(1) /= start1(1) &
!   .or. array(2) /= start1(2) &
!   .or. array(3) /= ' '      &
!   .or. array(4) /= ' '      &
!   .or. array(5) /= ' '     ) then
!      stop 'mfile_blank_duplicates result error'
!    endif
!
!    num   = 1
!    if (mfile_blank_duplicates(array, num, num_dups) /= 0) then
!      stop 'mfile_blank_duplicates status error'
!    endif
!
!    if (num_dups /= 0 &
!   .or. array(1) /= start1(1)) then
!      stop 'mfile_blank_duplicates result error'
!    endif
!
!    array(1) = start1(1)
!    array(2) = start1(1)
!    array(3) = start1(2)
!    array(4) = start1(3)
!    array(5) = start1(4)
!    num   = 5
!    if (mfile_blank_duplicates(array, num, num_dups) /= 0) then
!      stop 'mfile_blank_duplicates status error'
!    endif
!
!    if (num_dups /= 1 &
!   .or. array(1) /= start1(1) &
!   .or. array(2) /= ' '      &
!   .or. array(3) /= start1(2) &
!   .or. array(4) /= start1(3) &
!   .or. array(5) /= start1(4)) then
!      stop 'mfile_blank_duplicates result error'
!    endif
!
!    array(1) = start1(1)
!    array(2) = start1(2)
!    array(3) = start1(3)
!    array(4) = start1(4)
!    array(5) = start1(4)
!    num   = 5
!    if (mfile_blank_duplicates(array, num, num_dups) /= 0) then
!      stop 'mfile_blank_duplicates status error'
!    endif
!
!    if (num_dups /= 1 &
!   .or. array(1) /= start1(1) &
!   .or. array(2) /= start1(2) &
!   .or. array(3) /= start1(3) &
!   .or. array(4) /= start1(4) &
!   .or. array(5) /= ' '      ) then
!      stop 'mfile_blank_duplicates result error'
!    endif
!
!    array = start1
!    extra = start2
!    num   = 5
!    if (mfile_blank_duplicates(array, num, num_dups) /= 0) then
!      stop 'mfile_blank_duplicates status error'
!    endif
!
!    if (mfile_remove_blank_lines(array, num, extra, ' ') /= 0) then
!      stop 'mfile_remove_blank_lines status error'
!    endif
!
!    if (num /= 4 &
!   .or. size(array) /= 4 &
!   .or. size(extra) /= 4 &
!   .or. array(1) /= start1(1) &
!   .or. array(2) /= start1(2) &
!   .or. array(3) /= start1(3) &
!   .or. array(4) /= start1(4) &
!   .or. extra(1) /= start2(1) &
!   .or. extra(2) /= start2(2) &
!   .or. extra(3) /= start2(3) &
!   .or. extra(4) /= start2(4)) then
!      stop 'mfile_remove_blank_lines result error'
!    endif
!
!    array(2) = ' '
!    extra(2) = 'except'
!    if (mfile_remove_blank_lines(array, num, extra, 'except') /= 0) then
!      stop 'mfile_remove_blank_lines status error'
!    endif
!
!    if (num /= 4 &
!   .or. size(array) /= 4 &
!   .or. size(extra) /= 4 &
!   .or. array(1) /= start1(1) &
!   .or. array(2) /= ' '       &
!   .or. array(3) /= start1(3) &
!   .or. array(4) /= start1(4) &
!   .or. extra(1) /= start2(1) &
!   .or. extra(2) /= 'except'  &
!   .or. extra(3) /= start2(3) &
!   .or. extra(4) /= start2(4)) then
!      stop 'mfile_remove_blank_lines result error'
!    endif
!
!    array(4) = ' '
!    extra(4) = ' '
!    if (mfile_remove_blank_lines(array, num, extra, ' ') /= 0) then
!      stop 'mfile_remove_blank_lines status error'
!    endif
!
!    if (num /= 3 &
!   .or. size(array) /= 3 &
!   .or. size(extra) /= 3 &
!   .or. array(1) /= start1(1) &
!   .or. array(2) /= start1(3) &
!   .or. array(3) /= ' '       &
!   .or. extra(1) /= start2(1) &
!   .or. extra(2) /= start2(3) &
!   .or. extra(3) /= ' '      ) then
!      stop 'mfile_remove_blank_lines result error'
!    endif
!
!    call mem_free(array)
!    call mem_free(extra)
!    nullify(array)
!
!    if (mfile_create_new_lines_struct(new_lines_struct) /= 0) then
!      stop 'mfile_create_new_lines_struct status error'
!    endif
!
!    call mem_alloc(array, 5, status)
!    if (status /= 0) then
!      stop 'mem_alloc'
!    endif
!
!    array(1) = start1(1)
!    num      = 1
!    if (mfile_new_lines(new_lines_struct, array, num, new_indices) /= 0) then
!      stop 'mfile_new_lines status error'
!    endif
!
!    if (new_indices(1) /= -1) then
!      stop 'mfile_new_lines result error'
!    endif
!
!    if (mfile_new_lines(new_lines_struct, array, num, new_indices) /= 0) then
!      stop 'mfile_new_lines status error'
!    endif
!
!    if (new_indices(1) /= 1) then
!      stop 'mfile_new_lines result error'
!    endif
!
!    num = 0
!    if (mfile_new_lines(new_lines_struct, array, num, new_indices) /= 0) then
!      stop 'mfile_new_lines status error'
!    endif
!
!    call mfile_delete_new_lines_struct(new_lines_struct)
!
!    if (mfile_create_new_lines_struct(new_lines_struct) /= 0) then
!      stop 'mfile_create_new_lines_struct status error'
!    endif
!
!    num = 0
!    if (mfile_new_lines(new_lines_struct, array, num, new_indices) /= 0) then
!      stop 'mfile_new_lines status error'
!    endif
!
!    call mfile_delete_new_lines_struct(new_lines_struct)
!
!    if (mfile_create_new_lines_struct(new_lines_struct) /= 0) then
!      stop 'mfile_create_new_lines_struct status error'
!    endif
!
!    start1(5) = '/u/poepsn61/schmaeh/tull'
!    array = start1
!    num = 5
!    if (mfile_new_lines(new_lines_struct, array, num, new_indices) /= 0) then
!      stop 'mfile_new_lines status error'
!    endif
!
!    if (new_indices(1) /= -1 &
!   .or. new_indices(2) /= -1 &
!   .or. new_indices(3) /= -1 &
!   .or. new_indices(4) /= -1 &
!   .or. new_indices(5) /= -1) then
!      stop 'mfile_new_lines result error'
!    endif
!
!    num = 3
!    if (mfile_new_lines(new_lines_struct, array, num, new_indices) /= 0) then
!      stop 'mfile_new_lines status error'
!    endif
!
!    if (new_indices(1) /= 1 &
!   .or. new_indices(2) /= 2 &
!   .or. new_indices(3) /= 3) then
!      stop 'mfile_new_lines result error'
!    endif
!
!    num = 5
!    if (mfile_new_lines(new_lines_struct, array, num, new_indices) /= 0) then
!      stop 'mfile_new_lines status error'
!    endif
!
!    if (new_indices(1) /=  1 &
!   .or. new_indices(2) /=  2 &
!   .or. new_indices(3) /=  3 &
!   .or. new_indices(4) /= -1 &
!   .or. new_indices(5) /= -1) then
!      stop 'mfile_new_lines result error'
!    endif
!
!    array(1) = start1(1)
!    array(2) = start1(3)
!    array(3) = start1(5)
!    num = 3
!    if (mfile_new_lines(new_lines_struct, array, num, new_indices) /= 0) then
!      stop 'mfile_new_lines status error'
!    endif
!
!    if (new_indices(1) /= 1 &
!   .or. new_indices(2) /= 3 &
!   .or. new_indices(3) /= 5) then
!      stop 'mfile_new_lines result error'
!    endif
!
!    array = start1
!    num = 5
!    if (mfile_new_lines(new_lines_struct, array, num, new_indices) /= 0) then
!      stop 'mfile_new_lines status error'
!    endif
!
!    if (new_indices(1) /=  1 &
!   .or. new_indices(2) /= -1 &
!   .or. new_indices(3) /=  2 &
!   .or. new_indices(4) /= -1 &
!   .or. new_indices(5) /=  3) then
!      stop 'mfile_new_lines result error'
!    endif
!
!    array(4) = start1(5)
!    num = 4
!    if (mfile_new_lines(new_lines_struct, array, num, new_indices) /= 0) then
!      stop 'mfile_new_lines status error'
!    endif
!
!    if (new_indices(1) /= 1 &
!   .or. new_indices(2) /= 2 &
!   .or. new_indices(3) /= 3 &
!   .or. new_indices(4) /= 5) then
!      stop 'mfile_new_lines result error'
!    endif
!
!    array(2) = start1(3)
!    array(3) = start1(4)
!    num = 4
!    if (mfile_new_lines(new_lines_struct, array, num, new_indices) /= 0) then
!      stop 'mfile_new_lines status error'
!    endif
!
!    if (new_indices(1) /=  1 &
!   .or. new_indices(2) /=  3 &
!   .or. new_indices(3) /= -1 &
!   .or. new_indices(4) /=  4) then
!      stop 'mfile_new_lines result error'
!    endif
!
!    array = start1
!    num = 5
!    if (mfile_new_lines(new_lines_struct, array, num, new_indices) /= 0) then
!      stop 'mfile_new_lines status error'
!    endif
!
!    if (new_indices(1) /=  1 &
!   .or. new_indices(2) /= -1 &
!   .or. new_indices(3) /=  2 &
!   .or. new_indices(4) /=  3 &
!   .or. new_indices(5) /=  4) then
!      stop 'mfile_new_lines result error'
!    endif
!
!    array(2) = trim(array(2)) // 'change'
!    if (mfile_new_lines(new_lines_struct, array, num, new_indices) /= 0) then
!      stop 'mfile_new_lines status error'
!    endif
!
!    if (new_indices(1) /=  1 &
!   .or. new_indices(2) /= -1 &
!   .or. new_indices(3) /=  3 &
!   .or. new_indices(4) /=  4 &
!   .or. new_indices(5) /=  5) then
!      stop 'mfile_new_lines result error'
!    endif
!
!    array(2) = trim(array(2)) // 'change'
!    array(4) = trim(array(4)) // 'change'
!    if (mfile_new_lines(new_lines_struct, array, num, new_indices) /= 0) then
!      stop 'mfile_new_lines status error'
!    endif
!
!    if (new_indices(1) /=  1 &
!   .or. new_indices(2) /= -1 &
!   .or. new_indices(3) /=  3 &
!   .or. new_indices(4) /= -1 &
!   .or. new_indices(5) /=  5) then
!      stop 'mfile_new_lines result error'
!    endif
!
!    temp     = array(2)
!    array(2) = array(3)
!    array(3) = array(4)
!    array(4) = temp
!    if (mfile_new_lines(new_lines_struct, array, num, new_indices) /= 0) then
!      stop 'mfile_new_lines status error'
!    endif
!
!    if (new_indices(1) /= 1 &
!   .or. new_indices(2) /= 3 &
!   .or. new_indices(3) /= 4 &
!   .or. new_indices(4) /= 2 &
!   .or. new_indices(5) /= 5) then
!      stop 'mfile_new_lines result error'
!    endif
!
!    call mfile_delete_new_lines_struct(new_lines_struct)
!
!    call mem_free(array)
!    nullify(array)
!
!    print *, 'Success!!!'
!
!  end program test
!
!</TEST_CODE>

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
