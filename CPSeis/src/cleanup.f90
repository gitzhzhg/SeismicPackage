!<CPS_v1 type="PROCESS"/>

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
!                         C P S   P R O C E S S             
!
! Name       : cleanup
! Category   : miscellaneous
! Written    : 2001-06-12   by: Donna K. Vunderink
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Automatically delete disk files using jobfile commands.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! CLEANUP automatically deletes disk files whose pathnames are specified by the
! user.  Files may be specified in two ways.
!
!     1.  Enter the individual pathnames of files to be deleted in the
!     PATH_DELETE array.
!
!     2. Specify, in PATH_FILES, the pathname of a textfile containing
!     pathnames of files to be deleted.  This file must be an ascii file with
!     one pathname on each line.
!
! Files may be specified by either or both method in the same instance of the
! process.
!
!
! If OPT_DELETE = BEFORE, then the specified files will be deleted when the job
! starts.
!
! If OPT_DELETE = AFTER, then the specified files will be deleted only after
! the job completes normally.  The files will NOT be deleted if the job aborts.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! None.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! None.  (traces pass through)
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NONE
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! NONE
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                
!
!     Date        Author      Description
!     ----        ------      -----------
!005. 2006-09-18  D. Glover   Added NULLIFY statements for Intel compiler.
! 4.  2004-02-02  Bill Menger Added dummy calls for operation as a non-setup-
!                             only job.
! 3.  2001-10-16  Vunderink   Added cleanup_wrapup subroutine
! 2.  2001-06-15  Vunderink   Added shell command to ignore remove errors
! 1.  2001-06-12  Vunderink   Initial version.
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
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS       
!
! This process only operates at setup and at wrapup, but is called as a normal
! process.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS     
!
!  None provided.
!
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


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


!<gui_def>
!<NS Cleanup Process/NC=85>
!
!                     Delete disk files before or after job executes.
!
!
! OPT_DELETE~~=`CCCCCCCCCC
!
! Select PATH_FILES [PATH_FILES]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! [/YST]PATH_DELETE
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!<PARMS OPT_DELETE[/XST/YST]>
!<PARMS PATH_FILES[/ML=140/XST]>
!<PARMS PATH_DELETE[/ML=140/XST]>
!</gui_def>


!<HelpSection>
!
!<Help KEYWORD="OPT_DELETE">
!<Tip> Whether files will be deleted when job starts or after job runs. </Tip>
! Default = AFTER
! Allowed = AFTER   (Delete specified files after job comples normally.)
! Allowed = BEFORE  (Delete specified files when job starts.)
! If OPT_DELETE = AFTER, specified files will NOT be deleted if job aborts.
!</Help>
!
!<Help KEYWORD="PATH_FILES">
!<Tip> Pathname of file containing pathnames of files to be deleted. </Tip>
! Default =  -
! Allowed = char
! Pathname of file containing list of pathnames of files to be deleted.  This
! file must be an ascii file with one pathname on each line.
!
! Files may be specified for deletion by either PATH_FILES or PATH_DELETE or
! both in the same instance of CLEANUP.
!</Help>
!
!<Help KEYWORD="SELECT_PATH_FILES">
!<Tip> This button accesses the Select PATH_FILES dialog box. </Tip>
! Choose a PATH_FILES via a file selection dialog.
!</Help>
!
!<Help KEYWORD="PATH_DELETE">
!<Tip> Array of pathnames of files to be deleted. </Tip>
! Default =  -
! Allowed = char (array 50)
! Files may be specified for deletion by either PATH_FILES or PATH_DELETE or
! both in the same instance of CLEANUP.
!</Help>
!
!</HelpSection>
!
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module cleanup_module
      use finquire_module
      use named_constants_module
      use pathcheck_module
      use pathchoose_module
      use pc_module
      use string_module
      implicit none
      private
      public :: cleanup_create
      public :: cleanup_initialize
      public :: cleanup_update
      public :: cleanup_delete
      public :: cleanup_wrapup
      public :: cleanup


      character(len=100),public,save :: cleanup_IDENT = &
'$Id: cleanup.f90,v 1.5 2006/09/18 13:32:44 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: cleanup_struct              
 
        private
        logical                                :: skip_wrapup !wrapup flag
        character(len=8)                       :: opt_delete ! process parameter
        character(len=FILENAME_LENGTH)         :: path_files ! process parameter
        character(len=FILENAME_LENGTH),pointer :: path_delete(:) 
                                                             ! process parameter
        integer                                :: npath_delete
                                                             ! process parameter
        type(pathchoose_struct),pointer        :: path_files_dialog
                                                             ! process parameter

      end type cleanup_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(cleanup_struct),pointer,save :: object      ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine cleanup_create (obj)
      implicit none
      type(cleanup_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%path_delete)
      nullify (obj%path_files_dialog) ! jpa
      call pathchoose_create (obj%path_files_dialog,'PATH_FILES','lst')
      call cleanup_initialize (obj)
      return
      end subroutine cleanup_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine cleanup_delete (obj)
      implicit none
      type(cleanup_struct),pointer :: obj       ! arguments

      call cleanup_wrapup (obj)

      if (associated(obj%path_delete)) deallocate(obj%path_delete)
      call pathchoose_delete (obj%path_files_dialog)

      deallocate(obj)

      return
      end subroutine cleanup_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine cleanup_initialize (obj)
      implicit none
      type(cleanup_struct),intent(inout) :: obj       ! arguments

      obj%opt_delete   = 'AFTER'
      obj%path_files   = PATHCHECK_EMPTY
      obj%npath_delete = 0

      call cleanup_update (obj)

      return
      end subroutine cleanup_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine cleanup_update (obj)
      implicit none
      type(cleanup_struct),intent(inout),target     :: obj         ! arguments

      integer                                       :: i           ! local
      integer                                       :: j           ! local
      integer                                       :: ncmds       ! local
      character(len=10+FILENAME_LENGTH),allocatable :: cmds(:)     ! local
      character(len=4)                              :: cipn        ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      if (pathchoose_update(obj%path_files_dialog,obj%path_files)) return

      call pc_get   ('OPT_DELETE',  obj%opt_delete, cleanup_trap)
      call pc_get   ('PATH_FILES',  obj%path_files, cleanup_trap)

      call pc_alloc           ('PATH_DELETE', obj%path_delete, obj%npath_delete)
      call pc_call_array_trap ('PATH_DELETE', cleanup_trap)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      call pc_call_end_trap (cleanup_end_trap)


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('OPT_DELETE', (/'AFTER ','BEFORE'/), 2)

      call pc_put ('OPT_DELETE' , obj%opt_delete)
      call pc_put ('PATH_FILES' , obj%path_files)
      call pc_put ('PATH_DELETE', obj%path_delete, obj%npath_delete)

      !--- This next line breaks cfebld for parallell jobs...
      !--- call pc_put_control ('SETUP_ONLY', .true.)

      i = pc_get_ipn()
      call string_ii2cc (i,cipn)

      ncmds = 0
      if (obj%npath_delete .gt. 0) then
        if (len_trim(obj%path_files) .gt. 0 .and.  &
            trim(obj%path_files) .ne. PATHCHECK_EMPTY) then
          ncmds = obj%npath_delete + 4
          allocate(cmds(ncmds))
          cmds(1) = 'echo "----- Begin CLEANUP '//cipn// ' ----- " > /dev/null'
          cmds(2) = 'set +e'
          cmds(3) = '/usr/app/vendors/sps/scripts/prm_list '//  &
                                                            trim(obj%path_files)
          j = 1
          do i=4,ncmds-1
            cmds(i) = 'prm '//trim(obj%path_delete(j))
            j = j + 1
          enddo
          cmds(ncmds) = 'echo "----- End   CLEANUP '//cipn//  &
                                                          ' ----- " > /dev/null'
        else
          ncmds = obj%npath_delete + 3
          allocate(cmds(ncmds))
          cmds(1) = 'echo "----- Begin CLEANUP '//cipn//' ----- " > /dev/null'
          cmds(2) = 'set +e'
          j = 1
          do i=3,ncmds-1
            cmds(i) = 'prm '//trim(obj%path_delete(j))
            j = j + 1
          enddo
          cmds(ncmds) = 'echo "----- End   CLEANUP '//cipn//  &
                                                          ' ----- " > /dev/null'
        endif
      else if (len_trim(obj%path_files) .gt. 0 .and.  &
               trim(obj%path_files) .ne. PATHCHECK_EMPTY) then
        ncmds = 4
        allocate(cmds(ncmds))
        cmds(1) = 'echo "----- Begin CLEANUP '//cipn//' ----- " > /dev/null'
        cmds(2) = 'set +e'
        cmds(3) = '/usr/app/vendors/sps/scripts/prm_list '//trim(obj%path_files)
        cmds(4) = 'echo "----- End   CLEANUP '//cipn//' ----- " > /dev/null'
      endif

      if (ncmds .gt. 0) then
        if (trim(obj%opt_delete) .eq. 'AFTER') then
          call pc_put_control ('CMD_AFTER_EXE' ,cmds,ncmds)
        else
          call pc_put_control ('CMD_BEFORE_EXE',cmds,ncmds)
        endif       
        deallocate(cmds)
      endif

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine cleanup_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


      subroutine cleanup_trap (keyword)
      implicit none
      character(len=*),intent(in)  ::  keyword   ! argument

      integer                      ::  i         ! local

      select case (keyword)

        case ('OPT_DELETE')
          call string_to_upper (object%opt_delete)
          if (trim(object%opt_delete) .ne. 'AFTER' .and.  &
              trim(object%opt_delete) .ne. 'BEFORE') then
            call pc_error ('Invalid OPT_DELETE '//trim(object%opt_delete))
            object%opt_delete = 'AFTER'
          endif

        case ('PATH_FILES')
          call pathcheck ('PATH_FILES',object%path_files)

        case ('PATH_DELETE')
          if (object%npath_delete .gt. 0) then
            do i=1,object%npath_delete
              call pathcheck ('PATH_DELETE',object%path_delete(i))
            enddo
          endif

        case default

      end select

      end subroutine cleanup_trap


      subroutine cleanup_end_trap

      if (object%npath_delete .eq. 0         .and.  &
         (len_trim(object%path_files) .eq. 0 .or.   &
          trim(object%path_files) .eq. PATHCHECK_EMPTY)) then
        call pc_info ('No files to cleanup.')
      endif

      end subroutine cleanup_end_trap


!!------------------------------- dummy run---------------------------------!!
!!------------------------------- dummy run---------------------------------!!
!!------------------------------- dummy run---------------------------------!!

      subroutine cleanup (obj,ntr,hd,tr)
        type(cleanup_struct),pointer    :: obj      ! parm block
        integer         ,intent(inout)  :: ntr      ! num trc
        double precision,intent(inout)  :: hd(:,:)  ! headers
        real            ,intent(inout)  :: tr(:,:)  ! traces
        return
      end subroutine cleanup



!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine cleanup_wrapup (obj)
      implicit none
      type(cleanup_struct),pointer        :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine cleanup_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module cleanup_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

