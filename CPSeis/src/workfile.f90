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
! Name       : workfile 
! Category   : cfe
! Written    : 1999-08-03   by: Donna K. Vunderink
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : CFE Workfile Object Module.
! Portability: No known limitations, but see note below.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This primitive is used by cfe.
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
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!021. 2006-09-18  D. Glover    Added NULLIFY statements for Intel compiler.
! 20. 2006-01-10  B. Menger    Remove Unused Variables.
! 19. 2003-11-18  Stoeckley    Provide workarounds for Portland Group compiler.
! 18. 2003-09-15  Stoeckley    Changed type to primitive; changed category to
!                               cfe.
! 17. 2002-09-23  Vunderink    Changed prints to writes for checkc.
! 16. 2002-08-20  Vunderink    Fixed jobname in workfile_copy.
! 15. 2002-03-07  Vunderink    Remove file lock from write opens of workfiles.
! 14. 2001-08-08  Vunderink    Set packing option after cardset create and clear
!                                and next process may not have parameters so,
!                                clear parameter cardset after process is in
!                                workfile.
! 13. 2001-06-11  Vunderink    Fixed bug in workfile_copy causing bad 
!                                FRONTEND_PATH for the JOB_DATA process
! 12. 2001-02-26  Vunderink    Set FRONTEND_PATH for JOB_DATA process
! 11. 2000-12-05  Vunderink    Made changes to run_all_traps for multi-workfile
!                                builder
! 10. 2000-09-18  Vunderink    Add optional starting process to run_all_traps
!  9. 2000-09-06  Vunderink    Fixed bug in workfile_process_list
!  8. 2000-09-04  Vunderink    Added frontend argument to run_all_traps and
!                                use pc_quick_update unless argument is set.
!  7. 2000-08-15  Vunderink    Removed use of cfe_constants, added pclun
!                                module for parameter cache lun, and added
!                                routines copy and reload for multi-workfile
!                                builder
!  6. 2000-03-17  Vunderink    Modified to use new process_module routines
!                                instead of super_module routines
!  5. 2000-03-02  Vunderink    Added workfile_insert_first_process
!  4. 2000-02-29  Vunderink    Added calls to update a mates previous whenever
!                                it's previous is deleted.
!  3. 2000-02-05  Vunderink    Added report_warnings and report_infos to
!                                workfile_run_all_traps, and fixed to work with
!                                parameter cache change of checking ipn on put
!                                of jdata and pdata cards.
!  2. 2000-02-03  Vunderink    Improved use of cio_module in workfile_read and
!                                workfile_write, and added report_errors option
!                                to workfile_run_all_traps
!  1. 1999-08-03  Vunderink    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! The Portland Group compiler incorrectly executes code such as the following:
!
!       subroutine glotch_get_next (aaa,next)
!       type(xxxx_struct),pointer :: aaa,next
!       next => aaa%next
!       end subroutine
!
!       call glotch_get_next (ccc,ccc)
!
! To work around the bug, calls such as the above (in this file and several
! others) have been changed to:
!
!       call glotch_get_next (ccc,next)
!       ccc => next
!
! Changing the subroutine like this does not fix the problem:
!
!       subroutine glotch_get_next (aaa,next)
!       type(xxxx_struct),pointer :: aaa,next
!       type(xxxx_struct),pointer :: temp
!       temp => aaa%next
!       next => temp
!       end subroutine
!
! The Portland Group compiler required modifications in the code as follows:
!
!       Replaced reverse INDEX intrinsic function calls with STRING_INDEX.
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

      module workfile_module

      use pc_module
      use cardset_module
      use process_module
      use string_module
      use cio_module
      use pclun_module
      use path_module

      implicit none

      private
      public :: workfile_create
      public :: workfile_delete
      public :: workfile_copy
      public :: workfile_read
      public :: workfile_reload
      public :: workfile_write
      public :: workfile_delete_process
      public :: workfile_replace_process
      public :: workfile_insert_process
      public :: workfile_insert_first_process
      public :: workfile_set_first_process
      public :: workfile_set_last_process
      public :: workfile_set_num_processes
      public :: workfile_set_name
      public :: workfile_set_comment
      public :: workfile_set_previous
      public :: workfile_set_next
      public :: workfile_get_first_process
      public :: workfile_get_last_process
      public :: workfile_get_deleted
      public :: workfile_get_num_processes
      public :: workfile_get_name
      public :: workfile_get_comment
      public :: workfile_get_previous
      public :: workfile_get_next
      public :: workfile_clear_deleted
      public :: workfile_reset_deleted
      public :: workfile_process_list
      public :: workfile_run_all_traps

      character(len=100),public,save :: workfile_ident = &
       '$Id: workfile.f90,v 1.21 2006/09/18 13:32:52 Glover prod sps $'

      integer,parameter,private   :: STDOUT               = 6
      integer,parameter,public    :: WORKFILE_NAME_LEN    = 132
      integer,parameter,public    :: WORKFILE_COMMENT_LEN = 132


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,public :: workfile_struct              
        private
        character(len=WORKFILE_NAME_LEN)       :: name
        character(len=WORKFILE_COMMENT_LEN)    :: comment
        integer                                :: num_processes
        logical                                :: save_deleted
        type(process_struct),pointer           :: first_process
        type(process_struct),pointer           :: last_process
        type(process_struct),pointer           :: deleted
        type(workfile_struct),pointer          :: previous
        type(workfile_struct),pointer          :: next
      end type workfile_struct

      integer,save,private                     :: pc_lun = 6


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine workfile_create (obj)
      implicit none
      type(workfile_struct),pointer :: obj       ! arguments

      allocate (obj)
      obj%name    = ' '
      obj%comment = ' '
      obj%num_processes = 0
      obj%save_deleted = .true.
      nullify(obj%first_process)
      nullify(obj%last_process)
      nullify(obj%deleted)
      nullify(obj%previous)
      nullify(obj%next)

      return
      end subroutine workfile_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine workfile_delete (obj)
      implicit none
      type(workfile_struct),pointer     :: obj       ! arguments

      type(process_struct),pointer      :: previous  ! local

      nullify (previous) ! jpa
      if (.not. associated(obj)) return

      do
        if (.not. associated(obj%last_process)) exit
        call process_get_previous (obj%last_process,previous)
        call process_delete (obj%last_process)
        obj%last_process => previous
      enddo

      do
        if (.not. associated(obj%deleted)) exit
        call process_get_previous (obj%deleted,previous)
        call process_delete (obj%deleted)
        obj%deleted => previous
      enddo

      deallocate(obj)

      return
      end subroutine workfile_delete


!!------------------------------- copy ------------------------------------!!
!!------------------------------- copy ------------------------------------!!
!!------------------------------- copy ------------------------------------!!


      subroutine workfile_copy (from_obj,to_obj,report_errors,report_warnings, &
                                report_infos)
      implicit none
      type(workfile_struct),pointer           :: from_obj         ! argument
      type(workfile_struct),pointer           :: to_obj           ! argument
      logical,optional                        :: report_errors    ! argument
      logical,optional                        :: report_warnings  ! argument
      logical,optional                        :: report_infos     ! argument

      integer                                 :: i                ! local
      integer                                 :: i2               ! local
      character(len=PROCESS_NAME_LEN)         :: name             ! local
      character(len=CARDSET_LENGTH)           :: path             ! local
      character(len=CARDSET_LENGTH),pointer   :: errors(:)        ! local
      integer                                 :: nerrors          ! local
      character(len=CARDSET_LENGTH),pointer   :: warnings(:)      ! local
      integer                                 :: nwarnings        ! local
      character(len=CARDSET_LENGTH),pointer   :: infos(:)         ! local
      integer                                 :: ninfos           ! local


      type(process_struct) ,pointer           :: from_process     ! local
      type(process_struct) ,pointer           :: to_process       ! local
      type(process_struct) ,pointer           :: previous         ! local
      type(process_struct) ,pointer           :: next             ! local
      logical                                 :: rpt_errors       ! local
      logical                                 :: rpt_warnings     ! local
      logical                                 :: rpt_infos        ! local

      nullify (next) ! jpa
      if (.not. associated(from_obj)) return
      if (.not. associated(to_obj)  ) return

      path         = ' '
      rpt_errors   = .false.
      rpt_warnings = .false.
      rpt_infos    = .false.

      if (present(report_errors)) then
        if (report_errors) rpt_errors = .true.
      endif
      if (present(report_warnings)) then
        if (report_warnings) rpt_warnings = .true.
      endif
      if (present(report_infos)) then
        if (report_infos) rpt_infos = .true.
      endif

      nullify(errors)
      nullify(warnings)
      nullify(infos)
      nerrors   = 0
      nwarnings = 0
      ninfos    = 0

      call workfile_clear_deleted (to_obj)
      to_process => to_obj%last_process
      do
        if (.not. associated(to_process)) exit
        call process_get_name (to_process,name)
        if (trim(name) .eq. 'JOB_DATA') then
          call process_get_by_keyword(to_process,'PROCESS','FRONTEND_PATH',path)
        endif
        call workfile_delete_process (to_obj,to_process)
        to_process => to_obj%last_process
      enddo
          
      from_process => from_obj%first_process
      i = 1
      call process_get_name           (from_process,name)
      call process_create             (to_process,name,i)
      call process_copy               (from_process,to_process)
      if (rpt_errors)   call process_get_errors (to_process,errors,nerrors)
      if (rpt_warnings) call process_get_warnings(to_process,warnings,nwarnings)
      if (rpt_infos)    call process_get_infos (to_process,infos,ninfos)
      call workfile_set_first_process (to_obj,to_process)
!!!   call process_get_next           (from_process,from_process)
      call process_get_next           (from_process,next)
      from_process => next
      previous => to_process

      do
        if (.not. associated(from_process)) exit
        i = i + 1
        call process_get_name           (from_process,name)
        call process_create             (to_process  ,name,i)
        call process_copy_pdata_cards   (previous    ,to_process)
        call process_copy_jdata_cards   (previous    ,to_process)
        call process_copy_global_cards  (previous    ,to_process)
        call process_copy_process_cards (from_process,to_process)
        call process_copy_control_cards (from_process,to_process)

        if (trim(name) .eq. 'JOB_DATA') then
!!!       i2 = index        (to_obj%name,'.wrk',.TRUE.) - 1
          i2 = string_index (to_obj%name,'.wrk',.TRUE.) - 1
          if (i2 .le. 0) i2 = len_trim(to_obj%name)
          call process_put_process_cscalar(to_process,'JOBNAME',  &
                                           to_obj%name(1:i2))
          if (len_trim(path) .gt. 0) then
            call process_put_process_cscalar(to_process,'FRONTEND_PATH',  &
                                                                    trim(path))
          endif
        endif

        call process_create_super (to_process)
        if (rpt_errors)   call process_get_errors (to_process,errors,nerrors)
        if (rpt_warnings) call process_get_warnings (to_process,warnings, &
                                                                     nwarnings)
        if (rpt_infos)    call process_get_infos (to_process,infos,ninfos)
        call workfile_insert_process   (to_obj,to_process,previous)
!!!     call process_get_next          (from_process,from_process)
        call process_get_next          (from_process,next)
        from_process => next
        previous => to_process
      enddo
      call workfile_set_num_processes (to_obj,i)

      if (rpt_errors .and. nerrors .gt. 0) then
        call pc_put_gui ('ERROR','ERROR',errors,nerrors)
        do i=1,nerrors
          write(STDOUT,*) 'ERROR -  ',errors(i)
        enddo
      endif
      if (associated(errors)) deallocate(errors)

      if (rpt_warnings .and. nwarnings .gt. 0) then
        call pc_put_gui ('WARNING','WARNING',warnings,nwarnings)
        do i=1,nwarnings
          write(STDOUT,*) 'WARNING -  ',warnings(i)
        enddo
        deallocate(warnings)
      endif
      if (associated(warnings)) deallocate(warnings)

      if (rpt_errors .and. ninfos .gt. 0) then
        call pc_put_gui ('INFO','INFO',infos,ninfos)
        do i=1,ninfos
          write(STDOUT,*) 'INFO -  ',infos(i)
        enddo
        deallocate(infos)
      endif
      if (associated(infos)) deallocate(infos)

      return
      end subroutine workfile_copy


!!--------------------------------- read ----------------------------------!!
!!--------------------------------- read ----------------------------------!!
!!--------------------------------- read ----------------------------------!!


      subroutine workfile_read (obj,filename,report_errors,report_warnings,  &
                                report_infos)
      implicit none
      type(workfile_struct),pointer         :: obj              ! arguments
      character(len=*)     ,intent(in)      :: filename         ! arguments
      logical,optional                      :: report_errors    ! argument
      logical,optional                      :: report_warnings  ! argument
      logical,optional                      :: report_infos     ! argument

      character(len=CARDSET_LENGTH)         :: card_sav         ! local
      type(process_struct),pointer          :: process_obj      ! local
      type(cardset_struct),pointer          :: parameters       ! local
      character(len=CARDSET_LENGTH),pointer :: cards(:)         ! local
      integer                               :: ncards           ! local
      character(len=CARDSET_LENGTH),pointer :: errors(:)        ! local
      integer                               :: nerrors          ! local
      character(len=CARDSET_LENGTH),pointer :: warnings(:)      ! local
      integer                               :: nwarnings        ! local
      character(len=CARDSET_LENGTH),pointer :: infos(:)         ! local
      integer                               :: ninfos           ! local
      integer                               :: i                ! local
      integer                               :: i1               ! local
      integer                               :: i2               ! local
      integer                               :: istat            ! local
      integer                               :: lun              ! local
      character(len=132)                    :: process_name     ! local
      character(len=2)                      :: mode             ! local
      character(len=132)                    :: string           ! local
      character(len=CARDSET_LENGTH)         :: path             ! local
      integer                               :: nstring          ! local
      logical                               :: rpt_errors       ! local
      logical                               :: rpt_warnings     ! local
      logical                               :: rpt_infos        ! local

      nullify (process_obj) ! jpa
      mode = "r"
      lun = cio_fopen(trim(filename),mode)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      nstring = cio_fgetline(string,132,lun)
      call string_strip_blanks(string,i)
      call string_to_upper(string)
      if (string(1:i) .ne. '<CPS_V1TYPE="WORKFILE"/>') then
        call pc_error ('File is not a CPS Workfile')
        return
      endif

      rpt_errors   = .false.
      rpt_warnings = .false.
      rpt_infos    = .false.

      if (present(report_errors)) then
        if (report_errors) rpt_errors = .true.
      endif
      if (present(report_warnings)) then
        if (report_warnings) rpt_warnings = .true.
      endif
      if (present(report_infos)) then
        if (report_infos) rpt_infos = .true.
      endif

      nullify(cards)
      nullify(errors)
      nullify(warnings)
      nullify(infos)
      ncards    = 0
      nerrors   = 0
      nwarnings = 0
      ninfos    = 0

      nullify(parameters)
      call cardset_create             (parameters)
      call cardset_set_packing_option (parameters, CARDSET_PACKED)

!!!   i1 = index        (filename,'/',.TRUE.)
      i1 = string_index (filename,'/',.TRUE.)
      if (i1 .eq. 0) then
        i1 = 1
      else
        i1 = i1 + 1
      endif
!     i2 = index(filename,'.wrk',.TRUE.)
!     if (i2 .eq. 0 .or. i2 .lt. i1) then
!       i2 = len_trim(filename)
!     else
!       i2 = i2 - 1
!     endif
!     obj%name          = filename(i1:i2)
      obj%name          = filename(i1:)
      obj%comment       = 'No Comment'
      obj%num_processes = 0

      pc_lun = pclun_get()
      call pc_frontend_update (pc_lun)

      do
        nstring = cio_fgetline(string,132,lun)
        if (nstring .lt. 0) exit
        if (string(1:1) .eq. '<') then                       ! PROCESS card
          if (string(1:2) .eq. '</') then
            call pc_next
            call cardset_alloc_cards (parameters, cards, ncards)
            call pc_put_process_cards (cards ,ncards)
            call process_create (process_obj, process_name, obj%num_processes)
            i = pc_get_ipn()
            call process_set_ipn(process_obj, i)
            if (i .eq. 2) then                   ! JOB_DATA
              path = path_get_dir (filename)
              if (trim(path) .ne. 'NONE') then
                call pc_put ('FRONTEND_PATH',trim(path))
              endif
            endif
            call process_super_create_only (process_obj)
            if (rpt_errors)   call workfile_errors_private     &
                                               (process_obj,errors  ,nerrors)
            if (rpt_warnings) call workfile_warnings_private   &
                                               (process_obj,warnings,nwarnings)
            if (rpt_infos)    call workfile_infos_private      &
                                               (process_obj,infos   ,ninfos)
            ncards = 0
            call pc_alloc_pdata_cards      (cards, ncards)
            call process_put_pdata_cards   (process_obj, cards, ncards)
            call pc_alloc_jdata_cards      (cards, ncards)
            call process_put_jdata_cards   (process_obj, cards, ncards)
            call pc_alloc_global_cards     (cards, ncards)
            call process_put_global_cards  (process_obj, cards, ncards)
            call pc_alloc_process_cards    (cards, ncards)
            call process_put_process_cards (process_obj, cards, ncards)
            call pc_alloc_control_cards    (cards, ncards)
            call process_put_control_cards (process_obj, cards, ncards)
            call pc_alloc_gui_cards        (cards, ncards)
            call process_put_gui_cards     (process_obj, cards, ncards)
            if (.not. associated(obj%first_process)) then
              obj%first_process => process_obj
              obj%last_process  => process_obj
            else
              call process_set_next (obj%last_process,process_obj)
              call process_set_previous (process_obj,obj%last_process)
              obj%last_process => process_obj
            endif
            call cardset_clear              (parameters)
            call cardset_set_packing_option (parameters, CARDSET_PACKED)
            cycle
          endif
          call string_strip_blanks(string)
          call string_to_upper(string)
          i1 = index(string,'<PROCESSNAME="')
          if (i1 .gt. 0) then
            i1 = i1+14
            i2 = index(string(i1:),'"')
            if (i2 .gt. 0) then
              process_name  = string(i1:i1+i2-2)
            else
              process_name  = 'NONE'
            endif
          else
              process_name  = 'NONE'
          endif
          obj%num_processes = obj%num_processes + 1

        else if (string(1:2) .eq. ' <') then             ! TAG card

          card_sav = string

        else                                             ! DATA card

          if (card_sav(1:13) .eq. ' <PARAMETERS>') then
            call cardset_add_card (parameters,string)
          endif

        endif
      enddo

      call cardset_delete (parameters)
      istat = cio_fclose(lun)
      if (associated(cards)) deallocate(cards)

      call pc_get_jdata ('JOB_COMMENT', obj%comment)
      call pc_restore

      if (rpt_errors .and. nerrors .gt. 0) then
        call pc_put_gui ('ERROR','ERROR',errors,nerrors)
        do i=1,nerrors
          write(STDOUT,*) 'ERROR -  ',errors(i)
        enddo
      endif
      if (associated(errors)) deallocate(errors)

      if (rpt_warnings .and. nwarnings .gt. 0) then
        call pc_put_gui ('WARNING','WARNING',warnings,nwarnings)
        do i=1,nwarnings
          write(STDOUT,*) 'WARNING -  ',warnings(i)
        enddo
        deallocate(warnings)
      endif
      if (associated(warnings)) deallocate(warnings)

      if (rpt_errors .and. ninfos .gt. 0) then
        call pc_put_gui ('INFO','INFO',infos,ninfos)
        do i=1,ninfos
          write(STDOUT,*) 'INFO -  ',infos(i)
        enddo
        deallocate(infos)
      endif
      if (associated(infos)) deallocate(infos)

      return
      end subroutine workfile_read




!!-------------------------------- reload ---------------------------------!!
!!-------------------------------- reload ---------------------------------!!
!!-------------------------------- reload ---------------------------------!!


      subroutine workfile_reload (obj,filename,err)
      implicit none
      type(workfile_struct),pointer         :: obj              ! arguments
      character(len=*)     ,intent(in)      :: filename         ! arguments
      logical              ,intent(out)     :: err              ! arguments

      character(len=CARDSET_LENGTH)         :: card_sav         ! local
      type(process_struct),pointer          :: process_obj      ! local
      type(process_struct),pointer          :: next             ! local
      type(cardset_struct),pointer          :: parameters       ! local
      character(len=CARDSET_LENGTH),pointer :: cards(:)         ! local
      integer                               :: ncards           ! local
      integer                               :: i                ! local
      integer                               :: i1               ! local
      integer                               :: i2               ! local
      integer                               :: istat            ! local
      integer                               :: lun              ! local
      character(len=132)                    :: process_name     ! local
      character(len=132)                    :: ctemp            ! local
      character(len=2)                      :: mode             ! local
      character(len=132)                    :: string           ! local
      integer                               :: nstring          ! local

      nullify (next) ! jpa
      nullify (cards) ! jpa
      nullify (process_obj) ! jpa
      err = .false.

      mode = "r"
      lun = cio_fopen(trim(filename),mode)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        err = .true.
        return
      endif

      nstring = cio_fgetline(string,132,lun)
      call string_strip_blanks(string,i)
      call string_to_upper(string)
      if (string(1:i) .ne. '<CPS_V1TYPE="WORKFILE"/>') then
        call pc_error ('File is not a CPS Workfile')
        err = .true.
        return
      endif

      nullify(parameters)
      call cardset_create             (parameters)
      call cardset_set_packing_option (parameters, CARDSET_PACKED)

!!!   i = index        (filename,'/',.TRUE.)
      i = string_index (filename,'/',.TRUE.)
      if (i .eq. 0) then
        i = 1
      else
        i = i + 1
      endif
      obj%name          = filename(i:)
      obj%comment       = 'No Comment'
      obj%num_processes = 0

      call workfile_get_first_process (obj,process_obj)
      pc_lun = pclun_get()
      call pc_frontend_update (pc_lun)

      do
        nstring = cio_fgetline(string,132,lun)
        if (nstring .lt. 0) exit
        if (string(1:1) .eq. '<') then                       ! PROCESS card
          if (string(1:2) .eq. '</') then
            call pc_next
            call cardset_alloc_cards (parameters, cards, ncards)
            call pc_put_process_cards (cards ,ncards)
            if (.not. associated(process_obj)) then
              write(STDOUT,*) 'Ran out of processes before reaching end of file'
              err = .true.
              exit
            endif
            call process_get_name (process_obj,ctemp)
            if (trim(process_name) .ne. trim(ctemp)) then
              write(STDOUT,*) 'Process name did not match'
              err = .true.
              exit
            endif
            call pc_alloc_pdata_cards      (cards, ncards)
            call process_put_pdata_cards   (process_obj, cards, ncards)
            call pc_alloc_jdata_cards      (cards, ncards)
            call process_put_jdata_cards   (process_obj, cards, ncards)
            call pc_alloc_global_cards     (cards, ncards)
            call process_put_global_cards  (process_obj, cards, ncards)
            call pc_alloc_process_cards    (cards, ncards)
            call process_put_process_cards (process_obj, cards, ncards)
            call pc_alloc_control_cards    (cards, ncards)
            call process_put_control_cards (process_obj, cards, ncards)
            call pc_alloc_gui_cards        (cards, ncards)
            call process_put_gui_cards     (process_obj, cards, ncards)
!!!         call process_get_next          (process_obj,process_obj)
            call process_get_next          (process_obj,next)
            process_obj => next
            cycle
          endif
          call string_strip_blanks(string)
          call string_to_upper(string)
          i1 = index(string,'<PROCESSNAME="')
          if (i1 .gt. 0) then
            i1 = i1+14
            i2 = index(string(i1:),'"')
            if (i2 .gt. 0) then
              process_name  = string(i1:i1+i2-2)
            else
              process_name  = 'NONE'
            endif
          else
              process_name  = 'NONE'
          endif
          obj%num_processes = obj%num_processes + 1

        else if (string(1:2) .eq. ' <') then             ! TAG card

          if (string(1:13) .eq. ' <PARAMETERS>') then
            call cardset_clear              (parameters)
            call cardset_set_packing_option (parameters, CARDSET_PACKED)
          endif
          card_sav = string

        else                                             ! DATA card

          if (card_sav(1:13) .eq. ' <PARAMETERS>') then
            call cardset_add_card (parameters,string)
          endif

        endif
      enddo

      call cardset_delete (parameters)
      istat = cio_fclose(lun)

      call pc_restore

      return
      end subroutine workfile_reload


!!-------------------------------- write ----------------------------------!!
!!-------------------------------- write ----------------------------------!!
!!-------------------------------- write ----------------------------------!!


      subroutine workfile_write (obj,filename)
      implicit none
      type(workfile_struct),pointer         :: obj              ! arguments
      character(len=*)                      :: filename         ! arguments

      type(process_struct),pointer          :: pcurrent,next    ! local

      character(len=CARDSET_LENGTH),pointer :: cards(:)         ! local
      integer                               :: ncards           ! local
      integer                               :: i                ! local
      integer                               :: istat            ! local
      integer                               :: lun              ! local
      character(len=132)                    :: process_name     ! local

      character(len=2)                      :: mode             ! local
      character(len=PC_DATACARD_LENGTH)     :: string           ! local
      integer                               :: nstring,temp     ! local

      nullify (next) ! jpa
      nullify(cards)

      mode = "w"
      lun = cio_fopen(filename,mode, &
                      file_space_commit=PREALLOCATE_FILE_SPACE_DISABLED, &
                      file_lock=FILE_LOCK_DISABLED)

      string  =  '<CPS_v1 type="WORKFILE"/>'
      temp = len_trim(string)
      nstring = cio_fputline(string,temp,lun)

      pcurrent => obj%first_process
      do
        if (.not. associated(pcurrent)) exit
        call process_get_name (pcurrent, process_name)
        string  = ' '
        temp = len_trim(string)
        nstring = cio_fputline(string,temp,lun)
        string  = '<PROCESS name="' // trim(process_name) // '">'
        temp = len_trim(string)
        nstring = cio_fputline(string,temp,lun)
        call process_alloc_global_cards (pcurrent , cards, ncards)
        if (ncards .gt. 0) then
          string  = ' <GLOBALS>'
          temp = len_trim(string)
          nstring = cio_fputline(string,temp,lun)
          do i=1,ncards
            string  = '  ' // cards(i)
            temp = len_trim(string)
            nstring = cio_fputline(string,temp,lun)
          enddo
          string  = ' </GLOBALS>'
          temp = len_trim(string)
          nstring = cio_fputline(string,temp,lun)
        endif
        call process_alloc_control_cards (pcurrent, cards, ncards)
        if (ncards .gt. 0) then
          string  = ' <CONTROLS>'
          temp = len_trim(string)
          nstring = cio_fputline(string,temp,lun)
          do i=1,ncards
            string  = '  ' // cards(i)
            temp = len_trim(string)
            nstring = cio_fputline(string,temp,lun)
          enddo
          string  = ' </CONTROLS>'
          temp = len_trim(string)
          nstring = cio_fputline(string,temp,lun)
        endif
        call process_alloc_process_cards (pcurrent, cards, ncards)
        if (ncards .gt. 0) then
          string  = ' <PARAMETERS>'
          temp = len_trim(string)
          nstring = cio_fputline(string,temp,lun)
          do i=1,ncards
            string  = '  ' // cards(i)
            temp = len_trim(string)
            nstring = cio_fputline(string,temp,lun)
          enddo
          string  = ' </PARAMETERS>'
          temp = len_trim(string)
          nstring = cio_fputline(string,temp,lun)
        endif
        string  = '</PROCESS>'
        temp = len_trim(string)
        nstring = cio_fputline(string,temp,lun)
!!!     call process_get_next (pcurrent,pcurrent)
        call process_get_next (pcurrent,next)
        pcurrent => next
      enddo

      istat = cio_fclose(lun)
      if (associated(cards)) deallocate(cards)

      return
      end subroutine workfile_write


!!--------------------------- delete_process ------------------------------!!
!!--------------------------- delete_process ------------------------------!!
!!--------------------------- delete_process ------------------------------!!


      subroutine workfile_delete_process (workfile_obj,process_obj)
      implicit none
      type(workfile_struct),pointer                  :: workfile_obj  ! argument
      type(process_struct),pointer                   :: process_obj   ! argument

      type(process_struct),pointer                   :: previous      ! local
      type(process_struct),pointer                   :: next          ! local
      type(process_struct),pointer                   :: mate          ! local

      nullify(previous)
      nullify(next)
      nullify(mate)

      call process_get_previous (process_obj,previous)
      call process_get_next     (process_obj,next)
      call process_get_mate     (next       ,mate)

      call process_set_next     (previous,next)
      call process_set_previous (next    ,previous)
      call process_set_previous (mate    ,previous)

      if (.not. associated(previous)) then
        if (associated(next)) then
          workfile_obj%first_process =>  next
        else
          nullify(workfile_obj%first_process)
          nullify(workfile_obj%last_process)
        endif
      else if (.not. associated(next)) then
        if (associated(previous)) then
          workfile_obj%last_process => previous
        else
          nullify(workfile_obj%last_process)
        endif
      endif

      if (workfile_obj%save_deleted) then
        call process_clear_previous (process_obj)
        call process_clear_next     (process_obj)
        if (associated(workfile_obj%deleted)) then
          call process_set_next     (workfile_obj%deleted,process_obj)
          call process_set_previous (process_obj,workfile_obj%deleted)
        endif
        workfile_obj%deleted => process_obj
      else
        call process_delete (process_obj)
      endif

      workfile_obj%num_processes = workfile_obj%num_processes - 1

      return
      end subroutine workfile_delete_process


!!--------------------------- replace_process -----------------------------!!
!!--------------------------- replace_process -----------------------------!!
!!--------------------------- replace_process -----------------------------!!


      subroutine workfile_replace_process (workfile_obj,pnew_obj,pold_obj)
      implicit none
      type(workfile_struct),pointer                  :: workfile_obj  ! argument
      type(process_struct),pointer                   :: pnew_obj      ! argument
      type(process_struct),pointer                   :: pold_obj      ! argument

      type(process_struct),pointer                   :: previous      ! local
      type(process_struct),pointer                   :: next          ! local

      nullify(previous)
      nullify(next)

      call process_get_previous (pold_obj,previous)
      call process_get_next     (pold_obj,next)

      call process_set_previous (pnew_obj,previous)
      call process_set_next     (pnew_obj,next)

      call process_set_next     (previous,pnew_obj)
      call process_set_previous (next    ,pnew_obj)

      if (.not. associated(previous)) then
        workfile_obj%first_process => pnew_obj
      endif

      if (.not. associated(next)) then
        workfile_obj%last_process => pnew_obj
      endif

      if (workfile_obj%save_deleted) then
        call process_clear_previous (pold_obj)
        call process_clear_next     (pold_obj)
        if (associated(workfile_obj%deleted)) then
          call process_set_next     (workfile_obj%deleted,pold_obj)
          call process_set_previous (pold_obj,workfile_obj%deleted)
        endif
        workfile_obj%deleted => pold_obj
      else
        call process_delete (pold_obj)
      endif

      return
      end subroutine workfile_replace_process


!!---------------------------- insert_process -----------------------------!!
!!---------------------------- insert_process -----------------------------!!
!!---------------------------- insert_process -----------------------------!!


      subroutine workfile_insert_process (workfile_obj,pnew_obj,pafter_obj)
      implicit none
      type(workfile_struct),pointer                  :: workfile_obj  ! argument
      type(process_struct),pointer                   :: pnew_obj      ! argument
      type(process_struct),pointer                   :: pafter_obj    ! argument

      type(process_struct),pointer                   :: next          ! local

      nullify(next)
      call process_get_next     (pafter_obj,next)

      call process_set_next     (pafter_obj,pnew_obj)
      call process_set_previous (pnew_obj,pafter_obj)
      call process_set_next     (pnew_obj,next)
      call process_set_previous (next,pnew_obj)

      if (.not. associated(next)) workfile_obj%last_process => pnew_obj
      workfile_obj%num_processes = workfile_obj%num_processes + 1

      return
      end subroutine workfile_insert_process


!!------------------------- insert_first_process --------------------------!!
!!------------------------- insert_first_process --------------------------!!
!!------------------------- insert_first_process --------------------------!!


      subroutine workfile_insert_first_process (obj,pnew_obj)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      type(process_struct),pointer                   :: pnew_obj      ! argument

      type(process_struct),pointer                   :: old_first     ! local

      nullify (old_first) ! jpa
      if (.not. associated(obj)) return

      call workfile_get_first_process (obj,old_first)
      if (associated(old_first)) then
        call process_set_previous (old_first,pnew_obj )
        call process_set_next     (pnew_obj ,old_first)
      else
        call process_clear_previous   (pnew_obj)
        call process_clear_next       (pnew_obj)
      endif
      call workfile_set_first_process (obj,pnew_obj)
      obj%num_processes = obj%num_processes + 1

      return
      end subroutine workfile_insert_first_process


!!-------------------------- set_first_process ----------------------------!!
!!-------------------------- set_first_process ----------------------------!!
!!-------------------------- set_first_process ----------------------------!!


      subroutine workfile_set_first_process (obj,first_process)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      type(process_struct),pointer                   :: first_process ! argument

      if (.not. associated(obj)) return

      if (associated(first_process)) then
        obj%first_process => first_process
      else
        nullify(obj%first_process)
      endif
 
      return
      end subroutine workfile_set_first_process


!!--------------------------- set_last_process ----------------------------!!
!!--------------------------- set_last_process ----------------------------!!
!!--------------------------- set_last_process ----------------------------!!


      subroutine workfile_set_last_process (obj,last_process)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      type(process_struct),pointer                   :: last_process  ! argument

      if (.not. associated(obj)) return

      if (associated(last_process)) then
        obj%last_process => last_process
      else
        nullify(obj%last_process)
      endif
 
      return
      end subroutine workfile_set_last_process


!!--------------------------- set_num_processes ---------------------------!!
!!--------------------------- set_num_processes ---------------------------!!
!!--------------------------- set_num_processes ---------------------------!!


      subroutine workfile_set_num_processes (obj,num_processes)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      integer,intent(in)                             :: num_processes ! argument

      if (.not. associated(obj)) return

      obj%num_processes = num_processes
 
      return
      end subroutine workfile_set_num_processes


!!------------------------------- set_name --------------------------------!!
!!------------------------------- set_name --------------------------------!!
!!------------------------------- set_name --------------------------------!!


      subroutine workfile_set_name (obj,name)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      character(len=*),intent(in)                    :: name          ! argument

      if (.not. associated(obj)) return

      obj%name = name
 
      return
      end subroutine workfile_set_name


!!------------------------------ set_comment ------------------------------!!
!!------------------------------ set_comment ------------------------------!!
!!------------------------------ set_comment ------------------------------!!


      subroutine workfile_set_comment (obj,comment)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      character(len=*),intent(in)                    :: comment       ! argument

      if (.not. associated(obj)) return

      obj%comment = comment
 
      return
      end subroutine workfile_set_comment


!!----------------------------- set_previous ------------------------------!!
!!----------------------------- set_previous ------------------------------!!
!!----------------------------- set_previous ------------------------------!!


      subroutine workfile_set_previous (obj,previous)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      type(workfile_struct),pointer                  :: previous      ! argument

      if (.not. associated(obj)) return

      if (associated(previous)) then
        obj%previous => previous
      else
        nullify(obj%previous)
      endif
 
      return
      end subroutine workfile_set_previous


!!------------------------------- set_next --------------------------------!!
!!------------------------------- set_next --------------------------------!!
!!------------------------------- set_next --------------------------------!!


      subroutine workfile_set_next (obj,next)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      type(workfile_struct),pointer                  :: next          ! argument

      if (.not. associated(obj)) return

      if (associated(next)) then
        obj%next => next
      else
        nullify(obj%next)
      endif
 
      return
      end subroutine workfile_set_next


!!-------------------------- get_first_process ----------------------------!!
!!-------------------------- get_first_process ----------------------------!!
!!-------------------------- get_first_process ----------------------------!!


      subroutine workfile_get_first_process (obj,first_process)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      type(process_struct),pointer                   :: first_process ! argument

      if (associated(obj)) then
        if (associated(obj%first_process)) then
          first_process => obj%first_process
        else
          nullify(first_process)
        endif
      else
        nullify(first_process)
      endif
 
      return
      end subroutine workfile_get_first_process


!!--------------------------- get_last_process ----------------------------!!
!!--------------------------- get_last_process ----------------------------!!
!!--------------------------- get_last_process ----------------------------!!


      subroutine workfile_get_last_process (obj,last_process)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      type(process_struct),pointer                   :: last_process  ! argument

      if (associated(obj)) then
        if (associated(obj%last_process)) then
          last_process => obj%last_process
        else
          nullify(last_process)
        endif
      else
        nullify(last_process)
      endif
 
      return
      end subroutine workfile_get_last_process


!!------------------------------ get_deleted ------------------------------!!
!!------------------------------ get_deleted ------------------------------!!
!!------------------------------ get_deleted ------------------------------!!


      subroutine workfile_get_deleted (obj,deleted)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      type(process_struct),pointer                   :: deleted       ! argument

      if (associated(obj)) then
        if (associated(obj%deleted)) then
          deleted => obj%deleted
        else
          nullify(deleted)
        endif
      else
        nullify(deleted)
      endif
 
      return
      end subroutine workfile_get_deleted


!!--------------------------- get_num_processes ---------------------------!!
!!--------------------------- get_num_processes ---------------------------!!
!!--------------------------- get_num_processes ---------------------------!!


      subroutine workfile_get_num_processes (obj,num_processes)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      integer,intent(out)                            :: num_processes ! argument

      if (associated(obj)) then
        num_processes = obj%num_processes
      else
        num_processes = 0
      endif
 
      return
      end subroutine workfile_get_num_processes


!!------------------------------- get_name --------------------------------!!
!!------------------------------- get_name --------------------------------!!
!!------------------------------- get_name --------------------------------!!


      subroutine workfile_get_name (obj,name)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      character(len=*),intent(out)                   :: name          ! argument

      if (associated(obj)) then
        name = obj%name
      else
        name = ''
      endif
 
      return
      end subroutine workfile_get_name


!!------------------------------ get_comment ------------------------------!!
!!------------------------------ get_comment ------------------------------!!
!!------------------------------ get_comment ------------------------------!!


      subroutine workfile_get_comment (obj,comment)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      character(len=*),intent(out)                   :: comment       ! argument

      if (associated(obj)) then
        comment = obj%comment
      else
        comment = ''
      endif
 
      return
      end subroutine workfile_get_comment


!!----------------------------- get_previous ------------------------------!!
!!----------------------------- get_previous ------------------------------!!
!!----------------------------- get_previous ------------------------------!!


      subroutine workfile_get_previous (obj,previous)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      type(workfile_struct),pointer                  :: previous      ! argument

      if (associated(obj)) then
        if (associated(obj%previous)) then
          previous => obj%previous
        else
          nullify(previous)
        endif
      else
        nullify(previous)
      endif
 
      return
      end subroutine workfile_get_previous


!!------------------------------- get_next --------------------------------!!
!!------------------------------- get_next --------------------------------!!
!!------------------------------- get_next --------------------------------!!


      subroutine workfile_get_next (obj,next)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument
      type(workfile_struct),pointer                  :: next          ! argument

      if (associated(obj)) then
        if (associated(obj%next)) then
          next => obj%next
        else
          nullify(next)
        endif
      else
        nullify(next)
      endif
 
      return
      end subroutine workfile_get_next


!!---------------------------- reset_deleted ------------------------------!!
!!---------------------------- reset_deleted ------------------------------!!
!!---------------------------- reset_deleted ------------------------------!!


      subroutine workfile_reset_deleted (obj)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument

      if (associated(obj)) then
        nullify(obj%deleted)
      endif
 
 
      return
      end subroutine workfile_reset_deleted


!!---------------------------- clear_deleted ------------------------------!!
!!---------------------------- clear_deleted ------------------------------!!
!!---------------------------- clear_deleted ------------------------------!!


      subroutine workfile_clear_deleted (obj)
      implicit none
      type(workfile_struct),pointer                  :: obj           ! argument

      type(process_struct),pointer                   :: previous      ! local

      nullify (previous) ! jpa
      if (associated(obj)) then
        do
          if (.not. associated(obj%deleted)) exit
          call process_get_previous (obj%deleted,previous)
          call process_delete (obj%deleted)
          obj%deleted => previous
        enddo
      endif
 
      return
      end subroutine workfile_clear_deleted


!!---------------------------- process_list -------------------------------!!
!!---------------------------- process_list -------------------------------!!
!!---------------------------- process_list -------------------------------!!


      subroutine workfile_process_list (obj,list,nlist)
      implicit none
      type(workfile_struct),pointer                     :: obj       ! argument
      character(len=*),pointer                          :: list(:)   ! argument
      integer                                           :: nlist     ! argument

      type(process_struct),pointer                      :: pcurrent  ! local
      type(process_struct),pointer                      :: next      ! local
      integer                                           :: i         ! local

      nullify (next) ! jpa
      if (associated(list)) deallocate(list)
      nlist = obj%num_processes
      allocate(list(nlist))

      pcurrent => obj%first_process
      do i = 1, obj%num_processes
        if (.not. associated(pcurrent)) exit
        call process_get_name (pcurrent, list(i))
!!!     call process_get_next (pcurrent,pcurrent)
        call process_get_next (pcurrent,next)
        pcurrent => next
      enddo

      return
      end subroutine workfile_process_list


!!---------------------------- run_all_traps ------------------------------!!
!!---------------------------- run_all_traps ------------------------------!!
!!---------------------------- run_all_traps ------------------------------!!


      subroutine workfile_run_all_traps (obj,start,from_cards,frontend,  &
                                         report_errors,report_warnings,  &
                                         report_infos,workfile_indx)
      implicit none
      type(workfile_struct),pointer                 :: obj            ! argument
      type(process_struct),pointer,optional         :: start          ! argument
      logical,optional                              :: from_cards     ! argument
      logical,optional                              :: frontend       ! argument
      logical,optional                              :: report_errors  ! argument
      logical,optional                              :: report_warnings! argument
      logical,optional                              :: report_infos   ! argument
      integer,optional                              :: workfile_indx  ! argument

      character(len=CARDSET_LENGTH),pointer         :: cards(:)       ! local
      integer                                       :: ncards         ! local
      character(len=CARDSET_LENGTH),pointer         :: errors(:)      ! local
      integer                                       :: nerrors        ! local
      character(len=CARDSET_LENGTH),pointer         :: warnings(:)    ! local
      integer                                       :: nwarnings      ! local
      character(len=CARDSET_LENGTH),pointer         :: infos(:)       ! local
      integer                                       :: ninfos         ! local


      character(len=CARDSET_LENGTH)                 :: process_name   ! local
      integer                                       :: ipn            ! local
      character(len=10)                             :: cindx          ! local
      type(process_struct),pointer                  :: pcurrent       ! local
      type(process_struct),pointer                  :: previous       ! local
      type(process_struct),pointer                  :: next           ! local
      integer                                       :: i              ! local
      logical                                       :: rpt_errors     ! local
      logical                                       :: rpt_warnings   ! local
      logical                                       :: rpt_infos      ! local
      logical                                       :: rpt_workfile   ! local

      nullify(cards)
      nullify(errors)
      nullify(warnings)
      nullify(infos)
      nullify (next) ! jpa
      nullify (previous) ! jpa
      nerrors      = 0
      nwarnings    = 0
      ninfos       = 0
      rpt_errors   = .false.
      rpt_warnings = .false.
      rpt_infos    = .false.
      rpt_workfile = .false.

      if (present(report_errors)) then
        if (report_errors) rpt_errors = .true.
      endif
      if (present(report_warnings)) then
        if (report_warnings) rpt_warnings = .true.
      endif
      if (present(report_infos)) then
        if (report_infos) rpt_infos = .true.
      endif
      if (present(workfile_indx)) then
        rpt_workfile = .true.
        call string_ii2cc (workfile_indx,cindx)
      endif

      pc_lun = pclun_get()
      if (present(frontend)) then
        if (frontend) then
          call pc_frontend_update (pc_lun)
        else
          call pc_quick_update (pc_lun)
        endif
      else
        call pc_quick_update (pc_lun)
      endif

      if (present(start)) then
        pcurrent => start
      else
        pcurrent => obj%first_process
      endif

      process_name = ' '
      ipn          = 0
      call process_get_name (pcurrent,process_name)
      call process_get_ipn  (pcurrent,ipn)

      call process_get_previous (pcurrent,previous)
      if (associated(previous)) then
        ncards = 0
        call process_alloc_pdata_cards (previous, cards, ncards)
        if (ncards .gt. 0) then
          call pc_set_ipn (1)
          call pc_put_pdata_cards (cards, ncards)
        endif
        ncards = 0
        call process_alloc_jdata_cards (previous, cards, ncards)
        if (ncards .gt. 0) then
          call pc_set_ipn (2)
          call pc_put_jdata_cards (cards, ncards)
        endif
        ncards = 0
        call process_alloc_global_cards (previous, cards, ncards)
        if (ncards .gt. 0) then
          call process_get_ipn (previous,ipn)
           call pc_set_ipn (ipn)
          call pc_put_global_cards (cards, ncards)
        endif
      endif

      do
        if (.not. associated(pcurrent)) exit
        call pc_next
        if (present(from_cards)) then
          if (from_cards) then
            call process_alloc_process_cards (pcurrent ,cards ,ncards)
            call pc_put_process_cards        (cards ,ncards)
          endif
        endif
        call process_get_ipn (pcurrent,ipn)
        call pc_set_ipn (ipn)
        call process_super_update_only (pcurrent)
        if (rpt_errors)   call workfile_errors_private     &
                                               (pcurrent,errors  ,nerrors)
        if (rpt_warnings) call workfile_warnings_private   &
                                               (pcurrent,warnings,nwarnings)
        if (rpt_infos)    call workfile_infos_private      &
                                               (pcurrent,infos   ,ninfos)
        ncards = 0
        call pc_alloc_pdata_cards      (cards, ncards)
        call process_put_pdata_cards   (pcurrent, cards, ncards)
        call pc_alloc_jdata_cards      (cards, ncards)
        call process_put_jdata_cards   (pcurrent, cards, ncards)
        call pc_alloc_global_cards     (cards, ncards)
        call process_put_global_cards  (pcurrent, cards, ncards)
        call pc_alloc_process_cards    (cards, ncards)
        call process_put_process_cards (pcurrent, cards, ncards)
        call pc_alloc_control_cards    (cards, ncards)
        call process_put_control_cards (pcurrent, cards, ncards)
        call pc_alloc_gui_cards        (cards, ncards)
        call process_put_gui_cards     (pcurrent, cards, ncards)
!!!     call process_get_next (pcurrent,pcurrent)
        call process_get_next (pcurrent,next)
        pcurrent => next
      enddo
      if (associated(cards)) deallocate(cards)

      call pc_restore


      if (rpt_errors .and. nerrors .gt. 0) then
!       call pc_put_gui ('ERROR','ERROR',errors,nerrors)
        if (rpt_workfile) then
          write(STDOUT,*) '---------- Workfile #'//trim(cindx)//' ----------'
          call pc_error ('---------- Workfile #'//trim(cindx)//' ----------')
        endif
        do i=1,nerrors
          write(STDOUT,*) 'ERROR -  ',errors(i)
          call pc_error (errors(i))
        enddo
      endif
      if (associated(errors)) deallocate(errors)

      if (rpt_warnings .and. nwarnings .gt. 0) then
!       call pc_put_gui ('WARNING','WARNING',warnings,nwarnings)
        if (rpt_workfile) then
          write(STDOUT,*) '---------- Workfile #'//trim(cindx)//' ----------'
          call pc_error ('---------- Workfile '//trim(cindx)//' ----------')
        endif
        do i=1,nwarnings
          write(STDOUT,*) 'WARNING -  ',warnings(i)
          call pc_warning (warnings(i))
        enddo
        deallocate(warnings)
      endif
      if (associated(warnings)) deallocate(warnings)

      if (rpt_errors .and. ninfos .gt. 0) then
!       call pc_put_gui ('INFO','INFO',infos,ninfos)
        if (rpt_workfile) then
          write(STDOUT,*) '---------- Workfile #'//trim(cindx)//' ----------'
          call pc_error ('---------- Workfile '//trim(cindx)//' ----------')
        endif
        do i=1,ninfos
          write(STDOUT,*) 'INFO -  ',infos(i)
          call pc_info (infos(i))
        enddo
        deallocate(infos)
      endif
      if (associated(infos)) deallocate(infos)

      return
      end subroutine workfile_run_all_traps


!!--------------------------- errors_private ------------------------------!!
!!--------------------------- errors_private ------------------------------!!
!!--------------------------- errors_private ------------------------------!!


      subroutine workfile_errors_private (obj,errors,nerrors)
      implicit none
      type(process_struct),pointer                  :: obj            ! argument
      character(len=PC_LENGTH),pointer              :: errors(:)      ! argument
      integer                                       :: nerrors        ! argument

      character(len=PC_LENGTH),pointer              :: cards(:)       ! local
      integer                                       :: ncards         ! local
      character(len=CARDSET_LENGTH),allocatable     :: temp_cards(:)  ! local

      character(len=CARDSET_LENGTH)                 :: process_name   ! local
      integer                                       :: ipn            ! local
      character(len=10)                             :: cipn           ! local

      nullify(cards)
      ncards = 0

      call pc_alloc_gui ('ERROR','ERROR',cards,ncards)
      if (ncards .gt. 0) then
        if (nerrors .eq. 0) then
          if (associated(errors)) deallocate(errors)
          nerrors = ncards + 1
          allocate(errors(nerrors))
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          errors(1) = '---- Process '//trim(cipn)//'  '//  &
                      trim(process_name)//'----'
          errors(2:nerrors) = cards
        else
          allocate(temp_cards(nerrors))
          temp_cards = errors
          deallocate(errors)
          allocate(errors(nerrors+ncards+1))
          errors(1:nerrors) = temp_cards(1:nerrors)
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          errors(nerrors+1) = '---- Process '//trim(cipn)//'  '//  &
                              trim(process_name)//'----'
          errors(nerrors+2:nerrors+ncards+1) = cards
          nerrors = nerrors + ncards + 1
          deallocate(temp_cards)
        endif
      endif

      if (associated(cards)) deallocate(cards)

      return
      end subroutine workfile_errors_private


!!-------------------------- warnings_private -----------------------------!!
!!-------------------------- warnings_private -----------------------------!!
!!-------------------------- warnings_private -----------------------------!!


      subroutine workfile_warnings_private (obj,warnings,nwarnings)
      implicit none
      type(process_struct),pointer                  :: obj            ! argument
      character(len=PC_LENGTH),pointer              :: warnings(:)    ! argument
      integer                                       :: nwarnings      ! argument

      character(len=PC_LENGTH),pointer              :: cards(:)       ! local
      integer                                       :: ncards         ! local
      character(len=CARDSET_LENGTH),allocatable     :: temp_cards(:)  ! local

      character(len=CARDSET_LENGTH)                 :: process_name   ! local
      integer                                       :: ipn            ! local
      character(len=10)                             :: cipn           ! local

      nullify(cards)
      ncards = 0

      call pc_alloc_gui ('WARNING','WARNING',cards,ncards)
      if (ncards .gt. 0) then
        if (nwarnings .eq. 0) then
          if (associated(warnings)) deallocate(warnings)
          nwarnings = ncards + 1
          allocate(warnings(nwarnings))
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          warnings(1) = '---- Process '//trim(cipn)//'  '//  &
                      trim(process_name)//'----'
          warnings(2:nwarnings) = cards
        else
          allocate(temp_cards(nwarnings))
          temp_cards = warnings
          deallocate(warnings)
          allocate(warnings(nwarnings+ncards+1))
          warnings(1:nwarnings) = temp_cards(1:nwarnings)
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          warnings(nwarnings+1) = '---- Process '//trim(cipn)//'  '//  &
                              trim(process_name)//'----'
          warnings(nwarnings+2:nwarnings+ncards+1) = cards
          nwarnings = nwarnings + ncards + 1
          deallocate(temp_cards)
        endif
      endif

      if (associated(cards)) deallocate(cards)

      return
      end subroutine workfile_warnings_private


!!---------------------------- infos_private ------------------------------!!
!!---------------------------- infos_private ------------------------------!!
!!---------------------------- infos_private ------------------------------!!


      subroutine workfile_infos_private (obj,infos,ninfos)
      implicit none
      type(process_struct),pointer                  :: obj            ! argument
      character(len=PC_LENGTH),pointer              :: infos(:)       ! argument
      integer                                       :: ninfos         ! argument

      character(len=PC_LENGTH),pointer              :: cards(:)       ! local
      integer                                       :: ncards         ! local
      character(len=CARDSET_LENGTH),allocatable     :: temp_cards(:)  ! local

      character(len=CARDSET_LENGTH)                 :: process_name   ! local
      integer                                       :: ipn            ! local
      character(len=10)                             :: cipn           ! local

      nullify(cards)
      ncards = 0

      call pc_alloc_gui ('INFO','INFO',cards,ncards)
      if (ncards .gt. 0) then
        if (ninfos .eq. 0) then
          if (associated(infos)) deallocate(infos)
          ninfos = ncards + 1
          allocate(infos(ninfos))
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          infos(1) = '---- Process '//trim(cipn)//'  '//  &
                      trim(process_name)//'----'
          infos(2:ninfos) = cards
        else
          allocate(temp_cards(ninfos))
          temp_cards = infos
          deallocate(infos)
          allocate(infos(ninfos+ncards+1))
          infos(1:ninfos) = temp_cards(1:ninfos)
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          infos(ninfos+1) = '---- Process '//trim(cipn)//'  '//  &
                              trim(process_name)//'----'
          infos(ninfos+2:ninfos+ncards+1) = cards
          ninfos = ninfos + ncards + 1
          deallocate(temp_cards)
        endif
      endif

      if (associated(cards)) deallocate(cards)

      return
      end subroutine workfile_infos_private


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module workfile_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

