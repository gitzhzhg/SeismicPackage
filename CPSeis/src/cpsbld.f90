!<CPS_v1 type="AUXILIARY_FILE"/>
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
!                        C P S  P R O G R A M   F I L E
!
! Name       : cpsbld
! Category   : stand-alone
! Written    : 2007-11-08   by: Karen Goodger
! Revised    : 2008-01-16   by: Goodger
! Maturity   : beta
! Purpose    : Subroutine that does the work of building the jobfile
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  5. 2008-01-16  Goodger      Fix cnumCpus.  It was using num_nodes.
!  4. 2008-01-14  Goodger      Utilize spearate templates for B queue
!                              single cpu and A queue.
!                              Remove ppn=2 if S queue.
!  3. 2008-01-04  Goodger      Add ability to handle custom code with 
!                              additional templates.  Custom links and
!                              custom modules NOT handled.
!  2. 2007-11-13  Goodger      Move valid platform check to cpsbld_main.c.
!  1. 2007-11-09  Goodger      Replacement for buildjob.f90. This program
!                              makes it much easier to add new platforms by
!                              utilizing templates. The jcl portion has been
!                              moved to templates, and the portion that
!                              builds the main program is taken from 
!                              buildjob.f90.
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


      module cpsbld_module

      use cardset_module
      use cio_module
      use cnfg_module
      use pc_module
      use project_data_module
      use getsys_module
      use putsys_module
      use string_module
      use path_module

      implicit none

      private
      public :: buildjob

      integer, parameter :: STDOUT               = 6
      integer, parameter :: NUM_TAGS             = 4
      integer, parameter :: TAG_MAX_LEN          = 20
      integer, parameter :: PNAME_MAX            = 12
      integer, parameter :: GLOBALS_TAG          = 1
      integer, parameter :: CONTROLS_TAG         = 2
      integer, parameter :: PARAMETERS_TAG       = 3
      integer, parameter :: DATA_CARD_TAG        = 4
      integer, parameter :: MAX_IO_PAIRS         = 100
      integer, parameter :: PCPSNAME_MAX         = 30

      type CARD
        character(len=PC_LENGTH) :: data
        type(CARD),pointer       :: next
      end type CARD

      type CARD_POINTER
        type(CARD),pointer :: card
      end type CARD_POINTER

      type PROCESS
        character(len=PNAME_MAX)               :: name
        integer                                :: nname
        integer                                :: pid
        character(len=4)                       :: cpid
        integer                                :: ncpid
        character(len=4)                       :: parallel_safe
        character(len=4)                       :: need_request
        character(len=4)                       :: need_label
        character(len=4)                       :: twosets
        character(len=4)                       :: setup_only
        character(len=4)                       :: requires_landmark
        character(len=4)                       :: signal_handler
        character(len=PCPSNAME_MAX)            :: pcps_send_mode
        character(len=PCPSNAME_MAX)            :: pcps_receive_mode
        character(len=PCPSNAME_MAX)            :: pcps_send_eof_mode
        character(len=PCPSNAME_MAX)            :: pcps_alt_send_mode
        character(len=PCPSNAME_MAX)            :: pcps_alt_receive_mode
        character(len=PCPSNAME_MAX)            :: pcps_resequence_mode
        character(len=PCPSNAME_MAX)            :: pcps_generator_mode
        character(len=PCPSNAME_MAX)            :: pcps_bunch_mode
        character(len=PCPSNAME_MAX)            :: pcps_boss_exec_mode
        character(len=PCPSNAME_MAX)            :: pcps_process_tasks
        integer                                :: ltr
        integer                                :: lhd
        integer                                :: ltr2
        integer                                :: lhd2
        integer                                :: ntr
        integer                                :: nscratch
        integer                                :: nstore
        integer                                :: ndisk
        integer                                :: ntapes
        character(len=8)                       :: tmedia
        integer                                :: n_io_pairs
        character(len=4)                       :: iftd
        logical                                :: start_loop
        logical                                :: start_fork
        integer                                :: parallel_group
        integer                                :: label
        integer                                :: request
        integer                                :: parallel_begin
        integer                                :: parallel_end
        type(CARD_POINTER)                     :: tag_card_start(NUM_TAGS)
        type(CARD_POINTER)                     :: tag_card_last(NUM_TAGS)
        type(PROCESS),pointer                  :: next
      end type PROCESS

      type PGROUP
        integer                                :: job_loop
        character(len=PCPSNAME_MAX)            :: pcps_send_mode
        character(len=PCPSNAME_MAX)            :: pcps_receive_mode
        character(len=PCPSNAME_MAX)            :: pcps_send_eof_mode
        character(len=PCPSNAME_MAX)            :: pcps_alt_send_mode
        character(len=PCPSNAME_MAX)            :: pcps_alt_receive_mode
        character(len=PCPSNAME_MAX)            :: pcps_resequence_mode
        character(len=PCPSNAME_MAX)            :: pcps_generator_mode
        character(len=PCPSNAME_MAX)            :: pcps_bunch_mode
        character(len=PCPSNAME_MAX)            :: pcps_boss_exec_mode
        type(PGROUP),pointer                   :: next
      end type PGROUP

      character(len=80), save                  :: main_host
      character(len=80), save                  :: pbs_type
      character(len=80), save                  :: rcp_node
      character(len=80) ,save                  :: platform_default_linux
      character(len=80) ,save                  :: platform_default_sol
      character(len=80) ,save                  :: sps_home
      character(len=80) ,save                  :: sps_install
      character(len=80) ,save                  :: cps_log
      character(len=80) ,save                  :: batchtmp_dir
      character(len=80) ,save                  :: batchtmp_nodes
      character(len=80) ,save                  :: binpath
      integer           ,save                  :: total_memory
      character(len=24) ,save                  :: account
      character(len=15) ,save                  :: jobname
      character(len=16) ,save                  :: frontend_node
      character(len=16) ,save                  :: frontend_user
      character(len=132),save                  :: frontend_path
      character(len=132),save                  :: thisbatchtmp
      character(len=4)  ,save                  :: mail_opt
      character(len=8)  ,save                  :: queue
      character(len=8)  ,save                  :: rlocation,location
      integer           ,save                  :: time_limit
      character(len=10) ,save                  :: machine,whichicps
      character(len=4)  ,save                  :: rerun,speed
      integer           ,save                  :: num_cpus,priority
      integer           ,save                  :: num_nodes = 0
      character(len=24) ,save                  :: std_libs
      integer           ,save                  :: ncustom_modules
      character(len=PC_LENGTH) ,pointer,save   :: custom_modules(:)
      integer           ,save                  :: ncustom_compile
      character(len=PC_LENGTH) ,pointer,save   :: custom_compile(:)
      integer           ,save                  :: ncustom_link
      character(len=PC_LENGTH) ,pointer,save   :: custom_link(:)
      character(len=PC_LENGTH)                 :: custom_exec_b
      character(len=PC_LENGTH)                 :: custom_exec_a
      character(len=4)                         :: custom_lam
      integer           ,save                  :: ncustom_nodes
      character(len=12)        ,pointer,save   :: custom_nodes(:)
      character(len=132),save                  :: tmpdir
      logical           ,save                  :: rsh_compile
      logical           ,save                  :: requires_landmark
      integer           ,save                  :: max_tapes
      character(len=8)  ,save                  :: tape_media,tapequeue
      logical           ,save                  :: same_speed
      integer           ,save                  :: max_pid_digits
      logical           ,save                  :: anycustom,useicps

      character(len=100),public :: cpsbld_ident = &
        '$Id: cpsbld.f90,v 1.5 2008/01/16 22:11:48 Goodger beta sps $'

      character(len=160),save :: templateHome = '/home/sps/templates'

      contains

!!------------------------------- buildjob ---------------------------------!!
!!------------------------------- buildjob ---------------------------------!!
!!------------------------------- buildjob ---------------------------------!!

      subroutine buildjob (workfile,jobfile,mode,batch,customp,custom_mpi,istat)

      character(len=*),intent(in)               :: workfile           ! argument
      character(len=*),intent(in)               :: jobfile            ! argument
      character(len=*),intent(in)               :: mode               ! argument
      character(len=*),intent(in)               :: batch              ! argument
      character(len=*),intent(in)               :: customp            ! argument
      character(len=*),intent(in)               :: custom_mpi         ! argument
      integer         ,intent(inout)            :: istat              ! argument

      integer                                   :: lun           = 6  ! local
      integer                                   :: wunit         = 1  ! local
      integer                                   :: junit         = 2  ! local
      integer                                   :: i,nc               ! local
      integer                                   :: i1                 ! local
      integer                                   :: i2                 ! local
      integer                                   :: ostype,ntrin=0     ! local
      character(len=132)                        :: jobmode            ! local
      character(len=4)                          :: batchsystem        ! local
      character(len=132)                        :: platform           ! local
      character(len=132)                        :: mpi_version        ! local
      character(len=132)                        :: current_card       ! local
      character(len=132)                        :: ctmp               ! local
      character(len=300)                        :: jobfileMain,cmd
      integer                                   :: process_count      ! local
      type(PROCESS),pointer                     :: start              ! local
      type(PROCESS),pointer                     :: last               ! local
      type(PROCESS),pointer                     :: current            ! local
      type(CARD_POINTER)                        :: tag_card           ! local
      logical                                   :: invalid_process    ! local
      logical                                   :: first,entireloop
      logical                                   :: do_parallel        ! local
      logical                                   :: sorryNotParallel
      logical                                   :: tag_flag(NUM_TAGS) ! local
      character(len=TAG_MAX_LEN)                :: tags(NUM_TAGS)     ! local
      integer                                   :: ntags(NUM_TAGS)    ! local


      custom_exec_b   = ' '
      custom_exec_a   = ' '
      ncustom_modules = 0
      ncustom_compile = 0
      ncustom_link    = 0
      ncustom_nodes   = 0
      nullify(custom_modules)
      nullify(custom_compile)
      nullify(custom_link)
      nullify(custom_nodes)

      tags(1)  = ' <GLOBALS>'
      tags(2)  = ' <CONTROLS>'
      tags(3)  = ' <PARAMETERS>'
      tags(4)  = ' <DATA_CARD>'
      ntags(1) = 10
      ntags(2) = 11
      ntags(3) = 13
      ntags(4) = 12

      jobmode = mode
      call string_to_upper (jobmode)
      if (jobmode(1:1) .eq. 'I') then
        jobmode    = 'INTERACTIVE'
        custom_lam = 'YES'
      else
        jobmode    = 'BATCH'
        custom_lam = 'NO'
      endif

      batchsystem = batch
      call string_to_upper (batchsystem)
      if (batchsystem(1:1) .eq. 'P') then
        batchsystem = 'PBS'
      else
        batchsystem = 'NQS'
      endif

      call path_parse (trim(workfile),frontend_user,frontend_node,  &
                       frontend_path,ctmp)

      nc=index(jobfile,'.')
      jobfileMain=jobfile(1:nc-1) // '.main'
      open(unit=wunit,file=workfile,status='OLD' ,access='SEQUENTIAL',err=900)
      open(unit=2,file=jobfileMain ,status='UNKNOWN',access='SEQUENTIAL',err=901)
      open(unit=3              ,status='SCRATCH',access='SEQUENTIAL',err=904,  &
           form='FORMATTED')

      process_count = 0
      nullify(start)
      nullify(last)

      read(wunit,'(A80)',err=902,end=100) current_card
      call string_strip_blanks(current_card,i)
      call string_to_upper(current_card)
      if (current_card(1:i) .ne. '<CPS_V1TYPE="WORKFILE"/>') then
        write(STDOUT,*) 'File is not a CPS Workfile'
        return
      endif

      invalid_process = .false.
! read wrk file loop
      do
        read(wunit,'(A80)',err=902,end=100) current_card
        if (current_card(1:1) .eq. '<') then
          if (current_card(1:10) .eq. '</PROCESS>') cycle
          allocate(current,stat=istat)
          if (istat .ne. 0) goto 903
          do i=1,NUM_TAGS          
            nullify(current%tag_card_start(i)%card)
            nullify(current%tag_card_last(i)%card)
          enddo
          nullify(current%next)
          call string_strip_blanks(current_card)
          call string_to_upper(current_card)
          i1 = index(current_card,'<PROCESSNAME="') ! <PROCESS name=  in job file
          if (i1 .gt. 0) then
            i1 = i1+14
            i2 = index(current_card(i1:),'"')
            if (i2 .gt. 1) then
              current%name  = current_card(i1:i1+i2-2)
            else
              current%name  = 'NONE'
            endif
          else
              current%name  = 'NONE'
          endif
          call string_strip_blanks(current%name,current%nname)
          process_count = process_count + 1
          current%pid   = process_count
          write(current%cpid,'(I4)') current%pid
          call string_strip_blanks(current%cpid,current%ncpid)
          if (current%name .eq. 'NONE') then
            invalid_process = .true.
            write(STDOUT,*) 'Invalid process '//current%cpid
          endif
          current%parallel_safe         = 'NO'
          current%need_request          = 'NO'
          current%need_label            = 'NO'
          current%twosets               = 'NO'
          current%setup_only            = 'NO'
          current%requires_landmark     = 'NO'
          current%signal_handler        = 'NO'
          current%pcps_send_mode        = 'PCPS_SEND_ROUND_ROBIN'
          current%pcps_receive_mode     = 'PCPS_RECEIVE_PASSTHRU'
          current%pcps_send_eof_mode    = 'PCPS_SEND_ALL_EOF'
          current%pcps_alt_send_mode    = 'PCPS_SEND_ROUND_ROBIN'
          current%pcps_alt_receive_mode = 'PCPS_RECEIVE_PASSTHRU'
          current%pcps_resequence_mode  = 'PCPS_NO_RESEQUENCE'
          current%pcps_generator_mode   = 'PCPS_NO_TRACE_GEN'
          current%pcps_bunch_mode       = 'PCPS_BUNCH_TRACE_GROUPS'
          current%pcps_boss_exec_mode   = ' '
          current%ltr                   = 0
          current%lhd                   = 0
          current%ltr2                  = 0
          current%lhd2                  = 0
          current%ntr                   = 0
          current%nscratch              = 0
          current%nstore                = 0
          current%ndisk                 = 0
          current%ntapes                = 0
          current%tmedia                = '3590'
          current%n_io_pairs            = 0
          current%start_loop            = .FALSE.
          current%start_fork            = .FALSE.
          current%parallel_group        = 0
          current%label                 = 0
          current%request               = 0
          current%parallel_begin        = 0
          current%parallel_end          = 0
          if (trim(current%name) .eq. 'PROJECT_DATA' .or.  &
              trim(current%name) .eq. 'JOB_DATA') current%setup_only   = 'YES'
          call buildjob_store(current,start,last)
          do i=1,NUM_TAGS    
            tag_flag(i) = .FALSE.
          enddo
        else if (current_card(1:2) .eq. ' <') then    ! a start tag
          do i=1,NUM_TAGS
            tag_flag(i) = .FALSE.
          enddo
          do i=1,NUM_TAGS
            if (current_card(1:ntags(i)) .eq. tags(i)(1:ntags(i))) then
              tag_flag(i) = .TRUE.
              exit
            endif
          enddo
        else
          ctmp = current_card
          call string_strip_blanks(ctmp)
          if (ctmp(1:1) .eq. ' ') cycle
          do i=1,NUM_TAGS
            if (tag_flag(i)) then
              allocate(tag_card%card,stat=istat)
              if (istat .ne. 0) goto 903
              tag_card%card%data = current_card(3:)
              call buildjob_store_card(tag_card,                   &
                                        current%tag_card_start(i),  &
                                        current%tag_card_last(i))
              exit
            endif
          enddo
        endif
      enddo

  100 continue  ! end of wrk file

      write(ctmp,'(I4)') process_count
      call string_strip_blanks(ctmp,max_pid_digits)

      if (invalid_process) then
        write(STDOUT,*) ' '
        write(STDOUT,*) 'Job build ABORTED'
        return
      endif

      call pc_frontend_update          (3)
      call buildjob_load_job_data     (junit, start, wunit)
      call buildjob_load_project_data (junit, start)

!     Do parallel ??
      do_parallel = .false.
      same_speed  = .false.
      entireloop  = .false.
      sorryNotParallel = .false.
      if (num_cpus .gt. 1) then
        current => start
        do
          if (.not. associated(current)) then
            entireloop=.true.
            exit
          endif
          tag_card%card => current%tag_card_start(CONTROLS_TAG)%card
          first = .TRUE.
          call pc_next
          do
            if (.not. associated(tag_card%card)) exit
            if (first) then
              call pc_put_control_card(tag_card%card%data)
              first = .FALSE.
            else
              call pc_add_control_card(tag_card%card%data)
            endif
            tag_card%card => tag_card%card%next
          enddo
          call pc_get_control ('PARALLEL_SAFE',current%parallel_safe)
!    If trin, it is starting a new loop.  Each loop must have a
!      parallel process to be parallel safe
      if(current%name.eq.'TRIN')then
        ntrin=ntrin+1
        if(ntrin.gt.1.and..not.do_parallel)then
          sorryNotParallel=.true.
          exit
        endif
        do_parallel=.false.
      endif
      
          call string_to_upper (current%parallel_safe)
          if (trim(current%parallel_safe) .eq. 'YES') then
            do_parallel = .true.
!!!            if (same_speed) exit
          endif
          call pc_get_control ('PCPS_SEND_MODE',current%pcps_send_mode)
          call string_to_upper (current%pcps_send_mode)
          if (index(current%pcps_send_mode,'PCPS_BOSS_EXECS') .gt. 0) then
            same_speed = .true.
!!!            if (do_parallel) exit
          endif
          current => current%next
        enddo
        if(.not.do_parallel.and.entireloop)sorryNotParallel=.true.
      endif

!     Uses Tape Drives ??
      max_tapes  = 0
      tape_media = '3590'
      current => start
      do
        if (.not. associated(current)) exit
        tag_card%card => current%tag_card_start(CONTROLS_TAG)%card
        first = .TRUE.
        call pc_next
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            call pc_put_control_card(tag_card%card%data)
            first = .FALSE.
          else
            call pc_add_control_card(tag_card%card%data)
          endif
          tag_card%card => tag_card%card%next
        enddo
        call pc_get_control ('NTAPES',current%ntapes)
        max_tapes  = max(max_tapes, current%ntapes)
        if (current%ntapes .gt. 0) then
          call pc_get_control ('TMEDIA',current%tmedia)
          tape_media = current%tmedia
          call pc_get_control('TAPEQUEUE',tapequeue)
        endif
        current => current%next
      enddo


      if (do_parallel .and. max_tapes .gt. 0) do_parallel = .false.

!     Requires Landmark ??
      requires_landmark = .false.
      current => start
      do
        if (.not. associated(current)) exit
        tag_card%card => current%tag_card_start(CONTROLS_TAG)%card
        first = .TRUE.
        call pc_next
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            call pc_put_control_card(tag_card%card%data)
            first = .FALSE.
          else
            call pc_add_control_card(tag_card%card%data)
          endif
          tag_card%card => tag_card%card%next
        enddo
        call pc_get_control ('REQUIRES_LANDMARK',current%requires_landmark)
        call string_to_upper (current%requires_landmark)
        if (trim(current%requires_landmark) .eq. 'YES') then
          requires_landmark = .true.
          useicps=.false.
          exit
        endif
        current => current%next
      enddo

      if (trim(machine).eq.'Custom' .and. ncustom_nodes.gt.0) then
        machine = custom_nodes(1)
      else if (trim(custom_lam) .eq. 'YES'   .and.  &
               trim(jobmode)    .eq. 'BATCH' .and.  &
               ncustom_nodes    .eq. 0       ) then
         custom_lam = 'NO'
         write(STDOUT,*) ' '
         write(STDOUT,*) 'WARNING - custom_lam ignored for batch'
         write(STDOUT,*) ' '
      endif

      if (.not. do_parallel) then
        num_cpus  = 1
        num_nodes = 1
      endif

      if (trim(custom_mpi) .ne. 'default'   .and.  &
          trim(custom_mpi) .ne. 'new'       .and.  &
          trim(custom_mpi) .ne. '6.5.6'   ) then
        write(STDOUT,*) 'Mpi version not supported'
        return
      endif
      if (trim(custom_mpi) .eq. 'default' ) then
        mpi_version = '6.5.6'
      else if (trim(custom_mpi) .eq. 'new') then
        mpi_version = '6.5.6'
      else
        mpi_version = custom_mpi
      endif
      call string_to_upper (machine,ctmp)
      if (trim(ctmp) .eq. 'LINUX') then
        ostype  = GETSYS_LINUX
      else if (trim(ctmp) .eq. 'SOLARIS') then
        ostype  = GETSYS_SOLARIS
      else if (trim(ctmp).eq.'CUSTOM') then
        if (ncustom_nodes.gt.0) then
          ostype  = getsys_machine(machine)
        else
           ostype  = GETSYS_LINUX
        endif
      else
        ostype = getsys_machine(machine)
      endif
      if (trim(customp) .eq. 'default') then
        select case (ostype)
          case (GETSYS_LINUX)
            platform = trim(platform_default_linux)
            if(queue(1:5).eq.'B3060'.or.location.eq.'pony'.or.&
              rlocation.eq.'PONY')&
            then
              platform='linuxab80_xeon'
            endif
          case (GETSYS_SOLARIS)
            platform = trim(platform_default_sol)
          case default
            platform = 'unknown'
        end select
      else if (trim(customp) .eq. 'debug') then
        select case (ostype)
          case (GETSYS_LINUX)
            platform = 'linuxab80_debug'
          case (GETSYS_SOLARIS)
            platform = 'sol62_debug'
          case default
            platform = 'unknown'
        end select
      else if (trim(customp) .eq. 'new') then
        select case (ostype)
          case (GETSYS_LINUX)
            platform = 'linuxab80'
          case (GETSYS_SOLARIS)
            platform = 'sol62'
          case default
            platform = 'unknown'
        end select
      else
        platform = customp
        select case (ostype)
          case (GETSYS_LINUX)
            i = index(platform,'linux')
            if (i .eq. 0) platform = 'unknown'
          case (GETSYS_SOLARIS)
            i = index(platform,'sol')
            if (i .eq. 0) platform = 'unknown'
          case default
            platform = 'unknown'
        end select
      endif
      if (trim(platform) .eq. 'unknown') then
        write(stdout,*) 'Invalid platform'
        return
      endif
      if(trim(platform).ne.trim(platform_default_linux))useicps=.false.

      call buildjob_write_setup (junit,start,trim(jobmode),trim(batchsystem), &
                                  trim(platform),trim(mpi_version))



      if (do_parallel) then
        call buildjob_write_parallel (junit,start)
      else
        call buildjob_write_main     (junit,start)
      endif
      if(sorryNotParallel)then
        print*,' Parallel process not found in TRIN loop ',ntrin
        print*,' Job will be set to use only 1 cpu.'
      endif
      call buildjob_write_data (junit,start)
      call buildjob_write_jcl  (junit,start,trim(jobmode),trim(batchsystem),  &
                                 trim(platform),trim(mpi_version))

      close(unit=wunit)
      close(unit=2)
      close(unit=3)

!!        Put the edited template files together to make the job file
      cmd='cat ' // trim(jobname) // '.begin '   // &
                    trim(jobname) // '.main '    // &
                    trim(jobname) // '.compile ' // &
                    trim(jobname) // '.mid '     // &
                    trim(jobname) // '.end '     // &
                    ' > ' // trim(jobname) // '.job'

      call putsys_cmd(cmd)

      if (trim(jobmode) .eq. 'INTERACTIVE') then
        call putsys_cmd ('chmod +x ' // jobfile)
      endif

      return

  900 write(STDOUT,*) 'Workfile not found'
      return
  901 write(STDOUT,*) 'Can not open ' // trim(jobfileMain)
      return
  902 write(STDOUT,*) 'Error reading workfile'
      return
  903 write(STDOUT,*) 'Out of memory'
      return
  904 write(STDOUT,*) 'Could not open parameter cache'
      return
      end subroutine buildjob


!!---------------------------- buildjob_store ------------------------------!!
!!---------------------------- buildjob_store ------------------------------!!
!!---------------------------- buildjob_store ------------------------------!!


      subroutine buildjob_store(current,start,last)

      type(PROCESS),pointer                     :: current            ! argument
      type(PROCESS),pointer                     :: start              ! argument
      type(PROCESS),pointer                     :: last               ! argument

      if (.not. associated(last)) then
        start => current
      else
        last%next => current
      endif
      nullify(current%next)
      last => current

      return
      end subroutine buildjob_store


!!------------------------- buildjob_store_card ----------------------------!!
!!------------------------- buildjob_store_card ----------------------------!!
!!------------------------- buildjob_store_card ----------------------------!!


      subroutine buildjob_store_card(current,start,last)

      type(CARD_POINTER)                        :: current            ! argument
      type(CARD_POINTER)                        :: start              ! argument
      type(CARD_POINTER)                        :: last               ! argument

      if (.not. associated(last%card)) then
        start%card => current%card
      else
        last%card%next => current%card
      endif
      nullify(current%card%next)
      last%card => current%card

      return
      end subroutine buildjob_store_card


!!----------------------- buildjob_write_parallel --------------------------!!
!!----------------------- buildjob_write_parallel --------------------------!!
!!----------------------- buildjob_write_parallel --------------------------!!


      subroutine buildjob_write_parallel(junit,start)

      integer              ,intent(in)          :: junit              ! argument
      type(PROCESS),pointer                     :: start              ! argument

      integer                         :: i                            ! local
      integer                         :: j                            ! local
      integer                         :: k                            ! local
      integer                         :: j1                           ! local
      integer                         :: j2                           ! local
      type(PROCESS),pointer           :: current                      ! local
      type(PROCESS),pointer           :: loop_start_process           ! local
      type(PROCESS),pointer           :: previous                     ! local
      type(PGROUP),pointer            :: current_pgroup               ! local
      type(PGROUP),pointer            :: loop_start_pgroup            ! local
      type(PGROUP),pointer            :: start_pgroup                 ! local
      type(PGROUP),pointer            :: last_pgroup                  ! local
      logical                         :: first                        ! local
      logical                         :: null_action                  ! local
      logical                         :: in_fork                      ! local
      logical                         :: do_parallel                  ! local
      logical                         :: need_extra_io                ! local
      logical                         :: found                        ! local
      integer                         :: label                        ! local
      integer                         :: last_label      = 0          ! local
      integer                         :: last_fork       = 0          ! local
      integer                         :: last_loop       = 0          ! local
      type(CARD_POINTER)              :: tag_card                     ! local
      character(len=PCPSNAME_MAX+1)   :: pblank                       ! local
      character(len=80)               :: ctmp                         ! local
      integer                         :: nctmp                        ! local
      character(len=80)               :: ctmp2                        ! local
      integer                         :: nctmp2                       ! local
      character(len=80)               :: ctmp3                        ! local
      character(len=80)               :: ctmp4                        ! local
      integer                         :: nctmp4                       ! local
      integer                         :: nwih                         ! local
      integer                         :: ndpt                         ! local
      integer                         :: ntr                          ! local
      integer                         :: nscratch_max      = 0        ! local
      integer                         :: nstore_max        = 0        ! local
      integer                         :: ndisk_max         = 0        ! local
      integer                         :: ncart_max         = 0        ! local
      integer                         :: nmag_max          = 0        ! local
      integer                         :: nloops            = 0        ! local
      integer                         :: n_io_pairs        = 0        ! local
      integer                         :: n_io_pairs_total  = 0        ! local
      integer                         :: nstore_loop       = 0        ! local
      integer                         :: nscratch_loop     = 0        ! local
      integer                         :: n_do_parallel     = 0        ! local
      integer                         :: n_parallel_groups = 0        ! local
      integer                         :: n_parallel_groups_start = 0  ! local
      integer                         :: n_parallel_groups_end = 0    ! local
      integer                         :: ntr_x(MAX_IO_PAIRS)          ! local
      integer                         :: hd_x_len(MAX_IO_PAIRS)       ! local
      integer                         :: tr_x_len(MAX_IO_PAIRS)       ! local
      integer                         :: tr_parallel(MAX_IO_PAIRS)    ! local

      do i=1,PCPSNAME_MAX+1
        pblank(i:i) = ' '
      enddo

      nullify(start_pgroup)
      nullify(last_pgroup)

      tr_parallel      = 0
      ntr_x            = 0
      hd_x_len         = 0
      tr_x_len         = 0

      nwih             = 0
      ndpt             = 0
      ntr              = 0
      label            = 0
      i                = 0
      in_fork          = .false.
      nullify(previous)
      current => start
      do
        if (.not. associated(current)) exit
        tag_card%card => current%tag_card_start(CONTROLS_TAG)%card
        first = .TRUE.
        i = i + 1
        write(STDOUT,*) 'process #',i,' = ',current%name(1:current%nname)
        call pc_next
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            call pc_put_control_card(tag_card%card%data)
            first = .FALSE.
          else
            call pc_add_control_card(tag_card%card%data)
          endif
          tag_card%card => tag_card%card%next
        enddo

        call pc_get_control ('PARALLEL_SAFE'       ,current%parallel_safe      )
        call pc_get_control ('PCPS_SEND_MODE'      ,current%pcps_send_mode     )
        call pc_get_control ('PCPS_RECEIVE_MODE'   ,current%pcps_receive_mode  )
        call pc_get_control ('PCPS_SEND_EOF_MODE'  ,current%pcps_send_eof_mode )
        call pc_get_control ('PCPS_ALT_SEND_MODE'  ,current%pcps_alt_send_mode )
        call pc_get_control ('PCPS_ALT_RECEIVE_MODE',  &
                                                  current%pcps_alt_receive_mode)
        call pc_get_control ('PCPS_RESEQUENCE_MODE' ,  &
                                                  current%pcps_resequence_mode )
        call pc_get_control ('PCPS_GENERATOR_MODE' ,current%pcps_generator_mode)
        call pc_get_control ('PCPS_BUNCH_MODE'     ,current%pcps_bunch_mode    )
        call pc_get_control ('PCPS_BOSS_EXEC_MODE' ,current%pcps_boss_exec_mode)
        call pc_get_control ('NEED_REQUEST'        ,current%need_request       )
        call pc_get_control ('NEED_LABEL'          ,current%need_label         )
        call pc_get_control ('TWOSETS'             ,current%twosets            )
        call pc_get_control ('SETUP_ONLY'          ,current%setup_only         )
        call pc_get_control ('SIGNAL'              ,current%signal_handler     )
        call pc_get_control ('NSCRATCH'            ,current%nscratch           )
        call pc_get_control ('NSTORE'              ,current%nstore             )
        call pc_get_control ('NDISK'               ,current%ndisk              )
        call pc_get_control ('IFTD'                ,current%iftd               )

        call string_to_upper (current%parallel_safe       )
        call string_to_upper (current%need_request        )
        call string_to_upper (current%need_label          )
        call string_to_upper (current%twosets             )
        call string_to_upper (current%setup_only          )
        call string_to_upper (current%iftd                )

        if (current%setup_only .eq. 'YES') then
         current%need_request = 'NO'
         current%need_label   = 'NO'
         current%twosets      = 'NO'
        endif

        tag_card%card => current%tag_card_start(GLOBALS_TAG)%card
        first   = .TRUE.
        call pc_next
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            call pc_put_global_card(tag_card%card%data)
            first = .FALSE.
          else
            call pc_add_global_card(tag_card%card%data)
          endif
          tag_card%card => tag_card%card%next
        enddo

        current%ltr  = ndpt
        current%lhd  = nwih

        if (pc_global_keyword_present('NWIH'))  call pc_get_global('NWIH' ,nwih)
        if (pc_global_keyword_present('NDPT'))  call pc_get_global('NDPT' ,ndpt)
        if (pc_global_keyword_present('NUMTR')) call pc_get_global('NUMTR',ntr )
        current%ntr  = ntr

        if (current%twosets(1:1) .eq. 'Y') then
          current%ltr2 = ndpt
          current%lhd2 = nwih
        else
          current%ltr  = ndpt
          current%lhd  = nwih
        endif

        nscratch_max = max(nscratch_max , current%nscratch)
        nstore_max   = nstore_max + current%nstore

        if (current%setup_only .eq. 'YES') then
          current  => current%next
          cycle
        endif

        if ((nloops .eq. 0 .and. current%ntr .ne. 0) .or.                     &
            (current%need_label(1:1) .eq. 'Y' .and.                           &
             current%need_request(1:1) .eq. 'N')) then
          nloops = nloops + 1
          current%start_loop = .TRUE.
          n_io_pairs = n_io_pairs + 1
        endif

        if (current%parallel_safe(1:1) .eq. 'Y') then
          if (n_parallel_groups .eq. 0) then
            n_parallel_groups      = n_parallel_groups + 1
            current%parallel_group = n_parallel_groups
            allocate(current_pgroup)
            current_pgroup%job_loop              = nloops
            current_pgroup%pcps_send_mode        = current%pcps_send_mode
            current_pgroup%pcps_receive_mode     = current%pcps_receive_mode
            current_pgroup%pcps_send_eof_mode    = current%pcps_send_eof_mode
            current_pgroup%pcps_alt_send_mode    = current%pcps_alt_send_mode
            current_pgroup%pcps_alt_receive_mode = current%pcps_alt_receive_mode
            current_pgroup%pcps_resequence_mode  = current%pcps_resequence_mode
            current_pgroup%pcps_generator_mode   = current%pcps_generator_mode
            current_pgroup%pcps_bunch_mode       = current%pcps_bunch_mode
            current_pgroup%pcps_boss_exec_mode   = current%pcps_boss_exec_mode
            nullify(current_pgroup%next)
            if (.not. associated(last_pgroup)) then
              start_pgroup => current_pgroup
            else
              last_pgroup%next => current_pgroup
            endif
            last_pgroup => current_pgroup
          else
            if (.not. in_fork  &
                .or.           &
                current_pgroup%pcps_send_mode   .ne.current%pcps_send_mode  &
                .or.           &
                current_pgroup%pcps_receive_mode.ne.current%pcps_receive_mode  &
                .or.           &
                current_pgroup%pcps_resequence_mode.ne.'PCPS_NO_RESEQUENCE')then
              if (in_fork) then
                label = label + 10           ! Label for do_parallel_end
                current%parallel_end = label
                in_fork = .false.
                if (need_extra_io) then
                  n_io_pairs = n_io_pairs + 1
                  tr_x_len(n_io_pairs) = tr_x_len(n_io_pairs - 1)
                  hd_x_len(n_io_pairs) = hd_x_len(n_io_pairs - 1)
                  ntr_x(n_io_pairs)    = ntr_x(n_io_pairs - 1)
                endif
                tr_parallel(n_do_parallel) = n_io_pairs
              endif
              need_extra_io = .true.
              n_parallel_groups      = n_parallel_groups + 1
              current%parallel_group = n_parallel_groups
              in_fork                = .false.
              allocate(current_pgroup)
              current_pgroup%job_loop             = nloops
              current_pgroup%pcps_send_mode       = current%pcps_send_mode
              current_pgroup%pcps_receive_mode    = current%pcps_receive_mode
              current_pgroup%pcps_send_eof_mode   = current%pcps_send_eof_mode
              current_pgroup%pcps_alt_send_mode   = current%pcps_alt_send_mode
              current_pgroup%pcps_alt_receive_mode=current%pcps_alt_receive_mode
              current_pgroup%pcps_resequence_mode = current%pcps_resequence_mode
              current_pgroup%pcps_generator_mode  = current%pcps_generator_mode
              current_pgroup%pcps_bunch_mode      = current%pcps_bunch_mode
              current_pgroup%pcps_boss_exec_mode  = current%pcps_boss_exec_mode
              nullify(current_pgroup%next)
              if (.not. associated(last_pgroup)) then
                start_pgroup => current_pgroup
              else
                last_pgroup%next => current_pgroup
              endif
              last_pgroup => current_pgroup
            endif
          endif
          if (.not. in_fork) then
            current%start_fork    = .true.
            label = label + 10                   ! Label for do_parallel_begin
            current%parallel_begin = label
            n_do_parallel = n_do_parallel + 1
            in_fork = .true.
          else
            current_pgroup%pcps_receive_mode     = current%pcps_receive_mode
            current_pgroup%pcps_alt_receive_mode = current%pcps_alt_receive_mode
          endif
          if (current%pcps_resequence_mode.ne.'PCPS_NO_RESEQUENCE' .and.  &
              current_pgroup%pcps_resequence_mode.eq.'PCPS_NO_RESEQUENCE')  &
              current_pgroup%pcps_resequence_mode = current%pcps_resequence_mode
          if (current%pcps_generator_mode  .eq. 'PCPS_TRACE_GEN'   )  &
            current_pgroup%pcps_generator_mode  = 'PCPS_TRACE_GEN'
        else                             ! Not parallel safe
          if (in_fork) then
            label = label + 10           ! Label for do_parallel_end
            current%parallel_end = label
            in_fork = .false.
            if (need_extra_io) then
              n_io_pairs = n_io_pairs + 1
              tr_x_len(n_io_pairs) = tr_x_len(n_io_pairs - 1)
              hd_x_len(n_io_pairs) = hd_x_len(n_io_pairs - 1)
              ntr_x(n_io_pairs)    = ntr_x(n_io_pairs - 1)
            endif
            tr_parallel(n_do_parallel) = n_io_pairs
          endif
          need_extra_io = .true.
        endif

        if (current%need_request(1:1) .eq. 'Y') then
          current%request = label
        endif

        if (current%need_label(1:1) .eq. 'Y') then
          label = label + 10
          current%label = label
        endif

        if (current%twosets(1:1) .eq. 'Y') then
          tr_x_len(n_io_pairs) = max(tr_x_len(n_io_pairs),current%ltr)
          hd_x_len(n_io_pairs) = max(hd_x_len(n_io_pairs),current%lhd)
          n_io_pairs = n_io_pairs + 1
          tr_x_len(n_io_pairs) = current%ltr2
          hd_x_len(n_io_pairs) = current%lhd2
          if (in_fork) need_extra_io = .false.
        else if (current%ntr.ne.0) then
          tr_x_len(n_io_pairs) = max(tr_x_len(n_io_pairs),current%ltr)
          hd_x_len(n_io_pairs) = max(hd_x_len(n_io_pairs),current%lhd)
        endif

        if (current%ntr.gt.0)  then
          if (current%start_loop) then
            ntr_x(n_io_pairs) = current%ntr
          else
            ntr_x(n_io_pairs) = max(ntr_x(n_io_pairs),current%ntr)
          endif
        endif
        current%n_io_pairs = n_io_pairs
        previous => current
        current  => current%next
      enddo

      if (in_fork) then              ! Still in fork when out of processes
        in_fork = .false.
        if (need_extra_io) then
          n_io_pairs = n_io_pairs + 1
          tr_x_len(n_io_pairs) = tr_x_len(n_io_pairs - 1)
          hd_x_len(n_io_pairs) = hd_x_len(n_io_pairs - 1)
          ntr_x(n_io_pairs)    = ntr_x(n_io_pairs - 1)
        endif
        tr_parallel(n_do_parallel) = n_io_pairs
        previous%n_io_pairs = n_io_pairs
      endif
      need_extra_io = .true.

      total_memory = 5242880 + nscratch_max + nstore_max

      write(junit,'(A)') &
      '      use pcps_module'
      write(junit,'(A)') &
      '      use pcpsx_module'
      write(junit,'(A)') &
      '      use named_constants_module'
      write(junit,'(A)') &
      '! '
      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      use ' // current%name(1:current%nname) // '_module'
        current => current%next
      enddo
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      implicit none'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      public'
      do i=1,n_parallel_groups
        call string_ii2cc (i,ctmp)
        write(junit,'(A)') &
      '      type(PCPSX_DO_PARALLEL_struct),pointer :: p_obj' // trim(ctmp)
      enddo

      write(junit,'(A)') &
      '! '
      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      type(' // current%name(1:current%nname) //      &
        '_struct),pointer  ' // pblank(1:16-current%nname) //  &
        ':: obj' // current%cpid(1:current%ncpid)
        current => current%next
      enddo

      write(junit,'(A)') &
      '      end module program_global_data'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!! PROGRAM MAIN !!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      program '//trim(jobname)//'_cps'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      use program_global_data'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '!!!!! DECLARATIONS:'
      write(junit,'(A)') &
      '! '

      do i = 1,n_io_pairs
        write(ctmp,'(''tr'',I3,''(:,:)'')') i
        call string_strip_blanks(ctmp,nctmp)
        write(ctmp2,'(''hd'',I3,''(:,:)'')') i
        call string_strip_blanks(ctmp2,nctmp2)
        write(junit,'(A)') &
        '      double precision, pointer :: ' // ctmp2(1:nctmp2)
        write(junit,'(A)') &
        '      real            , pointer :: ' // ctmp(1:nctmp)
      enddo

      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      integer :: ntr, ierr'
      write(junit,'(A)') &
      '      logical :: error'

      write(junit,'(A)') &
      '! '

      do i=1,n_parallel_groups
        call string_ii2cc (i,ctmp)
        write(junit,'(A)') '      nullify (p_obj'//trim(ctmp)//')'
      enddo

      write(junit,'(A)') &
      '! '

      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)')   &
          '      nullify (obj'//current%cpid(1:current%ncpid)//')'
        current => current%next
      enddo

      write(junit,'(A)') &
      '! '

      write(junit,'(A)') &
      '      call pcpsx_start_processing'

      write(junit,'(A)') &
      '! '

      write(junit,'(A)') &
      '      call pcpsx_print_rcs_ident (trim(PCPS_ident))'
      write(junit,'(A)') &
      '      call pcpsx_print_rcs_ident (trim(PCPSX_ident))'
      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      call pcpsx_print_rcs_ident '//  &
        '(trim('//current%name(1:current%nname)//'_ident))'
        current => current%next
      enddo
      write(junit,'(A)') '      call pcpsx_custom_rcs_ident'

      n_io_pairs_total = n_io_pairs
      n_io_pairs       = 0
      loop_start_process => start
      n_parallel_groups = 0
      loop_start_pgroup => start_pgroup
      do i = 1,max(nloops,1)
        n_io_pairs = n_io_pairs + 1
        if (.not. associated(loop_start_process)) exit
        current => loop_start_process
        do_parallel = .false.
        if (i .gt. 1) then
          write(junit,'(A)') '!'
          write(junit,'(A)') '      call pcpsx_restart_processing()'
          write(junit,'(A)') '!'
        endif
        do
          if (.not. associated(current)) exit
          if (trim(current%parallel_safe).eq.'YES' .and. .not.do_parallel)then
            do_parallel = .true.
            write(junit,'(A)') &
            '! '
            write(junit,'(A)') &
            '!!!!! ALLOCATE ARRAYS:'
            write(junit,'(A)') &
            '! '
            j1 = max(loop_start_process%n_io_pairs,1)
            j2 = j1
            current => loop_start_process
            first = .TRUE.
            do
              if (.not. associated(current)) exit
              if (current%start_loop) then
                if (first) then
                  first = .FALSE.
                else
                  exit
                endif
              endif
              j2 = current%n_io_pairs
              current => current%next
            enddo
            current => loop_start_process
            do j = j1,j2
              call string_ii2cc (j          , ctmp )
              call string_ii2cc (ntr_x(j)   , ctmp2)
              call string_ii2cc (hd_x_len(j), ctmp3)
              call string_ii2cc (tr_x_len(j), ctmp4)
              do k = 1, n_do_parallel
                if (tr_parallel(k) .eq. j) then
                  found = .true.
                  exit
                endif
              enddo
              if (found .and.  &
                 (trim(current%pcps_receive_mode).eq.'PCPS_RECEIVE_GROUP'.or.  &
                  trim(current%pcps_alt_receive_mode).eq.  &
                                                     'PCPS_RECEIVE_GROUP'.or.  &
                  trim(current%pcps_send_mode).eq.'PCPS_BOSS_EXECS_GATHER'.or. &
                  trim(current%pcps_alt_send_mode).eq.  &
                                                 'PCPS_BOSS_EXECS_GATHER')) then
                write(junit,'(A)')  &
               '      ntr = '//trim(ctmp2)//' * max(1,pcps_get_num_workers())'
              else
                write(junit,'(A)')  &
                '      ntr = '//trim(ctmp2)
              endif
              write(junit,'(A)')  &
              '      allocate(hd'//trim(ctmp)//'('//trim(ctmp3)//','//  &
                              'ntr),stat=ierr)'
              write(junit,'(A)')  &
              '      if (ierr /= 0) then'
              write(junit,'(A)')  &
           '        call pcps_print (''Unable to allocate hd'//trim(ctmp)//''')'
              write(junit,'(A)')  &
              '        error = .true.'
              write(junit,'(A)')  &
              '        goto 9999'
              write(junit,'(A)')  &
              '      endif'
              write(junit,'(A)')  &
              '      allocate(tr'//trim(ctmp)//'('//trim(ctmp4)//','//  &
                              'ntr),stat=ierr)'
              write(junit,'(A)')  &
              '      if (ierr /= 0) then'
              write(junit,'(A)')  &
           '        call pcps_print (''Unable to allocate tr'//trim(ctmp)//''')'
              write(junit,'(A)')  &
              '        error = .true.'
              write(junit,'(A)')  &
              '        goto 9999'
              write(junit,'(A)')  &
              '      endif'
              write(junit,'(A)') &
              '! '
            enddo
          endif
          current => current%next
        enddo

        write(junit,'(A)') &
        '!!!!! SETUPS:'
        write(junit,'(A)') &
        '! '
        write(junit,'(A)') &
        '      call pcpsx_start_setups    ! stops if there is an error.'
        write(junit,'(A)') &
        '! '

        first = .TRUE.
        do_parallel = .false.
        current_pgroup => loop_start_pgroup
        j = n_parallel_groups
        n_parallel_groups_start = j + 1
        do
          if (.not. associated(current_pgroup)) exit
          if (current_pgroup%job_loop .gt. i) then
            loop_start_pgroup => current_pgroup
            exit
          endif
          j = j + 1
          call string_ii2cc (j,ctmp)
          write(junit,'(A)') '      call pcpsx_do_parallel_create (p_obj'//  &
                                    trim(ctmp)//',error)'
          if (trim(current_pgroup%pcps_send_mode).eq.'PCPS_BOSS_EXECS'   .or.  &
              trim(current_pgroup%pcps_send_mode).eq.'PCPS_BOSS_EXECS_GATHER'  &
                                                                         .or.  &
              trim(current_pgroup%pcps_send_mode).eq.'PCPS_BOSS_EXECS_MERGE')  &
                                                                            then
            write(junit,'(A)')  &
                        '      call pcps_set_send_mode       ('//         &
                               trim(current_pgroup%pcps_send_mode)//      &
                               pblank(1:PCPSNAME_MAX+1-                   &
                               len_trim(current_pgroup%pcps_send_mode))// &
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_generator_mode  ('//              &
                               trim(current_pgroup%pcps_generator_mode)//      &
                               pblank(1:PCPSNAME_MAX+1-                        &
                               len_trim(current_pgroup%pcps_generator_mode))// &
                               ',error)'
          else
            write(junit,'(A)')  &
                        '      call pcps_set_send_mode       ('//         &
                               trim(current_pgroup%pcps_send_mode)//      &
                               pblank(1:PCPSNAME_MAX+1-                   &
                               len_trim(current_pgroup%pcps_send_mode))// &
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_receive_mode    ('//             &
                               trim(current_pgroup%pcps_receive_mode)//       &
                               pblank(1:PCPSNAME_MAX+1-                       &
                               len_trim(current_pgroup%pcps_receive_mode))//  &
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_send_eof_mode   ('//             &
                               trim(current_pgroup%pcps_send_eof_mode)//      &
                               pblank(1:PCPSNAME_MAX+1-                       &
                               len_trim(current_pgroup%pcps_send_eof_mode))// &
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_alt_send_mode   ('//              &
                               trim(current_pgroup%pcps_alt_send_mode)//       &
                               pblank(1:PCPSNAME_MAX+1-                        &
                               len_trim(current_pgroup%pcps_alt_send_mode))//  &
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_alt_receive_mode('//              &
                               trim(current_pgroup%pcps_alt_receive_mode)//    &
                               pblank(1:PCPSNAME_MAX+1-                        &
                              len_trim(current_pgroup%pcps_alt_receive_mode))//&
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_resequence_mode ('//              &
                               trim(current_pgroup%pcps_resequence_mode)//     &
                               pblank(1:PCPSNAME_MAX+1-                        &
                               len_trim(current_pgroup%pcps_resequence_mode))//&
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_generator_mode  ('//              &
                               trim(current_pgroup%pcps_generator_mode)//      &
                               pblank(1:PCPSNAME_MAX+1-                        &
                               len_trim(current_pgroup%pcps_generator_mode))// &
                               ',error)'
            write(junit,'(A)')  &
                        '      call pcps_set_bunch_mode      ('//           &
                               trim(current_pgroup%pcps_bunch_mode)//       &
                               pblank(1:PCPSNAME_MAX+1-                     &
                               len_trim(current_pgroup%pcps_bunch_mode))//  &
                               ',error)'
           if(trim(current_pgroup%pcps_boss_exec_mode).ne.' ')then
            write(junit,'(A)')  &
                        '      call pcps_set_boss_exec_mode  ('//           &
                               trim(current_pgroup%pcps_boss_exec_mode)//   &
                               pblank(1:PCPSNAME_MAX+1-                     &
                               len_trim(current_pgroup%pcps_boss_exec_mode))//&
                               ',error)'
           endif
          endif
          write(junit,'(A)') '      call pcpsx_do_parallel_init   (p_obj'//  &
                                    trim(ctmp)//',error)'
          write(junit,'(A)') '      if (error) goto 9999'
          write(junit,'(A)') '! '
          current_pgroup => current_pgroup%next
        enddo
        n_parallel_groups_end = j

        current => loop_start_process
        first   = .TRUE.
        need_extra_io = .true.
        do
          if (.not. associated(current)) exit
          if (current%start_loop) then
            if (first) then
              first = .FALSE.
            else
              exit
            endif
          endif
          if (current%parallel_safe(1:1) .eq. 'Y') then
            if (trim(current%pcps_send_mode) .eq. 'PCPS_BOSS_EXECS' .or.  &
                trim(current%pcps_send_mode) .eq. 'PCPS_BOSS_EXECS_GATHER' .or.&
                trim(current%pcps_send_mode) .eq. 'PCPS_BOSS_EXECS_MERGE') then
              write(junit,'(A)') &
              '      call pcpsx_pre_setup     (PCPS_BOSS_PARALLEL)'
            else
              write(junit,'(A)') &
              '      call pcpsx_pre_setup     (PCPS_INSIDE_PARALLEL)'
            endif
          else
            write(junit,'(A)') &
            '      call pcpsx_pre_setup     (PCPS_OUTSIDE_PARALLEL)'
          endif
          write(junit,'(A)') &
          '      call ' // current%name(1:current%nname) //&
          '_create'// pblank(1:PNAME_MAX+1-current%nname) // '(obj' //  &
          current%cpid(1:current%ncpid) // ')'
          write(junit,'(A)') '      call pcpsx_post_setup'
            write(junit,'(A)') '! '
          current => current%next
        enddo
        write(ctmp,'(I10)') i + 9000
        call string_strip_blanks(ctmp,nctmp)
        write(junit,'(A)') &
        '      call pcpsx_finish_setups (error); if (error) go to '//trim(ctmp)
        write(junit,'(A)') &
        '! '
        if (nloops .gt. 0) then

          write(junit,'(A)') &
          '!!!!! EXECUTION:'
          write(junit,'(A)') &
          '! '
          write(junit,'(A)') &
          '      ntr = NEED_TRACES'
          write(junit,'(A)') &
          '! '

          nullify(previous)
          current => loop_start_process
          first             = .true.
          in_fork           = .false.
          null_action       = .false.
          n_do_parallel     = n_parallel_groups_start - 1
          do
            if (.not. associated(current)) exit
            if (current%start_loop) then
              if (first) then
                first = .FALSE.
              else
                exit
              endif
            endif
            if (current%setup_only.ne.'YES') then
              if (current%parallel_safe(1:1) .eq. 'Y') then
                if (in_fork .and. current%start_fork) then
                  write(ctmp,'(I10)') current%parallel_end
                  call string_strip_blanks(ctmp,nctmp)
                  call string_ii2cc (n_parallel_groups,ctmp2)
                  call string_ii2cc (tr_parallel(n_do_parallel),ctmp3)
                  call string_ii2cc (n_io_pairs,ctmp4)
                  if (.not. need_extra_io) then
                    write(junit,'(A)') &
                  ' '//ctmp(1:max(nctmp,5))//'call pcpsx_do_parallel_end   ('//&
                  'p_obj' // trim(ctmp2) // ',ntr,hd'//trim(ctmp4)//&
                  ',tr'//trim(ctmp4)//')'
                  else
                    n_io_pairs = n_io_pairs + 1
                    write(junit,'(A)') &
                  ' '//ctmp(1:max(nctmp,5))//'call pcpsx_do_parallel_end   ('//&
                  'p_obj' // trim(ctmp2) // ',ntr,hd'//trim(ctmp3)//    &
                  ',tr'//trim(ctmp3)//',hd'//trim(ctmp4)//    &
                  ',tr'//trim(ctmp4)//')'
                  endif
                  call string_ii2cc (last_fork,ctmp)
                  write(junit,'(A)') &
                  '      if (ntr == LOOP_BACK) goto ' //  trim(ctmp)
                  write(ctmp,'(I10)') i + 8000
                  call string_strip_blanks(ctmp,nctmp)
                  write(junit,'(A)') &
                  '      if (ntr == SHUT_DOWN) goto ' // trim(ctmp)
                  write(junit,'(A)') '!'
                  last_fork  = current%parallel_begin
                  last_label = current%parallel_end
                  in_fork = .false.
                  need_extra_io = .true.
                endif
                if (.not. in_fork) then
                  n_do_parallel     = n_do_parallel     + 1
                  n_parallel_groups = n_parallel_groups + 1
                  in_fork = .true.
                  if (null_action) then
                    write(junit,'(A)') '      endif'
                    write(junit,'(A)') '! '
                    null_action = .false.
                  endif
                  write(ctmp,'(I10)') current%parallel_begin
                  call string_strip_blanks(ctmp,nctmp)
                  call string_ii2cc (n_parallel_groups,ctmp2)
                  call string_ii2cc (n_io_pairs,ctmp3)
                  call string_ii2cc (tr_parallel(n_do_parallel),ctmp4)
                  write(junit,'(A)') &
                ' ' // ctmp(1:max(nctmp,5))//'call pcpsx_do_parallel_begin ('//&
                'p_obj' // trim(ctmp2) // ',ntr,hd'//trim(ctmp3)//',tr'//&
                trim(ctmp3)//',hd'//trim(ctmp4)//',tr'//trim(ctmp4)//')'
                  call string_ii2cc (last_label,ctmp)
                  write(junit,'(A)') &
                  '      if (ntr == LOOP_BACK) goto ' // trim(ctmp)
                  write(ctmp,'(I10)') i + 8000
                  call string_strip_blanks(ctmp,nctmp)
                  write(junit,'(A)') &
                  '      if (ntr == SHUT_DOWN) goto ' // trim(ctmp)
                  write(junit,'(A)') '!'
                  last_fork  = current%parallel_begin
                  last_label = max(current%label,current%parallel_begin)
                endif
              else
                if (in_fork) then
                  write(ctmp,'(I10)') current%parallel_end
                  call string_strip_blanks(ctmp,nctmp)
                  call string_ii2cc (n_parallel_groups,ctmp2)
                  call string_ii2cc (tr_parallel(n_do_parallel),ctmp3)
                  call string_ii2cc (n_io_pairs,ctmp4)
                  if (.not. need_extra_io) then
                    write(junit,'(A)') &
                  ' '//ctmp(1:max(nctmp,5))//'call pcpsx_do_parallel_end   ('//&
                  'p_obj' // trim(ctmp2) // ',ntr,hd'//trim(ctmp4)//&
                  ',tr'//trim(ctmp4)//')'
                  else
                    n_io_pairs = n_io_pairs + 1
                    write(junit,'(A)') &
                  ' '//ctmp(1:max(nctmp,5))//'call pcpsx_do_parallel_end   ('//&
                  'p_obj' // trim(ctmp2) // ',ntr,hd'//trim(ctmp3)//    &
                  ',tr'//trim(ctmp3)//',hd'//trim(ctmp4)//    &
                  ',tr'//trim(ctmp4)//')'
                  endif
                  call string_ii2cc (last_fork,ctmp)
                  write(junit,'(A)') &
                  '      if (ntr == LOOP_BACK) goto ' //  trim(ctmp)
                  write(ctmp,'(I10)') i + 8000
                  call string_strip_blanks(ctmp,nctmp)
                  write(junit,'(A)') &
                  '      if (ntr == SHUT_DOWN) goto ' // trim(ctmp)
                  write(junit,'(A)') '!'
                  last_fork  = current%parallel_end
                  last_label = max(current%label,current%parallel_end)
                  in_fork = .false.
                  need_extra_io = .true.
                endif
              endif
              if (current%need_label(1:1) .eq. 'Y') then
                if (current%start_loop) then
                  write(ctmp,'(I10)') current%label
                  call string_strip_blanks(ctmp,nctmp)
                  call string_ii2cc (n_parallel_groups+1,ctmp2)
                  write(junit,'(A)') &
                  ' ' // ctmp(1:max(nctmp,5))//'call pcpsx_start_loop ('// &
                  'p_obj' // trim(ctmp2) // ',ntr)'
                  last_loop  = current%label
                  last_fork  = 0
                  last_label = max(current%label,current%parallel_end)
                else
                  write(ctmp,'(I10)') current%label
                  call string_strip_blanks(ctmp,nctmp)
                  write(junit,'(A)') &
                  ' ' // ctmp(1:max(nctmp,5)) // 'continue'
                  last_label = max(current%label,current%parallel_end)
                endif
              endif
              if (current%twosets(1:1) .eq. 'Y') then
                if (in_fork) need_extra_io = .false.
                if (.not. null_action) then
                  write(junit,'(A)') '      if (ntr /= NULL_ACTION) then'
                  null_action = .true.
                endif
                if (current%need_request(1:1) .eq. 'Y') then
                  call string_ii2cc (current%request,ctmp)
                  write(junit,'(A)') '        call pcpsx_filter_ntr (' //  &
                                                   trim(current%cpid) // ',ntr)'
                  write(junit,'(A)') '        if (ntr == LOOP_BACK) goto ' //  &
                                                trim(ctmp)
                endif
                write(junit,'(A)') '        call pcpsx_pre_process  (' //    &
                                                       trim(current%cpid) // ')'
                write(ctmp,'(I10)') n_io_pairs
                call string_strip_blanks(ctmp,nctmp)
                n_io_pairs = n_io_pairs + 1
                write(ctmp2,'(I10)') n_io_pairs
                call string_strip_blanks(ctmp2,nctmp2)
                write(junit,'(A)') &
                '        call ' // current%name(1:current%nname) //            &
                pblank(1:PNAME_MAX+1-current%nname) // '      (obj' //         &
                current%cpid(1:current%ncpid) // ',ntr,hd' // ctmp(1:nctmp) // &
                ',tr' // ctmp(1:nctmp) // ',hd' // ctmp2(1:nctmp2) // ',tr' // &
                ctmp2(1:nctmp2) // ')'
                write(junit,'(A)') &
                '        call pcpsx_post_process (' //                  &
                current%cpid(1:current%ncpid) // ',ntr,hd' //         &
                ctmp2(1:nctmp2) // ',tr' // ctmp2(1:nctmp2) // ')'
              else
                if (.not. null_action) then
!                 write(junit,'(A)') '! '
                  write(junit,'(A)') '      if (ntr /= NULL_ACTION) then'
                  null_action = .true.
                endif
                if (current%need_request(1:1) .eq. 'Y') then
                  call string_ii2cc (current%request,ctmp)
                  write(junit,'(A)') '        call pcpsx_filter_ntr (' //  &
                                                   trim(current%cpid) // ',ntr)'
                  write(junit,'(A)') '        if (ntr == LOOP_BACK) goto ' //  &
                                                trim(ctmp)
                endif
                write(junit,'(A)') &
                '        call pcpsx_pre_process  (' //    &
                current%cpid(1:current%ncpid) // ')'
                write(ctmp,'(I10)') n_io_pairs
                call string_strip_blanks(ctmp,nctmp)
                write(junit,'(A)') &
                '        call ' // current%name(1:current%nname) //            &
                pblank(1:PNAME_MAX+1-current%nname) // '      (obj' //         &
                current%cpid(1:current%ncpid) // ',ntr,hd' // ctmp(1:nctmp) // &
                ',tr' // ctmp(1:nctmp) // ')'
                write(junit,'(A)') &
                '        call pcpsx_post_process (' //              &
                current%cpid(1:current%ncpid) // ',ntr,hd' //     &
                ctmp(1:nctmp) // ',tr' // ctmp(1:nctmp) // ')'
              endif
              if (current%need_request(1:1) .eq. 'Y') then
                write(ctmp,'(I10)') i + 7000
                call string_strip_blanks(ctmp,nctmp)
                if (.not. in_fork) then
                  write(ctmp2,'(I10)') current%request
                  call string_strip_blanks(ctmp2,nctmp2)
                  write(junit,'(A)') &
                  '      if (ntr == FATAL_ERROR) goto ' // ctmp(1:nctmp) //  &
                     ';  if (ntr == NEED_TRACES) goto ' // ctmp2(1:nctmp2)
                else
                  write(ctmp4,'(I10)') last_fork
                  call string_strip_blanks(ctmp4,nctmp4)
                  write(junit,'(A)') &
                  '        if (ntr == FATAL_ERROR) goto ' // ctmp(1:nctmp)
                  write(junit,'(A)') &
                  '        if (ntr == NEED_TRACES) then'
                  call string_ii2cc (n_parallel_groups,ctmp)
                  call string_ii2cc (n_io_pairs,ctmp3)
                  write(junit,'(A)') &
                  '          call pcpsx_do_parallel_end ('//&
                  'p_obj' // trim(ctmp) // ',ntr,hd'//trim(ctmp3)//&
                  ',tr'//trim(ctmp3)//')'
                  write(junit,'(A)') &
                  '          goto ' // ctmp4(1:nctmp4)
                  write(junit,'(A)') &
                  '        endif'
                endif
                if (null_action) then
                  write(junit,'(A)') '      endif'
                  write(junit,'(A)') '!'
                  null_action = .false.
                else
                  write(junit,'(A)') '!'
                endif
              else
                write(ctmp,'(I10)') i + 7000
                call string_strip_blanks(ctmp,nctmp)
                write(junit,'(A)') &
                '        if (ntr == FATAL_ERROR) goto ' // ctmp(1:nctmp)
                if (null_action) then
                  write(junit,'(A)') '      endif'
                  write(junit,'(A)') '!'
                  null_action = .false.
                else
                  write(junit,'(A)') '!'
                endif
              endif
            endif
            previous  => current
            current   => current%next
          enddo

          if (in_fork) then
!           write(ctmp,'(I10)') current%parallel_end
            last_label = last_label + 10
            write(ctmp,'(I10)') last_label
            call string_strip_blanks(ctmp,nctmp)
            call string_ii2cc (n_parallel_groups,ctmp2)
            call string_ii2cc (tr_parallel(n_do_parallel),ctmp3)
            call string_ii2cc (n_io_pairs,ctmp4)
            if (.not. need_extra_io) then
              write(junit,'(A)')  &
                  ' '//ctmp(1:max(nctmp,5))//'call pcpsx_do_parallel_end   ('//&
                  'p_obj' // trim(ctmp2) // ',ntr,hd'//trim(ctmp4)//&
                  ',tr'//trim(ctmp4)//')'
            else
              n_io_pairs = n_io_pairs + 1
              write(junit,'(A)') &
                  ' '//ctmp(1:max(nctmp,5))//'call pcpsx_do_parallel_end   ('//&
                  'p_obj' // trim(ctmp2) // ',ntr,hd'//trim(ctmp3)//    &
                  ',tr'//trim(ctmp3)//',hd'//trim(ctmp4)//    &
                  ',tr'//trim(ctmp4)//')'
            endif
            call string_ii2cc (last_fork,ctmp)
            write(junit,'(A)') &
                            '      if (ntr == LOOP_BACK) goto ' //  trim(ctmp)
            write(ctmp,'(I10)') i + 8000
            call string_strip_blanks(ctmp,nctmp)
            write(junit,'(A)') &
                            '      if (ntr == SHUT_DOWN) goto ' // trim(ctmp)
            write(junit,'(A)') '!'
!           last_fork  = current%parallel_end
!           last_label = max(current%label,current%parallel_end)
            in_fork = .false.
            need_extra_io = .true.
          endif

          write(ctmp,'(I10)') i + 7000
          call string_strip_blanks(ctmp,nctmp)
          call string_ii2cc (n_parallel_groups,ctmp2)
          write(junit,'(A)') &
          ' '//ctmp(1:max(nctmp,5))//'call pcpsx_end_loop (p_obj'//trim(ctmp2) &
          //',ntr)'
          write(ctmp,'(I10)') last_label
          call string_strip_blanks(ctmp,nctmp)
          write(junit,'(A)') &
          '      if (ntr == LOOP_BACK) goto ' // ctmp(1:nctmp)
          write(ctmp,'(I10)') last_loop
          call string_strip_blanks(ctmp,nctmp)
          write(junit,'(A)') &
          '      if (ntr == GO_BACK) goto ' // ctmp(1:nctmp)
          write(junit,'(A)') &
          '      if (ntr /= NO_MORE_TRACES) error = .true.'
          write(junit,'(A)') &
          '! '
          current => loop_start_process
          first = .TRUE.
        endif

        write(junit,'(A)') &
        '!!!!! DELETE:'
        write(junit,'(A)') &
        '! '
        write(ctmp,'(I10)') i + 8000
        call string_strip_blanks(ctmp,nctmp)
        write(junit,'(A)') &
        ' ' //ctmp(1:max(nctmp,5)) // 'continue'

        write(junit,'(A)') &
        '      call pcpsx_wrapup_processing'

        do j=n_parallel_groups_start,n_parallel_groups_end
           call string_ii2cc (j,ctmp)
           write(junit,'(A)') &
          '      call pcpsx_do_parallel_delete (p_obj'// trim(ctmp) // ')'
        enddo
        write(junit,'(A)') &
        '! '

        write(ctmp,'(I10)') i + 9000
        call string_strip_blanks(ctmp,nctmp)
        write(junit,'(A)') &
        ' ' //ctmp(1:max(nctmp,5)) // 'continue'

        current => loop_start_process
        first = .TRUE.
        do
          if (.not. associated(current)) exit
          if (current%start_loop) then
            if (first) then
              first = .FALSE.
            else
              exit
            endif
          endif
          write(junit,'(A)') &
          '      if (associated(obj'//current%cpid(1:current%ncpid)//  &
          '))'//pblank(1:max_pid_digits+1-current%ncpid)           //  &
          'call ' // current%name(1:current%nname) // '_delete'    //  &
          pblank(1:PNAME_MAX+1-current%nname) // '(obj' //             &
          current%cpid(1:current%ncpid) // ')'
          current => current%next
        enddo

        write(junit,'(A)') &
        '! '
        do j = j1,j2
          call string_ii2cc (j, ctmp )
          write(junit,'(A)')  &
          '      deallocate(hd'//trim(ctmp)//',stat=ierr)'
          write(junit,'(A)')  &
          '      if (ierr /= 0) then'
          write(junit,'(A)')  &
         '        call pcps_print (''Unable to deallocate hd'//trim(ctmp)//''')'
          write(junit,'(A)')  &
          '        error = .true.'
          write(junit,'(A)')  &
          '        goto 9999'
          write(junit,'(A)')  &
          '      endif'
          write(junit,'(A)')  &
          '      deallocate(tr'//trim(ctmp)//',stat=ierr)'
          write(junit,'(A)')  &
          '      if (ierr /= 0) then'
          write(junit,'(A)')  &
         '        call pcps_print (''Unable to deallocate tr'//trim(ctmp)//''')'
          write(junit,'(A)')  &
          '        error = .true.'
          write(junit,'(A)')  &
          '        goto 9999'
          write(junit,'(A)')  &
          '      endif'
          write(junit,'(A)') &
          '! '
        enddo
        write(junit,'(A)') &
        '      if (error) go to 9999'

        if (.not. loop_start_process%start_loop) then
          do
            loop_start_process => loop_start_process%next
            if (.not. associated(loop_start_process)) exit
            if (loop_start_process%start_loop) exit
          enddo
          if (.not. associated(loop_start_process)) exit
        endif

        loop_start_process => loop_start_process%next
        do
          if (.not. associated(loop_start_process)) exit
          if (loop_start_process%start_loop) exit
          loop_start_process => loop_start_process%next
        enddo
      enddo

      write(junit,'(A)') &
      ' 9999 continue'
      write(junit,'(A)') &
      '      call pcpsx_finish_processing (error)'

      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      end program '//trim(jobname)//'_cps'


      write(junit,'(A)') '! '
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!! SIGNAL HANDLER !!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      subroutine cpssig_shutdown (signal)'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      use program_global_data'
      write(junit,'(A)') '      use cps_module'
      write(junit,'(A)') '      use cio_module'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      integer,intent(in) :: signal          !argument'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      integer,parameter  :: kill_sig=12     !SIGUSR2'
      write(junit,'(A)') '      integer            :: lun             !local'
      write(junit,'(A)') '      logical,save       :: flag0 = .true.  !local'

      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      logical,save       :: flag'//trim(current%cpid)//  &
        ' = .true.  !local'
        current => current%next
      enddo

      write(junit,'(A)') '! '

      write(junit,'(A)') '      if (flag0) then'
      write(junit,'(A)') '        call cps_write_accounting_signal (signal)'
      write(junit,'(A)') '        call cps_print_current_status()'
      write(junit,'(A)') '      endif'

      write(junit,'(A)') '! '

      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      if (flag'//trim(current%cpid)//') then'
        write(junit,'(A)') &
        '        flag'//trim(current%cpid)//' = .false.'
        write(junit,'(A)') &
        '        if (associated(obj'//current%cpid(1:current%ncpid)//')) then'
        write(junit,'(A)') &
        '          call cpssig_print ("'//current%name(1:current%nname)// &
                   '",'//current%cpid(1:current%ncpid)//')'
        if (current%signal_handler .eq. 'YES') then
          write(junit,'(A)') &
        '          call '//current%name(1:current%nname)//'_wrapup'// &
                   pblank(1:PNAME_MAX+1-current%nname)//'(obj'//      &
                   current%cpid(1:current%ncpid)//',signal=signal)'
        else
          write(junit,'(A)') &
        '          call '//current%name(1:current%nname)//'_wrapup'// &
                   pblank(1:PNAME_MAX+1-current%nname)//'(obj'//      &
                   current%cpid(1:current%ncpid)//')'
        endif
        write(junit,'(A)') &
        '        endif'
        write(junit,'(A)') &
        '      endif'
        write(junit,'(A)') &
        '! '
        current => current%next
      enddo
      write(junit,'(A)') '      if (flag0) then'
      write(junit,'(A)') '        flag0 = .false.'
      write(junit,'(A)') '        if (pcps_current_worker_num.eq.0) &'
      write(junit,'(A)') '          call cps_finish_processing (signal=signal)'
      write(junit,'(A)') '! '
      write(junit,'(A)') '        call cio_finalize()'
      write(junit,'(A)') '        call pfio_exit()'
      write(junit,'(A)') '! '
      write(junit,'(A)') '        if(signal.ne.kill_sig) then'
      write(junit,'(A)') '          call pcps_kill_parallel_cpus (kill_sig)'
      write(junit,'(A)') '        endif'
      write(junit,'(A)') '! '
      write(junit,'(A)') '        lun = cps_get_lun()'
      write(junit,'(A)') '        close(unit=lun,status="keep")'
      write(junit,'(A)') '      endif'
      write(junit,'(A)') '      return'
      write(junit,'(A)') '      end subroutine cpssig_shutdown'
      write(junit,'(A)') '! '
      write(junit,'(A)') '! '

      write(junit,'(A)') '      subroutine cpssig_print (process,ipn)'
      write(junit,'(A)') '      use pcps_module'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      character(len=*),intent(in) :: process      '//&
                                '!argument'
      write(junit,'(A)') '      integer         ,intent(in) :: ipn          '//&
                                '!argument'
      write(junit,'(A)') '      character(len=132)          :: mess         '//&
                                '!local'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      write(mess,*) "+++++++ ",process," ipn=",'//  &
                         'ipn,"( worker",pcps_current_worker_num,") wrapup '//&
                         'called by signal handler"'
      write(junit,'(A)') '      call pcps_print (mess,2)'
      write(junit,'(A)') '      return'
      write(junit,'(A)') '      end subroutine cpssig_print'

      return
      end subroutine buildjob_write_parallel


!!------------------------- buildjob_write_main ----------------------------!!
!!------------------------- buildjob_write_main ----------------------------!!
!!------------------------- buildjob_write_main ----------------------------!!


      subroutine buildjob_write_main(junit,start)

      integer              ,intent(in)          :: junit              ! argument
      type(PROCESS),pointer                     :: start              ! argument

      integer                               :: i                      ! local
      integer                               :: j                      ! local
      integer                               :: j1                     ! local
      integer                               :: j2                     ! local
      type(PROCESS),pointer                 :: current                ! local
      type(PROCESS),pointer                 :: loop_start_process     ! local
      logical                               :: first                  ! local
      integer                               :: label                  ! local
      integer                               :: last_label             ! local
      type(CARD_POINTER)                    :: tag_card               ! local
      character(len=PNAME_MAX+1)            :: pblank                 ! local
      character(len=80)                     :: ctmp                   ! local
      integer                               :: nctmp                  ! local
      character(len=80)                     :: ctmp2                  ! local
      integer                               :: nctmp2                 ! local
      character(len=80)                     :: ctmp3                  ! local
      character(len=80)                     :: ctmp4                  ! local
      integer                               :: nwih                   ! local
      integer                               :: ndpt                   ! local
      integer                               :: ntr                    ! local
      integer                               :: nscratch_max    = 0    ! local
      integer                               :: nstore_max      = 0    ! local
      integer                               :: ndisk_max       = 0    ! local
      integer                               :: ncart_max       = 0    ! local
      integer                               :: nmag_max        = 0    ! local
      integer                               :: nloops          = 0    ! local
      integer                               :: n_io_pairs      = 0    ! local
      integer                               :: n_io_pairs_total= 0    ! local
      integer                               :: nstore_loop     = 0    ! local
      integer                               :: nscratch_loop   = 0    ! local
      integer                               :: ntr_x(MAX_IO_PAIRS)    ! local
      integer                               :: hd_x_len(MAX_IO_PAIRS) ! local
      integer                               :: tr_x_len(MAX_IO_PAIRS) ! local

      do i=1,PNAME_MAX+1
        pblank(i:i) = ' '
      enddo


      ntr_x     = 0
      hd_x_len  = 0
      tr_x_len  = 0

      nwih      = 0
      ndpt      = 0
      ntr       = 0
      label     = 0
      i         = 0
      current => start
      do
        if (.not. associated(current)) exit
        tag_card%card => current%tag_card_start(CONTROLS_TAG)%card
        first = .TRUE.
        i = i + 1
        write(STDOUT,*) 'process #',i,' = ',current%name(1:current%nname)
        call pc_next
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            call pc_put_control_card(tag_card%card%data)
            first = .FALSE.
          else
            call pc_add_control_card(tag_card%card%data)
          endif
          tag_card%card => tag_card%card%next
        enddo

        call pc_get_control ('NEED_REQUEST'  ,current%need_request  )
        call pc_get_control ('NEED_LABEL'    ,current%need_label    )
        call pc_get_control ('TWOSETS'       ,current%twosets       )
        call pc_get_control ('SETUP_ONLY'    ,current%setup_only    )
        call pc_get_control ('SIGNAL'        ,current%signal_handler)
        call pc_get_control ('NSCRATCH'      ,current%nscratch      )
        call pc_get_control ('NSTORE'        ,current%nstore        )
        call pc_get_control ('NDISK'         ,current%ndisk         )
        call pc_get_control ('IFTD'          ,current%iftd          )

        call string_to_upper (current%need_request  )
        call string_to_upper (current%need_label    )
        call string_to_upper (current%twosets       )
        call string_to_upper (current%setup_only    )
        call string_to_upper (current%signal_handler)
        call string_to_upper (current%iftd          )

        if (current%setup_only .eq. 'YES') then
         current%need_request = 'NO'
         current%need_label   = 'NO'
         current%twosets      = 'NO'
        endif

        tag_card%card => current%tag_card_start(GLOBALS_TAG)%card
        first = .TRUE.
        call pc_next
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            call pc_put_global_card(tag_card%card%data)
            first = .FALSE.
          else
            call pc_add_global_card(tag_card%card%data)
          endif
          tag_card%card => tag_card%card%next
        enddo

        current%ltr  = ndpt
        current%lhd  = nwih

        if (pc_global_keyword_present('NWIH')) call pc_get_global ('NWIH' ,nwih)
        if (pc_global_keyword_present('NDPT')) call pc_get_global ('NDPT' ,ndpt)
        if (pc_global_keyword_present('NUMTR')) call pc_get_global('NUMTR',ntr )
        current%ntr  = ntr

        if (current%twosets(1:1) .eq. 'Y') then
          current%ltr2 = ndpt
          current%lhd2 = nwih
        else
          current%ltr  = ndpt
          current%lhd  = nwih
        endif

        nscratch_max = max(nscratch_max , current%nscratch)
        nstore_max   = nstore_max + current%nstore

        if (current%setup_only .eq. 'NO') then
          if ((nloops .eq. 0 .and. current%ntr .ne. 0) .or.            &
              (current%need_label(1:1) .eq. 'Y' .and.                  &
               current%need_request(1:1) .eq. 'N')) then
            nloops = nloops + 1
            current%start_loop = .TRUE.
            n_io_pairs = n_io_pairs + 1
          endif
        endif

        if (current%need_request(1:1) .eq. 'Y') then
          current%request = label
        endif

        if (current%need_label(1:1) .eq. 'Y') then
          label = label + 10
          current%label = label
        endif

        if (current%twosets(1:1) .eq. 'Y') then
          tr_x_len(n_io_pairs) = max(tr_x_len(n_io_pairs),current%ltr)
          hd_x_len(n_io_pairs) = max(hd_x_len(n_io_pairs),current%lhd)
          n_io_pairs = n_io_pairs + 1
          tr_x_len(n_io_pairs) = current%ltr2
          hd_x_len(n_io_pairs) = current%lhd2
        else if (current%ntr.ne.0) then
          tr_x_len(n_io_pairs) = max(tr_x_len(n_io_pairs),current%ltr)
          hd_x_len(n_io_pairs) = max(hd_x_len(n_io_pairs),current%lhd)
        endif

        if (current%ntr.gt.0)  then
          if (current%start_loop) then
            ntr_x(n_io_pairs) = current%ntr
          else
            ntr_x(n_io_pairs) = max(ntr_x(n_io_pairs),current%ntr)
          endif
        endif

        current%n_io_pairs = n_io_pairs
        current => current%next
      enddo

      total_memory = 5242880 + nscratch_max + nstore_max

      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      use named_constants_module'
      write(junit,'(A)') &
      '      use cps_module'
      write(junit,'(A)') &
      '! '
      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      use ' // current%name(1:current%nname) // '_module'
        current => current%next
      enddo
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      implicit none'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      public'
      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      type(' // current%name(1:current%nname) //               &
        '_struct),pointer  ' // pblank(1:PNAME_MAX+1-current%nname) //  &
        ':: obj' // current%cpid(1:current%ncpid)
        current => current%next
      enddo
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      end module program_global_data'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!! PROGRAM MAIN !!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      program '//trim(jobname)//'_cps'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      use program_global_data'
      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '!!!!! DECLARATIONS:'
      write(junit,'(A)') &
      '! '

      do i = 1,n_io_pairs
        write(ctmp,'(''tr'',I3,''(:,:)'')') i
        call string_strip_blanks(ctmp,nctmp)
        write(ctmp2,'(''hd'',I3,''(:,:)'')') i
        call string_strip_blanks(ctmp2,nctmp2)
        write(junit,'(A)') &
        '      double precision, pointer :: ' // ctmp2(1:nctmp2)
        write(junit,'(A)') &
        '      real            , pointer :: ' // ctmp(1:nctmp)
      enddo

      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      integer :: ntr, ierr'
      write(junit,'(A)') &
      '      logical :: error'

      write(junit,'(A)') &
      '! '

      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)')  &
          '      nullify (obj'//current%cpid(1:current%ncpid)//')'
        current => current%next
      enddo

      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      call cps_start_processing'

      write(junit,'(A)') &
      '! '
      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      call cps_print_rcs_ident '//  &
        '(trim('//current%name(1:current%nname)//'_ident))'
        current => current%next
      enddo
      write(junit,'(A)') '      call cps_custom_rcs_ident'

      n_io_pairs_total = n_io_pairs
      n_io_pairs = 0
      loop_start_process => start
      do i = 1,max(nloops,1)
        n_io_pairs = n_io_pairs + 1
        if (.not. associated(loop_start_process)) exit
        current => loop_start_process
        write(junit,'(A)') &
        '! '
        write(junit,'(A)') &
        '!!!!! ALLOCATE ARRAYS:'
        write(junit,'(A)') &
        '! '
        j1 = max(loop_start_process%n_io_pairs,1)
        j2 = j1
        current => loop_start_process
        first = .TRUE.
        do
          if (.not. associated(current)) exit
          if (current%start_loop) then
            if (first) then
              first = .FALSE.
            else
              exit
            endif
          endif
          j2 = current%n_io_pairs
          current => current%next
        enddo
        do j = j1,j2
          call string_ii2cc (j          , ctmp )
          call string_ii2cc (ntr_x(j)   , ctmp2)
          call string_ii2cc (hd_x_len(j), ctmp3)
          call string_ii2cc (tr_x_len(j), ctmp4)
          write(junit,'(A)')  &
          '      ntr = '//trim(ctmp2)
          write(junit,'(A)')  &
          '      allocate(hd'//trim(ctmp)//'('//trim(ctmp3)//','//  &
                          'ntr),stat=ierr)'
          write(junit,'(A)')  &
          '      if (ierr /= 0) then'
          write(junit,'(A)')  &
          '        call cps_print (''Unable to allocate hd'//trim(ctmp)//''')'
          write(junit,'(A)')  &
          '        error = .true.'
          write(junit,'(A)')  &
          '        goto 9999'
          write(junit,'(A)')  &
          '      endif'
          write(junit,'(A)')  &
          '      allocate(tr'//trim(ctmp)//'('//trim(ctmp4)//','//  &
                          'ntr),stat=ierr)'
          write(junit,'(A)')  &
          '      if (ierr /= 0) then'
          write(junit,'(A)')  &
          '        call cps_print (''Unable to allocate tr'//trim(ctmp)//''')'
          write(junit,'(A)')  &
          '        error = .true.'
          write(junit,'(A)')  &
          '        goto 9999'
          write(junit,'(A)')  &
          '      endif'
        enddo

        write(junit,'(A)') &
        '! '
        write(junit,'(A)') &
        '!!!!! SETUPS:'
        write(junit,'(A)') &
        '! '
        write(junit,'(A)') &
        '      call cps_start_setups    ! stops if there is an error.'
        write(junit,'(A)') &
        '! '
        current => loop_start_process
        first = .TRUE.
        do
          if (.not. associated(current)) exit
          if (current%start_loop) then
            if (first) then
              first = .FALSE.
            else
              exit
            endif
          endif
          write(junit,'(A)') &
          '      call cps_pre_setup; call '//current%name(1:current%nname)//&
          '_create'//pblank(1:PNAME_MAX+1-current%nname)//'(obj'//          &
          current%cpid(1:current%ncpid)//');'//                             &
          pblank(1:max_pid_digits+1-current%ncpid) //'call cps_post_setup'
          current => current%next
        enddo
        write(junit,'(A)') &
        '! '
        write(ctmp,'(I10)') i + 8000
        call string_strip_blanks(ctmp,nctmp)
        write(junit,'(A)') &
        '      call cps_finish_setups (error); if (error) go to '//ctmp(1:nctmp)
        write(junit,'(A)') &
        '! '
        if (nloops .gt. 0) then
          write(junit,'(A)') &
          '!!!!! EXECUTION:'
          write(junit,'(A)') &
          '! '
          write(junit,'(A)') &
          '      ntr = NEED_TRACES'
          write(junit,'(A)') &
          '! '

          current => loop_start_process
          first = .TRUE.
          do
            if (.not. associated(current)) exit
            if (current%start_loop) then
              if (first) then
                first = .FALSE.
              else
                exit
              endif
            endif
            if (current%setup_only.ne.'YES') then
              if (current%need_label(1:1) .eq. 'Y') then
                write(ctmp,'(I10)') current%label
                call string_strip_blanks(ctmp,nctmp)
                write(junit,'(A)') &
                ' ' // ctmp(1:max(nctmp,5)) // 'call cps_top_of_loop  (' //  &
                current%cpid(1:current%ncpid) // ')'
                last_label = current%label
              endif
              if (current%twosets(1:1) .eq. 'Y') then
                write(junit,'(A)') &
                '      call cps_pre_process  (' //    &
                current%cpid(1:current%ncpid) // ')'
                write(ctmp,'(I10)') n_io_pairs
                call string_strip_blanks(ctmp,nctmp)
                n_io_pairs = n_io_pairs + 1
                write(ctmp2,'(I10)') n_io_pairs
                call string_strip_blanks(ctmp2,nctmp2)
                write(junit,'(A)') &
                '      call ' // current%name(1:current%nname) //              &
                pblank(1:PNAME_MAX+1-current%nname) // '    (obj' //           &
                current%cpid(1:current%ncpid) // ',ntr,hd' // ctmp(1:nctmp) // &
                ',tr' // ctmp(1:nctmp) // ',hd' // ctmp2(1:nctmp2) // ',tr' // &
                ctmp2(1:nctmp2) // ')'
                write(junit,'(A)') &
                '      call cps_post_process (' //                  &
                current%cpid(1:current%ncpid) // ',ntr,hd' //       &
                ctmp2(1:nctmp2) // ',tr' // ctmp2(1:nctmp2) // ')'
              else
                write(junit,'(A)') &
                '      call cps_pre_process  (' //    &
                current%cpid(1:current%ncpid) // ')'
                write(ctmp,'(I10)') n_io_pairs
                call string_strip_blanks(ctmp,nctmp)
                write(junit,'(A)') &
                '      call ' // current%name(1:current%nname) //              &
                pblank(1:PNAME_MAX+1-current%nname) // '    (obj' //           &
                current%cpid(1:current%ncpid) // ',ntr,hd' // ctmp(1:nctmp) // &
                ',tr' // ctmp(1:nctmp) // ')'
                write(junit,'(A)') &
                '      call cps_post_process (' //              &
                current%cpid(1:current%ncpid) // ',ntr,hd' //   &
                ctmp(1:nctmp) // ',tr' // ctmp(1:nctmp) // ')'
              endif
              if (current%need_request(1:1) .eq. 'Y') then
                write(ctmp,'(I10)') i + 7000
                call string_strip_blanks(ctmp,nctmp)
                write(ctmp2,'(I10)') current%request
                call string_strip_blanks(ctmp2,nctmp2)
                write(junit,'(A)') &
                '      if (ntr == FATAL_ERROR) goto ' // ctmp(1:nctmp) //  &
                   ';  if (ntr == NEED_TRACES) goto ' // ctmp2(1:nctmp2)
              else
                write(ctmp,'(I10)') i + 7000
                call string_strip_blanks(ctmp,nctmp)
                write(junit,'(A)') &
                '      if (ntr == FATAL_ERROR) goto ' // ctmp(1:nctmp)
              endif
              write(junit,'(A)') &
              '! '
            endif
            current => current%next
          enddo
          write(ctmp,'(I10)') i + 8000
          call string_strip_blanks(ctmp,nctmp)
          write(junit,'(A)') &
          '      if (ntr == NO_MORE_TRACES) goto ' // ctmp(1:nctmp)
          write(junit,'(A)') &
          '      ntr = NEED_TRACES'
          write(ctmp,'(I10)') last_label
          call string_strip_blanks(ctmp,nctmp)
          write(junit,'(A)') &
          '      goto ' // ctmp(1:nctmp)
          write(junit,'(A)') &
          '! '
          write(ctmp,'(I10)') i + 7000
          call string_strip_blanks(ctmp,nctmp)
          write(junit,'(A)') &
          ' ' // ctmp(1:max(nctmp,5)) // 'error = .true.'
          current => loop_start_process
          first = .TRUE.
          write(junit,'(A)') &
          '! '
        endif

        write(junit,'(A)') &
        '!!!!! DELETE:'
        write(junit,'(A)') &
        '! '
        write(ctmp,'(I10)') i + 8000
        call string_strip_blanks(ctmp,nctmp)
        write(junit,'(A)') &
        ' ' //ctmp(1:max(nctmp,5)) // 'continue'
        current => loop_start_process
        first = .TRUE.
        do
          if (.not. associated(current)) exit
          if (current%start_loop) then
            if (first) then
              first = .FALSE.
            else
              exit
            endif
          endif
          write(junit,'(A)') &
          '      if (associated(obj'//current%cpid(1:current%ncpid)//  &
          '))'//pblank(1:max_pid_digits+1-current%ncpid)           //  &
          'call ' // current%name(1:current%nname) // '_delete'    //  &
          pblank(1:PNAME_MAX+1-current%nname) // '(obj' //             &
          current%cpid(1:current%ncpid) // ')'
          current => current%next
        enddo

        write(junit,'(A)') &
        '! '
        do j = j1,j2
          call string_ii2cc (j,ctmp)
          write(junit,'(A)')  &
          '      deallocate(hd'//trim(ctmp)//',stat=ierr)'
          write(junit,'(A)')  &
          '      if (ierr /= 0) then'
          write(junit,'(A)')  &
          '        call cps_print (''Unable to deallocate hd'//trim(ctmp)//''')'
          write(junit,'(A)')  &
          '        error = .true.'
          write(junit,'(A)')  &
          '        goto 9999'
          write(junit,'(A)')  &
          '      endif'
          write(junit,'(A)')  &
          '      deallocate(tr'//trim(ctmp)//',stat=ierr)'
          write(junit,'(A)')  &
          '      if (ierr /= 0) then'
          write(junit,'(A)')  &
          '        call cps_print (''Unable to deallocate tr'//trim(ctmp)//''')'
          write(junit,'(A)')  &
          '        error = .true.'
          write(junit,'(A)')  &
          '        goto 9999'
          write(junit,'(A)')  &
          '      endif'
          write(junit,'(A)') &
          '! '
        enddo
        write(junit,'(A)') &
        '      if (error) go to 9999'

        if (.not. loop_start_process%start_loop) then
          do
            loop_start_process => loop_start_process%next
            if (.not. associated(loop_start_process)) exit
            if (loop_start_process%start_loop) exit
          enddo
          if (.not. associated(loop_start_process)) exit
        endif

        loop_start_process => loop_start_process%next
        do
          if (.not. associated(loop_start_process)) exit
          if (loop_start_process%start_loop) exit
          loop_start_process => loop_start_process%next
        enddo
      enddo

      write(junit,'(A)') &
      ' 9999 continue'
      write(junit,'(A)') &
      '      call cps_finish_processing (error)'

      write(junit,'(A)') &
      '! '
      write(junit,'(A)') &
      '      stop'
      write(junit,'(A)') &
      '      end program '//trim(jobname)//'_cps'

      write(junit,'(A)') '! '
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!! SIGNAL HANDLER !!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') &
      '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      subroutine cpssig_shutdown (signal)'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      use program_global_data'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      integer,intent(in) :: signal          !argument'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      logical,save       :: flag0 = .true.  !local'

      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      logical,save       :: flag'//trim(current%cpid)//  &
        ' = .true.  !local'
        current => current%next
      enddo

      write(junit,'(A)') '! '

      write(junit,'(A)') '      if (flag0) then'
      write(junit,'(A)') '        call cps_write_accounting_signal (signal)'
      write(junit,'(A)') '        call cps_print_current_status()'
      write(junit,'(A)') '      endif'

      write(junit,'(A)') '! '

      current => start
      do
        if (.not. associated(current)) exit
        write(junit,'(A)') &
        '      if (flag'//trim(current%cpid)//') then'
        write(junit,'(A)') &
        '        flag'//trim(current%cpid)//' = .false.'
        write(junit,'(A)') &
        '        if (associated(obj'//current%cpid(1:current%ncpid)//')) then'
        write(junit,'(A)') &
        '          call cps_print ("+++++++ '//current%name(1:current%nname)// &
                   ' ipn='//current%cpid(1:current%ncpid)//  &
                   ' wrapup called by signal handler")'
        if (current%signal_handler .eq. 'YES') then
          write(junit,'(A)') &
        '          call '//current%name(1:current%nname)//'_wrapup'// &
                   pblank(1:PNAME_MAX+1-current%nname)//'(obj'//      &
                   current%cpid(1:current%ncpid)//',signal=signal)'
        else
          write(junit,'(A)') &
        '          call '//current%name(1:current%nname)//'_wrapup'// &
                   pblank(1:PNAME_MAX+1-current%nname)//'(obj'//      &
                   current%cpid(1:current%ncpid)//')'
        endif
        write(junit,'(A)') &
        '        endif'
        write(junit,'(A)') &
        '      endif'
        write(junit,'(A)') &
        '! '
        current => current%next
      enddo
      write(junit,'(A)') '      if (flag0) then'
      write(junit,'(A)') '        flag0 = .false.'
      write(junit,'(A)') '        call cps_finish_processing (signal=signal)'
      write(junit,'(A)') '      endif'
      write(junit,'(A)') '! '
      write(junit,'(A)') '      return'
      write(junit,'(A)') '      end subroutine cpssig_shutdown'


      return
      end subroutine buildjob_write_main


!!------------------------- buildjob_write_setup ---------------------------!!
!!------------------------- buildjob_write_setup ---------------------------!!
!!------------------------- buildjob_write_setup ---------------------------!!


      subroutine buildjob_write_setup (junit,start,jobmode,batchsystem,  &
                                       platform,mpi_version)

      integer              ,intent(in)          :: junit              ! argument
      type(PROCESS),pointer                     :: start              ! argument
      character(len=*)     ,intent(in)          :: jobmode            ! argument
      character(len=*)     ,intent(in)          :: batchsystem        ! argument
      character(len=*)     ,intent(in)          :: platform           ! argument
      character(len=*)     ,intent(in)          :: mpi_version        ! argument

      character(len=8)                          :: cnumNodes,cnumCpus ! local
      character(len=132)                        :: beginFile,outFile
      character(len=400)                        :: cmd,cmda             ! local

      call string_ii2cc(num_nodes,cnumNodes)
      call string_ii2cc(num_cpus,cnumCpus)
      beginFile=trim(frontend_path) // '/' // trim(jobname) // '.TEMPbegin'
      outFile=trim(frontend_path) // '/' // trim(jobname) // '.begin'
!
      cmd='cp ' // trim(templateHome) // '/job_template_begin ' // trim(beginFile)
      call putsys_cmd(cmd)
      cmda=' '
      if(queue(1:1).eq.'S')then  ! remove the ppn=2 from the PBS command
        cmda = '-e s+:ppn=2++ '
      endif
      cmd='sed -e s+QQQQQ+' // trim(queue)           // '+g ' // &
              '-e s+NNNNN+' // trim(cnumNodes)       // '+g ' // &
              '-e s+UUUUU+' // trim(frontend_user)   // '+g ' // &
              '-e s+PPPPP+' // trim(frontend_path)   // '+g ' // &
              '-e s+CCCCC+' // trim(cnumCpus)        // '+g ' // &
              '-e s+JJJJJ+' // trim(jobname)         // '+g ' // &
              trim(cmda) // ' ' // &
              trim(beginFile) // '>' // &
              trim(outFile)
      call putsys_cmd(cmd)


      return
      end subroutine buildjob_write_setup


!!------------------------- buildjob_write_data ----------------------------!!
!!------------------------- buildjob_write_data ----------------------------!!
!!------------------------- buildjob_write_data ----------------------------!!


      subroutine buildjob_write_data(junit,start)


      integer              ,intent(in)          :: junit              ! argument
      type(PROCESS),pointer                     :: start              ! argument

      logical                                   :: first              ! local
      type(PROCESS),pointer                     :: current            ! local
      type(CARD_POINTER)                        :: tag_card           ! local
      character(len=TAG_MAX_LEN)                :: tags(NUM_TAGS)     ! local
      integer                                   :: ntags(NUM_TAGS)    ! local

      tags(1)  = ' <GLOBALS>'
      tags(2)  = ' <CONTROLS>'
      tags(3)  = ' <PARAMETERS>'
      tags(4)  = ' <DATA_CARD>'
      ntags(1) = 10
      ntags(2) = 11
      ntags(3) = 13
      ntags(4) = 12

      write(junit,'(A)') '/EOF'
      write(junit,'(A)') 'cat >process_parameters << ''/EOFA'''

      current => start
      do
        if (.not. associated(current)) exit
        tag_card%card => current%tag_card_start(PARAMETERS_TAG)%card
        first = .TRUE.
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            write(junit,'(A)') '<PROCESS name="' //   &
                                 current%name(1:current%nname) // '">'
            first = .FALSE.
          endif
          write(junit,'(A)') trim(tag_card%card%data)
          tag_card%card => tag_card%card%next
        enddo
        write(junit,'(A)') '</PROCESS>'
        current => current%next
      enddo

      write(junit,'(A)') '/EOFA'

      return
      end subroutine buildjob_write_data


!!----------------- ------- buildjob_write_jcl -----------------------------!!
!!----------------- ------- buildjob_write_jcl -----------------------------!!
!!----------------- ------- buildjob_write_jcl -----------------------------!!


      subroutine buildjob_write_jcl (junit,start,jobmode,batchsystem,  &
                                     platform,mpi_version)

      use getsys_module

      integer              ,intent(in)          :: junit               !argument
      type(PROCESS),pointer                     :: start               !argument
      character(len=*)     ,intent(in)          :: jobmode             !argument
      character(len=*)     ,intent(in)          :: batchsystem         !argument
      character(len=*)     ,intent(in)          :: platform            !argument
      character(len=*)     ,intent(in)          :: mpi_version         !argument

      character(len=1024)                       :: cmd                 
      integer                                   :: nextra_libs = 0     
      integer                                   :: nextra_lmrk = 0    
      character(len=8)                          :: cnumCpus,cnumNodes 
      character(len=80)                         :: lmrk_dot_so = ''    
      character(len=360)       :: midjclFile,compileFile,outFile,endFile,custFile
      integer                                   :: nobjs = 0  
      integer                                   :: i,k         


      call string_ii2cc(num_nodes,cnumNodes)
      call string_ii2cc(num_cpus,cnumCpus)
      compileFile=trim(frontend_path) // '/' // trim(jobname) // '.TEMPcompile'
      outFile=trim(frontend_path) // '/' // trim(jobname) // '.compile'
!single cpu  or mpi template
      if(num_cpus.eq.1)then
        ! B queue?
        if(queue(1:1).eq.'B')then
          midjclFile='job_template_mid_singleBq'
        else
          midjclFile='job_template_mid_single'
        endif
        compileFile='job_template_compile_single_' // trim(platform)
      else
        midjclFile='job_template_mid_mpi'
        compileFile='job_template_compile_mpi_' // trim(platform)
      endif
!compile template begin
      cmd='sed -e s+JJJJJ+'  // trim(jobname)  // '+g ' // &
              '-e s+UUUUU+' // trim(frontend_user)   // '+g ' // &
               trim(templateHome) // '/' // 'job_template_compile_begin' // '>' // &
               trim(outFile)
      call putsys_cmd(cmd)
!custom compile 
      do i=1,ncustom_compile
        k=index(custom_compile(i),'.f90')
        if(k.gt.0)then
          custFile=trim(templateHome) // '/' // 'job_template_compile_custom_f_' // &
                   trim(platform)
        else
          custFile=trim(templateHome) // '/' // 'job_template_compile_custom_c_' // &
                   trim(platform)
        endif
        cmd='sed -e s+CCCCC+'  // trim(custom_compile(i)) // '+g ' // &
               trim(custFile) // '>>' // &
               trim(outFile)
        call putsys_cmd(cmd)
      enddo
!remaining compile template
      cmd='sed -e s+PPPPP+'  // trim(frontend_path)  // '+g ' // &
              '-e s+UUUUU+' // trim(frontend_user)   // '+g ' // &
              '-e s+JJJJJ+' // trim(jobname)         // '+g ' // &
               trim(templateHome) // '/' // trim(compileFile) // '>>' // &
               trim(outFile)
      call putsys_cmd(cmd)

      outFile=trim(frontend_path) // '/' // trim(jobname) // '.mid'

      cmd='sed -e s+PPPPP+'  // trim(frontend_path)  // '+g ' // &
              '-e s+UUUUU+' // trim(frontend_user)   // '+g ' // &
              '-e s+NNNNN+' // trim(cnumNodes)       // '+g ' // &
              '-e s+JJJJJ+' // trim(jobname)         // '+g ' // &
               trim(templateHome) // '/' // trim(midjclFile)  // &
               '>' // &
               trim(outFile)
      call putsys_cmd(cmd)

      endFile=trim(frontend_path) // '/' // trim(jobname) // '.TEMPend'
      outFile=trim(frontend_path) // '/' // trim(jobname) // '.end'
      cmd='cp ' // trim(templateHome) // '/job_template_end ' // trim(endFile)
      call putsys_cmd(cmd)

      cmd='sed -e s+PPPPP+'  // trim(frontend_path)  // '+g ' // &
              '-e s+QQQQQ+' // trim(queue)           // '+g ' // &
              '-e s+NNNNN+' // trim(cnumNodes)       // '+g ' // &
              '-e s+UUUUU+' // trim(frontend_user)   // '+g ' // &
              '-e s+CCCCC+' // trim(cnumCpus)        // '+g ' // &
              '-e s+JJJJJ+' // trim(jobname)         // '+g ' // &
               trim(endFile) // '>' // &
               trim(outFile)
      call putsys_cmd(cmd)

!   edit the cleanup file
      outFile=trim(frontend_path) // '/' // trim(jobname) // '.cleanup'
      cmd='sed -e s+PPPPP+'  // trim(frontend_path)   // '+g ' // &
              '-e s+JJJJJ+' // trim(jobname)          // '+g ' // &
           trim(templateHome) // '/job_template_cleanup > '    // &
           trim(outFile)      
      call putsys_cmd(cmd)
      call putsys_cmd ('chmod +x ' // outFile)




      return
      end subroutine buildjob_write_jcl


!!------------------------- buildjob_write_cmd -----------------------------!!
!!------------------------- buildjob_write_cmd -----------------------------!!
!!------------------------- buildjob_write_cmd -----------------------------!!


      subroutine buildjob_write_cmd(junit,start,tag)

      integer              ,intent(in)          :: junit              ! argument
      type(PROCESS),pointer                     :: start              ! argument
      character(len=*)     ,intent(in)          :: tag                ! argument

      type(PROCESS),pointer                     :: current            ! local
      type(CARD_POINTER)                        :: tag_card           ! local
      logical                                   :: first              ! local
      character(len=1024),pointer               :: array(:)           ! local
      integer                                   :: narray             ! local
      integer                                   :: i                  ! local

      nullify(array)
      call pc_frontend_update (3)
      current => start
      do
        if (.not. associated(current)) exit
        tag_card%card => current%tag_card_start(CONTROLS_TAG)%card
        first = .true.
        call pc_next
        do
          if (.not. associated(tag_card%card)) exit
          if (first) then
            call pc_put_control_card(tag_card%card%data)
            first = .FALSE.
          else
            call pc_add_control_card(tag_card%card%data)
          endif
          tag_card%card => tag_card%card%next
        enddo
        narray = 0
        call pc_alloc_control (tag,array,narray)
        if (narray .gt. 0) then
          do i=1,narray
            write(junit,'(A)') trim(array(i))
          enddo
        endif
        current => current%next
      enddo

      call pc_restore
      if (associated(array)) deallocate(array)

      return
      end subroutine buildjob_write_cmd


!!--------------------------- load_job_data --------------------------------!!
!!--------------------------- load_job_data --------------------------------!!
!!--------------------------- load_job_data --------------------------------!!


      subroutine buildjob_load_job_data(junit,start,wunit)

      integer              ,intent(in)                 :: junit       ! argument
      type(PROCESS),pointer                            :: start       ! argument
      integer :: wunit

!!      type(JOB_DATA_STRUCT),pointer                    :: job_obj     ! local
      type(PROCESS),pointer                            :: current     ! local
      type(CARD_POINTER)                               :: tag_card    ! local
      logical                                          :: first       ! local
      character(len=80)                                :: ctmp        ! local
      integer :: istat,whichlib

      call pc_next
      call pc_set_ipn (2)
      current => start
      do
        if (.not. associated(current)) exit
        if (current%name(1:current%nname) .eq. 'JOB_DATA') then
          tag_card%card => current%tag_card_start(PARAMETERS_TAG)%card
          first = .TRUE.
          do
            if (.not. associated(tag_card%card)) exit
            if (first) then
              call pc_put_process_card(tag_card%card%data)
              first = .FALSE.
            else
              call pc_add_process_card(tag_card%card%data)
            endif
            tag_card%card => tag_card%card%next
          enddo
          exit
        endif
        current => current%next
      enddo


      rlocation='LOCAL'
      call buildjob_jobdatainfo(rlocation,jobname,machine,speed,&
          num_cpus,num_nodes,std_libs,frontend_user,frontend_node,&
          frontend_path,mail_opt,queue,time_limit,custom_exec_b,&
          custom_exec_a,custom_lam,ncustom_modules,custom_modules,&
          ncustom_compile,custom_compile,ncustom_link,custom_link,&
          ncustom_nodes,custom_nodes,wunit,rerun,priority)
      anycustom=.false.
      useicps=.true.
      if(ncustom_modules+ncustom_compile+ncustom_link.gt.0)anycustom=.true.
      if(num_cpus.gt.1.or.anycustom.or.requires_landmark)useicps=.false.
!!      call pc_get_jdata ('RLOCATION'     ,rlocation)
      whichlib=getsys_library()  ! 1=prod, 2=beta, 3=alpha
!         putsys_env must be called before any call to cnfg
      if(rlocation.eq.'ALASKA')then
!                Get alaska config file
       select case(whichlib)
        case(GETSYS_ALPHALIB)
         call putsys_env('cps_config_file_alpha',&
                      '/home/sps/offsite/alaska/cps_config_alpha.dat',&
                         istat)
        case(GETSYS_BETALIB)
         call putsys_env('cps_config_file_beta',&
                      '/home/sps/offsite/alaska/cps_config_beta.dat',&
                         istat)
        case(GETSYS_PRODLIB)
         call putsys_env('cps_config_file',&
                         '/home/sps/offsite/alaska/cps_config.dat',&
                          istat)
       end select
      endif
      if(rlocation.eq.'PONY')then
!                Get pony config file
        select case(whichlib)
         case(GETSYS_ALPHALIB)
          call putsys_env('cps_config_file_alpha',&
                       '/home/sps/offsite/pony/cps_config_alpha.dat',&
                          istat)
         case(GETSYS_BETALIB)
          call putsys_env('cps_config_file_beta',&
                          '/home/sps/offsite/pony/cps_config_beta.dat',&
                           istat)
         case(GETSYS_PRODLIB)
          call putsys_env('cps_config_file',&
                          '/home/sps/offsite/pony/cps_config.dat',&
                           istat)
        end select
      endif
      call cnfg_get_value ('location',location)
      call cnfg_get_value ('cps_pbs_type',pbs_type)
      call cnfg_get_value ('cps_main_host',main_host)
      call cnfg_get_value ('cps_rcp_node',rcp_node)
      call cnfg_get_value ('cps_platform_default_linux',platform_default_linux)
      call cnfg_get_value ('cps_platform_default_sol',platform_default_sol)
      call cnfg_get_value ('sps_home_dir'   ,sps_home   )
      call cnfg_get_value ('sps_install_dir',sps_install)
      call cnfg_get_value ('cps_log_dir'    ,cps_log    )
      call cnfg_get_value ('cps_batchtmp_dir'    ,batchtmp_dir    )
      call cnfg_get_value ('cps_batchtmp_nodes_file'    ,batchtmp_nodes    )
      if(batchtmp_dir.eq.' ')then
        print*,' WARNING--Unable to get batchtmp directory from &
               &config file--Using default'
        batchtmp_dir='/btmp/'
      endif
      if(platform_default_linux.eq.' ')platform_default_linux='linuxab80'
      if(platform_default_sol.eq.' ')platform_default_sol='sol62'


      call buildjob_remove_blank_rows (custom_modules ,ncustom_modules)
      call buildjob_remove_blank_rows (custom_compile ,ncustom_compile)
      call buildjob_remove_blank_rows (custom_link    ,ncustom_link   )
      call buildjob_remove_blank_rows (custom_nodes   ,ncustom_nodes  )

      if (trim(std_libs).eq.'CUSTOM' .and. &
          ncustom_compile.eq.0 .and. ncustom_link.eq.0) then
        if (getsys_library() .eq. GETSYS_PRODLIB) then
          std_libs = 'PRODLIB'
        else if (getsys_library() .eq. GETSYS_BETALIB) then
          std_libs = 'BETALIB'
        else
          std_libs = 'ALPHALIB'
        endif
        write(STDOUT,*) 'WARNING WARNING -- No custom compile or link specified'
        write(STDOUT,*) '                   Changing to '//trim(std_libs)
      else if (trim(std_libs).eq.'TESTLIB') then
        std_libs = 'BETALIB'
      endif

      if (num_nodes .eq. 0) then
        num_nodes = num_cpus / 2
        if (num_nodes*2 .lt. num_cpus) num_nodes = num_nodes + 1
      endif

      call string_to_upper (custom_lam)
      call string_to_upper (machine,ctmp)
      if (trim(ctmp)       .eq. 'CUSTOM' .and.  &
          trim(custom_lam) .eq. 'NO'     .and.  &
          ncustom_nodes    .eq. 0        ) then
        machine = 'Linux'
        write(STDOUT,*) 'WARNING WARNING -- No custom nodes or lam specified'
        write(STDOUT,*) '                   Changing machine to Linux'
      endif

!!!      call job_data_delete (job_obj)

      return
      end subroutine buildjob_load_job_data


!!------------------------- load_project_data ------------------------------!!
!!------------------------- load_project_data ------------------------------!!
!!------------------------- load_project_data ------------------------------!!


      subroutine buildjob_load_project_data(junit,start)

      integer              ,intent(in)                 :: junit       ! argument
      type(PROCESS),pointer                            :: start       ! argument

      type(PROJECT_DATA_STRUCT),pointer                :: project_obj ! local
      type(PROCESS),pointer                            :: current     ! local
      type(CARD_POINTER)                               :: tag_card    ! local
      logical                                          :: first       ! local

      call pc_next
      call pc_set_ipn (1)
      current => start
      do
        if (.not. associated(current)) exit
        if (current%name(1:current%nname) .eq. 'PROJECT_DATA') then
          tag_card%card => current%tag_card_start(PARAMETERS_TAG)%card
          first = .TRUE.
          do
            if (.not. associated(tag_card%card)) exit
            if (first) then
              call pc_put_process_card(tag_card%card%data)
              first = .FALSE.
            else
              call pc_add_process_card(tag_card%card%data)
            endif
            tag_card%card => tag_card%card%next
          enddo
          exit
        endif
        current => current%next
      enddo

      call project_data_create (project_obj)
      call pc_get_pdata ('ACCOUNT' ,account)
      call project_data_delete (project_obj)


      return
      end subroutine buildjob_load_project_data


!!------------------------- remove_blank_rows ------------------------------!!
!!------------------------- remove_blank_rows ------------------------------!!
!!------------------------- remove_blank_rows ------------------------------!!


      subroutine buildjob_remove_blank_rows (array,narray)

      character(len=*),pointer                 :: array(:)            ! argument
      integer                 ,intent(inout)   :: narray              ! argument

      integer                                  :: i                   ! local
      character(len=PC_LENGTH),allocatable     :: temp_array(:)       ! local

      if (.not. associated(array)) return
      if (narray .eq. 0) return

      do i = 1, narray
        if (len_trim(array(i)) .ne. 0) cycle
        allocate(temp_array(narray))
        temp_array(1:narray) = array(1:narray)
        deallocate(array)
        allocate(array(narray-1))
        if (i .gt. 1) then
          array(1:i-1) = temp_array(1:i-1)
          array(i:narray-1) = temp_array(i+1:narray)
        else
          array(1:narray-1) = temp_array(i+1:narray)
        endif
        narray = narray - 1
        deallocate(temp_array)
      enddo

      return
      end subroutine buildjob_remove_blank_rows


!!--------------------------- append_element -------------------------------!!
!!--------------------------- append_element -------------------------------!!
!!--------------------------- append_element -------------------------------!!


      subroutine buildjob_append_element (array,narray,value)
      implicit none
      character(len=*),pointer                 :: array(:)             !argument
      integer         ,intent(inout)           :: narray               !argument
      character(len=*),intent(in)              :: value                !argument

      character(len=PC_LENGTH),allocatable     :: temp_array(:)        !local

      if (narray .le. 0) then
        if (associated(array)) deallocate(array)
        narray = 1
        allocate(array(narray))
        array(narray) = value
      else
        allocate(temp_array(narray))
        temp_array(1:narray) = array(1:narray)
        deallocate(array)
        allocate(array(narray+1))
        array(1:narray) = temp_array(1:narray)
        array(narray+1:narray+1) = value
        narray = narray + 1
        deallocate(temp_array)
      endif

      return
      end subroutine buildjob_append_element

      subroutine buildjob_jobdatainfo(rlocation,jobname,machine,speed,&
          num_cpus,num_nodes,std_libs,frontend_user,frontend_node,&
          frontend_path,mail_opt,queue,time_limit,custom_exec_b,&
          custom_exec_a,custom_lam,ncustom_modules,custom_modules,&
          ncustom_compile,custom_compile,ncustom_link,custom_link,&
          ncustom_nodes,custom_nodes,wrklun,rerun,priority)

      character(len=*),pointer :: custom_modules(:),custom_compile(:),&
          custom_link(:),&
          custom_nodes(:)
      character(len=*) :: rlocation,jobname,machine,speed,std_libs,&
          frontend_user,frontend_path,mail_opt,queue,custom_exec_b,&
          custom_exec_a,custom_lam,frontend_node,rerun
      integer :: num_cpus,num_nodes,time_limit,ncustom_modules,ncustom_compile
      integer :: ncustom_link,ncustom_nodes,wrklun,priority
      type(cardset_struct),pointer :: cobj

      character(len=80) :: card,msg
      logical :: found=.false.
      integer :: istat,k

      rewind wrklun
      nullify(cobj)

      call cardset_create(cobj)
      DO
        read(wrklun,'(A)',iostat=istat)card
        if(istat.lt.0)return
        if(found)then
          k=index(card,'</PROCESS>')
          if(k.ne.0)exit
          call cardset_add_card(cobj,card)
        else
          k=index(card,'<PROCESS name="JOB_DATA">')
          if(k.ne.0)found=.true.
        endif
      ENDDO

      call getsys_hostname(frontend_node)
      call getsys_username(frontend_user)
      if (frontend_path .eq. 'NONE') then
        call getsys_current_dir(frontend_path)
      endif

      call cardset_get_scalar(cobj,'RLOCATION',rlocation,msg)
      call cardset_get_scalar (cobj,'JOBNAME'       ,jobname,msg)
      call cardset_get_scalar (cobj,'MACHINE'       ,machine,msg)
      if (machine(1:5) .eq. 'hotce') machine = 'Solaris'
      speed='TBD'
      call cardset_get_scalar (cobj,'RERUN'         ,rerun,msg)
      call cardset_get_scalar (cobj,'NUM_CPUS'      ,num_cpus,msg)
      call cardset_get_scalar (cobj,'NUM_NODES'     ,num_nodes,msg)
      priority=0
      call cardset_get_scalar (cobj,'PRIORITY'      ,priority,msg)
      call cardset_get_scalar (cobj,'STD_LIBS'      ,std_libs,msg)
      call string_to_upper (std_libs)
      call cardset_get_scalar (cobj,'MAIL_OPT'      ,mail_opt,msg)
      call cardset_get_scalar (cobj,'QUEUE'         ,queue,msg)
!!!      if(queue.eq.'B')queue='B1800'
      call cardset_get_scalar (cobj,'TIME_LIMIT'    ,time_limit,msg)
      call cardset_get_scalar (cobj,'CUSTOM_EXEC_B' ,custom_exec_b,msg)
      call cardset_get_scalar (cobj,'CUSTOM_EXEC_A' ,custom_exec_a,msg)
      call cardset_get_scalar (cobj,'CUSTOM_LAM'    ,custom_lam,msg)

      call cardset_alloc_array (cobj,'CUSTOM_MODULES',custom_modules,&
                                 ncustom_modules,msg)
      call cardset_alloc_array (cobj,'CUSTOM_COMPILE',custom_compile,&
                                 ncustom_compile,msg)
      call cardset_alloc_array (cobj,'CUSTOM_LINK',custom_link,ncustom_link,msg)
      call cardset_alloc_array (cobj,'CUSTOM_NODES',custom_nodes,ncustom_nodes,&
                                msg)

    end subroutine buildjob_jobdatainfo


    end module cpsbld_module
