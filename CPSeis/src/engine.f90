!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- engine.f90 --------------------------------!!
!!------------------------------- engine.f90 --------------------------------!!
!!------------------------------- engine.f90 --------------------------------!!


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
! Name       : ENGINE
! Category   : main_prog
! Written    : 2003-11-03   by: Tom Stoeckley
! Revised    : 2007-12-06   by: Bill Menger
! Maturity   : beta
! Purpose    : Engine which runs a CPS job.
! Portability: No known limitations, but see note below.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This engine runs a CPS job without having to compile a special main program
! tailored to the specific job.  This engine runs a CPS job using information
! from a CPS workfile or a process_parameters file.
!
! Only one instance of this module can exist in a program.  This is because
! only one instance of the CPS primitive can exist, and this module uses the
! CPS primitive.  As a consequence, a program can run only one CPS job at a
! time.  This module can be re-used for another job after the previous job
! finishes.
!
! Note: A workfile (.wrk extension) is created by CFE and contains global
! parameters, control parameters, and process parameters for each process.
! The job builder creates a process_parameters file which contains only the
! process parameters.  Either file can be read by this primitive.  If a
! workfile is read, only the process parameters are read from the file; the
! global and control parameters are ignored (except for the loop-controlling
! control parameters).
!
! Note: If a workfile is used, setups for each loop in the job will wait
! until the trace processing in the previous loop is finished.  But if a
! process_parameters file is used, all setups will be completed before any
! trace processing begins.
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
!                                       i       o
!          call engine_run          (workfile,whoops)
!
!                                       i       o      o
!          call engine_startup      (workfile,nloops,whoops)
!
!                                      i     o
!          call engine_setup_loop   (iloop,whoops)
!          call engine_execute_loop (iloop,whoops)
!          call engine_finish_loop  (iloop)
!
!                                      i
!          call engine_shutdown     (whoops)
!
! character(len=*)   workfile = CPS workfile or process_parameters file to use.
! logical              whoops = true if an error occurred.
! integer              nloops = number of processing loops to run.
! integer               iloop = which processing loop to run (1 thru nloops).
!
! ENGINE_RUN encompasses calls to all of the other routines.
!
! If routine returns an error, no further calls should be made to any routine
! in this primitive, except to begin a new job by calling ENGINE_RUN or
! ENGINE_STARTUP again.
!
! Later, ENGINE_EXECUTE_LOOP may be enhanced to process only part of the
! traces at a time, allowing more interactive control of the job.
!
!-------------------------------------------------------------------------------
!                          CALLING FROM C
!
!           #include "c2f_interface.h"
!
!           #if NEED_UNDERSCORE
!           #define engine_run_from_c    engine_run_from_c_
!           #elif NEED_CAPITALS
!           #define engine_run_from_c    ENGINE_RUN_FROM_C
!           #endif
!
!           INTEGER engine_run_from_c (const char *workfile);
!           INTEGER istat;
!
!           istat = engine_run_from_c (workfile);
!
!           if(istat != 0) exit(1);
!           exit(0);
!
! where  workfile = CPS workfile or process_parameters file to use.
! where     istat = non-zero if an error occurred.
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
!     Date        Author     Description
!     ----        ------     -----------
!  6. 2007-12-06  B. Menger  Removed the call to string_hh2cc
!005. 2006-12-04  D. Glover  Added NULLIFY statements for Intel compiler.
!004. 2006-01-10  B. Menger  Removed Unused Variables.
!  3. 2004-12-07  Stoeckley  Modify to work correctly when the job consists
!                             only of setup processes.
!  2. 2004-08-23  Stoeckley  Fixed a rarely-encountered bug whereby the
!                             last process in the loop received the wrong
!                             (uninitialized) trace and header arrays if
!                             it needed two trace and header arrays, causing
!                             an abort.
!  1. 2003-11-18  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
! The following commands (Solaris) generate internal compiler errors:
!
!     sol70_f90 -c -g     -ansi
!     sol70_f90 -c -g -O1 -ansi
!
! Leaving off -g makes no difference.
! Using -O or -O2 or -O3 or -O4 or -O5 all work fine (-O implies -O3).
!
! Workarounds such as the following have been put into the code:
!
!     call cardset_add_card (cardsets(npn)%cardset,card)    ! does not work.
!
!     cardset => cardsets(npn)%cardset                      ! works.
!     call cardset_add_card (cardset,card)                  ! works.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
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


      module engine_module
      use named_constants_module
      use cps_module
      use superproc_module
      use pc_module
      use string_module
      implicit none
      private
      public :: engine_run
      public :: engine_startup
      public :: engine_setup_loop
      public :: engine_execute_loop
      public :: engine_finish_loop
      public :: engine_shutdown

      character(len=100),public,save :: ENGINE_IDENT = &
"$Id: engine.f90,v 1.6 2007/12/07 15:25:25 Menger beta sps $"


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,private :: tracegroup_struct

        private
        double precision    ,pointer   :: hd(:,:)
        real                ,pointer   :: tr(:,:)

      end type tracegroup_struct


      type,private :: cpsproc_struct

        private
        character(len=40)                  :: name
        integer                            :: ipn
        logical                            :: need_request
        logical                            :: need_label
        logical                            :: twosets
        logical                            :: setup_only
        type(superproc_struct),pointer     :: super
        integer                            :: nwih  ! output by prev process.
        integer                            :: ndpt  ! output by prev process.
        integer                            :: numtr ! output by prev process.
        integer                            :: itg   ! index of tracegroup.

      end type cpsproc_struct


      type,private :: engine_struct

        private
        character(len=FILENAME_LENGTH)  :: workfile
        integer                         :: nprocesses
        type(cpsproc_struct)   ,pointer :: processes(:)
        type(tracegroup_struct),pointer :: tracegroups(:)
        integer                         :: ipn_first(22)
        integer                         :: ipn_last (22)

      end type engine_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      type(engine_struct),save :: obj

      contains


!!----------------------------- startup ------------------------------------!!
!!----------------------------- startup ------------------------------------!!
!!----------------------------- startup ------------------------------------!!


      subroutine engine_startup (workfile,nloops,whoops)
      character(len=*)       ,intent(in)  :: workfile              ! arguments
      integer                ,intent(out) :: nloops                ! arguments
      logical                ,intent(out) :: whoops                ! arguments
      type(cpsproc_struct)   ,pointer     :: cpsproc               ! local
      type(tracegroup_struct),pointer     :: tracegroup            ! local
      character(len=40)      ,pointer     :: process_names(:)      ! local

      character(len=200)                  :: ident,card,errmsg     ! local

      logical                    :: reading_control_parameters     ! local
      logical         :: need_label,need_request,setup_only        ! local
      logical                      :: control_parameters_found     ! local
      integer                             :: plun,istat,npn,ipn    ! local
      integer                             :: iloop,ncards          ! local

      type :: cardset_structwrap
        type(cardset_struct),pointer :: cardset
        logical                      :: trace_supplying
      end type cardset_structwrap

      type(cardset_structwrap)            :: cardsets(222)         ! local
      type(cardset_struct),pointer        :: cardset               ! local

!!!!!!!!!!!!!!! initialize the object:

      obj%workfile = workfile
      obj%nprocesses = 0
      nullify (obj%processes)
      nullify (obj%tracegroups)

!!!!!!!!!!!!!!! initial read of workfile:

      call getlun (plun)
      open(unit=plun,file=workfile,status='OLD',iostat=istat)

      if (istat /= 0) then
           print *, 'Engine error trying to open file '//trim(workfile)
           nloops = 0
           whoops = .true.
           call engine_private_quit (whoops)
           return
      end if

      control_parameters_found = .false.
      ncards = 0
      reading: do
        reading_control_parameters = .false.
        npn = 0
        do
          read(plun,'(A80)',iostat = istat) card

          if (istat /= 0) then
               if (ncards > 0) exit reading
               print *, 'Engine error trying to read file '//trim(workfile)
               nloops = 0
               whoops = .true.
               call engine_private_quit (whoops)
               return
          end if

          ncards = ncards + 1
          card = adjustl(card)
          call string_to_upper(card)

          if (card == ' ') then
            cycle
          else if (card(1:8) == '<PROCESS') then
            npn = npn + 1
            nullify (cardsets(npn)%cardset) ! jpa
            call cardset_create (cardsets(npn)%cardset)
          else if (card == '</CONTROLS>') then
            reading_control_parameters = .false.
          else if (card == '<CONTROLS>') then
            control_parameters_found   = .true.
            reading_control_parameters = .true.
          else if (reading_control_parameters) then
            cardset => cardsets(npn)%cardset
 !!!!!      call cardset_add_card (cardsets(npn)%cardset,card)
            call cardset_add_card (cardset,card)
          endif
        enddo
      enddo reading

      close(unit=plun,iostat=istat)

      if (istat /= 0) then
           print *, 'Engine error trying to close file '//trim(workfile)
           nloops = 0
           whoops = .true.
           call engine_private_quit (whoops)
           return
      end if

      if (npn == 0) then
           print *, 'Engine error - no processes on file '//trim(workfile)
           nloops = 0
           whoops = .true.
           call engine_private_quit (whoops)
           return
      end if

!!!!!!!!!!!!!!! find loops:

      nloops = 0
      do ipn = 1,npn
           need_label   = .false.
           need_request = .false.
           setup_only   = .false.
           cardset => cardsets(ipn)%cardset
           call cardset_get_scalar (cardset,'need_label'  ,need_label  ,errmsg)
           call cardset_get_scalar (cardset,'need_request',need_request,errmsg)
           call cardset_get_scalar (cardset,'setup_only'  ,setup_only  ,errmsg)
           cardsets(ipn)%trace_supplying = &
                  (need_label .and. .not.need_request .and. .not.setup_only)
           call cardset_delete (cardsets(ipn)%cardset)
           if (cardsets(ipn)%trace_supplying) then
                if (nloops == 0) then
                     nloops = 1
                     obj%ipn_first(nloops) = 1
                else
                     obj%ipn_last (nloops) = ipn - 1
                     nloops = nloops + 1
                     obj%ipn_first(nloops) = ipn
                end if
           end if
      end do
      if (nloops == 0) then
           nloops = 1
           obj%ipn_first(nloops) = 1
      end if
      obj%ipn_last(nloops) = npn

      print *, ' '
      print *, '==============================================================='
      print *, 'Looping information for file ',trim(workfile)
      print *, '==============================================================='
      print *, ' '
           do iloop = 1,nloops
                print *, 'loop ',iloop,                             &
                         '  process numbers ',obj%ipn_first(iloop), &
                                     ' thru ',obj%ipn_last(iloop)
           end do
      print *, ' '
      if (.not.control_parameters_found) then
      print *, 'No control parameters were found on the workfile.'
      print *, 'Therefore all setups are performed at the beginning of the job,'
      print *, 'whether or not there are more than one loop in the job.'
      print *, ' '
      end if

!!!!!!!!!!!!!!! start processing:

      nullify (process_names)

      call cps_start_processing &
                  (obj%nprocesses, process_names, workfile, whoops)

      if (whoops) then
           if (associated(process_names)) deallocate (process_names)
           call cps_print ('Error in cps_start_processing')
           call engine_shutdown (whoops)
           return
      end if

      if (obj%nprocesses <= 2) then
           if (associated(process_names)) deallocate (process_names)
           whoops = .true.
           call cps_print ('Error in cps_start_processing')
           call cps_print ('No processes in job')
           call engine_shutdown (whoops)
           return
      end if

      obj%ipn_last(nloops) = obj%nprocesses    ! in case npn was 0 above.

!!!!!!!!!!!!!!! print rcs ident strings:

      do ipn = 1,obj%nprocesses
           call superproc_get_rcs_ident     (process_names(ipn),ident)
           call cps_print_rcs_ident         (ident)
      end do

      call cps_custom_rcs_ident

!!!!!!!!!!!!!!! create and initialize the process structures:

      allocate (obj%processes(obj%nprocesses))

      !! NOTE: If the super_struct pointer is present in cpsproc_struct
      !! (and there is no other change to this source code file),
      !! the above allocate statement aborts with the following message
      !! on the linuxab75_debug and linuxab80_debug and linuxab80 (and
      !! presumably also linuxab75) platforms:
      !!
      !!   cps_sig_handler(host hoeplc01):
      !!       SIGSEGV - Invalid memory reference has occured
      !!   At shutdown -- cps_pre_process ipn =   0 ,
      !!       cps_post_process ipn =   0  trace =   0.000000000000000
      !!   At shutdown -- Resident Set Size =   1616  kb,
      !!       Virtual Memory Size =   27224  kb
      !!   Dbug system -- dbug system message was never set
      !!   Dbug -- dbug message was never set
      !!
      !! The solaris platforms work fine.
      !!
      !! Therefore we are using super_wrapper_module instead of super_module.
      !! Code calling both modules is included in this source file, with the
      !! super_module code commented out with the !!! characters.
      !!
      !! Using super_wrapper_module is much faster to compile, but probably
      !! slightly slower during trace processing.

      do ipn = 1,obj%nprocesses
           cpsproc => obj%processes(ipn)
           cpsproc%name          = process_names(ipn)
           cpsproc%ipn           = ipn
           cpsproc%need_request  = .false.
           cpsproc%need_label    = .false.
           cpsproc%twosets       = .false.
           cpsproc%setup_only    = .false.
           cpsproc%nwih          = 1
           cpsproc%ndpt          = 1
           cpsproc%numtr         = 1
           cpsproc%itg           = 1
           nullify (cpsproc%super)
      end do

      deallocate (process_names)

!!!!!!!!!!!!!!! create and initialize the tracegroup structures:

      allocate (obj%tracegroups(obj%nprocesses))

      do ipn = 1,obj%nprocesses
           tracegroup => obj%tracegroups(ipn)
           nullify (tracegroup%hd)
           nullify (tracegroup%tr)
      end do

      whoops = .false.

      end subroutine engine_startup


!!-------------------------- setup loop -----------------------------------!!
!!-------------------------- setup loop -----------------------------------!!
!!-------------------------- setup loop -----------------------------------!!


      subroutine engine_setup_loop (iloop,whoops)
      integer               ,intent(in)  :: iloop                 ! arguments
      logical               ,intent(out) :: whoops                ! arguments
      integer                            :: ipn,ipn1,ipn2         ! local
      type(cpsproc_struct)  ,pointer     :: cpsproc               ! local
      integer                            :: nextitg               ! local
      integer                            :: nwih,ndpt,numtr       ! local
      logical                            :: non_setup_reached     ! local

      ipn1 = obj%ipn_first(iloop)
      ipn2 = obj%ipn_last (iloop)

!!!!!!!!!!!!!!! start the setups:

      call cps_start_setups (whoops)

      if (whoops) then
           call cps_print ('Error in cps_start_setups')
           call engine_shutdown (whoops)
           return
      end if

!!!!!!!!!!!!!!! do the setups:

      non_setup_reached = .false.
      do ipn = ipn1,ipn2
           cpsproc => obj%processes(ipn)
           call cps_pre_setup
           call superproc_create     (cpsproc%super,cpsproc%name)

           call pc_get_control ('need_request', cpsproc%need_request)
           call pc_get_control ('need_label'  , cpsproc%need_label)
           call pc_get_control ('twosets'     , cpsproc%twosets)
           call pc_get_control ('setup_only'  , cpsproc%setup_only)

           if (ipn >= 2) call pc_get_global ('nwih'   , cpsproc%nwih)
           if (ipn >= 2) call pc_get_global ('ndpt'   , cpsproc%ndpt)
           if (ipn >= 3) call pc_get_global ('numtr'  , cpsproc%numtr)

           if (ipn == 1 .and. .not.cpsproc%setup_only) then
                whoops = .true.
                call cps_print ('process 1 must be setup_only')
                call engine_finish_loop (iloop)
                call engine_shutdown (whoops)
                return
           end if

           if (ipn == 2 .and. .not.cpsproc%setup_only) then
                whoops = .true.
                call cps_print ('process 2 must be setup_only')
                call engine_finish_loop (iloop)
                call engine_shutdown (whoops)
                return
           end if

           if (ipn >= 3 .and. .not.cpsproc%setup_only) then
           if (.not.non_setup_reached) then
                if (.not.cpsproc%need_label .or. cpsproc%need_request) then
                     whoops = .true.
                     call cps_print ('process '//trim(string_ii2ss(ipn))// &
                                     ' must be a trace supplying process')
                     call engine_finish_loop (iloop)
                     call engine_shutdown (whoops)
                     return
                end if
                non_setup_reached = .true.
           end if
           end if

           call cps_post_setup
      end do

!!!!!!!!!!!!!!! finish the setups:

      call cps_finish_setups (whoops)

      if (whoops) then
           call cps_print ('Error in cps_finish_setups')
           call engine_finish_loop (iloop)
           call engine_shutdown (whoops)
           return
      end if

!!!!!!!!!!!!!!! allocate trace and header arrays:

      call cps_print ('===================================================')
      call cps_print ('Allocating trace and header arrays.')
      call cps_print ('===================================================')
      call cps_print (' ')

      nextitg = 1
      nwih    = 1
      ndpt    = 1
      numtr   = 1

      do ipn = ipn1,ipn2
           cpsproc => obj%processes(ipn)
           cpsproc%itg = nextitg
           if (cpsproc%setup_only) then
                call cps_print ('process '//trim(string_ii2ss(ipn))//' ' &
                                //trim(cpsproc%name) &
                                //' (setup only) not using tracegroups')
                nwih  = max(cpsproc%nwih,nwih)
                ndpt  = max(cpsproc%ndpt,ndpt)
                numtr = max(cpsproc%numtr,numtr)
           else if (.not.cpsproc%twosets) then
                call cps_print ('process '//trim(string_ii2ss(ipn))//' ' &
                                //trim(cpsproc%name) &
                                //' using tracegroup ' &
                                //trim(string_ii2ss(nextitg)))
                nwih  = max(cpsproc%nwih,nwih)
                ndpt  = max(cpsproc%ndpt,ndpt)
                numtr = max(cpsproc%numtr,numtr)
           else
                call cps_print ('process '//trim(string_ii2ss(ipn))//' ' &
                                //trim(cpsproc%name) &
                                //' using tracegroups ' &
                                //trim(string_ii2ss(nextitg))//' and ' &
                                //trim(string_ii2ss(nextitg+1)))
                call engine_private_alloc_tracegroup &
                               (nextitg,nwih,ndpt,numtr,whoops)

                if (whoops) then
                     call engine_finish_loop (iloop)
                     call engine_shutdown (whoops)
                     return
                end if

                nextitg = nextitg + 1
                nwih  = cpsproc%nwih
                ndpt  = cpsproc%ndpt
                numtr = cpsproc%numtr
           end if
      end do

 !!!  cpsproc%itg = nextitg    !!!!! caused a bug (removed 2004-04-12).
      call engine_private_alloc_tracegroup (nextitg,nwih,ndpt,numtr,whoops)

      if (whoops) then
           call engine_finish_loop (iloop)
           call engine_shutdown (whoops)
           return
      end if

      call cps_print (' ')
      call cps_print ('===================================================')
      call cps_print ('Finished allocating trace and header arrays.')
      call cps_print ('===================================================')
      call cps_print (' ')
      call cps_print (' ')

      end subroutine engine_setup_loop


!!-------------------- private alloc tracegroup ----------------------------!!
!!-------------------- private alloc tracegroup ----------------------------!!
!!-------------------- private alloc tracegroup ----------------------------!!


      subroutine engine_private_alloc_tracegroup (itg,nwih,ndpt,numtr,whoops)
      integer             ,intent(in)    :: itg,nwih,ndpt,numtr  ! arguments
      logical             ,intent(out)   :: whoops               ! arguments
      character(len=222)                 :: msg                  ! local
      type(tracegroup_struct),pointer    :: tracegroup           ! local
      integer                            :: ierr                 ! local


      write (msg,*) 'allocating tracegroup ',itg,  &
                    ':  nwih=',nwih,'  ndpt=',ndpt,'  numtr=',numtr
      call cps_print (msg)
      tracegroup => obj%tracegroups(itg)

      allocate(tracegroup%hd(nwih,numtr),stat=ierr)
      if (ierr /= 0) then
           call cps_print ('Unable to allocate hd')
           whoops = .true.
           return
      endif

      allocate(tracegroup%tr(ndpt,numtr),stat=ierr)
      if (ierr /= 0) then
           call cps_print ('Unable to allocate tr')
           whoops = .true.
           return
      endif

      tracegroup%hd(:,:) = -itg     ! initialization for debug purposes.
      tracegroup%tr(:,:) =  itg     ! initialization for debug purposes.

      whoops = .false.

      end subroutine engine_private_alloc_tracegroup


!!---------------------------- execute loop --------------------------------!!
!!---------------------------- execute loop --------------------------------!!
!!---------------------------- execute loop --------------------------------!!


      subroutine engine_execute_loop (iloop,whoops)
      integer             ,intent(in)    :: iloop               ! arguments
      logical             ,intent(out)   :: whoops              ! arguments
      type(cpsproc_struct),pointer       :: cpsproc             ! local
      integer                            ::      next,ntr ! local
      double precision    ,pointer       :: hd1(:,:)            ! local
      real                ,pointer       :: tr1(:,:)            ! local
      double precision    ,pointer       :: hd2(:,:)            ! local
      real                ,pointer       :: tr2(:,:)            ! local
      logical                            :: forward             ! local
      integer                            :: ipn,ipn1,ipn2,itgp1 ! local

      ipn1 = obj%ipn_first(iloop)
      ipn2 = obj%ipn_last (iloop)

!!!!!!!!!!!!!!! start the trace processing loop:

      forward = .true.
      ntr = NEED_TRACES
      ipn = ipn1

      do

!!!!!!!!!!!!!!! call a single process:

           cpsproc => obj%processes(ipn)

           do
             if (cpsproc%setup_only) then
                  if (forward) then
                       next = ipn + 1
                  else
                       next = ipn - 1
                  end if
                  exit
             end if

             if (cpsproc%need_label) then
                  call cps_top_of_loop (ipn)
             else if (ntr == NEED_TRACES) then
                  next = ipn - 1
                  forward = .false.
                  exit
             end if
             if (cpsproc%twosets) then
                  hd1 => obj%tracegroups(cpsproc%itg)%hd
                  tr1 => obj%tracegroups(cpsproc%itg)%tr
                  itgp1=cpsproc%itg + 1
                  hd2 => obj%tracegroups(itgp1)%hd
                  tr2 => obj%tracegroups(itgp1)%tr
                  call cps_pre_process      (ipn)
                  call superproc_twosets    (cpsproc%super,ntr,hd1,tr1,hd2,tr2)
                  call cps_post_process     (ipn,ntr,hd2,tr2)
              else
                  hd1 => obj%tracegroups(cpsproc%itg)%hd
                  tr1 => obj%tracegroups(cpsproc%itg)%tr
                  call cps_pre_process      (ipn)
                  call superproc_oneset     (cpsproc%super,ntr,hd1,tr1)
                  call cps_post_process     (ipn,ntr,hd1,tr1)
             end if

             if (ntr == NEED_TRACES) then
                  forward = .false.
                  next = ipn - 1
                  exit
             end if

             forward = .true.
             next = ipn + 1
             exit
           end do

!!!!!!!!!!!!!!! finish the trace processing loop:

           if (ntr == FATAL_ERROR) then
                exit
           else if (next > ipn2) then
                if (ntr == NO_MORE_TRACES) exit
                forward = .false.
                ntr = NEED_TRACES
                ipn = ipn2
           else if (next < ipn1) then                  ! added 2004-12-07
                if (ntr == NO_MORE_TRACES) exit        ! added 2004-12-07
                forward = .true.                       ! added 2004-12-07
                ntr = NO_MORE_TRACES                   ! added 2004-12-07
                ipn = ipn1                             ! added 2004-12-07
           else if (ntr == NEED_TRACES) then
                ipn = next
           else
                ipn = next
                cpsproc => obj%processes(ipn)
                if (cpsproc%need_label .and. .not.cpsproc%need_request) then
                  if (ntr > NO_MORE_TRACES) then
                       forward = .false.
                       ntr = NEED_TRACES
                       ipn = ipn - 1
                  else
                       forward = .true.
                       ntr = NEED_TRACES
                  end if
                end if
           end if

      end do

      if (ntr == FATAL_ERROR) then
           whoops = .true.
           call engine_finish_loop (iloop)
           call engine_shutdown (whoops)
      else
           whoops = .false.
      end if

      end subroutine engine_execute_loop


!!-------------------------- finish loop --------------------------------!!
!!-------------------------- finish loop --------------------------------!!
!!-------------------------- finish loop --------------------------------!!


      subroutine engine_finish_loop (iloop)
      integer                ,intent(in)  :: iloop             ! arguments
      type(cpsproc_struct)   ,pointer     :: cpsproc           ! local

      integer                             :: ipn,ipn1,ipn2     ! local

      ipn1 = obj%ipn_first(iloop)
      ipn2 = obj%ipn_last (iloop)

!!!!!!!!!!!!!!! wrapup all processes in loop:

           do ipn = ipn1,ipn2
                cpsproc => obj%processes(ipn)
                call superproc_wrapup     (cpsproc%super)
           end do

!!!!!!!!!!!!!!! delete all processes in loop:

           do ipn = ipn1,ipn2
                cpsproc => obj%processes(ipn)
                call superproc_delete     (cpsproc%super)
           end do

      end subroutine engine_finish_loop


!!----------------------------- shutdown -----------------------------------!!
!!----------------------------- shutdown -----------------------------------!!
!!----------------------------- shutdown -----------------------------------!!


      subroutine engine_shutdown (whoops)
      logical                ,intent(in)  :: whoops            ! arguments

      type(tracegroup_struct),pointer     :: tracegroup        ! local
      integer                             :: ipn               ! local

!!!!!!!!!!!!!!! finish up the processing:

      call cps_finish_processing (whoops)

!!!!!!!!!!!!!!! delete the trace and header arrays:

      if (associated(obj%tracegroups)) then
           do ipn = 1,obj%nprocesses
                tracegroup => obj%tracegroups(ipn)
                if (associated(tracegroup%hd)) deallocate (tracegroup%hd)
                if (associated(tracegroup%tr)) deallocate (tracegroup%tr)
           end do
           deallocate (obj%tracegroups)
      end if

      call engine_private_quit (whoops)

      end subroutine engine_shutdown


!!-------------------------- private quit -------------------------------!!
!!-------------------------- private quit -------------------------------!!
!!-------------------------- private quit -------------------------------!!


      subroutine engine_private_quit (whoops)
      logical                ,intent(in)  :: whoops            ! arguments

      call cps_print (' ')
      call cps_print ('===================================================')
      if (whoops) then
           call cps_print ('Fatal error while running '//trim(obj%workfile))
      else
           call cps_print ('Finished running '//trim(obj%workfile))
      end if
      call cps_print ('===================================================')
      call cps_print (' ')

      end subroutine engine_private_quit


!!------------------------------ run -------------------------------------!!
!!------------------------------ run -------------------------------------!!
!!------------------------------ run -------------------------------------!!


      subroutine engine_run (workfile,whoops)
      implicit none
      character(len=*)       ,intent(in)  :: workfile              ! arguments
      logical                ,intent(out) :: whoops                ! arguments
      integer                             :: iloop,nloops          ! local

      call engine_startup (workfile,nloops,whoops)
      if (whoops) return

      do iloop = 1,nloops
           call engine_setup_loop (iloop,whoops)
           if (whoops) return

           call engine_execute_loop (iloop,whoops)
           if (whoops) return

           call engine_finish_loop (iloop)
      end do

      call engine_shutdown (whoops)

      end subroutine engine_run


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module engine_module


!!-------------------------- run from c ----------------------------------!!
!!-------------------------- run from c ----------------------------------!!
!!-------------------------- run from c ----------------------------------!!


      function engine_run_from_c (workfile) result (istat)
      use engine_module
      use string_module
      use convert_module
      implicit none
      character(len=1)       ,intent(in)  :: workfile(*)           ! arguments
      integer                             :: istat                 ! result
      character(len=200)                  :: workfile9             ! local
      logical                             :: whoops                ! local
      integer                             :: i                     ! local

!      call string_hh2cc  (workfile,workfile9)
      do i=1, 200
        if (workfile(i) == char(0)) then
            workfile9(i:) = ''
            exit
        end if
        workfile9(i:i) = workfile(i)
      end do
      call engine_run    (workfile9,whoops)
      call convert_ll2ii (whoops,istat)

      end function engine_run_from_c


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

