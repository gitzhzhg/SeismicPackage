!<CPS_v1 type="PROCESS"/>
!!------------------------------- avosts.f90 ---------------------------------!!
!!------------------------------- avosts.f90 ---------------------------------!!
!!------------------------------- avosts.f90 ---------------------------------!!

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
! Name       : AVOSTS
! Category   : velocity_analysis
! Written    : 2003-08-26   by: Bill Lucas
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : AVO Standard Suite (AVOSTS).
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This tool computes a standard suite of traces (described below) for every
! pair of 'A' and 'B' traces passed to it from AVEL. The standard suite is
! the basic input for every other tool in the ARCO AVO Seismic Workbench.
! This tool actually invokes the AVO Overburden Correction (UHCI) tool with
! a restricted set of user inputs.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! None.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! The input to this tool are the 'A' and 'B' traces from AVEL. These traces
! must have trace types of 43 and 44, respectively. The CDP numbers of the
! 'A' and 'B' traces must match. Every 'B' traces must have a corresponding
! 'A' trace and vice versa. The 'A' traces must preceed its corresponding
! 'B' trace within the ensemble.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! The output from this tool is a Standard Suite, consisting of the following
! traces:
! Trace                Trace Type
! Real AVO 'A' trace       43
! Real AVO 'B' trace       44
! Imag AVO 'A' trace       17
! Imag AVO 'B' trace       18
! Std. Deviation 'A'       45
! Std. Deviation 'B'       47
! Real correlation         48
! Imag correlation         41
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       increased.
! GATHERED  whether traces are a legitimate gather  none.
! NWIH      number of words in trace header         none.
! NDPT      number of sample values in trace        none.
! TSTRT     starting time on trace                  none.
! DT        trace sample interval                   none.
! GRID      grid transformation structure           none.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!    1    HDR_SEQUENCE               Used as trace sequence number.
!    3    HDR_CURRENT_GROUP          Used as CDP number.
!    4    HDR_CURRENT_CHANNEL        Used as sequence number.
!    5    HDR_FOLD                   Used as fold number.
!    6    HDR_OFFSET                 Used as offset value.
!    7    HDR_MIDPOINT_XGRID         Used as x-line number.
!    8    HDR_MIDPOINT_YGRID         Used as inline number.
!   49    HDR_USER_49                Set to trace type (see list below)
!                                       1  = live seismic trace.
!                                       51 = stacking velocity trace.
!                                       43 = real{A} trace.
!                                       44 = real{B} trace.
!                                       17 = imag{A} trace.
!                                       18 = imag{B} trace.
!                                       45 = standard dev {A} trace.
!                                       47 = standard dev {B} trace.
!                                       48 = real part of corr coeff trace.
!                                       41 = imag part of corr coeff trace.
!   50    HDR_USER_50                Set to gather type (seel list below).
!                                       1  = offset.
!                                       2  = angle.
!   51    HDR_USER_51                Set to iteration number.
!   52    HDR_USER_52                Set to AVO angle.
!   52    HDR_USER_53                Set to dominant frequency.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!002. 2006-06-20  B. Menger   Removed Unused Variables.
!  1. 2005-01-03  B. Lucas   Initial version.
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
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! None.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! This module acts primarily as a wrapper. The computational work is done
! in the B_UHCI and PPAVO primitives.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!
!<NS General Parameters>
!                              AVO Standard Suite
!
!   `-Analysis-------------------------------------------------------------------  
!   | Maximum CDPs per Line =~~~~~~~~~~~`IIIIIIIII
!   | Start of Window =~~~~~~~~~~~~~~~~~`IIIIIIIII
!   | End of Window =~~~~~~~~~~~~~~~~~~~`IIIIIIIII
!   `----------------------------------------------------------------------------  
!
!   `-Correlation----------------------------------------------------------------  
!   | Time Duration of Corr. Window =~~~`IIIIIIIII
!   | Spatial Extent of Corr. Window =~~`IIIIIIIII
!   | Vertical Shift of Corr. Window =~~`IIIIIIIII
!   | Correlation Weighting Option =~~~~`CCCCCCCCCC
!   `----------------------------------------------------------------------------  
!
!   `-Miscellaneous--------------------------------------------------------------  
!   | Pass Pre-Stack Data =~~~~~~~~~~~~~`CC
!   | Level of Debug Print =~~~~~~~~~~~~`CCCCCCCCCC
!   `----------------------------------------------------------------------------  
!   <PARMS Maximum CDPs per Line          [NCDP_MAX]>
!   <PARMS Start of Window                [STIM_WIN]>
!   <PARMS End of Window                  [ETIM_WIN]>
!   <PARMS Time Duration of Corr. Window  [CWIN_TIM]>
!   <PARMS Spatial Extent of Corr. Window [CWIN_CDP]>
!   <PARMS Vertical Shift of Corr. Window [CWIN_VSH]>
!   <PARMS Correlation Weighting Option   [CORR_OPT]>
!   <PARMS Pass Pre-Stack Data            [PASS_OPT]>
!   <PARMS Level of Debug Print           [DBUG_OPT]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="CWIN_CDP">
!<Tip> Spatial extent of correlation window (CDPs).</Tip>
! Default = 21
! Allowed = greater than 1, less than or equal to 99999
! Spatial extent of correlation window (CDPs).
!</Help>
!
!<Help KEYWORD="CORR_OPT">
!<Tip> Correlation weighting option.</Tip>
! Default = NO
! Allowed = NO, LIN, SQR
! Correlation weighting option:
!   NO -> No weighting; strong events dominate over weak.
!   LIN -> Inverse linear; strong events contribute less than NO.
!   SQR -> Inverse square; strong and weak events contribute equally.
!</Help>
!
!<Help KEYWORD="CWIN_TIM">
!<Tip> Time duration of correlation window (msec).</Tip>
! Default = 500
! Allowed = greater than or equal to 10, less than or equal to 5000
! Time duration or correlation window (msec).
!</Help>
!
!<Help KEYWORD="CWIN_VSH">
!<Tip> Vertical shift of correlation window (msec).</Tip>
! Default = 0
! Allowed = greater than or equal to -5000, less than or equal to 5000
! Vertical shift of correlation window.
!</Help>
!
!<Help KEYWORD="DBUG_OPT">
!<Tip> Level of debugging print.</Tip>
! Default = SILENT
! Allowed = SILENT, INFORMATION, MEM_CHECK, EXIT_CODES, EVERYTHING
! Level of debugging print:
!   SILENT -> Minimal diagnostic print.
!   INFORMATION -> Print CDPs processes and their statistics.
!   MEM_CHECK -> Frequenty verify memory integrity.
!   EXIT_CODES -> Exit codes are additionally checked.
!   EVERYTHING -> Full debug print is requested.
!</Help>
!
!<Help KEYWORD="ETIM_WIN">
!<Tip> End time of analysis window (msec).</Tip>
! Default = 0
! Allowed = greater than or equal to 0, less than or equal to 99999
! End time of analysis window (msec).
!</Help>
!
!<Help KEYWORD="STIM_WIN">
!<Tip> Start time of analysis window (msec).</Tip>
! Default = 0
! Allowed = greater than or equal to 0, less than or equal to 99999
! Start time of analysis window (msec).
!</Help>
!
!<Help KEYWORD="NCDP_MAX">
!<Tip> Maximum number of CDPs per line.</Tip>
! Default = 500
! Allowed = greater than 0, less than 20000
! Maximum number of CDPs per line.
!</Help>
!
!<Help KEYWORD="PASS_OPT">
!<Tip> Pass pre-stack data?</Tip>
! Default = YES
! Allowed = NO, YES
! Pass pre-stack data?
!</Help>
!
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!---------------------------- start of module -------------------------------!!
!!---------------------------- start of module -------------------------------!!
!!---------------------------- start of module -------------------------------!!


      module avosts_module
      use pc_module
      use named_constants_module
      use memman_module
      use grid_module            ! if you need the grid transformation.
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.
      use buhci_module
      use ppavo_module

      implicit none
      private
      public :: avosts_create
      public :: avosts_initialize
      public :: avosts_update
      public :: avosts_delete
      public :: avosts            ! main trace processing routine.
      public :: avosts_wrapup

      character(len=100),public,save :: AVOSTS_IDENT = &
'$Id: avosts.f90,v 1.2 2006/06/20 13:11:45 Menger prod sps $'


!!------------------------ parameter structure -------------------------------!!
!!------------------------ parameter structure -------------------------------!!
!!------------------------ parameter structure -------------------------------!!

      type,public :: buhci_ptr
         type(buhci_struct), pointer :: obj
      end type buhci_ptr

      type,public :: avosts_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.

        integer                    :: ipn      ! process number.
        integer                    :: numtr    ! max number of input traces.
        logical                    :: gathered ! whether properly gathered.
        integer                    :: nwih     ! number of header words.
        integer                    :: ndpt     ! number of trace samples.
        real                       :: tstrt    ! time of 1st trace sample (sec).
        real                       :: dt       ! trace sample interval (sec).
        type(grid_struct)          :: grid     ! grid transform.

        integer                        :: cwin_cdp
        integer                        :: cwin_tim
        integer                        :: cwin_vsh
        integer                        :: etim_win
        integer                        :: ncdp_max
        integer                        :: stim_win
        integer                        :: init_iterations
        integer                        :: fcdp
        integer                        :: lagndx
        integer                        :: purged

        logical                        :: end_of_data

        double precision               :: ilineno
        double precision               :: xlineno
        double precision               :: ocdpX

        character(len=3)               :: corr_opt
        character(len=11)              :: dbug_opt
        character(len=3)               :: pass_opt
        character(len=20)              :: survey_units
        character(len=FILENAME_LENGTH) :: pathname_dir

        type(buhci_ptr), pointer, dimension(:)   :: buhci
        real,             pointer, dimension(:,:) :: tr_g
        real,             pointer, dimension(:)   :: tr_t
        double precision, pointer, dimension(:,:) :: hd_g        
        double precision, pointer, dimension(:)   :: hd_t

      end type avosts_struct

      integer            :: corr_opt
      integer            :: dbug_opt
      integer            :: mplx_opt
      real               :: scal_rms
      character(len=256) :: otrc_opt


!!------------------------------ interfaces ----------------------------------!!
!!------------------------------ interfaces ----------------------------------!!
!!------------------------------ interfaces ----------------------------------!!


!!--------------------------------- data -------------------------------------!!
!!--------------------------------- data -------------------------------------!!
!!--------------------------------- data -------------------------------------!!

      integer,                  save :: lunprint  ! unit number for printing.
      type(avosts_struct),pointer,save :: object    ! needed for traps.

      contains


!!------------------------------- create -------------------------------------!!
!!------------------------------- create -------------------------------------!!
!!------------------------------- create -------------------------------------!!


      subroutine avosts_create (obj)
      type(avosts_struct),pointer :: obj       ! arguments
      integer                     :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) then
         call pc_error ('Unable to allocate obj in avosts_create.')
      end if

      allocate(obj%buhci(1))
      nullify(obj%hd_g)
      nullify(obj%tr_g)
      nullify(obj%hd_t)
      nullify(obj%tr_t)
      call buhci_create (obj%buhci(1)%obj)

      call avosts_initialize (obj)
      end subroutine avosts_create


!!--------------------------------- delete -----------------------------------!!
!!--------------------------------- delete -----------------------------------!!
!!--------------------------------- delete -----------------------------------!!


      subroutine avosts_delete (obj)
      type(avosts_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      call avosts_wrapup (obj)

      call buhci_delete (obj%buhci(1)%obj)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in avosts_delete")
      end subroutine avosts_delete


!!------------------------------ initialize ----------------------------------!!
!!------------------------------ initialize ----------------------------------!!
!!------------------------------ initialize ----------------------------------!!


      subroutine avosts_initialize (obj)
      type(avosts_struct),intent(inout) :: obj       ! arguments

      obj%cwin_cdp = 21
      obj%corr_opt = 'NO'
      obj%cwin_tim = 500
      obj%cwin_vsh = 0
      obj%dbug_opt = 'SILENT'
      obj%etim_win = 0
      obj%stim_win = 0
      obj%ncdp_max = 500
      obj%pass_opt = 'YES'

      obj%init_iterations = 1
      obj%fcdp = 1
      obj%lagndx = 1
      obj%purged = 0
      obj%end_of_data = .false.

      call buhci_init_parms (obj%buhci(1)%obj)

      call avosts_update (obj)
      end subroutine avosts_initialize


!!--------------------------- start of update --------------------------------!!
!!--------------------------- start of update --------------------------------!!
!!--------------------------- start of update --------------------------------!!


      subroutine avosts_update (obj)
      type(avosts_struct),intent(inout),target :: obj             ! arguments

      integer :: iter_cnt
      integer :: iter_num
      integer :: ntrtemp

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!--------------------------- read parameters --------------------------------!!
!!--------------------------- read parameters --------------------------------!!
!!--------------------------- read parameters --------------------------------!!

      obj%ipn = pc_get_ipn()

      call pc_get_global ('numtr'   , obj%numtr)
      call pc_get_global ('gathered', obj%gathered)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)
      call pc_get_global ('grid'    , obj%grid)

      call pc_get('CORR_OPT', obj%corr_opt, avosts_corr_opt)
      call pc_get('CWIN_CDP', obj%cwin_cdp, avosts_cwin_cdp)
      call pc_get('CWIN_TIM', obj%cwin_tim, avosts_cwin_tim)
      call pc_get('CWIN_VSH', obj%cwin_vsh, avosts_cwin_vsh)
      call pc_get('DBUG_OPT', obj%dbug_opt, avosts_dbug_opt)
      call pc_get('ETIM_WIN', obj%etim_win, avosts_etim_win)
      call pc_get('NCDP_MAX', obj%ncdp_max, avosts_ncdp_max)
      call pc_get('PASS_OPT', obj%pass_opt, avosts_pass_opt)
      call pc_get('STIM_WIN', obj%stim_win, avosts_stim_win)

!      *** screen traps ***

      call pc_call_screen_trap('GENERALPARAMETERS', avosts_generalparameters)

!      *** end trap ***

      call pc_call_end_trap(avosts_end)


!!--------------------------- verify parameters ------------------------------!!
!!--------------------------- verify parameters ------------------------------!!
!!--------------------------- verify parameters ------------------------------!!


!!------------------------- call processes internally ------------------------!!
!!------------------------- call processes internally ------------------------!!
!!------------------------- call processes internally ------------------------!!

      call pc_get_pdata('SURVEY_UNITS', obj%survey_units)
      call pc_get_jdata('PATHNAME_DIR', obj%pathname_dir)


!!------------------------- write parameters ---------------------------------!!
!!------------------------- write parameters ---------------------------------!!
!!------------------------- write parameters ---------------------------------!!


      call pc_put_options_field('CORR_OPT', (/'NO ', 'LIN', 'SQR'/) )
      call pc_put_options_field('DBUG_OPT', (/'SILENT     ', 'INFORMATION',&
     &                                        'MEM_CHECK  ', 'EXIT_CODES ',&
     &                                        'EVERYTHING '/) )
      call pc_put_options_field('PASS_OPT', (/'NO ', 'YES'/) )

      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('gathered', obj%gathered)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)
      call pc_put_global ('grid'    , obj%grid)

      call pc_put('CORR_OPT', obj%corr_opt)
      call pc_put('CWIN_CDP', obj%cwin_cdp)
      call pc_put('CWIN_TIM', obj%cwin_tim)
      call pc_put('CWIN_VSH', obj%cwin_vsh)
      call pc_put('DBUG_OPT', obj%dbug_opt)
      call pc_put('ETIM_WIN', obj%etim_win)
      call pc_put('NCDP_MAX', obj%ncdp_max)
      call pc_put('PASS_OPT', obj%pass_opt)
      call pc_put('STIM_WIN', obj%stim_win)

      call pc_put_control ('ntapes'       , 0)
      call pc_put_control ('need_request' , .true.)
      call pc_put_control ('need_label'   , .true.)
      call pc_put_control ('twosets'      , .false.)
      call pc_put_control ('nscratch'     , 0)
      call pc_put_control ('nstore'       , 0)
      call pc_put_control ('iftd'         , .false.)
      call pc_put_control ('ndisk'        , 0)
      call pc_put_control ('setup_only'   , .false.)
      call pc_put_control ('parallel_safe', .false.)

      select case (obj%corr_opt)
         case ('NO')
            corr_opt = 0
         case ('LIN')
            corr_opt = 1
         case ('SQR')
            corr_opt = 2
      end select
      select case (obj%dbug_opt)
         case ('SILENT')
            dbug_opt = -1
         case ('INFORMATION')
            dbug_opt = 0
         case ('MEM_CHECK')
            dbug_opt = 1
         case ('EXIT_CODES')
            dbug_opt = 2
         case ('EVERYTHING')
            dbug_opt = 3
      end select
      select case (obj%pass_opt)
         case ('NO')
            mplx_opt = 0
         case ('YES')
            mplx_opt = 2
      end select
      iter_cnt = obj%init_iterations
      iter_num = 1

      if(obj%pass_opt .eq. 'YES') then
         ntrtemp = (obj%numtr + 8) * obj%cwin_cdp
      else
         ntrtemp = 8 * obj%ncdp_max
      end if

      call buhci_set_parm(obj%buhci(1)%obj, 'iter_cnt', iter_cnt)
      call buhci_set_parm(obj%buhci(1)%obj, 'iter_num', iter_num)
      call buhci_set_parm(obj%buhci(1)%obj, 'corr_opt', corr_opt)
      call buhci_set_parm(obj%buhci(1)%obj, 'cwin_cdp', obj%cwin_cdp)
      call buhci_set_parm(obj%buhci(1)%obj, 'cwin_tim', obj%cwin_tim)
      call buhci_set_parm(obj%buhci(1)%obj, 'cwin_vsh', obj%cwin_vsh)
      call buhci_set_parm(obj%buhci(1)%obj, 'dbug_opt', dbug_opt)
      call buhci_set_parm(obj%buhci(1)%obj, 'stim_win', obj%stim_win)
      call buhci_set_parm(obj%buhci(1)%obj, 'etim_win', obj%etim_win)
      call buhci_set_parm(obj%buhci(1)%obj, 'ncdp_max', obj%ncdp_max)      
      call buhci_set_parm(obj%buhci(1)%obj, 'mplx_opt', mplx_opt)

      otrc_opt = 'ABCDSTRI'
      call buhci_set_parm(obj%buhci(1)%obj, 'otrc_opt', otrc_opt)
      scal_rms = -1.0
      call buhci_set_parm(obj%buhci(1)%obj, 'scal_rms', scal_rms)

      if(ntrtemp .gt. obj%numtr) then
         obj%numtr = ntrtemp
         call pc_put_global('numtr', obj%numtr)
      end if

!!------------------------- prepare for execution ----------------------------!!
!!------------------------- prepare for execution ----------------------------!!
!!------------------------- prepare for execution ----------------------------!!

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.


!!--------------------------- finish update ----------------------------------!!
!!--------------------------- finish update ----------------------------------!!
!!--------------------------- finish update ----------------------------------!!


      end subroutine avosts_update


!!-------------------------------- traps -------------------------------------!!
!!-------------------------------- traps -------------------------------------!!
!!-------------------------------- traps -------------------------------------!!


! *** Trap for variable CORR_OPT ***

      subroutine avosts_corr_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avosts_corr_opt

! *** Trap for variable CWIN_CDP ***

      subroutine avosts_cwin_cdp(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%cwin_cdp)
      if(object%cwin_cdp .lt. 1 .or. object%cwin_cdp .gt. 99999) then
         write (msg, '(a,a)') keyword,&
     &' must be between 1 and 99999.&
     & Restoring to default of 21.'
         call pc_error(msg)
         object%cwin_cdp = 21
         call pc_put(keyword, object%cwin_cdp)
      end if

      return
      end subroutine avosts_cwin_cdp

! *** Trap for variable CWIN_TIM ***

      subroutine avosts_cwin_tim(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%cwin_tim)
      if(object%cwin_tim .lt. 10 .or. object%cwin_tim .gt. 5000) then
         write (msg, '(a,a)') keyword,&
     &' must be between 10 and 5000.&
     & Restoring to default of 500.'
         call pc_error(msg)
         object%cwin_tim = 500
         call pc_put(keyword, object%cwin_tim)
      end if

      return
      end subroutine avosts_cwin_tim

! *** Trap for variable CWIN_VSH ***

      subroutine avosts_cwin_vsh(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%cwin_vsh)
      if(object%cwin_vsh .lt. -5000 .or. object%cwin_vsh .gt. 5000) then
         write (msg, '(a,a)') keyword,&
     &' must be between -5000 and 5000.&
     & Restoring to default of 0.'
         call pc_error(msg)
         object%cwin_vsh = 0
         call pc_put(keyword, object%cwin_vsh)
      end if

      return
      end subroutine avosts_cwin_vsh

! *** Trap for variable DBUG_OPT ***

      subroutine avosts_dbug_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avosts_dbug_opt

! *** Trap for variable ETIM_WIN ***

      subroutine avosts_etim_win(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%etim_win)
      if(object%etim_win .lt. 0 .or. object%etim_win .gt. 99999) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 99999.&
     & Restoring to default of 0.'
         call pc_error(msg)
         object%etim_win = 0
         call pc_put(keyword, object%etim_win)
      end if

      return
      end subroutine avosts_etim_win

! *** Trap for variable NCDP_MAX ***

      subroutine avosts_ncdp_max(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%ncdp_max)
      if(object%ncdp_max .lt. 1) then
         write (msg, '(a,a)') keyword,&
     &' must be greater than 0.&
     & Restoring to default of 500.'
         call pc_error(msg)
         object%ncdp_max = 500
         call pc_put(keyword, object%ncdp_max)
      end if

      return
      end subroutine avosts_ncdp_max

! *** Trap for variable PASS_OPT ***

      subroutine avosts_pass_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avosts_pass_opt

! *** Trap for variable STIM_WIN ***

      subroutine avosts_stim_win(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%stim_win)
      if(object%stim_win .lt. 0 .or. object%stim_win .gt. 99999) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 99999.&
     & Restoring to default of 0.'
         call pc_error(msg)
         object%stim_win = 0
         call pc_put(keyword, object%stim_win)
      end if

      return
      end subroutine avosts_stim_win


! *** Screen trap for  GENERALPARAMETERS ***

      subroutine avosts_generalparameters(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avosts_generalparameters

! *** End Trap ***

      subroutine avosts_end
      implicit none

      return
      end subroutine avosts_end



!!---------------------------- main execution --------------------------------!!
!!---------------------------- main execution --------------------------------!!
!!---------------------------- main execution --------------------------------!!


      subroutine avosts (obj,ntr,hd,tr)
      type(avosts_struct),intent(inout) :: obj                    ! arguments
      integer,            intent(inout) :: ntr                    ! arguments
      double precision,   intent(inout) :: hd(:,:)                ! arguments
      real,               intent(inout) :: tr(:,:)                ! arguments

      integer :: i, j   
      integer ::       jerr 
      integer :: promode, gath_type
      integer :: trc_type, trc_type_in
      integer :: nit, cnt, ndx, ntr1
      integer :: npiped
      integer :: nio, max_nio, subndx
      integer :: nflushed, max_nflushed
      integer :: ispurge, maxpurge
      integer :: hdrlag, hdrsub
      integer ::            ieoj_flag 
      integer :: gather_done
      integer :: stackword
      integer :: max_gather
      integer :: ntr_gather_in
      integer :: ntr_gather_out
      integer :: iflv, illv
      integer :: idiv, irem, indx
      integer :: acnt, bcnt, abcnt
      integer :: andx(200)
      integer :: bndx(200)
      double precision ::       ocdp 
      double precision :: cdp, cdp_out
      double precision ::         offset_out 
      double precision :: dom_freq, avo_angle
      double precision :: user

      if (.not. obj%buhci(1)%obj%initialized) then

         allocate(obj%hd_g(ppavo_nhdrpz+obj%nwih, obj%numtr))
         allocate(obj%tr_g(obj%ndpt, obj%numtr))

         allocate(obj%hd_t(ppavo_nhdrpz+obj%nwih))
         allocate(obj%tr_t(obj%ndpt))

         obj%buhci(1)%obj%ipn      = obj%ipn
         obj%buhci(1)%obj%numtr    = obj%numtr
         obj%buhci(1)%obj%gathered = obj%gathered
         obj%buhci(1)%obj%nwih     = obj%nwih
         obj%buhci(1)%obj%ndpt     = obj%ndpt
         obj%buhci(1)%obj%tstrt    = obj%tstrt
         obj%buhci(1)%obj%dt       = obj%dt
         obj%buhci(1)%obj%grid     = obj%grid

         obj%buhci(1)%obj%sampratz = obj%dt * 1000
         if(obj%survey_units .eq. 'FEET') then
            obj%buhci(1)%obj%iunitsz  = ppavo_englishpz
         else
            obj%buhci(1)%obj%iunitsz  = ppavo_metricpz
         end if
         obj%buhci(1)%obj%imultcz  = 0
         obj%buhci(1)%obj%icdpasnz = 1
         obj%buhci(1)%obj%igeoasnz = 1
         obj%buhci(1)%obj%ipsortz  = ppavo_cdppz
         obj%buhci(1)%obj%idtypez  = ppavo_normalpz
         obj%buhci(1)%obj%maxdtrz  = obj%numtr
         obj%buhci(1)%obj%numsmpz  = obj%ndpt
         obj%buhci(1)%obj%nthz     = ppavo_nhdrpz + obj%nwih
         obj%buhci(1)%obj%cleanupz = .false.

         call buhci_init (obj%buhci(1)%obj, nio)
         if(nio .eq. FATAL_ERROR) then
            ntr = FATAL_ERROR
            return
         end if

      end if

      if(ntr .eq. NO_MORE_TRACES) obj%end_of_data = .true.

      ntr_gather_in = ntr
      ntr_gather_out = 0

      ntr1 = 1
      maxpurge = obj%init_iterations
      acnt = 0
      bcnt = 0
      abcnt = 0

!     Record indices and count of A and B traces.
      do i = 1, ntr
         trc_type = hd(ppavo_trc_typez, i)
         if(trc_type .eq. 43) then
            acnt = acnt + 1
            andx(acnt) = i
         else if (trc_type .eq. 44) then
            bcnt = bcnt + 1
            bndx(bcnt) = i
         end if
      end do
      abcnt = acnt + bcnt

      if(obj%end_of_data) then
         ieoj_flag = 0
         ntr = 2
         abcnt = 2
         goto 69
      end if

!     Skip if either A or B trace count is zero.
      if(acnt .eq. 0 .or. bcnt .eq. 0) then
         jerr = 1
         goto 99999
      end if

!     Skip if unequal number of A and B traces.
      if(acnt .ne. bcnt) then
         jerr = 2
         goto 99999
      end if

!     Set end-of-job header to zero.
      obj%hd_t(ppavo_end_jobz) = ppavo_nlastpz

!     Get inline/crossline indices (3d data only).
!     Assume iline is constant (2d data only).
      obj%ilineno = hd(ppavo_iline_noz, 1)
      obj%xlineno = hd(ppavo_xline_noz, 1)         

!     Get CDP number for the gather (1st trace only).
      cdp = hd(ppavo_cdpz, 1)
      obj%ocdpX = hd(ppavo_cdpz, 1)
      
      do nit = 1, ntr
!        Copy trace sample values
         do j = 1, obj%ndpt
            obj%tr_g(j, nit) = tr(j, nit)
         end do
!        Copy trace header values.
         do j = 1, obj%nwih
            obj%hd_g(ppavo_nhdrpz+j, nit) = hd(j, nit)
         end do
         do j = 1, ppavo_nhdrpz
            obj%hd_g(j, nit) = 0.0
         end do

         obj%hd_g(ppavo_trace_noz,  nit) = hd(ppavo_trace_noz,  nit)
         obj%hd_g(ppavo_head_mutez, nit) = hd(ppavo_head_mutez, nit)
         obj%hd_g(ppavo_tail_mutez, nit) = hd(ppavo_tail_mutez, nit)
         obj%hd_g(ppavo_cdpz,       nit) = hd(ppavo_cdpz,       nit)
         obj%hd_g(ppavo_seq_noz,    nit) = hd(ppavo_seq_noz,    nit)
         obj%hd_g(ppavo_trc_foldz,  nit) = hd(ppavo_trc_foldz,  nit)
         obj%hd_g(ppavo_offsetz,    nit) = hd(ppavo_offsetz,    nit)
         obj%hd_g(ppavo_iline_noz,  nit) = hd(ppavo_iline_noz,  nit)
         obj%hd_g(ppavo_xline_noz,  nit) = hd(ppavo_xline_noz,  nit)
        
         obj%hd_g(ppavo_trc_typez,  nit) = hd(ppavo_trc_typez,  nit)
         obj%hd_g(ppavo_gath_typez, nit) = hd(ppavo_gath_typez, nit)
         obj%hd_g(ppavo_stackwordz, nit) = hd(ppavo_stackwordz, nit)
         obj%hd_g(ppavo_avo_anglez, nit) = hd(ppavo_avo_anglez, nit)
         obj%hd_g(ppavo_dom_freqz,  nit) = hd(ppavo_dom_freqz,  nit)
         obj%hd_g(ppavo_userz,      nit) = hd(ppavo_userz,      nit)

      end do

60    ieoj_flag = 0
      nit = 0
      promode = 1
      npiped = 0
      nflushed = 0
      gather_done = 0

62    max_nflushed = 8 * abcnt
      max_nio = ntr
      max_gather = abcnt / 2
      acnt = 0
      bcnt = 0

!     Run traces through B_UHCI to get A* and RMS.
64    if(npiped .le. max_nio .and. gather_done .lt. max_gather) then

         nit = npiped
         idiv = 2
         irem = mod(npiped, idiv)
         indx = (nit+2) / idiv

!        Copy trace sample values
         ndx = npiped + 1
         subndx = ndx

         trc_type  = obj%hd_g(ppavo_trc_typez,  ndx)
         gath_type = obj%hd_g(ppavo_gath_typez, ndx)
         stackword = obj%hd_g(ppavo_stackwordz, ndx)
         avo_angle = obj%hd_g(ppavo_avo_anglez, ndx)
         dom_freq  = obj%hd_g(ppavo_dom_freqz,  ndx)
         user      = obj%hd_g(ppavo_userz,      ndx)

         do j = 1, obj%ndpt
            obj%tr_t(j) = obj%tr_g(j, ndx)
         end do

!        Copy trace header values.
         do j = 1, ppavo_nhdrpz + obj%nwih
            obj%hd_t(j) = obj%hd_g(j, ndx)
         end do

         if(stackword .le. 0) then
            trc_type = ppavo_deadpz
         end if
         obj%hd_t(ppavo_trc_typez) = trc_type

         if(trc_type .eq. 43) acnt = acnt + 1
         if(trc_type .eq. 44) bcnt = bcnt + 1

         obj%hd_t(ppavo_purgez)    = obj%purged
         if(obj%purged .gt. 0) then
            obj%hd_t(ppavo_end_jobz) = 1
         else
            obj%hd_t(ppavo_end_jobz) = 0
         end if
         obj%hd_t(ppavo_end_ensz)  = ppavo_nlastpz
         obj%hd_t(ppavo_seq_noz)   = npiped
         obj%hd_t(ppavo_cdpz)      = obj%fcdp
         obj%hd_t(ppavo_userz)     = cdp
         obj%hd_t(ppavo_iline_noz) = obj%ilineno
         obj%hd_t(ppavo_xline_noz) = obj%xlineno

         call ppavo_trlive(obj%tr_t, obj%ndpt, iflv, illv)
         obj%hd_t(ppavo_live_sz)  = (iflv-1) * obj%dt * 1000
         obj%hd_t(ppavo_live_ez)  = (illv-1) * obj%dt * 1000
         obj%hd_t(ppavo_full_sz)   = obj%tstrt
         obj%hd_t(ppavo_full_ez)   = obj%tstrt + ((obj%ndpt-1) * obj%dt)
         obj%hd_t(ppavo_full_sz)   = obj%hd_t(ppavo_full_sz) * 1000
         obj%hd_t(ppavo_full_ez)   = obj%hd_t(ppavo_full_ez) * 1000
!         obj%hd_t(ppavo_live_sz)   = obj%hd_t(ppavo_full_sz)
!         obj%hd_t(ppavo_live_ez)   = obj%hd_t(ppavo_full_ez)
         obj%hd_t(ppavo_trc_foldz) = 1.0
         obj%hd_t(ppavo_amp_normz) = 1.0
         obj%hd_t(ppavo_lagz)      = obj%lagndx
         obj%hd_t(ppavo_subz)      = subndx

!        If fillmode or pipemode: increment npiped.
         if(promode .eq. 1 .or. promode .eq. 2) then
            npiped = npiped + 1
            if(npiped .eq. max_nio .or. trc_type .eq. 44) then
               obj%hd_t(ppavo_end_ensz) = ppavo_lasttrpz
            end if
            trc_type_in = trc_type
         end if

!        If flushmode: send a dummy trace.
         if(promode .eq. 3) then
            trc_type = ppavo_dummypz
            obj%hd_t(ppavo_trc_typez) = trc_type
         end if

!        Call main B_UHCI work routine.
         call buhci_work(obj%buhci(1)%obj, ntr1, obj%hd_t, obj%tr_t, promode)
         if(ntr1 .eq. FATAL_ERROR) then
            ntr = FATAL_ERROR
            return
         end if

!        If quitmode: skip rest of process.
         if(promode .eq. 0) then
            npiped = max_nio + 1
         end if

!        If pipemode or flushmode: increment output counter.
         if(promode .eq. 2 .or. promode .eq. 3) then

            nflushed = nflushed + 1

            trc_type  = obj%hd_t(ppavo_trc_typez)
            gath_type = obj%hd_t(ppavo_gath_typez)
            stackword = obj%hd_t(ppavo_stackwordz)
            avo_angle = obj%hd_t(ppavo_avo_anglez)
            dom_freq  = obj%hd_t(ppavo_dom_freqz)
            user      = obj%hd_t(ppavo_userz)
            ocdp      = obj%hd_t(ppavo_userz)
            if(obj%end_of_data .and. ocdp .eq. obj%ocdpX) then
               ieoj_flag = 1
            end if
            ispurge = obj%hd_t(ppavo_purgez)

            if(ispurge .ne. 0) then
               goto 69
            end if

            ntr_gather_out = ntr_gather_out + 1
            cnt = ntr_gather_out

            cdp_out = obj%hd_t(ppavo_userz)
            offset_out = 0.0
            if(trc_type .eq. 1) then
               offset_out = obj%hd_t(ppavo_offsetz)
            end if
            hdrlag = obj%hd_t(ppavo_lagz)
            hdrsub = obj%hd_t(ppavo_subz)
            if(hdrsub .lt. 1 .or. hdrsub .gt. ntr_gather_in) then
               hdrsub = 1
            end if

!           Copy trace sample values
            do j = 1, obj%ndpt
               tr(j, ntr_gather_out) = obj%tr_t(j)
            end do

!           Copy trace header values.
            do j = 1, obj%nwih
               hd(j, ntr_gather_out) = obj%hd_t(j+ppavo_nhdrpz)
            end do
            hd(ppavo_cdpz,       ntr_gather_out) = cdp_out
            hd(ppavo_offsetz,    ntr_gather_out) = offset_out
            hd(ppavo_trace_noz,  ntr_gather_out) = hdrsub

            hd(ppavo_trc_typez,  ntr_gather_out) = trc_type
            hd(ppavo_gath_typez, ntr_gather_out) = gath_type
            hd(ppavo_stackwordz, ntr_gather_out) = hdrlag
            hd(ppavo_avo_anglez, ntr_gather_out) = avo_angle
            hd(ppavo_dom_freqz,  ntr_gather_out) = dom_freq
            hd(ppavo_userz,      ntr_gather_out) = user

         end if

!        If fillmode or pipemode: increment input counters.
         if(promode .eq. 1 .or. promode .eq. 2) then
            if(trc_type_in .eq. 44) then
               gather_done = gather_done + 1
               obj%fcdp = obj%fcdp + 1
            end if
         end if

         goto 64

      end if

69    continue

!     Check for end-of-job flag.
      if (ieoj_flag .ne. 0) then
         obj%purged = maxpurge + 1
         goto 99999
      end if

!     If at end-of-job, manually force a purge.
      if(obj%end_of_data .and. obj%purged .lt. maxpurge) then
         nio = abcnt
         obj%purged = obj%purged + 1
         obj%ilineno = obj%ilineno + 1
         goto 60
      end if

99999 continue
      ntr = ntr_gather_out
      if(ntr .eq. 0 .and. .not. obj%end_of_data)  ntr = NEED_TRACES
      if(ntr .eq. 0 .and. obj%end_of_data) ntr = NO_MORE_TRACES

      end subroutine avosts


!!-------------------------------- wrapup ------------------------------------!!
!!-------------------------------- wrapup ------------------------------------!!
!!-------------------------------- wrapup ------------------------------------!!


      subroutine avosts_wrapup (obj)
      type(avosts_struct),intent(inout) :: obj       ! arguments
      integer :: ntr
      integer :: promode

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      if(obj%buhci(1)%obj%initialized) then
         obj%buhci(1)%obj%cleanupz = .true.
         call buhci_work(obj%buhci(1)%obj, ntr, obj%hd_t, obj%tr_t, promode)
      end if
      if(associated(obj%hd_g)) deallocate(obj%hd_g)
      if(associated(obj%tr_g)) deallocate(obj%tr_g)
      if(associated(obj%hd_t)) deallocate(obj%hd_t)
      if(associated(obj%tr_t)) deallocate(obj%tr_t)

      end subroutine avosts_wrapup


!!------------------------------ end of module -------------------------------!!
!!------------------------------ end of module -------------------------------!!
!!------------------------------ end of module -------------------------------!!


      end module avosts_module


!!---------------------------------- end -------------------------------------!!
!!---------------------------------- end -------------------------------------!!
!!---------------------------------- end -------------------------------------!!

