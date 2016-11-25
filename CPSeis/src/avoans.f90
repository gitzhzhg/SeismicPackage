!<CPS_v1 type="PROCESS"/>
!!------------------------------- avoans.f90 ---------------------------------!!
!!------------------------------- avoans.f90 ---------------------------------!!
!!------------------------------- avoans.f90 ---------------------------------!!

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
! Name       : AVOANS
! Category   : velocity_analysis
! Written    : 2003-08-26   by: Bill Lucas
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : AVO Alternate Norm Suite (AVOANS).
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This tool computes an alternate suite of traces (described below) for every
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
! --> Insert globals that this process uses or changes:
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       --> specify action taken.
! GATHERED  whether traces are a legitimate gather  --> specify action taken.
! NWIH      number of words in trace header         --> specify action taken.
! NDPT      number of sample values in trace        --> specify action taken.
! TSTRT     starting time on trace                  --> specify action taken.
! DT        trace sample interval                   --> specify action taken.
! GRID      grid transformation structure           --> specify action taken.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! --> Insert header words used or changed by this process:
!
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
!                                       41 = imag part of corr coeff trace
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
!  1. 2005-01-10  B. Lucas    Initial version.
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
! in the C_UHCI and PPAVO primitives.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!
!<NS General Parameters>
!                           AVO Alternate Norm. Suite
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
!   | Power of Statistical Norms =~~~~~~`FFFFFFFFF
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
!   <PARMS Power of Statistical Norms     [NORM_POW]>
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
!<Help KEYWORD="NCDP_MAX">
!<Tip> Maximum number of CDPs per line.</Tip>
! Default = 500
! Allowed = greater than 0, less than 20000
! Maximum number of CDPs per line.
!</Help>
!
!<Help KEYWORD="NORM_POW">
!<Tip> Power of Statistical Norms.</Tip>
! Default = 1.0
! Allowed = greater than or equal to 0.0, less than or equal to 2.0
! Power of statistical norms:
!   Use 2.0 for L2 norm.
!   Use 1.0 for L1 norm.
!   Use 0.5 for L1/2 norm.
!</Help>
!
!<Help KEYWORD="PASS_OPT">
!<Tip> Pass pre-stack data?</Tip>
! Default = YES
! Allowed = NO, YES
! Pass pre-stack data?
!</Help>
!
!<Help KEYWORD="STIM_WIN">
!<Tip> Start time of analysis window (msec).</Tip>
! Default = 0
! Allowed = greater than or equal to 0, less than or equal to 99999
! Start time of analysis window (msec).
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module avoans_module
      use pc_module
      use named_constants_module
      use memman_module
      use grid_module            ! if you need the grid transformation.
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.
      use cuhci_module
      use ppavo_module

! --> Insert here any other modules used by this process.

      implicit none
      private
      public :: avoans_create
      public :: avoans_initialize
      public :: avoans_update
      public :: avoans_delete
      public :: avoans            ! main trace processing routine.
      public :: avoans_wrapup

      character(len=100),public,save :: AVOANS_IDENT = &
'$Id: avoans.f90,v 1.2 2006/06/20 13:11:44 Menger prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: cuhci_ptr
         type(cuhci_struct), pointer :: obj
      end type cuhci_ptr

      type,public :: avoans_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.

! --> Below are commonly used globals - edit or remove as appropriate:

        integer                    :: ipn      ! process number.
        integer                    :: numtr    ! max number of input traces.
        logical                    :: gathered ! whether properly gathered.
        integer                    :: nwih     ! number of header words.
        integer                    :: ndpt     ! number of trace samples.
        real                       :: tstrt    ! time of 1st trace sample (sec).
        real                       :: dt       ! trace sample interval (sec).
        type(grid_struct)          :: grid     ! grid transform.

        integer                    :: cwin_cdp
        integer                    :: cwin_tim
        integer                    :: cwin_vsh
        character(len=11)          :: dbug_opt
        integer                    :: etim_win
        integer                    :: ncdp_max
        real                       :: norm_pow
        character(len=3)           :: pass_opt
        integer                    :: stim_win

        integer                    :: init_iterations
        integer                    :: fcdp
        integer                    :: lagndx
        integer                    :: purged
        logical                    :: end_of_data
        double precision           :: ilineno
        double precision           :: xlineno
        double precision           :: ocdpX
        character(len=20)          :: survey_units
        character(len=FILENAME_LENGTH) :: pathname_dir

        type(cuhci_ptr), pointer, dimension(:)   :: cuhci
        double precision, pointer, dimension(:,:) :: hd_x
        real,             pointer, dimension(:,:) :: tr_x
        double precision, pointer, dimension(:,:) :: hd_g
        real,             pointer, dimension(:,:) :: tr_g
        double precision, pointer, dimension(:,:) :: hd_m
        real,             pointer, dimension(:,:) :: tr_m
        double precision, pointer, dimension(:)   :: hd_t
        real,             pointer, dimension(:)   :: tr_t

      end type avoans_struct

      integer            :: corr_opt
      integer            :: dbug_opt
      integer            :: mplx_opt
      real               :: scal_rms
      character(len=256) :: otrc_opt


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


! --> Include any required interfaces here.


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(avoans_struct),pointer,save :: object    ! needed for traps.

! --> Insert here any data declarations needed by this process.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine avoans_create (obj)
      type(avoans_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in avoans_create")


! --> Nullify any additional pointers in the OBJ data structure here.
      allocate(obj%cuhci(2))      
      nullify(obj%hd_x)
      nullify(obj%tr_x)
      nullify(obj%hd_g)
      nullify(obj%tr_g)
      nullify(obj%hd_m)
      nullify(obj%tr_m)
      nullify(obj%hd_t)
      nullify(obj%tr_t)
      call cuhci_create (obj%cuhci(1)%obj)
      call cuhci_create (obj%cuhci(2)%obj)

      call avoans_initialize (obj)
      end subroutine avoans_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine avoans_delete (obj)
      type(avoans_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      call avoans_wrapup (obj)


! --> Deallocate any additional pointers in the OBJ data structure here.
      call cuhci_delete (obj%cuhci(1)%obj)
      call cuhci_delete (obj%cuhci(2)%obj)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in avoans_delete")
      end subroutine avoans_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine avoans_initialize (obj)
      type(avoans_struct),intent(inout) :: obj       ! arguments

! --> Change the default values below as needed

      obj%cwin_cdp = 21
      obj%cwin_tim = 500
      obj%cwin_vsh = 0
      obj%dbug_opt = 'SILENT'
      obj%etim_win = 0
      obj%ncdp_max = 500
      obj%norm_pow = 1.0
      obj%pass_opt = 'YES'
      obj%stim_win = 0

      obj%init_iterations = 2
      obj%fcdp = 1
      obj%lagndx = 1
      obj%purged = 0
      obj%end_of_data = .false.

      call cuhci_init_parms (obj%cuhci(1)%obj)
      call cuhci_init_parms (obj%cuhci(2)%obj)

      call avoans_update (obj)
      end subroutine avoans_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine avoans_update (obj)
      type(avoans_struct),intent(inout),target :: obj             ! arguments

! --> Insert code to declare all required local variables.
      integer :: nit, iter_cnt, iter_num
      integer :: ntrtemp

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!



! --> Delete any of the globals below that are not needed:

      obj%ipn = pc_get_ipn()

      call pc_get_global ('numtr'   , obj%numtr)
      call pc_get_global ('gathered', obj%gathered)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)
      call pc_get_global ('grid'    , obj%grid)

      call pc_get('CWIN_CDP', obj%cwin_cdp, avoans_cwin_cdp)
      call pc_get('CWIN_TIM', obj%cwin_tim, avoans_cwin_tim)
      call pc_get('CWIN_VSH', obj%cwin_vsh, avoans_cwin_vsh)
      call pc_get('DBUG_OPT', obj%dbug_opt, avoans_dbug_opt)
      call pc_get('ETIM_WIN', obj%etim_win, avoans_etim_win)
      call pc_get('NCDP_MAX', obj%ncdp_max, avoans_ncdp_max)
      call pc_get('NORM_POW', obj%norm_pow, avoans_norm_pow)
      call pc_get('PASS_OPT', obj%pass_opt, avoans_pass_opt)
      call pc_get('STIM_WIN', obj%stim_win, avoans_stim_win)

!      *** screen traps ***

      call pc_call_screen_trap('GENERALPARAMETERS', avoans_generalparameters)

!      *** end trap ***

      call pc_call_end_trap(avoans_end)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


! --> Insert code to verify process parameters here (and/or in traps).


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
      call pc_get_pdata('SURVEY_UNITS', obj%survey_units)
      call pc_get_jdata('PATHNAME_DIR', obj%pathname_dir)


! --> Insert code to call internal processes to verify process parameters.


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field('DBUG_OPT', (/'SILENT     ', 'INFORMATION',&
     &                                        'MEM_CHECK  ', 'EXIT_CODES ',&
     &                                        'EVERYTHING '/) )
      call pc_put_options_field('PASS_OPT', (/'NO ', 'YES'/) )

! --> Delete any of the globals below that have not changed:

      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('gathered', obj%gathered)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)
      call pc_put_global ('grid'    , obj%grid)

      call pc_put('CWIN_CDP', obj%cwin_cdp)
      call pc_put('CWIN_TIM', obj%cwin_tim)
      call pc_put('CWIN_VSH', obj%cwin_vsh)
      call pc_put('DBUG_OPT', obj%dbug_opt)
      call pc_put('ETIM_WIN', obj%etim_win)
      call pc_put('NCDP_MAX', obj%ncdp_max)
      call pc_put('NORM_POW', obj%norm_pow)
      call pc_put('PASS_OPT', obj%pass_opt)
      call pc_put('STIM_WIN', obj%stim_win)

! --> Change the control defaults below as appropriate:

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

! --> Add here any other parameter cache calls such as to set sensitivities.
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
            mplx_opt = 3
         case ('YES')
            mplx_opt = 2
      end select

      ntrtemp = 8 * obj%ncdp_max

      do nit = 1, obj%init_iterations
         iter_cnt = obj%init_iterations
         iter_num = nit
         call cuhci_set_parm(obj%cuhci(nit)%obj, 'iter_cnt', iter_cnt)
         call cuhci_set_parm(obj%cuhci(nit)%obj, 'iter_num', iter_num)
         if(nit .eq. 2) then
            corr_opt = 2
            call cuhci_set_parm(obj%cuhci(nit)%obj, 'corr_opt', corr_opt)
         end if
         call cuhci_set_parm(obj%cuhci(nit)%obj, 'alph_fct', obj%norm_pow)
         write(*,*) 'cwincdp = ',obj%cwin_cdp,nit
         call cuhci_set_parm(obj%cuhci(nit)%obj, 'cwin_cdp', obj%cwin_cdp)
         call cuhci_set_parm(obj%cuhci(nit)%obj, 'cwin_tim', obj%cwin_tim)
         call cuhci_set_parm(obj%cuhci(nit)%obj, 'cwin_vsh', obj%cwin_vsh)
         call cuhci_set_parm(obj%cuhci(nit)%obj, 'dbug_opt', dbug_opt)
         call cuhci_set_parm(obj%cuhci(nit)%obj, 'stim_win', obj%stim_win)
         call cuhci_set_parm(obj%cuhci(nit)%obj, 'etim_win', obj%etim_win)
         call cuhci_set_parm(obj%cuhci(nit)%obj, 'ncdp_max', obj%ncdp_max)
         call cuhci_set_parm(obj%cuhci(nit)%obj, 'mplx_opt', mplx_opt)

         if(nit .eq. 1) then
            otrc_opt = 'STAB'
         else
            otrc_opt = 'ABCDSTRI'
         end if
         call cuhci_set_parm(obj%cuhci(nit)%obj, 'otrc_opt', otrc_opt)
         scal_rms = -1.0
         call cuhci_set_parm(obj%cuhci(nit)%obj, 'scal_rms', scal_rms)

      end do

      if(ntrtemp .gt. obj%numtr) then
         obj%numtr = ntrtemp
         call pc_put_global('numtr', obj%numtr)
      end if


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


! --> Insert code to initialize variables for execution or deallocate arrays
! --> which will be reallocated below.

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

! --> Insert code to allocate needed permanent memory.

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

! --> Insert code to initialize anything needed for actual execution of process.


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine avoans_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


! *** Trap for variable CWIN_CDP ***

      subroutine avoans_cwin_cdp(keyword)
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
      end subroutine avoans_cwin_cdp

! *** Trap for variable CWIN_TIM ***

      subroutine avoans_cwin_tim(keyword)
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
      end subroutine avoans_cwin_tim

! *** Trap for variable CWIN_VSH ***

      subroutine avoans_cwin_vsh(keyword)
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
      end subroutine avoans_cwin_vsh

! *** Trap for variable DBUG_OPT ***

      subroutine avoans_dbug_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avoans_dbug_opt

! *** Trap for variable ETIM_WIN ***

      subroutine avoans_etim_win(keyword)
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
      end subroutine avoans_etim_win

! *** Trap for variable NCDP_MAX ***

      subroutine avoans_ncdp_max(keyword)
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
      end subroutine avoans_ncdp_max

! *** Trap for variable NORM_POW ***

      subroutine avoans_norm_pow(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%norm_pow)
      if(object%norm_pow .lt. 0.0 .or. object%norm_pow .gt. 2.0) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0.0 and 2.0.&
     & Restoring to default of 1.0.'
         call pc_error(msg)
         object%norm_pow = 1.0
         call pc_put(keyword, object%norm_pow)
      end if

      return
      end subroutine avoans_norm_pow

! *** Trap for variable PASS_OPT ***

      subroutine avoans_pass_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avoans_pass_opt

! *** Trap for variable STIM_WIN ***

      subroutine avoans_stim_win(keyword)
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
      end subroutine avoans_stim_win


! *** Screen trap for  GENERALPARAMETERS ***

      subroutine avoans_generalparameters(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avoans_generalparameters

! *** End Trap ***

      subroutine avoans_end
      implicit none

! --> Insert code to validate data input
      return
      end subroutine avoans_end



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine avoans (obj,ntr,hd,tr)
      type(avoans_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

! --> Insert declarations of local variables.
      integer :: i, j, k, m, n
      integer :: ierr, jerr
      integer :: promode, iteration
      integer :: trc_type   
      integer :: nit, cnt     , ntr1 
      integer :: npiped, isendens
      integer :: nio, max_nio, subndx
      integer :: nflushed   
      integer :: ispurge, maxpurge
      integer :: hdrlag, hdrsub
      integer :: ieog_flag, ieoj_flag

      integer :: gath_type, stackword
      integer :: max_gather   
      integer :: ntr_gather_in
      integer :: ntr_gather_out
      integer :: iflv, illv

      integer :: multigather, multiindex, multiiter, multicount
      integer :: acnt, bcnt, abcnt
      integer :: ghndx, gtndx
      integer :: mhndx, mtndx
      integer :: andx(200)
      integer :: bndx(200)
      double precision ::       ocdp 
      double precision :: cdp, cdp_out
      double precision ::         offset_out 
      double precision :: avo_angle, dom_freq, user
  
! --> Insert code for processing logic.
      ierr = 0
      jerr = 0

      if (.not. obj%cuhci(1)%obj%initialized) then

         allocate(obj%hd_x(ppavo_nhdrpz+obj%nwih, obj%numtr))
         allocate(obj%tr_x(obj%ndpt, obj%numtr))

         allocate(obj%hd_g(ppavo_nhdrpz+obj%nwih, obj%numtr))
         allocate(obj%tr_g(obj%ndpt, obj%numtr))

         allocate(obj%hd_m(ppavo_nhdrpz+obj%nwih, obj%numtr))
         allocate(obj%tr_m(obj%ndpt, obj%numtr))

         allocate(obj%hd_t(ppavo_nhdrpz+obj%nwih))
         allocate(obj%tr_t(obj%ndpt))

         do nit = 1, obj%init_iterations
            obj%cuhci(nit)%obj%ipn      = obj%ipn
            obj%cuhci(nit)%obj%numtr    = obj%numtr
            obj%cuhci(nit)%obj%gathered = obj%gathered
            obj%cuhci(nit)%obj%nwih     = obj%nwih
            obj%cuhci(nit)%obj%ndpt     = obj%ndpt
            obj%cuhci(nit)%obj%tstrt    = obj%tstrt
            obj%cuhci(nit)%obj%dt       = obj%dt
            obj%cuhci(nit)%obj%grid     = obj%grid

            obj%cuhci(nit)%obj%sampratz = obj%dt * 1000
            if(obj%survey_units .eq. 'FEET') then
               obj%cuhci(nit)%obj%iunitsz  = ppavo_englishpz
            else
               obj%cuhci(nit)%obj%iunitsz  = ppavo_metricpz
            end if
            obj%cuhci(nit)%obj%imultcz  = 0
            obj%cuhci(nit)%obj%icdpasnz = 1
            obj%cuhci(nit)%obj%igeoasnz = 1
            obj%cuhci(nit)%obj%ipsortz  = ppavo_cdppz
            obj%cuhci(nit)%obj%idtypez  = ppavo_normalpz
            obj%cuhci(nit)%obj%maxdtrz  = obj%numtr
            obj%cuhci(nit)%obj%numsmpz  = obj%ndpt
            obj%cuhci(nit)%obj%nthz     = ppavo_nhdrpz + obj%nwih
            obj%cuhci(nit)%obj%cleanupz = .false.
            call cuhci_init (obj%cuhci(nit)%obj, nio)
            if(nio .eq. FATAL_ERROR) then
               ntr = FATAL_ERROR
               return
            end if

         end do
      end if

      if(ntr .eq. NO_MORE_TRACES) obj%end_of_data = .true.

      ntr_gather_in = ntr
      ntr_gather_out = 0

      ntr1 = 1
      maxpurge = obj%init_iterations
      acnt = 0
      bcnt = 0
      abcnt = 0

      multigather = 0
      multiiter = 0
      nio = ntr_gather_in

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
      abcnt = (acnt + bcnt) / 2

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

      do nit = 1, obj%init_iterations
         obj%cuhci(nit)%obj%maxdtrz = ntr + (8 * abcnt) + 1
      end do

!     Set end-of-job flag to zero in header.
      do nit = 1, obj%numtr
         obj%hd_g(ppavo_end_jobz, nit) = 0
         obj%hd_g(ppavo_lagz, nit) = 0
         obj%hd_g(ppavo_subz, nit) = 0
      end do

!     Get inline/crossline indices (3d data only).
!     Assume iline is constant (2d data only).
      obj%ilineno = hd(ppavo_iline_noz, 1)
      obj%xlineno = hd(ppavo_xline_noz, 1)

!     Get CDP number for the gather (1st trace only).
      cdp = hd(ppavo_cdpz, 1)
      obj%ocdpX = hd(ppavo_cdpz, 1)

      max_nio = ntr_gather_in
      max_gather = abcnt
      acnt = 0
      bcnt = 0

60    ieoj_flag = 0
      promode = 1
      if(obj%end_of_data) then
         hd(ppavo_trc_typez, 1) = 43
         hd(ppavo_trc_typez, 2) = 44
      end if

      do nit = 1, nio
         subndx = nit

         trc_type  = hd(ppavo_trc_typez,  nit)
         gath_type = hd(ppavo_gath_typez, nit)
         stackword = hd(ppavo_stackwordz, nit)
         avo_angle = hd(ppavo_avo_anglez, nit)
         dom_freq  = hd(ppavo_dom_freqz,  nit)
         user      = hd(ppavo_userz,      nit)

!        Copy input traces to gather buffers(_g).
         do j = 1, obj%ndpt
            obj%tr_g(j, nit) = tr(j, nit)
         end do
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

         if(stackword .le. 0) then
            trc_type = ppavo_deadpz
         end if
         obj%hd_g(ppavo_trc_typez,  nit) = trc_type

         obj%hd_g(ppavo_purgez,     nit) = obj%purged
         obj%hd_g(ppavo_end_jobz,   nit) = 0
         obj%hd_g(ppavo_end_ensz,   nit) = ppavo_nlastpz
         if(nit .eq. nio) then
            obj%hd_g(ppavo_end_ensz, nit) = ppavo_lasttrpz
         end if
         obj%hd_g(ppavo_seq_noz,    nit) = nit
         obj%hd_g(ppavo_cdpz,       nit) = obj%fcdp
         obj%hd_g(ppavo_userz,      nit) = cdp
         obj%hd_g(ppavo_iline_noz,  nit) = obj%ilineno
         obj%hd_g(ppavo_xline_noz,  nit) = obj%xlineno

         if(trc_type .eq. 44) obj%fcdp = obj%fcdp + 1

         call ppavo_trlive(obj%tr_g(1:,nit), obj%ndpt, iflv, illv)
         obj%hd_g(ppavo_live_sz,    nit) = (iflv-1) * obj%dt * 1000
         obj%hd_g(ppavo_live_ez,    nit) = (illv-1) * obj%dt * 1000
         obj%hd_g(ppavo_full_sz,    nit) = obj%tstrt
         obj%hd_g(ppavo_full_ez,    nit) = obj%tstrt + ((obj%ndpt-1) * obj%dt)
         obj%hd_g(ppavo_full_sz,    nit) = obj%hd_g(ppavo_full_sz, nit) * 1000
         obj%hd_g(ppavo_full_ez,    nit) = obj%hd_g(ppavo_full_ez, nit) * 1000
!         obj%hd_g(ppavo_live_sz,    nit) = obj%hd_g(ppavo_full_sz, nit)
!         obj%hd_g(ppavo_live_ez,    nit) = obj%hd_g(ppavo_full_ez, nit)
         obj%hd_g(ppavo_trc_foldz,  nit) = 1.0
         obj%hd_g(ppavo_amp_normz,  nit) = 1.0
         obj%hd_g(ppavo_lagz,       nit) = obj%lagndx
         obj%hd_g(ppavo_subz,       nit) = subndx

      end do

      iteration = 1
      nit = 1
      if(obj%purged .gt. 0) then
         nit = obj%purged
         iteration = obj%purged
      end if

62    if(nit .le. obj%init_iterations) then

         promode = 1
         npiped = 1
         nflushed = 0
         obj%cuhci(nit)%obj%ipsortz = ppavo_cdppz
         obj%cuhci(nit)%obj%idtypez = ppavo_stackedpz

64       if(npiped .le. nio) then

!           Copy gather buffers(_g) to trace buffers(_t).
            do j = 1, (obj%nwih+ppavo_nhdrpz)
               obj%hd_t(j) = obj%hd_g(j, npiped)
            end do
            do j = 1, obj%ndpt
               obj%tr_t(j) = obj%tr_g(j, npiped)
            end do

!           If flushmode: send a dummy trace.
            if(promode .eq. 3) then
               obj%hd_t(ppavo_trc_typez) = ppavo_dummypz
            end if

!           Set the end-of-job flag
            obj%hd_t(ppavo_end_jobz) = 0
            if(obj%purged .gt. 0 .and. obj%purged .eq. nit) then
               obj%hd_t(ppavo_end_jobz) = 1
               obj%hd_t(ppavo_trc_typez) = ppavo_dummypz
            end if

!           Call main C_UHCI work routine.
            call cuhci_work(obj%cuhci(nit)%obj, ntr1, &
     &         obj%hd_t, obj%tr_t, promode)
            if(ntr1 .eq. FATAL_ERROR) then
               ntr = FATAL_ERROR
               return
            end if

!           If quitmode: skip rest of process (need to cleanup).
            if(promode .eq. 0) then
               npiped = nio + 1
            end if

!           If fillmode or pipemode: increment input counter.
            if(promode .eq. 1 .or. promode .eq. 2) then
               npiped = npiped + 1
            end if

!           If pipemode or flushmode: increment output counter.
            if(promode .eq. 2 .or. promode .eq. 3) then

               nflushed = nflushed + 1

!              Copy trace buffers(_t) to gather buffers(_x).
               do j = 1, (obj%nwih+ppavo_nhdrpz)
                  obj%hd_x(j, nflushed) = obj%hd_t(j)
               end do
               do j = 1, obj%ndpt
                  obj%tr_x(j, nflushed) = obj%tr_t(j)
               end do
               
               isendens = obj%hd_t(ppavo_end_ensz)
            end if

            goto 64
         end if

!        Copy gather buffers(_x) to gather buffers(_g).
         do k = 1, nflushed
            do j = 1, (obj%nwih+ppavo_nhdrpz)
               obj%hd_g(j, k) = obj%hd_x(j, k)
            end do
            do j = 1, obj%ndpt
               obj%tr_g(j, k) = obj%tr_x(j, k)
            end do
         end do

         nio = nflushed
         n = 1
65       if(n .le. nflushed) then
            ieog_flag = obj%hd_g(ppavo_end_ensz,   n)
            trc_type  = obj%hd_g(ppavo_trc_typez,  n)
            gath_type = obj%hd_g(ppavo_gath_typez, n)
            stackword = obj%hd_g(ppavo_stackwordz, n)
            avo_angle = obj%hd_g(ppavo_avo_anglez, n)
            dom_freq  = obj%hd_g(ppavo_dom_freqz,  n)
            user      = obj%hd_g(ppavo_userz,      n)
            ocdp      = obj%hd_g(ppavo_cdpz,       n)
            if(obj%end_of_data .and. nit .eq. obj%init_iterations .and.&
     &         ocdp .eq. obj%ocdpX) then
               ieoj_flag = 1
            end if
            ispurge = obj%hd_g(ppavo_purgez, n)
            
            if(ispurge .ne. 0) then
               goto 69
            end if
            
            if(nit .eq. obj%init_iterations) then
               ntr_gather_out = ntr_gather_out + 1
               cnt = ntr_gather_out

               cdp_out = obj%hd_g(ppavo_userz, n)
               offset_out = 0.0
               if(trc_type .eq. 1) then
                  offset_out = obj%hd_g(ppavo_offsetz, n)
               end if
               hdrlag = obj%hd_g(ppavo_lagz, n)
               hdrsub = obj%hd_g(ppavo_subz, n)
               if(hdrsub .lt. 1 .or. hdrsub .gt. ntr_gather_in) then
                  hdrsub = 1
               end if

!              Copy trace sample values
               do j = 1, obj%ndpt
                  tr(j, ntr_gather_out) = obj%tr_g(j, n)
               end do

!              Copy trace header values.
               do j = 1, obj%nwih
                  hd(j, ntr_gather_out) = obj%hd_g(j+ppavo_nhdrpz, n)
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

66          n = n + 1
            goto 65
         end if

!        If multiple gathers have been flushed, store excess
!        and then continue.
         do n = 1, nflushed
            if(obj%hd_g(ppavo_end_ensz, n) .ne. 0 .and. &
     &         obj%hd_g(ppavo_trc_typez, n) .eq. 1 .and. &
     &         n .lt. nflushed) then
               do m = (n+1), nflushed
                  gtndx = m
                  ghndx = m
                  mtndx = m-n
                  mhndx = m-n
                  if((obj%hd_g(ppavo_end_ensz,ghndx) .ne. 0 .and.&
     &                obj%hd_g(ppavo_trc_typez,ghndx) .eq. 1) .or.&
     &                m .eq. nflushed) then
                     multigather = multigather + 1
                  end if

!                 Copy gather buffers(_g) to multigather buffers(_m).
                  do j = 1, obj%ndpt
                     obj%tr_m(j,mtndx) = obj%tr_g(j,gtndx)
                  end do
                  do j = 1, (obj%nwih+ppavo_nhdrpz)
                     obj%hd_m(j,mhndx) = obj%hd_g(j,ghndx)
                  end do
               end do
               multiiter = iteration + 1
               multicount = nflushed - n
               multiindex = 1
               nio = n
               goto 67
            end if
         end do
67       iteration = iteration + 1
         nit = nit + 1

         goto 62
      end if

!     If excess gathers have been stored, load next
!     and then continue.
      if(multigather .ne. 0) then
         do m = 1, multicount
            gtndx = m
            ghndx = m
            mtndx = multiindex
            mhndx = multiindex
            multiindex = multiindex + 1
!           Copy multigather buffers(_g) to gather buffers(_m).
            do j = 1, obj%ndpt
               obj%tr_g(j,gtndx) = obj%tr_m(j,mtndx)
            end do
            do j = 1, (obj%nwih + ppavo_nhdrpz)
               obj%hd_g(j,ghndx) = obj%hd_m(j,mhndx)
            end do
            if((obj%hd_g(ppavo_end_ensz,ghndx) .ne. 0 .and.&
     &          obj%hd_g(ppavo_trc_typez,ghndx) .eq. 1) .or.&
     &          m .eq. multicount) then
               nio = m
               goto 68
            end if
         end do
68       multigather = multigather - 1
         multicount = multicount - nio
         nit = multiiter
         iteration = nit
         if(nit .le. obj%init_iterations) then
            goto 62
         end if
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
         nit = obj%purged
         if(obj%cwin_cdp .gt. 1) then
            goto 60
         end if
      end if

99999 continue
      ntr = ntr_gather_out
      if(ntr .eq. 0 .and. .not. obj%end_of_data)  ntr = NEED_TRACES
      if(ntr .eq. 0 .and. obj%end_of_data) ntr = NO_MORE_TRACES

      end subroutine avoans


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine avoans_wrapup (obj)
      type(avoans_struct),intent(inout) :: obj       ! arguments
      integer :: ntr, nit
      integer :: promode

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

! --> Insert any required wrapup code here, including wrapups of
! --> internally-called processes.

      do nit = 1, obj%init_iterations
         if(obj%cuhci(nit)%obj%initialized) then
            obj%cuhci(nit)%obj%cleanupz = .true.
            call cuhci_work(obj%cuhci(nit)%obj, ntr, &
     &         obj%hd_t, obj%tr_t, promode)
         end if
      end do
      if(associated(obj%hd_x)) deallocate(obj%hd_x)
      if(associated(obj%tr_x)) deallocate(obj%tr_x)
      if(associated(obj%hd_g)) deallocate(obj%hd_g)
      if(associated(obj%tr_g)) deallocate(obj%tr_g)
      if(associated(obj%hd_m)) deallocate(obj%hd_m)
      if(associated(obj%tr_m)) deallocate(obj%tr_m)
      if(associated(obj%hd_t)) deallocate(obj%hd_t)
      if(associated(obj%tr_t)) deallocate(obj%tr_t)

      end subroutine avoans_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module avoans_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

