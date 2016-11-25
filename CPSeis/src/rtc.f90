!<CPS_v1 type="PROCESS"/>
!!------------------------------- rtc.f90 ---------------------------------!!
!!------------------------------- rtc.f90 ---------------------------------!!
!!------------------------------- rtc.f90 ---------------------------------!!

 
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
! Name       : RTC         (Residual Time Correction)
! Category   : statics
! Written    : 1989-02-21   by: Tom Stoeckley and Greg Lazear
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Non-surface consistent trim statics process for 2D.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! RTC shifts each trace in a CMP according to its correlation with a base
! trace.  If only one correlation window is specified, the shift is a static
! shift.  If two or more windows are specified per trace, each window is
! correlated with the base trace separately and the shift is a dynamic
! (time-varying) shift.  RTC is not surface consistent.
!
!
! 2D and 3D Processing
!
! RTC can be used with 2D data or certain swaths from a 3D survey.  For 3D
! processing, RTC3D is recommended.
!
!
! Minimum Correlation Coefficient
!
! Shifts are picked for each window whose correlation exceeds CC_MIN.  If the
! correlation for a window fails this test, that window will be shifted by the
! average static found from all windows on that trace not failing this test.
! If all of the windows on a trace fail the test, the trace is not shifted.  If
! only one window passes the test (or only one window is specified), the trace
! is statically shifted.  Otherwise, the trace is dynamically shifted.
!
!
! Storing Correlation Coefficients
!
! If HDR_CC > 0, the maximum correlation coefficient for a trace is stored in
! this header word.  This header word can then be used later for weighted
! stacks (weighting by correlation coefficient), or for killing traces (using
! process SELECT) with small correlation coefficients.
!
!
! An Historical Note
!
! RTC is related historically to the Cyber processes MTC, RSIC, RSDP and RTC.
! RSIC and RSDP replaced stacked traces with base traces, while MTC did not.
! MTC, RSIC and RSDP employed a static shift, while Cyber RTC (with two or more
! windows) used a dynamic shift.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
! RTC is normally used as the last step in a statics solution to remove any
! small non-surface consistent residual static that may be present.
!
! Because RTC is not surface consistent, it can be somewhat unstable and is
! normally used with small values of MAX_STATIC (10 ms or less) to avoid larger
! shifts that might have resulted from wild correlations.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! Process is a multiple-trace process.
!
! RTC requires input traces to be sorted in CMP order, with header words 7 and
! 8 designating CMP bins and header word 3 a different whole number for each
! CMP.
!
! If BINS_X=1 and BINS_Y=1 then CMPs can be in any order.
!
! If BINS_X>1 and BINS_Y=1 then CMPs are assumed to be sorted sequentially
!       in the CMP X grid direction, and base traces are formed from BINS_X
!       stacked traces at a time.
!
! If BINS_X=1 and BINS_Y>1 then CMPs are assumed to be sorted sequentially
!       in the CMP Y grid direction, and base traces are formed from BINS_Y
!       stacked traces at a time.
!
! If BINS_X>1 and BINS_Y>1 then a swath of CMP lines in the X grid or Y grid
!       direction is assumed to be input, with CMPs sorted in across-the-swath
!       order.  A base trace is formed from BINS_X * BINS_Y stacked traces.
!       The width of the swath should be either BINS_X or BINS_Y.
!
! In all cases, more than one line or swath (in the X or Y direction, depending
! on the above choices) can be input, each one following the preceding one.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! This process alters input traces by shifting them.
! This process outputs the same traces as it receives.
! This process outputs one trace at a time.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name       Description                             Action taken
! ----       -----------                             ------------
! NWIH       number of words in trace header         used but not changed
! NDPT       number of sample values in trace        used but not changed
! TSTRT      starting time on trace                  used but not changed
! DT         trace sample interval                   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Hwd#    Description                            Action taken
! ----    -----------                            ------------
! 2       Head mute index                        used but not changed
! 64      Tail mute index                        used but not changed
! 3       Current CMP                            used but not changed
! 7,8     CMP grid coordinates                   used but not changed
! HDR_CC  header for storing correlations
! HDR_X   header designating CMP X grid coord (BASETRACE primitive parameter)
! HDR_Y   header designating CMP Y grid coord (BASETRACE primitive parameter)
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!028. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!027. 2006-01-10  B. Menger  Removed Unused Variables.
! 26. 2004-01-07  Stoeckley  Fix bug for METHOD=SDIP where traces were treated
!                             as dead since HDS(25) was not set.
! 25. 2003-06-02  Stoeckley  Fix buffer length sctr(5000) to sctr(obj%ndpt).
! 24. 2001-10-18  Stoeckley  Fix print format with field not wide enough.
! 23. 2001-08-24  Stoeckley  Add file selection box and file status message.
! 22. 2001-02-15  Stoeckley  Change wrapup flag.
! 21. 2000-10-20  Stoeckley  Add missing required documentation sections.
! 20. 2000-09-27  Stoeckley  Add workaround code near calls to BASETRACE_BUILD
!                             and MTH_AMPLITUDE_NORMALIZE to circumvent
!                             problems with the Portland Group compiler.
! 19. 2000-07-20  Stoeckley  Change INL and CRL parameter names to X and Y;
!                             change TEMPFILE back to TEMPTFILE since the
!                             improved throughput turned out to be an NFS issue.
! 18. 2000-06-21  Stoeckley  Change SHIFT_MAX parameter to MAX_STATIC to be
!                             consistent with other statics processes; fix
!                             subtle bug which sometimes caused RTC to quit
!                             prematurely; replace TEMPTFILE with TEMPFILE to
!                             improve throughput by a factor of five.
! 17. 2000-06-05  Stoeckley  More changes to be consistent with modifications
!                             to the BASETRACE primitive.
! 16. 2000-05-18  Stoeckley  Change to be consistent with modifications to
!                             the BASETRACE primitive.
! 15. 2000-05-01  Stoeckley  Converted from old system.
! 14. 1998-12-14  K.Goodger  Begin using the fortran90 compiler.
! 13. 1996-11-11  K.Goodger  If detect a zero record number for DTREAD,
!                             set N to zero and return.
! 12. 1994-04-04  Troutt     Change header array for STRINI of base trace
!                             file to a dummy array (HDUM).  It was HDS,
!                             which was being used to control flow through
!                             the process.
! 11. 1993-02-11  Troutt     Increase limit for number of windows from 10 to
!                             25 (new parameter NWTMAX).
! 10. 1992-08-11  Troutt     Allow UNITS=SHOT. This option uses hwds 37 and
!                             38 to be used for varying the data windows.
!  9. 1992-02-21  Troutt     Added handling of tail mute header word 64. Add
!                             call to MUTEHW.
!  8. 1991-07-30  Troutt     Fix call to FILTERG so that BASE array is not
!                             overrun.
!  7. 1990-12-07  Peterson   Correct error return argument on calls to
!                             CALL CLOSFIL (ARG1,ARG2,*error return address).
!  6. 1990-10-29  Stoeckley  Fix bug which occurred when first CMP bin is
!                             zero.  Also fix STATFILE format to allow
!                             larger offsets.
!  5. 1989-06-12  Stoeckley  Fix graded windows bug with UNITS=DIST.
!  4. 1989-05-30  Stoeckley  Add base trace header word parameters, and
!                             correct problems with 2-dimensional forms of
!                             TRMIX and ICDPREC.
!  3. 1989-05-02  Stoeckley  Added semblance dip option for base trace,
!                             plus some other parameter changes.
!  2. 1989-04-17  Stoeckley  Added parameters CCHDR and FILE, and removed
!                             trace killing.
!  1. 1989-02-21  Stoeckley  Original code built from Greg Lazear's MTC.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations, but see comments regarding the Portland Group
! compiler near calls to BASETRACE_BUILD and MTH_AMPLITUDE_NORMALIZE.
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
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        true      whether this process needs two trace/header arrays.
! NSCRATCH        >0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
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
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES              
!
! In this module, the dependent parameter IFLOW is used to keep track of
! whether traces were output or a request was made for more traces.  The
! input value of NTR is not tested to see whether it is set to NEED_TRACES.
! The IFLOW parameter could be removed if the input value of NTR is used
! instead.  However, while converting from the old system, the decision was
! made to keep the IFLOW parameter to avoid making any logic changes which
! could introduce bugs.  (Note that in the old system, the input value of
! NTR was undefined when a request is made for more traces, making the IFLOW
! parameter necessary.)
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS RTC Process/NC=80>
!                     Residual Time Correction Process
!            Non-surface consistent trim statics process for 2D
!
! MAX_STATIC=`FFFFFFFFFFF               NUM_WIN=`I
!
! FOLD_MAX=`IIIIIIII   REPLACE=`CC      HDR_CC=`IIIIII   CC_MIN=`FFFFFFFFFFF
!
! Select PATH_STAT[PATH_STAT]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [path_stat_info]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!<PARMS PATH_STAT[/ML=128/XST]>
!
!<NS Base Trace Specifications/NC=80>
!<include basetrace.f90>
!
!<NS Multiple Trace Window Specification/NC=80>
!<include multwin.f90>
!
!<PARMS Multiple Trace Window Specification [screen2]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="SELECT_PATH_STAT">
!<Tip> Choose PATH_STAT using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATH_STAT_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATH_STAT. </Tip>
!</Help>
!
!
!<Help KEYWORD="FOLD_MAX">
!<Tip> Maximum fold for any input CMP gather. </Tip>
! Default = 200
! Allowed = int > 0
! If a CMP is input with greater fold than FOLD_MAX, RTC will delete the excess
! traces.
!</Help>
!
!
!<Help KEYWORD="HDR_CC">
!<Tip> Header word used to store the trace's correlation coefficient. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! HDR_CC is the header word used to store the trace's maximum correlation
! coefficient.
! If HDR_CC = 0, then do not store the correlation coefficient.
!</Help>
!
!
!<Help KEYWORD="PATH_STAT">
!<Tip> Write an RTC static file to this pathname. </Tip>
! Default = -
! Allowed = char
! This can be a very large file.  Its use is not recommended since RTC can be
! rerun at little cost.
!
! If PATH_STAT = NONE, then do not write the RTC static file.
!</Help>
!
!
!<Help KEYWORD="REPLACE">
!<Tip> Replace the stacked trace with the base trace? </Tip>
! Default = NO
! Allowed = YES/NO
! Whether to replace the stacked trace with the corresponding base trace for
! forming subsequent base traces?
! (This has no effect if the base traces are being read from a file.)
!</Help>
!
!
!<Help KEYWORD="MAX_STATIC">
!<Tip> Maximum static shift, in ms, for individual traces within a CMP. </Tip>
! Default = 10.0
! Allowed = real > 0
! MAX_STATIC cannot be less than MSCTFB.
!</Help>
!
!
!<Help KEYWORD="NUM_WIN">
!<Tip> Number of windows on a trace. </Tip>
! Default = 1
! Allowed = integer > 0
! If there is only one window on a trace, the trace will be statically shifted.
! If there are two or more windows, the trace will be dynamically shifted
! such that the center of each window will be shifted based on the correlation
! of that window with the base trace.
!</Help>
!
!
!<Help KEYWORD="CC_MIN">
!<Tip> Minimum correlation coefficient allowed for a trace window. </Tip>
! Default = 0.0
! Allowed = 1.0 > real >= 0.0
! If the correlation for a window does not exceed CC_MIN, that window will be
! shifted by the average static found from all windows in the trace not failing
! this test.  More details in General Description.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module rtc_module
      use pc_module
      use named_constants_module
      use basetrace_module
      use multwin_module
      use pathcheck_module
      use pathchoose_module
      use mem_module
      use mth_module
      use temptfile_module
      use rtcstatio_module
      use cordip_module
      use mutehw_module
      implicit none
      private
      public :: rtc_create
      public :: rtc_initialize
      public :: rtc_update
      public :: rtc_delete
      public :: rtc
      public :: rtc_wrapup


      character(len=100),public,save :: rtc_IDENT = &
       '$Id: rtc.f90,v 1.28 2006/10/17 13:45:46 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: rtc_struct              
 
        private
        logical                        :: skip_wrapup    ! wrapup flag
        integer                        :: nwih,ndpt      ! globals  
        real                           :: tstrt,dt       ! globals  

        integer                        :: fold_max       ! process parameters
        integer                        :: hdr_cc         ! process parameters
        character(len=FILENAME_LENGTH) :: path_stat      ! process parameters
        logical                        :: replace        ! process parameters
        real                           :: max_static     ! process parameters
        real                           :: cc_min         ! process parameters
        integer                        :: num_win        ! process parameters

        type(basetrace_struct),pointer :: basetrace      ! dependent
        type(multwin_struct  ),pointer :: multwin        ! dependent
        type(temptfile_struct),pointer :: temptfile      ! dependent
        type(rtcstatio_struct),pointer :: rtcstatio      ! dependent

        integer                        :: bins_x       ! from basetrace
        integer                        :: bins_y       ! from basetrace

        integer :: nlefti, nlefto, nleftb, ntout, iflow, itrp, ipcdp
        integer :: nstkb, istkb, nccdp, ncdp, maxtr, ncdph, idx
        integer :: nkill, nstat, ndyn

        real                   ,pointer :: avcc  (:)      ! dependent
        integer                ,pointer :: ntotal(:)      ! dependent
        integer                ,pointer :: npass (:)      ! dependent
        real                   ,pointer :: avst  (:)      ! dependent
        real                   ,pointer :: stlo  (:)      ! dependent
        real                   ,pointer :: stup  (:)      ! dependent

        double precision       ,pointer :: hdmix  (:,:)   ! dependent
        real                   ,pointer :: trmix  (:,:)   ! dependent
        real                   ,pointer :: twin   (:,:)   ! dependent
        real                   ,pointer :: sttr   (:)     ! dependent
        integer                ,pointer :: nrod   (:)     ! dependent
        real                   ,pointer :: trbase (:)     ! dependent
        real                   ,pointer :: hds    (:)     ! dependent
        real                   ,pointer :: tab    (:)     ! dependent
        integer                ,pointer :: kfold  (:)     ! dependent
        integer                ,pointer :: icdprec(:,:)   ! dependent
        type(pathchoose_struct),pointer :: dialog

      end type rtc_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(rtc_struct),pointer,save :: object      ! needed for traps.


      integer,parameter     :: replace_noptions = 2
      logical         ,save :: replace_options(replace_noptions)

      data replace_options/.true.,.false./


      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine rtc_create (obj)
      implicit none
      type(rtc_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%temptfile)
      nullify (obj%rtcstatio)

      nullify (obj%avcc  )
      nullify (obj%ntotal)
      nullify (obj%npass )
      nullify (obj%avst  )
      nullify (obj%stlo  )
      nullify (obj%stup  )

      nullify (obj%hdmix  )
      nullify (obj%trmix  )
      nullify (obj%twin   )
      nullify (obj%sttr   )
      nullify (obj%nrod   )
      nullify (obj%trbase )
      nullify (obj%hds    )
      nullify (obj%tab    )
      nullify (obj%kfold  )
      nullify (obj%icdprec)
      nullify (obj%basetrace) ! jpa
      nullify (obj%multwin) ! jpa
      nullify (obj%dialog) ! jpa

      call basetrace_create  (obj%basetrace, 'rtc')
      call multwin_create    (obj%multwin)
      call pathchoose_create (obj%dialog, 'path_stat', 'rtc')
      call rtc_initialize    (obj)
      return
      end subroutine rtc_create


!!------------------------- private free -----------------------------------!!
!!------------------------- private free -----------------------------------!!
!!------------------------- private free -----------------------------------!!


      subroutine rtc_private_free (obj)
      implicit none
      type(rtc_struct),intent(inout) :: obj       ! arguments

      call mem_free (obj%avcc  )
      call mem_free (obj%ntotal)
      call mem_free (obj%npass )
      call mem_free (obj%avst  )
      call mem_free (obj%stlo  )
      call mem_free (obj%stup  )

      call mem_free (obj%hdmix  )
      call mem_free (obj%trmix  )
      call mem_free (obj%twin   )
      call mem_free (obj%sttr   )
      call mem_free (obj%nrod   )
      call mem_free (obj%trbase )
      call mem_free (obj%hds    )
      call mem_free (obj%tab    )
      call mem_free (obj%kfold  )
      call mem_free (obj%icdprec)
      return
      end subroutine rtc_private_free


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine rtc_delete (obj)
      implicit none
      type(rtc_struct),pointer :: obj       ! arguments

      call rtc_wrapup (obj)

      call rtc_private_free  (obj)
      call basetrace_delete  (obj%basetrace)
      call multwin_delete    (obj%multwin  )
      call pathchoose_delete (obj%dialog)

      deallocate(obj)
      return
      end subroutine rtc_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine rtc_initialize (obj)
      implicit none
      type(rtc_struct),intent(inout) :: obj       ! arguments

      obj%fold_max    = 200                     ! was maxfold
      obj%hdr_cc      = 0.0                     ! was icchdr
      obj%path_stat   = PATHCHECK_EMPTY         ! was statfile
      obj%replace     = .false.                 ! was replace
      obj%max_static  = 10.0                    ! was mxsh
      obj%cc_min      = 0.0                     ! was mncc and rmcc
      obj%num_win     = 1                       ! was nwt

      call basetrace_initialize (obj%basetrace)
      call multwin_initialize   (obj%multwin, obj%num_win)
      call rtc_update           (obj)
      return
      end subroutine rtc_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine rtc_update (obj)
      implicit none
      type(rtc_struct),intent(inout),target :: obj              ! arguments
      integer                               :: err,i ! local
      integer                               :: nstore,nscratch  ! local
      logical                               :: error ! local
      character(len=FILENAME_LENGTH)        :: pathkeep         ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      pathkeep = obj%path_stat

      if (pathchoose_update(obj%dialog, obj%path_stat)) return

      call pc_get   ('fold_max'  , obj%fold_max    )
      call pc_get   ('hdr_cc'    , obj%hdr_cc      )
      call pc_get   ('path_stat' , obj%path_stat   )
      call pc_get   ('replace'   , obj%replace     )
      call pc_get   ('max_static', obj%max_static  )
      call pc_get   ('cc_min'    , obj%cc_min      )
      call pc_get   ('num_win'   , obj%num_win     )

      obj%nwih        = 0
      obj%ndpt        = 0
      obj%tstrt       = 0.0
      obj%dt          = 0.0

      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (obj%dt <= 0.0) then
           call pc_error ('RTC: global DT not set')
           obj%dt = 0.004
      end if

      if (obj%nwih < HDR_NOMINAL_SIZE) then
           call pc_error ('RTC: global NWIH not set')
           obj%nwih = HDR_NOMINAL_SIZE
      end if

      if (obj%ndpt <= 1) then
           call pc_error ('RTC: global NDPT not set')
           obj%ndpt = 2
      end if

      call mth_constrain (obj%fold_max  ,   1,     9999)
      call mth_constrain (obj%hdr_cc    ,   0, obj%nwih)
      call mth_constrain (obj%max_static, 1.0,    999.0)
      call mth_constrain (obj%cc_min    , 0.0,      1.0)
      call mth_constrain (obj%num_win   ,   1,       99)


      call pathcheck &
             ('path_stat', obj%path_stat, '.rtc', show=PATHCHECK_INFO_OUTPUT) 

      if (obj%path_stat /= pathkeep .and. &
          obj%path_stat /= PATHCHECK_EMPTY) then
           call pc_warning ('RTC: PATH_STAT may become a very large file.')
           call pc_warning (' Are you sure you want to create this file?')
           call pc_warning (' Instead of creating this file,')
           call pc_warning ('  you can re-run RTC with little cost.')
      end if

      call basetrace_update (obj%basetrace, obj%max_static, 1, error, &
                                 bins_x = obj%bins_x,                 &
                                 bins_y = obj%bins_y)

      call multwin_update   (obj%multwin, obj%num_win, 'screen2')

      obj%nlefti = 0
      obj%nlefto = 0
      obj%nleftb = 0
      obj%ntout  = 0
      obj%iflow  = 0
      obj%itrp   = 0
      obj%ipcdp  = -99999
      obj%nstkb  = 0
      obj%istkb  = 0
      obj%nccdp  = 0
      obj%ncdp   = obj%bins_x * obj%bins_y
      obj%maxtr  = obj%fold_max * obj%ncdp
      obj%ncdph  = obj%ncdp / 2 + 1

      nstore   = 3 * obj%ndpt + obj%nwih + 2 * obj%maxtr + 1            &
                  + (obj%nwih + obj%ndpt) * obj%ncdp + obj%fold_max + 1
      nscratch = 2 * obj%ndpt


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('replace', replace_options, replace_noptions)

      call pc_put_global ('numtr'   ,   1    )
      call pc_put_global ('gathered', .false.)

      call pc_put ('fold_max'  , obj%fold_max    )
      call pc_put ('hdr_cc'    , obj%hdr_cc      )
      call pc_put ('path_stat' , obj%path_stat   )
      call pc_put ('replace'   , obj%replace     )
      call pc_put ('max_static', obj%max_static  )
      call pc_put ('cc_min'    , obj%cc_min      )
      call pc_put ('num_win'   , obj%num_win     )

      call pc_put_control ('need_request', .true.)
      call pc_put_control ('need_label'  , .true.)
      call pc_put_control ('twosets'     , .true.) 
      call pc_put_control ('nscratch'    , nscratch)
      call pc_put_control ('nstore'      , nstore)
   

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (associated(obj%temptfile)) call temptfile_close (obj%temptfile)
      if (associated(obj%rtcstatio)) call rtcstatio_close (obj%rtcstatio)

      call rtc_private_free (obj)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

!----------allocate and initialize statistical summary variables.

      call mem_alloc (obj%avcc  , obj%num_win)
      call mem_alloc (obj%ntotal, obj%num_win)
      call mem_alloc (obj%npass , obj%num_win)
      call mem_alloc (obj%avst  , obj%num_win)
      call mem_alloc (obj%stlo  , obj%num_win)
      call mem_alloc (obj%stup  , obj%num_win)

      if (pc_do_not_process_traces()) return    ! in case mem_alloc failed.

      obj%avcc  (1:obj%num_win) = 0.0
      obj%ntotal(1:obj%num_win) = 0
      obj%npass (1:obj%num_win) = 0
      obj%avst  (1:obj%num_win) = 0.0
      obj%stlo  (1:obj%num_win) = 99999.0
      obj%stup  (1:obj%num_win) = -99999.0
      obj%nkill                 = 0
      obj%nstat                 = 0
      obj%ndyn                  = 0
      obj%idx                   = 0

!----------allocate and initialize permanant storage arrays.

      call mem_alloc (obj%hdmix  , obj%nwih, obj%ncdp)       ! not initialized.
      call mem_alloc (obj%trmix  , obj%ndpt, obj%ncdp)       ! not initialized.
      call mem_alloc (obj%twin   , 2, obj%num_win)           ! not initialized.
      call mem_alloc (obj%sttr   , obj%ndpt)
      call mem_alloc (obj%nrod   , obj%maxtr + 1)
      call mem_alloc (obj%trbase , obj%ndpt)                 ! not initialized.
      call mem_alloc (obj%hds    , obj%nwih)
      call mem_alloc (obj%tab    , obj%fold_max + 1)
      call mem_alloc (obj%kfold  , obj%ndpt)
      call mem_alloc (obj%icdprec, obj%fold_max, obj%ncdp)   ! not initialized.

      if (pc_do_not_process_traces()) return    ! in case mem_alloc failed.

      obj%hdmix   = 0.0    ! now decided to initialize this.
      obj%trmix   = 0.0    ! now decided to initialize this.
      obj%twin    = 0.0    ! now decided to initialize this.
      obj%trbase  = 0.0    ! now decided to initialize this.
      obj%icdprec = 0      ! now decided to initialize this.

      obj%sttr (1:obj%ndpt)      = 0.0
      obj%kfold(1:obj%ndpt)      = 0
      obj%hds  (1:obj%nwih)      = 0
      obj%nrod (1:obj%maxtr + 1) = 0
      obj%nrod (1)               = 1
      obj%nrod (2)               = 1

!----------build mute scaling table.

      do i = 2,obj%fold_max + 1
           obj%tab(i) = 1.0/sqrt(float(i-1))
      end do
      obj%tab(1) = 0.0

!----------open temporary trace file.

      call temptfile_open &
         (obj%temptfile,'rtc_temptfile',obj%nwih,obj%ndpt, pc_get_lun(), err)
      if (err /= TEMPTFILE_OK) then
           call pc_error ('RTC: FATAL ERROR IN TEMPTFILE')
           return
      end if

!----------open static file to write.

      if (obj%path_stat /= PATHCHECK_EMPTY) then
           call rtcstatio_open_write &
                        (obj%rtcstatio, obj%path_stat, pc_get_lun(), err)
           if (err /= RTCSTATIO_OK) then
                call pc_error ('RTC: FATAL ERROR IN RTCSTATIO')
                return
           end if
      end if

!----------begin the (only) iteration.

      call basetrace_begin_iteration (obj%basetrace, error)
      if (error) then
           call pc_error ('RTC: FATAL ERROR IN BASETRACE_BEGIN_ITERATION')
      end if


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine rtc_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine rtc (obj,ntr,hdi,tri,hdo,tro)
      implicit none
      type(rtc_struct),intent(inout) :: obj                    ! arguments
      integer         ,intent(inout) :: ntr                    ! arguments
      double precision,intent(in)    :: hdi(:,:)               ! arguments
      real            ,intent(in)    :: tri(:,:)               ! arguments
      double precision,intent(out)   :: hdo(:,:)               ! arguments
      real            ,intent(out)   :: tro(:,:)               ! arguments
      integer                        :: err,i,lst,nout         ! local
      integer                        :: irec,ifold,ngood       ! local
      real                           :: static,ccbest          ! local
      logical                        :: error                  ! local
      real                           :: ccwin(100)             ! local
      real                           :: stwin(100)             ! local
      real                           :: sctr (obj%ndpt)        ! local
      real                           :: trmix(obj%ndpt)        ! local

!----------check for cmp traces to correlate and output.

      if (obj%nlefto > 0) go to 100

!----------check for more bases to process.

      if (obj%nleftb > 0) go to 90

!----------check for termination flag.

      if(obj%nlefti == -1) then
         ntr = NO_MORE_TRACES
         call rtc_wrapup (obj)
         return
      end if

!----------check for traces available in input array.

   5  if (obj%nlefti == 0) then

!----------was routine entered from below - if so, return for more traces.

         if(obj%iflow == 2) then
            obj%iflow = 1
            ntr = NEED_TRACES
            return
         end if
         if(ntr == NO_MORE_TRACES) then
           obj%nleftb = obj%ncdph
           obj%nlefti = -1
           if(obj%nstkb > 0) then

!----------normalize stack trace and store in mix array.

              do i = 1,obj%ndpt
                   obj%sttr(i) = obj%sttr(i)*obj%tab(obj%kfold(i) + 1)
              end do

      !!!     call mth_amplitude_normalize &
      !!!                          (obj%sttr,obj%trmix(1:,obj%istkb),obj%ndpt)

      !!! Portland Group compiler in subdirectory linuxp:
      !!! Aborts using the above call to MTH_AMPLITUDE_NORMALIZE.
      !!! Works using the following call which uses automatic array TRMIX
      !!!   as temporary storage.
      !!! Other places where MTH_AMPLITUDE_NORMALIZE is called are fixed
      !!!   in a similar manner as here.
      !!! Portland Group compiler in subdirectory intelsol works OK here.

              call mth_amplitude_normalize &
                                   (obj%sttr,trmix,obj%ndpt)
              obj%trmix(1:obj%ndpt,obj%istkb) = trmix(1:obj%ndpt)

              obj%hdmix(1:obj%nwih,obj%istkb) = obj%hds(1:obj%nwih)
              obj%sttr (1:obj%ndpt)           = 0.0
              obj%kfold(1:obj%ndpt)           = 0
              obj%hds  (1:obj%nwih)           = 0.0
           end if
           go to 90
         end if
         obj%nlefti = ntr
         obj%itrp = 0
      end if

!----------is next trace in buffer a new bin.

      obj%itrp = obj%itrp + 1
      obj%nlefti = obj%nlefti - 1
      if(nint(hdi(3,obj%itrp)) /= obj%ipcdp) then
         obj%ipcdp = nint(hdi(3,obj%itrp))

!----------if yes then check for complete stack trace.

         if(obj%nstkb > 0) then

!----------normalize stack trace and store in mix array.

              do i = 1,obj%ndpt
                   obj%sttr(i) = obj%sttr(i)*obj%tab(obj%kfold(i) + 1)
              end do

              call mth_amplitude_normalize &
                                   (obj%sttr,trmix,obj%ndpt)
              obj%trmix(1:obj%ndpt,obj%istkb) = trmix(1:obj%ndpt)

      !!!  Two lines below replaced by three lines above.
      !!!  This makes the Portland Group compiler much happier.

      !!!     call mth_amplitude_normalize &
      !!!                          (obj%sttr,obj%trmix(1:,obj%istkb),obj%ndpt)

              obj%hdmix(1:obj%nwih,obj%istkb) = obj%hds(1:obj%nwih)
              obj%sttr (1:obj%ndpt)           = 0.0
              obj%kfold(1:obj%ndpt)           = 0
              obj%hds  (1:obj%nwih)           = 0.0
         end if
         obj%nstkb = obj%nstkb + 1
         obj%istkb = mod(obj%istkb,obj%ncdp) + 1

!----------check for full stack buffer.

         if(obj%nstkb > obj%ncdp) then

!----------reset pointers to current trace in buffer - go correlate traces.

            obj%nstkb = obj%nstkb - 1
            obj%nlefti = obj%nlefti + 1
            lst = obj%istkb - 1
            if(lst == 0) lst = obj%ncdp
            nout = lst - obj%nccdp - obj%ncdph + 1
            if(lst < obj%nccdp) nout = nout + obj%ncdp
            obj%nleftb = nout
            obj%itrp = obj%itrp - 1
            go to 90
         end if
      end if

! **************************************************************
!   sum current trace into stack trace and go for another trace
! **************************************************************

!----------check for fold exceeding fold_max and delete trace.

      if(obj%hds(5) >= obj%fold_max) go to 25

      do i = 1,obj%ndpt
           if (tri(i,obj%itrp) /= 0.) obj%kfold(i) = obj%kfold(i) + 1
           obj%sttr(i) = obj%sttr(i) + tri(i,obj%itrp)
      end do

!----------bump fold of stack counter.

      obj%hds( 5) = obj%hds(5) + 1.0
      obj%hds( 7) = hdi( 7,obj%itrp)
      obj%hds( 8) = hdi( 8,obj%itrp)
      obj%hds(17) = hdi(17,obj%itrp)
      obj%hds(18) = hdi(18,obj%itrp)
      obj%hds( 3) = hdi( 3,obj%itrp)
      obj%hds(37) = hdi(37,obj%itrp)
      obj%hds(38) = hdi(38,obj%itrp)
      obj%hds(25) = hdi(25,obj%itrp)

!----------output current trace to disk.

      call rtc_drm (0,obj%nrod,irec)
      ifold = nint(obj%hds(5))
      obj%icdprec(ifold,obj%istkb) = irec

      call temptfile_write &
                   (obj%temptfile,irec,hdi(1:,obj%itrp),tri(1:,obj%itrp),err)
      if (err /= TEMPTFILE_OK) then
           call pc_error ('RTC: FATAL ERROR IN TEMPTFILE')
           ntr = FATAL_ERROR
           call rtc_wrapup (obj)
           return
      end if

!----------return above to get next trace.

  25  obj%iflow = 2
      go to 5

! ******************************************************************
!   form base trace for bin obj%nccdp from stack traces
! *******************************************************************

  90  obj%nccdp = mod(obj%nccdp,obj%ncdp) + 1
      obj%nlefto = nint(obj%hdmix(5,obj%nccdp))

!----------get space varying window for current cmp into twin(2,num_win).

      call multwin_get_windows (obj%multwin, obj%hdmix(1:,obj%nccdp), obj%twin)

!----------build a new base trace for this cmp.

     obj%trbase = obj%trmix(1:obj%ndpt,obj%nccdp)

          !!! Portland Group compiler in subdirectory linuxp:
          !!! Aborts in the call to BASETRACE_BUILD below if the above
          !!!   line which presets TRBASE is absent.
          !!! Portland Group compiler in subdirectory intelsol might
          !!!   be OK but has not been tested.

      call basetrace_build (obj%basetrace, obj%hdmix, obj%trmix, &
                            obj%ncdp, obj%nccdp,                 &
                            obj%twin, obj%num_win,               &
                            obj%hdmix(1:,obj%nccdp),obj%trbase,error)
      if (error) then
           call pc_error ('RTC: error building base trace')
           ntr = FATAL_ERROR
           call rtc_wrapup (obj)
           return
      end if

!----------if replace=true then replace stack trace with normalized base.

      if (obj%replace .and. obj%ncdp > 1) then

           call mth_amplitude_normalize &
                               (obj%trbase,trmix,obj%ndpt)
           obj%trmix(1:obj%ndpt,obj%nccdp) = trmix(1:obj%ndpt)

       !!!  Two lines below replaced by three lines above.
       !!!  This makes the Portland Group compiler much happier.

       !!! call mth_amplitude_normalize &
       !!!                     (obj%trbase,obj%trmix(1:,obj%nccdp),obj%ndpt)
      end if

      obj%nleftb = obj%nleftb - 1

! *******************************************************************
!   input traces from disk and correlate with base trace
! *******************************************************************

      obj%idx = 0
  100 obj%idx = obj%idx + 1
      obj%nlefto = obj%nlefto - 1
      irec = obj%icdprec(obj%idx,obj%nccdp)
      if(irec == 0)then
        ntr = NO_MORE_TRACES
        call rtc_wrapup (obj)
        return
      endif
      obj%icdprec(obj%idx,obj%nccdp) = 0
      call rtc_drm (1,obj%nrod,irec)

!----------read trace from disk.

      call temptfile_read (obj%temptfile,irec,hdo(1:,1),sctr,err)
      if (err /= TEMPTFILE_OK) then
           call pc_error ('RTC: FATAL ERROR IN TEMPTFILE')
           ntr = FATAL_ERROR
           call rtc_wrapup (obj)
           return
      end if

!----------dynamically adjust trace to match base.

      call cordip_more (obj%cc_min,obj%max_static,obj%twin,obj%num_win, &
                        obj%trbase,sctr,tro(1:,1),                      &
                        obj%ndpt,obj%tstrt,obj%dt,                      &
                        static,ccbest,ngood,                            &
                        obj%avcc,obj%ntotal,obj%npass,obj%avst,         &
                        obj%stlo,obj%stup,ccwin,stwin)

      if (ngood == 0) obj%nkill = obj%nkill + 1
      if (ngood == 1) obj%nstat = obj%nstat + 1
      if (ngood >= 2) obj%ndyn  = obj%ndyn  + 1

!----------save static/dynamic trace shift to static file.

      if (obj%path_stat /= PATHCHECK_EMPTY) then
           call rtcstatio_write_record (obj%rtcstatio,hdo(1:,1),ccbest,   &
                                        static*obj%dt*1000.0,obj%num_win, &
                                        ccwin,stwin,err)
           if (err /= RTCSTATIO_OK) then
                call pc_error ('RTC: FATAL ERROR IN RTCSTATIO')
                call rtc_wrapup (obj)
                ntr = FATAL_ERROR
                return
          end if
      end if

!----------set header words of output trace.

      obj%ntout = obj%ntout + 1
      hdo(1,1) = obj%ntout
      call mutehw (hdo(1:,1),tro(1:,1),obj%ndpt,static,MUTEHW_SET)
      hdo(43,1) = hdo(43,1) + static*obj%dt*1000.0
      if (obj%hdr_cc /= 0) hdo(obj%hdr_cc,1) = ccbest

!----------if current cmp is completed clear the stack trace and fold.

      if(obj%nleftb == 0 .and. obj%nlefto == 0) then
          obj%sttr (1:obj%ndpt) = 0.0
          obj%kfold(1:obj%ndpt) = 0
          obj%hds  (1:obj%nwih) = 0.0
      end if

!----------pass traces to next process and set flag for return from below.

      ntr = 1
      obj%iflow = 2
      return
      end subroutine rtc


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine rtc_wrapup (obj)
      implicit none
      type(rtc_struct),intent(inout) :: obj               ! arguments
      integer                        :: lun,k,nfail       ! local
      logical                        :: error             ! local

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      lun = pc_get_lun()

      write(lun,*) ' '
      write(lun,*) '************* RTC FINAL STATISTICS ************'
      write(lun,*) ' '

      write(lun,1000)
      do k = 1,obj%num_win
           if (obj%ntotal(k) > 0) obj%avcc(k) = obj%avcc(k)/obj%ntotal(k)
           if (obj%npass (k) > 0) obj%avst(k) = obj%avst(k)/obj%npass(k)
           nfail = obj%ntotal(k) - obj%npass(k)
           write(lun,2000) k,obj%avcc(k),obj%ntotal(k),nfail,   &
                           obj%npass(k),obj%avst(k),obj%stlo(k),obj%stup(k)
      end do
1000  format ('   WINDOW   AVERAGE   TOTAL       #        #    ',       &
         '--------GOOD STATIC--------'/                                 &
              '      #      CCOEF      #       FAIL     PASS   ',       &
         '|AVERAGE|     MIN       MAX')
2000  format (i7,f11.3,i9,i9,i9,f10.1,f10.1,f10.1)
      write(lun,*) 'TOTAL NUMBER OF TRACES NOT SHIFTED = '       ,obj%nkill
      write(lun,*) 'TOTAL NUMBER OF TRACES SHIFTED STATICALLY = ',obj%nstat
      write(lun,*) 'TOTAL NUMBER OF TRACES SHIFTED DYNAMICALLY= ',obj%ndyn
      write(lun,*) ' '

      if (associated(obj%temptfile)) call temptfile_close (obj%temptfile)
      if (associated(obj%rtcstatio)) call rtcstatio_close (obj%rtcstatio)

      call basetrace_end_iteration (obj%basetrace, error)
      call basetrace_wrapup        (obj%basetrace)

      write(lun,*) ' '
      write(lun,*) '*********** END RTC FINAL STATISTICS **********'
      write(lun,*) ' '
      return
      end subroutine rtc_wrapup


!!---------------------------- rtc drm ------------------------------------!!
!!---------------------------- rtc drm ------------------------------------!!
!!---------------------------- rtc drm ------------------------------------!!

! if iflag = 0 for output of a record, this routine returns the record
!              number to use. it is the next available record on disk,
!              or fills in a hole in the file.

! if iflag = 1 for input of a record, this routine sets a flag that
!              a hole is available in the file where the record irec
!              was input. this space is then available for another
!              output record.


      subroutine rtc_drm(iflag,nrod,irec)
      implicit none
      integer,intent(in)    :: iflag               ! arguments
      integer,intent(inout) :: irec,nrod(:)        ! arguments

!----------find available hole in disk file.

      if(iflag == 0) then
         if(nrod(1) == 1) then
            irec = nrod(2)
            nrod(2) = nrod(2) + 1
            return
         else
            irec = nrod(nrod(1) + 1)
            nrod(nrod(1) + 1) = 0
            nrod(1) = nrod(1) - 1
            return
         end if

!----------point to input record as a new hole.

      else
         nrod(1) = nrod(1) + 1
         nrod(nrod(1) + 1) = irec
         return
      end if
      return
      end subroutine rtc_drm


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module rtc_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

