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
!------------------------------------------------------------------------------
!                         C P S   P R O C E S S
!
! Name       : SHFT  (SHiFT traces by static)
! Category   : statics
! Written    : 1988-11-23   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Apply static shift to traces.
! Portability: No known limitations.
! Parallel   : Yes
!
!------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! SHFT is a process for applying static shifts to traces.  Typically the static
! values are obtained from statics files which contain a record of the header
! words that were used for generating them.
!
! Typical Statics Header Words
! ----------------------------
! The following header words are the ones most commonly used in source and
! receiver statics.
!
!      GROUP    statics:    9
!      SOURCE   statics:    33, or 33 & 34, or 46
!      RECEIVER statics:    35, or 35 & 36, or 47
!
! SOURCE=RECEIVER statics files use both source and receiver header words.
! (GROUP statics are statics based on original shot profile number rather than
! source ground position.)
!
! Self-Describing Statics Files
! -----------------------------
! Since statics files contain a record of the header words used in generating
! them, SHFT will apply these statics files properly by using the header words
! recorded in the files.  Only if statics files are to be applied using header
! words other than those used to generate the files should APPLY_1 and APPLY_2
! be set. APPLY_1 and APPLY_2 supercede the header words recorded in the files.
! Normally they should be left blank.
!
! If and only if the file is a SOURCE or RECEIVER or SOURCE=RECEIVER file,
! APPLY_1 or APPLY_2 can be specified to apply the file differently.  If
! APPLY_1 or APPLY_2 = R, S or SER, the file will be applied as
! a RECEIVER file, a SOURCE file or a SOURCE=RECEIVER file.
!
! Cumulative Static Header Words
! ------------------------------
! The cumulative static header words are updated by SHFT as follows.
!
! OPT_MODE = PRE or POST           cumulative datum      static header word 41
! OPT_MODE = RPRE or RPOST         cumulative refraction static header word 42
! OPT_MODE = FILES with type DATUM cumulative datum      static header word 41
! OPT_MODE = FILES with type REFR  cumulative refraction static header word 42
! OPT_MODE = FILES with type RESID cumulative residual   static header word 43
! OPT_MODE = anything else         nothing
!
! Interpolation and Extrapolation
! -------------------------------
! When static files are applied by SHFT, the static will be linearly
! interpolated in case a trace has a ground position that falls between two
! adjacent ground positions in the file.  Also, the static will be extrapolated
! with the first or last value in case a trace has a ground position preceding
! the first ground position in the file, or following the last ground position
! in the file.  Nils in a static file are used as if they are zero (literally
! -1.E-30 ms).
!
! RTC Files
! ---------
! If PATHNAME_1 refers to an RTC file, you must set APPLY_1 = RTC.  At present,
! the following restrictions apply to the application of an RTC file:  (1) The
! traces must be the same traces that were passed  to RTC when RTC generated
! the file.  (2) The traces must be in the same order as they were when RTC
! generated the file.  (3) If RTC was run in a multi-window mode, the average
! static derived for the trace will be applied to the entire trace.
!
!------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
!
! No special requirements.
!
!------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process may alter input traces.
! This process outputs the same traces as it receives (possibly altered).
! This process outputs traces with same gather status as the input traces.
!
!------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NWIH     number of words in trace header       used but not changed
! NDPT     number of sample values in trace      used but not changed
! DT       trace sample interval                 used but not changed
!
!------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 1       HDR_FLAG                   flagword - not changed
! 41      cumulative datum header    may update
! 42      cumulative refr header     may update
! 43      cumulative resid header    may update
!
! 1       HDR_FLAG                   flagword
! 41      cum. datum hdr             may update
! 42      cum. refr hdr              may update
! 43      cum. resid hdr             may update
!
!------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
!031. 2006-10-16 D. Glover  Added NULLIFY statements for Intel compiler.
!030. 2006-01-10 B. Menger  Removed Unused Variables.
! 29. 2002-05-06 Vunderink  Added parallel control parameters
! 28. 2002-02-14 Stoeckley  Remove an unnecessary error box popup.
! 27. 2002-02-04 Stoeckley  Fix documentation and display error regarding
!                            header words 56 and 57 for OPT_MODE = RPOST.
! 26. 2001-10-18 Stoeckley  Add file selection boxes and file status messages.
! 25. 2001-02-13 Brad Kruse Change name for wrapup flag to SKIP_WRAPUP for
!                            clarity, and slightly change how it is used.
! 24. 2000-10-05 Brad Kruse Request: Add 'HelpSection' entries for readonly
!                            parameters SAMPLE_SPACING and SAMPLE_SHIFT.
! 23. 2000-07-13 Brad Kruse Request: Lengthen STATIC FILE field.  Correct 
!                           unpaired 'execute_only' tag.
! 22. 2000-06-27 Brad Kruse Bug: Missing obj%shift_factor caused MUTEHW to
!                            record incorrect mute words.  Shift in ms. is
!                            now converted to samples for call to MUTEHW.
!                            Changed gui-only parameters SAMPLE_SPACING and
!                            SAMPLE_SHIFT to only write to the GUI, and not
!                            the job file.
! 21. 2000-06-13 Brad Kruse Review team comment 'GUI element line up can 
!                           be improved'.
! 20. 2000-05-23 Kruse      Correct SAMPLE_SPACING info field, to eliminate
!                           crashing for sub-millisecond shifts.
! 19. 2000-04-19 Kruse      Rearrange writing SAMPLE_SPACING info field to
!                           avoid failing on Pacific Group/Linux compiler.
! 18. 2000-04-05 Kruse      Change filename check to use PATHCHECK_EMPTY.
! 17. 2000-04-04 Kruse      Add RCS ID.
! 16. 2000-03-24 Kruse      Fixed bugs.  Polarity switch was set incorrectly.
! 15. 2000-03-10 Kruse      Change to use STATIO and STATUTIL instead of
!                            STATICS_UTIL (by Stoeckley).  Change parameter
!                            SUB to POLARITY, per CIB.
!                            Add combo boxes where needed.  Add LAV update.
!                            Place EZGUI layout in the header.
! 14. 1999-11-29 Dorman     Insert "use statcc_module" statement into
!                           module shft_module.
! 13. 1999-09-23 Dorman     Convert to new CPS standards and Fortran 90
! 12. 1998-11-11 Goodger    Begin using fortran90 compiler.
! 11. 1992-02-18 Troutt     Add mute header word adjustment for HW64 (add
!                           call to MUTEHW)
! 10. 1990-10-23 Peterson   Include error and abort arguments on calls to
!                           HPALLOC and HPDEALLC.
! 9.  1990-10-12 Stoeckley  Fix READ format for RTC files to overcome
!                           read error when offset is too large.
! 8.  1990-05-25 Stoeckley  Replace code by calls to STATUTIL routines.
! 7.  1989-08-25 Stoeckley  Add application of RTC files, and add SAVE
!                           statement to get around compiler bug.
! 6.  1989-07-24 Stoeckley  Add COMMENT parameter.
! 5.  1989-05-15 Stoeckley  Add notes to documentation and change use of
!                           the FLAGWORD parameter.
! 4.  1989-05-01 Stoeckley  Fix bug in SHFTSET.
! 3.  1989-03-21 Stoeckley  Fix bug when applying static files, add mute
!                           header word adjustment, and add options RPRE
!                           and RPOST.
! 2.  1989-01-26 Stoeckley  Fix documentation for FLAGWORD, and add
!                           interpolation between static file values.
! 1.  1988-11-23 Stoeckley  Original Version
!
!------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!
!------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!
!------------------------------------------------------------------------------
!</programming_doc>


!------------------------------------------------------------------------------
!<gui_def>
!<NS SHFT Process/NC=80/NR=28>
!                              SHFT (trace shift)
!                         Apply static shift to traces.
!
! HDR_FLAG =`III      [/L]<- Header word denoting flagged traces.
! OPT_MODE =`CCCCC    [/L]<- Type of static shift to apply.
! POLARITY =`CCCC     [/L]<- Whether to add or subtract the static correction.
! HDR_STAT =`III      [/L]<- Header word containing static value to apply.
! SHFT_BULK=`FFFFFFFF [/L]<- Bulk shift (in ms) for OPT_MODE = BULK, GUN, or SL.
!
!         [SS]`XXXXXXXXX [/L]<- Current DT Sample Spacing (in ms).
!         [BS]`XXXXXXXXX [/L]<- Current Bulk Shift in Samples.
!
! [/L]First Static File:
! Select PATHNAME_1[PATHNAME_1]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                   [info1]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! APPLY_1 =`CCC  [/L]<- Override First Static File default.
!
! [/L]Optional Second Static File:
! Select PATHNAME_2[PATHNAME_2]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                   [info2]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! APPLY_2 =`CCC  [/L]<- Override Optional Second Static File default.
!
! [/L]History Comment:
! COMMENT=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!<PARMS COMMENT[/ML=80]>
!<PARMS PATHNAME_1[/XST/ML=140]>
!<PARMS PATHNAME_2[/XST/ML=140]>
!<PARMS SS[SAMPLE_SPACING/EN]>
!<PARMS BS[SAMPLE_SHIFT/EN]>
!<PARMS info1[pathname_1_info/EN]>
!<PARMS info2[pathname_2_info/EN]>
!</gui_def>
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="OPT_MODE">
!<Tip> Option of what type of static shift to apply. </Tip>
! Default = FILES
! Allowed = BULK    (Bulk shift - same shift for all traces.)
! Allowed = FILES   (Apply shifts from one or two static files.)
! Allowed = GUN     (Gun delay correction, a bulk shift.)
! Allowed = HEAD    (Apply static value in a given header word.)
! Allowed = PRE     (Pre-NMO datum shift in header word 39.)
! Allowed = POST    (Post-NMO datum shift in header word 40.)
! Allowed = RPRE    (Pre-NMO refraction, or other, static in HDR 56.)
! Allowed = RPOST   (Post-NMO refraction, or other, static in HDR 57.)
! Allowed = SL      (Sea level correction, a bulk shift.)
!</Help>
!
!
!<Help KEYWORD="PATHNAME_1_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME_1. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME_2_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME_2. </Tip>
!</Help>
!
!
!<Help KEYWORD="SELECT_PATHNAME_1">
!<Tip> Choose PATHNAME_1 using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="SELECT_PATHNAME_2">
!<Tip> Choose PATHNAME_2 using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="SHFT_BULK">
!<Tip> Amount of bulk shift static to apply, in ms. </Tip>
! Default = 0.0
! Allowed = real
!
! SHFT_BULK is active only if OPT_MODE = BULK, GUN or SL.
!</Help>
!
!
!<Help KEYWORD="SAMPLE_SPACING" TYPE= "DISPLAY_ONLY">
!<Tip> Current DT Sample spacing (in ms). </Tip>
! Allowed = real
!
! SAMPLE_SPACING is an information-only field.  The value is calculated by
! multiplying the global sample spacing (DT, in seconds) by 1000.0.  The
! result is displayed as the sample spacing, in milliseconds.
!</Help>
!
!
!<Help KEYWORD="SAMPLE_SHIFT" TYPE= "DISPLAY_ONLY">
!<Tip> Current Bulk Shift in Samples. </Tip>
! Allowed = real
!
! SAMPLE_SHIFT is only active if OPT_MODE = BULK, SL, or GUN.
!
! SAMPLE_SHIFT is an information-only field.  The value is calculated by 
! dividing the bulk shift value (SHFT_BULK) by the global parameter DT, and
! multiplying the result by 1000.0.  The sign is negated if POLARITY = SUB.
!</Help>
!
!
!<Help KEYWORD="PATHNAME_1">
!<Tip> Pathname of static file containing static values to apply. </Tip>
! Default = NONE
! Allowed = char
!
! PATHNAME_1 is only active if OPT_MODE = FILES.
!</Help>
!
!
!<Help KEYWORD="PATHNAME_2">
!<Tip> Pathname of second static file containing static values to apply. </Tip>
! Default = NONE
! Allowed = char
!
! PATHNAME_2 is only active if OPT_MODE = FILES.
!</Help>
!
!
!<Help KEYWORD="APPLY_1">
!<Tip> Method for applying the file specified by PATHNAME_1. </Tip>
! Default =  -
! Allowed = REC  (Apply static as a receiver static.)
! Allowed = SRC  (Apply static as a source static.)
! Allowed = SER  (Apply static as a source=receiver static.)
! Allowed = RTC  (Apply static as an RTC static.)
!
! Normally APPLY_1 should be left blank, allowing static files to be applied in
! the same manner as they were created.
!</Help>
!
!
!<Help KEYWORD="APPLY_2">
!<Tip> Method for applying the file specified by PATHNAME_2. </Tip>
! Default =  -
! Allowed = REC  (Apply static as a receiver static.)
! Allowed = SRC  (Apply static as a source static.)
! Allowed = SER  (Apply static as a source=receiver static.)
!
! Normally APPLY_2 should be left blank, allowing static files to be applied in
! the same manner as they were created.
!</Help>
!
!
!<Help KEYWORD="POLARITY">
!<Tip>Option to Add the static correction, or Subtract the correction.</Tip>
! Default = ADD
! Allowed = ADD  (Add the static correction)
! Allowed = SUB  (Subtract the static correction)
!
! Specify whether to invert the direction of the correction.
!</Help>
!
!
!<Help KEYWORD="HDR_STAT">
!<Tip> Header word containing static value to apply. </Tip>
! Default = 0
! Allowed = 0 - NWIH
!
! HDR_STAT is active only if OPT_MODE = HEAD.
!</Help>
!
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 1 - NWIH
!
! If HDR_FLAG = 0, then all traces are shifted.  Otherwise, only traces with
! a flag set in header word HDR_FLAG are shifted.
!</Help>
!
!
!<Help KEYWORD="COMMENT">
!<Tip> Optional one line comment for history file. </Tip>
! Default = NONE
! Allowed = char
!</Help>
!
!</HelpSection>
!-----------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

module shft_module
  !
  ! - Module references
  !
  use pc_module
  !
  use named_constants_module
  !
  use getlun_module
  !
  use pathchoose_module
  !
  use statio_module
  !
  use statutil_module
  !
  use statcc_module         ! added 1999-11-29
  !
  use mutehw_module
  !
  use pathcheck_module, only:   &
        FILENAME_LENGTH,        &
        pathcheck,              &
        PATHCHECK_EMPTY,        &
        PATHCHECK_VALID,        &
        PATHCHECK_UNSPECIFIED,  &
        PATHCHECK_INFO_INPUT
  !
  use lav_module, only:  lav_set_hdr
  !
  implicit none
  !
  private
  public :: shft_create     ! uses the parameter cache.
  public :: shft_initialize
  public :: shft_update     ! uses the parameter cache.
  public :: shft_delete

!<execute_only>

  public :: shft            ! main execution (trace processing) routine.
  public :: shft_wrapup

!</execute_only>

  character(len=100),public,save :: shft_ident =    &
       '$Id: shft.f90,v 1.31 2006/10/17 13:45:47 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

  type :: file_struct
    real, dimension (:,:), pointer  :: statics  ! s1
    character (len=FILENAME_LENGTH) :: pathname       ! process parameter
    character (len=3)               :: apply          ! process parameter
    integer                         :: f_status
    logical                         :: use
    logical                         :: rtc
    character (len=5)               :: f_type
    integer                         :: ihead
    integer                         :: nhx1
    integer                         :: nhx2
    integer                         :: nhy1
    integer                         :: nhy2
    real                            :: x
    real                            :: y
    real                            :: xinc
    real                            :: yinc
    integer                         :: nx
    integer                         :: ny
  end type file_struct


  type, public :: shft_struct

    private
    logical                        :: skip_wrapup      ! wrapup flag.
    !
    ! - globals
    !
    integer                        :: nwih             ! number header words
    integer                        :: ndpt             ! number trace samples
    real                           :: dt               ! trace interval
    !
    ! - process parameters
    !
    character(len=5)               :: opt_mode         ! process parameter
    real                           :: shft_bulk        ! process parameter
    type (file_struct)             :: file (2)
    character(len=3)               :: polarity         ! process parameter
    integer                        :: hdr_stat         ! process parameter
    integer                        :: hdr_flag         ! process parameter
    character(len=80)              :: comment          ! process parameter
    !
    ! - state variables
    !
    logical                         :: start
    integer                         :: jfile
    integer                         :: ihead
    real                            :: shift_factor
    type(pathchoose_struct),pointer :: dialog1
    type(pathchoose_struct),pointer :: dialog2
    !
  end type shft_struct

  !!--------------------------------- data ---------------------------------!!
  !!--------------------------------- data ---------------------------------!!
  !!--------------------------------- data ---------------------------------!!

  integer, parameter           :: nparm = 71
  character(len=64), parameter :: fmt = '(L,F,2L.80,3L,2I,H.80)'

  type(shft_struct),pointer,save :: object      ! needed for traps.


  integer, parameter :: N_APPLY_VALS   = 5
  integer, parameter :: N_APPLY_2_VALS = N_APPLY_VALS - 1
  character (len = *), dimension (N_APPLY_VALS), parameter :: APPLY_VALS     &
    = (/ '   ', 'REC', 'SRC', 'SER', 'RTC' /)

  integer, parameter :: N_OPT_VALS   = 9
  character (len = *), dimension (N_OPT_VALS), parameter :: OPT_VALS  &
    = (/'BULK ', 'FILES','GUN  ', 'HEAD ', 'PRE  ', 'POST ', 'RPRE ',     &
        'RPOST', 'SL   '/)

contains

  !!-------------------------------- create --------------------------------!!
  !!-------------------------------- create --------------------------------!!
  !!-------------------------------- create --------------------------------!!

  subroutine shft_create (obj)
    !
    type(shft_struct),pointer :: obj       ! arguments
    !
    ! - Begin shft_create
    !
    allocate (obj)

    nullify  (obj%file (1)%statics)       ! all pointers must be nullified
    nullify  (obj%file (2)%statics)
    nullify (obj%dialog1) ! jpa
    nullify (obj%dialog2) ! jpa
    !
    call pathchoose_create (obj%dialog1, 'PATHNAME_1', '*')
    call pathchoose_create (obj%dialog2, 'PATHNAME_2', '*')

    call shft_initialize (obj)
    !
  end subroutine shft_create


  !!-------------------------------- delete --------------------------------!!
  !!-------------------------------- delete --------------------------------!!
  !!-------------------------------- delete --------------------------------!!

  subroutine shft_delete (obj)
    !
    type(shft_struct),pointer :: obj       ! arguments
    !
    ! - Begin shft_delete
    !
!<execute_only>
    call shft_wrapup (obj)
!</execute_only>

    if (associated(obj%file (1)%statics)) deallocate(obj%file (1)%statics)
    if (associated(obj%file (2)%statics)) deallocate(obj%file (2)%statics)

    call pathchoose_delete (obj%dialog1)
    call pathchoose_delete (obj%dialog2)

    deallocate(obj)
    !
  end subroutine shft_delete


  !!------------------------------ initialize ------------------------------!!
  !!------------------------------ initialize ------------------------------!!
  !!------------------------------ initialize ------------------------------!!

  subroutine shft_initialize (obj)
    !
    type(shft_struct),pointer :: obj       ! arguments
    !
    ! - Begin shft_initialize
    !
    obj%skip_wrapup = .true.

    obj%opt_mode      = OPT_VALS (2)
    obj%polarity      = 'ADD'
    obj%hdr_stat      = HDR_OFFSET
    obj%hdr_flag      = 0
    obj%comment       = ' '
    obj%nwih          = 0
    obj%ndpt          = 0
    obj%dt            = 0.0
    obj%start         = .true.
    obj%shft_bulk     = 0.0
    obj%shift_factor  = 1.0
    obj%ihead         = 0

    obj%file (1)%apply      = APPLY_VALS (1)
    obj%file (1)%f_status   = PATHCHECK_UNSPECIFIED
    obj%file (1)%f_type     = ' '
    obj%file (1)%ihead      = 0
    obj%file (1)%nhx1       = 0
    obj%file (1)%nhx2       = 0
    obj%file (1)%nhy1       = 0
    obj%file (1)%nhy2       = 0
    obj%file (1)%nx         = 0
    obj%file (1)%ny         = 0
    obj%file (1)%pathname   = PATHCHECK_EMPTY
    obj%file (1)%rtc        = .false.
    obj%file (1)%use        = .false.
    obj%file (1)%x          = 0.0
    obj%file (1)%xinc       = 0.0
    obj%file (1)%y          = 0.0
    obj%file (1)%yinc       = 0.0

    obj%file (2)            = obj%file (1)

    call shft_update (obj)
    !
  end subroutine shft_initialize

  !!--------------------------- start of update ----------------------------!!
  !!--------------------------- start of update ----------------------------!!
  !!--------------------------- start of update ----------------------------!!

  subroutine shft_update (obj)
    !
    ! - Arguments
    !
    type (shft_struct), target, intent (inout) :: obj
    !
    ! - Local variables
    !
    character (len = 10) :: info


    !
    ! - Begin shft_update
    !
    object => obj               ! needed for traps.
    obj%skip_wrapup = .true.    ! needed for the wrapup routine.

    !!-------------------------- read parameters ---------------------------!!
    !!-------------------------- read parameters ---------------------------!!
    !!-------------------------- read parameters ---------------------------!!

    if (pathchoose_update(obj%dialog1,obj%file(1)%pathname)) return
    if (pathchoose_update(obj%dialog2,obj%file(2)%pathname)) return

    call pc_get_global ('nwih', obj%nwih)  ! number of header words.
    call pc_get_global ('ndpt', obj%ndpt)  ! number of trace samples.
    call pc_get_global ('dt', obj%dt)    ! trace sample interval (sec).
    !
    call pc_get ('OPT_MODE',   obj%opt_mode)
    call pc_get ('SHFT_BULK',  obj%shft_bulk)

    call pc_get ('PATHNAME_1', obj%file (1)%pathname)
    call pc_get ('APPLY_1'   , obj%file (1)%apply)

    call pc_get ('PATHNAME_2', obj%file (2)%pathname)
    call pc_get ('APPLY_2'   , obj%file (2)%apply)

    call pc_get ('POLARITY'  , obj%polarity)
    call pc_get ('HDR_STAT'  , obj%hdr_stat)
    call pc_get ('HDR_FLAG'  , obj%hdr_flag)
    call pc_get ('COMMENT'   , obj%comment)
    obj%comment = adjustl(obj%comment)     ! anything is acceptable

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

    !
    ! - Global checks
    !
    if (obj%nwih  < 64)  call pc_warning ('NWIH must be at *least* 64.')
    !
    if (obj%ndpt <= 0) call pc_warning ('NDPT must be greater than 0.')
    !
    if (obj%dt   <= 0.0) then
      call pc_put_gui_only ('SAMPLE_SPACING', 'Not Set')
      call pc_error ('DT must be greater than 0.0.')
      obj%shift_factor = 0.001
    else
      obj%shift_factor = 1 / (1000.0 * obj%dt)  ! Convert shift ms to samples
      !
      write (info, '(f10.2)') obj%dt * 1000.0
      call pc_put_gui_only ('SAMPLE_SPACING', trim (info))
    end if

    !
    ! - Polarity
    !
    select case (trim (obj%polarity))
      case ('ADD', 'SUB')
      case default
        call pc_error ("SHFT_UPDATE: Unknown value for POLARITY: "     &
                       // trim (obj%polarity))
    end select
    !
    ! - OPT_MODE
    !
    call pc_put_gui_only ('SAMPLE_SHIFT',   'Not Used')
    !
  verify_opt_mode:   &
    select case (obj%opt_mode)

      !!------------------------------ S.L. ------------------------------!!
      !!----------------------- Mode - S.L. ------------------------------!!
      !!------------------------------ S.L. ------------------------------!!
      case ('S.L. ')
        obj%opt_mode = 'SL   '
        !
        obj%hdr_stat = 0

      !!------------------------- BULK, SL, GUN --------------------------!!
      !!------------------ Mode - BULK, SL, GUN --------------------------!!
      !!------------------------- BULK, SL, GUN --------------------------!!
      case ('BULK ', 'SL   ', 'GUN  ')
        !
        obj%hdr_stat = 0
        !
        if (obj%dt > 0.0) then
          if (obj%polarity == 'SUB') then
            write (info, '(f10.4)') -1 * obj%shift_factor * obj%shft_bulk
          else
            write (info, '(f10.4)') obj%shift_factor * obj%shft_bulk
          end if
        end if
        !
        call pc_put_gui_only ('SAMPLE_SHIFT',   trim (info)) 

      !!----------------------------- Files ------------------------------!!
      !!---------------------- Mode - Files ------------------------------!!
      !!----------------------------- Files ------------------------------!!
      case ('FILES')
        !
        obj%hdr_stat = 0
        !
        ! - PATHNAME_1
        !
        obj%file (1)%pathname = adjustl(obj%file (1)%pathname)
        if (obj%file (1)%pathname(1:1) /= PATHCHECK_EMPTY) then
          if (obj%opt_mode /= 'FILES') then
            call pc_warning ('PATHNAME_1 is only active if OPT_MODE = FILES.')
          endif
        endif
        !
        call pathcheck (KEYWORD  = 'PATHNAME_1',            &
                        PATHNAME = obj%file (1)%pathname,   &
                        STATUS   = obj%file (1)%f_status,   &
                        SHOW     = PATHCHECK_INFO_INPUT)
        !
        if (obj%file (1)%apply == 'RTC') then
          obj%file (1)%rtc = obj%file (1)%f_status == PATHCHECK_VALID
        else
          obj%file (1)%use = obj%file (1)%f_status == PATHCHECK_VALID
        end if
        !
        ! - APPLY_1
        !
        select case (obj%file (1)%apply)
          case ('   ', 'REC', 'SRC', 'SER', 'RTC')
          case default
            call pc_error ("SHFT: APPLY_1 value error -- "     &
                           // obj%file (1)%apply  &
                           // "APPLY_1 is set to ' '")
            obj%file (1)%apply = ' '
        end select
        !
        ! - PATHNAME_2
        !
        obj%file (2)%pathname = adjustl(obj%file (2)%pathname)
        if (obj%file (2)%pathname(1:1) /= PATHCHECK_EMPTY) then
          if (obj%opt_mode /= 'FILES') then
            call pc_error ('PATHNAME_2 is only active if OPT_MODE = FILES.')
          end if
        end if
        !
        call pathcheck (KEYWORD  = 'PATHNAME_2',            &
                        PATHNAME = obj%file (2)%pathname,   &
                        STATUS   = obj%file (2)%f_status,   &
                        SHOW     = PATHCHECK_INFO_INPUT)
        !
        obj%file (2)%use = obj%file (2)%f_status == PATHCHECK_VALID
        !
        ! - APPLY_2
        !
        select case (obj%file (2)%apply)
          case ('   ', 'REC', 'SRC', 'SER')
          case default
            call pc_error ("SHFT: APPLY_2 value error -- "     &
                           // obj%file (2)%apply   &
                           // "APPLY_2 is set to ' '")
            obj%file (1)%apply = ' '
        end select

        if(.not. (obj%file (1)%use     &
                  .or. obj%file (2)%use     &
                  .or. obj%file (1)%rtc)) then
          call pc_error('OPT_MODE = FILES specified, but pathname and apply &
                        & parameters are incorrectly specified.')
        end if

      !!----------------------------- HEAD  ------------------------------!!
      !!---------------------- Mode - HEAD  ------------------------------!!
      !!----------------------------- HEAD  ------------------------------!!
      case ('HEAD ')
        !
        ! - HDR_STAT
        !
        if (pc_verify_end()) then
        if (obj%hdr_stat < 1 .or. obj%hdr_stat > obj%nwih) then
          call pc_error ('HDR_STAT is ', obj%hdr_stat,   &
                         ', but must be in the range 1 through', obj%nwih)
        end if
        end if

      !!-------------------- PRE, POST, RPRE, RPOST  ---------------------!!
      !!------------- Mode - PRE, POST, RPRE, RPOST  ---------------------!!
      !!-------------------- PRE, POST, RPRE, RPOST  ---------------------!!
      case ('PRE  ', 'POST ', 'RPRE ', 'RPOST')
        select case (obj%opt_mode)
          case ('PRE  ') ; obj%hdr_stat = 39 ; obj%ihead = 41
          case ('POST ') ; obj%hdr_stat = 40 ; obj%ihead = 41
          case ('RPRE ') ; obj%hdr_stat = 56 ; obj%ihead = 42
          case ('RPOST') ; obj%hdr_stat = 57 ; obj%ihead = 42
        end select

      case default
        call pc_error ('OPT_MODE = ' // obj%opt_mode //    &
                       ' is not an acceptable OPT_MODE parameter.')
    end select verify_opt_mode
    !
    ! - HDR_FLAG
    !
    if (obj%hdr_flag < 0 .or. obj%hdr_flag > obj%nwih) then
      call pc_error ('HDR_FLAG is ', obj%hdr_flag,   &
                     ' but must be in the range 0 through', obj%nwih)
    end if


  call pc_put_sensitive_field_flag ('SELECT_PATHNAME_1',obj%opt_mode=='FILES')
  call pc_put_sensitive_field_flag ('SELECT_PATHNAME_2',obj%opt_mode=='FILES')
  call pc_put_sensitive_field_flag ('PATHNAME_1'       ,obj%opt_mode=='FILES')
  call pc_put_sensitive_field_flag ('PATHNAME_2'       ,obj%opt_mode=='FILES')
  call pc_put_sensitive_field_flag ('PATHNAME_1_INFO'  ,obj%opt_mode=='FILES')
  call pc_put_sensitive_field_flag ('PATHNAME_2_INFO'  ,obj%opt_mode=='FILES')
  call pc_put_sensitive_field_flag ('APPLY_1'          ,obj%opt_mode=='FILES')
  call pc_put_sensitive_field_flag ('APPLY_2'          ,obj%opt_mode=='FILES')

  call pc_put_sensitive_field_flag ('HDR_STAT'  ,obj%opt_mode=='HEAD')

  call pc_put_sensitive_field_flag ('SAMPLE_SHIFT',obj%opt_mode=='SL' .or. &
                         obj%opt_mode=='BULK' .or. obj%opt_mode=='GUN')

  call pc_put_sensitive_field_flag ('SHFT_BULK'   ,obj%opt_mode=='SL' .or. &
                         obj%opt_mode=='BULK' .or. obj%opt_mode=='GUN')


    !!--------------------- call processes internally ----------------------!!
    !!--------------------- call processes internally ----------------------!!
    !!--------------------- call processes internally ----------------------!!


    !!-------------------------- write parameters --------------------------!!
    !!-------------------------- write parameters --------------------------!!
    !!-------------------------- write parameters --------------------------!!

    call pc_put_global ('nwih',  obj%nwih)  ! number of header words.
    call pc_put_global ('ndpt',  obj%ndpt)  ! number of trace samples.
    call pc_put_global ('dt',    obj%dt)    ! trace sample interval (sec).
    !
    call pc_put_options_field (keyword  = 'OPT_MODE',   &
                               options  = OPT_VALS,     &
                               noptions = N_OPT_VALS)
    call pc_put ('OPT_MODE',   obj%opt_mode  )

    call pc_put ('SHFT_BULK',  obj%shft_bulk )
    !
    !
    call pc_put ('PATHNAME_1', obj%file (1)%pathname)
    call pc_put ('PATHNAME_2', obj%file (2)%pathname)
    !
    call pc_put ('APPLY_1',    obj%file (1)%apply   )
    call pc_put_options_field (keyword  = 'APPLY_1',   &
                             options  = APPLY_VALS,    &
                               noptions = N_APPLY_VALS)
    !
    call pc_put ('APPLY_2',    obj%file (2)%apply   )
    call pc_put_options_field (keyword  = 'APPLY_2',     &
                               options  = APPLY_VALS,    &
                               noptions = N_APPLY_2_VALS)
    !
    call pc_put_options_field (keyword  = 'POLARITY',                    &
                               options  = (/ 'ADD', 'SUB' /),    &
                               noptions = 2)
    call pc_put ('POLARITY',   obj%polarity  )

    if (obj%opt_mode == 'POST') then
      call pc_put ('HDR_STAT',   40  )
    else if (obj%opt_mode == 'RPRE') then
      call pc_put ('HDR_STAT',   56  )
    else if (obj%opt_mode == 'RPOST') then
      call pc_put ('HDR_STAT',   57  )
    else
      call pc_put ('HDR_STAT',   obj%hdr_stat  )
    end if

    call pc_put ('HDR_FLAG',   obj%hdr_flag  )
    call pc_put ('COMMENT',    obj%comment   )

    call pc_put_control ('PARALLEL_SAFE'        ,.true.)
    call pc_put_control ('PCPS_SEND_MODE'       ,'PCPS_SEND_FIRST_AVAIL')
    call pc_put_control ('PCPS_RECEIVE_MODE'    ,'PCPS_RECEIVE_PASSTHRU')
    call pc_put_control ('PCPS_BUNCH_MODE'      ,'PCPS_BUNCH_TRACE_GROUPS')
    call pc_put_control ('PCPS_SEND_EOF_MODE'   ,'PCPS_SEND_ALL_EOF')
    call pc_put_control ('PCPS_ALT_SEND_MODE'   ,'PCPS_SEND_ALL')
    call pc_put_control ('PCPS_ALT_RECEIVE_MODE','PCPS_RECEIVE_ALL_EOF')


    !!----------------------- prepare for execution ------------------------!!
    !!----------------------- prepare for execution ------------------------!!
    !!----------------------- prepare for execution ------------------------!!

!<execute_only>

    if (associated (obj%file (1)%statics)) deallocate (obj%file (1)%statics)
    if (associated (obj%file (2)%statics)) deallocate (obj%file (2)%statics)

    if (pc_do_not_process_traces()) return
    !
    obj%skip_wrapup = .false.

    ! do not allocate pointer obj%file (1)%statics
    ! do not allocate pointer obj%file (2)%statics

    obj%start      = .true.
    !
    if (obj%polarity == 'SUB') then
      obj%shift_factor = -1 * obj%shift_factor
    end if

!</execute_only>

    !!--------------------------- finish update ----------------------------!!
    !!--------------------------- finish update ----------------------------!!
    !!--------------------------- finish update ----------------------------!!

    !
  end subroutine shft_update

  !!-------------------------------- traps ---------------------------------!!
  !!-------------------------------- traps ---------------------------------!!
  !!-------------------------------- traps ---------------------------------!!

  !!---------------------------- main execution ----------------------------!!
  !!---------------------------- main execution ----------------------------!!
  !!---------------------------- main execution ----------------------------!!

!<execute_only>

  subroutine shft (obj,ntr,hd,tr)
    !
    ! - Arguments
    !
    type(shft_struct), intent (inout) :: obj                    ! arguments
    integer,           intent (inout) :: ntr                    ! arguments
    double precision,  intent (inout) :: hd(:,:)                ! arguments
    real,              intent (inout) :: tr(:,:)                ! arguments
    !
    ! - Local variables
    !
    character(len=80) :: msg                    ! local
    integer           :: ier1, ier2, ier, i  ! local
    integer           :: f

    real              :: shift, sss             ! local
    real              :: shifted_tr (obj%ndpt)
    !
    ! - Begin shft
    !
    if (ntr == NEED_TRACES) return
    loop_do_once: do
      !
      if (obj%start) then
        !
        if (obj%file (1)%use) then
          !
          call statio_read_file (FILENAME = obj%file (1)%pathname,     &
                                 STATTYPE = obj%file (1)%f_type,       &
                                 PSTATICS = obj%file (1)%statics,      &
                                 NHX      = obj%file (1)%nhx1,         &
                                 NHY      = obj%file (1)%nhy1,         &
                                 NHX2     = obj%file (1)%nhx2,         &
                                 NHY2     = obj%file (1)%nhy2,         &
                                 X1       = obj%file (1)%x,            &
                                 Y1       = obj%file (1)%y,            &
                                 XINC     = obj%file (1)%xinc,         &
                                 YINC     = obj%file (1)%yinc,         &
                                 NX       = obj%file (1)%nx,           &
                                 NY       = obj%file (1)%ny,           &
                                 err      = ier1,                &
                                 msg      = msg,                 &
                                 lunprint = pc_get_lun())
          !
          if (ier1 /= 0) then
            call pc_error ("SHFT:",msg)
            call pc_error ("SHFT: Open error in Statics.  Error:", ier1,   &
                           "   File: " // trim (obj%file (1)%pathname))
            call pc_error ('SHFT: Fatal error.')
            ntr = fatal_error
            exit loop_do_once
          end if
          !
          call shft_set (apply = obj%file (1)%apply,    &
                         nhx   = obj%file (1)%nhx1,     &
                         nhy   = obj%file (1)%nhy1,     &
                         nhx3  = obj%file (1)%nhx2,     &
                         nhy3  = obj%file (1)%nhy2,     &
                         ier   = ier2)
          !
          if (ier2 /= 0) then
            call pc_error ('SHFT:  Fatal error.')
            ntr = fatal_error
            exit loop_do_once
          end if
          !
          obj%file (1)%ihead=0
          if (obj%file (1)%f_type.eq.'DATUM') obj%file (1)%ihead=41
          if (obj%file (1)%f_type.eq.'REFR ') obj%file (1)%ihead=42
          if (obj%file (1)%f_type.eq.'RESID') obj%file (1)%ihead=43
          !
        end if
        !
        if (obj%file (2)%use) then
          !
          call statio_read_file (FILENAME = obj%file (2)%pathname,     &
                                 STATTYPE = obj%file (2)%f_type,       &
                                 PSTATICS = obj%file (2)%statics,      &
                                 NHX      = obj%file (2)%nhx1,         &
                                 NHY      = obj%file (2)%nhy1,         &
                                 NHX2     = obj%file (2)%nhx2,         &
                                 NHY2     = obj%file (2)%nhy2,         &
                                 X1       = obj%file (2)%x,            &
                                 Y1       = obj%file (2)%y,            &
                                 XINC     = obj%file (2)%xinc,         &
                                 YINC     = obj%file (2)%yinc,         &
                                 NX       = obj%file (2)%nx,           &
                                 NY       = obj%file (2)%ny,           &
                                 err      = ier1,                &
                                 msg      = msg,                 &
                                 lunprint = pc_get_lun())
          !
          if (ier1 /= 0) then
            call pc_error ("SHFT:",msg)
            call pc_error ("SHFT: Open error in Statics.  Error:", ier1,   &
                           "   File: " // trim (obj%file (2)%pathname))
            call pc_error ('SHFT: Fatal error.')
            ntr = fatal_error
            exit loop_do_once
          end if
          !
          call shft_set (apply = obj%file (2)%apply,    &
                         nhx   = obj%file (2)%nhx1,     &
                         nhy   = obj%file (2)%nhy1,     &
                         nhx3  = obj%file (2)%nhx2,     &
                         nhy3  = obj%file (2)%nhy2,     &
                         ier   = ier2)
          !
          if (ier2 /= 0) then
            call pc_error ('SHFT:  Fatal error.')
            ntr = fatal_error
            exit loop_do_once
          end if
          !
          obj%file (2)%ihead=0
          if (obj%file (2)%f_type.eq.'DATUM') obj%file (2)%ihead=41
          if (obj%file (2)%f_type.eq.'REFR ') obj%file (2)%ihead=42
          if (obj%file (2)%f_type.eq.'RESID') obj%file (2)%ihead=43
          !
        end if
        !
        if (obj%file (1)%rtc) then
          call shft_rtc1 (obj, ier)
          if (ier /= 0) then
            call pc_error ('SHFT:  Fatal error.')
            ntr = fatal_error
            exit loop_do_once
          end if
        end if
        !
        obj%start=.false.
        !
      end if
      !
      ! - process a group of traces.
      !
    loop_thru_ntr:    &
      do i = 1, ntr
        !
        ! - skip traces that are not flagged.
        !
        if (obj%hdr_flag > 0) then
          if (hd(obj%hdr_flag,i) == 0.0d0) cycle
        end if
        !
        ! - get the static shift from files.
        !
        if (obj%file (1)%use .or. obj%file (2)%use .or. obj%file (1)%rtc) then
          !
          shift=0.0
          !
        loop_thru_files:    &
          do f = 1, 2
            !
            if (obj%file (f)%use) then
              sss = statutil_get2 (HD      = hd(1:obj%nwih,i),       &
                                   STATICS = obj%file (f)%statics,   &
                                   NHX     = obj%file (f)%nhx1,      &
                                   NHY     = obj%file (f)%nhy1,      &
                                   NHX2    = obj%file (f)%nhx2,      &
                                   NHY2    = obj%file (f)%nhy2,      &
                                   X1      = obj%file (f)%x,         &
                                   Y1      = obj%file (f)%y,         &
                                   XINC    = obj%file (f)%xinc,      &
                                   YINC    = obj%file (f)%yinc,      &
                                   NX      = obj%file (f)%nx,        &
                                   NY      = obj%file (f)%ny)
              !
              if (obj%file (f)%ihead > 0) then
                hd(obj%file (f)%ihead, i) = hd(obj%file (f)%ihead, i) + sss
              end if
              !
              shift = shift + sss
              !
            end if
            !
          end do loop_thru_files
          !
          if (obj%file (1)%rtc) then
            sss = shft_rtc2(obj, hd(:, i), ntr)
            hd(HDR_CUM_RESID_STATIC,i)=hd(HDR_CUM_RESID_STATIC,i)+sss
            shift = shift + sss
          end if
          !
        else
          !
          ! - get the static shift from other information.
          !
          if (obj%hdr_stat == 0) then

            shift = obj%shft_bulk
          else
            shift = hd (obj%hdr_stat, i)
          end if
          !
          if (obj%ihead > 0) hd(obj%ihead, i) = hd(obj%ihead, i) + shift
          !
        end if
        !
        ! - apply the static shift.
        !
        call statcc (shift = shift * obj%shift_factor,    &
                     n     = obj%ndpt,                    &
                     a     = tr(:obj%ndpt,i),             &
                     b     = shifted_tr)
        !
        tr (1:obj%ndpt, i) = shifted_tr
        !
        call mutehw (hd    = hd (1:obj%nwih, i),          &
                     tr    = tr (1:obj%ndpt, i),          &
                     ndpt  = obj%ndpt,                    &
                     shift = shift * obj%shift_factor,    &
                     ikill = MUTEHW_SET)
        !
        if ( hd(HDR_TOP_MUTE,i) >= hd(HDR_BOTTOM_MUTE,i) ) then
          tr(:,i) = 0.0
        end if
        !
      end do loop_thru_ntr
      !
      exit loop_do_once
      !
    end do loop_do_once

    if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR ) then
      call shft_wrapup (obj)
    else
      call lav_set_hdr (tr   = tr,         &
                        hd   = hd,         &
                        ndpt = obj%ndpt,   &
                        ntr  = ntr)
    end if
    !
  end subroutine shft

  !!------------------------------ shft_rtc1 -------------------------------!!
  !!------------------------------ shft_rtc1 -------------------------------!!
  !!------------------------------ shft_rtc1 -------------------------------!!

  subroutine shft_rtc1 (obj, j3)
    !
    ! - Arguments
    !
    type (shft_struct), intent (inout) :: obj
    integer,            intent (out)   :: j3
    !
    ! - Local variables
    !
    integer                       ::     j2 

    !
    ! - Begin shft_rtc1
    !
    j3 = 0
    !
    ! - replace this call with the intrinsic fortran 90 open temporarily
    !
    !      call opnfil (cfile, obj%jfile, j2)
    !
    call getlun (obj%jfile)
    open (unit=obj%jfile, file=obj%file (1)%pathname, iostat=j2)
    !
    if (j2 /= 0) then
      call pc_error ('SHFT:  GET error on RTC static file.')
      j3 = 1
    end if
    !
  end subroutine shft_rtc1

  !!------------------------------- shft_set -------------------------------!!
  !!------------------------------- shft_set -------------------------------!!
  !!------------------------------- shft_set -------------------------------!!

  !
  ! - Reset static file header words for process shft.
  !
  subroutine shft_set (apply, nhx, nhy, nhx3, nhy3, ier)
    !
    ! - Arguments
    !
    character (len = 3), intent (in)    :: apply
    integer,             intent (inout) :: nhx, nhy, nhx3, nhy3
    integer,             intent (inout) :: ier
    !
    ! - Local Variables
    !
    character(len=*), parameter :: msg     &
                                   = 'SHFTSET:  Illegal use of APPLY_1/APPLY_2'
    !
    ! - Local Variables
    !
    logical           :: b1, b2, b3, b4, b5, b6
    !
    ! - Begin shft_set
    !
    b1 = (apply(1:1) == ' ')
    b2 = (apply(1:1) == 'S' .and. nhx == 9)
    b3 = (nhx /= 33 .and. nhx /= 35 .and. nhx /= 46 .and. nhx /= 47)
    b4 = (nhy /= 34 .and. nhy /= 36 .and. nhy /= 0)
    b5 = (nhx3 /= 33 .and. nhx3 /= 35 .and. nhx3 /= 46 .and. &
         &nhx3 /= 47 .and. nhx3 /= 0)
    b6 = (nhy3 /= 34 .and. nhy3 /= 36 .and. nhy3 /= 0)
    !
    if (b1 .or. b2) then                  ! legitimate settings
      !
      ! - 'SHFT:  Static file headers used = ', nhx, nhy, nhx3, nhy3
      !
      ier = 0                             ! no error condition
      return
    end if
    !
    if (b3 .or. b4 .or. b5 .or. b6) then  ! invalid header words
      call pc_error ('SHFTSET:  Illegal use of APPLY_1/APPLY_2')
      ier = 1                             ! error condition
      return
    end if
    !
    if (apply(1:1) == 'R') then           ! use file for receivers
      if (nhx == 33) nhx = 35
      if (nhy == 34) nhy = 36
      if (nhx == 46) nhx = 47
      nhx3 = 0
      nhy3 = 0
    else if (apply(1:3) == 'SER') then    ! use file for source=receiver
      if (nhx == 33) nhx3 = 35
      if (nhy == 34) nhy3 = 36
      if (nhx == 46) nhx3 = 47
      if (nhx == 35) nhx3 = 33
      if (nhy == 36) nhy3 = 34
      if (nhx == 47) nhx3 = 46
    else if (apply(1:1) == 'S') then      ! use file for sources
      if (nhx == 35) nhx = 33
      if (nhy == 36) nhy = 34
      if (nhx == 47) nhx = 46
      nhx3 = 0
      nhy3 = 0
    else
      call pc_error ('SHFTSET:  Illegal use of APPLY_1/APPLY_2')
      ier = 1
      return
    end if
    !
    ier = 0                             ! no error condition
    !
  end subroutine shft_set

  !!------------------------------ shft_rtc2 -------------------------------!!
  !!------------------------------ shft_rtc2 -------------------------------!!
  !!------------------------------ shft_rtc2 -------------------------------!!

  !
  ! - read next static from rtc file.
  !
  real function shft_rtc2 (obj, hd, ntr)
    !
    ! - Arguments
    !
    type (shft_struct), intent (inout) :: obj
    double precision,   intent (inout) :: hd (:)  ! Note: single dimension
    integer,            intent (inout) :: ntr
    !
    ! - Local variables
    !
    integer :: nwt, i, read_stat
    real , dimension(10) :: ccwin, stwin
    real :: hd7, hd8, hd9, hd33, hd34, hd35, hd36, hd46, hd47, ccbest, statms
    !
    character (len = *), parameter :: fmt5000 = '(5x,9f5.0,2f10.3,i5)'
    character (len = *), parameter :: fmt6000 = '(8f10.3)'

    ! - Begin shft_rtc2
    !
    ! - read one record
    !
    read (obj%jfile, fmt5000, iostat=read_stat)    &
      hd7, hd8, hd9, hd33, hd34, hd35, hd36, hd46, hd47, ccbest, statms, nwt
    !
    if (read_stat == 0) then
      !
      if (nwt > 1) then
          read (obj%jfile, fmt6000, iostat=read_stat)    &
            (ccwin(i), stwin(i),i=1,nwt)
      end if
      !
      shft_rtc2 = statms
    else if (read_stat > 0) then
      call pc_error ('SHFT:  Read error on RTC static file: ', read_stat)
      ntr = fatal_error
    else if (read_stat < 0) then
      call pc_error ('SHFT:  Endfile prematurely reached on RTC file: ',    &
                     read_stat)
      ntr = fatal_error
    end if
    !
  end function shft_rtc2


!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

  subroutine shft_wrapup (obj)
    !
    ! - Arguments
    !
    type(shft_struct) :: obj       ! arguments
    !
    ! - Begin shft_wrapup
    !
    if (obj%skip_wrapup) return
    obj%skip_wrapup = .true.
    !
    if (pc_do_not_process_traces()) return
    !
    ! - wrapup processing
    !
    if (obj%file (1)%use .and. associated (obj%file (1)%statics)) then
      deallocate (obj%file (1)%statics)
    end if
    !
    if (obj%file (2)%use .and. associated (obj%file (2)%statics)) then
      deallocate (obj%file (2)%statics)
    end if
    !
  end subroutine shft_wrapup

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module shft_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!



