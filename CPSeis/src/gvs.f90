!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 2000-08-29. />

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
!                        C P S   P R O C E S S
!
! Name       : GVS    (Graded Velocity Stack)  [Includes former CVST.]
! Category   : stacks
! Written    : 1986-07-28   by: John Sinton
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Apply graded or constant velocity NMO corrections to CMP gathers
!              and (optionally) stack the gathers.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! GVS produces NMO corrected CMP gathers and stacked datasets as diagnostics
! for velocity analysis.  GVS performs the following steps.
!
!        1.  Receive input data in CMP gathers.
!
!        2.  Perform NMO correction using a sequence of velocity fields.
!            The graded velocity field sequence is defined by a reference
!            velocity file and an array of additive or multiplicative
!            velocity modifiers (MODE = GVS).   In MODE = CVST, the velocity
!            field sequence is defined by an array of constant velocities.
!
!        3.  GVS outputs NMO corrected CMP gathers (STACK = NO) or stacks the
!            CMP gathers and outputs stacked traces (STACK = YES).  GVS stacks
!            traces by calling STK internally.
!
!        4.  GVS calls BINSORT internally to put traces into order by panels
!            when STACK = YES.  (There are as many panels as velocity fields
!            in the sequence, with one velocity field for each panel.)
!
! On output traces, GVS sets header word 24 to the panel number and header word
! 63 to the velocity modifier for each panel.   In MODE = CVST, header word 63
! is set to the constant velocity for that panel.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                         ADVICE FOR USERS
!
! PATHNAME_VEL is not active if MODE = CVST.
!
! If you want to use a single velocity function as the reference velocity field
! in GVS mode, then the velocity file specified by PATHNAME_VEL must contain
! only that single desired velocity function.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                       TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
!
! This process requires traces to be input in CMP gathers with header word 7
! set properly.
!
! Process requires traces to be input in CMP sort order.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
!
! This process alters input traces.
!
! If the panels are stacked and sorted (MODE=CVST or MODE=GVS with STACK=YES),
! output traces are not gathered (one trace at a time).
! If they are not stacked and sorted (MODE=GVS with STACK=NO),
! output traces are gathered the same as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                   GLOBAL PARAMETERS USED OR CHANGED
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NWIH     number of words in trace header       used but not changed
! NUMTR    max number of traces input/output     changed
! GATHERED traces are gathered (true) or not     used and possibly changed
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
! GRID     Transform (used by STK)               used iff stacked and sorted
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 1       Sequential Trace Count     Renumbered.
! 2       Set by STK                 Set iff stacked and sorted
!         Minimum head mute time in stack gather
! 3       Set by BINSORT             Set iff stacked and sorted
!         Primary sort bin number of current gather (i.e. panel number)
! 4       Set by BINSORT             Set iff stacked and sorted
!         Sequential trace within current gather (i.e. input gather number)
! 4       Trace number within gather Set if not stacked
! 5       Set by STK                 Set iff stacked and sorted
!         fold of stack
! 7       Mid point X grid           Used iff stacked and sorted
! 24      Panel Number               Set
! 25      Set by STK                 Set iff stacked and sorted
!         LAV of stacked trace
! 26      Velocity modifier          Set if GVS and not stacked
! 27      Panel Number               Set (for backward compatibility)
! 63      Velocity or modifier       Set
! 64      Set by STK                 Set iff stacked and sorted
!         Maximum tail mute time in stack gather
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
!048. 2006-06-20  B. Menger   Removed Unused Variables.
! 47. 2004-05-13 Stoeckley  Add trimmed mean stack capability.
! 46. 2002-04-18 Goodger    Reset TIM_END to global time if global time is
!                           less than TIM_END.
! 45. 2001-08-27 Chiu       Add error checking to TIM_END.
! 44. 2001-02-13 Selzler    Changed wrapup logic to use skip_wrapup
! 43. 2000-11-02 Selzler    Enhanced GUI per user request
! 42. 2000-10-16 Selzler    Corrected bug in front-end VELS rounding.
! 41. 2000-10-05 Selzler    Enhanced front-end to compute VELS sequence.
! 40. 2000-08-30 Selzler    Enhanced velocity function communication
! 39. 2000-06-07 Selzler    Removed NAME_FUNCT (historical artifact)
! 38. 2000-05-11 Selzler    Change from TSORT macro to BINSORT primitive
! 37. 2000-05-10 Selzler    Change OPT_NMO_RES to correspond to old system
! 36. 2000-04-28 Selzler    Conversion to Fortran 90.
! 35. 1999-01-11 Vunderink  Begin using the f90 compiler.
! 34. 1997-11-12 Goodger    Fix problem with header word 63 getting
!                           wiped out.
! 33. 1997-04-24 Goodger    Set header word 6 as well as 63 to the
!                           velocity modifier.
! 32. 1997-03-03 Goodger    Allow times 10 sec and over in the reference
!                           function which is printed to the history.
! 31. 1997-01-28 Vunderink  Fixed problem caused by not all output traces
!                           being initialized to zero.
! 30. 1996-09-17 Goodger    Allow 6 digits for velocity.
! 29. 1996-08-07 Goodger    Enclose reference velocity pairs in paren-
!                           theses for history file.
! 28. 1996-08-01 Goodger    Put reference velocity function in history
!                           file.
! 27. 1995-12-04 Goodger    Set header word 63 to velocity modifier.
! 26. 1990-12-06 Ball       Put in compile directive at DO 200 loop
!                           for CFT77 to speed up GVS.
! 25. 1990-04-06 Howard     Allow VN=NONE for CVST MACRO.
! 24. 1990-04-02 Troutt     Many changes including the following:
!                      a. Eliminate intermediate velocity interpolation
!                         step to 50-pts.  The input function is now
!                         LINEARLY interpolated to the trace sample rate
!                         (DT) as is the case in Process NMC.
!                      b. De-activate INTT as an input parameter, but
!                         leave its usage in code with a value set to 1.
!                         This process formerly did a linear interp. of
!                         each input trace by factor of INTT (def=4).
!                         Now each sample is interpolated linearly
!                         during nmo (same as Process NMC).
!                      c. De-activate SEMBLANC parameter. Process SVS
!                         no longer exists.
!                      d. De-activate XHMIN and XHMAX parameters. Their
!                         usage to limit offsets was a potential for
!                         misunderstanding. Use Process SELECT ahead of
!                         GVS if you need to discard offsets.
!                      e. Add refraction and cross-over mute per NMC
!                         and apply the same taper.
! 23. 1990-03-02 Peterson   Number of panels now stored in header word 24.
!                           Also in 27 for now to be backward compatible.
! 22. 1990-01-31 Troutt     No code changed. Added remarks about scratch
!                           header words (26,27).
! 21. 1989-10-13 Troutt     Eliminated calls to PUTP and GETP because GVS
!                           is NOT re-enterable.
! 20. 1989-05-10 Howard     Add ordinal velocity number for sorting.
! 19. 1989-04-12 Howard     Fix bug in MODE=NMO.
! 18. 1989-04-07 Howard     Save velocity modifier in scratch header 26.
! 17. 1989-04-04 Howard     Fix bug in dopler mute, now matches NMO.
! 16. 1989-03-21 Sinton     Modified dopler muting and use of original
!                           mute value.
! 15. 1989-03-15 Sinton     Fixed problem with header word 2.
! 14. 1989-02-22 Sinton     Changed VMOD defaults in CPS version to work
!                           better with DCODE.
! 13. 1989-01-12 Sinton     Converted to new GETV.
! 12. 1988-09-23 Howard     NWIH and NWPT conversion.
! 11. 1988-08-09 Baumel     Put in CPSPRT calls.
! 10. 1988-07-21 Sinton     Fixed bug with MODE=GVS.
! 9.  1988-05-23 Sinton     Added SEMBLANC parameter to work with the
!                           SVS process.
! 8.  1988-04-20 Sinton     Removed parameters MAXCMP, XB, and YB.
!                           Correcter documentation.
! 7.  1988-03-23 Sinton     Corrected print when MODE=GVS.
! 6.  1987-09-02 Sinton     Added the MODE parameter and made several
!                           corrections.
! 5.  1987-07-10 Sinton     Headers 1-6 will now be consistant with out-
!                           put traces and fixed the VMODTYPE=MULTIPLY
!                           option.
! 4.  1987-05-21 Menger     Fixed GETP call. Now reentrant.
! 3.  1987-05-20 Menger     Fixed PUTP call, added test for enough scr.
! 2.  1986-08-05 Sinton     Corrected for use with BTREG in CFT.
! 1.  1986-07-28 Sinton     First version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                    SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                   SPECIFIC CALLING CHARACTERISTICS
!
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NEED_TRACES    if there more traces are needed
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NEED_TRACES    if there more traces are needed
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!-------------------------------------------------------------------------------
!                  ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                 ALGORITHM DESCRIPTION FOR DEVELOPERS
!

!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                          PROGRAMMING NOTES
!
! Internal call to BINSORT with HDR_PRI = 24
! should appropriately reorder data regardless of mode.
!
! If MODE=CVST or if MODE=GVS and STACK=YES,
! then output NUMTR=1, GATHERED=NO, call STK macro
! and output traces one at a time (NTR = 1).
! Otherwise preserve NUMTR value, GATHERED status (YES), don't call macros
! and output traces in NTR gathers.
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS GVS Process/NC=80>
!
! Apply graded or constant velocity NMO corrections to CMP gathers
!
! MODE=~~~~~~~~`CCCC                              TRIM_PERCENT=`FFFFFFFFFFF
!
! PATHNAME_VEL=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!   OPT_MOD=`CCCC
!
!        `------------------------------------
!         MODS              VELS
!         `FFFFFFFFFFF      `FFFFFFFFFFF
!         `FFFFFFFFFFF      `FFFFFFFFFFF
!         `FFFFFFFFFFF      `FFFFFFFFFFF
!         `FFFFFFFFFFF      `FFFFFFFFFFF
!         `FFFFFFFFFFF      `FFFFFFFFFFF
!         `FFFFFFFFFFF      `FFFFFFFFFFF
!         `FFFFFFFFFFF      `FFFFFFFFFFF
!        `------------------------------------
!
!         FAC1 =`FFFFF      VEL1 =`FFFFFFF     VMIN =`FFFFFFF
!         FINC =`FFFFF      VINC =`FFFFFFF     VINC1=`FFFFFFF
!         NFAC =`IIIII      NVEL =`III         VMAX =`FFFFFFF
!
!  STACK=~~`CCC           DOPPLER=`FFFFFFFFFFF
!
!  TIM_BEG=`FFFFFFFFFFF   TIM_END=`FFFFFFFFFFF
!<PARMS PATHNAME_VEL[/ML=128/XST/YST]>
!<PARMS MODS[/XST/YST]>
!<PARMS VELS[/XST/YST]>
!</gui_def>
!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Whether to use graded or constant velocity sequences. </Tip>
! Default = CVST
! Allowed = CVST  (Use constant velocity sequences.)
! Allowed = GVS   (Use graded velocity sequences.)
!</Help>
!
!<Help KEYWORD="TRIM_PERCENT">
!<Tip> Percent of stack to trim (for trimmed mean stack). </Tip>
! Default = 0.0
! Allowed = 0.0 - 100.0
!
! If TRIM_PERCENT is greater than zero, each stacked sample point in the
! stacked trace will be calculated from the input points using a trimmed
! mean calculation.  A median stack is calculated if TRIM_PERCENT is 100.
!
! Execution time is much slower when TRIM_PERCENT is greater than zero.
!
! This parameter is passed to the internally-called STK process.
!</Help>
!
!<Help KEYWORD="PATHNAME_VEL">
!<Tip> Pathname for the GVS reference velocity file. </Tip>
! Default = NONE
! Allowed = char
!
! If you want to use a single velocity function as the reference velocity
! field, then the velocity file specified by PATHNAME_VEL must contain
! only that single desired velocity function.
!
! PATHNAME_VEL is active only if MODE = GVS.
!</Help>
!
!<Help KEYWORD="OPT_MOD">
!<Tip> Option whether velocity modifiers are additive or multiplicative. </Tip>
! Default = MULT
! Allowed = ADD   (Velocity function modifiers are additive.)
! Allowed = MULT  (Velocity function modifiers are multiplicative.)
!
! OPT_MOD is active only if MODE = GVS.
!</Help>
!
!<Help KEYWORD="MODS">
!<Tip> Array of velocity modifiers. </Tip>
! Default =  -
! Allowed = real array (100)
! If OPT_MOD = ADD, MODS is an array of velocities that add to the reference
! velocity function.
! If OPT_MOD = MULT, MODS is an array of fractions that multiply the reference
! velocity function.
!
! MODS is active only if MODE = GVS.
!</Help>
!
!<Help KEYWORD="VELS">
!<Tip> Array of constant velocities for the CVST mode. </Tip>
! Default =  -
! Allowed = real array (100)
! VELS is an array of constant velocities that constitute the sequence of
! velocity functions for the CVST mode.
!
! VELS is active only if MODE = CVST.
!</Help>
!
!<Help KEYWORD="VEL1">
!<Tip> First VELS entry (linear algorithm). </Tip>
! Default = 1500.0
! Allowed = real > 0.0
!
! VEL1 is active only if MODE = CVST.
! VEL1, VINC and NVEL are front-end only parameters.
!</Help>
!
!<Help KEYWORD="VINC">
!<Tip> Increment for VELS entries (linear algorithm).</Tip>
! Default = 100.0
! Allowed = real > 0.0
!
! VINC is active only if MODE = CVST.
! VEL1, VINC and NVEL are front-end only parameters.
!</Help>
!
!<Help KEYWORD="NVEL">
!<Tip> Total number of VELS entries (linear algorithm).</Tip>
! Default = 0
! Allowed = 0 <= NVEL <= 100
!
! If a value is entered, the VELS table is recomputed (linear):
!     VELS(1) = VEL1
!     VELS(J) = VEL1 + (J-1) * VINC
!         where J = 2 through NVEL.
!
! After the VELS table is recomputed, NVEL is reset to zero.
! NVEL is active only if MODE = CVST.
! VEL1, VINC and NVEL are front-end only parameters.
!</Help>
!
!<Help KEYWORD="VMIN">
!<Tip> First VELS entry (cubic algorithm). </Tip>
! Default = 1500.0
! Allowed = real > 0.0
!
! VMIN is active only if MODE = CVST.
! VMIN, VINC1 and VMAX are front-end only parameters.
!</Help>
!
!<Help KEYWORD="VINC1">
!<Tip> Increment factor for VELS entries (cubic algorithm).</Tip>
! Default = 150.0
! Allowed = real > 0.0
!
! VINC1 is active only if MODE = CVST.
! VMIN, VINC1 and VMAX are front-end only parameters.
!</Help>
!
!<Help KEYWORD="VMAX">
!<Tip> Maximum VELS entry (cubic algorithm).</Tip>
! Default = 0.0
! Allowed = real > 0.0
!
! If a value is entered, the VELS table is recomputed (cubic):
!     VELS(1) = VMIN
!     VELS(J) = VELS(J-1) + VINC1 * (V(J-1) ** 3) / VMIN ** 3
!         where J <= 100 and VELS(J) <= VMAX.
!
! After the VELS table is recomputed, VMAX is reset to zero.
! VMAX is active only if MODE = CVST.
! VMIN, VINC1 and VMAX are front-end only parameters.
!</Help>
!
!<Help KEYWORD="FAC1">
!<Tip> First MODS entry. </Tip>
! Default = 0.80
! Allowed = real > 0.0
!
! If OPT_MOD is MULT, FAC1 should be small (0.75 to 0.95).
! If OPT_MOD is ADD, FAC1 may be large (-700.0, +800.0)
!
! FAC1 is active only if MODE = GVS.
! FAC1, FINC and NFAC are front-end only parameters.
!</Help>
!
!<Help KEYWORD="FINC">
!<Tip> Increment for MODS entries.</Tip>
! Default = 0.02
! Allowed = real
!
! If OPT_MOD is MULT, FINC should be small (0.01 to 0.05).
! If OPT_MOD is ADD, FINC may be large (+100.0, +800.0)
!
! FINC is active only if MODE = GVS.
! FAC1, FINC and NFAC are front-end only parameters.
!</Help>
!
!<Help KEYWORD="NFAC">
!<Tip> Total number of MODS entries.</Tip>
! Default = 0
! Allowed = 0 <= NFAC <= 100
!
! If a value is entered, the MODS table is recomputed:
!     MODS(1) = FAC1
!     MODS(J) = FAC1 + (J-1) * FINC
!         where J = 2 through NFAC.
!
! After the MODS table is recomputed, NFAC is reset to zero.
! NFAC is active only if MODE = GVS.
! FAC1, FINC and NFAC are front-end only parameters.
!</Help>
!
!<Help KEYWORD="STACK">
!<Tip> Whether to output stacked data (YES) or NMO corrected CMPs (NO). </Tip>
! Default = YES
! Allowed = YES   (Output stacked and sorted data.)
! Allowed = NO    (Output NMO corrected CMPs.)
!
! If MODE = CVST then data is stacked and sorted (implies STACK=YES).
!</Help>
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Starting time for the output traces. </Tip>
! Default = TSTRT
! Allowed = real
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> Ending time for the output traces. </Tip>
! Default = end of trace
! Allowed = real > TSTRT
!</Help>
!
!<Help KEYWORD="DOPPLER">
!<Tip> Doppler mute (stretch mute) parameter. </Tip>
! Default = 1.7
! Allowed = real>1.0
! DOPPLER is the maximum NMO stretch factor allowed by the stretch mute.
! Larger values of DOPPLER correspond to less severe muting.  DOPPLER = 0.0
! disables the stretch mute, but allows automatic muting to eliminate crossing
! events and refractions.  DOPPLER < 0.0 disables all automatic muting in NMC.
!</Help>
!


!</HelpSection>
!-------------------------------------------------------------------------------
!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module gvs_module
      use pc_module
      use named_constants_module
      use pathcheck_module
      use nmo_module
      use stk_module
      use binsort_module
      implicit none
      private
      public :: gvs_create     ! uses the parameter cache.
      public :: gvs_initialize
      public :: gvs_update     ! uses the parameter cache.
      public :: gvs_delete

!<execute_only>

      public :: gvs            ! main execution (trace processing) routine.
      public :: gvs_wrapup

!</execute_only>

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      INTEGER, PARAMETER :: NVELF = 100

      ! This odd beast is required to create an array of nmo_struct pointers.
      ! One structure is required for each nmo macro process (each panel).
      type,private :: nmo_struct_ptr
        type(nmo_struct), pointer   :: nmo ! dependent parameter.
      end type nmo_struct_ptr

      type,public :: gvs_struct
      private
        logical                    :: skip_wrapup      ! dependent parameter

        character(len=4)           :: mode             ! process parameter
        real                       :: trim_percent     ! process parameter
        character(len=FILENAME_LENGTH) :: pathname_vel ! process parameter
        character(len=4)           :: opt_mod          ! process parameter
        real,dimension(NVELF)      :: mods             ! process parameter
        real,dimension(NVELF)      :: vels             ! process parameter
        logical                    :: stack            ! process parameter
        real                       :: tim_beg          ! process parameter
        real                       :: tim_end          ! process parameter
        real                       :: doppler          ! process parameter

        real                       :: vel1             ! front-end parameter
        real                       :: vinc             ! front-end parameter
        integer                    :: nvel             ! front-end parameter
        real                       :: vmin             ! front-end parameter
        real                       :: vinc1            ! front-end parameter
        real                       :: vmax             ! front-end parameter
        real                       :: fac1             ! front-end parameter
        real                       :: finc             ! front-end parameter
        integer                    :: nfac             ! front-end parameter

        integer                    :: nwih             ! global parameter
        integer                    :: ndpt             ! global parameter
        real                       :: dt               ! global parameter
        real                       :: tstrt            ! global parameter
        integer                    :: numtr            ! global parameter
        logical                    :: gathered         ! global parameter
        integer                    :: numtr_out        ! global parameter
        logical                    :: gathered_out     ! global parameter

        integer                    :: mods_cnt         ! dependent variable
        integer                    :: vels_cnt         ! dependent variable

        integer                    :: panel_num        ! dependent variable
        integer                    :: panel_cnt        ! dependent variable
        integer                    :: cmp_in           ! dependent variable
        integer                    :: tr_in            ! dependent variable
        integer                    :: tr_out           ! dependent variable
        logical                    :: more_input       ! dependent variable

        logical                    :: all_done         ! dependent variable

        real                       :: min_basement     ! dependent variable
        real                       :: max_basement     ! dependent variable
        real                       :: min_offset       ! dependent variable
        real                       :: max_offset       ! dependent variable

        integer                    :: ntr_save         ! dependent variable

        real, dimension(:,:), pointer :: tr_save        ! dependent variable
        double precision,dimension(:,:),pointer :: hd_save ! dependent variable

        type(nmo_struct_ptr), dimension(NVELF):: nmo_ptr ! dependent parameter.
        type(stk_struct),pointer   :: stk               ! dependent parameter.
        type(binsort_struct),pointer :: binsort         ! dependent parameter.
      end type gvs_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(gvs_struct),pointer,save :: object      ! needed for traps.

      character(len=100),public :: gvs_ident = &
        "$Id: gvs.f90,v 1.48 2006/06/20 13:11:54 Menger prod sps $"

      real :: global_tim_end

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine gvs_create (obj)
      implicit none
      type(gvs_struct),pointer :: obj       ! arguments
      integer :: vels_do

      allocate (obj)

      nullify (obj%tr_save)
      nullify (obj%hd_save)

      do vels_do=1,NVELF
        nullify (obj%nmo_ptr(vels_do)%nmo)
      end do

      nullify (obj%stk)
      nullify (obj%binsort)

      call gvs_initialize (obj)

      return
      end subroutine gvs_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine gvs_delete (obj)
      implicit none
      type(gvs_struct),pointer :: obj       ! arguments
      integer :: vels_do

!<execute_only>
      call gvs_wrapup (obj)
!</execute_only>

      if (associated(obj%tr_save ))    deallocate        (obj%tr_save)
      if (associated(obj%hd_save ))    deallocate        (obj%hd_save)

      do vels_do = 1, NVELF
        if (associated(obj%nmo_ptr(vels_do)%nmo )) &
          call nmo_delete(obj%nmo_ptr(vels_do)%nmo)
      end do

      if (associated(obj%stk ))        call stk_delete(obj%stk)
      if (associated(obj%binsort ))    call binsort_close(obj%binsort)

      deallocate(obj)

      return
      end subroutine gvs_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine gvs_initialize (obj)
      implicit none
      type(gvs_struct),pointer :: obj       ! arguments

      call pc_get_global('ndpt', obj%ndpt)
      call pc_get_global('dt', obj%dt)
      call pc_get_global('tstrt', obj%tstrt)

      obj%mode = 'CVST'
      obj%trim_percent = 0.0
      obj%pathname_vel = PATHCHECK_EMPTY
      obj%opt_mod = 'MULT'
      obj%mods = 0.0
      obj%mods(1) = 0.88
      obj%mods(2) = 0.90
      obj%mods(3) = 0.92
      obj%mods(4) = 0.94
      obj%mods(5) = 0.96
      obj%mods(6) = 0.98
      obj%mods(7) = 1.00
      obj%mods(8) = 1.02
      obj%mods(9) = 1.04
      obj%mods(10) = 1.06
      obj%mods(11) = 1.08
      obj%mods(12) = 1.10
      obj%mods(13) = 1.12

      obj%mods_cnt = 13

      obj%vels = 0.0
      obj%vels(1) = 8000.0
      obj%vels_cnt = 1

      obj%stack = .true.
      obj%tim_beg = obj%tstrt
      obj%tim_end = obj%tstrt + (obj%ndpt - 1) * obj%dt
      global_tim_end=obj%tim_end

      obj%doppler = 1.7

      obj%vel1 = 1500.0
      obj%vinc = 100.0
      obj%nvel = 0
      obj%vmin = 1500.0
      obj%vinc1= 150.0
      obj%vmax = 0.0
      obj%fac1 = 0.80
      obj%finc = 0.02
      obj%nfac = 0

      obj%numtr = 0
      obj%numtr_out = 0

      obj%panel_num = 0
      obj%panel_cnt = obj%mods_cnt
      obj%cmp_in = 0
      obj%tr_in = 0
      obj%tr_out = 0
      obj%more_input = .true.
      obj%all_done = .false.

      obj%min_basement = +HUGE(obj%min_basement)
      obj%max_basement = -HUGE(obj%max_basement)
      obj%min_offset = +HUGE(obj%min_offset)
      obj%max_offset = -HUGE(obj%max_offset)

      obj%ntr_save = 0

      call gvs_update (obj)

      return
      end subroutine gvs_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine gvs_update (obj)
      implicit none
      type(gvs_struct),target :: obj                           ! arguments

      integer :: ier1, ier2        ! local
      integer :: mods_do, vels_do  ! local
      integer :: nstore  ! local
      integer status
      real :: mods_min, mods_max
      real :: alpha
      logical :: verify_nmo
      logical :: gathered
      integer :: state

      object => obj         ! needed for traps.
      obj%skip_wrapup = .true.

      state = pc_get_update_state()

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_get_global('nwih', obj%nwih)
      call pc_get_global('ndpt', obj%ndpt)
      call pc_get_global('dt', obj%dt)
      call pc_get_global('tstrt', obj%tstrt)
      call pc_get_global('numtr', obj%numtr)
      call pc_get_global('gathered', gathered)

      call pc_get('mode', obj%mode)
      call pc_get('trim_percent', obj%trim_percent)
      call string_to_upper(obj%mode)
      call pc_get('pathname_vel', obj%pathname_vel)
      call pc_get('opt_mod', obj%opt_mod)
      call string_to_upper(obj%opt_mod)
      call pc_get('mods', obj%mods, obj%mods_cnt)
      call pc_get('vels', obj%vels, obj%vels_cnt)
      call pc_get('stack', obj%stack)
      call pc_get('tim_beg', obj%tim_beg)
      call pc_get('tim_end', obj%tim_end)
      if(obj%tim_end.gt.global_tim_end)obj%tim_end=global_tim_end
      call pc_get('doppler', obj%doppler)

      call pc_get('vel1', obj%vel1)
      call pc_get('vinc', obj%vinc)
      call pc_get('nvel', obj%nvel)
      call pc_get('vmin', obj%vmin)
      call pc_get('vinc1', obj%vinc1)
      call pc_get('vmax', obj%vmax)
      call pc_get('fac1', obj%fac1)
      call pc_get('finc', obj%finc)
      call pc_get('nfac', obj%nfac)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if(.not.gathered) then
        call pc_error('this process must be preceded by a gather')
      end if

      if ( obj%tim_end <= 0.0) then
         call pc_error('TIM_END must be greater than 0.0') 
      end if 

      if ( obj%tim_end < obj%tim_beg) then
         call pc_error('TIM_END must be greater than or equal TIM_BEG') 
      end if

      verify_nmo = .true.

      verify_mode: &
      if(obj%mode(1:1) == 'G') then
        obj%mode = 'GVS'

        if(state == PC_FRONTEND .or. state == PC_BACKEND .or. &
          pc_verify_scalar('pathname_vel')) then
          call pathcheck('pathname_vel', obj%pathname_vel, '.vel', &
            required=.false., status=status)

          if(status /= PATHCHECK_VALID) then
            call pc_error('valid PATHNAME_VEL is required for velocity info')
            verify_nmo = .false.
          end if
        end if

        if(obj%opt_mod(1:1) == 'A') then
          obj%opt_mod = 'ADD'
        else if(obj%opt_mod(1:1) == 'M') then
          obj%opt_mod = 'MULT'
        else
          call pc_error('OPT_MOD must be either ADD or MULT')
          obj%opt_mod = 'MULT'
        end if

        if(obj%nfac /= 0) then
          if(obj%opt_mod(1:1) == 'A') then
            if(obj%finc <= 0.0) then
              call pc_error('FINC must be greater than 0.0')
              obj%finc = 200.0
              obj%nfac = 0
            end if
          else
            if(obj%fac1 <= 0.0) then
              call pc_error('FAC1 must be greater than 0.0')
              obj%fac1 = 0.80
              obj%nfac = 0
            end if

            if(obj%finc <= 0.0) then
              call pc_error('FINC must be greater than 0.0')
              obj%finc = 0.02
              obj%nfac = 0
            end if
          end if

          if(obj%nfac < 0) then
            call pc_error('NFAC must be greater than or equal to 0')
          else if(obj%nfac > NVELF) then
            call pc_error('NFAC must be less than or equal to ', NVELF)
          else if(obj%nfac > 0) then
            obj%mods(1) = obj%fac1

            do mods_do = 2, obj%nfac
              obj%mods(mods_do) = obj%fac1 + (mods_do - 1) * obj%finc
            end do

            obj%mods_cnt = obj%nfac
          end if
        end if

        if(state == PC_FRONTEND .or. state == PC_BACKEND .or. &
          obj%nfac /= 0 .or. &
          pc_verify_array('mods')) then
          verify_mods: &
          if(obj%mods_cnt < 1) then
            call pc_error('MODS at least one velocity modifier is required')
            verify_nmo = .false.
          else ! verify_mods
            if(obj%opt_mod == 'MULT') then
              mods_min = 0.10
              mods_max = 5.00
            else
              mods_min = -3000.
              mods_max = +50000.
            end if

            do mods_do = 1, obj%mods_cnt
              if(obj%mods(mods_do) < mods_min .or. &
                 obj%mods(mods_do) > mods_max) then
                call pc_error('MODS value(s) exceed rational limits')
                verify_nmo = .false.
                exit
              end if
            end do

            obj%panel_cnt = obj%mods_cnt
          end if verify_mods
        end if

        obj%nfac = 0
      else if(obj%mode(1:1) == 'C') then ! verify_mode
        obj%mode = 'CVST'

        if(obj%nvel /= 0) then
          if(obj%vel1 <= 0.0) then
            call pc_error('Invalid VEL1 value.  Must be > 0.0')
            obj%vel1 = 1500.0
            obj%nvel = 0
          end if

          if(obj%vinc <= 0.0) then
            call pc_error('Invalid VINC value.  Must be > 0.0')
            obj%vinc1 = 150.0
            obj%nvel = 0
          end if

          if(obj%nvel < 0) then
            call pc_error('NVEL must be greater than or equal to 0')
          else if(obj%nvel > NVELF) then
            call pc_error('NVEL must be less than or equal to ', NVELF)
          else if(obj%nvel > 0) then
            obj%vels(1) = obj%vel1

            do vels_do = 2, obj%nvel
              obj%vels(vels_do) = obj%vel1 + (vels_do - 1) * obj%vinc
            end do

            obj%vels_cnt = obj%nvel
          end if
        else if(obj%vmax /= 0.0) then
          if(obj%vmin <= 0.0) then
            call pc_error('Invalid VMIN value.  Must be > 0.0')
            obj%vmin = 1500.0
            obj%vmax = 0.0
          end if

          if(obj%vinc1 <= 0.0) then
            call pc_error('Invalid VINC1 value.  Must be > 0.0')
            obj%vinc1 = 150.0
            obj%vmax = 0
          end if

          if(obj%vmax < 0) then
            call pc_error('VMAX must be greater than or equal to VMIN')
          else if(obj%vmax > 0) then
            obj%vels(1) = obj%vmin

            vels_do = 1
            alpha = obj%vinc1 / obj%vmin ** 3

            do while(vels_do < NVELF .and. &
              obj%vels(vels_do) < obj%vmax)
              vels_do = vels_do + 1
              obj%vels(vels_do) = obj%vels(vels_do - 1) + &
                alpha * obj%vels(vels_do - 1) ** 3
            end do

            if(obj%vels(vels_do) > obj%vmax) then
              obj%vels(vels_do) = obj%vmax
            end if

            obj%vels_cnt = vels_do

            do vels_do = 1, obj%vels_cnt
              obj%vels(vels_do) = nint(obj%vels(vels_do))
            end do
          end if
        end if

        if(state == PC_FRONTEND .or. state == PC_BACKEND .or. &
          obj%vmax /= 0.0 .or. obj%nvel /= 0 .or. &
          pc_verify_array('vels')) then
          verify_vels: &
          if(obj%vels_cnt < 1) then
            call pc_error('VELS at least one velocity is required')
            verify_nmo = .false.
          else ! verify_vels
            do vels_do = 1, obj%vels_cnt
              if(obj%vels(vels_do) < 400.0 .or. &
                 obj%vels(vels_do) > 50000.0) then
                call pc_error('VELS value(s) exceed rational limits')
                verify_nmo = .false.
                exit
              end if
            end do

            obj%panel_cnt = obj%vels_cnt
          end if verify_vels
        end if

        obj%vmax = 0.0
        obj%nvel = 0
      else ! verify_mode
        call pc_error('MODE must be either GVS or CVST')
        obj%mode = 'CVST'
      end if verify_mode

      if(obj%stack .or. obj%mode == 'CVST') then
        obj%numtr_out = 1
        obj%gathered_out = .false.
      else
        obj%numtr_out = obj%numtr
        obj%gathered_out = .true.
      end if

      call mth_constrain (obj%trim_percent, 0.0, 100.0)

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

      if(state == PC_FRONTEND .or. state == PC_BACKEND .or. verify_nmo) then
        ! Create one NMO macro call for each velocity panel
        nmo_setup: &
        do vels_do = 1, NVELF
          ! Examine all nmo array entries (used and not used).
          if(vels_do <= obj%panel_cnt) then
            ! This nmo_prt entry is actually used.
            call pc_clear()

            if(obj%mode == 'GVS') then
              call pc_put('pathname', obj%pathname_vel)

              if(obj%opt_mod == 'MULT') then
                call pc_put_process('vel_bias', 0.0)
                call pc_put_process('vel_scale', obj%mods(vels_do))
              else
                call pc_put_process('vel_bias', obj%mods(vels_do))
                call pc_put_process('vel_scale', 1.0)
              end if
            else
              call pc_put('pathname', PATHCHECK_EMPTY)
              call pc_put_process('vel_bias', obj%vels(vels_do))
              call pc_put_process('vel_scale', 1.0)
            end if

            call pc_put('opt_nmo_res', 'NONE')

            ! Explicit default definition of all parameters.
            call pc_put('opt_nmo', 'FORWARD')
            call pc_put('opt_interp', 'LINEAR')
            call pc_put('order_nmo', 2)
            call pc_put('opt_demult', .false.)

!     These parameters are not used by NMO when opt_demult is .false.
!           call pc_put('off_min', 0.0)
!           call pc_put('off_max', 5000.0)
!           call pc_put('num_off', 120)
!           call pc_put('tim_res', 0.35)
!           call pc_put('vel_min', 1200.)
!           call pc_put('vel_mute', 1500.)
!           call pc_put('tim_add', 0.0)
!           call pc_put('off_mute', 0.0)
!           call pc_put('ratio', 0.7)
!           call pc_put('freq_mean', 20.0)

            call pc_put('tim_beg', obj%tim_beg)
            call pc_put('tim_end', obj%tim_end)
            call pc_put('doppler', obj%doppler)

            if (associated(obj%nmo_ptr(vels_do)%nmo)) then
              call nmo_update  (obj%nmo_ptr(vels_do)%nmo)
            else
              call nmo_create (obj%nmo_ptr(vels_do)%nmo)
            end if

!     Assume the time inputs are correct
!           call pc_get('tim_beg', obj%tim_beg)
!           call pc_get('tim_end', obj%tim_end)

            call pc_get('doppler', obj%doppler)

            call pc_restore()
          else
            ! This nmo_prt entry is not used.
            if (associated(obj%nmo_ptr(vels_do)%nmo)) then
              call nmo_delete  (obj%nmo_ptr(vels_do)%nmo)
              nullify  (obj%nmo_ptr(vels_do)%nmo)
            end if
          end if
        end do nmo_setup
      end if

      if(state == PC_FRONTEND .or. state == PC_BACKEND .or. &
        pc_verify_scalar('stack') .or. &
        pc_verify_scalar('mode')) then

        stk_setup: &
        if(obj%stack .or. obj%mode == 'CVST') then
          ! Massage STK macro call.
          call pc_clear()

          call pc_put('opt_input', 'GATHER')

          ! Explicit default definition of all parameters.
          call pc_put('hdr_flag', 0)
          call pc_put('fse', 0.5)
          call pc_put('mscl', 0.25)
          call pc_put('tvfse', 0.0)
          call pc_put('opt_out', 'TRACE')
          call pc_put('trim_percent', obj%trim_percent)

          if (associated(obj%stk)) then
            call stk_update  (obj%stk)
          else
            call stk_create (obj%stk)
          end if

          call pc_restore()
        else ! stk_setup
          ! Remove STK from the processing sequence.
          if (associated(obj%stk)) then
            call stk_delete  (obj%stk)
            nullify (obj%stk)
          end if
        end if stk_setup
      end if

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field('mode', (/ "CVST", "GVS " /), 2)
      call pc_put('mode', obj%mode)
      call pc_put('trim_percent', obj%trim_percent)

      call pc_put('pathname_vel', obj%pathname_vel)

      call pc_put_options_field('opt_mod', (/ "ADD ", "MULT" /), 2)
      call pc_put('opt_mod', obj%opt_mod)

      call pc_put('mods', obj%mods, obj%mods_cnt)
      call pc_put('vels', obj%vels, obj%vels_cnt)

      call pc_put_options_field('stack', (/ "YES", "NO " /), 2)
      call pc_put('stack', obj%stack)

      call pc_put('tim_beg', obj%tim_beg)
      call pc_put('tim_end', obj%tim_end)
      call pc_put('doppler', obj%doppler)

      call pc_put('vel1', obj%vel1)
      call pc_put('vinc', obj%vinc)
      call pc_put('nvel', obj%nvel)
      call pc_put('vmin', obj%vmin)
      call pc_put('vinc1', obj%vinc1)
      call pc_put('vmax', obj%vmax)
      call pc_put('fac1', obj%fac1)
      call pc_put('finc', obj%finc)
      call pc_put('nfac', obj%nfac)

      if(obj%mode == 'GVS') then
        call pc_put_sensitive_field_flag('pathname_vel', .true.)
        call pc_put_sensitive_field_flag('opt_mod', .true.)
        call pc_put_sensitive_field_flag('mods', .true.)
        call pc_put_sensitive_field_flag('stack', .true.)

        call pc_put_sensitive_field_flag('vels', .false.)

        call pc_put_sensitive_field_flag('vel1', .false.)
        call pc_put_sensitive_field_flag('vinc', .false.)
        call pc_put_sensitive_field_flag('nvel', .false.)
        call pc_put_sensitive_field_flag('vmin', .false.)
        call pc_put_sensitive_field_flag('vinc1', .false.)
        call pc_put_sensitive_field_flag('vmax', .false.)
        call pc_put_sensitive_field_flag('fac1', .true.)
        call pc_put_sensitive_field_flag('finc', .true.)
        call pc_put_sensitive_field_flag('nfac', .true.)
      else
        call pc_put_sensitive_field_flag('pathname_vel', .false.)
        call pc_put_sensitive_field_flag('opt_mod', .false.)
        call pc_put_sensitive_field_flag('mods', .false.)
        call pc_put_sensitive_field_flag('stack', .false.)

        call pc_put_sensitive_field_flag('vels', .true.)

        call pc_put_sensitive_field_flag('vel1', .true.)
        call pc_put_sensitive_field_flag('vinc', .true.)
        call pc_put_sensitive_field_flag('nvel', .true.)
        call pc_put_sensitive_field_flag('vmin', .true.)
        call pc_put_sensitive_field_flag('vinc1', .true.)
        call pc_put_sensitive_field_flag('vmax', .true.)
        call pc_put_sensitive_field_flag('fac1', .false.)
        call pc_put_sensitive_field_flag('finc', .false.)
        call pc_put_sensitive_field_flag('nfac', .false.)
      end if

      call pc_put_global('gathered', obj%gathered_out)

      call pc_put_global('numtr', obj%numtr_out)

      if(obj%mode == 'CVST') then
        call pc_put_global('modifier_vel', 'CONSTANT')
      else
        if(obj%opt_mod == 'MULT') then
          call pc_put_global('modifier_vel', 'MULTIPLICATIVE')
        else
          call pc_put_global('modifier_vel', 'ADDITIVE')
        end if

        call pc_put_global('path_vel', obj%pathname_vel)
      end if

      call pc_put_control ('need_label'  , .true.)
      call pc_put_control ('need_request'  , .true.)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      allocate(obj%hd_save(obj%nwih,obj%numtr),stat=ier1)
      allocate(obj%tr_save(obj%ndpt,obj%numtr),stat=ier2)

      nstore = (2 * obj%nwih + obj%ndpt) * obj%numtr

      if(ier1 /= 0 .or. ier2 /= 0) then
        call pc_error('memory allocation failed')
        return
      end if

      call binsort_open(obj%binsort, obj%panel_cnt, 'gvsBinsort', &
        obj%nwih, obj%ndpt, pc_get_lun(), ier1)

      if(ier1 /= BINSORT_OK) then
        call pc_error('Error returned from BINSORT')
      end if

      call pc_print('tim_beg', obj%tim_beg, 'MIN OUTPUT TIME')
      call pc_print('tim_end', obj%tim_end, 'MAX OUTPUT TIME')
      call pc_print('doppler', obj%doppler, 'DOPPLER MUTE')
      call pc_print('numtr', obj%numtr, '# OF OFFSETS')

      if(obj%mode == 'GVS') then
        call pc_print('opt_mod ' // trim(obj%opt_mod) // &
          ' TYPE OF VEL MODIFIER')
        call pc_print('mods_cnt', obj%mods_cnt, '# OF VEL MODIFIER')
      else
        ! mode == 'CVST'
        call pc_print('vels_cnt', obj%vels_cnt, '# number of constant vels')
      end if

      call pc_put_control ('NSTORE', nstore)

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine gvs_update

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine gvs (obj,ntr,hd,tr)
      implicit none
      type(gvs_struct)               :: obj                    ! arguments
      integer         ,intent(inout)  :: ntr                   ! arguments
      double precision,intent(inout)  :: hd(:,:)               ! arguments
      real            ,intent(inout)  :: tr(:,:)               ! arguments

      integer ntr_do
      REAL :: mute_idx
      real :: mid_pt_inline_grid_coord, simple_offset, simple_offset_squared

      if(ntr > 0) then
        obj%cmp_in = obj%cmp_in + 1
        obj%tr_in = obj%tr_in + ntr

        ! Scan input traces for minimum and maximum spatial coordinates.
        do ntr_do = 1, ntr
          mute_idx = hd(2,ntr_do)
          mid_pt_inline_grid_coord = hd(7,ntr_do)

          simple_offset = hd(6,ntr_do)
          simple_offset_squared = simple_offset*simple_offset

          obj%min_basement = AMIN1(obj%min_basement,mid_pt_inline_grid_coord)
          obj%max_basement = AMAX1(obj%max_basement,mid_pt_inline_grid_coord)
          obj%min_offset = AMIN1(obj%min_offset,simple_offset)
          obj%max_offset = AMAX1(obj%max_offset,simple_offset)
        end do
      end if

      mode_choice: &
      if(obj%stack .or. obj%mode == 'CVST') then
        ! Nmo, stack and sort to create velocity panels.
        phase_choice: &
        if(obj%more_input) then
          ! Consume all input CMP gathers before any are output.
          if(ntr > 0 .and. obj%panel_cnt > 1) then
            ! Save input cmp for subsequent processing in panel loop.
            ! Note: starting a new panel, i.e. obj%panel_num = 0.
            obj%ntr_save = ntr
            obj%hd_save(:obj%nwih,:ntr) = hd(:obj%nwih,:ntr)
            obj%tr_save(:obj%ndpt,:ntr) = tr(:obj%nwih,:ntr)
          else if(ntr == NEED_TRACES .and. obj%panel_num == 0) then
            ! Gvs also needs more traces to continue processing.
            ! Note: starting a new panel, i.e. obj%panel_num = 0.
            return
          else if(ntr == NO_MORE_TRACES) then
            ! Input has dried up, transition to output only mode.
            ! Note: starting a new panel, i.e. obj%panel_num = 0.
            obj%more_input = .false.
          end if

          nmo_stk_binsort_loop: &
          do while(obj%panel_num < obj%panel_cnt)
            obj%panel_num = obj%panel_num + 1
 
            if(obj%ntr_save > 0 .and. obj%panel_num > 1) then
              ! Restore input cmp for subsequent processing.
              ntr = obj%ntr_save
              hd(:obj%nwih,:ntr) = obj%hd_save(:obj%nwih,:ntr)
              tr(:obj%ndpt,:ntr) = obj%tr_save(:obj%ndpt,:ntr)
            end if
 
            call nmo(obj%nmo_ptr(obj%panel_num)%nmo, ntr, hd, tr)
            if(ntr == FATAL_ERROR) return
 
            call stk(obj%stk, ntr, hd, tr)
            if(ntr == FATAL_ERROR) return
 
            ! Note: stk returns only one trace for binsort.
 
            if(obj%mode == 'GVS') then
              hd(6,1) = obj%mods(obj%panel_num)
              hd(63,1) = obj%mods(obj%panel_num)
            else
              hd(6,1) = obj%vels(obj%panel_num)
              hd(63,1) = obj%vels(obj%panel_num)
            end if
 
            hd(24,1) = obj%panel_num
 
            call binsort(obj%binsort, ntr, hd, tr)
            if(ntr == FATAL_ERROR) return
 
            if(ntr == 1) then
              ! binsort was called with ntr = NO_MORE_TRACES.
              exit nmo_stk_binsort_loop
            else if(ntr == NO_MORE_TRACES) then
              ! The previous process did not supply ANY CMPs.
              obj%all_done = .true.
              return
            end if
 
            ! At this point ntr must be NEED_TRACES.
          end do nmo_stk_binsort_loop

          if(obj%panel_num == obj%panel_cnt) then
            obj%panel_num = 0
          end if

          if(ntr == NEED_TRACES) then
            return
          end if
        else if(ntr == NEED_TRACES) then ! phase_choice
          ! Drain all traces accumulated by binsort.
          call binsort(obj%binsort, ntr, hd, tr)
          if(ntr == FATAL_ERROR) return
        end if phase_choice

        if(ntr == 1) then
          obj%tr_out = obj%tr_out + 1
        else if(ntr == NO_MORE_TRACES) then
          ! Normal completion.
          obj%all_done = .true.
        end if
      else  ! mode_choice
        ! Nmo only to create velocity panels.
        ! Consume CMPs one at a time and output velocity panels
        ! before the next input CMP is consumed.
        if(ntr > 0 .and. obj%panel_cnt > 1) then
          ! Save input cmp for subsequent processing in panel loop.
          ! Note: starting a new panel, i.e. obj%panel_num = 0.
          obj%ntr_save = ntr
          obj%hd_save(:obj%nwih,:ntr) = hd(:obj%nwih,:ntr)
          obj%tr_save(:obj%ndpt,:ntr) = tr(:obj%nwih,:ntr)
        else if(ntr == NEED_TRACES .and. obj%panel_num == 0) then
          ! Gvs also needs more traces to continue processing.
          ! Note: starting a new panel, i.e. obj%panel_num = 0.
          return
        else if(ntr == NO_MORE_TRACES) then
          ! Normal completion.
          obj%all_done = .true.
          return
        end if

        obj%panel_num = obj%panel_num + 1

        if(obj%ntr_save > 0 .and. obj%panel_num > 1) then
          ! Restore input cmp for subsequent processing.
          ntr = obj%ntr_save
          hd(:obj%nwih,:ntr) = obj%hd_save(:obj%nwih,:ntr)
          tr(:obj%ndpt,:ntr) = obj%tr_save(:obj%ndpt,:ntr)
        end if
 
        call nmo(obj%nmo_ptr(obj%panel_num)%nmo, ntr, hd, tr)
        if(ntr == FATAL_ERROR) return

        do ntr_do = 1, ntr
          obj%tr_out = obj%tr_out + 1

          hd(4,ntr_do) = ntr_do

          hd(26,ntr_do) = obj%mods(obj%panel_num)
          hd(63,ntr_do) = obj%mods(obj%panel_num)
          hd(6,ntr_do) = obj%mods(obj%panel_num)
          hd(24,ntr_do) = obj%panel_num
        end do

        if(obj%panel_num == obj%panel_cnt) then
          obj%panel_num = 0
        end if
      end if mode_choice

      return
      end subroutine gvs

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine gvs_wrapup (obj)
      implicit none
      type(gvs_struct) :: obj       ! arguments
      integer :: panel_do

      if(obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      if(obj%all_done) then
        call pc_print('GVS: Processing completed normally.')
      else
        call pc_error('GVS: abnormal termination')
      end if
  
      call pc_print('  Input data basement-offset limits.')
      call pc_print('    min basement= ', obj%min_basement)
      call pc_print('    max basement= ', obj%max_basement)
      call pc_print('    min offset= ', obj%min_offset)
      call pc_print('    max offset= ', obj%max_offset)
      call pc_print('  Input  trace count= ', obj%tr_in)
      call pc_print('  Input  CMP   count= ', obj%cmp_in)
      call pc_print('  Output trace count= ', obj%tr_out)
      call pc_print('GVS: End of GVS listing')
 
      do panel_do = 1, obj%panel_cnt
        if(associated(obj%nmo_ptr(panel_do)%nmo)) &
          call nmo_wrapup(obj%nmo_ptr(panel_do)%nmo)
      end do

      if(associated(obj%stk))   call stk_wrapup(obj%stk)

      return
      end subroutine gvs_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module gvs_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
