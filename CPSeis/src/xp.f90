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
!                         C P S    P R O C E S S
!
! Process name: XP   (eXPand amplitudes)
! Category   : Amplitude_Mod
! Written    : 1986-07-01  by: Bob Baumel and Richard Day
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : XP rescales trace samples to balance amplitudes.
! Portability: No known limitations.
! Parallel   : Yes
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! XP is a single trace process that balances trace amplitudes and is the CPS
! version of industry standard AGC.  It is a two-step process, amplitude
! balance followed by an optional debrighten.  XP is normally used in
! structural processing. It does not preserve amplitudes.
!
! This process calls the XPUTIL primitive to do all the work.  See the XPUTIL
! primitive for further information.
! 
! Amplitude Balance:
!
!  XP calculates a gain function by first calculating the L1 NORM (mean
!  absolute value) of the non-zero trace samples in each window.
!  The gain function at the center of each window is the reciprocal of that
!  window's mean and is linearly interpolated between window centers. (The gain
!  function is constant above the first window center and below the last window
!  center.)
!
!  XP reduces the trace amplitude variation on a time-scale greater than the
!  window length, while allowing some amplitude variation on a time-scale
!  smaller than the window length.
!
! Debrighten:
!
!  If you set DEBRI=YES, then debrightening is performed AFTER the XP amplitude
!  balance.  The choice of debrightening threshold (DEBRI_MAX) does not depend
!  on the absolute scale of the input data, as the amplitude balance attempts to
!  scale the trace so that the average absolute value in each window is 1.
!
!  Debrightening is performed as follows:  If any absolute values in the trace
!  exceed the DEBRI_MAX threshold, then the largest peak is reduced to
!  DEBRI_MAX, tapering the amount of reduction for distance DEBRI_TPR to either
!  side of this peak.  This process is repeated until no values exceed
!  DEBRI_MAX.
!
! Killing Traces:
!
!  If no window has at least 10 samples, XP will kill that trace.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! Window Length and Location:
!
!  If WINDOWS is "REGULAR" (instead of "CUSTOM") then a single constant window
!  size is specified by WIN_LEN and WIN_INC.
!
!  WIN_LEN is normally 0.5 sec, although 1.0 sec or more is sometimes used to
!  allow more amplitude variation.  WIN_INC normally equals WIN_LEN, although
!  setting WIN_INC smaller than WIN_LEN should produce a smoother gain function
!  Some window overlap is recommended when using unusually LONG windows, so that
!  you still get a reasonable number of windows in the trace, and obtain a
!  reasonably smooth gain function.
!
!  The START_VAL parameter is useful if you want to apply XP to data that hasn't
!  been muted.  Normally (with START_VAL=0), the first window starts at the mute
!  time.  But if you set START_VAL non-zero, the first window will start at the
!  first trace value whose absolute value exceeds START_VAL * maximum trace
!  amplitude.
!
!  TIM_LAST should be set earlier than end of trace if there are deep events
!  that shouldn't contribute to the gain.  The last window will have its
!  BOTTOM at time TIM_LAST or (HW64-1)*DT, whichever value is smaller, but its
!  length will still be WIN_LEN (i.e. the window increment between the last 2
!  windows may be less than WIN_INC).  The ENTIRE trace is always balanced, but
!  the gain will be constant from the center of the last  window to the end of
!  the trace.
!
!  If WINDOWS is "CUSTOM" (instead of "REGULAR") then time varying windows
!  may be specified by TIM_WIN and TIM_LEN arrays.  TIM_WIN specifies a window
!  start time and TIM_LEN specifies a window length in seconds.
!  Time varying windows start at the mute time.  When "CUSTOM" WINDOWS are
!  specified, XP will calculate gain based on these variable windows rather
!  than the constant window size specified by WIN_LEN and WIN_INC.
!
! Removing the gain:
!
!  The gain applied by this process can also be removed by this process
!  unless DEBRI=YES.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
! This process outputs the same traces as it receives (possibly altered).

! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
!  Name        Description                      Action Taken
!  ----        -----------                      ------------
!  NWIH        Number of trace headers          used and sometimes changed
!  NDPT        Number of samples per trace      used but not changed
!  TSTRT       Time for first trace sample      used but not changed
!  DT          Trace sample interval            used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
!  HDW           Description                 Action Taken
!  ---           -----------                 ------------
!   2            Head mute index             used but changed if killing trace
!   25           largest absolute value      recomputed
!   64           Tail mute index             used but changed if killing trace
!
!  HDR_FIRST     First user-defined header word containing gain information
!  HDR_LAST      Last user-defined header word containing gain information
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date     Author       Description
!     ----     -------      -----------
!048. 2006-10-16 D. Glover  Added NULLIFY statements for Intel compiler.
! 47. 2003-07-28 Stoeckley  Added reverse xp option; move most of the code
!                           to the new XPUTIL primitive.
! 46. 2002-05-06 Vunderink  Added parallel control parameters
! 45. 2001-04-26 Selzler    Changed wrapup logic to use skip_wrapup
! 44. 2000-08-15 Selzler    Clarified documentation regarding XP and mean.
! 43. 2000-03-29 Selzler    Rearranged GUI per Bamuels suggestion.
! 42. 2000-03-27 Selzler    Round values to DT and display for user.
! 41. 2000-03-02 Selzler    Added WINDOWS option to GUI
! 40. 2000-02-15 Selzler    Modified default for GAIN_MAX
! 39. 2000-02-11 Selzler    Corrected bug in PWR_MODE parameter cache
! 38. 2000-02-10 Selzler    Corrected bug in wrapup logic
! 37. 2000-02-09 Selzler    synchronized source with CIB's latest newdoc.
! 36. 2000-02-07 Selzler    improved gui support
! 35. 2000-02-02 Selzler    Added support for GUI and general cleanup
! 34. 1999-12-09 Selzler    Corrected default for debrighten flag
! 33. 1999-11-20 Selzler    Added RCS "Id" strings to tag executeable
! 32. 1999-09-16 Selzler    Conversion to f90.
! 31. 1999-02-02 Baumel     Correct interpolation of expansion rates thru
!                           windows where an expansion rate isn't calcula-
!                           ted because the window is less than 50% full
!                           (had been wrong since 1991). Also set header
!                           64 when killing traces. These changes affect
!                           both XP and MVXP.
! 30. 1998-11-10 Vunderink  Begin using the f90 compiler.
! 29. 1997-01-02 Vunderink  Fixed problem with debrighten window when
!                           TSTRT not zero.
! 28. 1996-12-30 Vunderink  Added debrighten window parameters
! 27. 1993-07-20 Troutt     No change in actual code.  Just added this
!                           comment to document the fact that the last
!                           input trace sample (ndpt) never gets included
!                           in the expansion rate calculation, but it
!                           does get expanded.  It would NOT be a simple
!                           change to correct this situation - it falls
!                           out of the intertwined logic for window
!                           length, increment, and last window.
! 26. 1992-02-24 Troutt     Add logic for tail mute header word 64, in-
!                           cluding call to MUTEHW. The last sample to
!                           use in the expansion rate calculation comes
!                           from the LESSER of 2 values:
!                             a) TLAST
!                             b) Header Word 64
!                           Removed logic concerning header words 41, 42,
!                           and 43 (also removed note 7).
! 25. 1991-04-30 Loumos     Raised the window criterion to 50% full.
! 24. 1991-03-07 Howard     Only use windows which are at least 20% full.
! 23. 1990-04-19 Peterson   Zero XFUN scratch array before expansion
!                           rate calculation.
! 22. 1990-04-05 Peterson   Speed-up vectorized loop with stop check.
! 21. 1990-03-23 Peterson   Code cleanup and better prog. documentation.
! 20. 1990-03-20 Peterson   Adjust variable window start times to
!                           start at the mute time.
! 19. 1990-03-13 Peterson   Add option for time varient expansion
!                           window start and length. See note 8.
! 18. 1990-03-07 Peterson   Change to not build expansion rates near
!                           the end of an upshifted trace. See note 7
! 17. 1989-07-07 Baumel     Minor change in operation of TLAST parm.
! 16. 1989-07-05 Baumel     Include PWR=MED option, TLAST parameter,
!                           use John Reed's DBRITR primitive.
! 15. 1988-09-22 Ball       NWIH and NWPT conversion.
! 14. 1988-06-03 Baumel     New convention for mute header word.
! 13. 1988-04-22 Baumel     Change CSPPRT calls to CPSPRT.
! 12. 1988-04-20 Baumel     Allow windows longer than half the trace.
!                           Adjust window for traces shorter than a
!                           window length.
! 11. 1987-10-21 Baumel     Kills trace if already dead in every
!                           window.
! 10. 1987-04-13 Baumel     Write to history file (NCODE call).
!  9. 1987-01-15 Baumel     Remove XPT entry (new convention
!                           for internal calling), add IPRT.
!  8. 1986-10-16 Sinton     Anded entry point XPT.
!  7. 1986-08-18 Baumel     Replace "kill" by DBRI, improve handling
!                           of dead segments, remove L2 option.
!  6. 1986-08-09 Howard     option to "kill" bad traces.
!  5. 1986-08-07 Baumel     Fix bug involving dead windows.
!  4. 1986-08-01 Baumel     Remove TNORM, Add WINC, L1/L2.
!  3. 1986-07-29 Day        Add MEMPRT; TNORM for non-XP traces.
!  2. 1986-07-21 Day        Fixed bug in scaling.
!  1. 1986-07-18 Day        Corrected bug in power calculation.
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
!  This process uses a single set of trace and header arrays.
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
!  Window location indices are first calculated, the mean calculation is done,
!  gain function is determined and applied.  The debrighten primitive (DBRITR)
!  is called after the trace balance at user option.
!
!  Process MVXP is identical to XP except that MVXP uses a MEDIAN statistic
!  rather than the L1 NORM (mean absolute value) which XP uses.  MVXP and XP
!  also have some different parameter defaults.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!  Note that a window must be at least 50% non-zero for normal gain calculation,
!  otherwise the mean for that window is taken to be 0.0.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS XP Process/NC=80> 
!
!            XP rescales trace samples to balance amplitudes.
!
!<include xputil.f90>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module xp_module
      use pc_module
      use named_constants_module
      use xputil_module
      implicit none
      private
      public :: xp_create
      public :: xp_initialize
      public :: xp_update
      public :: xp            ! main execution (trace processing) routine.
      public :: xp_wrapup
      public :: xp_delete


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: xp_struct
        private
        logical                     :: skip_wrapup       ! wrapup flag.

        type(xputil_struct),pointer :: xputil            ! dependent parameter

      end type xp_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(xp_struct),pointer,save :: object      ! needed for traps.

      character(len=100),public :: xp_ident = &
        "$Id: xp.f90,v 1.48 2006/10/17 13:45:50 Glover prod sps $"

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine xp_create (obj)
      implicit none
      type(xp_struct),pointer :: obj       ! arguments

      allocate (obj)
      nullify (obj%xputil) ! jpa

      call xputil_create (obj%xputil, XPUTIL_XP_PROCESS)
      call xp_initialize (obj)

      end subroutine xp_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine xp_delete (obj)
      implicit none
      type(xp_struct),pointer :: obj       ! arguments

      call xp_wrapup     (obj)
      call xputil_delete (obj%xputil)

      deallocate(obj)

      end subroutine xp_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine xp_initialize (obj)
      implicit none
      type(xp_struct),intent(inout) :: obj       ! arguments

      call xputil_initialize (obj%xputil)
      call xp_update         (obj)

      end subroutine xp_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine xp_update (obj)
      implicit none
      type(xp_struct),target,intent(inout) :: obj               ! arguments

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!



!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      call xputil_update (obj%xputil)


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine xp_update


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine xp (obj,ntr,hd,tr)
      implicit none
      type(xp_struct) ,intent(inout)  :: obj                    ! arguments
      integer         ,intent(inout)  :: ntr                    ! arguments
      double precision,intent(inout)  :: hd(:,:)                ! arguments
      real            ,intent(inout)  :: tr(:,:)                ! arguments
      integer                         :: itr,ntr2               ! local
      logical                         :: whoops                 ! local

      if (ntr == NO_MORE_TRACES) then
           call xp_wrapup (obj)
           return
      end if

      ntr2 = ntr
      do itr = 1,ntr2
           call xputil_execute (obj%xputil, hd(:,itr), tr(:,itr), whoops)
           if (whoops) then
                call xp_wrapup (obj)
                ntr = FATAL_ERROR
                return
           end if
      end do

      end subroutine xp


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine xp_wrapup (obj)
      implicit none
      type(xp_struct),intent(inout) :: obj                     ! arguments

      if(obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine xp_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module xp_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
