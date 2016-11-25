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
! Process name: MVXP   (Median Value eXpansion)
! Category   : Amplitude_Mod
! Written    : 1988-10-11  by: Jim Tippett and Bob Baumel
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : A median-based trace balance for relative amplitude processing.
! Portability: No known limitations.
! Parallel   : Yes
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! MVXP works the same way as XP, except that MVXP uses the median statistic
! in calculating the gain function whereas XP uses the L1 NORM (mean absolute
! value).  MVXP is an appropriate trace amplitude balance process for
! relative amplitude processing because the median statistic is insensitive to
! extreme trace amplitude values.
!
! All MVXP parameters have the same meaning as in XP (but some defaults are
! different). For detailed explanation of the process see the documentation for
! XP.
!
! This process calls the XPUTIL primitive to do all the work.  See the XPUTIL
! primitive (and also the XP process) for further information.
! 
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! Window Length and Location:
!
!  WIN_LEN is normally 1.0 sec, although 0.5 sec is sometimes used to constrain
!  amplitude variation.  WIN_INC typically equals WIN_LEN, although setting
!  WIN_INC smaller than WIN_LEN should produce a smoother gain function.
!
!  Some window overlap is recommended when using unusually long windows, so that
!  you still get a reasonable number of windows in the trace, and obtain a
!  reasonably smooth gain function.
!
!  The Lafayette office has asked that all future MVXP processing use WIN_INC
!  that is 25% of WIN_LEN.   This will result in windows overlapping 75%.
!
! Debrighten:
!
!  Debrighten should not be used in relative amplitude work except in rare
!  circumstances where it is needed to control spikes.
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
!
! This process alters input traces.
! This process outputs the same traces as it receives (possibly altered).
!
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
!     Date       Author       Description
!     ----       -------      -----------
!028. 2006-10-16 D. Glover    Added NULLIFY statements for Intel compiler.
! 27. 2003-07-28 Stoeckley    Added reverse mvxp option; move most of the code
!                             to the new XPUTIL primitive; now call XPUTIL
!                             instead of XP internally.
! 26. 2002-05-06 Vunderink    Added parallel control parameters
! 25. 2001-06-04 Selzler      Changed wrapup logic to use skip_wrapup
! 24. 2000-08-29 Selzler      Eliminated memory leak when cfe deletes mvxp.
! 23. 2000-08-15 Selzler      Clarified documentation regarding XP and mean.
! 22. 2000-03-29 Selzler      Rearranged GUI per Bamuels suggestion.
! 21. 2000-03-28 Selzler      Removed !<INCLUDE...> from GUI layout
! 20. 2000-03-13 Selzler      Corrected default for WIN_INC.
! 19. 2000-03-10 Selzler      Added WINDOWS option to GUI
! 18. 2000-02-15 Selzler      Modified default for GAIN_MAX
! 17. 2000-02-11 Selzler      Corrected bug in PWR_MODE parameter cache
! 16. 2000-02-10 Selzler      Corrected bug in wrapup logic
! 15. 2000-02-09 Selzler      synchronized source with CIB's latest newdoc.
! 14. 2000-02-07 Selzler      improved gui support
! 13. 2000-02-02 Selzler      Added support for GUI and general cleanup
! 12. 1999-11-19 Selzler      Added RCS "Id" strings to tag executeable
! 11. 1999-09-17 Selzler      Conversion to f90.
! 10. 1999-01-21 Vunderink    Changed default for WIN_INC parameter. See note 6
!  9. 1998-11-10 Vunderink    Begin using the f90 compiler.
!  8. 1997-01-06 Vunderink    Added debrighten window parameters.
!  7. 1996-02-05 Vunderink    Set PWR=MED default so that it is displayed
!                             in history.
!  6. 1992-02-24 Troutt       Add logic for tail mute header word 64, including
!                             call to MUTEHW. The last sample to
!                             use in the expansion rate calculation comes
!                             from the LESSER of 2 values:
!                               a) TLAST
!                               b) Header Word 64
!                             Removed logic concerning header words 41, 42, 43.
!                             These changes actually implemented by code
!                             changes to XP.
!  5. 1990-03-20 D. Peterson  Add option for time varient expansion
!                             window start and length. See note 4.
!                             Adjust variable window start times to
!                             start at the mute time in XP.
!  4. 1989-07-07 Bob Baumel   Restore WLN=1 default from older version;
!                             minor change in operation of TLAST parm.
!  3. 1989-07-05 Bob Baumel   Turn into shell that internally calls XP.
!  2. 1989-01-24 Jim Tippett  Fix NWIN=0 bug.
!  1. 1988-10-11 Jim Tippett  Converted to CPS from CONSEIS.
!                             (original CONSEIS version by Chuck I.Burch)
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
!  On input, NTR must have one of these values:
!  NTR >= 1                   Means process the input traces.
!  NTR == NO_MORE_TRACES      Means there are no more input traces.
!
!  On output, NTR must have one of these values:
!  NTR >= 1                   Means this process is outputing traces.
!  NTR == NO_MORE_TRACES      Means there are no more traces to output.
!  NTR == FATAL_ERROR         Means this process has a fatal error.
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
!  Window location indices are first calculated, the median calculation is done,
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
!       MVXP is a median-based trace balance for relative amplitude.
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


      module mvxp_module
      use pc_module
      use named_constants_module
      use xputil_module
      implicit none
      private
      public :: mvxp_create  
      public :: mvxp_initialize
      public :: mvxp_update 
      public :: mvxp_delete
      public :: mvxp            ! main execution (trace processing) routine.
      public :: mvxp_wrapup


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: mvxp_struct
        private
        logical                     :: skip_wrapup       ! wrapup flag.

        type(xputil_struct),pointer :: xputil            ! dependent parameter

      end type mvxp_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(mvxp_struct),pointer,save :: object      ! needed for traps.

      character(len=100),public :: mvxp_ident = &
        "$Id: mvxp.f90,v 1.28 2006/10/17 13:45:46 Glover prod sps $"

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine mvxp_create (obj)
      implicit none
      type(mvxp_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%xputil) ! jpa
      call xputil_create   (obj%xputil, XPUTIL_MVXP_PROCESS)
      call mvxp_initialize (obj)

      end subroutine mvxp_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine mvxp_delete (obj)
      implicit none
      type(mvxp_struct),pointer :: obj       ! arguments

      call mvxp_wrapup   (obj)
      call xputil_delete (obj%xputil)

      deallocate(obj)

      end subroutine mvxp_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine mvxp_initialize (obj)
      implicit none
      type(mvxp_struct),intent(inout) :: obj       ! arguments

      call xputil_initialize (obj%xputil)
      call mvxp_update       (obj)

      end subroutine mvxp_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine mvxp_update (obj)
      implicit none
      type(mvxp_struct),target,intent(inout) :: obj        ! arguments

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


      end subroutine mvxp_update


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine mvxp (obj,ntr,hd,tr)
      implicit none
      type(mvxp_struct),intent(inout)  :: obj                    ! arguments
      integer          ,intent(inout)  :: ntr                    ! arguments
      double precision ,intent(inout)  :: hd(:,:)                ! arguments
      real             ,intent(inout)  :: tr(:,:)                ! arguments
      integer                          :: itr,ntr2               ! local
      logical                          :: whoops                 ! local

      if (ntr == NO_MORE_TRACES) then
           call mvxp_wrapup (obj)
           return
      end if

      ntr2 = ntr
      do itr = 1,ntr2
           call xputil_execute (obj%xputil, hd(:,itr), tr(:,itr), whoops)
           if (whoops) then
                call mvxp_wrapup (obj)
                ntr = FATAL_ERROR
                return
           end if
      end do

      end subroutine mvxp


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine mvxp_wrapup (obj)
      implicit none
      type(mvxp_struct),intent(inout) :: obj       ! arguments

      if(obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine mvxp_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module mvxp_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

