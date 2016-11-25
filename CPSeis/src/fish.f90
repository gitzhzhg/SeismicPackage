!<CPS_v1 type="PROCESS"/>
!!------------------------------- fish.f90 ---------------------------------!!
!!------------------------------- fish.f90 ---------------------------------!!
!!------------------------------- fish.f90 ---------------------------------!!


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
! Name       : FISH      (File Shift)
! Category   : statics
! Written    : 1989-03-27   by: Tom Stoeckley
! Revised    : 2007-04-24   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : 2D and 3D surface consistent residual statics process.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
!  Purpose:       This is a residual static program designed to
!                 derive surface consistent static files from
!                 2D and 3D reflection data.
!
!  Method:        This process derives statics by correlating traces
!                 to a base (or pilot) trace at the same CMP location.
!                 In contrast to IMS, this process stacks the
!                 correlations for a given ground position, instead of
!                 picking each correlation separately and doing an
!                 iterative solution.  Also, a partial stack of traces
!                 with the same ground position is done, followed by a
!                 correlation with a base stacked over the same CMP
!                 range.
!
!  Advantages:    Events that are not flat in a CMP gather, such as
!                 multiples or linear interference, cause less damage
!                 than they might with IMS.  Also, lateral structural
!                 variations have less influence because both traces
!                 being correlated are stacked over the same CMP range.
!
!  Editing:       This process outputs stacked correlation files in
!                 addition to the statics files.  The correlation files
!                 can be viewed in CBYT in order to help edit the statics
!                 files.  The statics files can also be edited in ISEP
!                 or MSEPITA.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
! FISH Operation Summary:
!
!   1. Correlate each trace in a shot profile with a base trace formed by
!      compositing all remaining traces in that trace's CMP.  Since all
!      correlation is done within a given CMP, there should be no contamination
!      of the solution by structure.
!
!   2. Composite the correlations for individual traces in the shot profile
!      and pick the time shift from the composited correlation.  This is the
!      static shift for this shot location.
!
!   3. Repeat steps 1 and 2 for all shot profiles to find the source static for
!      all the source locations.
!
!   4. Repeat steps 1 - 3 exchanging the word "source" (or "shot") with
!      "receiver" to find the receiver static for all the receiver locations.
!
!   5. FISH can be iterated by applying the static solution and repeating the
!      algorithm (steps 1 - 4).  Two or three iterations are usually sufficient.
!
!
! FISH can partially composite traces prior to the correlation step:
!
!   1. Consider a range of several CMPs.  Composite over this CMP range the
!      traces that belong to the shot profile in question.
!
!   2. Now composite over the same CMP range the remaining traces from the
!      CMPs and use this composite as the base.  (Note that compositing of
!      traces within the shot profile and compositing of traces to form their
!      base is done over the same CMP range so structural contamination is
!      avoided.)
!
!   3. Correlate the composited shot profile traces with the composited base
!      traces and proceed as described above.
!
! This compositing step helps attenuate energy that is not flat with offset,
! such as linear interference and multiples, and may also help the solution
! when random noise is a problem.  Correlation is still done within the same
! CMP range so there is no contamination by structure.  Five CMPs is a typical
! number to composite and is helpful even on good data.
!
!
! 3D Work:
!
! FISH can be used on 2D or 3D data, although it may be quite time consuming on
! 3D data.  The process CC3D is designed specifically for 3D work and should
! give results very similar to FISH and run substantially faster.
!
!
! Advantages of FISH:
!
!   1. FISH is good at solving for large and abrupt statics.
!
!   2. It gives more precise results than SISC (but less precise than IMS).
!
!   3. Because FISH composites correlations before picking (and optionally
!      composites before correlating) it works better on noisy data than other
!      statics processes.  It will give usable results on data so poor that all
!      other statics processes will fail.
!
!   4. FISH allows stacked correlations to be displayed and edited.  This is
!      essential for most data.
!
!
! Disadvantages of FISH:
!
!   1. FISH is blind to certain types of statics that equally effect all
!      traces in a CMP.
!
!   2. Editing the correlations may not be obvious.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! This process is an all-trace (loop-splitting) process.
! This process allows traces to be input in gathers or one at a time.
! This process allows traces to be input in any sort order.
! Traces should have been expanded with XP or MVXP or equivalent.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! If DEAD_END = YES, this process does not output any traces.
! If DEAD_END = NO, this process outputs the same traces it received,
! one at a time, shifted by the statics solution, in the same order it
! received them.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       input unused; set to 1.
! GATHERED  whether traces are a legitimate gather  input unused; set to false.
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Hwd#       Description                       Action taken
! ----       -----------                       ------------
!  6         offset                            used but not changed
!  7         CMP X grid coordinate             used but not changed
!  8         CMP Y grid coordinate             used but not changed
! HDR_FLAG   trace flag                        used but not changed
! HDR_SX     source X ground position          used but not changed
! HDR_SY     source Y ground position          used but not changed
! HDR_RX     source X ground position          used but not changed
! HDR_RY     source Y ground position          used but not changed
! WIN_HDR_X  first coord of window location    used but not changed
! WIN_HDR_Y  second coord of window location   used but not changed
!  2         top mute index                    used (updated if DEAD_END is NO)
! 64         bottom mute index                 used (updated if DEAD_END is NO)
! 25         largest absolute value                 (updated if DEAD_END is NO)
! 43         cumulative residual static                 (set if DEAD_END is NO)
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
! 36. 2007-04-24  Stoeckley  Never set NUMTR to zero.
! 35. 2006-12-04  D. Glover  Added NULLIFY statements for Intel compiler.
! 34. 2001-10-18  Stoeckley  Add file selection boxes and file status messages.
! 33. 2001-05-14  Stoeckley  Change wrapup flag.
! 32. 2000-10-20  Stoeckley  Documentation change only to clarify the definition
!                             of the NUM_COMP parameter.
! 31. 2000-09-27  Stoeckley  Fix bug in FISH_BUILD_BASE_TRACES (which caused
!                             base traces to always be single stacked traces)
!                             and FISH_ALGORITHM (which skipped some traces).
! 30. 2000-08-08  Stoeckley  Minor documentation change regarding header words
!                             7, 8, WIN_HDR_X, and WIN_HDR_Y.
! 29. 2000-07-20  Stoeckley  Removed two lines of code which tested the ERROR
!                             variable before it was defined; fix default for
!                             the CONVERGE parameter (from 40 to 1).
! 28. 2000-06-16  Stoeckley  Converted from old system.
! 27. 1998-12-15  Vunderink  Begin using the f90 compiler.
! 26. 1997-08-18  Vunderink  Initialize IR in FISHBEG1 for the case where
!                             only one statics file.
! 25. 1997-08-14  Vunderink  Fixed bug caused by typo in update yesterday
! 24. 1997-08-13  Vunderink  Fixed bug in spatial window which was causing
!                             the traces to be windowed on output
! 23. 1997-08-11  Vunderink  Added spatial correlation window
! 22. 1996-10-09  Stoeckley  Changed #RUN default from -1 to 0.
! 21. 1996-09-26  Vunderink  Added RLSTAPE and removed SORTDIR keywords.
! 20. 1996-06-20  Vunderink  Removed unused variable from print statement
! 19. 1995-11-02  Vunderink  The program will now determine the number of
!                             bits required to store the sequential trace
!                             number.  If the number of bits is less than
!                             21, then packed word for each trace is still
!                             split into 3 equal part of 21 bits each.
!                             Otherwise, the required number of bits are
!                             allocated to store the trace number and the
!                             remaining bits are split into 2 equal parts.
! 18. 1995-09-28  Vunderink  Add STOP and BYTSAV keywords.
! 17. 1994-05-20  Troutt     Make code for removal of $SORTDIR file conditional.
! 16. 1994-05-16  Stoeckley  Changes to FISHPOND regarding FORTRAN direct
!                             access file LTAPE.  Instead of one file w/
!                             up to 4 records, there are now 4 sequential
!                             files (LTAPE1-4).  Latest Unicos version was
!                             allocating 4 buffers (each with size of record
!                             as given in OPEN) which used too much memory.
! 15. 1994-02-11  Troutt     Add error check for HPALLOC call.
! 14. 1994-01-24  Stoeckley  Add keywords SORTDIR, SHEAD, RHEAD, and MHEAD.
!                             (Do not yet implement the use of keywords
!                             SHEAD, RHEAD, and MHEAD.)
!                             Also add call to get_nparm.
! 13. 1993-11-03  Stoeckley  Change DTROTSI argument to use $SORTDIR
!                             file system for the large trace file.
!                             Also break up the cmp/base trace file into
!                             two files (lun5 and lun6).
! 12. 1993-11-03  Stoeckley  Make changes to explicitely save CMP info,
!                             rather than assuming that (hdr46 + hdr47)/2
!                             uniquely identifies a midpoint.  Needed for
!                             3-D data where source and receiver ground
!                             positions are on different systems.  Needed
!                             to replace FISIBEG1,2 (in FISIUTIL) with
!                             FISHBEG1,2 (new routine in this process),
!                             plus make changes in subroutines FISH,
!                             FISHPOND, FISHADJ, FISHLIM.
! 11. 1993-03-23  Troutt     Add PATH parameter to allow stacked correla-
!                             tions to be put into BYTE file(s).
! 10. 1992-02-25  Troutt     Add logic for tail mute HW64 by updating
!                             primitive FISIUTIL (routine FISISHFT).
!  9. 1991-03-05  Troutt     Changed LTAPE in FISHPOND from alpha name to
!                             unit number for direct access i/o file. Unicos
!                             aborted at the OPEN. Using GETLUN to get a
!                             unique unit number.
!                             Also added /GLOBALS/ to FISHPOND so NDPT would
!                             have a value.
!  8. 1990-10-23  Peterson   Include error and abort arguments on calls to
!                             HPALLOC and HPDEALLC.
!  7. 1990-06-19  Stoeckley  Replace some routines by calls to FISIUTIL
!                             routines.
!  6. 1989-06-02  Stoeckley  Change FISHSORT to better version.
!  5. 1989-05-26  Stoeckley  Add if-check in FISHSORT to prohibit operand
!                             range error which sometimes occurs when not
!                             compiled with DEBUG option.
!  4. 1989-05-25  Stoeckley  Add check for dead traces.
!  3. 1989-05-15  Stoeckley  Change use of parameter HF#.
!  2. 1989-05-08  Stoeckley  Change MXSH and MXPK from seconds to millisecs.
!  1. 1989-03-27  Stoeckley  Minor print format changes.
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
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed (unknown).
! NSTORE           0       amount of permanent memory needed (unknown).
! IFTD           true      whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large (unknown).
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces (DEAD_END = NO).
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces (DEAD_END = NO).
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
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS FISH Process/NC=80>
!
!<include stathelper.f90>
!
!                     Static Calculation Parameters
! `---------------------------------------------------------------------------
!            NUM_ITER~~=`I           NUM_COMP=`III     NUM_CMP =`III
!            MAX_STATIC=`FFFFF ms    ICC_MIN =`FFFF    FOLD_MIN=`III
!            CONVERGE~~=`FFFFF ms    SCC_MIN =`FFFF    MAX_ASYM=`III
! `---------------------------------------------------------------------------
!
!<NS Correlation Window Specifications/NC=80>
!<include latwin.f90>
!
!<PARMS Correlation Window Specifications [screen2]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!
!<Help KEYWORD="NUM_ITER">
!<Tip> Number of iterations to do to solve the statics. </Tip>
! Default =  3
! Allowed =  int >= 1
!
! Normally more than 3 iterations are not required.
!</Help>
!
!
!<Help KEYWORD="MAX_STATIC">
!<Tip> Maximum static shift (milliseconds). </Tip>
! Default = 40.0
! Allowed = real > 0.0
!
! This is the maximum static that is solved for in any one iteration.
! The final static (accumulated over all iterations) could exceed this
! value for a few ground positions.
!</Help>
!
!
!<Help KEYWORD="CONVERGE">
!<Tip> Convergence criterion (milliseconds). </Tip>
! Default = 1.0
! Allowed = real >= 0.0
!
! Iterations are terminated when the maximum absolute change in any static
! value is less than CONVERGE.  The number of iterations will not exceed
! NUM_ITER.
!</Help>
!
!
!<Help KEYWORD="NUM_COMP">
!<Tip> Maximum number of CMPs to composite prior to correlating. </Tip>
! Default = 5 
! Allowed = int >= 1
!
! Partial composite over CMPs (common midpoints) prior to correlation reduces
! the effects of random and linear noise.  Some partial composite prior to
! correlation is beneficial even on good data.
!
! For example: For a source file solution, several traces with the same
! source ground position will be composited over several CMPs, and then
! correlated with a base trace which is a full stack over the same CMPs.
! This helps attenuate energy that is not flat with offset, such as linear
! interference or multiples, and may also help the solution when random noise
! is a problem.
!
! NOTE: A CMP consists of all traces sharing common (nearest integer) values
! of header words 7 and 8.
!</Help>
!
!
!<Help KEYWORD="ICC_MIN">
!<Tip> Minimum individual correlation coefficient to use. </Tip>
! Default = 0.0
! Allowed = 0.0 <= real < 1.0
!
! If the correlation coefficient for an individual correlation is less than
! ICC_MIN, then that correlation will not be added to the stacked correlation.
!</Help>
!
!
!<Help KEYWORD="SCC_MIN">
!<Tip> Minimum stacked correlation coefficient to use. </Tip>
! Default = 0.0
! Allowed = 0.0 <= real < 1.0
! 
! If the correlation coefficient for a stacked correlation is less than
! SCC_MIN, then the static value will be set to nil.
!</Help>
!
!
!<Help KEYWORD="NUM_CMP">
!<Tip> Maximum number of CMPs to composite to form a base trace. </Tip>
! Default = 1
! Allowed = int >= 1
!
! Usually NUM_CMP should be set to 1, unless NUM_COMP = 1.
!</Help>
!
!
!<Help KEYWORD="FOLD_MIN">
!<Tip> Minimum fold for base trace. </Tip>
! Default = -1
! Allowed = -1 or int >= 1
!
! CDPs are composited if necessary to reach the desired minimum fold.
! If FOLD_MIN = -1, then half the average fold is used.
!</Help>
!
!
!<Help KEYWORD="MAX_ASYM">
!<Tip> Maximum assymetry allowed at edges of line when compositing CMPs. </Tip>
! Default = -1
! Allowed = -1 or int >= 0
!
! If MAX_ASYM = -1, then the larger of either FOLD_MIN or NUM_CMP/2 is used as
! a constraint.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module fish_module
      use pc_module
      use named_constants_module
      use stathelper_module
      use mth_module
      implicit none
      private
      public :: fish_create
      public :: fish_initialize
      public :: fish_update
      public :: fish_delete
!<execute_only>
      public :: fish
      public :: fish_wrapup
!</execute_only>


      character(len=100),public,save :: FISH_IDENT = &
       '$Id: fish.f90,v 1.36 2007/04/25 15:46:22 Stoeckley beta sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: fish_struct              
 
        private
        logical                         :: skip_wrapup       ! wrapup flag.
        integer                         :: num_iter          ! process params
        real                            :: max_static        ! process params
        real                            :: converge          ! process params
        integer                         :: num_comp          ! process params
        real                            :: icc_min           ! process params
        real                            :: scc_min           ! process params
        integer                         :: num_cmp           ! process params
        integer                         :: fold_min          ! process params
        integer                         :: max_asym          ! process params
        type(stathelper_struct),pointer :: helper            ! dependent

      end type fish_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(fish_struct),pointer,save :: object      ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine fish_create (obj)
      implicit none
      type(fish_struct),pointer :: obj       ! arguments

      allocate (obj)
      nullify (obj%helper) ! jpa

      call stathelper_create    (obj%helper, 'FISH')
      call fish_initialize      (obj)
      return
      end subroutine fish_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine fish_delete (obj)
      implicit none
      type(fish_struct),pointer :: obj       ! arguments

!<execute_only>
      call fish_wrapup (obj)
!</execute_only>

      call stathelper_delete (obj%helper)

      deallocate(obj)
      return
      end subroutine fish_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine fish_initialize (obj)
      implicit none
      type(fish_struct),intent(inout) :: obj       ! arguments

      obj%num_iter   = 3              ! was npass
      obj%max_static = 40.0           ! was mxpk
      obj%converge   = 1.0            ! was converge
      obj%num_comp   = 5              ! was ncomp
      obj%icc_min    = 0.0            ! was mncc
      obj%scc_min    = 0.0            ! was mnfc
      obj%num_cmp    = 1              ! was ncdp
      obj%fold_min   = -1             ! was nfold
      obj%max_asym   = -1             ! was nassym

      call stathelper_initialize (obj%helper)
      call fish_update           (obj)
      return
      end subroutine fish_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine fish_update (obj)
      implicit none
      type(fish_struct),intent(inout),target :: obj                   ! args
      logical                                :: error                 ! local
      integer         ,parameter             :: RA_SRC       = 1      ! local
      integer         ,parameter             :: RA_REC       = 1      ! local
      character(len=8),parameter             :: HDR_CORR_SRC = 'CMP'  ! local
      character(len=8),parameter             :: HDR_CORR_REC = 'CMP'  ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      call pc_get        ('num_iter'    , obj%num_iter  )
      call pc_get        ('max_static'  , obj%max_static)
      call pc_get        ('converge'    , obj%converge  )
      call pc_get        ('num_comp'    , obj%num_comp  )
      call pc_get        ('icc_min'     , obj%icc_min   )
      call pc_get        ('scc_min'     , obj%scc_min   )
      call pc_get        ('num_cmp'     , obj%num_cmp   )
      call pc_get        ('fold_min'    , obj%fold_min  )
      call pc_get        ('max_asym'    , obj%max_asym  )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      call mth_constrain (obj%num_iter  ,   1,    99)
      call mth_constrain (obj%max_static, 1.0, 999.0)
      call mth_constrain (obj%converge  , 0.0,  99.0)
      call mth_constrain (obj%num_comp  ,   1,   999)
      call mth_constrain (obj%icc_min   , 0.0,   0.9)
      call mth_constrain (obj%scc_min   , 0.0,   0.9)
      call mth_constrain (obj%num_cmp   ,   1,   999)
      call mth_constrain (obj%fold_min  ,  -1,   999)     ! 0 not allowed.
      call mth_constrain (obj%max_asym  ,  -1,   999)     ! 0 is allowed.

      if (obj%fold_min == 0) obj%fold_min = 1

      call stathelper_update (obj%helper,                                  &
                              obj%num_iter, obj%max_static, obj%converge,  &
                              RA_SRC, RA_REC, HDR_CORR_SRC, HDR_CORR_REC,  &
                              error)


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put         ('num_iter'    , obj%num_iter  )
      call pc_put         ('max_static'  , obj%max_static)
      call pc_put         ('converge'    , obj%converge  )
      call pc_put         ('num_comp'    , obj%num_comp  )
      call pc_put         ('icc_min'     , obj%icc_min   )
      call pc_put         ('scc_min'     , obj%scc_min   )
      call pc_put         ('num_cmp'     , obj%num_cmp   )
      call pc_put         ('fold_min'    , obj%fold_min  )
      call pc_put         ('max_asym'    , obj%max_asym  )


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine fish_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!




!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine fish (obj,ntr,hd,tr)
      implicit none
      type(fish_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments
      logical                         :: error                  ! local

      if(ntr /= NEED_TRACES) then
           call stathelper_input_traces (obj%helper,ntr,hd,tr)
           if (ntr == NEED_TRACES) return
      end if

      if (ntr == NO_MORE_TRACES) then
           call fish_solve (obj,error)
           if (error) then
                ntr = FATAL_ERROR
           else
                ntr = NEED_TRACES
           end if
      end if

      if (ntr == NEED_TRACES .or. ntr == FATAL_ERROR) then
           call fish_wrapup (obj)
           call stathelper_output_traces (obj%helper,ntr,hd,tr)
      end if
      return
      end subroutine fish


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine fish_wrapup (obj)
      implicit none
      type(fish_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine fish_wrapup


!!-------------------------- fish solve ------------------------------------!!
!!-------------------------- fish solve ------------------------------------!!
!!-------------------------- fish solve ------------------------------------!!


      subroutine fish_solve (obj,error)
      implicit none
      type(fish_struct),intent(inout) :: obj                    ! arguments
      logical          ,intent(out)   :: error                  ! arguments
      integer                         :: nfiles,nwin,ncorr      ! local
      integer                         :: ntraces,ncmp           ! local
      integer                         :: iter,ifile,nfoldav     ! local
      logical                         :: converged              ! local

      nfiles  = stathelper_get_nfiles  (obj%helper)
      nwin    = stathelper_get_nwin    (obj%helper)
      ncorr   = stathelper_get_ncorr   (obj%helper)
      ntraces = stathelper_get_ntraces (obj%helper)
      ncmp    = stathelper_get_ncmp    (obj%helper)
      nfoldav = ntraces / max(ncmp,1)

      if (obj%fold_min == -1) obj%fold_min = nfoldav / 2
      if (obj%max_asym == -1) obj%max_asym = max(obj%fold_min,obj%num_cmp/2)

      call pc_print (' ')
      call pc_print ('FISH: average fold =',nfoldav)
      call pc_print ('FISH: base trace parameter NUM_CMP  =',obj%num_cmp)
      call pc_print ('FISH: base trace parameter FOLD_MIN =',obj%fold_min)
      call pc_print ('FISH: base trace parameter MAX_ASYM =',obj%max_asym)
      call pc_print (' ')

      do iter = 1,obj%num_iter

        call stathelper_begin_iteration (obj%helper,error)
        if (error) return

        do ifile = 1,nfiles

           call stathelper_begin_file_iteration (obj%helper,ifile,error)
           if (error) return

           call fish_build_stacks (obj,nwin,error)
           if (error) then
                call pc_error ('FISH: error building stacked traces')
                return
           end if

           call fish_build_bases (obj,nwin,error)
           if (error) then
                call pc_error ('FISH: error building base traces')
                return
           end if

           call fish_algorithm (obj,ifile,nwin,ncorr,error)
           if (error) then
                call pc_error ('FISH: error executing solution algorithm')
                return
           end if

           call stathelper_end_file_iteration (obj%helper,ifile,error)
           if (error) return

        end do

        call stathelper_end_iteration (obj%helper,converged,error)
        if (error .or. converged) return

      end do

      error = .false.
      return
      end subroutine fish_solve


!!---------------------- fish build stacks ---------------------------------!!
!!---------------------- fish build stacks ---------------------------------!!
!!---------------------- fish build stacks ---------------------------------!!


      subroutine fish_build_stacks (obj,nwin,error)
      implicit none
      type(fish_struct)   ,intent(inout) :: obj                  ! arguments
      integer             ,intent(in)    :: nwin                 ! arguments
      logical             ,intent(out)   :: error                ! arguments
      real                               :: aaa(nwin)            ! local
      real                               :: bbb(nwin)            ! local
      integer                            :: icmp,icmp2,ifold     ! local
      integer                            :: itrace,ntraces,ncmp  ! local

      call stathelper_sort_to_cmps (obj%helper)

      ntraces = stathelper_get_ntraces (obj%helper)
      ncmp    = stathelper_get_ncmp    (obj%helper)
      itrace  = 1

      do icmp = 1,ncmp
           bbb(:) = 0.0
           ifold  = 0
           do
                if (itrace > ntraces) exit
                icmp2 = stathelper_get_icmp (obj%helper,itrace)
                if (icmp2 > icmp) exit
                call stathelper_read_shifted_window &
                                      (obj%helper,itrace,aaa,error)
                if (error) return
                bbb(:) = bbb(:) + aaa(:)
                ifold  = ifold  + 1
                itrace = itrace + 1
           end do
           call stathelper_write_stacked_window &
                                      (obj%helper,icmp,ifold,bbb,error)
           if (error) return
      end do

      error = .false.
      return
      end subroutine fish_build_stacks


!!---------------------- fish build bases ---------------------------------!!
!!---------------------- fish build bases ---------------------------------!!
!!---------------------- fish build bases ---------------------------------!!


      subroutine fish_build_bases (obj,nwin,error)
      implicit none
      type(fish_struct)   ,intent(inout) :: obj                    ! arguments
      integer             ,intent(in)    :: nwin                   ! arguments
      logical             ,intent(out)   :: error                  ! arguments
      real                               :: aaa(nwin)              ! local
      real                               :: bbb(nwin)              ! local
      integer                            :: icmp,ncmp,kount,istep  ! local
      integer                            :: icmp2,ifolda,ifoldb    ! local
      integer                            :: ia,ib                  ! local

      ncmp    = stathelper_get_ncmp (obj%helper)

      do icmp = 1,ncmp
           call stathelper_read_stacked_window &
                               (obj%helper,icmp,ifoldb,bbb,error)
           if (error) return
           kount = 1
           istep = 0

           do
                if (ifoldb == 0) exit
                if (ifoldb >= obj%fold_min .and. kount >= obj%num_cmp) exit
                istep = istep + 1
                if (abs(min(istep,icmp-   1) - &
                        min(istep,ncmp-icmp)) > obj%max_asym) exit
                ia = max(icmp-istep,1)
                ib = min(icmp+istep,ncmp)
                do icmp2 = ia,ib,2*istep
                     call stathelper_read_stacked_window &
                                       (obj%helper,icmp2,ifolda,aaa,error)
                     if (error) return
                     if (ifolda == 0) cycle
                     bbb(:) = bbb(:) + aaa(:)
                     ifoldb = ifoldb + ifolda
                     kount  = kount  + 1
                end do
           end do

           call stathelper_write_base_window &
                                  (obj%helper,icmp,ifoldb,bbb,error)
           if (error) return
      end do

      error = .false.
      return
      end subroutine fish_build_bases


!!------------------------- fish algorithm ---------------------------------!!
!!------------------------- fish algorithm ---------------------------------!!
!!------------------------- fish algorithm ---------------------------------!!


      subroutine fish_algorithm (obj,ifile,nwin,ncorr,error)
      implicit none
      type(fish_struct)   ,intent(inout) :: obj                     ! arguments
      integer             ,intent(in)    :: ifile,nwin,ncorr        ! arguments
      logical             ,intent(out)   :: error                   ! arguments
      integer                            :: nnc,nnt,nnt2            ! local
      integer                            :: igp2,icmp,igp,ngp,istop ! local
      integer                            :: itrace,ntraces,ifold    ! local
      real                               :: denom2,denom            ! local
      real                               :: stat,ccoef              ! local
      real                               :: ttt(nwin)               ! local
      real                               :: aaa(nwin)               ! local
      real                               :: bbb(nwin)               ! local
      real                               :: ccc(ncorr)              ! local
      real                               :: ddd(ncorr)              ! local

!----------get started.

      call stathelper_sort_to_file (obj%helper,ifile)

      ntraces = stathelper_get_ntraces (obj%helper)
      ngp     = stathelper_get_ngp     (obj%helper,ifile)
      itrace  = 0

!----------go through the loop.

      do igp = 1,ngp
           nnc    = 0
           nnt    = 0
           ddd(:) = 0.0
           denom  = 0.0

!----------begin a new partial stack.

           do
                aaa(:) = 0.0
                bbb(:) = 0.0
                nnt2   = 0
                do
                     itrace = itrace + 1
                     if (itrace > ntraces) then
                          itrace = itrace - 1
                          exit
                     end if
                     igp2 = stathelper_get_igp  (obj%helper,itrace)
                     icmp = stathelper_get_icmp (obj%helper,itrace)
                     if (igp2 < igp) cycle
                     if (igp2 > igp) then
                          itrace = itrace - 1
                          exit
                     end if
                     if (nnt2 == 0) istop = icmp + obj%num_comp - 1
                     if (icmp > istop) then
                          itrace = itrace - 1
                          exit
                     end if

!----------get base trace.

                     call stathelper_read_base_window &
                                  (obj%helper,icmp,ifold,ttt,error)
                     if (error) return
                     if (ifold <= 1) cycle
                     bbb(:) = bbb(:) + ttt(:)
                     nnt2   = nnt2 + 1

!----------get individual trace.

                     call stathelper_read_shifted_window &
                                        (obj%helper,itrace,ttt,error)
                     if (error) return
                     aaa(:) = aaa(:) + ttt(:)
                end do

!----------correlate the composited traces.

                if (nnt2 == 0) exit
                bbb(:) = bbb(:) - aaa(:)
                call stathelper_corr (obj%helper,aaa,bbb,ccc,denom2)

!----------decide whether to use this correlation.

                call stathelper_pick &
                        (obj%helper,ccc,obj%icc_min,denom2,  stat,ccoef)
                if (stat /= FNIL) then
                     nnt    = nnt + nnt2
                     nnc    = nnc + 1
                     denom  = denom + denom2
                     ddd(:) = ddd(:) + ccc(:)
                end if
           end do

!----------pick the static and finish this ground position.

           call stathelper_pick &
                        (obj%helper,ddd,obj%scc_min,denom,  stat,ccoef)
           call stathelper_report_static &
                        (obj%helper,ifile,igp,ddd,stat,ccoef,nnt,nnc,error)
           if (error) return
      end do

!----------finish up.

      error = .false.
      return
      end subroutine fish_algorithm


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


!</execute_only>

      end module fish_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

