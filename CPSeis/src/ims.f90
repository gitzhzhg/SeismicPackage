!<CPS_v1 type="PROCESS"/>
!!------------------------------- ims.f90 ---------------------------------!!
!!------------------------------- ims.f90 ---------------------------------!!
!!------------------------------- ims.f90 ---------------------------------!!


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
! Name       : IMS      (Iterative CMP Residual Statics)
! Category   : statics
! Written    : 1988-08-18   by: Greg Lazear
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Iterated 2D and 3D surface consistent residual statics process.
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
!  Method:        IMS is an automated base trace oriented statics process.
!                 Many options are provided for building the base trace,
!                 which is formed by compositing a number of CMP stacked
!                 traces centered on the trace in question.  Within the
!                 industry, processes similar to IMS are often called
!                 "automatic statics".  In contrast to FISH, this process
!                 obtains and picks an individual correlation for each
!                 trace, and then solves the statics by Gauss-Seidel
!                 iterations.
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
! IMS Operation Summary:
!
!   1. Using one of the options, build the base trace centered on the CMP
!      containing the trace in question.
!
!   2. Correlate the trace with the base trace, pick the correlation and store
!      the shift value.
!
!   3. Repeat steps 1 and 2 for all the pre-stack traces.
!
!   4. Decompose the stored static shift values into a surface consistent form
!      by using a Gauss-Seidel iteration scheme.  Setting NUM_IRLS > 0
!      eliminates outlying values, producing a more stable solution.
!
!   5. Repeat steps 1 - 4 after applying the calculated shifts.  The base
!      trace should improve with each solution.  4 to 6 iterations are
!      generally used.
!
!
! Advantages of IMS:
!
!   1. IMS gives very accurate results.
!
!   2. The IMS surface consistent decomposition procedure provides more
!      properly surface consistent results than does SISC or FISH.
!
!   3. Because IMS is automated, it requires no user intervention once
!      parameters are chosen.
!
!   4. IMS provides many options for building the base trace.
!
!
! Disadvantages of IMS:
!
!   1. IMS works well only on data with small statics remaining.
!
!   2. IMS requires fairly good data for reliable operation.
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
! NUMTR     max number of traces input/output       input unused; set to 0 or 1.
! GATHERED  whether traces are a legitimate gather  input unused; set to false.
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
!
! NUMTR is set to 0 if DEAD_END is YES and 1 if DEAD_END is NO.
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
!045. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!044. 2006-01-10  B. Menger  Removed Unused Variables.
! 43. 2003-06-16  Stoeckley  Rearrange subroutine argument declarations to
!                             remove compiler warning.
! 42. 2001-10-18  Stoeckley  Add file selection boxes and file status messages.
! 41. 2001-01-15  Stoeckley  Change wrapup flag.
! 40. 2000-10-09  Stoeckley  Add missing required documentation section.
! 39. 2000-08-21  Stoeckley  Converted from old system.
! 38. 1998-12-15  Vunderink  Begin using the f90 compiler.
! 37. 1996-09-26  Vunderink  Added RLSTAPE and removed DTSAV keywords.
! 36. 1996-05-28  Vunderink  Added BYTSAV keyword.
! 35. 1995-02-22  Troutt     Change defaults (& DOC) to 2.0 for DELTAX and
!                             DELTAY.  Also document that they must be >0.
! 34. 1994-04-20  Troutt     Correct mispelled variable (NGPX-->NSGPX) in
!                             function ISTATS.  Bug caused improper indexing
!                             of source static arrays when MODE=SSR and
!                             SGP=GRID.
! 33. 1994-03-03  Troutt     Correct DOC only (for RGP).
! 32. 1994-03-01  Troutt     Add new parameters DTSAV, STOP, EVERY.
!                             Add a comment to static files indicating the
!                             number of iterations completed.  Assign a new
!                             file (LLFILE) to source static file (no longer
!                             reuse redundancy file).
!                             Also fix bug relating to CMP binning: if XGV1
!                             was input as a value greater than 1st bin
!                             center, negative bin#s resulted and INDEX was
!                             not properly built for sorting the traces.
!                             The statics were computed properly, but the
!                             output traces were not properly sorted.  This
!                             bug would not likely have affected 2D data.
! 31. 1994-02-11  Troutt     Add error checks for HPALLOC calls.
! 30. 1992-12-15  Troutt     Abort with message if #TRJ exceeded.
! 29. 1992-02-21  Troutt     Add handling of tail mute header word 64. Add
!                             call to MUTEHW.
! 28. 1992-02-07  Troutt     Fix rare problem where base trace is dead
!                             within correlation window but input trace
!                             is not (window for input trace is larger by
!                             2*MXSH).
! 27. 1991-09-18  Troutt     Delete DTROT files upon completion in order
!                             to save disk space.
! 26. 1991-09-09  Troutt     Fix call to FILTERG for bandpass filter of
!                             stacked trace ("-1" for #pts in SCTR).
! 25. 1991-06-20  Troutt     UNICOS fix: Close LFILE before using it for
!                             static file "FILS" so as to avoid conflict due
!                             to its usage as an unformatted redundancy file.
! 24. 1990-12-07  Peterson   Correct error return argument on calls to
!                             CALL CLOSFIL (ARG1,ARG2,*error return address).
! 23. 1990-11-07  Ball       Change CARDS from 100 to 200 like NCODE
! 22. 1990-10-23  Peterson   Include error and abort arguments on calls to
!                             HPALLOC.
! 21. 1989-02-16  Lazear     Moved bandpass filter to stack traces instead
!                             of base traces.
! 20. 1989-01-25  Lazear     Fix bug in base trace save by putting the
!                             parameter KSAVE in common.
! 19. 1989-12-13  Lazear     Move setup of base trace files after index
!                             sort,and fix dynamic allocation of excess
!                             scratch mem.  Save static files before trace
!                             output.
! 18. 1989-09-19  Lazear     remove restriction on windows due to max.shift
! 17. 1989-08-01  Lazear     Add option for shift header word (SHDR)
! 16. 1989-07-20  Lazear     Move conversion of MXSH to sec. after NCODE
! 15. 1989-05-26  Lazear     Add CMP grid definition parameters
! 14. 1989-05-05  Lazear     Add base trace file I/O.
! 13. 1989-05-02  Lazear     Install code for CCHDR and new base forming
!                             routines.
! 12. 1989-04-26  Lazear     Add METHOD=SDIP,#XICB,#YICB,#XGAP,#YGAP,DELTAX
!                             DELTAY,TSLC parameters, and change GP header
!                             specification.
! 11. 1989-03-21  Lazear     Add IRLS solution.
! 10. 1989-03-17  Lazear     Keep track of GP increments.
!  9. 1989-03-15  Lazear     Correct bug in solution I/O
!  8. 1989-01-23  Lazear     Modify for new DTREAD and DTWRITE calls.
!  7. 1988-11-30  Lazear     Add CMP and Residual NMO terms to Gauss-Siedel
!                             solution, and apply running average removal
!                             filter to static soultions after each IMS
!                             iteration.
!  6. 1988-11-14  Lazear     Removed grid-GP calculation,put in static file
!                             creation calls.
!  5. 1988-10-20  Lazear     Rewritten for better handling of 3D and to
!                             allow data in any order.
!  4. 1988-10-05  Lazear     Converted to new CPS call sequence and globals
!  3. 1988-08-26  Lazear     Change to single trace buffer and eliminate
!                             one trace write to disk.
!  2. 1988-08-25  Lazear     Added #TRJ parameter for disk file size, and
!                             add cumulative static arrays CSTATS and CSTATR
!  1. 1988-08-18  Lazear     Original Version.
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
!<NS IMS Process/NC=80>
!       Iterated Static Estimation and Surface Consistent Solution
!
!<include stathelper.f90>
!
!                     Static Calculation Parameters
! `---------------------------------------------------------------------------
!       NUM_ITER~~=`I              HDR_CC~~=`I           REPLACE =`CC
!       MAX_STATIC=`FFFFF ms       CC_MIN~~=`FFFF        USE_CMP =`CC
!       CONVERGE~~=`FFFFF ms       NUM_IRLS=`II          USE_RNMO=`CC
! `---------------------------------------------------------------------------
!
!<NS Base Trace Specifications/NC=80>
!<include basetrace.f90>
!
!<NS Correlation Window Specifications/NC=80>
!<include latwin.f90>
!
!<PARMS Base Trace Specifications         [screen2]>
!<PARMS Correlation Window Specifications [screen3]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!
!<Help KEYWORD="NUM_ITER">
!<Tip> Number of iterations to do to solve the statics. </Tip>
! Default =  6
! Allowed =  int >= 1
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
!<Help KEYWORD="CC_MIN">
!<Tip> Minimum correlation coefficient to use for any trace. </Tip>
! Default = 0.0
! Allowed = 0.0 <= real < 1.0
!
! If the correlation coefficient for an individual correlation is less than
! CC_MIN, then the static pick from that correlation will not be used.
!</Help>
!
!
!<Help KEYWORD="REPLACE">
!<Tip> Replace the stacked trace with the base trace? </Tip>
! Default = NO
! Allowed = YES/NO
!
! Whether to replace the stacked trace with the corresponding base trace for
! forming subsequent base traces?
! (This has no effect if the base traces are being read from a file.)
!</Help>
!
!
!<Help KEYWORD="HDR_CC">
!<Tip> Header word used to store the trace's correlation coefficient. </Tip>
! Default = 0
! Allowed = 0 - NWIH
!
! HDR_CC is the header word used to store the trace's correlation coefficient
! on the last iteration.
!
! If HDR_CC = 0, then do not store the correlation coefficient.
!</Help>
!
!
!<Help KEYWORD="USE_CMP">
!<Tip> Whether to use a CMP static term in the Gauss-Seidel solution. </Tip>
! Default = NO
! Allowed = YES/NO
!
! IMS can include a separate CMP static term in the Gauss-Seidel iterations.
!</Help>
!
!
!<Help KEYWORD="USE_RNMO">
!<Tip> Whether to use a residual NMO term in the Gauss-Seidel solution. </Tip>
! Default = NO
! Allowed = YES/NO
!
! IMS can include a separate residual NMO term in the Gauss-Seidel iterations.
! This term varies with offset only.
!</Help>
!
!
!<Help KEYWORD="NUM_IRLS">
!<Tip> Number of Iteratively Reweighted Least Squares solutions. </Tip>
! Default = 0
! Allowed = int >= 0
!
! The additional IRLS solutions eliminate outlying static shifts and produce a
! solution that approaches the median fit to the observed shifts.  The total
! number of Gauss-Seidel iterations used is NUM_ITER * (NUM_IRLS + 1).
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module ims_module
      use pc_module
      use named_constants_module
      use stathelper_module
      use basetrace_module
      use mth_module
      use pkutil_module
      implicit none
      private
      public :: ims_create
      public :: ims_initialize
      public :: ims_update
      public :: ims_delete
      public :: ims
      public :: ims_wrapup


      character(len=100),public,save :: IMS_IDENT = &
'$Id: ims.f90,v 1.45 2006/10/17 13:45:44 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: ims_struct              
 
        private
        logical                         :: skip_wrapup         ! wrapup flag.
        integer                         :: nwih                ! globals
        real                            :: dt                  ! globals
        integer                         :: num_iter            ! process params
        real                            :: max_static          ! process params
        real                            :: converge            ! process params
        real                            :: cc_min              ! process params
        integer                         :: num_irls            ! process params
        integer                         :: hdr_cc              ! process params
        logical                         :: replace             ! process params
        logical                         :: use_cmp             ! process params
        logical                         :: use_rnmo            ! process params
        type(stathelper_struct),pointer :: helper              ! dependent
        type(basetrace_struct) ,pointer :: basetrace           ! dependent
        real                            :: twin(2,1)           ! from stathelper
        integer                         :: hdr_x,hdr_y         ! from basetrace
        real                            :: x_init,y_init       ! from basetrace
        real                            :: x_inc,y_inc         ! from basetrace
        integer                         :: nxhalf,nyhalf,nbase ! from basetrace


      end type ims_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                 ,save :: lun         ! unit number for printing.
      type(ims_struct),pointer,save :: object      ! needed for traps.

      integer,parameter,private :: IMS_NGSI = 6    ! # gauss-seidel iterations.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine ims_create (obj)
      implicit none
      type(ims_struct),pointer :: obj       ! arguments

      allocate (obj)
      nullify (obj%helper) ! jpa
      nullify (obj%basetrace) ! jpa

      call stathelper_create   (obj%helper   , 'IMS')
      call basetrace_create    (obj%basetrace, 'IMS')
      call ims_initialize      (obj)
      return
      end subroutine ims_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine ims_delete (obj)
      implicit none
      type(ims_struct),pointer :: obj       ! arguments

      call ims_wrapup (obj)

      call stathelper_delete (obj%helper)
      call basetrace_delete  (obj%basetrace)

      deallocate(obj)
      return
      end subroutine ims_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine ims_initialize (obj)
      implicit none
      type(ims_struct),intent(inout) :: obj       ! arguments

      lun = pc_get_lun()

      obj%num_iter   = 6              ! was nit
      obj%max_static = 40.0           ! was mxsh
      obj%converge   = 1.0            ! did not exist
      obj%cc_min     = 0.0            ! was mncc
      obj%num_irls   = 0              ! was irls
      obj%hdr_cc     = 0              ! was icchdr
      obj%replace    = .false.        ! was irepl
      obj%use_cmp    = .false.        ! was cmpterm
      obj%use_rnmo   = .false.        ! was rmoterm

      call stathelper_initialize (obj%helper)
      call basetrace_initialize  (obj%basetrace)
      call ims_update            (obj)
      return
      end subroutine ims_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine ims_update (obj)
      implicit none
      type(ims_struct),intent(inout),target :: obj                   ! args
      logical                               :: error                 ! local
      integer         ,parameter            :: RA_SRC       = 1      ! local
      integer         ,parameter            :: RA_REC       = 1      ! local
      character(len=8),parameter            :: HDR_CORR_SRC = 'CMP'  ! local
      character(len=8),parameter            :: HDR_CORR_REC = 'CMP'  ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      call pc_get_global ('nwih'        , obj%nwih      )
      call pc_get_global ('dt'          , obj%dt        )

      call pc_get        ('num_iter'    , obj%num_iter  )
      call pc_get        ('max_static'  , obj%max_static)
      call pc_get        ('converge'    , obj%converge  )
      call pc_get        ('cc_min'      , obj%cc_min    )
      call pc_get        ('num_irls'    , obj%num_irls  )
      call pc_get        ('hdr_cc'      , obj%hdr_cc    )
      call pc_get        ('replace'     , obj%replace   )
      call pc_get        ('use_cmp'     , obj%use_cmp   )
      call pc_get        ('use_rnmo'    , obj%use_rnmo  )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      call mth_constrain (obj%num_iter  ,   1,        99)
      call mth_constrain (obj%max_static, 1.0,     999.0)
      call mth_constrain (obj%converge  , 0.0,      99.0)
      call mth_constrain (obj%cc_min    , 0.0,       0.9)
      call mth_constrain (obj%num_irls  ,   0,        99)
      call mth_constrain (obj%hdr_cc    ,   0,  obj%nwih)

      call stathelper_update (obj%helper,                                  &
                              obj%num_iter, obj%max_static, obj%converge,  &
                              RA_SRC, RA_REC, HDR_CORR_SRC, HDR_CORR_REC,  &
                              error)

      call ims_basetrace_update (obj, error)      


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put        ('num_iter'    , obj%num_iter  )
      call pc_put        ('max_static'  , obj%max_static)
      call pc_put        ('converge'    , obj%converge  )
      call pc_put        ('cc_min'      , obj%cc_min    )
      call pc_put        ('num_irls'    , obj%num_irls  )
      call pc_put        ('hdr_cc'      , obj%hdr_cc    )
      call pc_put        ('replace'     , obj%replace   )
      call pc_put        ('use_cmp'     , obj%use_cmp   )
      call pc_put        ('use_rnmo'    , obj%use_rnmo  )


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine ims_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!




!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine ims (obj,ntr,hd,tr)
      implicit none
      type(ims_struct),intent(inout) :: obj                    ! arguments
      integer         ,intent(inout) :: ntr                    ! arguments
      double precision,intent(inout) :: hd(:,:)                ! arguments
      real            ,intent(inout) :: tr(:,:)                ! arguments
      logical                        :: error                  ! local

      if(ntr /= NEED_TRACES) then
           call stathelper_input_traces (obj%helper,ntr,hd,tr)
           if (ntr == NEED_TRACES) return
      end if

      if (ntr == NO_MORE_TRACES) then
           call ims_solve (obj,error)
           if (error) then
                ntr = FATAL_ERROR
           else
                ntr = NEED_TRACES
           end if
      end if

      if (ntr == NEED_TRACES .or. ntr == FATAL_ERROR) then
           call ims_wrapup (obj)
           call stathelper_output_traces (obj%helper,ntr,hd,tr)
      end if
      return
      end subroutine ims


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine ims_wrapup (obj)
      implicit none
      type(ims_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call basetrace_wrapup  (obj%basetrace)

      return
      end subroutine ims_wrapup


!!-------------------------- ims solve ------------------------------------!!
!!-------------------------- ims solve ------------------------------------!!
!!-------------------------- ims solve ------------------------------------!!

! This routine is similar to FISH_SOLVE except that IMS_BUILD_STACKS and
! IMS_BUILD_BASES are called only once per iteration (rather than once for
! each static file at each iteration), and the loop over the static files
! is omitted.  Both static files are solved for at the same time in the
! IMS_ALGORITHM routine.


      subroutine ims_solve (obj,error)
      implicit none
      type(ims_struct),intent(inout) :: obj                    ! arguments
      logical         ,intent(out)   :: error                  ! arguments
      integer                        :: nfiles,nwin,ncorr      ! local
      integer                        :: ntraces,ncmp,ncoor8    ! local
      integer                        :: nxcmp,nycmp,ngp1,ngp2  ! local
      integer                        :: iter,ifile             ! local
      logical                        :: converged              ! local

      call stathelper_sort_to_cmps (obj%helper)

      nfiles  = stathelper_get_nfiles  (obj%helper)
      nwin    = stathelper_get_nwin    (obj%helper)
      ncorr   = stathelper_get_ncorr   (obj%helper)
      ntraces = stathelper_get_ntraces (obj%helper)
      ncmp    = stathelper_get_ncmp    (obj%helper)
      nxcmp   = stathelper_get_nxcmp   (obj%helper)
      nycmp   = stathelper_get_nycmp   (obj%helper)
      ngp1    = stathelper_get_ngp     (obj%helper,1)
      ngp2    = stathelper_get_ngp     (obj%helper,2)
      ncoor8  = pkutil_npack8          (ncorr)

      do iter = 1,obj%num_iter

        call stathelper_begin_iteration (obj%helper, error)
        if (error) return

        call basetrace_begin_iteration (obj%basetrace, error)
        if (error) return

        do ifile = 1,nfiles
           call stathelper_begin_file_iteration (obj%helper, ifile, error)
           if (error) return
        end do

        call ims_build_stacks (obj, nwin, ntraces, ncmp, error)
        if (error) then
             call pc_error ('IMS: error building stacked traces')
             return
        end if

        call ims_build_bases (obj, nwin, ncmp, nxcmp, nycmp, error)
        if (error) then
             call pc_error ('IMS: error building base traces')
             return
        end if

        call ims_algorithm &
          (obj, nfiles, nwin, ncorr, ncoor8, ntraces, ncmp, ngp1, ngp2, error)
        if (error) then
             call pc_error ('IMS: error executing solution algorithm')
             return
        end if

        do ifile = 1,nfiles
           call stathelper_end_file_iteration (obj%helper, ifile, error)
           if (error) return
        end do

        call basetrace_end_iteration (obj%basetrace, error)
        if (error) return

        call stathelper_end_iteration (obj%helper, converged, error)
        if (error .or. converged) return

      end do

      error = .false.
      return
      end subroutine ims_solve


!!---------------------- ims basetrace update ----------------------------!!
!!---------------------- ims basetrace update ----------------------------!!
!!---------------------- ims basetrace update ----------------------------!!


      subroutine ims_basetrace_update  (obj, error)      
      implicit none
      type(ims_struct),intent(inout) :: obj                      ! arguments
      logical                        :: error                    ! arguments
      integer                        :: ndpt,nwin,itwin,ibwin    ! local
      real                           :: tstrt,dt                 ! local
      integer                        :: bins_x,bins_y            ! local

      nwin  = stathelper_get_nwin  (obj%helper)
      itwin = stathelper_get_itwin (obj%helper)
      ibwin = stathelper_get_ibwin (obj%helper)

      call pc_get_global ('ndpt'   , ndpt    )
      call pc_get_global ('tstrt'  , tstrt   )
      call pc_get_global ('dt'     , dt      )

      obj%twin(1,1) = tstrt + (itwin-1) * dt
      obj%twin(2,1) = tstrt + (ibwin-1) * dt

      call pc_put_global ('ndpt'   , nwin         )        ! for basetrace.
      call pc_put_global ('tstrt'  , obj%twin(1,1))        ! for basetrace.

      call basetrace_update  (obj%basetrace, obj%max_static,  &
                              obj%num_iter, error,            &
                              hdr_x  = obj%hdr_x ,            &
                              hdr_y  = obj%hdr_y ,            &
                              x_init = obj%x_init,            &
                              y_init = obj%y_init,            &
                              x_inc  = obj%x_inc ,            &
                              y_inc  = obj%y_inc ,            &
                              bins_x =     bins_x,            &
                              bins_y =     bins_y)
      obj%nxhalf = bins_x / 2
      obj%nyhalf = bins_y / 2
      obj%nbase  = bins_x * bins_y

      call pc_put_global ('ndpt'   , ndpt )         ! restored.
      call pc_put_global ('tstrt'  , tstrt)         ! restored.
      return
      end subroutine ims_basetrace_update


!!---------------------- ims basetrace build ----------------------------!!
!!---------------------- ims basetrace build ----------------------------!!
!!---------------------- ims basetrace build ----------------------------!!


      subroutine ims_basetrace_build (obj, icmp, icmp2, trmix,   &
                                      nwin, ntr,                 &
                                      trbase,error)
      implicit none
      type(ims_struct)   ,intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: nwin,ntr              ! arguments
      integer            ,intent(in)    :: icmp                  ! arguments
      integer            ,intent(in)    :: icmp2      (ntr)      ! arguments
      real               ,intent(inout) :: trmix (nwin,ntr)      ! arguments
      real               ,intent(out)   :: trbase(nwin)          ! arguments
      logical                           :: error                 ! arguments
      integer,parameter                 :: NWT = 1               ! local
      double precision                  :: hdmix(obj%nwih,ntr)   ! local
      integer                           :: i,nxcmp,mid           ! local
      integer                           :: ixcmp,iycmp           ! local

      if (ntr == 0) then
           trbase(1:nwin) = 0.0
           return
      end if

      nxcmp = stathelper_get_nxcmp (obj%helper)
      mid   = 1

      do i = 1,ntr
           call mth_split_index (icmp2(i),nxcmp,  ixcmp,iycmp)
           hdmix        (:,i) = 0.0
           hdmix(1        ,i) = i
           hdmix(3        ,i) = icmp
           hdmix(4        ,i) = i
           hdmix(obj%hdr_x,i) = obj%x_init + (ixcmp - 1) * obj%x_inc
           hdmix(obj%hdr_y,i) = obj%y_init + (iycmp - 1) * obj%y_inc
           if (icmp2(i) == icmp) mid = i
      end do

      call basetrace_build (obj%basetrace, hdmix, trmix, &
                            ntr, mid,                    &
                            obj%twin, NWT,               &
                            hdmix(1:,mid),trbase,error)
      if (error) then
           call pc_error ('IMS: error building base trace')
      end if
      return
      end subroutine ims_basetrace_build


!!---------------------- ims build stacks ---------------------------------!!
!!---------------------- ims build stacks ---------------------------------!!
!!---------------------- ims build stacks ---------------------------------!!

! This routine is the same as FISH_BUILD_STACKS except that the stacked
! trace is normalized by the fold point by point, then normalized as a whole,
! before saving to disk.  The new lines of code are identified with !NORM.


      subroutine ims_build_stacks (obj,nwin,ntraces,ncmp,error)
      implicit none
      type(ims_struct)   ,intent(inout) :: obj                  ! arguments
      integer            ,intent(in)    :: nwin,ntraces,ncmp    ! arguments
      logical            ,intent(out)   :: error                ! arguments
      real                              :: aaa (nwin)           ! local
      real                              :: bbb (nwin)           ! local
      real                              :: fold(nwin)           ! local  !NORM
      integer                           :: icmp,icmp2,ifold     ! local
      integer                           :: itrace               ! local

      itrace = 1

      do icmp = 1,ncmp
           bbb (:) = 0.0
           fold(:) = 0.0                                                 !NORM
           ifold   = 0
           do
                if (itrace > ntraces) exit
                icmp2   = stathelper_get_icmp (obj%helper,itrace)
                if (icmp2 > icmp) exit
                call stathelper_read_shifted_window &
                                      (obj%helper,itrace,aaa,error)
                if (error) return
                where (aaa(:) /= 0.0) fold(:) = fold(:) + 1.0            !NORM
                bbb(:) = bbb(:) + aaa(:)
                ifold  = ifold  + 1
                itrace = itrace + 1
           end do
           bbb(:) = bbb(:) * sqrt(fold(:))                               !NORM
           call mth_amplitude_normalize (bbb,bbb,nwin)                   !NORM
           call stathelper_write_stacked_window &
                                      (obj%helper,icmp,ifold,bbb,error)
           if (error) return
      end do

      error = .false.
      return
      end subroutine ims_build_stacks


!!---------------------- ims build bases ---------------------------------!!
!!---------------------- ims build bases ---------------------------------!!
!!---------------------- ims build bases ---------------------------------!!


      subroutine ims_build_bases (obj,nwin,ncmp,nxcmp,nycmp,error)
      implicit none
      type(ims_struct)   ,intent(inout) :: obj                    ! arguments
      integer            ,intent(in)    :: nwin,ncmp,nxcmp,nycmp  ! arguments
      logical            ,intent(out)   :: error                  ! arguments

      integer           :: icmp2   (obj%nbase)                    ! local
      real              :: bbb(nwin,obj%nbase)                    ! local
      real              :: ccc(nwin)                              ! local
      real              :: fold(nwin)                             ! local
      integer           :: icmp,ixcmp,iycmp                       ! local
      integer           :: ifoldb,ifoldc,ntr                      ! local
      integer           :: ixmin,ixmax,iymin,iymax                ! local

      do icmp = 1,ncmp

           call mth_split_index (icmp,nxcmp,  ixcmp,iycmp)

           ixmin = max(ixcmp - obj%nxhalf,1)
           ixmax = min(ixcmp + obj%nxhalf,nxcmp)
           iymin = max(iycmp - obj%nyhalf,1)
           iymax = min(iycmp + obj%nyhalf,nycmp)

           if (obj%replace) then
                ifoldc = 0
                fold(:) = 0.0 
           end if

           ntr = 0

           do iycmp = iymin,iymax
           do ixcmp = ixmin,ixmax

                ntr        = ntr + 1
                icmp2(ntr) = ixcmp + (iycmp -1) * nxcmp

                call stathelper_read_stacked_window &
                              (obj%helper,icmp2(ntr),ifoldb,bbb(1:,ntr),error)
                if (error) return

                if (obj%replace) then
                     ifoldc = ifoldc + ifoldb
                     where (bbb(:,ntr) /= 0.0) fold(:) = fold(:) + 1.0 
                end if

           end do
           end do

           call ims_basetrace_build (obj, icmp, icmp2, bbb,   &
                                     nwin, ntr,               &
                                     ccc, error)
           if (error) return

           call stathelper_write_base_window &
                                  (obj%helper,icmp,ntr,ccc,error)
           if (error) return

           if (obj%replace) then
                ccc(:) = ccc(:) * sqrt(fold(:)) 
                call mth_amplitude_normalize (ccc,ccc,nwin)
                call stathelper_write_stacked_window &
                                    (obj%helper,icmp,ifoldc,ccc,error)
                if (error) return
           end if

      end do

      error = .false.
      return
      end subroutine ims_build_bases


!!------------------------- ims algorithm ---------------------------------!!
!!------------------------- ims algorithm ---------------------------------!!
!!------------------------- ims algorithm ---------------------------------!!


      subroutine ims_algorithm &
           (obj, nfiles, nwin, ncorr, ncorr8, ntraces, ncmp, ngp1, ngp2, error)
      implicit none
      type(ims_struct)   ,intent(inout) :: obj                      ! arguments
      integer            ,intent(in)    :: nfiles,nwin,ncorr,ncorr8 ! arguments
      integer            ,intent(in)    :: ntraces,ncmp,ngp1,ngp2   ! arguments
      logical            ,intent(out)   :: error                    ! arguments

      integer                           :: ifile,nnt,ifold          ! local
      integer                           :: itrace,icmp,icmpkeep     ! local
      integer                           :: ngp,igp,indx             ! local
      real                              :: denom,stat,ccoef ! local

      real                              :: corr  (ncorr)            ! local
      real                              :: corr2 (ncorr)            ! local
      real                              :: win   (nwin )            ! local
      real                              :: base  (nwin )            ! local
      real                              :: static(ntraces)          ! local

      integer                           :: corrs(ncorr8,ngp1+ngp2)  ! local
      real                              :: denoms      (ngp1+ngp2)  ! local
      integer                           :: nnts        (ngp1+ngp2)  ! local
      real                              :: stats       (ngp1+ngp2)  ! local

!integer igp1,igp2,iaoff

!----------get started.

      static  (:) = 0.0
      corrs (:,:) = 0
      denoms  (:) = 0.0
      nnts    (:) = 0
      stats   (:) = 0.0

!----------start looping through traces.

      icmpkeep = -9999

      do itrace = 1,ntraces

!----------get next trace.

           call stathelper_read_shifted_window (obj%helper,itrace,win,error)
           if (error) return

!----------get corresponding base trace if necessary.

           icmp = stathelper_get_icmp (obj%helper,itrace)
           if (icmp /= icmpkeep) then
                call stathelper_read_base_window &
                                   (obj%helper,icmp,ifold,base,error)
                if (error) return
                icmpkeep = icmp
           end if

!----------perform and pick the correlation.

           call stathelper_corr (obj%helper,win,base,corr,denom)
           call stathelper_pick (obj%helper,corr,obj%cc_min,denom,  stat,ccoef)

!igp1  = stathelper_get_this_igp (obj%helper,1,itrace)
!igp2  = stathelper_get_this_igp (obj%helper,2,itrace)
!iaoff = stathelper_get_iaoff    (obj%helper,itrace)
!print *, 'itrace,icmp,igp1,igp2,iaoff,denom,stat,ccoef = ', &
!          itrace,icmp,igp1,igp2,iaoff,denom,stat,ccoef

           if (stat == FNIL) cycle

!----------update the stacked correlations for this trace.

           do ifile = 1,nfiles

                igp = stathelper_get_this_igp (obj%helper,ifile,itrace)

                indx = igp
                if (ifile == 2) indx = ngp1 + igp

                nnts  (indx) = nnts  (indx) + 1
                denoms(indx) = denoms(indx) + denom

                call pkutil_unpack8 (corrs(:,indx), ncorr8, corr2, ncorr)
                corr2(:) = corr2(:) + corr(:)
                call pkutil_pack8   (corr2, ncorr, corrs(:,indx), ncorr8)

           end do

!----------store the static value for this trace.

           static(itrace) = stat

!----------finish looping through traces.

      end do

!----------solve the statics.

      call ims_gauss_seidel (obj,ntraces,nfiles,ncmp,ngp1,ngp2,static, &
                             stats(1:ngp1),stats(ngp1+1:ngp1+ngp2))

!----------report the static values.

      do ifile = 1,nfiles

           ngp = stathelper_get_ngp (obj%helper,ifile)

           do igp = 1,ngp

                indx = igp
                if (ifile == 2) indx = ngp1 + igp

                call pkutil_unpack8 (corrs(:,indx), ncorr8, corr, ncorr)
                denom = denoms(indx)
                nnt   = nnts  (indx)

                call stathelper_pick &
                             (obj%helper,corr,0.0,denom,  stat,ccoef)
                                  ! the above is called only to get ccoef.

!print *, 'ifile,igp,indx,denom,stat,ccoef,nnt,stats(indx) = ', &
!          ifile,igp,indx,denom,stat,ccoef,nnt,stats(indx)
                stat = stats(indx)   ! override stat from stathelper_pick.

                call stathelper_report_static &
                        (obj%helper,ifile,igp,corr,stat,ccoef,nnt,nnt,error)
                if (error) return

           end do

      end do

!----------finish up.

      error = .false.
      return
      end subroutine ims_algorithm


!!-------------------------- ims gauss seidel -------------------------------!!
!!-------------------------- ims gauss seidel -------------------------------!!
!!-------------------------- ims gauss seidel -------------------------------!!

! This routine loops over reweighted least square calculations,
! which then loop over gauss-seidel iterations.


      subroutine ims_gauss_seidel &
                     (obj,ntraces,nfiles,ncmp,ngp1,ngp2,static,stat1,stat2)
      implicit none
      type(ims_struct)   ,intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: ntraces,nfiles,ncmp   ! arguments
      integer            ,intent(in)    :: ngp1,ngp2             ! arguments
      real               ,intent(in)    :: static(ntraces)       ! arguments
      real               ,intent(out)   :: stat1 (ngp1)          ! arguments
      real               ,intent(out)   :: stat2 (ngp2)          ! arguments

      real              :: weight(ntraces)
      real              :: bsmt  (ncmp)  
      real              :: rmo   (ncmp) 
      real              :: denom (max(ncmp,ngp1,ngp2))

      integer           :: itrace,igsi,igp1,igp2,iaoff,icmp,icmpkeep,nirls
      real              :: armo,vn,vnt,vd,vdt,vq,vqt,vx,vxt,vy,vyt,x
      real              :: resid,epsilon

!----------start looping over reweighted least square calculations.
!print *, ' '
!print *, 'static = ',static
!print *, ' '

      weight(:) = 1.0

      do nirls = 1,obj%num_irls+1
!print *, ' '
!print *, 'nirls = ',nirls
!print *, ' '

!----------start looping over gauss-seidel iterations.

      stat1  (:) = 0.0
      stat2  (:) = 0.0
      bsmt   (:) = 0.0
      rmo    (:) = 0.0
      denom  (:) = 0.0
      armo       = 0.0

      do igsi= 1,IMS_NGSI
!print *, ' '
!print *, 'igsi = ',igsi
!print *, ' '

!----------test for a cmp term in solution.

      if (obj%use_cmp) then
 
        bsmt (:) = 0.0
        denom(:) = 0.0
 
        do itrace = 1,ntraces

          igp1  = stathelper_get_this_igp (obj%helper,1,itrace)
          igp2  = stathelper_get_this_igp (obj%helper,2,itrace)
          iaoff = stathelper_get_iaoff    (obj%helper,itrace)
          icmp  = stathelper_get_icmp     (obj%helper,itrace)

          call ims_private_verify ('igp1 (solving for CMP term)',igp1,ngp1)
          call ims_private_verify ('igp2 (solving for CMP term)',igp2,ngp2)
          call ims_private_verify ('icmp (solving for CMP term)',icmp,ncmp)

          resid = static(itrace) - stat1(igp1) - stat2(igp2) - armo*(iaoff**2)

          bsmt (icmp)  = bsmt (icmp) + resid * weight(itrace)
          denom(icmp)  = denom(icmp) +         weight(itrace)

        end do
 
        where (denom(:) > 0.0) bsmt(:) = bsmt(:) / denom(:)
           
      end if

!----------test for a residual nmo term in solution.

      if (obj%use_rnmo) then

        rmo(:)   = 0.0
        icmpkeep = 1
        vn       = 0.0
        vnt      = 0.0
        vd       = 0.0
        vdt      = 0.0
        vq       = 0.0
        vqt      = 0.0
        vx       = 0.0
        vxt      = 0.0
        vy       = 0.0
        vyt      = 0.0
 
        do itrace = 1,ntraces

          igp1  = stathelper_get_this_igp (obj%helper,1,itrace)
          igp2  = stathelper_get_this_igp (obj%helper,2,itrace)
          iaoff = stathelper_get_iaoff    (obj%helper,itrace)
          icmp  = stathelper_get_icmp     (obj%helper,itrace)

          call ims_private_verify ('igp1 (solving for residual NMO)',igp1,ngp1)
          call ims_private_verify ('igp2 (solving for residual NMO)',igp2,ngp2)
          call ims_private_verify ('icmp (solving for residual NMO)',icmp,ncmp)

          resid = static(itrace) - stat1(igp1) - stat2(igp2) - bsmt(icmp)

      ! rmo is computed only for printing at this time:

          if (icmp /= icmpkeep) then
            vnt      = vnt + vn
            vdt      = vdt + vd
            vyt      = vyt + vy
            vxt      = vxt + vx
            vqt      = vqt + vq
            icmpkeep = icmp
            vn       = 0.0
            vd       = 0.0
            vy       = 0.0
            vx       = 0.0
            vq       = 0.0
          end if
          
          x  = iaoff**2
          vn = vn + resid * x * weight(itrace)
          vd = vd + x     * x * weight(itrace)
          vy = vy + resid     * weight(itrace)
          vx = vx +         x * weight(itrace)
          vq = vq +             weight(itrace)
          
        end do
 
 
   ! rmo computed only for printing at this time:

        vnt  = vnt + vn
        vdt  = vdt + vd
        vyt  = vyt + vy
        vxt  = vxt + vx
        vqt  = vqt + vq
        x    = (vqt*vdt-vxt*vxt)
        armo = 0.0
        if (x /= 0.0) armo = (vqt*vnt-vxt*vyt) / x
 
!        if (igsi == IMS_NGSI) then
!          call pc_print (' ')
!          call pc_print &
!   ('AVERAGE RESIDUAL NMO TERM (IN MS WHEN MULTIPLIED BY OFFSET SQUARED) = ', &
!                                         armo*obj%dt*1000.0)
!          call pc_print (' ')
!        end if
 
      end if

!----------solve for the first static file (stat1):

      if (ngp1 > 0) then

        stat1(:) = 0.0
        denom(:) = 0.0

        do itrace = 1,ntraces

          igp1  = stathelper_get_this_igp (obj%helper,1,itrace)
          igp2  = stathelper_get_this_igp (obj%helper,2,itrace)
          iaoff = stathelper_get_iaoff    (obj%helper,itrace)
          icmp  = stathelper_get_icmp     (obj%helper,itrace)

          call ims_private_verify ('igp1 (solving for file one)',igp1,ngp1)
          call ims_private_verify ('igp2 (solving for file one)',igp2,ngp2)
          call ims_private_verify ('icmp (solving for file one)',icmp,ncmp)

          resid = static(itrace) - stat2(igp2) - bsmt(icmp) - armo*(iaoff**2)

          stat1(igp1) = stat1(igp1) + resid * weight(itrace)
          denom(igp1) = denom(igp1) +         weight(itrace)

        end do

        where (denom(:) > 0.0)
            stat1(:) = stat1(:) / denom(:)
        elsewhere
            stat1(:) = FNIL
        end where
           
      end if

!----------solve for the second static file (stat2):

      if (ngp2 > 0) then

        stat2(:) = 0.0
        denom(:) = 0.0

        do itrace = 1,ntraces

          igp1  = stathelper_get_this_igp (obj%helper,1,itrace)
          igp2  = stathelper_get_this_igp (obj%helper,2,itrace)
          iaoff = stathelper_get_iaoff    (obj%helper,itrace)
          icmp  = stathelper_get_icmp     (obj%helper,itrace)

          call ims_private_verify ('igp1 (solving for file 2)',igp1,ngp1)
          call ims_private_verify ('igp2 (solving for file 2)',igp2,ngp2)
          call ims_private_verify ('icmp (solving for file 2)',icmp,ncmp)

          resid = static(itrace) - stat1(igp1) - bsmt(icmp) - armo*(iaoff**2)

          stat2(igp2) = stat2(igp2) + resid * weight(itrace)
          denom(igp2) = denom(igp2) +         weight(itrace)

        end do

        where (denom(:) > 0.0)
            stat2(:) = stat2(:) / denom(:)
        elsewhere
            stat2(:) = FNIL
        end where
           
      end if

!----------finish looping over gauss-seidel iterations.

!print *, ' '
!print *, 'armo  = ',armo 
!print *, ' '
!print *, 'bsmt  = ',bsmt 
!print *, ' '
!print *, 'stat1 = ',stat1
!print *, ' '
!print *, 'stat2 = ',stat2
!print *, ' '
      end do

!----------recalculate the weights.

      epsilon = obj%dt * 1000.0

      do itrace = 1,ntraces

        igp1  = stathelper_get_this_igp (obj%helper,1,itrace)
        igp2  = stathelper_get_this_igp (obj%helper,2,itrace)
        iaoff = stathelper_get_iaoff    (obj%helper,itrace)
        icmp  = stathelper_get_icmp     (obj%helper,itrace)

        call ims_private_verify ('igp1 (recalculating weights)',igp1,ngp1)
        call ims_private_verify ('igp2 (recalculating weights)',igp2,ngp2)
        call ims_private_verify ('icmp (recalculating weights)',icmp,ncmp)

        resid = static(itrace) - stat1(igp1) - stat2(igp2) &
                                         - bsmt(icmp) - armo*(iaoff**2)
        resid = max(abs(resid),epsilon)

        weight(itrace) = epsilon / resid

      end do
!print *, ' '
!print *, 'weight = ',weight
!print *, ' '

!----------finish looping over reweighted least square calculations.

      end do
      return
      end subroutine ims_gauss_seidel


!!------------------------- private verify --------------------------------!!
!!------------------------- private verify --------------------------------!!
!!------------------------- private verify --------------------------------!!


      subroutine ims_private_verify (label,ielement,nelements)
      implicit none
      character(len=*),intent(in) :: label
      integer         ,intent(in) :: ielement,nelements

      if (ielement < 1 .or. ielement > nelements) then
           write(lun,*) 'Index ',trim(label),' = ',ielement,' is illegal.'
           write(lun,*) 'This is a programming error in IMS.'
           stop
      endif
      return
      end subroutine ims_private_verify


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module ims_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

