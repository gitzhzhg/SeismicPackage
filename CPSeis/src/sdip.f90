!<CPS_v1 type="PROCESS"/>
!!------------------------------- sdip.f90 ---------------------------------!!
!!------------------------------- sdip.f90 ---------------------------------!!
!!------------------------------- sdip.f90 ---------------------------------!!


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
! Name       : SDIP           (Semblance Dip Mix)
! Category   : filters
! Written    : 1988-12-01   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Coherence enhancement by mixing along dominant dip direction.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! SDIP is a 2D coherence enhancement process that operates on stacked or
! migrated lines.  Each input trace is replaced by a mix of itself and its
! neighbors.  Traces are mixed along the direction of dominant dip as
! determined by a semblance search within small windows down the trace.
!
! SDIP uses the MGATHER primitive to maintain a moving gather of traces,
! and the SDIPUTIL primitive to do the dip search and calculate the output
! traces.
!
!-------------------------------------------------------------------------------
!                         DETAILS OF OPERATION
!
! 1. Within each window, (2*NUM_TR_DIP + 1) traces, centered on the input trace,
!    are used to perform a semblance dip search to determine which dip has
!    the maximum semblance.  A total of (2*DIP_MAX + 1) dips are tested.  The
!    dip with the maximum semblance is referred to as the dominant dip.  Each
!    window length is WIN_LEN seconds long, and the windows move down the trace
!    one sample at a time.
!
!
! 2. For each window, (2*NUM_TR_MIX + 1) traces, centered on the input trace,
!    are weighted by the taper specified by OPT_TAPER and then mixed along the
!    dominant dip to form a mixed trace.
!
!
! 3. The output trace is formed from the mixed trace, sample by sample, using
!    one of the following two equations:
!
!    If OPT_SEMB = NO, then only the mixed trace is weighted by the
!    semblance and the output trace is formed as follows:
!
!      TR_OUT =   TR_IN * FCTR_MIX  +  TR_MIX * (1-FCTR_MIX) * SEMB**PWR_SEMB
!
!    If OPT_SEMB = YES, then the input trace and the mixed trace are both
!    weighted by the semblance and the output trace is formed as follows:
!
!      TR_OUT = [ TR_IN * FCTR_MIX + TR_MIX * (1-FCTR_MIX) ] * SEMB**PWR_SEMB
!
!    where:
!
!      TR_OUT   = Output trace.
!      TR_IN    = Input trace.
!      TR_MIX   = Mixed trace.
!      FCTR_MIX = Weighting factor between input and mixed trace.
!      SEMB     = Local semblance measured along the dominant dip.
!      PWR_SEMB = Power to raise the semblance for semblance weighting.
!
!    OPT_SEMB = YES results in an output trace that is more strongly modulated
!    by the local semblance than if OPT_SEMB = NO.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! Process is a multiple-trace process.
! Process requires traces to be input as 2D lines.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! This process alters input traces.
! This process outputs the same traces as it receives (altered).
! This process outputs one trace at a time.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       set to 1
! GATHERED  whether traces are a legitimate gather  set to false
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
! Hwd#       Description                Action taken
! ----       -----------                ------------
! 2          Head mute index            used but not changed
! 25         LAV                        reset
! 64         Tail mute index            used but not changed
! HDR_LINE   Line number                used but not changed
! 58,59,60   Scratch                    used
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!014. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
! 13. 2002-03-11  Stoeckley  Set limits on number of displayed digits in GUI
!                             for floating point numbers to correct an
!                             occasional GUI display problem.
! 12. 2001-12-11  Stoeckley  Change the DIP_INC_X and DIP_INC_Y parameters
!                             to be informational only (now automatically
!                             calculated to protect against aliasing);
!                             put output diagnostics onto separate trace
!                             files so that they can all be obtained at the
!                             same time, improve the dip search algorithm.
! 11. 2001-08-17  Stoeckley  Move some code to the new SDIPUTIL and TWEIGHTS
!                             primitives; add optional diagnostic output.
! 10. 2001-06-11  Stoeckley  Change wrapup flag.
!  9. 2000-11-27  Stoeckley  Add missing required documentation sections.
!  8. 2000-04-25  Stoeckley  Converted from old system.
!  7. 1998-11-05  Goodger    Begin using the fortran90 compiler.
!  6. 1996-06-03  Vunderink  Added call to MUTEHW to mute input traces.
!  5. 1996-05-30  Vunderink  Added call to MUTEHW to mute mixed trace.
!  4. 1990-07-09  Stoeckley  Add parameter MT2.
!  3. 1989-05-01  Stoeckley  Changed call to modified primitive SEMDIP.
!  2. 1989-04-11  Stoeckley  Fix bug in SDIPSCL.
!  1. 1988-12-01  Stoeckley  Original Version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!             PORTABILITY ISSUES WITH THE PORTLAND GROUP COMPILER
!
! Calling MUTEHW from SDIP uncovered a bug in the Portland Group compiler.
! To get around this bug, the colons were replaced by asterisks in the HD
! and TR declarations in the main MUTEHW subroutine.  This bug was manifested
! during execution when MUTEHW was called from SDIP as follows (04-20-2000
! and 04-24-2000):
!
!   call mutehw (obj%hdgather(:,i),obj%trgather(:,i),obj%ndpt,0.0,MUTEHW_BOTH)
!
! The bug manifested itself in ways which changed when new executables were
! created.  Originally, a segmentation fault occurred.  After print messages
! were introduced, a message was printed out before termination; I think the
! message was a "COPY_IN" message but although it was reproducible on 04-20,
! I can no longer reproduce it (with a new executable) on 04-24).  Now,
! without print messages, a segmentation fault occurs, but with print messages,
! the program runs to completion but only executes a DO loop (from which
! MUTEHW is called) once.
!
! This bug could be programmed around this way (which however eventually
! failed part way through execution with an error message which I think was
! different from the other one):
!
!        double precision ,pointer :: hdtemp(:,:)
!        real             ,pointer :: trtemp(:,:)
!
!        hdtemp => obj%hdgather
!        trtemp => obj%trgather
!
!        call mutehw (   hdtemp(:,i),   trtemp(:,i),obj%ndpt,0.0,MUTEHW_BOTH)
!
! This bug could also be programmed around this way (which is less efficient
! by requiring copies before and after):
!
!        double precision :: hdtemp2(obj%nwih)
!        real             :: trtemp2(obj%ndpt)
!
!        hdtemp2(1:obj%nwih) = obj%hdgather(1:obj%nwih,i)
!        trtemp2(1:obj%ndpt) = obj%trgather(1:obj%ndpt,i)
!
!        call mutehw (   hdtemp2    ,   trtemp2    ,obj%ndpt,0.0,MUTEHW_BOTH)
!
!        obj%hdgather(1:obj%nwih,i) = obj%hdtemp2(1:obj%nwih,i)
!        obj%trgather(1:obj%ndpt,i) = obj%trtemp2(1:obj%ndpt,i)
!
! Although this kind of behavior (different manifestations of the bug with
! different executables) seems typical of memory problems in the code, Purify
! was run on the code and showed no problems whatsoever.
!
! An almost identical bug with the Portland Group compiler was found in a
! completely different executable running different code (RTC calling MULTWIN).
! This bug also disappeared when changing (:) to (*) in MULTWIN.
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
! NSCRATCH         0       amount of temporary memory needed.       
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
!  NTR == 1              if this process is outputting traces.
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
!<NS SDIP Process/NC=80>
!                          2D Semblance Dip Mix
! 
! OPT_SEMB~~=`KKK    [/L]Whether to weight input trace by local semblance.
! PWR_SEMB~~=`FFFFFF [/L]Power of semblance for dip mix.
! FCTR_MIX~~=`FFFFFF [/L]Fraction of original trace to combine with mixed trace.
! WIN_LEN~~~=`FFFFFF [/L]Semblance window length in seconds for dip search.
! OPT_TAPER =`CCCCC  [/L]Type of taper to apply.
!
! DIP_MAX~~~=`FFFFFF [/L]Maximum dip in milliseconds per trace.
! dip_inc~~~=`XXXXX  [/L]Dip increment in milliseconds per trace.
! ndips~~~~~=`XXXXX  [/L]Total number of dips which will be tested.
!
! NUM_TR_DIP=`II     [/L]# traces on each side of center trace for dip testing.
! ngather~~~=`XX     [/L]Total # traces which will be used for dip testing.
!
! NUM_TR_MIX=`II     [/L]# traces on each side of center trace to mix.
! nmix~~~~~~=`XX     [/L]Total # traces which will be used for mixing.
!
! HDR_LINE~~=`I      [/L]Header word to identify lines.
! LINE_INIT =`FFFFFF [/L]Header value for center of first (or any) line.
! LINE_INC~~=`FFFFFF [/L]Increment between lines.
!
!<include sdiputil.f90>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="ndips" TYPE="DISPLAY_ONLY">
!<Tip> Total number of dips to be tested. </Tip>
!</Help>
!
!
!<Help KEYWORD="ngather" TYPE="DISPLAY_ONLY">
!<Tip> Total number of traces for dip testing. </Tip>
!</Help>
!
!
!<Help KEYWORD="nmix" TYPE="DISPLAY_ONLY">
!<Tip> Total number of traces to mix together. </Tip>
!</Help>
!
!
!<Help KEYWORD="OPT_SEMB">
!<Tip> Option whether to weight input trace by the local semblance value. </Tip>
! Default = NO
! Allowed = YES/NO
!
! If OPT_SEMB = NO, then only the mixed trace is weighted by the semblance:
!
!    TR_OUT = TR_IN * FCTR_MIX + TR_MIX * (1-FCTR_MIX) * SEMB**PWR_SEMB.
!
! If OPT_SEMB = YES, then both the input trace and the mixed trace are weighted
! by the semblance:
!
!    TR_OUT = [ TR_IN * FCTR_MIX + TR_MIX * (1-FCTR_MIX) ] * SEMB**PWR_SEMB.
!
! OPT_SEMB = YES results in an output trace that is more strongly modulated by
! the local semblance than if OPT_SEMB = NO.
!</Help>
!
!
!<Help KEYWORD="PWR_SEMB">
!<Tip> Raise local semblance to the PWR_SEMB power for trace weighting. </Tip>
! Default = 0.5
! Allowed = real >= 0.0
!</Help>
!
!
!<Help KEYWORD="FCTR_MIX">
!<Tip> Fraction of original trace to combine with mixed trace. </Tip>
! Default = 0.5
! Allowed = 0.0 <= real <= 1.0
!</Help>
!
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Length of the time window, in sec, for the dip search calculation. </Tip>
! Default = 0.1
! Allowed = real > 0.0
!
! WIN_LEN is used for dip search calculations prior to generating mixed traces.
!</Help>
!
!
!<Help KEYWORD="DIP_MAX">
!<Tip> Maximum dip, in ms/tr, to use in the dip search calculation. </Tip>
! Default = 10.0
! Allowed = real >= 0.0
!
! A total of NDIPS dips are tested, from -DIP_MAX to +DIP_MAX.
! Computer time is roughly proportional to the number of dips tested.
!</Help>
!
!
!<Help KEYWORD="dip_inc" TYPE="DISPLAY_ONLY">
!<Tip> Dip increment, in ms/tr, to use in the dip search calculation. </Tip>
! Default = 2.0
! Allowed = real > 0.0
!
! This value is calculated from the Nyquist frequency and the NUM_TR_DIP
! parameter to keep from aliasing the dip search on the farthest traces.
!</Help>
!
!
!<Help KEYWORD="NUM_TR_DIP">
!<Tip> Number of traces on each side of the center trace for dip testing. </Tip>
! Default = 3
! Allowed = integer >= 0
!
! A total of (2*NUM_TR_DIP + 1) traces are used in the dip search calculation.
!</Help>
!
!
!<Help KEYWORD="NUM_TR_MIX">
!<Tip> Number of traces on each side of the center trace to mix. </Tip>
! Default = 1
! Allowed = integer >= 0 and =< NUM_TR_DIP
!
! A total of (2*NUM_TR_MIX + 1) traces are mixed together.
!</Help>
!
!
!<Help KEYWORD="OPT_TAPER">
!<Tip> Type of taper to apply when forming the mixed trace. </Tip>
! Default = NONE
! Allowed = LINEAR
! Allowed = COSINE
! Allowed = NONE
!
! If OPT_TAPER = LINEAR or COSINE, then a taper is applied to traces to be
! mixed so that the center trace has the greatest weight and the farthest
! traces have the least weight.  Either a linear or cosine taper is used, as
! specified.
!
! If OPT_TAPER = NONE, then the traces to be mixed are all weighted uniformly.
!</Help>
!
!
!<Help KEYWORD="HDR_LINE">
!<Tip> Header word designating 2D lines. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!
! HDR_LINE is used to identify 2D lines that SDIP will operate on individually.
!</Help>
!
!
!<Help KEYWORD="LINE_INIT">
!<Tip> Value of HDR_LINE for the first (or any) 2D line. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the center of any bin designating a 2D line.
! This value does not have to correspond to the first actual 2D line.
!</Help>
!
!
!<Help KEYWORD="LINE_INC">
!<Tip> Increment of HDR_LINE between 2D lines. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the bin increment (or width) designating a 2D line.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module sdip_module
      use pc_module
      use named_constants_module
      use mgather_module
      use sdiputil_module
      use tweights_module
      implicit none
      private
      public :: sdip_create
      public :: sdip_initialize
      public :: sdip_update
      public :: sdip_delete
!<execute_only>
      public :: sdip            ! main execution (trace processing) routine.
      public :: sdip_wrapup
!</execute_only>


      character(len=100),public,save :: SDIP_IDENT = &
       '$Id: sdip.f90,v 1.14 2006/10/17 13:45:47 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: sdip_struct              
 
        private
        logical                  :: skip_wrapup      ! wrapup flag
        integer                  :: nwih,ndpt        ! globals  
        real                     :: tstrt,dt         ! globals  

        character(len=28)   :: opt_output            ! not in gui
        real                :: pwr_edge              ! not in gui
        real                :: pwr_semb              ! process parameters
        logical             :: opt_semb              ! process parameters
        real                :: fctr_mix              ! process parameters
        character(len=8)    :: opt_taper             ! process parameters
        real                :: win_len               ! process parameters
        integer             :: win_nsamp             ! not in gui
        real                :: dip_max_x             ! process parameters
        real                :: dip_max_y             ! not in gui
        integer             :: num_tr_dip_x          ! process parameters
        integer             :: num_tr_dip_y          ! not in gui
        integer             :: num_tr_mix_x          ! process parameters
        integer             :: num_tr_mix_y          ! not in gui
        integer             :: hdr_x                 ! not in gui
        integer             :: hdr_y                 ! process parameters
        real                :: x_init                ! not in gui
        real                :: y_init                ! process parameters
        real                :: x_inc                 ! not in gui
        real                :: y_inc                 ! process parameters
        integer             :: max_x_bins            ! not in gui
        integer             :: max_y_bins            ! not in gui
        logical             :: quick_dip_weights     ! not in gui
        logical             :: quick_dip_search      ! not in gui

        integer                       :: ngather     ! dependent
        type(mgather_struct) ,pointer :: mgather     ! dependent
        type(sdiputil_struct),pointer :: sdiputil    ! dependent

      end type sdip_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(sdip_struct),pointer,save :: object      ! needed for traps.

      integer,parameter      :: SCRATCH_NSX = HDR_SCRATCH_58
      integer,parameter      :: SCRATCH_NSY = HDR_SCRATCH_59
      integer,parameter      :: SCRATCH_NSW = HDR_SCRATCH_60

      integer,parameter      :: opt_taper_nopt  = 3

      character(len=8) ,save :: opt_taper_options  (opt_taper_nopt)

      data opt_taper_options  /'LINEAR','COSINE','NONE'/


      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine sdip_create (obj)
      implicit none
      type(sdip_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%mgather)
      nullify (obj%sdiputil) ! jpa

      call sdiputil_create (obj%sdiputil)
      call sdip_initialize (obj)
      return
      end subroutine sdip_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine sdip_delete (obj)
      implicit none
      type(sdip_struct),pointer :: obj       ! arguments

!<execute_only>
      call sdip_wrapup (obj)
!</execute_only>
      call sdiputil_delete (obj%sdiputil)

      deallocate(obj)
      return
      end subroutine sdip_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine sdip_initialize (obj)
      implicit none
      type(sdip_struct),intent(inout) :: obj       ! arguments

      obj%opt_output         = 'MIXED TRACES'  ! not in gui
      obj%pwr_edge           = 2.0             ! not in gui
      obj%pwr_semb           = 0.5
      obj%opt_semb           = .false.
      obj%fctr_mix           = 0.5
      obj%opt_taper          = 'NONE'
      obj%win_len            = 0.10
      obj%win_nsamp          = 5               ! not in gui
      obj%dip_max_x          = 10.0
      obj%dip_max_y          = 10.0            ! not in gui
      obj%num_tr_dip_x       = 3
      obj%num_tr_dip_y       = 3               ! not in gui
      obj%num_tr_mix_x       = 1
      obj%num_tr_mix_y       = 1               ! not in gui
      obj%hdr_x              = 7               ! not in gui
      obj%hdr_y              = 8
      obj%x_init             = 1.0             ! not in gui
      obj%y_init             = 1.0
      obj%x_inc              = 1.0             ! not in gui
      obj%y_inc              = 1.0
      obj%max_x_bins         = 1               ! not in gui
      obj%max_y_bins         = 1               ! not in gui
      obj%quick_dip_weights  = .false.         ! not in gui
      obj%quick_dip_search   = .false.         ! not in gui

      call sdiputil_initialize (obj%sdiputil)
      call sdip_update         (obj)
      return
      end subroutine sdip_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine sdip_update (obj)
      implicit none
      type(sdip_struct),intent(inout),target :: obj               ! arguments
      integer                    :: nstore,nscratch               ! local
      real,allocatable           :: weights(:)                    ! local
      integer                    :: nxgather,nxmix,nxdips         ! local
      integer                    :: nygather,nymix,nydips         ! local
      real                       :: dip_inc_x,dip_inc_y           ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      call pc_get_global ('nwih'       , obj%nwih)
      call pc_get_global ('ndpt'       , obj%ndpt)
      call pc_get_global ('tstrt'      , obj%tstrt)  
      call pc_get_global ('dt'         , obj%dt)  

      call pc_get        ('pwr_semb'   , obj%pwr_semb    )
      call pc_get        ('opt_semb'   , obj%opt_semb    )
      call pc_get        ('fctr_mix'   , obj%fctr_mix    )
      call pc_get        ('opt_taper'  , obj%opt_taper   )
      call pc_get        ('win_len'    , obj%win_len     )
      call pc_get        ('dip_max'    , obj%dip_max_x   )
      call pc_get        ('num_tr_dip' , obj%num_tr_dip_x)
      call pc_get        ('num_tr_mix' , obj%num_tr_mix_x)
      call pc_get        ('hdr_line'   , obj%hdr_y       )
      call pc_get        ('line_init'  , obj%y_init      )
      call pc_get        ('line_inc'   , obj%y_inc       )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      call sdiputil_update (obj%sdiputil, obj%opt_output,                 &
                 obj%pwr_edge, obj%pwr_semb, obj%opt_semb, obj%fctr_mix,  &
                 obj%win_len          , obj%win_nsamp       ,             &
                 obj%quick_dip_weights, obj%quick_dip_search,             &
                 obj%dip_max_x        , obj%dip_max_y       ,             &
                 obj%num_tr_dip_x     , obj%num_tr_dip_y    ,             &
                 obj%num_tr_mix_x     , obj%num_tr_mix_y    ,             &
                 obj%hdr_x            , obj%hdr_y           ,             &
                 obj%x_inc            , obj%y_inc           ,             &
                 obj%max_x_bins       , obj%max_y_bins      ,             &
                 nxgather             , nygather            ,             &
                 nxmix                , nymix               ,             &
                 nxdips               , nydips              ,             &
                 dip_inc_x            , dip_inc_y)

      obj%ngather = nxgather

      call tweights_verify (obj%opt_taper, nxgather, nxmix)


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


  call pc_put_options_field ('opt_taper', opt_taper_options, opt_taper_nopt)

      call pc_put         ('pwr_semb'   , obj%pwr_semb     ,7)
      call pc_put         ('opt_semb'   , obj%opt_semb       )
      call pc_put         ('fctr_mix'   , obj%fctr_mix     ,7)
      call pc_put         ('opt_taper'  , obj%opt_taper      )
      call pc_put         ('win_len'    , obj%win_len      ,7)
      call pc_put         ('dip_max'    , obj%dip_max_x    ,7)
      call pc_put         ('num_tr_dip' , obj%num_tr_dip_x   )
      call pc_put         ('num_tr_mix' , obj%num_tr_mix_x   )
      call pc_put         ('hdr_line'   , obj%hdr_y          )
      call pc_put         ('line_init'  , obj%y_init       ,7)
      call pc_put         ('line_inc'   , obj%y_inc        ,7)

      call pc_put_gui_only ('dip_inc'   , dip_inc_x      ,6,2)
      call pc_put_gui_only ('ndips'     , nxdips             )
      call pc_put_gui_only ('ngather'   , nxgather           )
      call pc_put_gui_only ('nmix'      , nxmix              )

      nscratch = nxgather * (obj%nwih + obj%ndpt)
      nstore   = mgather_store(obj%nwih,obj%ndpt,nxgather)

      call pc_put_control ('need_request', .true.)
      call pc_put_control ('need_label'  , .true.)
      call pc_put_control ('twosets'     , .true.)
      call pc_put_control ('nscratch'    , nscratch)
      call pc_put_control ('nstore'      , nstore)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      call mgather_delete (obj%mgather)

!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      allocate (weights(nxgather))

      call tweights_calculate (obj%opt_taper, weights, nxgather, nxmix)

      write(pc_get_lun(),*) 'SDIP: weights = ',weights

      call mgather_create (obj%mgather, obj%nwih, obj%ndpt, nxgather,    &
                           obj%hdr_y, obj%y_init, obj%y_inc,             &
                           weights,SCRATCH_NSX,SCRATCH_NSY,SCRATCH_NSW)

      deallocate (weights)

      call sdiputil_prepare (obj%sdiputil,SCRATCH_NSX,SCRATCH_NSY,SCRATCH_NSW)


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

!</execute_only>

      return
      end subroutine sdip_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

      subroutine sdip (obj,ntr,hdi,tri,hdo,tro)
      implicit none
      type(sdip_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(in)    :: hdi(:,:)               ! arguments
      real             ,intent(in)    :: tri(:,:)               ! arguments
      double precision ,intent(inout) :: hdo(:,:)               ! arguments
      real             ,intent(inout) :: tro(:,:)               ! arguments
      double precision        :: hdmove(obj%nwih,obj%ngather)   ! local
      real                    :: trmove(obj%ndpt,obj%ngather)   ! local
      integer                 :: mid                            ! local
      real                    :: xmid,ymid                      ! local

!----------get the mix gather.

      call mgather (obj%mgather,ntr,hdi,tri,hdmove,trmove)

      if (ntr == NEED_TRACES) return

      if (ntr == FATAL_ERROR) then
           call pc_error ('SDIP: FATAL ERROR IN MGATHER')
           call sdip_wrapup (obj)
           return
      end if

      if (ntr == NO_MORE_TRACES) then
           call sdip_wrapup (obj)
           return
      end if

!----------get output trace (or gather).

      mid  = (ntr+1)/2
      xmid = hdmove(SCRATCH_NSX,mid)
      ymid = hdmove(SCRATCH_NSY,mid)

      call sdiputil_solve (obj%sdiputil,ntr,hdmove,trmove,xmid,ymid,hdo,tro)

      if (ntr == FATAL_ERROR) then
           call pc_error ('SDIP: FATAL ERROR IN SDIPUTIL')
           call sdip_wrapup (obj)
      end if
      return
      end subroutine sdip


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine sdip_wrapup (obj)
      implicit none
      type(sdip_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call pc_print        (' ')
      call pc_print        ('SDIP: WRAPUP')
      call mgather_delete  (obj%mgather)
      call sdiputil_wrapup (obj%sdiputil)
      call pc_print        ('SDIP: FINISHED')
      call pc_print        (' ')
      return
      end subroutine sdip_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

!</execute_only>

      end module sdip_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

