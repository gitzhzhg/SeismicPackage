!<CPS_v1 type="PROCESS"/>
!!------------------------------- reg.f90 ------------------------------------!!
!!------------------------------- reg.f90 ------------------------------------!!
!!------------------------------- reg.f90 ------------------------------------!!

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
! Name       : REG           (REGularize trace data)
! Category   : miscellaneous
! Written    : 2000-07-11   by: Jon Gaston 
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : REG performs various regularizing operations on input gathers.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! 
! REG performs various regularizing operations on individual input trace 
! gathers.  REG operates only on one gather at a time. 
! 
!
! Binning Options
!
! Traces within a gather are binned according to binning parameters based on 
! HDR_BIN.  Traces that fall outside the bin array are deleted.  The following
! options relate to binning.
!
!       VARY_BINS (Y/N)   Whether to define the first bin center value by the 
!                         first trace in the gather.
!
!       RESTRICT = COMP   Composite traces that occupy a single bin.
!       RESTRICT = FIRST  Delete all but the first trace to occupy a bin.
!       RESTRICT = LAST   Delete all but the last trace to occupy a bin.
!       RESTRICT = NONE   Do not restrict number of traces that occupy a bin.
!
!       FILL = YES   Insert a dead trace in any unoccupied bin.
!       FILL = NO    Do not insert dead traces in unoccupied bins.
!       FILL = PART  Insert a dead trace in any non-trailing unoccupied bin.
!
! The order of traces in output gathers starts with the first bin and ends
! with the last bin.
!
!
! PNMO Option
!
! If RESTRICT = COMP and OPT_PNMO = YES and HDR_BIN = 6, PNMO is performed on
! all the traces in the bin to adjust their offset to the bin center value.
! After PNMO is performed, the traces are composited.  This option should be
! faster than applying NMO forward followed by NMO reverse later in the process
! sequence.
!
! PNMO is performed by an internal call to the PNMO option in NMO.
!
!
! Composited Trace Headers
!
! If RESTRICT = COMP, the output trace header values are simply averages of the
! corresponding input header values (except for HDR_SEQUENCE, HDR_CURRENT_GROUP,
! HDR_CURRENT_CHANNEL, HDR_FOLD, and HDR_LAV, which are handled differently,
! and HDR_OFFSET, which is set to the bin center offset if PNMO = YES).
! If dead traces are inserted by REG, the nearest non-dead output trace
! on either side of the inserted dead trace is used for interpolation.
!
!
! Filled-in Trace Headers
!
! If FILL = YES then REG inserts a dead trace in all empty bins.  Header words
! 1, 2, 3, 4, 25, 64 and HDR_BIN are set in the usual manner for these dead 
! traces.  For all other header words for these dead traces, REG scans the live
! traces in the gather and determines which header words, if any, are constant.
! If a header word value is constant for all the live traces in the gather, 
! then REG sets that header word in the filled-in dead traces to the constant
! value.
!
! If FILL = PART then REG performs identically to FILL = YES but with the
! following exception: REG will not insert a dead trace in any bin with a
! bin number larger than the largest input bin number in the gather.  In
! other words, the largest output bin number will not exceed the largest
! input bin number, but small and intermediate empty bins will be filled
! in with dead traces.  (The largest output bin number will however always
! be at least 1.)
!
! Flagword
!
! If HDR_FLAG > 0, then the RESTRICT = COMP option will composite only those 
! traces with a flag in HDR_FLAG set by the SELECT process.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Input traces must be gathered.
!
! If you want to use the PNMO option, HDR_BIN must be set to 6.
!
!
! WARNING
!
! Using the PNMO option causes the source and receiver surveyed coordinate
! values to be inconsistent with the HDR_OFFSET values (the coordinates are
! averaged, whereas the offset is based on the bin center).  Certain subsequent
! processes, e.g. DMO and Kirchoff migration, could be adversely affected.
!
!
! Specifying Binning Parameters
!
! Because REG always outputs trace gathers starting with the first bin and 
! ending with the last bin, specifying bin parameters is important as this 
! example shows.
!
!     Let input gathers be CMPs with offset from 1000 ft to 10000 ft 
!     incrementing by 100 ft.
!
!     Setting bin parameters HDR_BIN = 6, BIN_INIT = 1000, BIN_INC = 100,
!     BIN_LAST = 10000 and BIN_TOT = 91 causes output gathers to be sorted 
!     in INCREASING order of offset.
!
!     Setting bin parameters HDR_BIN = 6, BIN_INIT = 10000, BIN_INC = -100,
!     BIN_LAST = 1000 and BIN_TOT = 91 causes output gathers to be sorted 
!     in DECREASING order of offset.
!
! Sort order of the input gathers does not affect the sort order of output 
! gathers.
!
! A fatal error will during execution if FILL = NO and a gather is encountered
! having no traces that satisfy the binning parameters.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a multiple-trace process.
!
! This process requires traces to be input in gathers.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process may alter input traces.
!
! This process outputs traces in gathers, possibly with more or fewer traces
! than the input gather.  Traces in output gathers are ordered starting with 
! the first bin and ending with the last bin.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED          
!
! 
! Name       Description                           Action taken
! ----       -----------                           ------------
! nwih       Number of words in header             Used, not changed
! numtr      Maximum number of traces per gather   Used, not changed
! gathered   Whether input traces are gathered     Used, not changed
! 
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
! 1       Sequential Trace Count     Renumbered.
! 2       Top mute                   Possibly set
! 3       Gather number              Possibly renumbered
! 4       Number within a gather     Possibly renumbered
! 5       Fold                       Possibly accumulated
! 32      Scratch 32                 Set if RESTRICT = COMP
! 25      LAV                        Recalculated if RESTRICT = COMP
!         HDR_FLAG                   Flagword
!         HDR_BIN                    Used to define binning
! 64      Bottom mute                Possibly set
!
! Output header word values, for composited traces, are the average of the 
! corresponding header word values of the traces composited together, except 
! that header word 5 is the sum of the header word 5 values of the traces 
! composited together and header word 1 is reset to the correct sequential 
! trace number.  Header word 25 is recalculated.
!
! If FILL = YES then REG inserts a dead trace in all empty bins.  Header words
! 1, 2, 3, 4, 25, 64 and HDR_BIN are set in the usual manner for these dead 
! traces.  For all other header words for these dead traces, REG scans the live
! traces in the gather and determines which header words, if any, are constant.
! If a header word value is constant for all the live traces in the gather, 
! then REG sets that header word in the filled-in dead traces to the constant
! value.  
!
! If FILL = PART then REG performs identically to FILL = YES but with the
! following exception: REG will not insert a dead trace in any bin with a
! bin number larger than the largest input bin number in the gather.  In
! other words, the largest output bin number will not exceed the largest
! input bin number, but small and intermediate empty bins will be filled
! in with dead traces.
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author       Description
!     ----        ------       -----------
! 19. 2006-06-20  B. Menger    Removed Unused Variables.
! 18. 2004-08-23  Stoeckley    Add option FILL = PART.
! 17. 2003-10-13  Stoeckley    Fix incorrect averaging of trace headers when
!                               dead traces (or traces with zero fold) are
!                               composited.
! 16. 2002-09-09  Stoeckley    Change to use the MTH module for binning.
! 15. 2002-04-17 Burch/Goodger Remove FILL_XY option.
! 14. 2002-03-22  Selzler      Added logic to copy constant header words
!                              into dead traces inserted into output.
! 13. 2002-01-16  Selzler      Added FILL_XY option and associated support.
!                              Changed binning logic, hopefully to be simpler.
!                              Eliminated memory overrun and seg fault.
!                              Created mute header logic.
! 12. 2001-10-18  SMCook       Changed some documentation and logic relating to
!                               COMP and PNMO situations.  If PNMO = YES,
!                               partial moveout is now applied to all traces in
!                               the bin.  The previous scheme (and presumably
!                               old CPS) skipped PNMO for bins having only one
!                               trace, occasionally yielding irregular offsets.
! 11. 2001-10-08  SMCook       Fixed bug report #533 -- erroneous offset values
!                               when compositing traces and FILL = YES.
! 10. 2001-06-04  SMCook       Option to sort by HDR_SORT was completely
!                               removed, and documentation changed accordingly.
!                              Replaced internal bin_init/inc/last/tot
!                               calculations with call to pattern primitive.
!                              Changed final HDR_SEQUENCE numbering logic --
!                               appeared to be making the assumption that ntr
!                               was invariant.  Replaced with counter.
!  9. 2001-05-14  SMCook       Output now has the same sort direction relative
!                               to the HDR_BIN word, if used.
!  8. 2001-01-15  SMCook       HDR_BIN is given a value if FILL = YES.
!  7. 2000-12-08  Stoeckley    Change wrapup flag.
!  6. 2000-09-27  O'Brien      Report NSTORE to parameter cache
!                              Fix allocation bug for hd_sort
!  5. 2000-09-08  O'Brien      Changed initialization of RESTRICT to 'NONE'
!  4. 2000-09-07  O'Brien      Fixed NMO implementation to use only one
!                                nmo structure.
!                              Changed test for NMO application to follow
!                                documentation.
!                              Embellished GUI Sensitivity settings
!                              Implemented traps
!                              Allow sorting without binning
!                              Rearranged all the loops for better clarity
!                              Fixed the FILL option
!  3. 2000-07-18  O'Brien      Fixed setup for sorting
!                              Force HDR_SORT to be positive after setup
!                              Added check before allocating obj%nmo_ptr
!                              Added control variable TWOSETS
!  2. 2000-07-11  Jon Gaston   Initial Version. 
!  1. 1999-11-17  C I Burch    Original design.
!
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
!                      SPECIAL COMPILING REQUIREMENTS             
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS            
!
! This process uses a single set of trace and header arrays.
! 
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more input traces.
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
!                    ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
! Sort option in original version has been removed.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS REG Process/NC=80>
!
!   REG performs various regularizing operations on input gathers.
!
!
! HDR_FLAG=`IIIIII
!
!
! HDR_BIN= `IIIIII
! BIN_INIT=`FFFFFFFFFFF  BIN_INC=`FFFFFFFFFFF
! BIN_LAST=`FFFFFFFFFFF  BIN_TOT=`IIIIIIII
!
!
! VARY_BINS=`CC     RESTRICT=`CCCC      FILL=`CCCC
!
!
! OPT_PNMO=`CC
! PATHNAME_VEL=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!<PARMS PATHNAME_VEL[/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!<HelpSection>
!-------------------------------------------------------------------------------
!
!<Help KEYWORD="HDR_BIN">
!<Tip> Header word designating bins. </Tip>
! Default = 6
! Allowed = 1 - NWIH
! Traces falling outside the array of bins will be deleted.
!</Help>
!
!<Help KEYWORD="BIN_INIT">
!<Tip> Value of HDR_BIN for center of the first bin. </Tip>
! Default = 1.0
! Allowed = real
! This value is superceded if VARY_BINS = YES.
!</Help>
!
!<Help KEYWORD="BIN_INC">
!<Tip> Increment of HDR_BIN value between bins. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="BIN_LAST">
!<Tip> Value of HDR_BIN for center of last bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="BIN_TOT">
!<Tip> Total number of bins. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="VARY_BINS">
!<Tip> Define the first bin center by the first trace in the gather? </Tip>
! Default = NO
! Allowed = YES/NO
! If VARY_BINS = YES, then set the value of the first bin center to the value of
! HDR_BIN of the first trace in the gather.
!</Help>
!
!<Help KEYWORD="RESTRICT">
!<Tip> Method to use in regularizing number of traces in a bin. </Tip>
! Default = NONE
! Allowed = COMP   (Composite traces that occupy a single bin.)
! Allowed = FIRST  (Delete all but the first trace to occupy a bin.)
! Allowed = LAST   (Delete all but the last trace to occupy a bin.)
! Allowed = NONE   (Do not restrict number of traces that occupy a bin.)
!</Help>
!
!<Help KEYWORD="OPT_PNMO">
!<Tip> Whether to use PNMO prior to compositing traces within a bin. </Tip>
! Default = NO
! Allowed = YES/NO
! If RESTRICT = COMP and OPT_PNMO = YES and HDR_BIN = 6, then if more than one 
! trace occupies a bin, PNMO is performed on all the traces in the bin to 
! adjust their offset to bin center value.  After PNMO is performed, the traces
! are composited.  This option should be faster than applying NMO forward 
! followed by NMO reverse later in the process sequence.  This option does NOT
! adjust the source or receiver surveyed coordinate values.
!</Help>
!
!<Help KEYWORD="PATHNAME_VEL">
!<Tip> Pathname for the velocity file to use in the PNMO option. </Tip>
! Default = NONE
! Allowed = char
! Active only if OPT_PNMO = YES and HDR_BIN = 6.
!</Help>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces for the COMP option. </Tip>
! Default = 0 
! Allowed = 0 - NWIH
! If HDR_FLAG = 0, then the COMP option may operate on all traces.  Otherwise, 
! the COMP option can composite only traces with a flag set in header word 
! HDR_FLAG.  
!</Help>
!
!<Help KEYWORD="FILL">
!<Tip> Whether to insert a dead trace in each empty bin. </Tip>
! Default = NO
! Allowed = YES/NO/PART
!
! If FILL = YES:  Insert a dead trace in any bin not occupied by at least
!                 one input trace.
!
! If FILL = NO:   Do not insert a dead trace in any bin not occupied by any
!                 input traces.
!
! If FILL = PART: Insert a dead trace in any bin not occupied by at least
!                 one input trace, but do NOT insert a dead trace in any bin
!                 with a bin number larger than the largest input bin number.
!                 In other words, the largest output bin number will not
!                 exceed the largest input bin number, but small and
!                 intermediate empty bins will be filled in with dead traces.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module reg_module
      use pc_module
      use named_constants_module
      use mutehw_module
      use pathcheck_module
      use nmo_module
      use lav_module
      use sizeof_module
      use pattern_module
      use mth_module

      implicit none
      private

      public :: reg_create
      public :: reg_initialize
      public :: reg_update
      public :: reg_delete
      public :: reg            ! main execution (trace processing) routine.
      public :: reg_wrapup

      character(len=100),public,save :: reg_ident = &
       '$Id: reg.f90,v 1.19 2006/06/20 13:12:04 Menger prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type, public :: reg_struct              

        private 

        integer                    :: hdr_bin          ! process parameter
        real                       :: bin_init         ! process parameter
        real                       :: bin_inc          ! process parameter
        real                       :: bin_last         ! process parameter
        integer                    :: bin_tot          ! process parameter
        logical                    :: vary_bins        ! process parameter
        character(len=5)           :: restrict         ! process parameter
        logical                    :: opt_pnmo         ! process parameter
        character(len=FILENAME_LENGTH) :: pathname_vel ! process parameter
        integer                    :: hdr_flag         ! process parameter
        character(len=8)           :: fill             ! process parameter
        logical                    :: fill_xy          ! process parameter

        integer                    :: nwih             ! global parameter
        integer                    :: ndpt             ! global parameter
        integer                    :: numtr_in         ! global parameter
        integer                    :: numtr            ! global parameter
        logical                    :: gathered         ! global parameter
        
        logical                    :: skip_wrapup      ! dependent parameter
        integer                    :: ngathers         ! for final renumbering
        integer                    :: ntraces          ! for final renumbering
        integer,pointer            :: bin_indx(:)      ! Bin index for each
                                                       ! input trace.
                                                       ! Zero if ignored.
        double precision,pointer   :: bin_fold(:)      ! Bin fold for each bin.
                                                       ! Zero if bin is empty.
        integer,pointer            :: bin_pop(:)       ! Bin population.
                                                       ! Zero if bin is empty.
        integer,pointer            :: bin_out(:)       ! First output trace
                                                       ! index for each bin.
                                                       ! Zero iff bin has no
                                                       ! associated output
                                                       ! (possible if FILL=NO).
                                                       ! If RESTRICT = NONE
                                                       ! then the increment
                                                       ! between bin_out values
                                                       ! equals bin_pop (min
                                                       ! of 1 if FILL = YES).
        integer,pointer            :: bin_flag(:)      ! Bin processing flag.
                                                       ! Usage depends on value
                                                       ! of RESTRICT.
        logical                    :: comp_fold        ! True iff trace fold
                                                       ! should be honored
                                                       ! when RESTRICT=COMP.
        integer                    :: hdr_ref          ! Reference headers
                                                       ! for hdr_const test.
        logical,pointer            :: hdr_const(:)     ! True iff trace header
                                                       ! is constant for each
                                                       ! contributing to output.

        type(nmo_struct), pointer  :: nmo              ! dependent parameter

      end type reg_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(reg_struct),pointer,save :: object      ! needed for traps.
      character(len=5),save  :: restrict_options(4)
      integer                :: restrict_nopt=4

      data restrict_options/'NONE', 'COMP', 'FIRST', 'LAST'/

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine reg_create (obj)
      implicit none
      type(reg_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify(obj%bin_indx)
      nullify(obj%bin_fold)
      nullify(obj%bin_pop)
      nullify(obj%bin_out)
      nullify(obj%bin_flag)
      nullify(obj%hdr_const)

      nullify(obj%nmo)

      call reg_initialize (obj)

      return
      end subroutine reg_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine reg_delete (obj)
      implicit none
      type(reg_struct),pointer :: obj       ! arguments



      call reg_wrapup (obj)

! deallocate all pointers      
      if(associated(obj%bin_indx))     deallocate(obj%bin_indx)   
      if(associated(obj%bin_fold))     deallocate(obj%bin_fold)   
      if(associated(obj%bin_pop))      deallocate(obj%bin_pop)   
      if(associated(obj%bin_out))      deallocate(obj%bin_out)   
      if(associated(obj%bin_flag))     deallocate(obj%bin_flag)   
      if(associated(obj%hdr_const))    deallocate(obj%hdr_const)   

! delete nmo structure if present
      if (associated(obj%nmo)) call nmo_delete(obj%nmo)

      deallocate(obj)
      return
      end subroutine reg_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine reg_initialize (obj)
      implicit none
      type(reg_struct),intent(inout) :: obj       ! arguments

  
! Initialize user parameters
      obj%hdr_bin        = HDR_OFFSET
      obj%bin_init       = 1.0
      obj%bin_inc        = 1.0
      obj%bin_last       = 1.0
      obj%bin_tot        = 1
      obj%vary_bins      = .false.
      obj%restrict       = 'NONE'
      obj%opt_pnmo       = .false.
      obj%pathname_vel   = PATHCHECK_EMPTY
      obj%hdr_flag       = 0 
      obj%fill           = 'NO'
      obj%fill_xy        = .false.

! Initialize dependent params
      obj%ntraces        = 0
      obj%ngathers       = 0
      obj%hdr_ref        = 0

      ! The original code implicitly assumed FALSE.
      ! No one seems to need the functionality associated with TRUE
      ! therefore KISS (Keep It Simple Simon) the GUI off.
      obj%comp_fold      = .false.

! Initialize globals
      obj%numtr_in = 0
      obj%numtr    = 0
      obj%nwih     = 0
      obj%ndpt     = 0
      obj%gathered = .false.

      call reg_update (obj)
      return
      end subroutine reg_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine reg_update (obj)
      implicit none
      type(reg_struct),intent(inout),target :: obj             ! arguments

      integer              :: update_state      , ier 

      integer  :: SIZEOF_INT
      integer  :: SIZEOF_REAL
      integer  :: nstore
!----------------------------------------------------

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.
      update_state = pc_get_update_state()

      SIZEOF_INT    = sizeof(1)
      SIZEOF_REAL   = sizeof(1.0)


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

! Retrieve and test globals

      call pc_get_global ('GATHERED', obj%gathered)
      call pc_get_global ('NWIH'    , obj%nwih    )
      call pc_get_global ('NDPT'    , obj%ndpt    )
      call pc_get_global ('NUMTR'   , obj%numtr_in)

      if(.not.obj%gathered) then
        call pc_error('REG: This process must be preceded by a gather.')
      endif

      if(obj%nwih <= 0) then
        call pc_error('REG: NWIH has not been set')
      endif

      if(obj%ndpt <= 0) then
        call pc_error('REG: NDPT has not been set')
      endif

! Retrieve user params

      call pc_get('HDR_FLAG'    , obj%hdr_flag     , reg_trap)
      call pc_get('HDR_BIN'     , obj%hdr_bin      , reg_trap)

      call pc_get('BIN_INIT'    , obj%bin_init )   ! pattern primitive checks
      call pc_get('BIN_INC'     , obj%bin_inc  )
      call pc_get('BIN_LAST'    , obj%bin_last )
      call pc_get('BIN_TOT'     , obj%bin_tot  )

      call pc_get('VARY_BINS'   , obj%vary_bins    , reg_trap)
      call pc_get('RESTRICT'    , obj%restrict     , reg_trap)
      call pc_get('FILL'        , obj%fill         , reg_trap)
!!      call pc_get('FILL_XY'     , obj%fill_xy)

      call pc_get('OPT_PNMO'    , obj%opt_pnmo     , reg_trap)
      call pc_get('PATHNAME_VEL', obj%pathname_vel)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

! SMCook incorporated pattern primitive

      ier = pattern_stop2('REG:', .true.,                            &
          obj%bin_init, obj%bin_inc, obj%bin_last, obj%bin_tot,      &
          'BIN_INIT', 'BIN_INC', 'BIN_LAST', 'BIN_TOT',              &
          pc_verify_scalar('bin_init'), pc_verify_scalar('bin_inc'), &
          pc_verify_scalar('bin_last'), pc_verify_scalar('bin_tot'))


! Setup nmo structure if all conditions are favorable

      if( obj%hdr_bin ==HDR_OFFSET .and. &
          obj%restrict=='COMP'     .and. &
          obj%opt_pnmo                  ) then

        if ( update_state==PC_GUI ) then
          call pathcheck ('PATHNAME_VEL', obj%pathname_vel, '.vel', &
                           required=.false.)
        else
          call pathcheck ('PATHNAME_VEL', obj%pathname_vel, '.vel', &
                           required=.true.)
        endif

        call pc_clear
        call pc_put_process('opt_nmo', 'PARTIAL')
        call pc_put_process('pathname',obj%pathname_vel)
        if (associated(obj%nmo)) then
           call nmo_update (obj%nmo)
        else
           call nmo_create (obj%nmo)
        endif
        call pc_restore

      endif

      if(obj%fill /= 'YES' .and. obj%fill /= 'PART') obj%fill = 'NO'


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field('RESTRICT', restrict_options, restrict_nopt)
      call pc_put_options_field('FILL', (/'YES  ', 'NO   ', 'PART '/))

      call pc_put('HDR_FLAG'     , obj%hdr_flag    )
      call pc_put('HDR_BIN'      , obj%hdr_bin     )

      call pc_put('BIN_INIT'     , obj%bin_init    )
      call pc_put('BIN_INC'      , obj%bin_inc     )
      call pc_put('BIN_LAST'     , obj%bin_last    )
      call pc_put('BIN_TOT'      , obj%bin_tot     )

      call pc_put('VARY_BINS'    , obj%vary_bins   )
      call pc_put('RESTRICT'     , obj%restrict    )
      call pc_put('FILL'         , obj%fill        )
!!      call pc_put('FILL_XY'      , obj%fill_xy     )

      call pc_put('OPT_PNMO'     , obj%opt_pnmo    )
      call pc_put('PATHNAME_VEL' , obj%pathname_vel)


! Adjust control parameters

      nstore = 0
      nstore = nstore + obj%numtr_in*SIZEOF_INT
      nstore = nstore + obj%bin_tot*SIZEOF_INT
      nstore = nstore + obj%numtr*SIZEOF_INT
      nstore = nstore/SIZEOF_REAL

      call pc_put_control ('NSTORE',nstore)
      call pc_put_control ('TWOSETS','YES')


! Under some conditions, NUMTR needs to be adjusted too

      if ( obj%restrict == 'NONE' ) then
        if ( obj%fill /= 'NO' ) then
          obj%numtr = obj%numtr_in + obj%bin_tot - 1
        else
          obj%numtr = obj%numtr_in
        endif
      else
        obj%numtr = obj%bin_tot
      endif

      call pc_put_global('NUMTR', obj%numtr)


!!--------------------- set GUI sensitivities ------------------------------!!
!!--------------------- set GUI sensitivities ------------------------------!!
!!--------------------- set GUI sensitivities ------------------------------!!


      if ( update_state == PC_GUI ) then
        call reg_set_sensitivities (obj)
      endif


!!--------------------- prepare for execution ------------------------------!!
!!--------------------- prepare for execution ------------------------------!!
!!--------------------- prepare for execution ------------------------------!!

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

! Allocate space used to monitor binning
      allocate(obj%bin_indx(obj%numtr_in))
      allocate(obj%bin_fold(obj%bin_tot))
      allocate(obj%bin_pop(obj%bin_tot))
      allocate(obj%bin_out(obj%bin_tot))
      allocate(obj%bin_flag(obj%bin_tot))
      if(obj%fill /= 'NO') then
        allocate(obj%hdr_const(obj%nwih))
      end if


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine reg_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

      subroutine reg_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword           ! Arguments
!-------------------------------------------------

      select case (keyword)

        case('HDR_FLAG')
          if (object%hdr_flag<0 .or. object%hdr_flag>object%nwih)  then
            call pc_error ('REG: HDR_FLAG is out of range 0 to ', &
                           object%nwih,' Resetting to default.')
            object%hdr_flag = 0
          endif

        case('HDR_BIN')
          if (object%hdr_bin<1 .or. object%hdr_bin>object%nwih)  then
            call pc_error ('REG: HDR_BIN is out of range 1 to ', &
                           object%nwih,' Resetting to default.')
            object%hdr_bin = HDR_OFFSET
          endif

        case('RESTRICT')
          if (all(object%restrict /= restrict_options)) then
            call pc_info ("REG: Resetting RESTRICT to default 'NONE'.")
            object%restrict = 'NONE'
          endif

      end select

      return
      end subroutine reg_trap

!!----------------------- set sensitivities for GUI ------------------------!!
!!----------------------- set sensitivities for GUI ------------------------!!
!!----------------------- set sensitivities for GUI ------------------------!!

      subroutine reg_set_sensitivities (obj)
      implicit none
      type(reg_struct) :: obj
!---------------------------------------------------------

      call pc_put_sensitive_field_flag ('BIN_INIT',     .true. )
      call pc_put_sensitive_field_flag ('BIN_LAST',     .true. )

      call pc_put_sensitive_field_flag ('OPT_PNMO',     .false.)
      call pc_put_sensitive_field_flag ('PATHNAME_VEL', .false.)

! Now get exceptions based on parameter settings

      if ( obj%hdr_bin  == HDR_OFFSET .and. obj%restrict == 'COMP' ) then
        call pc_put_sensitive_field_flag ('OPT_PNMO',     .true. )
        if(obj%opt_pnmo) then
          call pc_put_sensitive_field_flag ('PATHNAME_VEL', .true. )
        endif
      endif

      if(obj%vary_bins) then
        call pc_put_sensitive_field_flag ('BIN_INIT',     .false.)
        call pc_put_sensitive_field_flag ('BIN_LAST',     .false.)
      endif

!!      if(obj%fill ) then
!!        call pc_put_sensitive_field_flag ('FILL_XY',     .true.)
!!      else
!!        call pc_put_sensitive_field_flag ('FILL_XY',     .false.)
!!      endif


      return
      end subroutine reg_set_sensitivities


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine reg (obj,ntr,hd,tr,hdo,tro)
      implicit none
      type(reg_struct) ,intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments
      double precision ,intent(inout) :: hdo(:,:)               ! arguments
      real             ,intent(inout) :: tro(:,:)               ! arguments

      integer :: nmo_ntr
      integer :: bin_here, bin_max
      integer :: i, itr, ibin
      integer :: ntr_in, idx_out, next_idx

      integer :: mute_top, mute_bottom
      real    :: hdr_factor, trc_factor

!-------------------------------------------------

! do check for end of processing traces

      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
        call reg_wrapup (obj)
        return
      endif

! if VARY_BINS is yes then we need to define first bin center
! as the first trace in the gather

      if(obj%vary_bins) then
        obj%bin_init = hd(obj%hdr_bin,1)
      endif

      ntr_in = ntr

      ! all bins are initially empty
      obj%bin_fold = 0.0
      obj%bin_pop = 0

      if(obj%fill /= 'NO') then
        ! assume all headers are constant, until proven otherwise
        obj%hdr_ref = 0
        obj%hdr_const = .true.
      end if

      ! scan input traces and initialize bin_indx, bin_fold and bin_pop.
      bin_max = 0
      do itr = 1,ntr_in
        ! find the bin for this trace
        bin_here = mth_bin_number  &
                        (obj%bin_init, obj%bin_inc, real(hd(obj%hdr_bin,itr)))

        if(obj%hdr_flag/=0 .and. hd(max(1,obj%hdr_flag),itr)==0) then
          ! Ignore unflagged input trace.
          obj%bin_indx(itr) = 0
        else if(bin_here<1 .or. bin_here>obj%bin_tot) then
          ! Ignore input trace outside of requested bins.
          obj%bin_indx(itr) = 0
        else if(obj%comp_fold .and. hd(HDR_FOLD,itr) <= 0.0) then
          ! Ignore input trace with zero fold.
          obj%bin_indx(itr) = 0    ! added 2003-06-03
        else if(hd(HDR_LAV,itr) <= 0.0) then
          ! Ignore dead input trace.
          obj%bin_indx(itr) = 0    ! added 2003-06-03
        else
          ! Accept this input trace.
          obj%bin_indx(itr) = bin_here
          obj%bin_fold(bin_here) = obj%bin_fold(bin_here) + hd(HDR_FOLD,itr)
          obj%bin_pop(bin_here) = obj%bin_pop(bin_here) + 1
          bin_max = max(bin_max,bin_here)
        end if
      end do

      ! do regularization on bins for selected option 
      restrict_bin: &
      select case(obj%restrict)

        case ('COMP') 

          ! Compute bin_out starting index of output trace for each bin.
          if(obj%fill /= 'NO') then
            ! If a bin is empty, dead traces ARE filled in.

            do ibin = 1,obj%bin_tot
              hdo(:obj%nwih,ibin) = 0.0
              hdo(HDR_TOP_MUTE,ibin) = 1.0
              hdo(HDR_BOTTOM_MUTE,ibin) = obj%ndpt
              tro(:obj%ndpt,ibin) = 0.0

              obj%bin_out(ibin) = ibin

              if(obj%bin_pop(ibin) == 0) then
                ! Initialize dead output traces to zero.
                hdo(obj%hdr_bin,ibin) = mth_bin_center  &
                                           (obj%bin_init,obj%bin_inc,ibin)
              end if
            end do

            ntr = obj%bin_tot
          else
            ! Do NOT create a dead trace for empty bins.
            next_idx = 1

            do ibin = 1,obj%bin_tot

              hdo(:obj%nwih,ibin) = 0.0
              tro(:obj%ndpt,ibin) = 0.0

              if(obj%bin_pop(ibin) == 0) then
                ! bin is empty and dead traces are not created.
                obj%bin_out(ibin) = 0
              else
                ! Bin is populated, another output trace is needed.
                obj%bin_out(ibin) = next_idx
                next_idx = next_idx + 1
              end if
            end do

            ntr = next_idx - 1
          end if

          ! bin_flag is zero, until the first trace is accumulated
          obj%bin_flag = 0

          ! now process the traces
          do itr = 1,ntr_in
            bin_here = obj%bin_indx(itr)

            if(bin_here == 0) cycle  ! ignore this trace

 !!         if(obj%comp_fold .and. hd(HDR_FOLD,itr) <= 0.0) then
 !!           ! Honor the input trace fold, which happens to be zero.
 !!           cycle
 !!         end if
 !!
 !!         if(hd(HDR_LAV,itr) <= 0.0) cycle ! Honor dead trace flags

 !!  The commented lines above were removed 2003-06-03 because
 !!  the code has been moved above to the location where obj%bin_indx(:)
 !!  is calculated.  This fixes an inconsistency between the trace counts
 !!  and the actual traces composited.  Previously, dead traces and traces
 !!  with 0 fold were counted, but not added together.  This caused trace
 !!  headers to be improperly averaged (i.e. trc_factor and hdr_factor
 !!  were wrong).

            ! perform partial NMO if warranted
            if ( obj%hdr_bin==HDR_OFFSET .and. obj%opt_pnmo ) then
              hd(HDR_SCRATCH_32,itr) = mth_bin_center  &
                                           (obj%bin_init,obj%bin_inc,bin_here)
              nmo_ntr = 1
              call nmo(obj%nmo, nmo_ntr, &
                       hd(:,itr:itr),tr(:,itr:itr))
              if ( nmo_ntr == FATAL_ERROR ) then
                call reg_wrapup (obj)
                ntr = FATAL_ERROR
                return
              endif
            endif

            ! accumulate the trace to this bin

            ! make sure mute has been applied to input trace samples.
            call mutehw(hd, tr, obj%ndpt, 0.0, MUTEHW_BOTH)

            if(obj%bin_flag(bin_here) == 0) then
              ! initialize the top and bottom mute, once only
              obj%bin_flag(bin_here) = 1

              mute_top    = hd(HDR_TOP_MUTE,itr)
              mute_bottom = hd(HDR_BOTTOM_MUTE,itr)
            else
              ! extend top and bottom mute as needed
              mute_top = max(dble(1.0), &
                min(hdo(HDR_TOP_MUTE,idx_out),hd(HDR_TOP_MUTE,itr)))

              mute_bottom = min(dble(obj%ndpt), &
                max(hdo(HDR_BOTTOM_MUTE,idx_out),hd(HDR_BOTTOM_MUTE,itr)))
            endif

            if(obj%comp_fold) then
              ! This scheme should honor the input trace HDR_FOLD.
              ! It assumes the input HDR_FOLD is correct.
              trc_factor = hd(HDR_FOLD,itr) / obj%bin_fold(bin_here)
              hdr_factor = trc_factor
            else
              ! This scheme assumes each input trace has the same fold
              ! and works even if HDR_FOLD is zero.
              ! It corresponds to the original compositing algorithm.
              hdr_factor = 1.0 / obj%bin_pop(bin_here)
              trc_factor = hdr_factor
            end if

            idx_out = obj%bin_out(bin_here)

            if(obj%fill /= 'NO' .and. hd(HDR_LAV,itr) > 0.0) then
              ! input trace is not dead and missing output traces are filled
              if(obj%hdr_ref == 0) then
                ! save reference trace index for constant header test
                obj%hdr_ref = itr
              else
                ! reset hdr_const for any headers different than reference
                where(hd(:obj%nwih,itr) /= hd(:obj%nwih,obj%hdr_ref))
                  obj%hdr_const = .false.
                end where
              end if
            end if

            hdo(:obj%nwih,idx_out) = &
              hdo(:obj%nwih,idx_out) + hd(:obj%nwih,itr) * hdr_factor

            hdo(HDR_TOP_MUTE,idx_out)    = mute_top
            hdo(HDR_BOTTOM_MUTE,idx_out) = mute_bottom

            tro(:obj%ndpt,idx_out) = &
              tro(:obj%ndpt,idx_out) + tr(:obj%ndpt,itr) * trc_factor

            hdo(HDR_FOLD,idx_out) = obj%bin_fold(bin_here)

          enddo

        case ('FIRST')

          call reg_first_last(obj, ntr, hd, tr, hdo, tro)

        case ('LAST')

          call reg_first_last(obj, ntr, hd, tr, hdo, tro)

        case ('NONE')

          ! Compute bin_out starting index of output trace for each bin.
          if(obj%fill /= 'NO') then
            ! If a bin is empty, dead traces ARE filled in.
            next_idx = 1

            ! Determine starting index of output trace for each bin.
            do ibin = 1,obj%bin_tot
              if(obj%bin_pop(ibin) == 0) then
                ! Initialize dead output traces to zero.
                hdo(:obj%nwih,next_idx) = 0.0
                hdo(obj%hdr_bin,next_idx) = mth_bin_center  &
                                              (obj%bin_init, obj%bin_inc, ibin)
                hdo(HDR_TOP_MUTE,next_idx) = 1.0
                hdo(HDR_BOTTOM_MUTE,next_idx) = obj%ndpt
                tro(:obj%ndpt,next_idx) = 0.0

                obj%bin_out(ibin) = next_idx
                next_idx = next_idx + 1
              else
                ! Bin is populated, another output trace is live.
                obj%bin_out(ibin) = next_idx
                next_idx = next_idx + obj%bin_pop(ibin)
              end if
            end do

            ntr = next_idx - 1
          else
            ! If a bin is empty, dead traces are NOT output for it.
            next_idx = 1

            ! Determine starting index of output trace for each bin.
            do ibin = 1,obj%bin_tot
              if(obj%bin_pop(ibin) == 0) then
                obj%bin_out(ibin) = 0
              else
                ! Bin is populated, another output trace is needed.
                obj%bin_out(ibin) = next_idx
                next_idx = next_idx + obj%bin_pop(ibin)
              end if
            end do

            ntr = next_idx - 1
          end if

          ! bin_flag is index of next output trace for each bin.
          obj%bin_flag = obj%bin_out

          ! now process the traces
          do itr = 1,ntr_in
            bin_here = obj%bin_indx(itr)

            if(bin_here == 0) cycle  ! ignore this trace

            if(obj%bin_flag(bin_here) > 0) then
              idx_out = obj%bin_flag(bin_here)

              hdo(:obj%nwih,idx_out) = hd(:obj%nwih,itr)
              tro(:obj%ndpt,idx_out) = tr(:obj%ndpt,itr)

              obj%bin_flag(bin_here) = obj%bin_flag(bin_here) + 1

              if(obj%fill /= 'NO' .and. hd(HDR_LAV,itr) > 0.0) then
                ! input trace is not dead and missing output traces are filled
                if(obj%hdr_ref == 0) then
                  ! save reference trace index for constant header test
                  obj%hdr_ref = itr
                else
                  ! reset hdr_const for any headers different than reference
                  where(hd(:obj%nwih,itr) /= hd(:obj%nwih,obj%hdr_ref))
                    obj%hdr_const = .false.
                  end where
                end if
              end if
            end if
          end do

      end select restrict_bin


      if(obj%fill /= 'NO') then
        if(obj%fill_xy) then
          call reg_fill_xy (obj, hd, hdo)
        else
          do ibin=1,obj%bin_tot
            if(obj%bin_pop(ibin) == 0) then
              ! copy constant headers into the dead trace being inserted
              where(obj%hdr_const)
                hdo(:,obj%bin_out(ibin)) = hd(:,obj%hdr_ref)
              end where
            end if
          end do
        end if
      end if

      ! check unlikely event that no traces satisfy binning scheme

      if(ntr == 0) then
        call pc_error('REG: ntr = 0 (no traces matched binning scheme)')
        call pc_error('REG: error occurred for gather # ',obj%ngathers + 1)
        ntr = FATAL_ERROR
        return
      end if


      ! keep up with the gathers processed
      ! set header words 1, 3 and 4 for output

      obj%ngathers = obj%ngathers + 1
      do i = 1, ntr
        if (obj%fill == 'PART' .and. i > 1) then
             bin_here = mth_bin_number  &
                        (obj%bin_init, obj%bin_inc, real(hdo(obj%hdr_bin,i)))
             if (bin_here > bin_max) then
                  ntr = i - 1
                  exit
             endif
        endif

        obj%ntraces = obj%ntraces + 1

        hdo(HDR_SEQUENCE,i) = obj%ntraces
        hdo(HDR_CURRENT_GROUP,i) = obj%ngathers
        hdo(HDR_CURRENT_CHANNEL,i) = i
      enddo
      
      if ( obj%restrict=='COMP' ) call lav_set_hdr(hdo, tro, obj%ndpt, ntr)

      return
      end subroutine reg


!!--------------------------- reg_first_last -------------------------------!!
!!--------------------------- reg_first_last -------------------------------!!
!!--------------------------- reg_first_last -------------------------------!!


      ! Restrict input to first or last trace within bin.

      subroutine reg_first_last (obj, ntr, hd, tr, hdo, tro)
      implicit none
      type(reg_struct),intent(inout):: obj          ! arguments
      integer,intent(inout)         :: ntr          ! arguments
      double precision ,intent(in)  :: hd(:,:)      ! arguments
      real             ,intent(in)  :: tr(:,:)      ! arguments
      double precision ,intent(out) :: hdo(:,:)     ! arguments
      real             ,intent(out) :: tro(:,:)     ! arguments

      integer :: ibin      ! bin number
      integer :: next_idx  ! next index for an output trace
      integer :: bin_here  ! bin number for an input  trace
      integer :: itr       ! input trace number
      integer :: idx_out   ! output trace number
      integer :: itr_start ! itr starting index
      integer :: itr_stop  ! itr stopping index
      integer :: itr_inc   ! itr index increment

      if(obj%restrict == 'FIRST') then
        ! walk forward through the list of input traces
        itr_start = 1
        itr_stop  = ntr
        itr_inc   = +1
      else if(obj%restrict == 'LAST') then
        ! walk backward through the list of input traces
        itr_start = ntr
        itr_stop  = 1
        itr_inc   = -1
      end if

      ! Compute bin_out starting index of output trace for each bin.
      if(obj%fill /= 'NO') then
        ! If a bin is empty, dead traces ARE filled in.
        do ibin = 1,obj%bin_tot
          obj%bin_out(ibin) = ibin

          if(obj%bin_pop(ibin) == 0) then
            ! Initialize dead output traces to zero.
            hdo(:obj%nwih,ibin) = 0.0
            hdo(obj%hdr_bin,ibin) = mth_bin_center  &
                                      (obj%bin_init, obj%bin_inc, ibin)
            hdo(HDR_TOP_MUTE,ibin) = 1.0
            hdo(HDR_BOTTOM_MUTE,ibin) = obj%ndpt
            tro(:obj%ndpt,ibin) = 0.0
          end if
        end do

        ntr = obj%bin_tot
      else
        ! Do NOT create a dead trace for empty bins.
        next_idx = 1

        do ibin = 1,obj%bin_tot
          if(obj%bin_pop(ibin) == 0) then
            obj%bin_out(ibin) = 0
          else
            ! Bin is populated, another output trace is needed.
            obj%bin_out(ibin) = next_idx
            next_idx = next_idx + 1
          end if
        end do

        ntr = next_idx - 1
      end if

      ! bin_flag == bin_out until an input trace is copied to output,
      ! then bin_flag is zeroed.
      obj%bin_flag = obj%bin_out

      ! now process the traces
      do itr = itr_start,itr_stop,itr_inc
        bin_here = obj%bin_indx(itr)

        if(bin_here == 0) cycle  ! ignore this trace

        if(obj%bin_flag(bin_here) > 0) then
          idx_out = obj%bin_flag(bin_here)

          ! accept one trace only.
          obj%bin_flag(bin_here) = 0

          hdo(:obj%nwih,idx_out) = hd(:obj%nwih,itr)
          tro(:obj%ndpt,idx_out) = tr(:obj%ndpt,itr)

          if(obj%fill /= 'NO' .and. hd(HDR_LAV,itr) > 0.0) then
            ! input trace is not dead and missing output traces are filled
            if(obj%hdr_ref == 0) then
              ! save reference trace index for constant header test
              obj%hdr_ref = itr
            else
              ! reset hdr_const for any headers different than reference
              where(hd(:obj%nwih,itr) /= hd(:obj%nwih,obj%hdr_ref))
                obj%hdr_const = .false.
              end where
            end if
          end if
        end if
      end do

      return
      end subroutine reg_first_last


!!--------------------------- reg_fill_xy --------------------------------!!
!!--------------------------- reg_fill_xy --------------------------------!!
!!--------------------------- reg_fill_xy --------------------------------!!


      ! Interpolate XY coordinates within dead traces.

      subroutine reg_fill_xy (obj, hd, hdo)
      implicit none
      type(reg_struct),intent(inout) :: obj       ! arguments
      double precision ,intent(inout) :: hd(:,:)  ! arguments
      double precision ,intent(inout) :: hdo(:,:) ! arguments

      integer :: idx_lo, idx_hi, idx_in, idx_out
      integer :: in_lo, in_hi
      double precision :: bin_hi, bin_lo, bin_delta, bin_out
      real :: factor_lo, factor_hi

      ! Find first bin that is occupied
      do idx_lo = 1, obj%bin_tot
        if(obj%bin_pop(idx_lo) > 0) then
          ! idx_lo bin is occupied
          exit
        end if
      end do

      if(idx_lo > obj%bin_tot) then
        ! all bins are vacant, retain zeros in all dead trace headers
        return
      else if(idx_lo > 1) then
        ! Find nearest trace in the first occupied bin
        call reg_near_trc (obj, hdo, idx_lo, -obj%bin_inc, idx_in)

        ! Copy XY coordinates into dead trace headers for vacant bins
        copy_lo: &
        do idx_out = 1, idx_lo - 1
          call reg_linear_xy (hdo, idx_out, &
            idx_in, idx_in, 1.0, 0.0)

          ! copy constant headers into the dead trace being inserted
          where(obj%hdr_const)
            hdo(:,idx_out) = hd(:,obj%hdr_ref)
          end where
        end do copy_lo
      end if

      ! Variable usage:
      ! idx_lo = index of an occupied bin, idx_out nearest lower bound.
      !          Initialized above to the first occupied bin.
      ! idx_hi = index of an occupied bin, idx_out nearest upper bound.
      ! idx_out = index of a vacant bin, where idx_lo < idx_out < idx_hi.
      idx_out = idx_lo + 1

      find_dead: &
      do while(idx_out <= obj%bin_tot)
        if(obj%bin_pop(idx_out) == 0) then
          ! idx_out bin is vacant
          ! Find the nearest trace in bin idx_lo
          call reg_near_trc (obj, hdo, idx_lo, +obj%bin_inc, in_lo)

          find_live: &
          do idx_hi = idx_out + 1, obj%bin_tot
            if(obj%bin_pop(idx_hi) > 0) then
              ! Find the nearest trace in bin idx_hi
              call reg_near_trc (obj, hdo, idx_hi, -obj%bin_inc, in_hi)

              interpolate: &
              do while(idx_out < idx_hi)
                ! Interpolate XY coordinate into dead traces for vacant bin
                bin_lo = hdo(obj%hdr_bin,in_lo)
                bin_hi = hdo(obj%hdr_bin,in_hi)

                bin_delta = bin_hi - bin_lo
                bin_out = mth_bin_center (obj%bin_init, obj%bin_inc, idx_out)

                factor_lo = (bin_hi - bin_out) / bin_delta
                factor_hi = (bin_out - bin_lo) / bin_delta

                call reg_linear_xy (hdo, idx_out, &
                  in_lo, in_hi, factor_lo, factor_hi)

                ! copy constant headers into the dead trace being inserted
                where(obj%hdr_const)
                  hdo(:,idx_out) = hd(:,obj%hdr_ref)
                end where

                idx_out = idx_out + 1
              end do interpolate

              exit find_live

            end if
          end do find_live

          if(idx_hi > obj%bin_tot) then
            ! Copy XY coordinates into dead trace headers for vacant bins
            idx_in = in_lo

            copy_hi: &
            do idx_out = idx_out, obj%bin_tot
              call reg_linear_xy (hdo, idx_out, &
                idx_in, idx_in, 1.0, 0.0)

              ! copy constant headers into the dead trace being inserted
              where(obj%hdr_const)
                hdo(:,idx_out) = hd(:,obj%hdr_ref)
              end where
            end do copy_hi
          end if

          idx_lo  = idx_hi
          idx_out = idx_hi + 1
        else
          idx_lo  = idx_out
          idx_out = idx_out + 1
        end if
      end do find_dead

      return
      end subroutine reg_fill_xy


!!---------------------------- reg_near_trc --------------------------------!!
!!---------------------------- reg_near_trc --------------------------------!!
!!---------------------------- reg_near_trc --------------------------------!!


      ! Find the nearest trace within a bin in a given hi_lo direction

      subroutine reg_near_trc (obj, hdo, idx_bin, hi_lo, idx_near)
      implicit none
      type(reg_struct),intent(inout)  :: obj       ! arguments
      double precision ,intent(inout) :: hdo(:,:)  ! arguments
      integer,intent(in)              :: idx_bin   ! arguments
      real,intent(in)                 :: hi_lo     ! arguments
      integer,intent(inout)           :: idx_near  ! arguments

      ! hi_lo = -BIN_INC: find the trace nearest a lower  bin index.
      ! hi_lo = +BIN_INC: find the trace nearest a higher bin index.

      integer :: idx_test
      double precision :: bin_value, tst_value

      if(hi_lo > 0.0) then
        idx_near = obj%bin_out(idx_bin)
        bin_value = hdo(obj%hdr_bin,idx_near)

        do idx_test = idx_near + 1, idx_near + obj%bin_pop(idx_bin) - 1
          tst_value = hdo(obj%hdr_bin,idx_test)

          if(tst_value > bin_value) then
            idx_near = idx_test
            bin_value = tst_value
          end if
        end do
      else ! assume hi_lo < 0.0
        idx_near = obj%bin_out(idx_bin)
        bin_value = hdo(obj%hdr_bin,idx_near)

        do idx_test = idx_near + 1, idx_near + obj%bin_pop(idx_bin) - 1
          tst_value = hdo(obj%hdr_bin,idx_test)

          if(tst_value < bin_value) then
            idx_near = idx_test
            bin_value = tst_value
          end if
        end do
      end if

      return
      end subroutine reg_near_trc


!!--------------------------- reg_linear_xy -------------------------------!!
!!--------------------------- reg_linear_xy -------------------------------!!
!!--------------------------- reg_linear_xy -------------------------------!!


      ! Linearly interpolate XY coordinates in trace headers

      subroutine reg_linear_xy (hdo, idx_out, &
        idx_in_lo, idx_in_hi, factor_lo, factor_hi)
      implicit none
      double precision ,intent(inout) :: hdo(:,:) ! arguments
      integer,intent(in) :: idx_out               ! arguments
      integer,intent(in) :: idx_in_lo             ! arguments
      integer,intent(in) :: idx_in_hi             ! arguments
      real,intent(in) :: factor_lo                ! arguments
      real,intent(in) :: factor_hi                ! arguments

      hdo(HDR_MIDPOINT_XGRID,idx_out) = &
        factor_lo * hdo(HDR_MIDPOINT_XGRID,idx_in_lo) + &
        factor_hi * hdo(HDR_MIDPOINT_XGRID,idx_in_hi)
      hdo(HDR_MIDPOINT_YGRID,idx_out) = &
        factor_lo * hdo(HDR_MIDPOINT_YGRID,idx_in_lo) + &
        factor_hi * hdo(HDR_MIDPOINT_YGRID,idx_in_hi)

      hdo(HDR_SOURCE_XLOC,idx_out) = &
        factor_lo * hdo(HDR_SOURCE_XLOC,idx_in_lo) + &
        factor_hi * hdo(HDR_SOURCE_XLOC,idx_in_hi)
      hdo(HDR_SOURCE_YLOC,idx_out) = &
        factor_lo * hdo(HDR_SOURCE_YLOC,idx_in_lo) + &
        factor_hi * hdo(HDR_SOURCE_YLOC,idx_in_hi)

      hdo(HDR_RECEIVER_XLOC,idx_out) = &
        factor_lo * hdo(HDR_SOURCE_XLOC,idx_in_lo) + &
        factor_hi * hdo(HDR_SOURCE_XLOC,idx_in_hi)
      hdo(HDR_RECEIVER_YLOC,idx_out) = &
        factor_lo * hdo(HDR_SOURCE_YLOC,idx_in_lo) + &
        factor_hi * hdo(HDR_SOURCE_YLOC,idx_in_hi)

      hdo(HDR_MIDPOINT_XLOC,idx_out) = &
        factor_lo * hdo(HDR_MIDPOINT_XLOC,idx_in_lo) + &
        factor_hi * hdo(HDR_MIDPOINT_XLOC,idx_in_hi)
      hdo(HDR_MIDPOINT_YLOC,idx_out) = &
        factor_lo * hdo(HDR_MIDPOINT_YLOC,idx_in_lo) + &
        factor_hi * hdo(HDR_MIDPOINT_YLOC,idx_in_hi)

      hdo(HDR_SOURCE_XGRID,idx_out) = &
        factor_lo * hdo(HDR_SOURCE_XGRID,idx_in_lo) + &
        factor_hi * hdo(HDR_SOURCE_XGRID,idx_in_hi)
      hdo(HDR_SOURCE_YGRID,idx_out) = &
        factor_lo * hdo(HDR_SOURCE_YGRID,idx_in_lo) + &
        factor_hi * hdo(HDR_SOURCE_YGRID,idx_in_hi)

      hdo(HDR_RECEIVER_XGRID,idx_out) = &
        factor_lo * hdo(HDR_RECEIVER_XGRID,idx_in_lo) + &
        factor_hi * hdo(HDR_RECEIVER_XGRID,idx_in_hi)
      hdo(HDR_RECEIVER_YGRID,idx_out) = &
        factor_lo * hdo(HDR_RECEIVER_YGRID,idx_in_lo) + &
        factor_hi * hdo(HDR_RECEIVER_YGRID,idx_in_hi)

      hdo(HDR_MIDPOINT_SHOTPOINT,idx_out) = &
        factor_lo * hdo(HDR_MIDPOINT_SHOTPOINT,idx_in_lo) + &
        factor_hi * hdo(HDR_MIDPOINT_SHOTPOINT,idx_in_hi)
      hdo(HDR_MIDPOINT_LINE,idx_out) = &
        factor_lo * hdo(HDR_MIDPOINT_LINE,idx_in_lo) + &
        factor_hi * hdo(HDR_MIDPOINT_LINE,idx_in_hi)

      return
      end subroutine reg_linear_xy


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine reg_wrapup (obj)
      implicit none
      type(reg_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine reg_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module reg_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
