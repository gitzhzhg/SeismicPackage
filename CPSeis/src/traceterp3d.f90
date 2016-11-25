!<CPS_v1 type="PROCESS"/>
!!------------------------------- traceterp3d.f90 ---------------------------------!!
!!------------------------------- traceterp3d.f90 ---------------------------------!!
!!------------------------------- traceterp3d.f90 ---------------------------------!!


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
! Name       : TRACETERP3D          (3D Trace Interpolation)
! Category   : miscellaneous
! Written    : 2010-04-07   by: Tom Stoeckley
! Revised    : 2010-04-07   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Interpolate traces in a 3D data volume.
! Portability: No known limitations.
! Parallel   : No.
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! TRACETERP3D generates interpolated traces between the input traces.  Exactly
! one interpolated trace is placed between adjacent input traces.  No extrapolations
! are performed.  Traces can be interpolated in the X direction, the Y direction,
! or both directions.
!
! TRACETERP3D can be run on 2D gathers, interpolating in the channel or offset
! direction, or on stacked 2D lines, interpolating in the CMP direction, or
! on 3D stacked datasets, interpolating in the CMP direction along lines, or
! interpolating new lines between the existing lines, or both.
!
! TRACETERP3D is functionally somewhat similar to SDIP, SDIP3D, EDA3D, and
! RANLINE in the following ways: (1) all of these processing modules use
! the MGATHER or MGATHER3D primitive to maintain a 2D or 3D moving trace
! gather; and (2) all of these processing modules use the SDIPUTIL primitive
! to do a semblance dip search and to calculate a modified or interpolated
! trace.  The SDIP and SDIP3D modules replace a trace by a smoothed version
! of the trace.  The EDA3D module replaces a trace with an edge detection
! attribute trace.  The RANLINE and TRACETERP3D modules generate interpolated
! traces.
!-------------------------------------------------------------------------------
!                           DETAILS OF OPERATION
!
! 1. Within each time window, (2*NUM_TR_DIP_X + 1) times (2*NUM_TR_DIP_Y + 1)
!    input traces, centered on the desired output trace location, are used to perform
!    a semblance dip search to determine which dip has the maximum semblance.
!    A total of (2*DIP_MAX_X + 1) times (2*DIP_MAX_Y + 1) dips are tested.
!    The dip with the maximum semblance is referred to as the dominant dip.
!    Each time window length is WIN_LEN seconds long, and the windows move down
!    the trace one sample at a time.
!
! 2. Using new smaller time windows, one input trace on each side of the desired
!    interpolated trace location is used to obtain coherency values to use for
!    calculating the interpolated trace.  These coherency values are calculated
!    along the dominant dip previously found.  Each of these new smaller time
!    windows is 5 trace samples long, and the windows move down the trace
!    one sample at a time.
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
!  1. The bin centers and bin widths are used to identify the locations
!     of input traces to use for dip searching and calculating the interpolated
!     trace.  This insures that the correct input traces are used in both
!     the X and Y directions, even if the starting X coordinates of different
!     lines are not the same or there are missing traces.  The following
!     binning parameters:
!                        X_INIT       Y_INIT
!                        X_INC        Y_INC
!     should match the actual inline and crossline spacing of the input
!     traces.  If two or more input traces fall into the same bin, only one
!     trace in the bin will be used for dip searching and for calculating
!     the interpolated trace.
!
!  2. Example of use of the following dip parameters:
!                        DIP_X         DIP_Y
!                        DIP_INC_X     DIP_INC_Y
!
!     Using DIP_X = 10 and DIP_INC_X = 2:
!            five negative X-dips (-10,-8,-6,-4,-2),
!            one zero X-dip,
!            and five positive X-dips (2,4,6,8,10) will be tested.
!
!     Using DIP_Y = 6 and DIP_INC_Y = 3:
!            two negative Y-dips (-6,-3),
!            one zero Y-dip,
!            and two positive Y-dips (3,6) will be tested.
!
!     WARNING: Computer time is proportional to the product of the number
!     of dips tested in each direction!
!
!  3. The input traces should be sorted to either inline or crossline
!     order.  The inline direction corresponds to input traces in the same
!     Y bin, and with X bins incrementing by one bin per input trace.  The
!     crossline direction corresponds to input traces in the same X bin, and
!     with Y bins incrementing by one bin per input trace.  The input trace
!     spacing should be such that there is generally one trace in each bin,
!     although some bins can be empty.
!
!  4. Each output trace is an original or interpolated trace.  Traces are
!     output in the same order as they are input.
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! This processing module is a multiple-trace process.
!
! For stacked data, the traces must be input with the primary sort in the Y
! direction and the secondary sort in the X direction.
!
! For prestack data, there are several options:
!
!!!!!!!!!!!!!!!!!!! need to work on this.................
!!!!!!!!!!!!!!!!!!! need to work on this.................
!!!!!!!!!!!!!!!!!!! need to work on this.................
!!!!!!!!!!!!!!!!!!! need to work on this.................
!!!!!!!!!!!!!!!!!!! need to work on this.................
! (1) The traces can be input in shot gathers, and the sort within each gather
! should normally be by receiver station or offset.  For 3D shot gathers, the
! sort within the gather should normally be by Y then X receiver station.
!
! (2) The traces can be input in CMP gathers, and the sort within each gather
! should normally be by offset.  For 3D CMP gathers, the sort within the gather
! should normally be by Y then X receiver or shot station.
!
! (3) The traces can be input in constant offset gathers, and the sort within
! each gather should be by Y then X CMP location.
!
! For 3D prestack gathers, a header word must be specified which changes for
! each gather, so there will be no unintended interpolation from one gather
! to the next.
!
! For prestack data, no new gathers are interpolated between the existing
! gathers; interpolation is performed only within each gather.  This
! restriction can be overcome if you parameterize this module to make it
! think the traces are post-stack.
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! This process outputs the original input traces and the interpolated traces.
! This process outputs one trace at a time.
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
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Hwd#          Description                 Action taken
! ----          -----------                 ------------
! 2             Head mute index             used but not changed
! 25            LAV                         reset
! 64            Tail mute index             used but not changed
! HDR_X         X coordinate                used but not changed
! HDR_Y         Y coordinate                used but not changed
! HDR_DIVIDER   separates separate sections used but not changed
! 58,59,60,61   Scratch                     used
!
! Most header words are interpolated from nearest neighbors in the
! interpolated traces.
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2010-04-07  Stoeckley  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY ISSUES           
!
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS        
!
! No special requirements.
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
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS    
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES              
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS TRACETERP3D Process/NC=80>
!                         3D Trace Interpolation
!
! INTERPOLATION_MODE=`CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! WIN_LEN=`FFFFFF [/L]Semblance window length in seconds for dip search.
!
! `-------------------- `--------------------
!  HDR_X~~~~~~~=`II      HDR_Y~~~~~~~=`II     [/L]Header word.
!  X_INIT~~~~~~=`FFFFF   Y_INIT~~~~~~=`FFFFF  [/L]First (or any) input bin center.
!  X_INC~~~~~~~=`FFFFF   Y_INC~~~~~~~=`FFFFF  [/L]Input bin increment.
!  MAX_X_BINS~~=`IIIII   MAX_Y_BINS~~=`IIIII  [/L]Maximum approximate number of input bins (traces).
!
!  DIP_MAX_X~~~=`FFFFF   DIP_MAX_Y~~~=`FFFFF  [/L]Maximum dip (ms per input trace).
!  dip_inc_x~~~=`XXXXX   dip_inc_y~~~=`XXXXX  [/L]Dip increment (ms per input trace).
!  nxdips~~~~~~=`XXXXX   nydips~~~~~~=`XXXXX  [/L]Total number of dips.
!
!  NUM_TR_DIP_X=`II      NUM_TR_DIP_Y=`II     [/L]Number of input traces each side for dip search.
! `-------------------- `--------------------
!
!<include sdiputil.f90>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="INTERPOLATION_MODE">
!<Tip> Whether to interpolate in the inline or crossline direction. </Tip>
! Default = INLINE ONLY                (X direction)
! Allowed = INLINE ONLY                (X direction)
! Allowed = CROSSLINE ONLY             (Y direction)
! Allowed = INLINE AND CROSSLINE       (both X and Y directions)
!
! A two-to-one interpolation is performed in the specified direction(s).
!</Help>
!
!
!<Help KEYWORD="nxdips" TYPE="DISPLAY_ONLY">
!<Tip> Total number of dips to be tested in X direction. </Tip>
!</Help>
!
!<Help KEYWORD="nydips" TYPE="DISPLAY_ONLY">
!<Tip> Total number of dips to be tested in Y direction. </Tip>
!</Help>
!
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Length of the time window, in sec, for the semblance calculation. </Tip>
! Default = 0.1
! Allowed = real > 0.0
!
! WIN_LEN is used for dip search calculations prior to generating interpolated
! traces.
!</Help>
!
!
!<Help KEYWORD="DIP_MAX_X">
!<Tip> Maximum dip in X direction to use in the dip search calculation. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
!
! Dips are in milliseconds per input trace in the X direction.
!
! A total of (2*DIP_X/DIP_INC_X + 1) dips are tested in the X direction,
! from -DIP_MAX_X to +DIP_MAX_X.
!
! Computer time is roughly proportional to the number of dips tested in the
! X direction times the number of dips tested in the Y direction.
!</Help>
!
!
!<Help KEYWORD="DIP_MAX_Y">
!<Tip> Maximum dip in Y direction to use in the dip search calculation. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
!
! Dips are in milliseconds per input trace in the Y direction.
!
! A total of (2*DIP_X/DIP_INC_Y + 1) dips are tested in the Y direction,
! from -DIP_MAX_Y to +DIP_MAX_Y.
!
! Computer time is roughly proportional to the number of dips tested in the
! X direction times the number of dips tested in the Y direction.
!</Help>
!
!
!<Help KEYWORD="dip_inc_x" TYPE="DISPLAY_ONLY">
!<Tip> Dip increment in X direction to use in the dip search calculation. </Tip>
!
! Dip increments are in milliseconds per input trace in the X direction.
!
! A total of (2*DIP_X/DIP_INC_X + 1) dips are tested in the X direction,
! from -DIP_MAX_X to +DIP_MAX_X.
!
! Computer time is roughly proportional to the number of dips tested in the
! X direction times the number of dips tested in the Y direction.
!
! This value is calculated from the Nyquist frequency and the NUM_TR_DIP_X
! parameter to keep from aliasing the dip search on the farthest traces.
!</Help>
!
!
!<Help KEYWORD="dip_inc_y" TYPE="DISPLAY_ONLY">
!<Tip> Dip increment in Y direction to use in the dip search calculation. </Tip>
!
! Dip increments are in milliseconds per input trace in the Y direction.
!
! A total of (2*DIP_X/DIP_INC_Y + 1) dips are tested in the Y direction,
! from -DIP_MAX_Y to +DIP_MAX_Y.
!
! Computer time is roughly proportional to the number of dips tested in the
! X direction times the number of dips tested in the Y direction.
!
! This value is calculated from the Nyquist frequency and the NUM_TR_DIP_Y
! parameter to keep from aliasing the dip search on the farthest traces.
!</Help>
!
!
!<Help KEYWORD="NUM_TR_DIP_X">
!<Tip> Number of input traces on each side in X direction for dip testing. </Tip>
! Default = 3
! Allowed = integer >= 0
!
! This is the number of input traces on each side of the interpolated trace.
! A total of (2*NUM_TR_DIP_X + 1) input traces are used in the dip search calculation
! in the X direction.
!
! A total of (2*NUM_TR_DIP_X + 1) times (2*NUM_TR_DIP_Y + 1) input traces are used
! altogether in the dip search calculation.
!</Help>
!
!
!<Help KEYWORD="NUM_TR_DIP_Y">
!<Tip> Number of input traces on each side in Y direction for dip testing. </Tip>
! Default = 3
! Allowed = integer >= 0
!
! This is the number of input traces on each side of the interpolated trace.
! A total of (2*NUM_TR_DIP_Y + 1) input traces are used in the dip search calculation
! in the Y direction.
!
! A total of (2*NUM_TR_DIP_X + 1) times (2*NUM_TR_DIP_Y + 1) traces are used
! altogether in the dip search calculation.
!</Help>
!
!
!<Help KEYWORD="HDR_X">
!<Tip> Header word containing the X coordinate. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word containing the Y coordinate. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="X_INIT">
!<Tip> X coordinate of the first (or any) trace in the X direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the center of any bin measured in the X direction.
! This value does not have to correspond to the first actual trace in the
! X direction, since this could in fact vary from line to line.
!</Help>
!
!
!<Help KEYWORD="Y_INIT">
!<Tip> Y coordinate of the first (or any) trace in the Y direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the center of any bin measured in the Y direction.
! This value does not have to correspond to the first actual trace (or
! first actual line) in the Y direction.
!</Help>
!
!
!<Help KEYWORD="X_INC">
!<Tip> Increment of HDR_X between bin centers in the X direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the bin increment (or width) in the X direction.
!</Help>
!
!
!<Help KEYWORD="Y_INC">
!<Tip> Increment of HDR_Y between bin centers in the Y direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the bin increment (or width) in the Y direction.
!</Help>
!
!
!<Help KEYWORD="MAX_X_BINS">
!<Tip> Maximum number of X bins (traces) in any one line. </Tip>
! Default = blank
! Allowed = integer > 0
!
! This value must be specified.  The maximum number of traces which will
! be stored on disk at any one time will be slightly greater than MAX_X_BINS
! times (2*NUM_TR_DIP_Y + 1).
!</Help>
!
!
!<Help KEYWORD="MAX_Y_BINS">
!<Tip> Maximum number of Y bins (lines). </Tip>
! Default = blank
! Allowed = integer > 0 (or blank if not requesting diagnostic traces)
!
! This value must be specified if you are requesting any diagnostic output
! files (see separate screen).
!
! This parameter is needed only for reserving disk space for the requested
! files, and needs to be approximate only.  But if the files will be
! very large, the parameter is important because the number of available
! file extents may otherwise be insufficient.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module traceterp3d_module
      use pc_module
      use named_constants_module
      use mgather3d_module
      use sdiputil_module
      use tweights_module
      implicit none
      private
      public :: traceterp3d_create
      public :: traceterp3d_initialize
      public :: traceterp3d_update
      public :: traceterp3d_delete
      public :: traceterp3d            ! main execution (trace processing) routine.
      public :: traceterp3d_wrapup

      character(len=100),public,save :: TRACETERP3D_IDENT = &
      '$Id: traceterp3d.f90,v 1.6 2006/09/18 13:32:46 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: traceterp3d_struct

        private
        logical             :: skip_wrapup          ! wrapup flag
        integer             :: nwih,ndpt            ! globals
        real                :: tstrt,dt             ! globals

        character(len=28)   :: opt_output           ! not in gui - sdiputil
        real                :: pwr_edge             ! not in gui - sdiputil
        real                :: pwr_semb             ! not in gui - sdiputil
        logical             :: opt_semb             ! not in gui - sdiputil
        real                :: fctr_mix             ! not in gui - sdiputil
        integer             :: win_nsamp            ! not in gui - sdiputil
        logical             :: quick_dip_weights    ! not in gui - sdiputil
        logical             :: quick_dip_search     ! not in gui - sdiputil
        integer             :: num_tr_mix_x         ! not in gui - sdiputil
        integer             :: num_tr_mix_y         ! not in gui - sdiputil
        character(len=8)    :: opt_taper            ! not in gui - tweights

        character(len=28)   :: interpolation_mode   ! parameter
        real                :: win_len              ! parameter - sdiputil
        real                :: dip_max_x            ! parameter - sdiputil
        real                :: dip_max_y            ! parameter - sdiputil
        integer             :: num_tr_dip_x         ! parameter - sdiputil (altered)
        integer             :: num_tr_dip_y         ! parameter - sdiputil (altered)
        integer             :: hdr_x                ! parameter - sdiputil mgather3d
        integer             :: hdr_y                ! parameter - sdiputil mgather3d
        real                :: x_init               ! parameter - mgather3d
        real                :: y_init               ! parameter - mgather3d
        real                :: x_inc                ! parameter - sdiputil mgather3d (altered)
        real                :: y_inc                ! parameter - sdiputil mgather3d (altered)
        integer             :: max_x_bins           ! parameter - sdiputil mgather3d (altered)
        integer             :: max_y_bins           ! parameter - sdiputil (altered)

        integer                        :: ngather        ! dependent
        type(mgather3d_struct),pointer :: mgather3d      ! dependent
        type(sdiputil_struct) ,pointer :: sdiputil       ! dependent

      end type traceterp3d_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer,parameter     :: SCRATCH_NSX = HDR_SCRATCH_58
      integer,parameter     :: SCRATCH_NSY = HDR_SCRATCH_59
      integer,parameter     :: SCRATCH_NSW = HDR_SCRATCH_60
      integer,parameter     :: SCRATCH_NSF = HDR_SCRATCH_61

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine traceterp3d_create (obj)
      implicit none
      type(traceterp3d_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%mgather3d)
      nullify (obj%sdiputil) ! jpa

      call sdiputil_create        (obj%sdiputil)
      call traceterp3d_initialize (obj)
      return
      end subroutine traceterp3d_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine traceterp3d_delete (obj)
      implicit none
      type(traceterp3d_struct),pointer :: obj       ! arguments

      call traceterp3d_wrapup (obj)
      call sdiputil_delete    (obj%sdiputil)

      deallocate(obj)
      return
      end subroutine traceterp3d_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine traceterp3d_initialize (obj)
      implicit none
      type(traceterp3d_struct),intent(inout) :: obj       ! arguments

      obj%opt_output         = 'MIXED TRACES'   ! not in gui - sdiputil
      obj%pwr_edge           = 2.0              ! not in gui - sdiputil
      obj%pwr_semb           = 0.5              ! not in gui - sdiputil
      obj%opt_semb           = .false.          ! not in gui - sdiputil
      obj%fctr_mix           = 0.0              ! not in gui - sdiputil
      obj%win_nsamp          = 5                ! not in gui - sdiputil
      obj%quick_dip_weights  = .true.           ! not in gui - sdiputil
      obj%quick_dip_search   = .true.           ! not in gui - sdiputil
      obj%num_tr_mix_x       = 1                ! not in gui - sdiputil
      obj%num_tr_mix_y       = 1                ! not in gui - sdiputil
      obj%opt_taper          = 'NONE'           ! not in gui - tweights

      obj%interpolation_mode = 'INLINE ONLY'    ! parameter
      obj%win_len            = 0.10             ! parameter - sdiputil
      obj%dip_max_x          = 10.0             ! parameter - sdiputil
      obj%dip_max_y          = 10.0             ! parameter - sdiputil
      obj%num_tr_dip_x       = 3                ! parameter - sdiputil (altered)
      obj%num_tr_dip_y       = 3                ! parameter - sdiputil (altered)
      obj%hdr_x              = 7                ! parameter - sdiputil mgather3d
      obj%hdr_y              = 8                ! parameter - sdiputil mgather3d
      obj%x_init             = 1.0              ! parameter - mgather3d
      obj%y_init             = 1.0              ! parameter - mgather3d
      obj%x_inc              = 1.0              ! parameter - sdiputil mgather3d (altered)
      obj%y_inc              = 1.0              ! parameter - sdiputil mgather3d (altered)
      obj%max_x_bins         = INIL             ! parameter - sdiputil mgather3d (altered)
      obj%max_y_bins         = INIL             ! parameter - sdiputil (altered)

      call sdiputil_initialize (obj%sdiputil)
      call traceterp3d_update  (obj)
      return
      end subroutine traceterp3d_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine traceterp3d_update (obj)
      implicit none
      type(traceterp3d_struct),intent(inout) :: obj                   ! arguments
      integer                                :: lun                   ! local
      integer                                :: nstore,nscratch       ! local
      real,allocatable                       :: xweights(:)           ! local
      real,allocatable                       :: yweights(:)           ! local
      integer                                :: nxgather,nxmix,nxdips ! local
      integer                                :: nygather,nymix,nydips ! local
      real                                   :: dip_inc_x,dip_inc_y   ! local
      real                                   :: local_x_inc           ! local
      real                                   :: local_y_inc           ! local
      integer                                :: local_num_tr_dip_x    ! local
      integer                                :: local_num_tr_dip_y    ! local
      integer                                :: local_max_x_bins      ! local
      integer                                :: local_max_y_bins      ! local

      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      lun = pc_get_lun()

      call pc_get_global ('nwih'              , obj%nwih)
      call pc_get_global ('ndpt'              , obj%ndpt)
      call pc_get_global ('tstrt'             , obj%tstrt)
      call pc_get_global ('dt'                , obj%dt)

      call pc_get        ('interpolation_mode', obj%interpolation_mode)
      call pc_get        ('win_len'           , obj%win_len           ) ! sdiputil
      call pc_get        ('dip_max_x'         , obj%dip_max_x         ) ! sdiputil
      call pc_get        ('dip_max_y'         , obj%dip_max_y         ) ! sdiputil
      call pc_get        ('num_tr_dip_x'      , obj%num_tr_dip_x      ) ! sdiputil (altered)
      call pc_get        ('num_tr_dip_y'      , obj%num_tr_dip_y      ) ! sdiputil (altered)
      call pc_get        ('hdr_x'             , obj%hdr_x             ) ! sdiputil mgather3d
      call pc_get        ('hdr_y'             , obj%hdr_y             ) ! sdiputil mgather3d
      call pc_get        ('x_init'            , obj%x_init            ) ! mgather3d
      call pc_get        ('y_init'            , obj%y_init            ) ! mgather3d
      call pc_get        ('x_inc'             , obj%x_inc             ) ! sdiputil mgather3d (altered)
      call pc_get        ('y_inc'             , obj%y_inc             ) ! sdiputil mgather3d (altered)
      call pc_get        ('max_x_bins'        , obj%max_x_bins        ) ! sdiputil mgather3d (altered)
      call pc_get        ('max_y_bins'        , obj%max_y_bins        ) ! sdiputil (altered)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      local_x_inc        = obj%x_inc
      local_y_inc        = obj%y_inc
      local_num_tr_dip_x = obj%num_tr_dip_x
      local_num_tr_dip_y = obj%num_tr_dip_y
      local_max_x_bins   = obj%max_x_bins  
      local_max_y_bins   = obj%max_y_bins  

      if (obj%interpolation_mode == 'INLINE ONLY' .or. &
          obj%interpolation_mode == 'INLINE AND CROSSLINE') then
                                        local_x_inc        = 0.5 * obj%x_inc
                                        local_num_tr_dip_x = 2   * obj%num_tr_dip_x
            if (obj%max_x_bins /= INIL) local_max_x_bins   = 2   * obj%max_x_bins  
      endif

      if (obj%interpolation_mode == 'CROSSLINE ONLY' .or. &
          obj%interpolation_mode == 'INLINE AND CROSSLINE') then
                                        local_y_inc        = 0.5 * obj%y_inc
                                        local_num_tr_dip_y = 2   * obj%num_tr_dip_y
            if (obj%max_y_bins /= INIL) local_max_y_bins   = 2   * obj%max_y_bins  
      endif

      call sdiputil_update (obj%sdiputil, obj%opt_output,                 &
                 obj%pwr_edge, obj%pwr_semb, obj%opt_semb, obj%fctr_mix,  &
                 obj%win_len          , obj%win_nsamp       ,             &
                 obj%quick_dip_weights, obj%quick_dip_search,             &
                 obj%dip_max_x        , obj%dip_max_y       ,             &
                 local_num_tr_dip_x   , local_num_tr_dip_y  ,             &
                 obj%num_tr_mix_x     , obj%num_tr_mix_y    ,             &
                 obj%hdr_x            , obj%hdr_y           ,             &
                 local_x_inc          , local_y_inc         ,             &
                 local_max_x_bins     , local_max_y_bins    ,             &
                 nxgather             , nygather            ,             &
                 nxmix                , nymix               ,             &
                 nxdips               , nydips              ,             &
                 dip_inc_x            , dip_inc_y)

      obj%ngather = nxgather * nygather


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('interpolation_mode',               &
                                   (/'INLINE ONLY                 ', &
                                     'CROSSLINE ONLY              ', &
                                     'INLINE AND CROSSLINE        '/))

!     call pc_put_global ('gathered'   , .false.)     ! set by sdiputil.
!     call pc_put_global ('numtr'      ,       1)     ! set by sdiputil.

      call pc_put        ('interpolation_mode', obj%interpolation_mode )
      call pc_put        ('win_len'           , obj%win_len          ,7) ! sdiputil
      call pc_put        ('dip_max_x'         , obj%dip_max_x        ,6) ! sdiputil
      call pc_put        ('dip_max_y'         , obj%dip_max_y        ,6) ! sdiputil
      call pc_put        ('num_tr_dip_x'      , obj%num_tr_dip_x       ) ! sdiputil (altered)
      call pc_put        ('num_tr_dip_y'      , obj%num_tr_dip_y       ) ! sdiputil (altered)
      call pc_put        ('hdr_x'             , obj%hdr_x              ) ! sdiputil mgather3d
      call pc_put        ('hdr_y'             , obj%hdr_y              ) ! sdiputil mgather3d
      call pc_put        ('x_init'            , obj%x_init           ,6) ! mgather3d
      call pc_put        ('y_init'            , obj%y_init           ,6) ! mgather3d
      call pc_put        ('x_inc'             , obj%x_inc            ,6) ! sdiputil mgather3d (altered)
      call pc_put        ('y_inc'             , obj%y_inc            ,6) ! sdiputil mgather3d (altered)
      call pc_put        ('max_x_bins'        , obj%max_x_bins         ) ! sdiputil mgather3d (altered)
      call pc_put        ('max_y_bins'        , obj%max_y_bins         ) ! sdiputil (altered)

      call pc_put_gui_only ('dip_inc_x' , dip_inc_x      ,6,2)
      call pc_put_gui_only ('dip_inc_y' , dip_inc_y      ,6,2)
      call pc_put_gui_only ('nxdips'    , nxdips         )
      call pc_put_gui_only ('nydips'    , nydips         )

      nscratch = mgather3d_scratch(obj%nwih,obj%ndpt,nxgather,nygather) &
                   + obj%ngather * (obj%nwih + obj%ndpt)
      nstore   = mgather3d_store &
                      (obj%nwih,obj%ndpt,nxgather,nygather,local_max_x_bins)

      call pc_put_control ('need_request', .true.)
      call pc_put_control ('need_label'  , .true.)
      call pc_put_control ('twosets'     , .true.)
      call pc_put_control ('nscratch'    , nscratch)
      call pc_put_control ('nstore'      , nstore)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      call mgather3d_delete (obj%mgather3d)   ! in case previously created.

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      allocate (xweights(nxgather))
      allocate (yweights(nygather))

      call tweights_calculate (obj%opt_taper, xweights, nxgather, nxmix)
      call tweights_calculate (obj%opt_taper, yweights, nygather, nymix)

      write(lun,*) 'TRACETERP3D: X weights = ',xweights
      write(lun,*) 'TRACETERP3D: Y weights = ',yweights

      call mgather3d_create (obj%mgather3d, lun, obj%nwih, obj%ndpt,     &
                             obj%hdr_x, obj%hdr_y,                       &
                             obj%x_init, obj%y_init,                     &
                             local_x_inc, local_y_inc,                   &
                             nxgather, nygather,                         &
                             xweights, yweights,                         &
                             SCRATCH_NSX, SCRATCH_NSY,                   &
                             SCRATCH_NSW, SCRATCH_NSF, local_max_x_bins, &
                             'TRACETERP3D')

      deallocate (xweights)
      deallocate (yweights)

      call sdiputil_prepare (obj%sdiputil,SCRATCH_NSX,SCRATCH_NSY,SCRATCH_NSW)


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine traceterp3d_update


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine traceterp3d (obj,ntr,hdi,tri,hdo,tro)
      implicit none
      type(traceterp3d_struct),intent(inout) :: obj         ! arguments
      integer                 ,intent(inout) :: ntr         ! arguments
      double precision        ,intent(inout) :: hdi(:,:)    ! arguments
      real                    ,intent(in)    :: tri(:,:)    ! arguments
      double precision        ,intent(out)   :: hdo(:,:)    ! arguments
      real                    ,intent(out)   :: tro(:,:)    ! arguments
      double precision     :: hdmove(obj%nwih,obj%ngather)  ! local
      real                 :: trmove(obj%ndpt,obj%ngather)  ! local
      integer              :: mid                           ! local
      real                 :: xmid,ymid                     ! local

!----------add dead interpolated traces to the input traces.

! I think the following will not work, because mgather3d will only return
! gathers centered on existing input traces, not on each location specified
! by x_init, x_inc, y_init, and y_inc.  Therefore, to make this work, a dead trace
! must be passed to mgather3d for each desired interpolated trace location.
! One way to do this might be to require the traces to be ungathered coming in,
! and then, when getting each trace, add also a dead trace before or after it
! (if interpolating in the X direction), and add an entire dead line before or
! after a live line (if interpolating in the Y direction).  A primitive could
! be written to manage this.  Then this processing module would simply output
! an interpolated trace for each input dead trace, or output the original input
! trace if it is not dead.

! The FILL process can be used to create dead traces between live traces,
! which can then be turned into interpolated traces by this process or even
! by EDA3d.

! It is also possible that sdip3d might just output a dead trace instead of
! replacing it, but I'll have to check this (maybe not since ranline uses
! sdiputil).

! Another way to do this might be to use mgather3d for input traces similarly
! to SDIP3D and EDA3D.  Then, when getting a moving gather from mgather3d, first
! output the middle trace unaltered, then create a dead trace half a bin later,
! set it to hdo and tro, then call sdiputil, then output the resulting
! interpolated trace.  When interpolating in the Y direction, all the moving
! gathers would have to be saved and used a second time to generate the
! interpolated line, creating in turn a dead trace half a bin lower than the
! middle trace in each gather.  Or, instead of saving the moving gathers,
! create and output the interpolated traces right away for the interpolated line.
! This will result in output traces which would have to be sorted by TSORT.

! Here might be a way to interpolate a dataset without writing any new processes
! (including this one): First, run the traces through FILL to create dead traces
! between live traces.  Second, run the traces through EDA3D to create mixed
! traces which would be interpolated traces with a careful setting of parameters.
! A possible downside is the requirement by FILL to specify the range of bins,
! which might result in a lot of extrapolated traces around the edge of the
! dataset if it has uneven boundaries.

!----------get the mix gather.

      call mgather3d (obj%mgather3d,ntr,hdi,tri,hdmove,trmove)

      if (ntr == NEED_TRACES) return

      if (ntr == FATAL_ERROR) then
           call pc_error ('TRACETERP3D: FATAL ERROR IN MGATHER3D')
           call traceterp3d_wrapup (obj)
           return
      end if

      if (ntr == NO_MORE_TRACES) then
           call traceterp3d_wrapup (obj)
           return
      end if

!----------get output trace.

      mid  = (ntr+1)/2
      xmid = hdmove(SCRATCH_NSX,mid)
      ymid = hdmove(SCRATCH_NSY,mid)

      call sdiputil_solve (obj%sdiputil,ntr,hdmove,trmove,xmid,ymid,hdo,tro)

      if (ntr == FATAL_ERROR) then
           call pc_error ('TRACETERP3D: FATAL ERROR IN SDIPUTIL')
           call traceterp3d_wrapup (obj)
      end if
      return
      end subroutine traceterp3d


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine traceterp3d_wrapup (obj)
      implicit none
      type(traceterp3d_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call pc_print         (' ')
      call pc_print         ('TRACETERP3D: WRAPUP')
      call mgather3d_delete (obj%mgather3d)
      call sdiputil_wrapup  (obj%sdiputil)
      call pc_print         ('TRACETERP3D: FINISHED')
      call pc_print         (' ')
      return
      end subroutine traceterp3d_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module traceterp3d_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

