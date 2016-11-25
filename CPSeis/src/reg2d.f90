!<CPS_v1 type="PROCESS"/>
!!------------------------------- reg2d.f90 ---------------------------------!!
!!------------------------------- reg2d.f90 ---------------------------------!!
!!------------------------------- reg2d.f90 ---------------------------------!!


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
! Name       : REG2D          (2D Regularization)
! Category   : filters
! Written    : 2004-04-16   by: Tom Stoeckley
! Revised    : 2005-03-07   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Regularize 2D streamer shot profiles.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! REG2D regularizes traces within shot profiles, and from one shot profile
! to another, in order to make each acquisition line of 3D data look like
! a regularized 2D line.
!
! Each shot profile will contain the same number of traces, in regularized
! receiver increments beginning at zero offset.
!
! All shot profiles will be located at regularized source locations along
! the line.
!
! Duplicate sources or receivers are deleted.
! Receivers beyond the specified maximum offset are deleted.
! Missing sources or receivers are filled in with dead traces.
!
! Exception: If STARTING_OFFSET is greater than zero, missing near offsets
! smaller than STARTING_OFFSET will not be filled in.
!
! This process assumes that the input data is arranged like traditional
! marine acquisition, with receivers in a cable behind the source.
!
! Swath data (multiple sources and/or multiple cables) should be treated
! as if each source/cable combination is a different line.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! When users want to use this algorithm for preparing data used for "surface
! multiple attenuation (IBSMA)", they agree that their input data
! meets the following requirements.
!
! 1. One input file per job can contain data along one 3D sail line with
!    multiple cables. Input data order (fastest first) should be: time,
!    receiver, shot, cable. For one source/cable combination, input shot
!    gathers should follow "FIELD ACQUISITION ORDER", i.e., the first shot
!    acquired in the field comes in "FIRST" and the last one comes in "LAST".
!    Within a shot gather, traces are ordered from near to far offset.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! This process is a simple multiple-trace process which works with only one
! trace at a time.
!
! This process requires traces to be input one at a time (not gathered).
!
! The traces must be sorted as follows:
!
!  (1) The primary sort must be such that all traces for a single line
!      are together, although the line numbers need not be in any particular
!      order.  A new line starts when header word 26 or 27 changes.
!
!  (2) The secondary sort must be by shot profile (in the order of the
!      desired output 2D source coordinate).  A new shot profile starts
!      when header word 9, 11, or 12 changes.
!
!  (3) The tertiary sort must be from nearest to farthest offset within
!      a shot profile.  The offset is in header word 6.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process does not alter input traces.
! This process outputs one trace at a time.
!
! This process outputs traces in the same order as the input traces, but
! with some input traces deleted and some new dead traces inserted.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name                  Description                 Action taken
! ----                  -----------                 ------------
! NUMTR         max number of traces input/output   verified to be equal to 1
! NWIH          number of words in trace header     used and possibly increased
! NDPT          number of sample values in trace    used but not changed
! REG2D_SOURCE_INC      new global                  set to SOURCE_INC
! REG2D_RECEIVER_INC    new global                  set to RECEIVER_INC
! REG2D_LAST_CHANNEL    new global                  set to NUM_CHANNELS
! REG2D_NUM_SHOTS       new global                  set to NUM_SHOTS
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#          Description              Action taken
! ----          -----------              ------------
!
!        >>> INPUT TRACE HEADERS:
!
! 11           source X survey coord    used to identify and locate source
! 12           source Y survey coord    used to identify and locate source
! 14           receiver X survey coord  used to locate receiver
! 15           receiver Y survey coord  used to locate receiver
! 6            offset                   used to locate offset
! 9            original group           used to identify source
! 26           source line number       used to identify line change
! 27           receiver line number     used to identify line change
!
!        >>> ALL OUTPUT TRACE HEADERS:
!
! 1             sequential trace number set to sequential trace number
! 3             current gather          set to source index within line
! 4             current channel         set to receiver index within shot
! 26            source line number      set to line index
! 27            receiver line number    set to line index
! HDR_SOURCE    source 2D coordinate    set to regularized coordinate
! HDR_RECEIVER  receiver 2D coordinate  set to regularized coordinate
! HDR_MIDPOINT  midpoint 2D coordinate  set to regularized coordinate
! HDR_CMP_INDEX midpoint 2D index       set to regularized index
! HDR_INSERTED  whether trace inserted  set to 0 (input) or -97 (new dead trace)
!
!        >>> OUTPUT TRACE HEADERS OF NEW DEAD TRACES:
!
! 11           source X survey coord    set to interpolated value
! 12           source Y survey coord    set to interpolated value
! 14           receiver X survey coord  set to interpolated value
! 15           receiver Y survey coord  set to interpolated value
! 6            offset                   set to interpolated value
! 9            original group           set to input value
! 26           source line number       set to input value
! 27           receiver line number     set to input value
! 2            top mute index           set to 1
! 5            fold                     set to 1
! 64           bottom mute index        set to NDPT
!              all other headers        set to 0
!
!
! Hwd3 will be set to 1 for the first shot on each line, and will increment
! by the ratio SOURCE_INC/RECEIVER_INC for subsequent shots.
!
! Hwd4 will be set to 1 for the zero offset receiver in each shot, and will
! increment by 1 for subsequent receivers, up to NUM_CHANNELS for the last
! receiver in the shot.
!
! Hwd26 and Hwd27 will be set to 1 for the first line, and will increment
! by the 1 for subsequent lines.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  6. 2005-03-07  Stoeckley  Add ability to abort if the shots appear to
!                             be received in reverse order.
!  5. 2004-09-07  Y. Shen    Add one advice for users of input data order.
!  4. 2004-06-01  Stoeckley  Fix bug introduced in previous revision.
!  3. 2004-05-18  Stoeckley  Fix bug regarding same shot input twice; improved
!                             debug printouts.
!  2. 2004-04-27  Stoeckley  Add user-defined header word for CMP index.
!  1. 2004-04-16  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY ISSUES
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
!<NS REG2D Process/NC=80>
!                         Regularize 2D Streamer Data
!
! `---------------------------
!   SOURCE_INC~~=`FFFFFFFFFF   [/L]Source increment (from one shot to the next).
!   RECEIVER_INC=`FFFFFFFFFF   [/L]Receiver increment (within a cable).
!   RATIO~~~~~~~=`II           [/L]Ratio source/receiver increment (integer).
! `---------------------------
!
! `---------------------------
!   MIN_OFFSET~~=`XXXXXXXXXX   [/L]Minimum output offset.
!   OFFSET_INC~~=`XXXXXXXXXX   [/L]Output offset increment (within a cable).
!   MAX_OFFSET~~=`FFFFFFFFFF   [/L]Maximum output offset.
!   NUM_CHANNELS=`IIIIII       [/L]Number of output channels per shot gather.
! `---------------------------
!
!   STARTING_OFFSET=`FFFFFFFFFF   [/L]Missing near offsets always filled in starting here.
!   NUM_SHOTS~~~~~~=`IIIIII       [/L]Number of output shot gathers per line.
!
! `-------------------
!   HDR_SOURCE~~~=`II   [/L]Header word set to regularized 2D source coordinate.
!   HDR_RECEIVER =`II   [/L]Header word set to regularized 2D receiver coordinate.
!   HDR_MIDPOINT =`II   [/L]Header word set to regularized 2D midpoint coordinate.
!   HDR_CMP_INDEX=`II   [/L]Header word set to regularized 2D midpoint index.
!   HDR_INSERTED =`II   [/L]Header word set to 0 if existing trace or -97 if inserted.
! `-------------------
!
!          NWIH_INPUT=`XX    NWIH_OUTPUT=`XX        DEBUG_PRINT=`IIIIII
!
!          DIRECTION_CHECK=`KKK
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="STARTING_OFFSET">
!<Tip> Missing near offsets are always filled in starting at this offset. </Tip>
! Default = 0.0
! Allowed = real >= 0.0 and <= MAX_OFFSET
!
! If STARTING_OFFSET is zero, each output source gather will contain all
! offsets from zero offset to MAX_OFFSET.  All missing offsets will be filled
! in with dead traces.
!
! If STARTING_OFFSET is greater than zero, each output source gather will
! contain all offsets from STARTING_OFFSET to MAX_OFFSET.  But if the nearest
! pre-existing offset in the gather is smaller than STARTING_OFFSET, each
! output source gather will contain all offsets from the nearest pre-existing
! offset in the gather to MAX_OFFSET.
!</Help>
!
!
!<Help KEYWORD="DEBUG_PRINT">
!<Tip> Number of traces to print for debugging. </Tip>
! Default = 0
! Allowed = int >= 0
!</Help>
!
!
!<Help KEYWORD="DIRECTION_CHECK">
!<Tip> Whether to check the direction of the input data. </Tip>
! Default = YES
! Allowed = YES or NO
!
! If YES, this process will abort with a message if the shots appear to be
! received in reverse order.  This process checks the source and receiver
! coordinates to verify that they change in a way that indicates that the
! receivers are trailing the source.  If the receivers are preceding the
! source, the assumption is that the shots are being received in reverse
! order.
!</Help>
!
!
!<Help KEYWORD="NWIH_INPUT">
!<Tip> Number of header words for input traces. </Tip>
!</Help>
!
!<Help KEYWORD="NWIH_OUTPUT">
!<Tip> Number of header words for output traces. </Tip>
!</Help>
!
!<Help KEYWORD="MIN_OFFSET">
!<Tip> Minimum output offset (always zero). </Tip>
!</Help>
!
!<Help KEYWORD="OFFSET_INC">
!<Tip> Offset increment (same as receiver increment). </Tip>
!</Help>
!
!
!<Help KEYWORD="SOURCE_INC">
!<Tip> Source increment (from one shot to the next). </Tip>
! Default = 100.0
! Allowed = real > 0.0    (must be a multiple of RECEIVER_INC)
!
! This value must be in units of the offset and survey coordinates (normally
! feet or meters) which are stored in header words 6, 11, 12, 14, and 15.
!</Help>
!
!
!<Help KEYWORD="RECEIVER_INC">
!<Tip> Receiver increment (within a cable). </Tip>
! Default = 100.0
! Allowed = real > 0.0
!
! This value must be in units of the offset and survey coordinates (normally
! feet or meters) which are stored in header words 6, 11, 12, 14, and 15.
!</Help>
!
!
!<Help KEYWORD="RATIO">
!<Tip> Ratio source/receiver increment (integer). </Tip>
! Default = 1
! Allowed = int > 0
!</Help>
!
!
!<Help KEYWORD="MAX_OFFSET">
!<Tip> Maximum offset. </Tip>
! Default = 10000.0
! Allowed = real > 0.0    (must be a multiple of RECEIVER_INC)
!
! This value is used to determine the number of traces in each output
! source gather.  The nearest offset will be zero.  Dead traces will be
! added at short, intermediate, and long offsets to fill out the source
! gather.  Input traces beyond the maximum offset will be deleted.
!
! This value must be in units of the offset and survey coordinates (normally
! feet or meters) which are stored in header words 6, 11, 12, 14, and 15.
!</Help>
!
!
!<Help KEYWORD="NUM_CHANNELS">
!<Tip> Number of output channels per shot gather. </Tip>
! Default = 101
! Allowed = integer > 0
!
! This value is the number of traces which should reside in each regularized
! output source gather.  The nearest offset will be zero.  Dead traces will
! added at short, intermediate, and long offsets to fill out the source
! gather.  Input traces beyond the maximum offset will be deleted.
!</Help>
!
!
!<Help KEYWORD="NUM_SHOTS">
!<Tip> Number of output shot gathers per line. </Tip>
! Default = 0
! Allowed = integer >= 0
!
! This value is the number of source gathers which should reside in each
! regularized output line.  If the number of input source gathers for any
! line is less than this value, the line will be padded with additional
! dead source gathers to bring the total number of source gathers up to
! this value.  If the number of input source gathers for any line exceeds
! this value, the excess source gathers will not be output.
!
! If this number is zero, output lines are not padded with extra source
! gathers beyond the last input source gather for the line.
!</Help>
!
!
!<Help KEYWORD="HDR_SOURCE">
!<Tip> Header word set to regularized 2D source coordinate. </Tip>
! Default = 0
! Allowed = 0                   (do not save coordinate)
! Allowed = 48 thru 55          (save coordinate in user-defined header word)
! Allowed = 65 thru to NWIH+5   (save coordinate in user-defined header word)
!
! If this header word number is not zero, this header word will receive
! the regularized 2D source coordinate.
!
! This value will be in units of the offset and survey coordinates (normally
! feet or meters) which are stored in header words 6, 11, 12, 14, and 15.
!</Help>
!
!
!<Help KEYWORD="HDR_RECEIVER">
!<Tip> Header word set to regularized 2D receiver coordinate. </Tip>
! Default = 0
! Allowed = 0                   (do not save coordinate)
! Allowed = 48 thru 55          (save coordinate in user-defined header word)
! Allowed = 65 thru to NWIH+5   (save coordinate in user-defined header word)
!
! If this header word number is not zero, this header word will receive
! the regularized 2D receiver coordinate.
!
! This value will be in units of the offset and survey coordinates (normally
! feet or meters) which are stored in header words 6, 11, 12, 14, and 15.
!</Help>
!
!
!<Help KEYWORD="HDR_MIDPOINT">
!<Tip> Header word set to regularized 2D midpoint coordinate. </Tip>
! Default = 0
! Allowed = 0                   (do not save coordinate)
! Allowed = 48 thru 55          (save coordinate in user-defined header word)
! Allowed = 65 thru to NWIH+5   (save coordinate in user-defined header word)
!
! If this header word number is not zero, this header word will receive
! the regularized 2D midpoint coordinate.
!
! This value will be in units of the offset and survey coordinates (normally
! feet or meters) which are stored in header words 6, 11, 12, 14, and 15.
!</Help>
!
!
!<Help KEYWORD="HDR_CMP_INDEX">
!<Tip> Header word set to regularized 2D midpoint index. </Tip>
! Default = 0
! Allowed = 0                   (do not save CMP index)
! Allowed = 48 thru 55          (save CMP index in user-defined header word)
! Allowed = 65 thru to NWIH+5   (save CMP index in user-defined header word)
!
! If this header word number is not zero, this header word will receive
! the regularized 2D midpoint index.
!</Help>
!
!
!<Help KEYWORD="HDR_INSERTED">
!<Tip> Header word set to 0 if existing trace or -97 if inserted. </Tip>
! Default = 0
! Allowed = 0                   (do not save flag)
! Allowed = 48 thru 55          (save flag in user-defined header word)
! Allowed = 65 thru to NWIH+5   (save flag in user-defined header word)
!
! If this header word number is not zero, this header word will receive
! a flag indicating whether this trace is a surviving input trace or has
! been created as a dead fill trace.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module reg2d_module
      use pc_module
      use named_constants_module
      use terputil_module
      use mth_module
      implicit none
      private
      public :: reg2d_create
      public :: reg2d_initialize
      public :: reg2d_update
      public :: reg2d_delete
      public :: reg2d
      public :: reg2d_wrapup

      character(len=100),public,save :: REG2D_IDENT = &
"$Id: reg2d.f90,v 1.6 2005/03/07 13:49:17 Stoeckley prod sps $"


!!------------------------------ bless structure --------------------------!!
!!------------------------------ bless structure --------------------------!!
!!------------------------------ bless structure --------------------------!!


      integer,private,parameter :: MAXBLESS = 7

      type,public :: reg2d_bless
        integer     :: source_xloc  (MAXBLESS)
        integer     :: source_yloc  (MAXBLESS)
        integer     :: receiver_xloc(MAXBLESS)
        integer     :: receiver_yloc(MAXBLESS)
        integer     :: offset       (MAXBLESS)
        integer     :: tolerance                ! offset tolerance
        integer     :: reference                ! reference offset
        integer     :: num
      end type reg2d_bless


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: reg2d_struct

        private
        logical           :: skip_wrapup       ! wrapup flag
        integer           :: nwih_input        ! global
        integer           :: nwih_output       ! global
        integer           :: ndpt              ! global

        real              :: starting_offset   ! process parameter
        real              :: source_inc        ! process parameter
        real              :: receiver_inc      ! process parameter
        integer           :: ratio             ! process parameter
        real              :: max_offset        ! process parameter
        integer           :: num_channels      ! process parameter
        integer           :: num_shots         ! process parameter
        integer           :: hdr_source        ! process parameter
        integer           :: hdr_receiver      ! process parameter
        integer           :: hdr_midpoint      ! process parameter
        integer           :: hdr_cmp_index     ! process parameter
        integer           :: hdr_inserted      ! process parameter
        integer           :: debug_print       ! process parameter
        logical           :: direction_check   ! process parameter

        integer     :: prev_current_line       ! previous input trace output
        integer     :: prev_current_gather     ! previous input trace output
        integer     :: prev_current_channel    ! previous input trace output
        real        :: prev_offset             ! previous input trace output
        integer     :: prev_original_group     ! previous input trace output
        real        :: prev_source_xloc        ! previous input trace output
        real        :: prev_source_yloc        ! previous input trace output
        real        :: prev_receiver_xloc      ! previous input trace output
        real        :: prev_receiver_yloc      ! previous input trace output
        integer     :: prev_source_line        ! previous input trace output
        integer     :: prev_receiver_line      ! previous input trace output

        integer     :: out_sequence            ! last output trace (hdr  1)

        integer     :: out_current_line        ! last output trace (hdr 26&27)
        integer     :: out_current_gather      ! last output trace (hdr  3)
        integer     :: out_current_channel     ! last output trace (hdr  4)
        real        :: out_offset              ! last output trace (hdr  6)
        integer     :: out_original_group      ! last output trace (hdr  9)
        real        :: out_source_xloc         ! last output trace (hdr 11)
        real        :: out_source_yloc         ! last output trace (hdr 12)
        real        :: out_receiver_xloc       ! last output trace (hdr 14)
        real        :: out_receiver_yloc       ! last output trace (hdr 15)
        integer     :: out_source_line         ! last output trace (hdr 26)
        integer     :: out_receiver_line       ! last output trace (hdr 27)

        integer     :: next_current_line       ! next input trace to output
        integer     :: next_current_gather     ! next input trace to output
        integer     :: next_current_channel    ! next input trace to output
        real        :: next_offset             ! next input trace to output
        integer     :: next_original_group     ! next input trace to output
        real        :: next_source_xloc        ! next input trace to output
        real        :: next_source_yloc        ! next input trace to output
        real        :: next_receiver_xloc      ! next input trace to output
        real        :: next_receiver_yloc      ! next input trace to output
        integer     :: next_source_line        ! next input trace to output
        integer     :: next_receiver_line      ! next input trace to output

        integer           :: starting_channel        ! dependent
        logical           :: input_trace_available   ! dependent
        logical           :: more_input_available    ! dependent
        type(reg2d_bless) :: bless                   ! dependent

      end type reg2d_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                   ,save :: lunprint  ! unit number for printing.
      type(reg2d_struct),pointer,save :: object    ! needed for traps.

      integer,parameter,private :: SCRATCH_SOURCE    = HDR_SCRATCH_58
      integer,parameter,private :: SCRATCH_RECEIVER  = HDR_SCRATCH_59
      integer,parameter,private :: SCRATCH_MIDPOINT  = HDR_SCRATCH_60
      integer,parameter,private :: SCRATCH_CMP_INDEX = HDR_SCRATCH_61
      integer,parameter,private :: SCRATCH_OFFSET    = HDR_SCRATCH_62
      integer,parameter,private :: SCRATCH_INSERTED  = HDR_SCRATCH_30

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine reg2d_create (obj)

      type(reg2d_struct),pointer :: obj       ! arguments

      lunprint = pc_get_lun()

      allocate (obj)

      call reg2d_initialize (obj)

      end subroutine reg2d_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine reg2d_delete (obj)

      type(reg2d_struct),pointer :: obj       ! arguments

      call reg2d_wrapup (obj)

      deallocate(obj)

      end subroutine reg2d_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine reg2d_initialize (obj)

      type(reg2d_struct),intent(inout) :: obj       ! arguments

      obj%starting_offset  = 0.0
      obj%source_inc       = 100.0
      obj%receiver_inc     = 100.0
      obj%ratio            = 1
      obj%max_offset       = 10000.0
      obj%num_channels     = 101
      obj%num_shots        = 0
      obj%hdr_source       = 0
      obj%hdr_receiver     = 0
      obj%hdr_midpoint     = 0
      obj%hdr_cmp_index    = 0
      obj%hdr_inserted     = 0
      obj%debug_print      = 0
      obj%direction_check  = .true.

      call reg2d_update (obj)

      end subroutine reg2d_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine reg2d_update (obj)

      type(reg2d_struct),intent(inout),target :: obj                ! arguments
      integer                                 :: numtr              ! local
      real                                    :: keep_source_inc    ! local
      integer                                 :: keep_ratio         ! local
      real                                    :: keep_max_offset    ! local
      integer                                 :: keep_num_channels  ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      keep_source_inc   = obj%source_inc
      keep_ratio        = obj%ratio
      keep_max_offset   = obj%max_offset
      keep_num_channels = obj%num_channels

      call pc_get_global ('numtr'             , numtr)
      call pc_get_global ('nwih'              , obj%nwih_input)
      call pc_get_global ('ndpt'              , obj%ndpt)

      call pc_get        ('starting_offset'   , obj%starting_offset    )
      call pc_get        ('source_inc'        , obj%source_inc         )
      call pc_get        ('receiver_inc'      , obj%receiver_inc       )
      call pc_get        ('ratio'             , obj%ratio              )
      call pc_get        ('max_offset'        , obj%max_offset         )
      call pc_get        ('num_channels'      , obj%num_channels       )
      call pc_get        ('num_shots'         , obj%num_shots          )
      call pc_get        ('hdr_source'        , obj%hdr_source         )
      call pc_get        ('hdr_receiver'      , obj%hdr_receiver       )
      call pc_get        ('hdr_midpoint'      , obj%hdr_midpoint       )
      call pc_get        ('hdr_cmp_index'     , obj%hdr_cmp_index      )
      call pc_get        ('hdr_inserted'      , obj%hdr_inserted       )
      call pc_get        ('debug_print'       , obj%debug_print        )
      call pc_get        ('direction_check'   , obj%direction_check    )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (numtr > 1) then
           call pc_error ('REG2D can accept only one trace at a time.')
           call pc_error ('Please insert an UNGATHER process before REG2D.')
      end if

      if (obj%source_inc   <= 0.0) obj%source_inc   = 100.0
      if (obj%receiver_inc <= 0.0) obj%receiver_inc = 100.0
      if (obj%max_offset   <= 0.0) obj%max_offset   = 10000.0
      if (obj%num_channels <=   0) obj%num_channels = 101
      if (obj%ratio        <=   0) obj%ratio        = 1

      if (obj%ratio      == keep_ratio .and. &
          obj%source_inc /= keep_source_inc) then
           obj%ratio = nint(obj%source_inc / obj%receiver_inc)
           obj%ratio = max(obj%ratio,1)
      end if

      if (obj%num_channels == keep_num_channels .and. &
          obj%max_offset   /= keep_max_offset) then
           obj%num_channels = 1 + nint(obj%max_offset / obj%receiver_inc)
      end if

      obj%max_offset   = (obj%num_channels - 1) * obj%receiver_inc
      obj%source_inc   = obj%ratio * obj%receiver_inc
      obj%debug_print  = max(obj%debug_print,0)
      obj%num_shots    = max(obj%num_shots,0)
      obj%nwih_output  = obj%nwih_input

      call mth_constrain &
                (obj%starting_offset, 0.0, obj%max_offset, obj%receiver_inc)

      call reg2d_user_defined (obj, 'HDR_SOURCE'   , obj%hdr_source)
      call reg2d_user_defined (obj, 'HDR_RECEIVER' , obj%hdr_receiver)
      call reg2d_user_defined (obj, 'HDR_MIDPOINT' , obj%hdr_midpoint)
      call reg2d_user_defined (obj, 'HDR_CMP_INDEX', obj%hdr_cmp_index)
      call reg2d_user_defined (obj, 'HDR_INSERTED' , obj%hdr_inserted)


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_global ('nwih'              , obj%nwih_output        )
      call pc_put_global ('reg2d_source_inc'  , obj%source_inc         )
      call pc_put_global ('reg2d_receiver_inc', obj%receiver_inc       )
      call pc_put_global ('reg2d_last_channel', obj%num_channels       )
      call pc_put_global ('reg2d_num_shots'   , obj%num_shots          )

      call pc_put        ('starting_offset'   , obj%starting_offset    )
      call pc_put        ('source_inc'        , obj%source_inc         )
      call pc_put        ('receiver_inc'      , obj%receiver_inc       )
      call pc_put        ('ratio'             , obj%ratio              )
      call pc_put        ('max_offset'        , obj%max_offset         )
      call pc_put        ('num_channels'      , obj%num_channels       )
      call pc_put        ('num_shots'         , obj%num_shots          )
      call pc_put        ('hdr_source'        , obj%hdr_source         )
      call pc_put        ('hdr_receiver'      , obj%hdr_receiver       )
      call pc_put        ('hdr_midpoint'      , obj%hdr_midpoint       )
      call pc_put        ('hdr_cmp_index'     , obj%hdr_cmp_index      )
      call pc_put        ('hdr_inserted'      , obj%hdr_inserted       )
      call pc_put        ('debug_print'       , obj%debug_print        )
      call pc_put        ('direction_check'   , obj%direction_check    )

      call pc_put_gui_only ('nwih_input'      , obj%nwih_input   )
      call pc_put_gui_only ('nwih_output'     , obj%nwih_output  )
      call pc_put_gui_only ('min_offset'      , 0.0              )
      call pc_put_gui_only ('offset_inc'      , obj%receiver_inc )

      call pc_put_control ('need_request', .true.)
      call pc_put_control ('need_label'  , .true.)
      call pc_put_control ('twosets'     , .true.)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      obj%prev_current_line    = 0   ! line    # of prev live trace output.
      obj%prev_current_gather  = 0   ! gather  # of prev live trace output.
      obj%prev_current_channel = 0   ! channel # of prev live trace output.
      obj%prev_original_group  = 0   ! group   # of prev live trace output.
      obj%prev_offset          = 0.0 ! offset    of prev live trace output.
      obj%prev_source_xloc     = 0.0 ! source X  of prev live trace output.
      obj%prev_source_yloc     = 0.0 ! source Y  of prev live trace output.
      obj%prev_receiver_xloc   = 0.0 ! source X  of prev live trace output.
      obj%prev_receiver_yloc   = 0.0 ! source Y  of prev live trace output.
      obj%prev_source_line     = 0   ! line    # of prev live trace output.
      obj%prev_receiver_line   = 0   ! line    # of prev live trace output.

      obj%out_sequence         = 0                 ! last sequence # output.

      obj%out_current_line     = 0                 ! last line     # output.
      obj%out_current_gather   = 0                 ! last gather   # output.
      obj%out_current_channel  = obj%num_channels  ! last channel  # output.
      obj%out_original_group   = 0                 ! last group    # output.
      obj%out_offset           = 0.0               ! last offset     output.
      obj%out_source_xloc      = 0.0               ! last source X   output.
      obj%out_source_yloc      = 0.0               ! last source Y   output.
      obj%out_receiver_xloc    = 0.0               ! last source X   output.
      obj%out_receiver_yloc    = 0.0               ! last source Y   output.
      obj%out_source_line      = 0                 ! last src line # output.
      obj%out_receiver_line    = 0                 ! last rec line # output.

      obj%next_current_line    = 0   ! line    # of next live trace to output.
      obj%next_current_gather  = 0   ! gather  # of next live trace to output.
      obj%next_current_channel = 0   ! channel # of next live trace to output.
      obj%next_original_group  = 0   ! group   # of next live trace to output.
      obj%next_offset          = 0.0 ! offset    of next live trace to output.
      obj%next_source_xloc     = 0.0 ! source X  of next live trace to output.
      obj%next_source_yloc     = 0.0 ! source Y  of next live trace to output.
      obj%next_receiver_xloc   = 0.0 ! source X  of next live trace to output.
      obj%next_receiver_yloc   = 0.0 ! source Y  of next live trace to output.
      obj%next_source_line     = 0   ! line    # of next live trace to output.
      obj%next_receiver_line   = 0   ! line    # of next live trace to output.

      obj%starting_channel = 1 + nint(obj%starting_offset / obj%receiver_inc)

      obj%input_trace_available = .false.   ! whether a live input trace
                                            ! is waiting for output.

      obj%more_input_available  = .true.    ! whether more input traces
                                            ! are waiting to be received.

      call reg2d_bless_init (obj%bless, obj%max_offset, obj%receiver_inc)


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine reg2d_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


!!-------------------------- user defined ----------------------------------!!
!!-------------------------- user defined ----------------------------------!!
!!-------------------------- user defined ----------------------------------!!

           ! nwih_output must be initialized to nwih_input.


      subroutine reg2d_user_defined (obj,name,hdr)

      type(reg2d_struct),intent(inout) :: obj                 ! arguments
      character(len=*)  ,intent(in)    :: name                ! arguments
      integer           ,intent(inout) :: hdr                 ! arguments

      if (hdr == 0) return
      if (hdr >= 48 .and. hdr <= 55) return

      if (hdr >= 65 .and. hdr <= obj%nwih_input + 5) then
           obj%nwih_output = max(obj%nwih_output,hdr)
           return
      end if

      call pc_warning ('BAD',name,'- reset to 0')
      hdr = 0

      end subroutine reg2d_user_defined


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine reg2d (obj,ntr,hd1,tr1,hd2,tr2)

      type(reg2d_struct),intent(inout) :: obj                 ! arguments
      integer           ,intent(inout) :: ntr                 ! arguments
      double precision  ,intent(in)    :: hd1(:,:)            ! arguments
      real              ,intent(in)    :: tr1(:,:)            ! arguments
      double precision  ,intent(out)   :: hd2(:,:)            ! arguments
      real              ,intent(out)   :: tr2(:,:)            ! arguments
      logical                          :: whoops              ! local

!----------receive input information:

      if (ntr == 1) then

           if (obj%direction_check) then
                call reg2d_bless_direction (obj%bless,hd1(:,1),whoops)

                if (whoops) then
                     call pc_error ('REG2D: Shots appear to be in reverse &
                                                &order of shooting.')
                     call reg2d_wrapup (obj)
                     ntr = FATAL_ERROR
                     return
                end if
           end if

           call reg2d_input (obj,hd1(:,1),tr1(:,1))

           ! above sets INPUT_TRACE_AVAILABLE = true if input trace is usable.
           ! above sets INPUT_TRACE_AVAILABLE = false if input trace is no good.

           if (.not.obj%input_trace_available) then
                ntr = NEED_TRACES
                return                                  ! get another trace.
           end if

      else if (ntr == NO_MORE_TRACES) then

           obj%more_input_available = .false.

      else if (ntr == NEED_TRACES) then

           if (obj%more_input_available .and. &
              .not.obj%input_trace_available) return    ! get another trace.

      else

           call pc_error ('REG2D: Illegal input value for NTR =',ntr)
           call reg2d_wrapup (obj)
           ntr = FATAL_ERROR
           return

      end if

!----------generate output information:

      if (obj%input_trace_available                  .or. &
          obj%out_current_channel < obj%num_channels .or. &
          obj%out_current_gather < obj%num_shots) then

           call reg2d_output (obj,hd1(:,1),tr1(:,1),hd2(:,1),tr2(:,1))

           ! above sets INPUT_TRACE_AVAILABLE = false if input trace is output.
           ! above does not set INPUT_TRACE_AVAILABLE if dead trace is output.

           ntr = 1

      else if (obj%more_input_available) then

           ntr = NEED_TRACES

      else

           ntr = NO_MORE_TRACES

      end if

      end subroutine reg2d


!!----------------------------- bless init ------------------------!!
!!----------------------------- bless init ------------------------!!
!!----------------------------- bless init ------------------------!!


      subroutine reg2d_bless_init (bless,max_offset,offset_inc)

      type(reg2d_bless),intent(inout) :: bless             ! arguments
      real             ,intent(in)    :: max_offset        ! arguments
      real             ,intent(in)    :: offset_inc        ! arguments

      bless%source_xloc  (:) = 0
      bless%source_yloc  (:) = 0
      bless%receiver_xloc(:) = 0
      bless%receiver_yloc(:) = 0
      bless%offset       (:) = 0
      bless%reference        = nint(0.6 * max_offset)
      bless%tolerance        = nint(2.0 * offset_inc)
      bless%num              = 0

      end subroutine reg2d_bless_init


!!----------------------------- bless direction ------------------------!!
!!----------------------------- bless direction ------------------------!!
!!----------------------------- bless direction ------------------------!!


      subroutine reg2d_bless_direction (bless,hd1,whoops)

      type(reg2d_bless),intent(inout) :: bless                  ! arguments
      double precision ,intent(in)    :: hd1(:)                 ! arguments
      logical          ,intent(out)   :: whoops                 ! arguments
      integer                         :: offset,indx,bad        ! local
      integer                         :: source_xloc            ! local
      integer                         :: source_yloc            ! local
      integer                         :: receiver_xloc          ! local
      integer                         :: receiver_yloc          ! local
      real                            :: xdist,ydist            ! local
      integer                         :: distance,previous      ! local

!----------return right away if the direction has already been blessed:

      if (bless%num >= MAXBLESS) then
           whoops = .false.
           return
      end if

!----------gather information for the blessing:

      offset        = nint(abs(hd1(HDR_OFFSET        )))
      source_xloc   = nint(    hd1(HDR_SOURCE_XLOC   ))
      source_yloc   = nint(    hd1(HDR_SOURCE_YLOC   ))
      receiver_xloc = nint(    hd1(HDR_RECEIVER_XLOC ))
      receiver_yloc = nint(    hd1(HDR_RECEIVER_YLOC ))

!----------return if the offset is not within tolerance:

      if (abs(offset - bless%reference) > bless%tolerance) then
           whoops = .false.
           return
      end if

!----------save information for the blessing:

      if (bless%num == 0) then
           bless%num                      = bless%num + 1
           bless%offset       (bless%num) = offset    
           bless%source_xloc  (bless%num) = source_xloc
           bless%source_yloc  (bless%num) = source_yloc 
           bless%receiver_xloc(bless%num) = receiver_xloc
           bless%receiver_yloc(bless%num) = receiver_yloc
      else if (source_xloc /= bless%source_xloc(bless%num) .or. &
               source_yloc /= bless%source_yloc(bless%num)) then
           bless%num                      = bless%num + 1
           bless%offset       (bless%num) = offset    
           bless%source_xloc  (bless%num) = source_xloc
           bless%source_yloc  (bless%num) = source_yloc 
           bless%receiver_xloc(bless%num) = receiver_xloc
           bless%receiver_yloc(bless%num) = receiver_yloc
      else if (abs(offset - bless%reference) <  &
               abs(offset - bless%offset(bless%num))) then
           bless%offset       (bless%num) = offset    
           bless%receiver_xloc(bless%num) = receiver_xloc
           bless%receiver_yloc(bless%num) = receiver_yloc
      end if

!----------return if we still need more information:

      if (bless%num < MAXBLESS) then
           whoops = .false.
           return
      end if

!----------examine information for the blessing:

      call pc_print (' ')
      call pc_print ('REG2D: Checking the direction of the input data:')
      bad = 0
      do indx = 1,MAXBLESS
           xdist = bless%receiver_xloc(indx) - bless%source_xloc(indx)
           ydist = bless%receiver_yloc(indx) - bless%source_yloc(indx)
           distance = nint(sqrt(xdist**2 + ydist**2))
           if (indx > 1) then
                xdist = bless%receiver_xloc(indx) - bless%source_xloc(indx-1)
                ydist = bless%receiver_yloc(indx) - bless%source_yloc(indx-1)
                previous = nint(sqrt(xdist**2 + ydist**2))
           end if
           if (indx == 1) then
                write(lunprint,*) 'REG2D: ',indx,              &
                         '  offset = ',bless%offset(indx),     &
                         '  distance to source = ',distance
           else if (previous >= distance) then
                bad = bad + 1
                write(lunprint,*) 'REG2D: ',indx,              &
                         '  offset = ',bless%offset(indx),     &
                         '  distance to source = ',distance,   &
                         '  distance to previous source = ',previous,'  bad'
           else
                write(lunprint,*) 'REG2D: ',indx,              &
                         '  offset = ',bless%offset(indx),     &
                         '  distance to source = ',distance,   &
                         '  distance to previous source = ',previous
           end if
      end do

!----------now decide whether to bless the direction:

      call pc_print ('REG2D: Number of failed comparisons =',bad, &
                                 'out of',MAXBLESS-1)
      if (bad <= 1) then
           call pc_print ('REG2D: The direction of the data &
                            &appears to be correct.')
           whoops = .false.
      else if (bad >= MAXBLESS-1) then
           call pc_print ('REG2D: THE DIRECTION OF THE DATA &
                            &APPEARS TO BE BACKWARD.')
           whoops = .true.
      else
           call pc_error ('REG2D: THE DIRECTION OF THE DATA &
                            &IS UNCERTAIN AND MIGHT BE BACKWARD.')
           whoops = .true.
      end if
      call pc_print (' ')

      end subroutine reg2d_bless_direction


!!----------------------------- input -------------------------------------!!
!!----------------------------- input -------------------------------------!!
!!----------------------------- input -------------------------------------!!


      subroutine reg2d_input (obj,hd1,tr1)

      type(reg2d_struct),intent(inout) :: obj                ! arguments
      double precision  ,intent(in)    :: hd1(:)             ! arguments
      real              ,intent(in)    :: tr1(:)             ! arguments
      real                             :: xsdiff,ysdiff      ! local
      real                             :: xrdiff,yrdiff      ! local
      integer                          :: source_moveup      ! local
      integer                          :: offset_moveup      ! local
      integer                          :: offset_bin         ! local

!----------save previous input trace information:

      obj%prev_current_line    = obj%next_current_line  
      obj%prev_current_gather  = obj%next_current_gather
      obj%prev_current_channel = obj%next_current_channel
      obj%prev_offset          = obj%next_offset
      obj%prev_original_group  = obj%next_original_group
      obj%prev_source_xloc     = obj%next_source_xloc
      obj%prev_source_yloc     = obj%next_source_yloc
      obj%prev_receiver_xloc   = obj%next_receiver_xloc
      obj%prev_receiver_yloc   = obj%next_receiver_yloc
      obj%prev_source_line     = obj%next_source_line
      obj%prev_receiver_line   = obj%next_receiver_line

!----------get information from new trace headers:

      obj%input_trace_available = .true.

      obj%next_offset          =  abs(hd1(HDR_OFFSET        ))
      obj%next_original_group  = nint(hd1(HDR_ORIGINAL_GROUP))
      obj%next_source_xloc     =      hd1(HDR_SOURCE_XLOC   )
      obj%next_source_yloc     =      hd1(HDR_SOURCE_YLOC   )
      obj%next_receiver_xloc   =      hd1(HDR_RECEIVER_XLOC )
      obj%next_receiver_yloc   =      hd1(HDR_RECEIVER_YLOC )
      obj%next_source_line     = nint(hd1(HDR_SOURCE_LINE   ))
      obj%next_receiver_line   = nint(hd1(HDR_RECEIVER_LINE ))

!----------determine the gather and channel number for this trace:

      xsdiff = obj%next_source_xloc - obj%out_source_xloc
      ysdiff = obj%next_source_yloc - obj%out_source_yloc

      xrdiff = obj%next_receiver_xloc - obj%out_receiver_xloc
      yrdiff = obj%next_receiver_yloc - obj%out_receiver_yloc

      source_moveup = nint(sqrt(xsdiff**2 + ysdiff**2) / obj%source_inc)
      offset_moveup = nint(sqrt(xrdiff**2 + yrdiff**2) / obj%receiver_inc)
      offset_bin    = nint(obj%next_offset / obj%receiver_inc) + 1

 !! The following causes traces to be skipped for smaller offsets when
 !! the source-to-nearest-receiver direction is very different from the
 !! receiver cable direction, since in such cases the offset moveup is
 !! small:
 !!
 !!   offset_moveup = nint((obj%next_offset - obj%out_offset) &
 !!                                        / obj%receiver_inc)

 !! The following probably means the start of a duplicate shot:

      if (obj%next_offset <= obj%out_offset) offset_moveup = 0.0

   !----------choose values if this is the first trace ever:

      if (obj%next_current_gather == 0) then

           obj%next_current_line    = 1
           obj%next_current_gather  = 1
           obj%next_current_channel = offset_bin

   !----------choose values if this is the start of a new line:

      else if (obj%next_source_line   /= obj%out_source_line .or. &
               obj%next_receiver_line /= obj%out_receiver_line) then

           obj%next_current_line    = obj%out_current_line + 1
           obj%next_current_gather  = 1
           obj%next_current_channel = offset_bin

   !----------choose values if this is the start of a new shot profile:

      else if (obj%next_original_group /= obj%out_original_group      .or. &
        .not.mth_ameq(obj%next_source_xloc, obj%out_source_xloc, 0.1) .or. &
        .not.mth_ameq(obj%next_source_yloc, obj%out_source_yloc, 0.1) ) then

           obj%next_current_gather  = obj%out_current_gather + source_moveup
           obj%next_current_channel = offset_bin

           if (obj%next_current_gather <= obj%out_current_gather) then
                obj%input_trace_available = .false.
           end if

   !----------choose values if this is part of the previous shot profile:

      else

           obj%next_current_gather  = obj%out_current_gather
           obj%next_current_channel = obj%out_current_channel + offset_moveup

           if (obj%next_current_channel <= obj%out_current_channel) then
                obj%input_trace_available = .false.
           end if

      end if

!----------check whether input trace is usable:

      if (obj%next_current_channel > obj%num_channels) then
           obj%input_trace_available = .false.
      end if

      if (obj%num_shots > 0) then
           if (obj%next_current_gather > obj%num_shots) then
                obj%input_trace_available = .false.
           end if
      end if

!----------revert to previous input trace info if input trace is no good:

      if (.not.obj%input_trace_available) then
           obj%next_current_line    = obj%prev_current_line  
           obj%next_current_gather  = obj%prev_current_gather
           obj%next_current_channel = obj%prev_current_channel
           obj%next_offset          = obj%prev_offset
           obj%next_original_group  = obj%prev_original_group
           obj%next_source_xloc     = obj%prev_source_xloc
           obj%next_source_yloc     = obj%prev_source_yloc
           obj%next_receiver_xloc   = obj%prev_receiver_xloc
           obj%next_receiver_yloc   = obj%prev_receiver_yloc
           obj%next_source_line     = obj%prev_source_line
           obj%next_receiver_line   = obj%prev_receiver_line
           call reg2d_debug1 (obj,hd1)
      end if

      end subroutine reg2d_input


!!----------------------------- output -------------------------------------!!
!!----------------------------- output -------------------------------------!!
!!----------------------------- output -------------------------------------!!


      subroutine reg2d_output (obj,hd1,tr1,hd2,tr2)

      type(reg2d_struct),intent(inout) :: obj                  ! arguments
      double precision  ,intent(in)    :: hd1(:)               ! arguments
      real              ,intent(in)    :: tr1(:)               ! arguments
      double precision  ,intent(out)   :: hd2(:)               ! arguments
      real              ,intent(out)   :: tr2(:)               ! arguments
      real                             :: xs,ys,xr,yr          ! local
      real                             :: reg_source           ! local
      real                             :: reg_receiver         ! local
      real                             :: reg_midpoint         ! local
      real                             :: reg_offset           ! local
      integer                          :: reg_cmp_index        ! local
      integer                          :: inserted,group       ! local
      integer                          :: keep(5)              ! local

!----------increment the counters:

      obj%out_sequence = obj%out_sequence + 1

      if (obj%out_sequence == 1) then

           obj%out_current_line    = 1
           obj%out_current_gather  = 1
           obj%out_current_channel = 1

           if (obj%input_trace_available) then
                obj%out_original_group  = obj%next_original_group
                obj%out_source_line     = obj%next_source_line
                obj%out_receiver_line   = obj%next_receiver_line
           else
                obj%out_original_group  = 0
                obj%out_source_line     = 0
                obj%out_receiver_line   = 0
           end if

      else if (obj%out_current_channel < obj%num_channels) then

           obj%out_current_channel = obj%out_current_channel + 1

      else if (obj%num_shots == 0) then

           obj%out_current_gather  = obj%out_current_gather + 1
           obj%out_current_channel = 1

           if (.not. obj%input_trace_available) then
                obj%out_original_group  = 0
           else if (obj%next_current_line == obj%out_current_line) then
                obj%out_original_group  = obj%next_original_group
           else
                obj%out_current_gather  = 1
                obj%out_current_line    = obj%next_current_line  
                obj%out_original_group  = obj%next_original_group
                obj%out_source_line     = obj%next_source_line
                obj%out_receiver_line   = obj%next_receiver_line
           end if

      else if (obj%out_current_gather < obj%num_shots) then

           obj%out_current_gather  = obj%out_current_gather + 1
           obj%out_current_channel = 1

           if (obj%input_trace_available) then
                obj%out_original_group  = obj%next_original_group
           else
                obj%out_original_group  = 0
           end if

      else

           obj%out_current_line    = obj%out_current_line + 1
           obj%out_current_gather  = 1
           obj%out_current_channel = 1

           if (obj%input_trace_available) then
                obj%out_original_group  = obj%next_original_group
                obj%out_source_line     = obj%next_source_line
                obj%out_receiver_line   = obj%next_receiver_line
           else
                obj%out_original_group  = 0
           end if

      end if

!----------adjust counters if not filling in near offsets:

      if (obj%out_current_channel == 1) then
           if (obj%next_current_gather == obj%out_current_gather) then
                obj%out_current_channel = &
                       min(obj%starting_channel,obj%next_current_channel)
           else
                obj%out_current_channel = obj%starting_channel
           end if
      end if

!----------calculate regularized output coordinates:

      reg_offset    = (obj%out_current_channel - 1) * obj%receiver_inc

      reg_source    = (obj%num_channels        - 1) * obj%receiver_inc + &
                      (obj%out_current_gather  - 1) * obj%source_inc

      reg_receiver  = reg_source - reg_offset

      reg_midpoint  = 0.5 * (reg_source + reg_receiver)

      reg_cmp_index = 2 - obj%num_channels + &
                            nint(2.0 * reg_midpoint / obj%receiver_inc)

!----------set output values for input trace:

      if (obj%input_trace_available                           .and. &
          obj%out_current_gather  == obj%next_current_gather  .and. &
          obj%out_current_channel == obj%next_current_channel .and. &
          obj%out_source_line     == obj%next_source_line     .and. &
          obj%out_receiver_line   == obj%next_receiver_line) then

           hd2(1:obj%nwih_input) = hd1(1:obj%nwih_input)
           tr2(1:obj%ndpt)       = tr1(1:obj%ndpt)
           inserted              = 0

           obj%input_trace_available = .false.

           obj%out_offset         = obj%next_offset
           obj%out_source_xloc    = obj%next_source_xloc
           obj%out_source_yloc    = obj%next_source_yloc
           obj%out_receiver_xloc  = obj%next_receiver_xloc
           obj%out_receiver_yloc  = obj%next_receiver_yloc

!----------set output values for new dead trace:

      else

           hd2(1:obj%nwih_input) = 0.0
           tr2(1:obj%ndpt)       = 0.0
           inserted              = -97

           call reg2d_interpolate (obj,xs,ys,xr,yr)

           obj%out_offset        = sqrt((xs - xr)**2 + (ys - yr)**2)
           obj%out_source_xloc   = xs
           obj%out_source_yloc   = ys
           obj%out_receiver_xloc = xr
           obj%out_receiver_yloc = yr

           hd2(HDR_TOP_MUTE       ) = 1
           hd2(HDR_FOLD           ) = 1
           hd2(HDR_BOTTOM_MUTE    ) = obj%ndpt
           hd2(HDR_OFFSET         ) = obj%out_offset
           hd2(HDR_SOURCE_XLOC    ) = obj%out_source_xloc
           hd2(HDR_SOURCE_YLOC    ) = obj%out_source_yloc
           hd2(HDR_RECEIVER_XLOC  ) = obj%out_receiver_xloc
           hd2(HDR_RECEIVER_YLOC  ) = obj%out_receiver_yloc
           hd2(HDR_ORIGINAL_GROUP ) = obj%out_original_group

      end if

!----------set output values for all traces:

      if (obj%nwih_output > obj%nwih_input) then
           hd2(obj%nwih_input+1:obj%nwih_output) = 0.0
      end if

      group = 1 + (obj%out_current_gather - 1) * obj%ratio

      keep(1) = nint(hd2(HDR_SEQUENCE       ))
      keep(2) = nint(hd2(HDR_CURRENT_GROUP  ))
      keep(3) = nint(hd2(HDR_CURRENT_CHANNEL))
      keep(4) = nint(hd2(HDR_SOURCE_LINE    ))
      keep(5) = nint(hd2(HDR_RECEIVER_LINE  ))

      hd2(HDR_SEQUENCE       ) = obj%out_sequence
      hd2(HDR_CURRENT_GROUP  ) = group
      hd2(HDR_CURRENT_CHANNEL) = obj%out_current_channel
      hd2(HDR_SOURCE_LINE    ) = obj%out_current_line
      hd2(HDR_RECEIVER_LINE  ) = obj%out_current_line

      hd2(SCRATCH_SOURCE   ) = reg_source
      hd2(SCRATCH_RECEIVER ) = reg_receiver
      hd2(SCRATCH_MIDPOINT ) = reg_midpoint
      hd2(SCRATCH_CMP_INDEX) = reg_cmp_index
      hd2(SCRATCH_OFFSET   ) = reg_offset
      hd2(SCRATCH_INSERTED ) = inserted

      if (obj%hdr_source    > 0) hd2(obj%hdr_source   ) = reg_source
      if (obj%hdr_receiver  > 0) hd2(obj%hdr_receiver ) = reg_receiver
      if (obj%hdr_midpoint  > 0) hd2(obj%hdr_midpoint ) = reg_midpoint
      if (obj%hdr_cmp_index > 0) hd2(obj%hdr_cmp_index) = reg_cmp_index
      if (obj%hdr_inserted  > 0) hd2(obj%hdr_inserted ) = inserted

      call reg2d_debug2 (obj,hd2,keep)

      end subroutine reg2d_output


!!----------------------------- interpolate ---------------------------------!!
!!----------------------------- interpolate ---------------------------------!!
!!----------------------------- interpolate ---------------------------------!!


      subroutine reg2d_interpolate (obj,xs,ys,xr,yr)

      type(reg2d_struct),intent(in)  :: obj                         ! arguments
      real              ,intent(out) :: xs,ys,xr,yr                 ! arguments
      real                           :: outs,prevs,nexts            ! local
      real                           :: outr,prevr,nextr            ! local
      real                           :: prevxs,nextxs,prevys,nextys ! local
      real                           :: prevxr,nextxr,prevyr,nextyr ! local

      outs   = obj%out_current_gather
      prevs  = obj%prev_current_gather
      nexts  = obj%next_current_gather
      prevxs = obj%prev_source_xloc
      nextxs = obj%next_source_xloc
      prevys = obj%prev_source_yloc
      nextys = obj%next_source_yloc

      xs = terputil_root (outs, prevs, nexts, prevxs, nextxs)
      ys = terputil_root (outs, prevs, nexts, prevys, nextys)

      if (obj%out_current_channel == 1) then

           xr = xs
           yr = ys
           return

      else if (outs == prevs .and. outs == nexts) then

           outr   = obj%out_current_channel
           prevr  = obj%prev_current_channel
           nextr  = obj%next_current_channel
           prevxr = obj%prev_receiver_xloc
           nextxr = obj%next_receiver_xloc
           prevyr = obj%prev_receiver_yloc
           nextyr = obj%next_receiver_yloc

      else if (outs == nexts) then

           outr   = obj%out_current_channel
           prevr  = 1.0
           nextr  = obj%next_current_channel
           prevxr = xs
           nextxr = obj%next_receiver_xloc
           prevyr = ys
           nextyr = obj%next_receiver_yloc

      else if (outs == prevs) then

           outr   = obj%out_current_channel
           prevr  = obj%prev_current_channel
           nextr  = obj%out_current_channel
           prevxr = obj%prev_receiver_xloc
           nextxr = obj%prev_receiver_xloc - (nextr - prevr) * obj%receiver_inc
           prevyr = obj%prev_receiver_yloc
           nextyr = obj%prev_receiver_yloc

      else

           outr   = obj%out_current_channel
           prevr  = 1.0
           nextr  = obj%out_current_channel
           prevxr = xs
           nextxr = xs - (nextr - prevr) * obj%receiver_inc
           prevyr = ys
           nextyr = ys

      end if

      xr = terputil_root (outr, prevr, nextr, prevxr, nextxr)
      yr = terputil_root (outr, prevr, nextr, prevyr, nextyr)

      end subroutine reg2d_interpolate


!!------------------------------ debug0 -----------------------------------!!
!!------------------------------ debug0 -----------------------------------!!
!!------------------------------ debug0 -----------------------------------!!


      subroutine reg2d_debug0 (flag)

      integer,intent(in) :: flag                          ! arguments

                     write (lunprint,3000)
      if (flag == 1) write (lunprint,4000)
                     write (lunprint,6000)

      write (lunprint,3000) 1,3,4,26,27,          &
                            HDR_SEQUENCE       ,  &
                            HDR_CURRENT_GROUP  ,  &
                            HDR_CURRENT_CHANNEL,  &
                            HDR_OFFSET         ,  &
                            HDR_ORIGINAL_GROUP ,  &
                            HDR_SOURCE_XLOC    ,  &
                            HDR_SOURCE_YLOC    ,  &
                            HDR_RECEIVER_XLOC  ,  &
                            HDR_RECEIVER_YLOC  ,  &
                            HDR_SOURCE_LINE    ,  &
                            HDR_RECEIVER_LINE  ,  &
                            '     SRC'         ,  &
                            '     REC'         ,  &
                            '     CMP'         ,  &
                               '  CMP'         ,  &
                            '  OFFSET'

      if (flag == 3) write (lunprint,5000)
                     write (lunprint,3000)

      3000 format (1x,i8,i6,i5,2i5,1x,i8,i6,i5,i6,i5,4i8,2i5,3a8,a5,a8)
      4000 format (36x, &
   '-------------------START OF REG2D DEBUG PRINTOUT--------------------')
      5000 format (36x, &
   '--------------------END OF REG2D DEBUG PRINTOUT---------------------')
      6000 format (6x,'---INPUT TRACE HEADERS---',5x, &
   '-----------------------OUTPUT TRACE HEADERS-------------------------', &
                   4x,'------USER DEFINED HEADERS------')

      end subroutine reg2d_debug0


!!------------------------------ debug1 -----------------------------------!!
!!------------------------------ debug1 -----------------------------------!!
!!------------------------------ debug1 -----------------------------------!!


      subroutine reg2d_debug1 (obj,hd1)

      type(reg2d_struct),intent(inout) :: obj                ! arguments
      double precision  ,intent(in)    :: hd1(:)             ! arguments
      integer                          :: sequence           ! local

      obj%debug_print = obj%debug_print - 1

      if (obj%debug_print < 0) return

      sequence = nint(hd1(HDR_SEQUENCE))

      if (sequence == 1) call reg2d_debug0 (1)

      write (lunprint,1000) nint(hd1(HDR_SEQUENCE       )),  &
                            nint(hd1(HDR_CURRENT_GROUP  )),  &
                            nint(hd1(HDR_CURRENT_CHANNEL)),  &
                            nint(hd1(HDR_SOURCE_LINE    )),  &
                            nint(hd1(HDR_RECEIVER_LINE  )),  &
                            nint(hd1(HDR_OFFSET         )),  &
                            nint(hd1(HDR_ORIGINAL_GROUP )),  &
                            nint(hd1(HDR_SOURCE_XLOC    )),  &
                            nint(hd1(HDR_SOURCE_YLOC    )),  &
                            nint(hd1(HDR_RECEIVER_XLOC  )),  &
                            nint(hd1(HDR_RECEIVER_YLOC  ))

      1000 format (2x,i8,i6,i5,2i5,1x,8x,6x,5x,i6,i5,4i8,10x,35x,1x,'bypassed')

      if (obj%debug_print == 0) call reg2d_debug0 (3)

      end subroutine reg2d_debug1


!!------------------------------ debug2 ------------------------------------!!
!!------------------------------ debug2 ------------------------------------!!
!!------------------------------ debug2 ------------------------------------!!


      subroutine reg2d_debug2 (obj,hd2,keep)

      type(reg2d_struct),intent(inout) :: obj                ! arguments
      double precision  ,intent(in)    :: hd2(:)             ! arguments
      integer           ,intent(in)    :: keep(:)            ! arguments
      integer                          :: sequence           ! local
      integer                          :: channel            ! local
      integer                          :: inserted           ! local

      obj%debug_print = obj%debug_print - 1

      if (obj%debug_print < 0) return

      sequence = nint(hd2(HDR_SEQUENCE))
      channel  = nint(hd2(HDR_CURRENT_CHANNEL))
      inserted = nint(hd2(SCRATCH_INSERTED))

      if (sequence == 1) call reg2d_debug0 (1)

      if (inserted == 0) then
           write (lunprint,2000) keep(:),                         &
                                 nint(hd2(HDR_SEQUENCE       )),  &
                                 nint(hd2(HDR_CURRENT_GROUP  )),  &
                                 nint(hd2(HDR_CURRENT_CHANNEL)),  &
                                 nint(hd2(HDR_OFFSET         )),  &
                                 nint(hd2(HDR_ORIGINAL_GROUP )),  &
                                 nint(hd2(HDR_SOURCE_XLOC    )),  &
                                 nint(hd2(HDR_SOURCE_YLOC    )),  &
                                 nint(hd2(HDR_RECEIVER_XLOC  )),  &
                                 nint(hd2(HDR_RECEIVER_YLOC  )),  &
                                 nint(hd2(HDR_SOURCE_LINE    )),  &
                                 nint(hd2(HDR_RECEIVER_LINE  )),  &
                                 nint(hd2(SCRATCH_SOURCE     )),  &
                                 nint(hd2(SCRATCH_RECEIVER   )),  &
                                 nint(hd2(SCRATCH_MIDPOINT   )),  &
                                 nint(hd2(SCRATCH_CMP_INDEX  )),  &
                                 nint(hd2(SCRATCH_OFFSET     ))
      else
           write (lunprint,2001) nint(hd2(HDR_SEQUENCE       )),  &
                                 nint(hd2(HDR_CURRENT_GROUP  )),  &
                                 nint(hd2(HDR_CURRENT_CHANNEL)),  &
                                 nint(hd2(HDR_OFFSET         )),  &
                                 nint(hd2(HDR_ORIGINAL_GROUP )),  &
                                 nint(hd2(HDR_SOURCE_XLOC    )),  &
                                 nint(hd2(HDR_SOURCE_YLOC    )),  &
                                 nint(hd2(HDR_RECEIVER_XLOC  )),  &
                                 nint(hd2(HDR_RECEIVER_YLOC  )),  &
                                 nint(hd2(HDR_SOURCE_LINE    )),  &
                                 nint(hd2(HDR_RECEIVER_LINE  )),  &
                                 nint(hd2(SCRATCH_SOURCE     )),  &
                                 nint(hd2(SCRATCH_RECEIVER   )),  &
                                 nint(hd2(SCRATCH_MIDPOINT   )),  &
                                 nint(hd2(SCRATCH_CMP_INDEX  )),  &
                                 nint(hd2(SCRATCH_OFFSET     ))
      end if

   2000 format (1x,i8,i6,i5,2i5,1x,i8,i6,i5,i6,i5,4i8,2i5,3i8,i5,i6,2x,'kept')
   2001 format (1x,8x,6x,5x,10x,1x,i8,i6,i5,i6,i5,4i8,2i5,3i8,i5,i6,2x,'new')

      if (obj%debug_print == 0) then
           call reg2d_debug0 (3)
      else if (mod(channel,50) == 0 .or. channel == obj%num_channels) then
           call reg2d_debug0 (2)
      end if

      end subroutine reg2d_debug2


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine reg2d_wrapup (obj)

      type(reg2d_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine reg2d_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module reg2d_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

