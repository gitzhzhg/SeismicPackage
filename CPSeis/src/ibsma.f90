!<CPS_v1 type="PROCESS"/>
!!------------------------------- ibsma.f90 ---------------------------------!!
!!------------------------------- ibsma.f90 ---------------------------------!!
!!------------------------------- ibsma.f90 ---------------------------------!!

        ! other files are:  ibsma_crou.c  ibsma_crou.h

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
! Name       : IBSMA
! Category   : miscellaneous
! Written    : 2003-08-14   by: B. Lucas
! Revised    : 2007-11-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Multiple prediction.
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Two dimensional wave equation surface multiple prediction program. The current
! version of this method works only for 2D, prestack, shot-ordered, marine-
! streamer lines. 3D marine towed streamer data can be processed one sail line 
! with multiple cables at once. This method predicts all surface related 
! multiples, which include multiples whose downward reflections occurs at the 
! water surface. The prediction does not depend on moveout information. Nor does
! it require any knowledge of either subsurface velocities or the nature and
! positions of the water bottom and subsurface reflectors. Because it is a 2D 
! algorithm, it cannot not predict multiples precisely in terms of amplitude, 
! phase, and time due to 3D effect and acquisition geometry related issues.
! Typically it predicts reasonably well the kinematics of near and middle 
! offset multiples, but yields large time error for far-offset.
!
! This method works reasonably well in deep water environment. Its performance
! degrades considerably for a water depth shallower than 250m.
!
! Most of the time, this method followed by RADON method yields best results
! of multiple attenuation.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! When users want to use this algorithm to predict surface related multiples, 
! they agree that their input data meets the following requirements.
!
! 1. IBSMA only accepts data organized in common-shot domain. One input file
!    per job can contain data along one 3D sail line with multiple cables. Input
!    data order (fastest first) should be: time, receiver, shot, cable. For one
!    source/cable combination, input shot gathers should follow "FIELD 
!    ACQUISITION ORDER", i.e., the first shot acquired in the field comes in
!    "FIRST" and the last one comes in "LAST". Within a shot gather, traces
!    are ordered either from near to far offset or from far to near offset.
!
! 2. Source and receiver spacing should be same. "fxti" in CPS can be used to 
!    interploate shot/receiver. It includes three steps: (1) sort common-shot 
!    gather into common-offset gather; (2) apply "fxti" on each common-offset 
!    gather; (3) sort common-offset gather back to common-shot gather.
!
! 3. Missing near offset traces should be extrapolated. "cnearts" in CPS can be 
!    used to extrapolate near offset traces to traces all back to zero offset 
!    or to the offset closest to zero offset.
!
! 4. A mute zone should be applied just above the water bottom reflection and
!    refractions, i.e., direct waves should be removed.
!
! 5. Deterministic signature decon should be applied to data prior to IBSMA, so 
!    that the wavelet in the data is as close as possible to a band-limited,
!    zero-phase spike. However, statistical decon should not be used.
!
! 6. Both source-side and receiver-side ghosts should be removed. Spatial 
!    filtering effects of source and receiver arrays should also be removed 
!    from the data. In practice, IBSMA still works when both ghost and array
!    effect are left in data, but the result may not be optimal.
!
! 7. Shot/receiver trace balancing should be applied. However, don't apply any 
!    time-varying gain to compensate spherical divergence.
!
! 8. IBSMA accept irregular input with missing shots or receivers. It relies on
!    trace header source/receiver index to detect missing traces, and replacing
!    them with zero traces. However, if the percentage of total zero traces is 
!    more than 5%, IBSMA becomes less effective.
!
! When a 3D input contains multiple cables (the number of cables is NCABLE)
! along one sail line, a parallel job should be submitted in order to achieve
! short turnover time. To optimally use computer resource, the number of PEs
! should be (NCABLE+1), though user can get the same result by using a different
! number of PEs.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! header word 27 must be set to the receiver line number
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! --> Insert how this process affects output traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! --> Insert globals that this process uses or changes:
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       --> specify action taken.
! GATHERED  whether traces are a legitimate gather  --> specify action taken.
! NWIH      number of words in trace header         --> specify action taken.
! NDPT      number of sample values in trace        --> specify action taken.
! TSTRT     starting time on trace                  --> specify action taken.
! DT        trace sample interval                   --> specify action taken.
! GRID      grid transformation structure           --> specify action taken.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! --> Insert header words used or changed by this process:
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 27      HDR_RECEIVER_LINE          Used to test for line breaks
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 18. 2007-11-29  Stoeckley  Removed unused reference to memman.
! 17. 2007-11-15  B. Menger  Fixed yet another bug and removed print statements.
! 16. 2007-11-13  B. Menger  Fixed bug, removed print statements.
! 15. 2007-11-13  B. Menger  Added call to ibsma_write_original and added mute
!                            panel to gui and mute to all sections.  This allows
!                            the user to create a mute as part of the workflow
!                            for IBSMA but not apply the mute to the original 
!                            (saved) data, only to the data going into the SMA
!                            routines.
! 14. 2006-06-20  B. Menger  Removed Unused Variables.
! 13. 2005-05-17  B. Lucas   Revised version from Shen's custom.
! 11. 2004-08-17  Y. Shen    add explanation for input data order and MLTP_SCALE
! 10. 2004-08-05  Y. Shen    add mltp_scale and change overall amplitude scaling
!  9. 2004-05-04  B. Lucas   Improved parameter checking/defaults.
!  8. 2004-04-16  B. Lucas   Fixed groupsize bug introduced with the
!                            redesign of the dataprep stage.
!  7. 2004-02-25  B. Lucas   Fixed single-cable PCPS_LINE_BREAK bug.
!  6. 2004-02-17  B. Lucas   Added memory size estimation to GUI.
!                            Redesign of dataprep I/O to temp file.
!                            Fixed parallelization problem with missing traces.
!  5. 2003-12-18  B. Lucas   Fixed groupsize bug in output mode.
!  4. 2003-12-12  Y. Shen    Added general decsription and advice for users
!  3. 2003-11-20  B. Lucas   Added MEMORY_SIZE and fixed scaling problem.
!  2. 2003-11-10  B. Lucas   Revised version.
!  1. 2003-08-14  B. Lucas   Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.  --> Change to add any platform dependencies.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.  --> Change if any special compiler/linking required.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
! --> Default values are shown below - edit as needed:
!
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE  true      whether this process can be in a parallelized loop.
!
! --> Edit the following lines as needed:
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
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
!  None provided.  --> Change if this statement is inappropriate.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! --> Insert description of algorithms used.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! --> Insert any useful programming notes here.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!
!<NS Parameters>
!                            SMA Multiple Prediction
!
!   `-Parameters-------------------------------------------------------------------  
!   | WORK_DIR =~~~~~~`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   | DTYPE =~~~~~~~~~`III
!   | WMIN =~~~~~~~~~~`IIIIIIII        WMAX =~~~~~~~~~~`IIIIIIII
!   | NSHOTTOTAL =~~~~`IIIIIIII        NRECVMAX =~~~~~~`IIIIIIII
!   | DSHOT =~~~~~~~~~`FFFFFFFF        DRECV =~~~~~~~~~`FFFFFFFF
!   | FMAX = ~~~~~~~~~`FFFFFFFF        MAXORDER =~~~~~~`II
!   | VWATER =~~~~~~~~`FFFFFFFF        MLTP_SCALE =~~~~`FFFFFFFF
!   | MEMORY_SIZE =~~~`CCC             MEMORY_EST = ~~~`SSSSSSSSSSSSSSSSSSSSS
!   | NRECV_HDLOC =~~~`II              OFFSET_REV = ~~~`CC
!   | ICABLE_HDLOC =~~`II     ICABLE_FIRST =~~`IIIII     ICABLE_INCR =~~`IIIII
!   | ISHOT_HDLOC =~~~`II     ISHOT_FIRST =~~~`FFFFF     ISHOT_INCR =~~~`FFFFF
!   | IRECV_HDLOC =~~~`II     IRECV_FIRST =~~~`FFFFF     IRECV_INCR =~~~`FFFFF
!   | Perform removeable Mute Before prediction? [OPT_MUTE_SMA]`CC
!   `----------------------------------------------------------------------------  
!<include mute.f90>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
! tabgroup = SMA
!
!<Help KEYWORD="DRECV">
!<Tip> Receiver spacing.</Tip>
! Default = 0.0
! Allowed = Any "REAL/FLOAT" value (greater than zero)
! Receiver spacing (should be same as DSHOT).
!</Help>
!
!<Help KEYWORD="DSHOT">
!<Tip> Shot spacing.</Tip>
! Default = 0.0
! Allowed = Any "REAL/FLOAT" value (greater than zero)
! Shot spacing (should be same as DRECV).
!</Help>
!
!<Help KEYWORD="DTYPE">
!<Tip> Data type. Use "3" for field data.</Tip>
! Default = 3
! Allowed = 2, 3 or 4
! 2=2d synthetic, 3=3d synthetic/field, 4=3d shallow synthetic/field.
!</Help>
!
!<Help KEYWORD="FMAX">
!<Tip> Stop frequency (Hz) for low-pass filter.</Tip>
! Default = 60
! Allowed = Must be greater than or equal to 25.
! A low-pass filter with this stop frequency will be applied on input data. 
! In Spectrum display, the amplitude at this frequency is zero.
!</Help>
!
!<Help KEYWORD="MAXORDER">
!<Tip> Maximum order of predicted multiples with correct amplitude.</Tip>
! Default = 1
! Allowed = Any "integer" value (greater than zero)
! Maximum order of predicted multiples with correct amplitude. Please use
! this default value.
!</Help>
!
!<Help KEYWORD="NRECVMAX">
!<Tip> Maximum number of receivers for any shot gather.</Tip>
! Default = 0
! Allowed = Any integer value (greater than zero)
! Maximum number of receivers for any shot gather on all cable lines
! processed by one job.
!</Help>
!
!<Help KEYWORD="NSHOTTOTAL">
!<Tip> Total number of shots per cable line.</Tip>
! Default = 0
! Allowed = Any integer value (greater than zero)
! Total number of shots per cable line. All cables processed by one
! job should have the same number of shots.
!</Help>
!
!<Help KEYWORD="WMAX">
!<Tip> Maximum frequency of which multiple is predicted.</Tip>
! Default = 0.0
! Allowed = Any value (greater than zero)
! SMA works in frequency domain. "WMAX" specifies the maximum frequency 
! of which multiple is predicted. "WMAX" should not be bigger than "FMAX"
!</Help>
!
!<Help KEYWORD="WMIN">
!<Tip> Minimum frequency of which multiple is predicted.</Tip>
! Default = 0.0
! Allowed = Any value (greater than zero)
! SMA works in frequency domain. "WMIN" specifies the minimum frequency 
! of which multiple is predicted. Reasonable range of "WMIN" is [5.0,15.0]
!</Help>
!
!<Help KEYWORD="VWATER">
!<Tip> Water velocity.</Tip>
! Default = 1500 m/s.
! Allowed = Any "REAL/FLOAT" value (greater than zero)
! The current version of SMA works only for marine streamer data. So the 
! top surface is water which has a Water velocity.
!</Help>
!
!<Help KEYWORD="MLTP_SCALE">
!<Tip> A global amplitude scale multiples to the predicted multiples.</Tip>
! Default = 1.0
! Allowed = Any "REAL/FLOAT" value (Not equal to zero)
! A global amplitude scale multiples to the predicted multiples.
!</Help>
!
!<Help KEYWORD="WORK_DIR">
!<Tip> Temporary working directory.</Tip>
! Default = DEFAULT
! Allowed = Any directory or DEFAULT
! Temporary working directory. Please use "DEFAULT" unless for testing
!</Help>
!
!<Help KEYWORD="ICABLE_HDLOC">
!<Tip> Header location for cable number (fixed at 27).</Tip>
! Default = 27
! Allowed = 27
! Header location for cable number (fixed at 27).
!</Help>
!
!<Help KEYWORD="ISHOT_HDLOC">
!<Tip> Header location for shot number.</Tip>
! Default = 3
! Allowed = Any integer (greater than zero)
! Header location for shot number.
!</Help>
!
!<Help KEYWORD="IRECV_HDLOC">
!<Tip> Header location for receiver number.</Tip>
! Default = 4
! Allowed = Any integer (greater than zero)
! Header location for receiver number.
!</Help>
!
!<Help KEYWORD="NRECV_HDLOC">
!<Tip> Header location for cable receiver count.</Tip>
! Default = 51
! Allowed = Any integer (greater than zero)
! Header location for cable receiver count. All shots with one cable 
! should have the same number of receiver count.
!</Help>
!
!<Help KEYWORD="ICABLE_FIRST">
!<Tip> First cable number.</Tip>
! Default = 1
! Allowed = Any integer (greater than zero)
! First cable number.
!</Help>
!
!<Help KEYWORD="ISHOT_FIRST">
!<Tip> First shot number.</Tip>
! Default = 1
! Allowed = Any "REAL/FLOAT" (greater than zero)
! First shot number within cable.
!</Help>
!
!<Help KEYWORD="IRECV_FIRST">
!<Tip> First receiver number.</Tip>
! Default = 1
! Allowed = Any "REAL/FLOAT" (greater than zero)
! First receiver number within shot gather.
!</Help>
!
!<Help KEYWORD="ICABLE_INCR">
!<Tip> Increment for cable number.</Tip>
! Default = 1
! Allowed = Any integer (greater than zero)
! Increment for cable number.
!</Help>
!
!<Help KEYWORD="ISHOT_INCR">
!<Tip> Increment for shot number.</Tip>
! Default = 1
! Allowed = Any "REAL/FLOAT" (greater than zero)
! Increment for shot number within one cable.
!</Help>
!
!<Help KEYWORD="IRECV_INCR">
!<Tip> Increment for receiver number.</Tip>
! Default = 1
! Allowed = Any "REAL/FLOAT" (greater than zero)
! Increment for receiver number within a shot gather.
!</Help>
!
!<Help KEYWORD="OFFSET_REV">
!<Tip> Flag indicating if gathers are reversed by offset.</Tip>
! Default = NO
! Allowed = NO or YES
! Flag indicating if receivers are reversed by offset.
!   NO  -> Gathers arranged in order of increasing offset.
!   YES -> Gathers arranged in order of decreasing offset.
!</Help>
!
!<Help KEYWORD="MEMORY_SIZE">
!<Tip> Memory Size per CPU. Be sure to submit the job on the proper node.</Tip>
! Default = 1_GB
! Allowed = 1_GB, 2_GB
! Memory size per CPU. Be sure to submit the job on the proper node.
!</Help>
!
!<Help KEYWORD="MEMORY_EST">
!<Tip> Estimated memory requirements.</Tip>
! Default = NONE
! Allowed = See below.
! Estimated memory requirements.
!    If MEMORY_EST < 100: use 1_GB.
!    If MEMORY_EST > 100 and < 200: use 2_GB.
!    If MEMORY_EST > 200: break data in two parts..
!</Help>
! tabgroup = Mute
!<Help KEYWORD="opt_mute_sma">
!<Tip> Do you want to add a mute prior to multiple prediction but restored to the data?</Tip>
! Default = NO
! Allowed = YES or NO
! The mute is applied after saving original data (which you will see later in the flow for
! adaptive subtraction phase) but prior to predicting the multiple field.
!</Help>
! tabgroup = SMA
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module ibsma_module
      use pc_module
      use named_constants_module
      use string_module
      use grid_module            ! if you need the grid transformation.
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.
      use mute_module            ! internally call mute.
      use lav_module
      use cio_module
      use pcps_module

! --> Insert here any other modules used by this process.

      implicit none
      private
      public :: ibsma_create
      public :: ibsma_initialize
      public :: ibsma_update
      public :: ibsma_delete
      public :: ibsma            ! main trace processing routine.
      public :: ibsma_wrapup

      character(len=100),public,save :: IBSMA_IDENT = &
'$Id: ibsma.f90,v 1.18 2007/11/30 13:55:19 Stoeckley beta sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: ibsma_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.

! --> Below are commonly used globals - edit or remove as appropriate:

        integer                    :: ipn      ! process number.
        integer                    :: numtr    ! max number of input traces.
        logical                    :: gathered ! whether properly gathered.
        integer                    :: nwih     ! number of header words.
        integer                    :: ndpt     ! number of trace samples.
        real                       :: tstrt    ! time of 1st trace sample (sec).
        real                       :: dt       ! trace sample interval (sec).
        !type(grid_struct)          :: grid     ! grid transform.

        real                       :: drecv
        real                       :: dshot
        integer                    :: dtype
        real                       :: fmax
        integer                    :: maxorder
        integer                    :: nrecvmax
        integer                    :: nrecv
        integer                    :: nshottotal
        integer                    :: ntestbeg
        integer                    :: ntestshot
        integer                    :: wmax
        integer                    :: wmin
        integer                    :: restartx
        integer                    :: memory_sizex
        integer                    :: offset_revx
        real                       :: scale
        integer                    :: testx
        real                       :: vwater
        real                       :: vwater_default
        real                       :: mltp_scale
        integer                    :: icable_hdloc
        integer                    :: ishot_hdloc
        integer                    :: irecv_hdloc
        integer                    :: nrecv_hdloc
        integer                    :: icable_first
        real                       :: ishot_first
        real                       :: irecv_first
        integer                    :: icable_incr
        real                       :: ishot_incr
        real                       :: irecv_incr
        integer                    :: intrp_hdloc
        character(len=4)           :: memory_size
        character(len=22)          :: memory_est
        character(len=3)           :: offset_rev
        character(len=FILENAME_LENGTH) :: work_dir

        logical                    :: initialized
        logical                    :: init_out
        integer                    :: ngroups
        integer                    :: groupsize0
        integer                    :: groupsize1
        integer                    :: group
        integer                    :: groupsize
        integer                    :: trcnt
        integer                    :: trcol
        integer                    :: nshotgs
        integer                    :: shotgsize0
        integer                    :: shotgsize1
        integer                    :: shotg
        integer                    :: shotgsize
        integer                    :: ishot
        integer                    :: irecv
        integer                    :: icable_next
        real                       :: ishot_next
        real                       :: irecv_next
        integer                    :: icable_prev
        real                       :: ishot_prev
        real                       :: irecv_prev
        integer                    :: icable_last
        real                       :: ishot_last
        real                       :: irecv_last
        integer                    :: nwihc
        integer                    :: out_flag
        integer                    :: out_mode
        integer                    :: gtrcnt
        integer :: jcable

        integer                    :: icable_ndx_first
        integer                    :: ishot_ndx_first
        integer                    :: irecv_ndx_first
        integer                    :: icable_ndx_last
        integer                    :: ishot_ndx_last
        integer                    :: irecv_ndx_last
        integer                    :: icable_ndx_next
        integer                    :: ishot_ndx_next
        integer                    :: irecv_ndx_next
        integer                    :: icable_ndx_prev
        integer                    :: ishot_ndx_prev
        integer                    :: irecv_ndx_prev

        integer                    :: iomode       !control input/output mode
        integer                    :: ntr_save     !save info on saved hd/tr
        double precision, pointer  :: hd_save(:,:)
        real,             pointer  :: tr_save(:,:)
        double precision, pointer  :: hd_rev(:,:)
        real,             pointer  :: tr_rev(:,:)
        
        double precision, dimension(:), pointer :: hd0
        real, dimension(:), pointer :: tr0
        

        character(len=20) :: survey_units

        character(len=FILENAME_LENGTH) :: filename1
        character(len=FILENAME_LENGTH) :: filename2
        character(len=FILENAME_LENGTH) :: filename3
        integer :: mode
        integer :: stepFlag
        logical :: scaleFlag
        logical :: outputFlag
        logical :: lineBreak

! --> Insert any other needed variables or pointers here.
        character(len=3)            :: opt_mute_sma ! 'YES' or 'NO ' if I want mute.
        type(mute_struct),pointer   :: mute     ! for internal mute call.

      end type ibsma_struct

      integer, parameter :: IBSMA_MODE_NONE      = 0
      integer, parameter :: IBSMA_MODE_DATASTORE = 1
      integer, parameter :: IBSMA_MODE_DATAPREP  = 2
      integer, parameter :: IBSMA_MODE_PREDICT   = 3
      integer, parameter :: IBSMA_MODE_OUTPUT    = 4

      integer,parameter  :: io_init_mode  =0
      integer,parameter  :: io_input_mode =1
      integer,parameter  :: io_output_mode=2
      integer,parameter  :: io_done_mode  =3
      integer,parameter  :: io_error_mode =4

      integer, parameter :: opt_mute_sma_noptions=2
      character(len=3),save :: opt_mute_sma_options(opt_mute_sma_noptions)
      data opt_mute_sma_options /'YES' , 'NO '/
      


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


! --> Include any required interfaces here.


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(ibsma_struct),pointer,save :: object    ! needed for traps.

! --> Insert here any data declarations needed by this process.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine ibsma_create (obj)
      type(ibsma_struct),pointer :: obj       ! arguments
      integer                    :: ierr      ! for error checking
      integer, save              :: i_call = 0
      i_call = i_call + 1
      !print'(" top ibsma_create c=",i8)', i_call
      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in ibsma_create")


! --> Nullify any additional pointers in the OBJ data structure here.
      nullify(obj%hd0)
      nullify(obj%tr0)
      nullify(obj%hd_rev)
      nullify(obj%tr_rev)
      nullify(obj%hd_save)
      nullify(obj%tr_save)
      nullify(obj%mute)

      !print'(" aa1 ibsma_create c=",i8)', i_call
      call mute_create(obj%mute)
      ! -- above calls mute_initialize and mute_update.
      !print'(" aa2 ibsma_create c=",i8)', i_call
      call ibsma_initialize (obj)
      !print'(" end ibsma_create c=",i8)', i_call
      return
      end subroutine ibsma_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine ibsma_delete (obj)
      type(ibsma_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      call ibsma_wrapup (obj)


! --> Deallocate any additional pointers in the OBJ data structure here.
      if(associated(obj%hd0)) then
        deallocate(obj%hd0,     stat=ierr)
        if (ierr /= 0) &
          call pc_warning ("error deallocating hd0 in ibsma_delete")
      endif   
      
      if(associated(obj%tr0)) then
        deallocate(obj%tr0,     stat=ierr)
        if (ierr /= 0) &
          call pc_warning ("error deallocating tr0 in ibsma_delete")
      endif

      if(associated(obj%hd_rev)) then
        deallocate(obj%hd_rev, stat=ierr)
        if (ierr /= 0) &
          call pc_warning ("error deallocating hd_rev in ibsma_delete")
      endif
      
      if(associated(obj%tr_rev)) then
        deallocate(obj%tr_rev, stat=ierr)
        if (ierr /= 0) &
          call pc_warning ("error deallocating tr_rev in ibsma_delete")
      endif 
     
      if(associated(obj%hd_save)) then
        deallocate(obj%hd_save, stat=ierr)
        if (ierr /= 0) &
          call pc_warning ("error deallocating hd_save in ibsma_delete")
      endif
      
      if(associated(obj%tr_save)) then
        deallocate(obj%tr_save, stat=ierr)
        if (ierr /= 0) &
          call pc_warning ("error deallocating tr_save in ibsma_delete")
      endif

      call mute_delete(obj%mute)
      if(associated(obj%mute)) then
        deallocate(obj%mute, stat=ierr)
        if (ierr /= 0) &
          call pc_warning ("error deallocating mute in ibsma_delete")
      endif

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in ibsma_delete")

      
      end subroutine ibsma_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine ibsma_initialize (obj)
      type(ibsma_struct),intent(inout) :: obj       ! arguments
      integer, save              :: i_call = 0
      i_call = i_call + 1
      !print'(" top ibsma_initialize c=",i8)', i_call

      obj%drecv        = 0.0
      obj%dshot        = 0.0
      obj%dtype        = 3
      obj%fmax         = 90.0
      obj%maxorder     = 1
      obj%nrecvmax     = 1
      obj%nshottotal   = 1
      obj%wmax         = 60
      obj%wmin         = 5
      obj%vwater       = 1500.0
      obj%vwater_default = 1500.0
      obj%mltp_scale   = 1.0
      obj%work_dir     = 'DEFAULT'
      obj%icable_hdloc = 27
      obj%ishot_hdloc  = 3
      obj%irecv_hdloc  = 4
      obj%nrecv_hdloc  = 51
      obj%intrp_hdloc  = 0
      obj%icable_first = 1
      obj%ishot_first  = 1.0
      obj%irecv_first  = 1.0
      obj%icable_incr  = 1
      obj%ishot_incr   = 1.0
      obj%irecv_incr   = 1.0
      obj%offset_rev   = 'NO'
      obj%memory_size  = '1_GB'
      obj%memory_est   = ' '
      
      obj%iomode       = io_init_mode
      obj%ntr_save     = 0

      obj%memory_sizex = 1
      obj%testx = 0
      obj%restartx = 0
      obj%opt_mute_sma     = 'NO '

      !print'(" aa1 ibsma_initialize c=",i8)', i_call
      call ibsma_init_obj(obj)
      !print'(" aa2 ibsma_initialize c=",i8)', i_call

      call ibsma_update (obj)
      !print'(" top ibsma_initialize c=",i8)', i_call
      return
      end subroutine ibsma_initialize

!------------------------------------------------------------------------
! reset the variables in obj needed to start processing a new cable
!
! Written August 2003 by Charles C Burch
!------------------------------------------------------------------------
      subroutine ibsma_init_obj (obj)
      type(ibsma_struct),intent(inout) :: obj       ! arguments
      integer, save              :: i_call = 0
      i_call = i_call + 1
      !print'(" top ibsma_init_obj c=",i8)', i_call

      obj%initialized = .false.
      obj%init_out = .false.
      obj%group = 1
      obj%trcnt = 0
      obj%trcol = 0

      obj%shotg = 1
      obj%ishot = 1
      obj%irecv = 1

      obj%icable_next = 1.0
      obj%ishot_next  = 1.0
      obj%irecv_next  = 1.0
      obj%icable_prev = 0.0
      obj%ishot_prev  = 0.0
      obj%irecv_prev  = 0.0

      obj%icable_ndx_first = 1
      obj%ishot_ndx_first  = 1
      obj%irecv_ndx_first  = 1
      obj%icable_ndx_last  = 1
      obj%ishot_ndx_last   = 1
      obj%irecv_ndx_last   = 1
      obj%icable_ndx_next  = 1
      obj%ishot_ndx_next   = 1
      obj%irecv_ndx_next   = 1
      obj%icable_ndx_prev  = 0
      obj%ishot_ndx_prev   = 0
      obj%irecv_ndx_prev   = 0

      obj%scale        = 1.0
      obj%jcable       = 0

      obj%mode = IBSMA_MODE_NONE
      obj%stepFlag = 1
      obj%scaleFlag = .true.
      obj%outputFlag = .false.
      obj%lineBreak = .false.

      obj%out_flag     = 0
      obj%out_mode     = 0
      obj%gtrcnt       = 0

      !print'(" end ibsma_init_obj c=",i8)', i_call

      return

      end subroutine ibsma_init_obj  

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine ibsma_update (obj)
      type(ibsma_struct),intent(inout),target :: obj             ! arguments

      integer :: ierr   

      real    :: memory_est
      integer, save              :: i_call = 0
      i_call = i_call + 1
      !print'(" top ibsma_update c=",i8)', i_call

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!



! --> Delete any of the globals below that are not needed:

      obj%ipn = pc_get_ipn()

!      call pc_get_global ('numtr'   , obj%numtr)
!      call pc_get_global ('gathered', obj%gathered)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)
      !print'(" aa1 ibsma_update c=",i8)', i_call
      !call pc_get_global ('grid'    , obj%grid)
      !print'(" aa2 ibsma_update c=",i8)', i_call

      call pc_get_pdata('SURVEY_UNITS', obj%survey_units)
      if (obj%survey_units .eq. 'FEET') then
         obj%vwater_default = 4920.0
      else
         obj%vwater_default = 1500.0
      endif

      call pc_get('DRECV      ', obj%drecv      , ibsma_drecv)
      call pc_get('DSHOT      ', obj%dshot      , ibsma_dshot)
      call pc_get('DTYPE      ', obj%dtype      , ibsma_dtype)
      call pc_get('MAXORDER   ', obj%maxorder   , ibsma_maxorder)
      call pc_get('NRECVMAX   ', obj%nrecvmax   , ibsma_nrecvmax)
      call pc_get('NSHOTTOTAL ', obj%nshottotal , ibsma_nshottotal)
      call pc_get('WMIN       ', obj%wmin       , ibsma_wmin)
      call pc_get('WMAX       ', obj%wmax       , ibsma_wmax)
      call pc_get('FMAX       ', obj%fmax       , ibsma_fmax)
      call pc_get('VWATER     ', obj%vwater     , ibsma_vwater)
      call pc_get('MLTP_SCALE ', obj%mltp_scale , ibsma_mltp_scale)
      call pc_get('WORK_DIR   ', obj%work_dir   , ibsma_work_dir)
      call pc_get('ICABLE_HDLOC', obj%icable_hdloc, ibsma_icable_hdloc)
      call pc_get('ISHOT_HDLOC ', obj%ishot_hdloc , ibsma_ishot_hdloc)
      call pc_get('IRECV_HDLOC ', obj%irecv_hdloc , ibsma_irecv_hdloc)
      call pc_get('NRECV_HDLOC ', obj%nrecv_hdloc , ibsma_nrecv_hdloc)
      call pc_get('ICABLE_FIRST', obj%icable_first, ibsma_icable_first)
      call pc_get('ISHOT_FIRST ', obj%ishot_first , ibsma_ishot_first)
      call pc_get('IRECV_FIRST ', obj%irecv_first , ibsma_irecv_first)
      call pc_get('ICABLE_INCR ', obj%icable_incr , ibsma_icable_incr)
      call pc_get('ISHOT_INCR  ', obj%ishot_incr  , ibsma_ishot_incr)
      call pc_get('IRECV_INCR  ', obj%irecv_incr  , ibsma_irecv_incr)
      call pc_get('OFFSET_REV  ', obj%offset_rev  , ibsma_offset_rev)
      call pc_get('MEMORY_SIZE ', obj%memory_size , ibsma_memory_size)
      call pc_get('MEMORY_EST  ', obj%memory_est  , ibsma_memory_est)

!     -- MUTE -- internally called process
      call pc_put_options_field('opt_mute_sma',opt_mute_sma_options, opt_mute_sma_noptions)
      call pc_get('opt_mute_sma    ', obj%opt_mute_sma    )
      call string_to_upper(obj%opt_mute_sma)
      call pc_put('opt_mute_sma    ', obj%opt_mute_sma    )
      if(obj%opt_mute_sma == 'YES' ) then
        call mute_update (obj%mute)
      endif
!     -- MUTE -- -------------------------

!      *** screen traps ***

      call pc_call_screen_trap('PARAMETERS', ibsma_parameters)

!      *** end trap ***

      call pc_call_end_trap(ibsma_end)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


! --> Insert code to verify process parameters here (and/or in traps).


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


! --> Insert code to call internal processes to verify process parameters.


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field('OFFSET_REV', (/'NO ', 'YES'/) )
      call pc_put_options_field('MEMORY_SIZE', (/'1_GB', '2_GB'/) )

! --> Delete any of the globals below that have not changed:

      obj%numtr = 1
      obj%gathered = .false.
      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('gathered', obj%gathered)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)
      !call pc_put_global ('grid'    , obj%grid)

      call pc_put('DRECV      ', obj%drecv)
      call pc_put('DSHOT      ', obj%dshot)
      call pc_put('DTYPE      ', obj%dtype)
      call pc_put('FMAX       ', obj%fmax)
      call pc_put('MAXORDER   ', obj%maxorder)
      call pc_put('NRECVMAX   ', obj%nrecvmax)
      call pc_put('NSHOTTOTAL ', obj%nshottotal)
      call pc_put('WMAX       ', obj%wmax)
      call pc_put('WMIN       ', obj%wmin)
      call pc_put('VWATER     ', obj%vwater)
      call pc_put('MLTP_SCALE ', obj%mltp_scale)
      call pc_put('WORK_DIR   ', obj%work_dir)
      call pc_put('ICABLE_HDLOC', obj%icable_hdloc)
      call pc_put('ISHOT_HDLOC ', obj%ishot_hdloc)
      call pc_put('IRECV_HDLOC ', obj%irecv_hdloc)
      call pc_put('NRECV_HDLOC ', obj%nrecv_hdloc)
      call pc_put('ICABLE_FIRST', obj%icable_first)
      call pc_put('ISHOT_FIRST ', obj%ishot_first)
      call pc_put('IRECV_FIRST ', obj%irecv_first)
      call pc_put('ICABLE_INCR ', obj%icable_incr)
      call pc_put('ISHOT_INCR  ', obj%ishot_incr)
      call pc_put('IRECV_INCR  ', obj%irecv_incr)
      call pc_put('OFFSET_REV  ', obj%offset_rev)
      call pc_put('MEMORY_SIZE ', obj%memory_size)
      memory_est = (2*(obj%nshottotal+100))
      memory_est = memory_est + ((1+obj%maxorder)*obj%nrecvmax)
      memory_est = memory_est * obj%nshottotal
      memory_est = memory_est / (1000 * 1000)
      if(memory_est .lt. 110) then
         write(obj%memory_est, '(f8.3, a)') &
     &      memory_est/110, ': suggest 1_GB'
      else if(memory_est .lt. 220) then
         write(obj%memory_est, '(f8.3, a)') &
     &      memory_est/110, ': suggest 2_GB'
      else
         write(obj%memory_est, '(f8.3, a)') &
     &      memory_est/110, ': breakup data'
      end if
      call pc_put('MEMORY_EST  ', obj%memory_est)

      call pc_put_global ('ibsma.nshot', obj%nshottotal)
      call pc_put_global ('ibsma.nrecv', obj%nrecvmax)
      call pc_put_global ('ibsma.nrecv_hdloc', obj%nrecv_hdloc)
      call pc_put_global ('ibsma.icable_hdloc', obj%icable_hdloc)
      call pc_put_global ('ibsma.icable_first', obj%icable_first)
      call pc_put_global ('ibsma.icable_incr',  obj%icable_incr)
      call pc_put_global ('ibsma.ishot_hdloc', obj%ishot_hdloc)
      call pc_put_global ('ibsma.ishot_first', obj%ishot_first)
      call pc_put_global ('ibsma.ishot_incr',  obj%ishot_incr)
      call pc_put_global ('ibsma.irecv_hdloc', obj%irecv_hdloc)
      call pc_put_global ('ibsma.irecv_first', obj%irecv_first)
      call pc_put_global ('ibsma.irecv_incr',  obj%irecv_incr)

! --> Change the control defaults below as appropriate:

      call pc_put_control ('ntapes'       , 0)
      call pc_put_control ('need_request' , .true.)
      call pc_put_control ('need_label'   , .true.)
      call pc_put_control ('twosets'      , .false.)
      call pc_put_control ('nscratch'     , 0)
      call pc_put_control ('nstore'       , 0)
      call pc_put_control ('iftd'         , .false.)
      call pc_put_control ('ndisk'        , 0)
      call pc_put_control ('setup_only'   , .false.)

      call pc_put_control ('parallel_safe', .true.)
      call pc_put_control ('PCPS_SEND_MODE'        , 'PCPS_SEND_LINE')

!      call pc_get('PCPS_RECEIVE_MODE      ', jmode)
!      write(*,*) 'get mode = ',jmode
!      jerr = .false.
!      call pcps_set_receive_mode(PCPS_RECEIVE_GATHER, jerr)
!      write(*,*) 'set mode error = ',jerr

! --> Add here any other parameter cache calls such as to set sensitivities.

      obj%icable_next = obj%icable_first
      obj%ishot_next  = obj%ishot_first
      obj%irecv_next  = obj%irecv_first
      if(obj%offset_rev .eq. 'YES') then
         obj%offset_revx = 1
      else
         obj%offset_revx = 0
      end if
      if(obj%memory_size .eq. '2_GB') then
         obj%memory_sizex = 2
      else
         obj%memory_sizex = 1
      end if

!     call pc_put_sensitive_field_flag('MLTP_SCALE', .false.)
      call pc_put_sensitive_field_flag('ICABLE_HDLOC', .false.)
      call pc_put_sensitive_field_flag('MEMORY_EST', .false.)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


! --> Insert code to initialize variables for execution or deallocate arrays
! --> which will be reallocated below.

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

! --> Insert code to allocate needed permanent memory.
      obj%nwihc = obj%nwih + 1
      allocate(obj%hd0(obj%nwihc), stat=ierr)
      allocate(obj%tr0(obj%ndpt),  stat=ierr)
      allocate(obj%hd_save(obj%nwih,1), stat=ierr)
      allocate(obj%tr_save(obj%ndpt,1), stat=ierr)


      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

! --> Insert code to initialize anything needed for actual execution of process.


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
      !print'(" end ibsma_update c=",i8)', i_call

      return

      end subroutine ibsma_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


! *** Trap for variable DRECV ***

      subroutine ibsma_drecv(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      if(object%drecv .lt. 0.0) then
         object%drecv = 0.0
         call pc_put(keyword, object%drecv)
      end if
      object%dshot = object%drecv
      call pc_put('DSHOT', object%dshot)
      return
      end subroutine ibsma_drecv

! *** Trap for variable DSHOT ***

      subroutine ibsma_dshot(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      if(object%dshot .lt. 0.0) then
         object%dshot = 0.0
         call pc_put(keyword, object%dshot)
      end if
      object%drecv = object%dshot
      call pc_put('DRECV', object%drecv)
      return
      end subroutine ibsma_dshot

! *** Trap for variable DTYPE ***

      subroutine ibsma_dtype(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input

!     dtype cannot be less than 2
      if(object%dtype .lt. 2) then
         object%dtype = 3
         call pc_put(keyword, object%dtype)
         call pc_warning('DTYPE invalid...resetting.')
      end if

!     dtype cannot be greater than 4
      if(object%dtype .gt. 4) then
         object%dtype = 3
         call pc_put(keyword, object%dtype)
         call pc_warning('DTYPE invalid...resetting.')
      end if

!     warn is dtype equals 2
      if(object%dtype .eq. 2) then
         call pc_warning('DTYPE = 2...use only if working on synthetic data.')
      end if

      return
      end subroutine ibsma_dtype

! *** Trap for variable FMAX ***

      subroutine ibsma_fmax(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: tmsg
      real :: max

! --> Insert code to validate data input

!     fmax cannot be less than 25
      if(object%fmax .lt. 25.0) then
         object%fmax = 25.0
         call pc_put(keyword, object%fmax)
         call pc_warning('FMAX is less than than 25...resetting.')
      end if

!     fmax cannot be less than wmax
      if(object%fmax .lt. object%wmax) then
         object%fmax = object%wmax
         call pc_put(keyword, object%fmax)
         call pc_warning('FMAX is less than WMAX...resetting.')
      end if

!     fmax cannot be greater than (1/(2*dt))
      max = 1.0 / (2.0 * object%dt)
      if(object%fmax .gt. max) then
         object%fmax = max
         call pc_put(keyword, object%fmax)
         write(tmsg, *)'FMAX is greater than ',max,'Hz...resetting.'
         call pc_warning(tmsg)
      end if
      return
      end subroutine ibsma_fmax

! *** Trap for variable MAXORDER ***

      subroutine ibsma_maxorder(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input

!     warn if maxorder is not equal to 1
      if(object%maxorder .ne. 1) then
         call pc_warning('MAXORDER is not set to 1.')
      end if

      return
      end subroutine ibsma_maxorder

! *** Trap for variable NRECVMAX ***

      subroutine ibsma_nrecvmax(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      integer :: minshot

      call pc_get('NSHOTTOTAL', object%nshottotal)
      minshot = (2 * object%nrecvmax) - 1

! --> Insert code to validate data input
      if(object%nrecvmax .lt. 1) then
         object%nrecvmax = 1
         call pc_put(keyword, object%nrecvmax)
      end if

      if(object%nshottotal .lt. minshot) then
         object%nshottotal = minshot
         call pc_put('NSHOTTOTAL', object%nshottotal)
      end if

      return
      end subroutine ibsma_nrecvmax

! *** Trap for variable NSHOTTOTAL ***

      subroutine ibsma_nshottotal(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      integer :: minshot

      call pc_get('NRECVMAX', object%nrecvmax)
      minshot = (2 * object%nrecvmax) - 1

! --> Insert code to validate data input
      if(object%nshottotal .lt. minshot) then
         object%nshottotal = minshot
         call pc_put(keyword, object%nshottotal)
      end if
      return
      end subroutine ibsma_nshottotal

! *** Trap for variable NT ***

      subroutine ibsma_nt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_nt

! *** Trap for variable WMAX ***

      subroutine ibsma_wmax(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: tmsg
      real :: max

! --> Insert code to validate data input

!     warn if wmax is greater than 90
      if(object%wmax .gt. 90.0) then
          call pc_warning('WMAX is greater than 90Hz.')   
      end if

!     wmax cannot be less than wmin
      if(object%wmax .lt. object%wmin) then
         object%wmax = object%wmin
         call pc_put(keyword, object%wmax)
         call pc_warning('WMAX is less than WMIN...resetting.')   
      end if

!     wmax cannot be greater than (1/(2*dt))
      max = 1.0 / (2.0 * object%dt)
      if(object%wmax .gt. max) then
         write(tmsg, *) 'WMAX is greater than ',max,'Hz...resetting.'
         call pc_warning(tmsg)
         object%wmax = max
         call pc_put(keyword, object%wmax)
      end if
      return
      end subroutine ibsma_wmax

! *** Trap for variable WMIN ***

      subroutine ibsma_wmin(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input

!     warn if wmin is greater than 20
      if(object%wmin .gt. 20.0) then
         call pc_warning('WMIN is greater than 20Hz.')
      end if
      return
      end subroutine ibsma_wmin

! *** Trap for variable VWATER ***

      subroutine ibsma_vwater(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      if(object%vwater .lt. 0.0) then
         object%vwater = object%vwater_default
         call pc_put(keyword, object%vwater)
         call pc_warning('VWATER less than zero...resetting.')
      end if
      return
      end subroutine ibsma_vwater

! *** Trap for variable MLTP_SCALE ***

      subroutine ibsma_mltp_scale(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      if(object%mltp_scale .eq. 0.0) then
         object%mltp_scale = 1.0
         call pc_put(keyword, object%mltp_scale)
      end if
      return
      end subroutine ibsma_mltp_scale

! *** Trap for variable WORK_DIR ***

      subroutine ibsma_work_dir(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_work_dir

! *** Trap for variable ICABLE_HDLOC ***

      subroutine ibsma_icable_hdloc(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_icable_hdloc

! *** Trap for variable ISHOT_HDLOC ***

      subroutine ibsma_ishot_hdloc(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_ishot_hdloc

! *** Trap for variable IRECV_HDLOC ***

      subroutine ibsma_irecv_hdloc(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_irecv_hdloc

! *** Trap for variable NRECV_HDLOC ***

      subroutine ibsma_nrecv_hdloc(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_nrecv_hdloc

! *** Trap for variable ICABLE_FIRST ***

      subroutine ibsma_icable_first(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_icable_first

! *** Trap for variable ISHOT_FIRST ***

      subroutine ibsma_ishot_first(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_ishot_first

! *** Trap for variable IRECV_FIRST ***

      subroutine ibsma_irecv_first(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_irecv_first

! *** Trap for variable ICABLE_INCR ***

      subroutine ibsma_icable_incr(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_icable_incr

! *** Trap for variable ISHOT_INCR ***

      subroutine ibsma_ishot_incr(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_ishot_incr

! *** Trap for variable IRECV_INCR ***

      subroutine ibsma_irecv_incr(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_irecv_incr

! *** Trap for variable OFFSET_REV ***

      subroutine ibsma_offset_rev(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_offset_rev

! *** Trap for variable MEMORY_SIZE ***

      subroutine ibsma_memory_size(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_memory_size

! *** Trap for variable MEMORY_EST ***

      subroutine ibsma_memory_est(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_memory_est

! *** Screen trap for  PARAMETERS ***

      subroutine ibsma_parameters(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine ibsma_parameters

! *** End Trap ***

      subroutine ibsma_end
      implicit none

! --> Insert code to validate data input
      return
      end subroutine ibsma_end



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!-----------------------------------------------------------------------------
! Original ibsma changed to ibsma_one_trace to process a single line at a time
! New ibsma senses line breaks, get output traces and 
! reinitialize obj for a new line
!
! Written August 2003 by Charles Burch
!-----------------------------------------------------------------------------
      subroutine ibsma (obj, ntr, hd, tr)
      type(ibsma_struct),intent(inout) :: obj                    ! arguments
      integer          ,  intent(inout) :: ntr                    ! arguments
      double precision ,  intent(inout) :: hd(:,:)                ! arguments
      real             ,  intent(inout) :: tr(:,:)                ! arguments

      integer                           :: i, n, ntr1
      integer, save                     :: pass=0
      integer                           :: temp
      integer, save              :: i_call = 0
      i_call = i_call + 1
      !print'(" top ibsma c=",i8," ntr=",i8)', i_call, ntr

      if(obj%jcable .eq. 0 .and. ntr .gt. 0) then
         obj%jcable = hd(27,1)
      end if
!      if(ntr .ge. 1) then
!         write(*,*) 'ICABLE',obj%jcable,' INPUT TRACE',hd(9,1),hd(10,1)
!      endif
!      if(ntr .eq. NO_MORE_TRACES) then
!         write(*,*) 'ICABLE',obj%jcable,' NO MORE TRACES'
!      end if
!      if(ntr .eq. NEED_TRACES) then
!         write(*,*) 'ICABLE',obj%jcable,' NEED TRACES'
!      end if

100   continue
      pass=pass+1
!     if(mod(pass,1000).eq.1) print *,"pass, iomode, ntr, line_break, cpu=",&
!        pass, obj%iomode, ntr, pcps_line_break, pcps_current_worker_num
      if(pcps_line_break .eq. 1) obj%lineBreak = .true.

      if(obj%iomode.eq.io_init_mode) then
! ----- initialization mode-wanting trace data: ntr>0 or NO_MORE_TRACES
        if(ntr.gt.0.or.ntr.eq.NO_MORE_TRACES) then
          call ibsma_init_obj(obj)           !reinitialize obj variables
          obj%iomode=io_input_mode
        else
          goto 900           !invalid ntr for this state
        endif
      endif
      
!200  know iomode is not io_init_mode     
      if(obj%iomode.eq.io_input_mode) then
! ----- data input mode-passing on data as long on same line or hit end of data 
! ----- ntr >0  or NO_MORE_TRACES
    
        if(ntr.eq.NO_MORE_TRACES) then
          obj%iomode=io_done_mode              !out of input data
          call ibsma_one_trace(obj,ntr,hd,tr) !sending NO_MORE_TRACES
          goto 350                             ! process output results
      
        else if(ntr.gt.0) then
! ------- removed line break logic as ibsma_one_trace handles it
          n=ntr
          do i=1, n      !send data
            ntr1=1
            call ibsma_one_trace(obj,ntr1,hd(:,i:i), tr(:,i:i))
            if(ntr1>0) then
              if(ntr1.gt.1) goto 900    !should be only 1 trace
              if(i.lt.n) call ibsma_save(obj, n-i, hd(:,i+1:n), tr(:,i+1:n))
              if(i.gt.1) then
                hd(:,1)=hd(:,i)
                tr(:,1)=tr(:,i)
              endif
              ntr=ntr1
              obj%iomode=io_output_mode
              goto 350 
            endif
            if(ntr1.ne.NEED_TRACES) goto 900
          enddo
       
          ntr=NEED_TRACES         !get more input
!          write(*,*) 'OCABLE',obj%jcable,' NEED TRACES'
          go to 1001
          !return
        else
          goto 900                !invalid ntr for this state
        endif
      endif

!300  know iomode not io_init_mode and not io_input_mode
      if(obj%iomode.ne.io_output_mode .and. &
         obj%iomode.ne.io_done_mode) goto 900
      
! ----- output data or done mode, input ntr should be NEED_TRACES 
      if(ntr.ne.NEED_TRACES) goto 900

! --- ntr is NEED_TRACES
      call ibsma_one_trace(obj,ntr,hd,tr)     !request data 
      
! --- process output of ibsma, should be >0 or NO_MORE_TRACES
350   continue
      if(ntr.gt.0) then
        temp = size(tr,1)
        call lav_set_hdr (hd, tr, temp,ntr)
!        write(*,*) 'OCABLE',obj%jcable,' NTR',ntr
        go to 1001
        !return            !got output traces

      else if(ntr.eq.NO_MORE_TRACES) then
! ----- NO_MORE_TRACES received-exit if no more input, else
!         get ready for another line
        if(obj%iomode.eq.io_done_mode) then
          obj%iomode=io_error_mode             !all done, error if exec again
!        write(*,*) 'OCABLE',obj%jcable,' NTR',ntr
          go to 1001
          !return                               !return NO_MORE_TRACES
        endif
        goto 400     !use any saved data

      else if(ntr.eq.NEED_TRACES) then
        if(obj%iomode.eq.io_done_mode) goto 900
        goto 400 
      endif
      goto 900                               !invalid ntr for this state

! --- need more data, first check for any saved data      
400   obj%iomode=io_init_mode                !prepare to get more data
      if(obj%ntr_save.gt.0) then
        ntr=obj%ntr_save                     !use up saved data 
        hd(:,1:ntr)=obj%hd_save(1:size(hd,1),1:ntr)
        tr(:,1:ntr)=obj%tr_save(1:size(tr,1),1:ntr)
        obj%ntr_save=0
        goto 100                             !process saved data
      endif
      
      ntr=NEED_TRACES                        !no saved data-try to get more
      go to 1001

900   continue
      ntr=FATAL_ERROR
      obj%iomode=io_error_mode
1001  continue
      !print'(" end ibsma c=",i8," ntr=",i8)', i_call, ntr
      return
      end subroutine ibsma

!-------------------------------------------------------------------
! Save ntr, hd/tr into obj%ntr_save, obj%hd_save and obj%tr_save
!  Expand size obj%hd_save and obj%tr_save as needed
!
! Written August 2003 by Charles Burch
!-------------------------------------------------------------------
      subroutine ibsma_save(obj,ntr,hd,tr)
      type(ibsma_struct),intent(inout) :: obj                    ! arguments
      integer            ,intent(in)   :: ntr                    ! arguments
      double precision   ,intent(in)   :: hd(:,:)                ! arguments
      real               ,intent(in)   :: tr(:,:)                ! arguments

      integer                           :: ierr

      if(ntr.le.0) return

      if(ntr.gt.size(obj%hd_save,2) .or. size(hd,1).gt.size(obj%hd_save,1)) then
        deallocate(obj%hd_save,stat=ierr)
        allocate(obj%hd_save(size(hd,1),max(ntr,size(obj%hd_save,2))),stat=ierr)
      endif
      
      if(ntr.gt.size(obj%tr_save,2) .or. size(tr,1).gt.size(obj%tr_save,1)) then
        deallocate(obj%tr_save,stat=ierr)
        allocate(obj%tr_save(size(tr,1),max(ntr,size(obj%tr_save,2))),stat=ierr)
      endif
      
      obj%hd_save(1:size(hd,1),1:ntr)=hd(:,1:ntr)
      obj%tr_save(1:size(tr,1),1:ntr)=tr(:,1:ntr)
      obj%ntr_save=ntr
      return
      
      end subroutine ibsma_save

!------------------------------------------------------------------------
! this is the original ibsma subroutine which handled one trace at a time
!  This gets feed by the new ibsma subroutine
!
! Written August 2003 by Charles Burch
!------------------------------------------------------------------------
      subroutine ibsma_one_trace (obj,ntr,hd,tr)
      type(ibsma_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

! --> Insert declarations of local variables.
      integer :: ier             , trndx            , i  
      integer :: holl(100)
      integer :: ihdr, itrc
      integer :: icable_ndx, ishot_ndx, irecv_ndx
      integer :: icable
      real    :: ishot, irecv
      integer :: gtrndx, trcol, ierr
      integer :: missing_traces,missing_traces1,missing_traces2,missing_traces3
      real    :: ampmax        , ampsum 
      double precision  :: HDTOPMUTE, HDBOTMUTE
      integer :: mute_ntr

      double precision, dimension (obj%nwih,1) :: hd1
      real            , dimension (obj%ndpt,1) :: tr1



! --> Insert code for processing logic.

!     Get cable, shot and receiver numbers from trace header.
      icable = hd(obj%icable_hdloc,1)
      ishot  = hd(obj%ishot_hdloc,1)
      irecv  = hd(obj%irecv_hdloc,1)

!     If 1st trace processed, compute necessary indices.
      if(obj%trcnt .eq. 0) then

         obj%nrecv = hd(obj%nrecv_hdloc,1)
         if(obj%nrecv .gt. obj%nrecvmax) obj%nrecv = obj%nrecvmax
         obj%icable_last = obj%icable_first
         obj%ishot_last  = obj%ishot_first + &
     &      ((obj%nshottotal - 1) * obj%ishot_incr)
         obj%irecv_last  = obj%irecv_first + ((obj%nrecv - 1) * obj%irecv_incr)
!         if(obj%offset_revx .eq. 1) then
!            irecv_temp = obj%irecv_last
!            obj%irecv_last = obj%irecv_first
!            obj%irecv_first = irecv_temp
!            obj%irecv_incr = -obj%irecv_incr
!            obj%irecv_next = obj%irecv_first 
!         end if
         obj%icable_ndx_last = obj%icable_ndx_first
         obj%ishot_ndx_last  = obj%ishot_ndx_first + obj%nshottotal - 1
         obj%irecv_ndx_last  = obj%irecv_ndx_first + obj%nrecv - 1
      end if

!     Compute indices for cable, shot and receiver.
      icable_ndx = 1 + ((icable - obj%icable_first) / obj%icable_incr)
      ishot_ndx  = 1 + ((ishot  - obj%ishot_first) / obj%ishot_incr)
      irecv_ndx  = 1 + ((irecv  - obj%irecv_first) / obj%irecv_incr)

!     Abort job if any index is less than 1.
      if(ntr .eq. 1) then
         if(icable_ndx .le. 0) then
            write(*,*) 'Error: Cable ',ishot,&
     &                 ' less than first specified cable ',obj%icable_first
            ntr = FATAL_ERROR
            return
         end if
         if(ishot_ndx .le. 0) then
            write(*,*) 'Error: Shot ',ishot,&
     &                 ' less than first specified shot ',obj%ishot_first
            ntr = FATAL_ERROR
            return
         end if
         if(irecv_ndx .le. 0) then
            write(*,*) 'Error: Receiver ',irecv,&
     &                 ' less than first specified receiver ',obj%irecv_first
            ntr = FATAL_ERROR
            return
         end if
      end if

!     if(icable .ne. obj%icable_next) then
      if(obj%icable_incr .gt. 1) then
        if(mod(icable-obj%icable_first, obj%icable_incr).ne.0) then
          ntr = FATAL_ERROR
          return
        endif
      end if

100   continue

!     If 'no_more_traces' input, check current operating mode...
      if(ntr .eq. NO_MORE_TRACES) then
!        If in 'collect' mode, compute any missing traces; otherwise return.
         if(obj%mode .eq. IBSMA_MODE_DATASTORE) then
            missing_traces1 = obj%irecv_ndx_last - obj%irecv_ndx_next + 1
            missing_traces2 = obj%nrecv * &
     &         (obj%ishot_ndx_last - obj%ishot_ndx_next)
            missing_traces3 = 0
            missing_traces = missing_traces1 + missing_traces2
            goto 1500
         else
            return
         end if
      end if

!     If 'need_traces' input, check current operating mode...
      if(ntr .eq. NEED_TRACES) then
!        If in 'outout' mode, jump to output code; otherwise return.
         if(obj%mode .eq. IBSMA_MODE_OUTPUT) then
            goto 4000
         else
            return
         end if
      end if

!     Do some preliminary initializations, if not already done.
      if(.not. obj%initialized) then
         obj%initialized = .true.

!        Allocate storage for trace/header reversals.
         allocate(obj%hd_rev(obj%nwihc,obj%nrecv), stat=ierr)
         allocate(obj%tr_rev(obj%ndpt,obj%nrecv),  stat=ierr)

         call string_cc2hh(obj%work_dir, holl)

!        Call C code prediction initialization.
         call ibsma_initx(ier, holl, obj%memory_sizex, icable,&
     &      obj%restartx, obj%scale, obj%testx, obj%ntestshot, obj%ntestbeg,&
     &      obj%wmin, obj%wmax, obj%dtype, obj%nshottotal, obj%nrecv,&
     &      obj%dshot, obj%drecv, obj%fmax, obj%maxorder, obj%vwater,&
     &      obj%mltp_scale, obj%ndpt, obj%dt, obj%tstrt, obj%nwihc,&
     &      obj%ngroups, obj%groupsize0, obj%groupsize1,&
     &      obj%nshotgs, obj%shotgsize0, obj%shotgsize1)
         if(ier .ne. 0) then
            ntr = FATAL_ERROR
            return
         end if

!        Determine initial groupsize of gathers to be collected.
         if(obj%group .lt. obj%ngroups) then
            obj%groupsize = obj%groupsize0
         else
            obj%groupsize = obj%groupsize1
         end if

      end if

1000  continue
!     Set mode to 'collect'.
      obj%mode = IBSMA_MODE_DATASTORE
      ier = 0

!     Check group again number of groups.
      if(obj%group .gt. obj%ngroups) then
         ntr = NO_MORE_TRACES
         return
      end if
      
!     If this is a test, skip until 1st trace is passed.
!      if(obj%trcnt .eq. 0) then
!         ampmax = 0.0
!         do it = 1, obj%ndpt
!            ampabs = abs(tr(it,1))
!            if(ampmax < ampabs) ampmax = ampabs
!         end do
!         if(ampmax .eq. 0.0) then
!            write(*,*) 'Error: The 1st trace has only zeros!'
!            ntr = FATAL_ERROR
!            return
!         else
!            obj%scale = ampmax
!         end if
!         call ibsma_scale(ier, obj%scale)
!      end if

      missing_traces  = 0
      missing_traces1 = 0
      missing_traces2 = 0
      missing_traces3 = 0

!     Check for missing shots and receivers.
      if(ishot_ndx .lt. obj%ishot_ndx_next) then
!        Case 1: pending shot.
         obj%irecv_next = obj%irecv_first
         obj%irecv_prev = obj%irecv_first - obj%irecv_incr
         obj%irecv_ndx_next = 1
         obj%irecv_ndx_prev = 0
         ntr = NEED_TRACES
         return
      else if(ishot_ndx .gt. obj%ishot_ndx_next) then
!        Case 2: missing shot(s), missing recv(s).
         if(obj%irecv_ndx_prev .lt. obj%irecv_ndx_last) then
            missing_traces1 = obj%irecv_ndx_last - obj%irecv_ndx_prev
         end if
         if(ishot_ndx .le. obj%ishot_ndx_last) then
            missing_traces2 = obj%nrecv * (ishot_ndx - obj%ishot_ndx_next - 1)
            if(irecv_ndx .lt. obj%irecv_ndx_first) then
               missing_traces3 = 0
            else if(irecv_ndx .gt. obj%irecv_ndx_last) then
               missing_traces3 = obj%nrecv
            else
               missing_traces3 = irecv_ndx - obj%irecv_ndx_first
            end if
         else
            missing_traces2 = obj%nrecv * &
     &         (obj%ishot_ndx_last - obj%ishot_ndx_next)
            missing_traces3 = 0
         end if
         missing_traces = missing_traces1 + missing_traces2 + missing_traces3
      else
         if(irecv_ndx .lt. obj%irecv_ndx_next) then
!           Case 3: on shot, pending recv.
            ntr = NEED_TRACES
            return
         else if(irecv_ndx .gt. obj%irecv_ndx_next) then
!           Case 4: on shot, missing recv(s).
            missing_traces = irecv_ndx - obj%irecv_ndx_next
            if(irecv_ndx .gt. obj%irecv_ndx_last) then
               missing_traces = obj%irecv_ndx_last - obj%irecv_ndx_next + 1
            end if
         else
!           Case 5: on shot, on recv.
            missing_traces = 0
         end if
      end if

1500  continue

!     Insert any missing traces.
      if(missing_traces .gt. 0) then
         ishot = obj%ishot_next
         irecv = obj%irecv_next
         ishot_ndx = obj%ishot_ndx_next
         irecv_ndx = obj%irecv_ndx_next
         do ihdr = 1, obj%nwih
            obj%hd0(ihdr) = hd(ihdr,1)
         end do
         do itrc = 1, obj%ndpt
            obj%tr0(itrc) = 0.0
         end do
         do itrc = 1, missing_traces
            obj%hd0(obj%icable_hdloc) = icable
            obj%hd0(obj%ishot_hdloc)  = ishot
            obj%hd0(obj%irecv_hdloc)  = irecv
            write(*,*) 'missing trace: cable = ',icable,&
     &         'shot=',ishot,'  recv=',irecv

!           Increment trace count.
            obj%trcnt = obj%trcnt + 1
            obj%trcol = obj%trcol + 1

!           Collect trace and header.      
            obj%hd0(obj%nwihc) = 1.0; ! flag for missing trace

!           Increment group trace count.
            obj%gtrcnt = obj%gtrcnt + 1

!           Apply trace order reversal, if necessary.
            if(obj%offset_revx .eq. 0) then
               gtrndx = obj%gtrcnt
            else
               gtrndx = obj%nrecv - obj%gtrcnt + 1
            end if
            obj%hd_rev(1:obj%nwihc,gtrndx) = obj%hd0(1:obj%nwihc)
            obj%tr_rev(1:obj%ndpt,gtrndx) = obj%tr0(1:obj%ndpt)

!           Pass gather of traces to C code, one at a time.
            if(obj%gtrcnt .eq. obj%nrecv) then
               if(obj%scaleFlag) then
                  ampsum = 0.0
                  do i = 1, obj%nrecv
                     ampsum = ampsum + obj%hd_rev(25,i)
                  end do
                  ampmax = ampsum / obj%nrecv
                  if(ampmax .gt. 0.0) then
                     obj%scale = ampmax
                     call ibsma_scale(ier, obj%scale)
                     obj%scaleFlag = .false.
                  end if
               end if
               do i = 1, obj%nrecv
                  trcol = obj%trcol - obj%nrecv + i

!                 Call C code data storage for input trace.
                  ! the "trcol" parameter is not used...
                  ! saves this header and trace to a temp file "ofd_save" (filename0)
                  call ibsma_datastore(ier, obj%hd_rev(1:,i),&
     &               obj%tr_rev(1:,i), trcol)            
                  if(ier .ne. 0) then
                     ntr = FATAL_ERROR
                     return
                  end if
               end do
               obj%gtrcnt = 0
            end if

            if(obj%trcol .ge. obj%groupsize * obj%nrecv) then
!              Increment group and collection flags.
               obj%trcol = 0
               obj%group = obj%group + 1
!              Determine next groupsize of gathers to be collected.
               if(obj%group .lt. obj%ngroups) then
                  obj%groupsize = obj%groupsize0
               else
                  obj%groupsize = obj%groupsize1
               end if
            endif
            irecv = irecv + obj%irecv_incr
            irecv_ndx = irecv_ndx + 1
            if(irecv_ndx .gt. obj%irecv_ndx_last) then
               irecv = obj%irecv_first
               ishot = ishot + obj%ishot_incr
               irecv_ndx = obj%irecv_ndx_first
               ishot_ndx = ishot_ndx + 1
            end if
         end do
         if(obj%group .gt. obj%ngroups) then
            obj%stepFlag = 2
         end if
      end if
      if(obj%stepFlag .eq. 2) goto 2000

!     Increment trace count.
      obj%trcnt = obj%trcnt + 1
      obj%trcol = obj%trcol + 1

!     Collect trace and header.
      obj%hd0(1:obj%nwih) = hd(1:obj%nwih,1)
      obj%hd0(obj%nwihc) = 0.0

!     Increment group trace count.
      obj%gtrcnt = obj%gtrcnt + 1

!     Apply trace order reversal, if necessary.
      if(obj%offset_revx .eq. 0) then
         gtrndx = obj%gtrcnt
      else
         gtrndx = obj%nrecv - obj%gtrcnt + 1
      end if
      obj%hd_rev(1:obj%nwihc,gtrndx) = obj%hd0(1:obj%nwihc)
      obj%tr_rev(1:obj%ndpt,gtrndx) = tr(1:obj%ndpt,1)

!     Pass gather of traces to C code, one at a time.
      if(obj%gtrcnt .eq. obj%nrecv) then
         if(obj%scaleFlag) then
            ampsum = 0.0
            do i = 1, obj%nrecv
               ampsum = ampsum + obj%hd_rev(25,i)
            end do
            ampmax = ampsum / obj%nrecv
            if(ampmax .gt. 0.0) then
               obj%scale = ampmax
               call ibsma_scale(ier, obj%scale)
               obj%scaleFlag = .false.
            end if
         end if
         do i = 1, obj%nrecv
            trcol = obj%trcol - obj%nrecv + i
!           Call C code data storage for input trace.
            ! the "trcol" parameter is not used...
            ! saves this header and trace to a temp file "ofd_save" (filename0)
            call ibsma_datastore(ier, obj%hd_rev(1:,i),&
     &         obj%tr_rev(1:,i), trcol)            
            if(ier .ne. 0) then
               ntr = FATAL_ERROR
               return
            end if
         end do
         obj%gtrcnt = 0
      end if

!     Compute next indices and update prev indices.
      obj%ishot_prev = hd(obj%ishot_hdloc,1)
      obj%irecv_prev = hd(obj%irecv_hdloc,1)
      obj%ishot_next = obj%ishot_prev
      obj%irecv_next = obj%irecv_prev + obj%irecv_incr
      obj%ishot_ndx_prev = 1 + ((obj%ishot_prev - obj%ishot_first) &
     &         / obj%ishot_incr)
      obj%irecv_ndx_prev = 1 + ((obj%irecv_prev - obj%irecv_first) &
     &         / obj%irecv_incr)
      obj%ishot_ndx_next = obj%ishot_ndx_prev
      obj%irecv_ndx_next = obj%irecv_ndx_prev + 1
      if(obj%irecv_ndx_next .gt. obj%irecv_ndx_last) then
         obj%irecv_next = obj%irecv_first
         obj%irecv_prev = obj%irecv_first - obj%irecv_incr
         obj%ishot_prev = obj%ishot_next
         obj%ishot_next = obj%ishot_next + obj%ishot_incr
         obj%irecv_ndx_next = obj%irecv_ndx_first
         obj%irecv_ndx_prev = obj%irecv_ndx_first - 1
         obj%ishot_ndx_prev = obj%ishot_ndx_next
         obj%ishot_ndx_next = obj%ishot_ndx_next + 1
         if(obj%ishot_ndx_next .gt. obj%ishot_ndx_last) then
            obj%irecv_next = obj%irecv_first
            obj%irecv_prev = obj%irecv_first - obj%irecv_incr
            obj%ishot_next = obj%ishot_first
            obj%ishot_prev = obj%ishot_first - obj%ishot_incr
            obj%irecv_ndx_next = obj%irecv_ndx_first
            obj%irecv_ndx_prev = obj%irecv_ndx_first - 1
            obj%ishot_ndx_next = obj%ishot_ndx_first
            obj%ishot_ndx_prev = obj%ishot_ndx_first - 1
         end if
      end if

!     Request more input if done with current group.
      if(obj%trcol .lt. (obj%groupsize * obj%nrecv)) then
         if(obj%lineBreak) then
            ntr = NO_MORE_TRACES
            goto 100
         end if
         ntr = NEED_TRACES
         return
      endif

!     Increment group and collection flags.
      obj%trcol = 0
      obj%group = obj%group + 1

!     Determine next groupsize of gathers to be collected.
      if(obj%group .lt. obj%ngroups) then
         obj%groupsize = obj%groupsize0
      else
         obj%groupsize = obj%groupsize1
      end if

!     If not done with groups, continue collecting traces.
      if(obj%group .le. obj%ngroups) then
         ntr = NEED_TRACES
         return
      end if

      obj%stepFlag = 2

!     Jumpstart position - dataprep.
2000  continue
      obj%mode = IBSMA_MODE_DATAPREP
      ier = 0

      obj%group = 1
!     Determine next groupsize of gathers to be collected.
      if(obj%group .lt. obj%ngroups) then
         obj%groupsize = obj%groupsize0
      else
         obj%groupsize = obj%groupsize1
      end if

!     Reset trace count.
      obj%trcol = 0
      trcol = 0
      do trndx = 1, obj%trcnt

2100     continue
         ! reads header and trace values for trace "trndx" from the
         ! temp file that stored this trace in the ibsma_datastore
         ! subroutine.
         call ibsma_dataprep0(ier, obj%hd0(1:), obj%tr0(1:), trndx)
         if(ier .ne. 0) then
            ntr = FATAL_ERROR
            return
         end if

!        Get cable, shot and receiver numbers from trace header.
         icable = obj%hd0(obj%icable_hdloc)
         ishot  = obj%hd0(obj%ishot_hdloc)
         irecv  = obj%hd0(obj%irecv_hdloc)

!        Compute indices for cable, shot and receiver.
         icable_ndx = 1 + ((icable - obj%icable_first) / obj%icable_incr)
         ishot_ndx  = 1 + ((ishot  - obj%ishot_first) / obj%ishot_incr)
         irecv_ndx  = 1 + ((irecv  - obj%irecv_first) / obj%irecv_incr)

!        Increment trace count.
         obj%trcol = obj%trcol + 1
         trcol = obj%trcol

         !----------- here is where we need a call that only writes the
         !----------- original data out, allowing a mute (optional) after
         !----------- the write.
         !
         !print*,'Calling write_original icable, ishot, irecv:',icable,ishot,irecv
         call ibsma_write_original(ier,obj%tr0(1:))
         !print*,'Called write_original'
         !
         !----------- Now apply the optional mute before doing any sma work.
         if(obj%opt_mute_sma == 'YES') then
           !print*,'Calling mute, hdr_top_mute=',hdr_top_mute,' hdr_bottom_mute=',hdr_bottom_mute
           !-- save the mute header words.
           HDTOPMUTE                = obj%hd0(HDR_TOP_MUTE)
           HDBOTMUTE                = obj%hd0(HDR_BOTTOM_MUTE)
           !print*,'Calling mute, top_mute=',HDTOPMUTE,' bottom_mute=',HDBOTMUTE
           !-- copy the header and trace to a temporary 2-D array so I can call mute without modification.
           hd1(:,1) = obj%hd0(:)
           tr1(:,1) = obj%tr0(:)
           !print*,'Calling mute.     hd1(:,1)=',    hd1(:,1)
           !print*,'              obj%hd0(:  )=',obj%hd0(:)
           !-- do the mute.
           MUTE_NTR=1
           call mute(obj%mute,MUTE_NTR,hd1,tr1)
           !print*,'Called  mute.     hd1(:,1)=',    hd1(:,1)
           !print*,'Out of Mute: mute_ntr=',MUTE_NTR
           !-- copy header and trace to normal 1-D array location
           obj%hd0(:) = hd1(:,1)
           obj%tr0(:) = tr1(:,1)
           !-- put the mute headers back as they were prior to mute
           obj%hd0(HDR_TOP_MUTE)    = HDTOPMUTE
           obj%hd0(HDR_BOTTOM_MUTE) = HDBOTMUTE
           !print*,'Final header.     hd0(:  )=',obj%hd0(:  )
         endif
         !----------- end of optional mute section.

!        Call C code data preparation for input trace.
         ! stores the trace header in memory
         ! scales the data, storing it into a temp array
         ! applies an fft to the trace
         ! applies a low pass filter
         ! copies the output to a "data_out" array (complex)
         call ibsma_dataprep1(ier, obj%hd0(1:), obj%tr0(1:), trcol)            
         if(ier .ne. 0) then
            ntr = FATAL_ERROR
            return
         end if

!        Request more input if done with current group.
         if(obj%trcol .lt. (obj%groupsize * obj%nrecv)) then
            goto 2900
         endif

!        Write collected trace and header.
         ! take the header from memory and write to "ofd_head" file.(filename2)
         ! date the frequency (complex numbers and low-pass filtered) data
         ! and write it to the "ofd_data" temp file. (filename1)
         call ibsma_dataprep2(ier)
         if(ier .ne. 0) then
            ntr = FATAL_ERROR
            return
         end if

!        Increment group and collection flags.
         obj%trcol = 0
         obj%group = obj%group + 1

!        Determine next groupsize of gathers to be collected.
         if(obj%group .lt. obj%ngroups) then
            obj%groupsize = obj%groupsize0
         else
            obj%groupsize = obj%groupsize1
         end if

!        If not done with groups, continue collecting traces.
         if(obj%group .le. obj%ngroups) then
            goto 2900
         end if

!        Call C code data preparation wrapup.      
         ! go to beginning of ofd_data and ofd_head files, free memory,
         ! close and delete ofd_save (filename0) file.
         call ibsma_dataprep3(ier)
         if(ier .ne. 0) then
            ntr = FATAL_ERROR
            return
         end if
         obj%stepFlag = 3

2900     continue

      end do

      obj%stepFlag = 3

!     Jumpstart position - prediction.
3000  continue
      obj%mode = IBSMA_MODE_PREDICT
      ier = 0

!     Call C code multiple prediction.
      call ibsma_predict(ier)
      if(ier .ne. 0) then
         ntr = FATAL_ERROR
         return
      end if

      obj%stepFlag = 4

!     Jumpstart position - output.
4000  continue
      obj%mode = IBSMA_MODE_OUTPUT
      ier = 0
!     Determine initial shotsize of gathers to be collected.
      if(.not. obj%init_out) then
         obj%init_out = .true.
         obj%out_flag = 0
         obj%out_mode = 0
         obj%gtrcnt = 0
!        Call C code output initialization.
         call ibsma_output1(ier)
         if(ier .ne. 0) then
            ntr = FATAL_ERROR
            return
         end if
         if(obj%shotg .lt. obj%nshotgs) then
            obj%shotgsize = obj%shotgsize0
         else
            obj%shotgsize = obj%shotgsize1
         end if
      end if
      if(obj%shotg .gt. obj%nshotgs) then
         if(obj%out_flag .eq. 0) then
            obj%out_flag = 1
            obj%shotg = 1
            obj%ishot = 1
            obj%irecv = 1                  
            if(obj%shotg .lt. obj%nshotgs) then
               obj%shotgsize = obj%shotgsize0
            else
               obj%shotgsize = obj%shotgsize1
            end if
         else
            if(obj%out_mode .eq. 0) then
!              Call C code output wrapup.
!!               call ibsma_output4(ier)
!!               if(ier .ne. 0) then
!!                  ntr = FATAL_ERROR
!!                  return
!!               end if
               ntr = NO_MORE_TRACES
               return
            end if
         end if
      end if

!     NOTE: obj%ishot and obj%irecv are actually index values!!!      
      if(obj%out_mode .eq. 0) then
         do i = 1, obj%nrecv
            if(obj%out_flag .eq. 0) then
!              Call C code original data trace recovery.
               ! read data from ofd_orig file
               call ibsma_output2(ier, obj%shotg, obj%ishot, obj%irecv,&
     &            obj%hd0(1:), obj%tr0(1:))
            else
!              Call C code predicted multiple recovery.
               call ibsma_output3(ier, obj%shotg, obj%ishot, obj%irecv,&
     &            obj%hd0(1:), obj%tr0(1:))
            end if
            if(ier .ne. 0) then
               ntr = FATAL_ERROR
               return
            end if

!           Apply trace order reversal, if necessary.
            if(obj%offset_revx .eq. 0) then
               gtrndx = obj%irecv
            else
               gtrndx = obj%nrecv - obj%irecv + 1
            end if
            obj%hd_rev(1:obj%nwihc,gtrndx) = obj%hd0(1:obj%nwihc)
            obj%tr_rev(1:obj%ndpt,gtrndx) = obj%tr0(1:obj%ndpt)

            obj%irecv = obj%irecv + 1
            if(obj%irecv .gt. obj%nrecv) then
               obj%irecv = 1
               obj%ishot = obj%ishot + 1
               if(obj%ishot .gt. obj%shotgsize) then
                  obj%ishot = 1
                  obj%shotg = obj%shotg + 1
                  if(obj%shotg .lt. obj%nshotgs) then
                     obj%shotgsize = obj%shotgsize0
                  else
                     obj%shotgsize = obj%shotgsize1
                  end if
               end if
            end if
         end do
         obj%out_mode = 1
         if(obj%out_flag .eq. 1 .and. obj%shotg .gt. obj%nshotgs) then
            call ibsma_output4(ier)
            if(ier .ne. 0) then
               ntr = FATAL_ERROR
               return
            end if
         end if
      end if

!     Copy output trace/headers to output buffers.
      ntr = 1 
      obj%gtrcnt = obj%gtrcnt + 1
      gtrndx = obj%gtrcnt
      hd(1:obj%nwih,1) = obj%hd_rev(1:obj%nwih,gtrndx)
      tr(1:obj%ndpt,1) = obj%tr_rev(1:obj%ndpt,gtrndx)
      if(obj%gtrcnt .ge. obj%nrecv) then
         obj%out_mode = 0
         obj%gtrcnt = 0
      end if

!     Do not output an interpolated trace.
      if(obj%hd_rev(obj%nwihc,gtrndx) .eq. 1.0) then
         ntr = NEED_TRACES
         goto 4000
      end if

      if(ier .ne. 0) then
         ntr = FATAL_ERROR
         return
      end if
      
      end subroutine ibsma_one_trace


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine ibsma_wrapup (obj)
      type(ibsma_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

! --> Insert any required wrapup code here, including wrapups of
! --> internally-called processes.
      if(obj%opt_mute_sma == 'YES') call mute_wrapup(obj%mute)

      end subroutine ibsma_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module ibsma_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

