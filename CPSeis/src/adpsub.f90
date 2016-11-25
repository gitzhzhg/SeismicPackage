!<CPS_v1 type="PROCESS"/>
!!------------------------------- adpsub.f90 ---------------------------------!!
!!------------------------------- adpsub.f90 ---------------------------------!!
!!------------------------------- adpsub.f90 ---------------------------------!!

! --> Edit the line below as appropriate:

        ! other files are:  adpsub_crou.c  adpsub_frou.f90  adpsub.h

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
! Name       : ADPSUB
! Category   : miscellaneous
! Written    : 2003-08-14   by: walucas
! Revised    : 2007-11-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Perform adaptive subtraction.
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! ADPSUB performs adaptive subtraction between user provided data and noise. It
! is initially designed to work with IBSMA for surface multiple attenuation. But
! it can be used for general purpose as well.
!
! ADPSUB tries to slightly reshape a noise pattern found in user provided noise
! to match a noise pattern found in user provided data, then allows user either
! to output noise removed signal or to output reshaped noise.
!
! The noise reshaping and matching is done within a user specified local window.
! ADPSUB can only work on linear noise pattern. The kinematics and wavelet
! shape of noise pattern is what ADPSUB looks for, but not the amplitude of
! noise pattern.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! ADPSUB only accepts data in a specific order. For example, a user tries to
! apply SMA on a 3D marine towed streamer data. The input is a single sail swath
! with multiple cables. After the input goes through IBSMA, the output of IBSMA,
! which is also the input of ADPSUB, has the following order:
!        time, receiver, shot, original input, predicted noise, cable
! The key is that ADPSUB accepts one input file. This file includes both
! original input and predicted noise. One cable original input should be
! immedately followed by its predicted noise. The traces in original input
! should one-to-one correspond to the traces in predicted noise. Their trace
! headers should also match with each other as well.
!
! The data order required by ADPSUB is automatically satisfied from the output
! of IBSMA. If a user want to apply ADPSUB on other applications, the input
! order of ADPSUB can be met by using trace merge/sorting, etc..
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
! --> Insert trace input requirements here.
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
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 13. 2007-11-29  Stoeckley  Remove unused reference to memman.
!012. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
!011. 2006-01-10  B. Menger  Removed Unused Variables.
! 10. 2005-05-17  B. Lucas   Removed MEMORY_SIZE parameter.
!  9. 2005-04-04  Lucas/Shen Going back to revision 7.
!  7. 2004-04-16  B. Lucas   Added some comments to code.
!  6. 2004-02-25  B. Lucas   Added pc_global_keyword_present checks for each
!                            of the 'ibmsa.xxxxx' parameters.
!  5. 2004-02-17  B. Lucas   Added hdr location updates from IBSMA locations.
!  4. 2003-12-12  Y. Shen    Added general decsription and advice for users
!  3. 2003-11-20  walucas    Added MEMORY_SIZE.
!  2. 2003-11-10  walucas    Revised version.
!  1. 2003-08-14  walucas    Initial version.
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
!                            Adaptive Subtraction
!
!   `-Parameters-------------------------------------------------------------------
!   | WORK_DIR =~~~~~~~`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   | DOMAIN =~~~~~~~~~`CCCCC
!   | IOPT =~~~~~~~~~~~`CCCCC         ISCALE =~~~~~~~`CCCCCCCCC
!   | NSHOT =~~~~~~~~~~`IIIII         NRECV =~~~~~~~~`IIIII
!   | S_TIME =~~~~~~~~~`FFFFF         E_TIME = ~~~~~~`FFFFF
!   | T_WIN =~~~~~~~~~~`FFFFF         LWX =~~~~~~~~~~`IIIII
!   | F_LEN =~~~~~~~~~~`FFFFF         F_LENX =~~~~~~~`IIIII
!   | FC_MAX =~~~~~~~~~`II            NITER =~~~~~~~~`II
!   | NRECV_HDLOC =~~~~`II
!   | ICABLE_HDLOC =~~~`II    ICABLE_FIRST =~~~`IIIII    ICABLE_INCR =~~`IIIII
!   | ISHOT_HDLOC =~~~~`II    ISHOT_FIRST =~~~~`FFFFF    ISHOT_INCR =~~~`FFFFF
!   | IRECV_HDLOC =~~~~`II    IRECV_FIRST =~~~~`FFFFF    IRECV_INCR =~~~`FFFFF
!   `----------------------------------------------------------------------------
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="DOMAIN">
!<Tip> Domain for adaptive subtraction.</Tip>
! Default = OFFSET
! Allowed = SHOT or OFFSET
! Domain for adaptive subtraction. Input data is required in common-shot domain.
! When ADPSUB is combined with IBSMA for demultiple, it yields best result and
! takes less CPU time when working in common-offset domain. That is why the
! default is "OFFSET". ADPSUB does sorting internally on the fly. When
! ADPSUB is used with other applications, it is up to user to test in which
! domain ADPSUB produces optimal result. If necessary, user can use some trace
! header index to let the program think that the input is in common-shot domain,
! but actually the input is in common-offset domain.
!</Help>
!
!<Help KEYWORD="E_TIME">
!<Tip> End time for adaptive subtraction.</Tip>
! Default = time of last sample
! Allowed = Any value.
! End time for adaptive subtraction. It should be equal to or less trace length,
! but greater than S_TIME.
!</Help>
!
!<Help KEYWORD="FC_MAX">
!<Tip> Number of filter coefficients.</Tip>
! Default = 6
! Allowed = Any integer (greater than zero)
! Number of filter coefficients. Please use the default.
!</Help>
!
!<Help KEYWORD="F_LEN">
!<Tip> Filter length in time.</Tip>
! Default = 0.02
! Allowed = Any value (greater than zero)
! Filter length in time. Please use the default.
!</Help>
!
!<Help KEYWORD="F_LENX">
!<Tip> Filter length in space.</Tip>
! Default = 3
! Allowed = Any integer (greater than zero)
! Filter length in space. Please use the default.
!</Help>
!
!<Help KEYWORD="IOPT">
!<Tip> Output noise-attenuated signal or estimated noise.</Tip>
! Default = SIGNAL
! Allowed = SIGNAL or NOISE.
! Output option.
!</Help>
!
!<Help KEYWORD="ISCALE">
!<Tip> Scaling option. Allow noise ampltide modification to match data.</Tip>
! Default = SCALING
! Allowed = SCALING or NO_SCALING
! Scaling option. Please use the default.
!</Help>
!
!<Help KEYWORD="LWX">
!<Tip> Spatial window length.</Tip>
! Default = 5
! Allowed = Any integer value (greater than zero)
! Spatial window length. Please use the default.
!</Help>
!
!<Help KEYWORD="NITER">
!<Tip> Number of iterations.</Tip>
! Default = 0
! Allowed = Any integer value
! Number of iterations. Please use the default.
!</Help>
!
!<Help KEYWORD="NRECV">
!<Tip> Number of receivers within each shot gather.</Tip>
! Default = 1
! Allowed = Any integer value (greater than zero)
! Maximum number of receivers for any shot gather on all cable lines
! processed by one job.
!</Help>
!
!<Help KEYWORD="NSHOT">
!<Tip> Number of shots.</Tip>
! Default = 1
! Allowed = Any integer value (greater than zero)
! Total number of shots per cable line. All cables processed by one
! job should have the same number of shots.
!</Help>
!
!<Help KEYWORD="S_TIME">
!<Tip> Start time for adaptive subtraction.</Tip>
! Default = time of first sample
! Allowed = Any value
! Start time for adaptive subtraction. It should be equal to or greater
! than the starting time of a trace, but less than E_TIME. To save
! computational time, it is a good practise to set S_TIME to be slightly
! less than the earliest travel time of zero offset 1st-order multiple
! along the entire sail line.
!
!<Help KEYWORD="T_WIN">
!<Tip> Temporal window length.</Tip>
! Default = 0.2
! Allowed = Any value (greater than zero)
! Temporal window length. Please use the default.
!</Help>
!
!<Help KEYWORD="WORK_DIR">
!<Tip> Temporary working directory.</Tip>
! Default = DEFAULT
! Allowed = Any directory or DEFAULT
! Temporary working directory. Please use the default unless for testing.
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
! Increment for shot number within cable.
!</Help>
!
!<Help KEYWORD="IRECV_INCR">
!<Tip> Increment for receiver number.</Tip>
! Default = 1
! Allowed = Any "REAL/FLOAT" (greater than zero)
! Increment for receiver number within shot gather.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module adpsub_module
      use pc_module
      use named_constants_module
      use string_module
      use grid_module            ! if you need the grid transformation.
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.
      use lav_module
      use cio_module
      use pcps_module

! --> Insert here any other modules used by this process.

      implicit none
      private
      public :: adpsub_create
      public :: adpsub_initialize
      public :: adpsub_update
      public :: adpsub_delete
      public :: adpsub            ! main trace processing routine.
      public :: ADPSUB_ASUBB
      public :: adpsub_asubbwin
      public :: adpsub_tent2
      public :: adpsub_patch3
      public :: adpsub_adjnull
      public :: adpsub_separt
      public :: adpsub_shap2d
      public :: adpsub_cgplus
      public :: adpsub_ddot2
      public :: adpsub_icaf2
      public :: adpsub_randm
      public :: adpsub_vmov
      public :: adpsub_vsub
      public :: adpsub_wrapup

      character(len=100),public,save :: ADPSUB_IDENT = &
"$Id: adpsub.f90,v 1.13 2007/11/30 13:55:15 Stoeckley beta sps $"


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: adpsub_struct

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
        type(grid_struct)          :: grid     ! grid transform.

        character(len=6)           :: domain
        integer                    :: domainx
        real                       :: e_time
        integer                    :: fc_max
        real                       :: f_len
        integer                    :: f_lenx
        character(len=6)           :: iopt
        integer                    :: ioptx
        character(len=10)          :: iscale
        integer                    :: iscalex
        integer                    :: lwx
        integer                    :: niter
        integer                    :: nrecv
        integer                    :: nshot
        character(len=11)          :: restart
        integer                    :: restartx
        integer                    :: memory_sizex
        real                       :: s_time
        real                       :: t_win
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
        integer                    :: out_mode
        integer                    :: gtrcnt

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

        character(len=FILENAME_LENGTH) :: filename1
        character(len=FILENAME_LENGTH) :: filename2
        character(len=FILENAME_LENGTH) :: filename3

        integer :: mode
        integer :: stepFlag
        integer :: dataFlag

! --> Insert any other needed variables or pointers here.

      end type adpsub_struct

      integer, parameter :: ADPSUB_MODE_NONE    = 0
      integer, parameter :: ADPSUB_MODE_COLLECT = 1
      integer, parameter :: ADPSUB_MODE_SUBTRACT = 2
      integer, parameter :: ADPSUB_MODE_OUTPUT  = 3

      integer,parameter  :: io_init_mode  =0
      integer,parameter  :: io_input_mode =1
      integer,parameter  :: io_output_mode=2
      integer,parameter  :: io_done_mode  =3
      integer,parameter  :: io_error_mode =4


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


! --> Include any required interfaces here.


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(adpsub_struct),pointer,save :: object    ! needed for traps.

! --> Insert here any data declarations needed by this process.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine adpsub_create (obj)
      type(adpsub_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in adpsub_create")

! --> Nullify any additional pointers in the OBJ data structure here.

      nullify (obj%hd_save) ! jpa
      nullify (obj%tr_save) ! jpa
      nullify (obj%hd_rev) ! jpa
      nullify (obj%tr_rev) ! jpa
      nullify (obj%hd0) ! jpa
      nullify (obj%tr0) ! jpa

      call adpsub_initialize (obj)
      end subroutine adpsub_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine adpsub_delete (obj)
      type(adpsub_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      call adpsub_wrapup (obj)


! --> Deallocate any additional pointers in the OBJ data structure here.

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in adpsub_delete")
      end subroutine adpsub_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine adpsub_initialize (obj)
      type(adpsub_struct),intent(inout) :: obj       ! arguments

! --> Change the default values below as needed
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)

      obj%domain   = 'OFFSET'
      obj%e_time   = obj%tstrt + ((obj%ndpt-1) * obj%dt)
      obj%fc_max   = 6
      obj%f_len    = 0.02
      obj%f_lenx   = 3
      obj%iopt     = 'SIGNAL'
      obj%iscale   = 'SCALING'
      obj%lwx      = 5
      obj%niter    = 0
      obj%nrecv    = 1
      obj%nshot    = 1
      obj%restart  = 'DATA_PREP'
      obj%s_time   = obj%tstrt
      obj%t_win    = 0.2
      obj%work_dir = 'DEFAULT'
      obj%icable_hdloc = 27
      obj%ishot_hdloc  = 3
      obj%irecv_hdloc  = 4
      obj%nrecv_hdloc  = 51
      obj%icable_first = 1
      obj%ishot_first  = 1.0
      obj%irecv_first  = 1.0
      obj%icable_incr  = 1
      obj%ishot_incr   = 1.0
      obj%irecv_incr   = 1.0

      obj%iomode       = io_init_mode
      obj%ntr_save     = 0

      obj%memory_sizex = 1
      obj%restartx = 0

      call adpsub_init_obj(obj)

      call adpsub_update (obj)
      end subroutine adpsub_initialize

!------------------------------------------------------------------------
! reset the variables in obj needed to start processing a new cable
!
! Written August 2003 by Charles C Burch
!------------------------------------------------------------------------
      subroutine adpsub_init_obj (obj)
      type(adpsub_struct),intent(inout) :: obj       ! arguments

      obj%initialized = .false.
      obj%init_out = .false.
      obj%group = 1
      obj%trcnt = 0
      obj%trcol = 0

      obj%ishot = 1
      obj%irecv = 1

      obj%icable_next = 1
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

      obj%mode = ADPSUB_MODE_NONE
      obj%stepFlag = 1
      obj%dataFlag = 1

      obj%out_mode     = 0
      obj%gtrcnt       = 0

      end subroutine adpsub_init_obj

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine adpsub_update (obj)
      type(adpsub_struct),intent(inout),target :: obj             ! arguments

! --> Insert code to declare all required local variables.
      integer :: ierr
      logical :: present

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
      call pc_get_global ('grid'    , obj%grid)

      call pc_get('DOMAIN  ', obj%domain  , adpsub_domain)
      call pc_get('E_TIME  ', obj%e_time  , adpsub_e_time)
      call pc_get('FC_MAX  ', obj%fc_max  , adpsub_fc_max)
      call pc_get('F_LEN   ', obj%f_len   , adpsub_f_len)
      call pc_get('F_LENX  ', obj%f_lenx  , adpsub_f_lenx)
      call pc_get('IOPT    ', obj%iopt    , adpsub_iopt)
      call pc_get('ISCALE  ', obj%iscale  , adpsub_iscale)
      call pc_get('LWX     ', obj%lwx     , adpsub_lwx)
      call pc_get('NITER   ', obj%niter   , adpsub_niter)
      call pc_get('NRECV   ', obj%nrecv   , adpsub_nrecv)
      call pc_get('NSHOT   ', obj%nshot   , adpsub_nshot)
      call pc_get('S_TIME  ', obj%s_time  , adpsub_s_time)
      call pc_get('T_WIN   ', obj%t_win   , adpsub_t_win)
      call pc_get('WORK_DIR', obj%work_dir, adpsub_work_dir)
      call pc_get('ICABLE_HDLOC', obj%icable_hdloc, adpsub_icable_hdloc)
      call pc_get('ISHOT_HDLOC ', obj%ishot_hdloc , adpsub_ishot_hdloc)
      call pc_get('IRECV_HDLOC ', obj%irecv_hdloc , adpsub_irecv_hdloc)
      call pc_get('NRECV_HDLOC ', obj%nrecv_hdloc , adpsub_nrecv_hdloc)
      call pc_get('ICABLE_FIRST', obj%icable_first, adpsub_icable_first)
      call pc_get('ISHOT_FIRST ', obj%ishot_first , adpsub_ishot_first)
      call pc_get('IRECV_FIRST ', obj%irecv_first , adpsub_irecv_first)
      call pc_get('ICABLE_INCR ', obj%icable_incr , adpsub_icable_incr)
      call pc_get('ISHOT_INCR  ', obj%ishot_incr  , adpsub_ishot_incr)
      call pc_get('IRECV_INCR  ', obj%irecv_incr  , adpsub_irecv_incr)

      present = pc_global_keyword_present('ibsma.nshot')
      if(present) then
         call pc_get_global ('ibsma.nshot', obj%nshot)
      end if

      present = pc_global_keyword_present('ibsma.nrecv')
      if(present) then
         call pc_get_global ('ibsma.nrecv', obj%nrecv)
      end if

      present = pc_global_keyword_present('ibsma.nrecv_hdloc')
      if(present) then
         call pc_get_global ('ibsma.nrecv_hdloc', obj%nrecv_hdloc)
      end if

      present = pc_global_keyword_present('ibsma.icable_hdloc')
      if(present) then
         call pc_get_global ('ibsma.icable_hdloc', obj%icable_hdloc)
      end if

      present = pc_global_keyword_present('ibsma.icable_first')
      if(present) then
         call pc_get_global ('ibsma.icable_first', obj%icable_first)
      end if

      present = pc_global_keyword_present('ibsma.icable_incr')
      if(present) then
         call pc_get_global ('ibsma.icable_incr',  obj%icable_incr)
      end if

      present = pc_global_keyword_present('ibsma.ishot_hdloc')
      if(present) then
         call pc_get_global ('ibsma.ishot_hdloc', obj%ishot_hdloc)
      end if

      present = pc_global_keyword_present('ibsma.ishot_first')
      if(present) then
         call pc_get_global ('ibsma.ishot_first', obj%ishot_first)
      end if

      present = pc_global_keyword_present('ibsma.ishot_incr')
      if(present) then
         call pc_get_global ('ibsma.ishot_incr',  obj%ishot_incr)
      end if

      present = pc_global_keyword_present('ibsma.irecv_hdloc')
      if(present) then
         call pc_get_global ('ibsma.irecv_hdloc', obj%irecv_hdloc)
      end if

      present = pc_global_keyword_present('ibsma.irecv_first')
      if(present) then
         call pc_get_global ('ibsma.irecv_first', obj%irecv_first)
      end if

      present = pc_global_keyword_present('ibsma.irecv_incr')
      if(present) then
          call pc_get_global ('ibsma.irecv_incr',  obj%irecv_incr)
      end if

!      *** screen traps ***

      call pc_call_screen_trap('PARAMETERS', adpsub_parameters)

!      *** end trap ***

      call pc_call_end_trap(adpsub_end)


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

      call pc_put_options_field('DOMAIN', (/'SHOT  ', 'OFFSET'/) )
      call pc_put_options_field('IOPT  ', (/'SIGNAL', 'NOISE '/) )
      call pc_put_options_field('ISCALE', (/'SCALING   ', 'NO_SCALING'/) )

! --> Delete any of the globals below that have not changed:

      obj%numtr = 1
      obj%gathered = .false.
      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('gathered', obj%gathered)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)
      call pc_put_global ('grid'    , obj%grid)

      call pc_put('DOMAIN  ', obj%domain)
      call pc_put('E_TIME  ', obj%e_time)
      call pc_put('FC_MAX  ', obj%fc_max)
      call pc_put('F_LEN   ', obj%f_len)
      call pc_put('F_LENX  ', obj%f_lenx)
      call pc_put('IOPT    ', obj%iopt)
      call pc_put('ISCALE  ', obj%iscale)
      call pc_put('LWX     ', obj%lwx)
      call pc_put('NITER   ', obj%niter)
      call pc_put('NRECV   ', obj%nrecv)
      call pc_put('NSHOT   ', obj%nshot)
      call pc_put('S_TIME  ', obj%s_time)
      call pc_put('T_WIN   ', obj%t_win)
      call pc_put('WORK_DIR', obj%work_dir)
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
      call pc_put_control ('PCPS_SEND_MODE', 'PCPS_SEND_LINE')

! --> Add here any other parameter cache calls such as to set sensitivities.
      obj%icable_next = obj%icable_first
      obj%ishot_next  = obj%ishot_first
      obj%irecv_next  = obj%irecv_first
      if(obj%domain .eq. 'OFFSET') then
         obj%domainx = 1
      else
         obj%domainx = 0
      end if
      if(obj%iopt .eq. 'NOISE') then
         obj%ioptx = 1
      else
         obj%ioptx = 0
      end if
      if(obj%iscale .eq. 'NO_SCALING') then
         obj%iscalex = 1
      else
         obj%iscalex = 0
      end if
      obj%restartx = 0
      obj%memory_sizex = 1
      call pc_put_sensitive_field_flag('ICABLE_HDLOC', .false.)

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


      end subroutine adpsub_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


! *** Trap for variable DOMAIN ***

      subroutine adpsub_domain(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_domain

! *** Trap for variable E_TIME ***

      subroutine adpsub_e_time(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_e_time

! *** Trap for variable FC_MAX ***

      subroutine adpsub_fc_max(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      if(object%fc_max .lt. 1) then
         object%fc_max = 1
         call pc_put(keyword, object%fc_max)
      end if
      return
      end subroutine adpsub_fc_max

! *** Trap for variable F_LEN ***

      subroutine adpsub_f_len(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      if(object%f_len .le. 0.0) then
         object%f_len = 0.02
         call pc_put(keyword, object%f_len)
      end if
      return
      end subroutine adpsub_f_len

! *** Trap for variable F_LENX ***

      subroutine adpsub_f_lenx(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      if(object%f_lenx .lt. 1) then
         object%f_lenx = 1
         call pc_put(keyword, object%f_lenx)
      end if
      return
      end subroutine adpsub_f_lenx

! *** Trap for variable IOPT ***

      subroutine adpsub_iopt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_iopt

! *** Trap for variable ISCALE ***

      subroutine adpsub_iscale(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_iscale

! *** Trap for variable LWX ***

      subroutine adpsub_lwx(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_lwx

! *** Trap for variable NITER ***

      subroutine adpsub_niter(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      if(object%niter .lt. 0) then
         object%niter = 0
         call pc_put(keyword, object%niter)
      end if
      return
      end subroutine adpsub_niter

! *** Trap for variable NRECV ***

      subroutine adpsub_nrecv(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      if(object%nrecv .lt. 1) then
         object%nrecv = 1
         call pc_put(keyword, object%nrecv)
      end if
      return
      end subroutine adpsub_nrecv

! *** Trap for variable NSHOT ***

      subroutine adpsub_nshot(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      if(object%nshot .lt. 1) then
         object%nshot = 1
         call pc_put(keyword, object%nshot)
      end if
      return
      end subroutine adpsub_nshot

! *** Trap for variable S_TIME ***

      subroutine adpsub_s_time(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_s_time

! *** Trap for variable T_WIN ***

      subroutine adpsub_t_win(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      if(object%t_win .le. 0.0) then
         object%t_win = 0.2
         call pc_put(keyword, object%t_win)
      end if
      return
      end subroutine adpsub_t_win

! *** Trap for variable WORK_DIR ***

      subroutine adpsub_work_dir(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_work_dir

! *** Trap for variable ICABLE_HDLOC ***

      subroutine adpsub_icable_hdloc(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_icable_hdloc

! *** Trap for variable ISHOT_HDLOC ***

      subroutine adpsub_ishot_hdloc(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_ishot_hdloc

! *** Trap for variable IRECV_HDLOC ***

      subroutine adpsub_irecv_hdloc(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_irecv_hdloc

! *** Trap for variable NRECV_HDLOC ***

      subroutine adpsub_nrecv_hdloc(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_nrecv_hdloc

! *** Trap for variable ICABLE_FIRST ***

      subroutine adpsub_icable_first(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_icable_first

! *** Trap for variable ISHOT_FIRST ***

      subroutine adpsub_ishot_first(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_ishot_first

! *** Trap for variable IRECV_FIRST ***

      subroutine adpsub_irecv_first(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_irecv_first

! *** Trap for variable ICABLE_INCR ***

      subroutine adpsub_icable_incr(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_icable_incr

! *** Trap for variable ISHOT_INCR ***

      subroutine adpsub_ishot_incr(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_ishot_incr

! *** Trap for variable IRECV_INCR ***

      subroutine adpsub_irecv_incr(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_irecv_incr

! *** Screen trap for  PARAMETERS ***

      subroutine adpsub_parameters(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine adpsub_parameters

! *** End Trap ***

      subroutine adpsub_end
      implicit none

! --> Insert code to validate data input
      return
      end subroutine adpsub_end



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!-----------------------------------------------------------------------------
! Original adpsub changed to adpsub_one_trace to process a single line at a time
! New adpsub senses line breaks, get output traces and
! reinitialize obj for a new line
!
! Written August 2003 by Charles Burch
!-----------------------------------------------------------------------------
      subroutine adpsub (obj, ntr, hd, tr)
      type(adpsub_struct),intent(inout) :: obj                    ! arguments
      integer          ,  intent(inout) :: ntr                    ! arguments
      double precision ,  intent(inout) :: hd(:,:)                ! arguments
      real             ,  intent(inout) :: tr(:,:)                ! arguments

      integer                           :: i, n, ntr1,temp
      integer, save                     :: pass=0

100   continue
      pass=pass+1
!     if(mod(pass,1000).eq.1) print *,"pass, iomode, ntr, line_break, cpu=",&
!        pass, obj%iomode, ntr, pcps_line_break, pcps_current_worker_num

      if(obj%iomode.eq.io_init_mode) then
! ----- initialization mode-wanting trace data: ntr>0 or NO_MORE_TRACES
        if(ntr.gt.0.or.ntr.eq.NO_MORE_TRACES) then
          call adpsub_init_obj(obj)           !reinitialize obj variables
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
          call adpsub_one_trace(obj,ntr,hd,tr) !sending NO_MORE_TRACES
          goto 350                             ! process output results

        else if(ntr.gt.0) then
! ------- removed line break logic as adpsub_one_trace handles it
          n=ntr
          do i=1, n      !send data
            ntr1=1
            call adpsub_one_trace(obj,ntr1,hd(:,i:i), tr(:,i:i))
            if(ntr1>0) then
              if(ntr1.gt.1) goto 900    !should be only 1 trace
              if(i.lt.n) call adpsub_save(obj, n-i, hd(:,i+1:n), tr(:,i+1:n))
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
          return
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
      call adpsub_one_trace(obj,ntr,hd,tr)     !request data

! --- process output of adpsub, should be >0 or NO_MORE_TRACES
350   continue
      if(ntr.gt.0) then
        temp = size(tr,1)
        call lav_set_hdr (hd, tr, temp,ntr)
        return            !got output traces

      else if(ntr.eq.NO_MORE_TRACES) then
! ----- NO_MORE_TRACES received-exit if no more input, else
!         get ready for another line
        if(obj%iomode.eq.io_done_mode) then
          obj%iomode=io_error_mode             !all done, error if exec again
          return                               !return NO_MORE_TRACES
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
      return

900   continue
      ntr=FATAL_ERROR
      obj%iomode=io_error_mode
      return
      end subroutine adpsub


!-------------------------------------------------------------------
! Save ntr, hd/tr into obj%ntr_save, obj%hd_save and obj%tr_save
!  Expand size obj%hd_save and obj%tr_save as needed
!
! Written August 2003 by Charles Burch
!-------------------------------------------------------------------
      subroutine adpsub_save(obj,ntr,hd,tr)
      type(adpsub_struct),intent(inout) :: obj                   ! arguments
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

      end subroutine adpsub_save


!------------------------------------------------------------------------
! this is the original adpsub subroutine which handled one trace at a time
!  This gets feed by the new adpsub subroutine
!
! Written August 2003 by Charles Burch
!------------------------------------------------------------------------
      subroutine adpsub_one_trace (obj,ntr,hd,tr)
      type(adpsub_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

! --> Insert declarations of local variables.
      integer :: ier                                , i   
      integer :: holl(100)
      integer :: ihdr, itrc
      integer :: icable_ndx, ishot_ndx, irecv_ndx
      integer :: icable            , nrecv 
      real    :: ishot, irecv
      integer :: gtrndx, trcol, ierr
      integer :: missing_traces,missing_traces1,missing_traces2,missing_traces3





! --> Insert code for processing logic.

!     Get cable, shot and receiver numbers from trace header.
      icable = hd(obj%icable_hdloc,1)
      ishot  = hd(obj%ishot_hdloc,1)
      irecv  = hd(obj%irecv_hdloc,1)

!     If 1st trace processed, compute necessary indices.
      if(obj%trcnt .eq. 0 .and. obj%dataFlag .eq. 1) then
         nrecv = hd(obj%nrecv_hdloc,1)
         obj%icable_last = obj%icable_first
         obj%ishot_last  = obj%ishot_first + &
     &      ((obj%nshot - 1) * obj%ishot_incr)
         obj%irecv_last  = obj%irecv_first + ((obj%nrecv - 1) * obj%irecv_incr)
         obj%icable_ndx_last = obj%icable_ndx_first
         obj%ishot_ndx_last  = obj%ishot_ndx_first + obj%nshot - 1
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

!        if(icable .ne. obj%icable_next) then
         if(obj%icable_incr .gt. 1) then
           if(mod(icable-obj%icable_first, obj%icable_incr).ne.0) then
             ntr = FATAL_ERROR
             return
           endif
         end if
      end if

      if(ntr .eq. NO_MORE_TRACES) then
!        If in 'collect' mode, compute any missing traces; otherwise return.
         if(obj%mode .eq. ADPSUB_MODE_COLLECT) then
            missing_traces1 = obj%irecv_ndx_last - obj%irecv_ndx_next + 1
            missing_traces2 = obj%nrecv * &
     &         (obj%ishot_ndx_last - obj%ishot_ndx_next)
            missing_traces3 = 0
            missing_traces = missing_traces1 + missing_traces2
            goto 500
         else
            return
         end if
      end if

      if(ntr .eq. NEED_TRACES) then
         if(obj%mode .eq. ADPSUB_MODE_OUTPUT) then
            goto 2000
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

         call adpsub_initx(ier, holl, obj%memory_sizex, icable,&
     &      obj%restartx, obj%domainx, obj%nshot, obj%nrecv, obj%ioptx, &
     &      obj%s_time, obj%e_time, obj%t_win, obj%lwx,&
     &      obj%f_len, obj%f_lenx, obj%fc_max, obj%iscalex, obj%niter,&
     &      obj%ndpt, obj%dt, obj%tstrt, obj%nwihc, ADPSUB_ASUBB,&
     &      obj%ngroups, obj%groupsize0, obj%groupsize1)
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

      if(obj%restartx .eq. -1) then
         write(*,*) 'Error: Bad restart flag!'
         ntr = NO_MORE_TRACES
         return
      else if(obj%restartx .eq. 1) then
         goto 1000
      else if(obj%restartx .eq. 2) then
         goto 2000
      end if

      obj%mode = ADPSUB_MODE_COLLECT

      if(obj%group .gt. obj%ngroups) then
         ntr = NO_MORE_TRACES
         return
      end if

!     Initialize missing trace counts to zero before checking.
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

500   continue

!      if(missing_traces .gt. 0) write(*,*) 'missing traces = ',missing_traces,&
!     &   missing_traces1, missing_traces2, missing_traces3
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

!           Apply trace order.
            gtrndx = obj%gtrcnt
            obj%hd_rev(1:obj%nwihc,gtrndx) = obj%hd0(1:obj%nwihc)
            obj%tr_rev(1:obj%ndpt,gtrndx) = obj%tr0(1:obj%ndpt)

!           Pass gather of traces to C code, one at a time.
            if(obj%gtrcnt .eq. obj%nrecv) then
               do i = 1, obj%nrecv
                  trcol = obj%trcol - obj%nrecv + i

!                 Call C code data preparation for input trace.
                  call adpsub_dataprep1(ier, obj%hd_rev(1:,i),&
     &               obj%tr_rev(1:,i), trcol)
                  if(ier .ne. 0) then
                     ntr = FATAL_ERROR
                     return
                  end if
               end do
               obj%gtrcnt = 0
            end if

            if(obj%trcol .ge. obj%groupsize * obj%nrecv) then
!              Call C code data preparation data write.
               call adpsub_dataprep2(ier)
               if(ier .ne. 0) then
                  ntr = FATAL_ERROR
                  return
               end if
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
!           Call C code data preparation wrapup.
            if(obj%dataFlag .eq. 1) then
               call adpsub_dataprep3(ier)
               if(ier .ne. 0) then
                  ntr = FATAL_ERROR
                  return
               end if
               obj%dataFlag = 2
               obj%group = 1
               obj%trcol = 0
               obj%trcnt = 0
               obj%ishot_next  = obj%ishot_first
               obj%irecv_next  = obj%irecv_first
               obj%ishot_ndx_next   = 1
               obj%irecv_ndx_next   = 1
               obj%ishot_ndx_prev   = 0
               obj%irecv_ndx_prev   = 0
               if(obj%group .lt. obj%ngroups) then
                  obj%groupsize = obj%groupsize0
               else
                  obj%groupsize = obj%groupsize1
               end if
               ntr = NEED_TRACES
               return
            else
               call adpsub_dataprep5(ier)
               if(ier .ne. 0) then
                  ntr = FATAL_ERROR
                  return
               end if
               obj%stepFlag = 2
            end if
         end if
      end if
      if(obj%stepFlag .eq. 2) goto 1000

!     Increment trace count.
      obj%trcnt = obj%trcnt + 1
      obj%trcol = obj%trcol + 1

!     Collect trace and header.
      obj%hd0(1:obj%nwih) = hd(1:obj%nwih,1)
      obj%hd0(obj%nwihc) = 0.0

!     Increment group trace count.
      obj%gtrcnt = obj%gtrcnt + 1

!     Apply trace order.
      gtrndx = obj%gtrcnt
      obj%hd_rev(1:obj%nwihc,gtrndx) = obj%hd0(1:obj%nwihc)
      obj%tr_rev(1:obj%ndpt,gtrndx) = tr(1:obj%ndpt,1)

!     Pass gather of traces to C code, one at a time.
      if(obj%gtrcnt .eq. obj%nrecv) then
         do i = 1, obj%nrecv
            trcol = obj%trcol - obj%nrecv + i
!           Call C code data preparation for input trace.
            call adpsub_dataprep1(ier, obj%hd_rev(1:,i),&
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
      if(obj%trcol .lt. obj%groupsize * obj%nrecv) then
         ntr = NEED_TRACES
         return
      endif

!     Write collected trace and header.
      if(obj%dataFlag .eq. 1) then
         call adpsub_dataprep2(ier)
      else if(obj%dataFlag .eq. 2) then
         call adpsub_dataprep4(ier)
      end if
      if(ier .ne. 0) then
         ntr = FATAL_ERROR
         return
      end if

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

!     Wrap up data preparation.
      if(obj%dataFlag .eq. 1) then
         call adpsub_dataprep3(ier)
         if(ier .ne. 0) then
            ntr = FATAL_ERROR
            return
         end if
         obj%dataFlag = 2
         obj%group = 1
         obj%trcol = 0
         obj%trcnt = 0
         obj%ishot_next  = obj%ishot_first
         obj%irecv_next  = obj%irecv_first
         obj%ishot_ndx_next   = 1
         obj%irecv_ndx_next   = 1
         obj%ishot_ndx_prev   = 0
         obj%irecv_ndx_prev   = 0
         if(obj%group .lt. obj%ngroups) then
            obj%groupsize = obj%groupsize0
         else
            obj%groupsize = obj%groupsize1
         end if
         ntr = NEED_TRACES
         return
      else
         call adpsub_dataprep5(ier)
         if(ier .ne. 0) then
            ntr = FATAL_ERROR
            return
         end if
      end if
      obj%stepFlag = 2

!     Jumpstart position - subtraction.
1000  continue
      obj%mode = ADPSUB_MODE_SUBTRACT
      ier = 0
      call adpsub_subtract(ier)
      if(ier .ne. 0) then
         ntr = FATAL_ERROR
         return
      end if
      obj%stepFlag = 3

!     Jumpstart position - output.
2000  continue
      obj%mode = ADPSUB_MODE_OUTPUT
      ier = 0
      if(.not. obj%init_out) then
         obj%init_out = .true.
         obj%out_mode = 0
         obj%group = 1
         obj%gtrcnt = 0
         call adpsub_output1(ier)
         if(ier .ne. 0) then
            ntr = FATAL_ERROR
            return
         end if
         if(obj%group .lt. obj%ngroups) then
            obj%groupsize = obj%groupsize0
         else
            obj%groupsize = obj%groupsize1
         end if
      end if

      if(obj%group .gt. obj%ngroups) then
         if(obj%out_mode .eq. 0) then
            call adpsub_output3(ier)
            if(ier .ne. 0) then
               ntr = FATAL_ERROR
               return
            end if
            ntr = NO_MORE_TRACES
            return
         end if
      end if

!     NOTE: obj%ishot and obj%irecv are actually index values!!!
      if(obj%out_mode .eq. 0) then
         do i = 1, obj%nrecv
            call adpsub_output2(ier, obj%group, obj%ishot, obj%irecv,&
     &         obj%hd0(1:), obj%tr0(1:))
            if(ier .ne. 0) then
               ntr = FATAL_ERROR
               return
            end if

!           Apply trace order.
            gtrndx = obj%irecv
            obj%hd_rev(1:obj%nwihc,gtrndx) = obj%hd0(1:obj%nwihc)
            obj%tr_rev(1:obj%ndpt,gtrndx) = obj%tr0(1:obj%ndpt)

            obj%irecv = obj%irecv + 1
            if(obj%irecv .gt. obj%nrecv) then
               obj%irecv = 1
               obj%ishot = obj%ishot + 1
               if(obj%ishot .gt. obj%groupsize) then
                  obj%ishot = 1
                  obj%group = obj%group + 1
                  if(obj%group .lt. obj%ngroups) then
                     obj%groupsize = obj%groupsize0
                  else
                     obj%groupsize = obj%groupsize1
                  end if
               end if
            end if
         end do
         obj%out_mode = 1
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
         goto 2000
      end if

      if(ier .ne. 0) then
         ntr = FATAL_ERROR
         return
      end if

      end subroutine adpsub_one_trace


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine adpsub_wrapup (obj)
      type(adpsub_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

! --> Insert any required wrapup code here, including wrapups of
! --> internally-called processes.

      end subroutine adpsub_wrapup

!!---------------------------- adpsub_asubb --------------------------------!!
!!---------------------------- adpsub_asubb --------------------------------!!
!!---------------------------- adpsub_asubb --------------------------------!!
      subroutine ADPSUB_ASUBB(traces,n,ntr, ip1,iv1,idi,&
     &                 ist,nsamp,lwt,lwx,iopt,lop,lopx,&
     &                 iscale, fc_max, nit, ierr,&
     &                 dttraces,noise, worka, work)

      integer,                    intent(inout) :: n,ntr,ip1,iv1,idi,ierr,nit
      integer,                    intent(inout) :: ist,nsamp,lwt,lwx
      integer,                    intent(inout) :: iopt,lop,lopx,iscale
      real,                       intent(inout) :: fc_max
      real, dimension(n,ntr*2),   intent(inout) :: traces
      real, dimension(7*n*ntr),   intent(inout) :: worka
      real, dimension(1*n*ntr),   intent(inout) :: work
      real, dimension(nsamp,ntr), intent(inout) :: dttraces
      real, dimension(nsamp,ntr), intent(inout) :: noise

      integer               :: n1,n2,n3,w1,w2,w3,k1,k2,k3
      integer               :: jp1,jv1,jo1,itr,isamp      ,inc  
      real, dimension(5000) :: aa
      double precision      :: smult,sdata

      inc = 1
      if (lop*lopx .gt. 5000) then
         write(*,*) 'Filter size greater than 5000 - error.'
         ierr = 5000
         write(*,*) 'Error=',ierr,' returned from window.'
         return
      endif
      jp1  = ip1
      jv1  = iv1
      do itr=1,ntr
         call adpsub_vmov(traces(ist:,jp1),1,dttraces(1:,itr),1,nsamp)
         call adpsub_vmov(traces(ist:,jv1),1,noise(1:,itr),1,nsamp)

         jp1  = jp1 + idi
         jv1  = jv1 + idi
      enddo

      ! Scale the data and multiples if the user wishes.
      if (iscale .eq. 0) then
         smult = 0.0
         sdata = 0.0
         do itr=1,ntr
            do isamp=1,nsamp
               smult = smult + noise(isamp,itr)**2
               sdata = sdata + dttraces(isamp,itr)**2
            enddo
         enddo
         smult = sqrt( smult )
         sdata = sqrt( sdata )

         if (smult .eq. 0.0 .or. sdata .eq. 0.0) then
            write(*,*) 'Window skipped - smult=',smult,' sdata=',sdata,'.'
            goto 100
         endif

         do itr=1,ntr
            do isamp=1,nsamp
               noise(isamp,itr) = noise(isamp,itr) / smult
               dttraces(isamp,itr) = dttraces(isamp,itr) / sdata
            enddo
         enddo
      endif

      n1 = nsamp
      n2 = ntr
      n3 = 1
      w1 = lwt
      w2 = lwx + 2 * (lopx-1)
      w3 = 1
      k1 = 1.5 * n1/w1
      k2 = 1.5 * n2/lwx
      k3 = 1

      call adpsub_asubbwin(w1,w2,w3,dttraces,noise,n1,n2,n3,&
     &   k1,k2,k3,ierr,fc_max,lop,lopx,aa,nit,n,ntr,&
     &   worka(1:),worka(n1*n2*n3+1:), worka(2*n1*n2*n3+1:),&
     &   worka(3*n1*n2*n3+1:),worka(4*n1*n2*n3+1:),&
     &   worka(5*n1*n2*n3+1:),worka(6*n1*n2*n3+1:), work(1:))

!      if(ierr .ne. 0) then
!         write(*,*) 'Error=',ierr,' returned from window.'
!         return
!      endif

      ! Scale the data and multiples if the user wishes.
      if (iscale .eq. 0) then
         do itr=1,ntr
            do isamp=1,nsamp
               dttraces(isamp,itr) = dttraces(isamp,itr)*sdata
            enddo
         enddo
      endif

100   continue
      jp1  = ip1
      jv1  = iv1
      if(iopt .eq. 0) then
         jo1 = jv1
      else
         jo1 = jp1
      end if

      ! Subtract the signal from the input to get the noise panel.
      do itr=1,ntr

         ! First copy input into noise panel.
         call adpsub_vmov(traces(ist:,jp1),1,traces(ist:,jv1),1,nsamp)

         ! Start the updated traces at lop to avoid the zeros
         ! at the top and bottom of the window.
         call adpsub_vmov(dttraces(lop:,itr),1,traces(ist+lop-1:,jp1),1,&
     &      nsamp-2*lop+2)

         call adpsub_vsub(traces(ist:,jv1),1,traces(ist:,jp1),1,&
     &                    traces(ist:,jo1),1,nsamp);
         jp1  = jp1 + idi
         jv1  = jv1 + idi
         jo1  = jo1 + idi
      enddo

      return
      end subroutine ADPSUB_ASUBB


!!--------------------------- adpsub_asubbwin ------------------------------!!
!!--------------------------- adpsub_asubbwin ------------------------------!!
!!--------------------------- adpsub_asubbwin ------------------------------!!
!
!  chop the volume into subvolumes and process each for information on
!  these routines, see the book
!  "three-dimensional filtering: environmental sounding image enhancement"
!  by jon claerbout.
!
      subroutine adpsub_asubbwin(w1,w2,w3,dattrc,noise,n1,n2,n3,&
     &   k1,k2,k3,ierr,fc_max,&
     &   lop,lopx,aa,nit,n,ntr,&
     &   resi, pdattrc, pnoise,&
     &   rdattrc,rnoise,&
     &   windwt,wallwt, work)

      integer,                     intent(inout) :: w1,w2,w3,n1,n2,n3,k1,k2,k3
      integer,                     intent(inout) :: lop,lopx,nit,ierr,n,ntr
      real,                        intent(inout) :: fc_max
      real, dimension(lop, lopx),  intent(inout) :: aa
      real, dimension(n1, n2, n3), intent(inout) :: resi
      real, dimension(n1, n2, n3), intent(inout) :: dattrc
      real, dimension(w1, w2, w3), intent(inout) :: pdattrc
      real, dimension(w1, w2, w3), intent(inout) :: rdattrc
      real, dimension(n1, n2, n3), intent(inout) :: noise
      real, dimension(w1, w2, w3), intent(inout) :: pnoise
      real, dimension(w1, w2, w3), intent(inout) :: rnoise
      real, dimension(w1, w2, w3), intent(inout) :: windwt
      real, dimension(n1, n2, n3), intent(inout) :: wallwt
      real, dimension(n*ntr),      intent(inout) :: work

      integer :: i1,i2,i3, j1,j2,j3   

      integer :: an1, an2, lag1, lag2



      an1 = lop
      an2 = lopx
      lag1 = (an1/2)+1
      lag2 = 1

      ! Make the wall weights.
      call adpsub_tent2(an1,an2, lag1,lag2, windwt, w1,w2)

      do i3= 1, n3
         do i2= 1, n2
            do i1= 1, n1
               wallwt(i1,i2,i3) = 0.0
            enddo
         enddo
      enddo

      do i3= 1, k3
         do i2= 1, k2
            do i1= 1, k1
               call adpsub_patch3(1,1, i1,i2,i3, k1,k2,k3, wallwt, n1,n2,n3,&
     &                            windwt, w1,w2,w3)
            enddo
         enddo
      enddo

      do i3= 1, n3
         do i2= 1, n2
            do i1= 1, n1
               if(wallwt(i1,i2,i3) .ne. 0.) then
                  wallwt(i1,i2,i3)  = 1./wallwt(i1,i2,i3)
               endif
            enddo
         enddo
      enddo

      do i3= 1, n3
         do i2= 1, n2
            do i1= 1, n1
               resi(i1,i2,i3) = 0.0
            enddo
         enddo
      enddo

      do i3= 1, k3
         do i2= 1, k2
            do i1= 1, k1

               call adpsub_patch3(0,0, i1,i2,i3, k1,k2,k3, dattrc, n1,n2,n3,&
     &                            pdattrc, w1,w2,w3)

               call adpsub_patch3(0,0, i1,i2,i3, k1,k2,k3, noise, n1,n2,n3,&
     &                            pnoise, w1,w2,w3)

               ! This is the normal case.
               if(i2 .ne. 1) then
                  call adpsub_separt(pdattrc,pnoise,w1,w2,n,ntr,&
     &                               aa,an1,an2,lag1,lag2,&
     &                               ierr,work,fc_max,nit)
                  if (ierr .ne. 0) then
                     write(*,*) 'Error =',ierr,' returned from separt.'
                     return
                  endif

               ! For the first window in space, predict the other way.
               else
                  do j3= 1, w3
                     do j2= 1, w2
                        do j1= 1, w1
                           rdattrc(j1,j2,j3) = pdattrc(w1-j1+1,w2-j2+1,j3)
                           rnoise(j1,j2,j3)  =  pnoise (w1-j1+1,w2-j2+1,j3)
                        enddo
                     enddo
                  enddo
                  ! Match filter the reversed data.
                  call adpsub_separt(rdattrc,rnoise,w1,w2,n,ntr,&
     &                            aa,an1,an2,lag1,lag2,&
     &                            ierr,work,fc_max,nit)
                  if(ierr .ne. 0) then
                     write(*,*) 'Error =',ierr,' returned from separt-2.'
                     return
                  endif

                  do j3= 1, w3
                     do j2= 1, w2
                        do j1= 1, w1
                           pdattrc(j1,j2,j3) = rdattrc(w1-j1+1,w2-j2+1,j3)
                        enddo
                     enddo
                  enddo
               endif

               ! Apply window weights.
               do j3=1,w3
                  do j2=1,w2
                     do j1=1,w1
                        pdattrc(j1,j2,j3) = pdattrc(j1,j2,j3)*windwt(j1,j2,j3)
                     enddo
                  enddo
               enddo

               call adpsub_patch3(1,1, i1,i2,i3, k1,k2,k3, resi, n1,n2,n3,&
     &                            pdattrc, w1,w2,w3)
            enddo
         enddo
      enddo

      ! Apply wall weights.
      do j3=1,n3
         do j2=1,n2
            do j1=1,n1
               dattrc(j1,j2,j3) = wallwt(j1,j2,j3)*resi(j1,j2,j3)
            enddo
         enddo
      enddo

      return
      end subroutine adpsub_asubbwin


!!----------------------------- adpsub_tent2 -------------------------------!!
!!----------------------------- adpsub_tent2 -------------------------------!!
!!----------------------------- adpsub_tent2 -------------------------------!!
      subroutine adpsub_tent2(a1,a2, lag1,lag2, windwt, w1,w2)

      integer,                intent(inout) :: a1, a2, lag1, lag2, w1, w2
      real, dimension(w1,w2), intent(inout) :: windwt

      integer :: i1,i2, s1,s2, e1,e2
      real    :: mid1,mid2, wide1,wide2, x,y
      real    :: rleast

      rleast = 0.001

      do i2=1,w2
         do i1=1,w1
            windwt(i1,i2) = 0.0
         enddo
      enddo

      s1= 1+a1-lag1
      e1= 1+w1-lag1
      mid1=(e1+s1)/2.
      wide1=(e1-s1+1.)/2.
      s2= 1
      e2= w2
      mid2=(e2+s2)/2.
      wide2=(e2-s2+1.)/2.

      do i2= s2, e2
         y = abs((i2-mid2)/wide2)
         do i1= s1, e1
            x = abs((i1-mid1)/wide1)
            windwt(i1,i2) = amax1(rleast, 1.-abs(x))&
     &                  * amax1(rleast, 1.-abs(y))
         enddo
      enddo

      do i2= 1, a2
         do i1= s1, e1
            windwt(i1,i2) = windwt(i1,i2) * 0.01
         enddo
      enddo

      return
      end subroutine adpsub_tent2


!!---------------------------- adpsub_patch3 -------------------------------!!
!!---------------------------- adpsub_patch3 -------------------------------!!
!!---------------------------- adpsub_patch3 -------------------------------!!
      subroutine adpsub_patch3(adj,add, j1,j2,j3, k1,k2,k3, wall, n1,n2,n3,&
     &                         wind, w1,w2,w3)

      integer,                   intent(in)    :: adj,add
      integer,                   intent(inout) :: j1,j2,j3, k1, k2, k3
      integer,                   intent(inout) :: n1,n2,n3, w1, w2, w3
      real, dimension(n1,n2,n3), intent(inout) :: wall
      real, dimension(w1,w2,n3), intent(inout) :: wind

      integer :: i1,i2,i3, s1,s2,s3, a1,a2,a3

      call adpsub_adjnull(adj,add, wall, n1*n2*n3, wind, w1*w2*w3)

      if(k3.eq.1) then
         s3=1
      else
         s3=1.5 + (n3 - w3) * (j3-1.)/(k3-1.)
      endif

      if(k2.eq.1) then
         s2=1
      else
         s2=1.5 + (n2 - w2) * (j2-1.)/(k2-1.)
      endif

      if(k1.eq.1) then
         s1=1
      else
         s1=1.5 + (n1 - w1) * (j1-1.)/(k1-1.)
      endif

      if(adj .eq. 0) then
         do i3= 1, w3
            a3= i3 + s3 - 1
            do i2= 1, w2
               a2= i2 + s2 - 1
               do i1= 1, w1
                  a1= i1 + s1 - 1
                  wind(i1,i2,i3) = wind(i1,i2,i3) + wall(a1,a2,a3)
               enddo
            enddo
         enddo
      else
         do i3= 1, w3
            a3= i3 + s3 - 1
            do i2= 1, w2
               a2= i2 + s2 - 1
               do i1= 1, w1
                  a1= i1 + s1 - 1
                  wall(a1,a2,a3) = wall(a1,a2,a3) + wind(i1,i2,i3)
               enddo
            enddo
         enddo
      endif

      return
      end subroutine adpsub_patch3


!!---------------------------- adpsub_adjnull -------------------------------!!
!!---------------------------- adpsub_adjnull -------------------------------!!
!!---------------------------- adpsub_adjnull -------------------------------!!
      subroutine adpsub_adjnull(adj, add, x, nx, y, ny)

      integer,             intent(in) :: adj, add, nx, ny
      real, dimension(nx), intent(inout) :: x
      real, dimension(ny), intent(inout) :: y

      integer :: ix, iy

      if(add .eq. 0) then
         if(adj .eq. 0) then
            do iy= 1, ny
               y(iy) = 0.
            enddo
         else
            do ix= 1, nx
               x(ix) = 0.
            enddo
         endif
      endif

      return
      end subroutine adpsub_adjnull


!!---------------------------- adpsub_separt --------------------------------!!
!!---------------------------- adpsub_separt --------------------------------!!
!!---------------------------- adpsub_separt --------------------------------!!
      subroutine adpsub_separt(signal,noise,n1,n2,n,ntr,an,an1,an2,lag1,lag2,&
     &                         ierr,work,fc_max,nit)

      integer, intent(inout) :: n1, n2, an1, an2, ierr
      integer, intent(inout) :: nit, lag1, lag2, n, ntr
      real,    intent(inout) :: fc_max

      real, dimension(n1,n2),   intent(inout) :: signal
      real, dimension(n1,n2),   intent(inout) :: noise
      real, dimension(an1,an2), intent(inout) :: an
      real, dimension(n*ntr),   intent(inout) :: work

      integer                 :: i, j, niterpef
      real                    :: randum, temp, sfaca,sfac
      real, dimension(4000,2) :: filtwrk
      double precision        :: temp2

      ierr = 0
      if(an1*an2 .gt. 4000) then
         write(*,*) 'SEPART: Filter buffer too small.'
         ierr = 4000
         return
      endif

      temp2 = 0.0
      do i=1,n2
         do j=1,n1
            temp2 = temp2 + noise(j,i)*noise(j,i)
         enddo
      enddo
      temp = temp2
      sfaca = sqrt(temp/(n1*n2))

      if(sfaca .ne. 0.0) then
         sfac = sfaca

         do i=1,n2
            do j=1,n1
               call adpsub_randm(randum)
               noise(j,i)  = noise(j,i) /sfac + 0.0001*randum
               signal(j,i) = signal(j,i)/sfac + 0.0001*randum
            enddo
         enddo

         niterpef = an1*an2
         if(nit .gt. 0) niterpef = nit

         call adpsub_shap2d(noise,signal, work(1:),n1,n2, niterpef, &
     &      lag1,lag2, an,an1,an2, filtwrk(1:,1), filtwrk(1:,2), &
     &      work(1+n1*n2:), work(1+n1*n2*2:), work(1+n1*n2*3:), &
     &      fc_max, ierr)
         if(ierr .ne. 0) then
            write(*,*) 'Error=',ierr,' returned from shap2d.'
            return
         endif

         do j=1,n2
            do i=1,n1
               if(noise(i,j) .ne. 0.0) then
                  signal(i,j) = signal(i,j) - noise(i,j)
               else
                  signal(i,j) = 0.0
               endif
            enddo
         enddo

         do j=1,n2
            do i=1,n1
               signal(i,j) = signal(i,j) * sfac
            enddo
         enddo
      endif

      return
      end subroutine adpsub_separt


!!---------------------------- adpsub_shap2d --------------------------------!!
!!---------------------------- adpsub_shap2d --------------------------------!!
!!---------------------------- adpsub_shap2d --------------------------------!!
      subroutine adpsub_shap2d(noise,signal,rr,n1,n2, niter, lag1,lag2,&
     &                         aa,a1,a2, da,sa, dr,sr,wr, fc_max, ierr)

      integer, intent(inout) :: n1,n2, niter, lag1,lag2, a1,a2, ierr
      real,    intent(inout) :: fc_max

      real, dimension(a1,a2),   intent(inout) :: aa
      real, dimension(a1,a2),   intent(inout) :: da
      real, dimension(a1,a2),   intent(inout) :: sa
      real, dimension(n1*n2),   intent(inout) :: dr
      real, dimension(n1*n2),   intent(inout) :: sr
      real, dimension(n1*n2),   intent(inout) :: wr
      real, dimension(n1*n2),   intent(inout) :: signal
      real, dimension(n1*n2),   intent(inout) :: noise
      real, dimension(n1*n2*2), intent(inout) :: rr

      integer ::        a12, n12, iter, i,j  
      real    :: scale

      a12= a1*a2
      n12= n1*n2
      do i=1,n12
         rr(i) = 0.0
         wr(i) = -signal(i)
      enddo

      do j=1,a2
         do i=1,a1
            aa(i,j) = 0.0
         enddo
      enddo

      do iter= 0, niter
         call adpsub_icaf2(1, 0,lag1,lag2, noise,n1,n2, da,a1,a2, wr)
         call adpsub_icaf2(0, 0,lag1,lag2, noise,n1,n2, da,a1,a2, dr)

         call adpsub_cgplus(iter, a12 , aa,da,sa,n12, wr,dr,sr ,ierr)
         if(ierr .ne. 0) then
            write(*,*) 'Error=',ierr,' returned from cgplus.'
            return
         endif
      enddo

      if(fc_max .gt. 0) then
         scale = 0.0
         do j=1,a2
            do i=1,a1
               scale = amax1(abs(aa(i,j)) ,scale)
            enddo
         enddo

         if(scale .gt. fc_max) then
            scale = fc_max/scale
            do j=1,a2
               do i=1,a1
                  aa(i,j) = aa(i,j)*scale
               enddo
            enddo
         endif
      endif

      call adpsub_icaf2(0, 0,lag1,lag2, noise,n1,n2, aa,a1,a2, rr)
      do i=1,n12
         noise(i) = rr(i)
      enddo

      return
      end subroutine adpsub_shap2d


!!---------------------------- adpsub_cgplus --------------------------------!!
!!---------------------------- adpsub_cgplus --------------------------------!!
!!---------------------------- adpsub_cgplus --------------------------------!!
      subroutine adpsub_cgplus(iter, n, x, g, s, m, rr, gg, ss, ierr)

      integer,            intent(inout) :: iter, n, m, ierr
      real, dimension(n), intent(inout) :: x, g, s
      real, dimension(m), intent(inout) :: rr, gg, ss
      integer          :: i
      double precision :: sds, gdg, gds, determ
      double precision :: gdr, sdr, alfa, beta


      gdg = adpsub_ddot2(m,gg,gg)
      gdr = -adpsub_ddot2(m,gg,rr)

      if(iter .lt. 1) then
         do i= 1, n
            s(i) = 0.
         enddo

         do i= 1, m
            ss(i) = 0.
         enddo

         if(gdg.eq.0) then
            ierr = 9998
            write(*,*) 'CGPLUS: First grad vanishes identically.'
            return
         endif

         alfa = gdr/gdg
         beta = 0.

      else
         if(gdg.eq.0.) then
            ierr = 9997
            write(*,*) 'CGPLUS: First grad vanishes identically.'
            return
         endif

         sds = adpsub_ddot2(m,ss,ss)
         if(sds.eq.0.) then
            alfa = gdr/gdg
            beta = 0.
         else
            gds = adpsub_ddot2(m,gg,ss)
            sdr = - adpsub_ddot2(m,ss,rr)
            determ = gdg*sds*dmax1(1.d0-(gds/gdg)*(gds/sds), 1.0d-12)
            alfa = ( sds*gdr - gds*sdr)/determ
            beta = (-gds*gdr + gdg*sdr)/determ
         endif
      endif

      do i= 1, n
         s(i) = alfa*g(i) + beta*s(i)
      enddo
      do i= 1, m
         ss(i) = alfa*gg(i) + beta*ss(i)
      enddo
      do i= 1, n
         x(i) = x(i) + s(i)
      enddo
      do i= 1, m
         rr(i) = rr(i) + ss(i)
      enddo

      return
      end subroutine adpsub_cgplus


!!---------------------------- adpsub_ddot2 ---------------------------------!!
!!---------------------------- adpsub_ddot2 ---------------------------------!!
!!---------------------------- adpsub_ddot2 ---------------------------------!!
      double precision function adpsub_ddot2(n, x, y)
      integer,            intent(inout) :: n
      real, dimension(n), intent(inout) :: x, y

      integer          :: i
      double precision :: val

      val = 0.
      do i=1,n
         val = val + x(i) * y(i)
      enddo
         adpsub_ddot2 = val
      return

      end function adpsub_ddot2


!!---------------------------- adpsub_icaf2 ---------------------------------!!
!!---------------------------- adpsub_icaf2 ---------------------------------!!
!!---------------------------- adpsub_icaf2 ---------------------------------!!
      subroutine adpsub_icaf2( adj,add, lag1,lag2, xx,n1,n2, aa,na1,na2, yy)

      integer,                  intent(in)    :: adj, add, lag1, lag2
      integer,                  intent(inout) :: n1, n2, na1, na2
      real, dimension(n1,n2),   intent(inout) :: xx
      real, dimension(na1,na2), intent(inout) :: aa
      real, dimension(n1,n2),   intent(inout) :: yy

      integer x1,x2, a1, a2, y1,y2, ll

      call adpsub_adjnull(adj, add, aa, na1*na2, yy, n1*n2)

      if(adj .eq. 0 ) then
         do a2=1,na2
            do y2= 1+na2-lag2, n2-lag2+1
               x2= y2 - a2 + lag2
               do a1=1,na1
                  ll = - a1 + lag1
                  do y1= 1+na1-lag1, n1-lag1+1
                     x1= y1 +ll
                     yy( y1,y2) = yy( y1,y2) + aa( a1,a2) * xx( x1,x2)
                  enddo
               enddo
            enddo
         enddo
      else
         do a2=1,na2
            do y2= 1+na2-lag2, n2-lag2+1
               x2= y2 - a2 + lag2
               do a1=1,na1
                  ll = - a1 + lag1
                  do y1= 1+na1-lag1, n1-lag1+1
                     x1= y1 + ll
                     aa( a1,a2) = aa( a1,a2) + yy( y1,y2) * xx( x1,x2)
                  enddo
               enddo
            enddo
         enddo
      endif

      return
      end subroutine adpsub_icaf2


!!---------------------------- adpsub_randm ---------------------------------!!
!!---------------------------- adpsub_randm ---------------------------------!!
!!---------------------------- adpsub_randm ---------------------------------!!
      subroutine adpsub_randm(randum)

      real, intent(inout) :: randum
      integer a,m,q,r,lo,hi,test

      integer seed
      data seed /1/
      save seed

      a = 16807
      m = 2147483647
      q = 127773
      r = 2836
      hi = seed/q
      lo = mod(seed,q)
      test = a*lo - r*hi

      if(test .gt. 0 ) then
         seed = test
      else
         seed = test + m
      endif

      randum = float(seed)/float(m)

      return
      end subroutine adpsub_randm


!!---------------------------- adpsub_vmov ---------------------------------!!
!!---------------------------- adpsub_vmov ---------------------------------!!
!!---------------------------- adpsub_vmov ---------------------------------!!
      subroutine adpsub_vmov(work1, inc1, work2, inc2, nsamp)
      implicit none

      integer,                     intent(in) :: inc1, inc2, nsamp
      real, dimension(nsamp*inc1), intent(inout) :: work1
      real, dimension(nsamp*inc2), intent(inout) :: work2

      integer i,j,k

      do i=1, nsamp
        j = (i-1)*inc1 + 1
        k = (i-1)*inc2 + 1
        work2(k) = work1(j)
      enddo

      return
      end subroutine adpsub_vmov


!!---------------------------- adpsub_vsub ---------------------------------!!
!!---------------------------- adpsub_vsub ---------------------------------!!
!!---------------------------- adpsub_vsub ---------------------------------!!
      subroutine adpsub_vsub(work1, inc1, work2, inc2, work3, inc3, nsamp)

      integer,                     intent(in) :: inc1, inc2, inc3, nsamp
      real, dimension(nsamp*inc1), intent(inout) :: work1
      real, dimension(nsamp*inc2), intent(inout) :: work2
      real, dimension(nsamp*inc3), intent(inout) :: work3

      integer i,j,k,m

      do i=1, nsamp
        j = (i-1)*inc1 + 1
        k = (i-1)*inc2 + 1
        m = (i-1)*inc3 + 1
        work3(m) = work1(j) - work2(k)
      enddo

      return
      end subroutine adpsub_vsub

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module adpsub_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

