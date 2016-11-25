!<CPS_v1 type="PROCESS"/>
!!------------------------------- trmo.f90 ---------------------------------!!
!!------------------------------- trmo.f90 ---------------------------------!!
!!------------------------------- trmo.f90 ---------------------------------!!


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
! Name       : TRMO
! Category   : velocity_analysis
! Written    : 2003-07-17   by: Michael Ried
! Revised    : 2007-11-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Residual moveout
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Residual moveout on reflected events after pre-stack depth migration is
! present when the migration velocity is not correct.  For horizontally layered
! media, the residual moveout (RMO) for an event can be described by a
! hyperbolic formula that expresses migration depth (Z) as a function of
! offset (H).
!
! Z**2  = Zo**2 + Beta*(H**2)/4
!
! Zo is the depth of the migrated event at zero offset.
!
! The Beta values, which are calculated by ABRA (Automated Blackbox RMO
! Analysis) and massaged by KA (Kombo Analysis), are used by TRMO (Two-term
! Residual Moveout) to "flatten" the events on the CIG gathers.   The output
! from TRMO may be used to QC the Beta values before they are used for
! tomographic inversion (TOMOCOP).  The output from TRMO may also be use to
! improve event flatness before stack and AVO analysis - this step may be
! especially helpful after the last pass of pre-stack depth migration.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! Applying a very mild smoothing filter with KA to the Beta field may improve
! moveout results (TRMO) and will reduce the stretching and compression of the
! seismic wavelet on the far offsets.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Beta Trace
! Velocity Trace
! Seismic Traces
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! ABRA traces and Seismic Traces (If option selected)
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
!    3    HDR_CURRENT_GROUP          Input
!    6    HDR_OFFSET                 Input into Calculations
!   49    HDR_USER_49                Input/Output trace type **
!                                    (Hwd#49 is the default header word #)
!
! **(Trace Types: 0=Dead trace,1=Seismic trace,51=Input Average Velocity trace,
!    81=Beta trace, 181=Beta trace #2)
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
!  3. 2007-11-29  Stoeckley     Remove unused reference to memman, and remove
!                                unused variables.
!  2. 2005-01-10  Michael Ried  New Changes to TRMO:
!                               -User can change Header word # for trace type
!                               -It now runs as a parallel job
!                               -Beta trace type can be 181 or 81
!                               -a new CDP does not have to increment by 1
!  1. 2003-07-17  Michael Ried  Initial version.
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
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.
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
!<NS trmo Process/NC=80>
!                       TRMO - 2-term Residual Moveout
!`---------------------------------------------------------------------------
!      Header word number for trace type[/R]  TTYP_HNO=`FFFFFFFFFFFFFFFFFFFFF
!      Direction for moveout application[/R]    DIRECT=`CCCCCCCCCCCCCCCCCCCCC
!                Stretch mute percentage[/R]  STR_MUTE=`FFFFFFFFFFFFFFFFFFFFF
! Apply remaining static during moveout?[/R]     LSTAT=`KKK
!`---------------------------------------------------------------------------
!`---------------------------------------------------------------------------
!      Select the type of data to output[/R]    OUTOPT=`CCCCCCCCCCCCCCCCCCCCC
!`---------------------------------------------------------------------------
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="DIRECT">
!<Tip> -->Choose a direction for the moveout application.</Tip>
! Default = --> Forward
! Allowed = --> Inverse
! --> Forward - Apply 3-term moveout...this is the normal application mode.
! --> Inverse - Back out (remove) previously applied 3-term moveout.
! --> Normally use FORWARD, but you can also remove (Inverse) previously
!     applied 3-term moveout.
!</Help>
!
!<Help KEYWORD="LSTAT">
!<Tip> -->Choose whether to apply any remaining static during moveout.</Tip>
! Default = --> YES
! Allowed = --> YES
! Allowed = --> NO
! --> If 'Yes', accumulated NA_STAT header word statics (such as the floating
!     datum static) will be applied during 3-term moveout.
!</Help>
!
!<Help KEYWORD="OUTOPT">
!<Tip> -->Choose the type of data to output</Tip>
! Default = --> SEISMIC TRACES ONLY
! Allowed = --> SEISMIC TRACES ONLY
! Allowed = --> ABRA AND SEISMIC TRCS
! --> Select the type of data to output
!</Help>
!
!<Help KEYWORD="STR_MUTE">
!<Tip> --> Enter a stretch mute percentage</Tip>
! Default = --> 200.0
! Allowed = --> Real
! --> Any sample strectched more than this perectage will be automatically
!     muted (enter 0.0 to disable)
!</Help>
!
!<Help KEYWORD="TTYP_HNO">
!<Tip> -->Enter the header word number for the trace type</Tip>
! Default = --> 49
! Allowed = --> A Header word number
! --> Header word number for the trace type
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module trmo_module
      use pc_module
      use named_constants_module
      use mem_module
      use grid_module            ! if you need the grid transformation.
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.

! --> Insert here any other modules used by this process.

      implicit none
      private
      public :: trmo_create
      public :: trmo_initialize
      public :: trmo_update
      public :: trmo_delete
      public :: trmo            ! main trace processing routine.
      public :: trmo_wrapup

      character(len=100),public,save :: TRMO_IDENT = &
'$Id: trmo.f90,v 1.3 2007/11/30 13:55:19 Stoeckley beta sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: trmo_struct

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

        character(len=21)          :: direct
        logical                    :: lstat
        character(len=21)          :: outopt
        real                       :: str_mute
        real                       :: ttyp_hno
        integer                    :: initfg
        integer                    :: inmo_dir
        integer                    :: istat
        integer                    :: ioutopt
        integer                    :: num_ways

        integer                    :: ix_beta
        integer                    :: ix_tmt
        integer                    :: ix_work
        integer                    :: ix_work2

! --> Insert any other needed variables or pointers here.

        real              ,pointer :: wa1(:)  ! work array

! --> Insert any other needed variables or pointers here.

      end type trmo_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


! --> Include any required interfaces here.


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(trmo_struct),pointer,save :: object    ! needed for traps.

! --> Insert here any data declarations needed by this process.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine trmo_create (obj)
      type(trmo_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in trmo_create")


! --> Nullify any additional pointers in the OBJ data structure here.
      nullify(obj%wa1)   ! must be done for all pointers.

      call trmo_initialize (obj)
      end subroutine trmo_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine trmo_delete (obj)
      type(trmo_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      call trmo_wrapup (obj)


! --> Deallocate any additional pointers in the OBJ data structure here.
      if (associated(obj%wa1)) deallocate(obj%wa1)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in trmo_delete")
      end subroutine trmo_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine trmo_initialize (obj)
      type(trmo_struct),intent(inout) :: obj       ! arguments

! --> Change the default values below as needed

      obj%direct       = 'Forward'
      obj%lstat        = .TRUE.
      obj%outopt       = 'Seismic traces only  '
      obj%str_mute     = 200.0
      obj%ttyp_hno     = 49.0
      obj%inmo_dir     = 1
      obj%istat        = 1
      obj%ioutopt      = 1

      call trmo_update (obj)
      end subroutine trmo_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine trmo_update (obj)
      type(trmo_struct),intent(inout),target :: obj             ! arguments

! --> Insert code to declare all required local variables.
      integer istat, inmo_dir
      integer lun, lwa1, max_samps

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!



! --> Delete any of the globals below that are not needed:

      obj%ipn = pc_get_ipn()

      call pc_get_global ('numtr'   , obj%numtr)
      call pc_get_global ('gathered', obj%gathered)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)
      call pc_get_global ('grid'    , obj%grid)

      call pc_get('DIRECT  ', obj%direct)
      call pc_get('LSTAT   ', obj%lstat)
      call pc_get('OUTOPT  ', obj%outopt)
      call pc_get('STR_MUTE', obj%str_mute)
      call pc_get('TTYP_HNO', obj%ttyp_hno)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!
! ..... Change options into integer values
!
      IF (OBJ%DIRECT.EQ.'Inverse             ') THEN
        OBJ%INMO_DIR=-1
      ELSE
        OBJ%INMO_DIR=1
      END IF
!
      IF (obj%LSTAT) THEN
        OBJ%ISTAT=1
      ELSE
        OBJ%ISTAT=0
      END IF
!
      IF (OBJ%OUTOPT.EQ.'Abra and Seismic trcs') THEN
        OBJ%IOUTOPT=2
      ELSE
        OBJ%IOUTOPT=1
      END IF
!
!      Initialize needed variables
!
      INMO_DIR = OBJ%INMO_DIR
      ISTAT = OBJ%ISTAT
!
      OBJ%INITFG=1
!
! ..... Change options into integer values
!
! ..... Set the flag for a 2-way NMO correction
      OBJ%NUM_WAYS = 2

! ..... See if the user wants to apply the static-not-yet-applied
      IF (INMO_DIR.EQ.1) THEN
        IF (ISTAT.EQ.1) THEN
          CALL PC_WARNING("Applying the remaining static NA_STAT during NMO")
        END IF
      END IF
! --> Insert code to verify process parameters here (and/or in traps).


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


! --> Insert code to call internal processes to verify process parameters.


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field('DIRECT ', (/'Forward              ',         &
        'Inverse              '/) )
      call pc_put_options_field('OUTOPT ', (/'Seismic traces only  ',         &
        'Abra and Seismic trcs'/) )

! --> Delete any of the globals below that have not changed:

      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('gathered', obj%gathered)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)
      call pc_put_global ('grid'    , obj%grid)

      call pc_put('DIRECT  ', obj%direct)
      call pc_put('LSTAT   ', obj%lstat)
      call pc_put('OUTOPT  ', obj%outopt)
      call pc_put('STR_MUTE', obj%str_mute)
      call pc_put('TTYP_HNO', obj%ttyp_hno)

! --> Change the control defaults below as appropriate:

      call pc_put_control ('ntapes'       , 0)
      call pc_put_control ('need_request' , .false.)
      call pc_put_control ('need_label'   , .false.)
      call pc_put_control ('twosets'      , .false.)
      call pc_put_control ('nscratch'     , 0)
      call pc_put_control ('nstore'       , 0)
      call pc_put_control ('iftd'         , .false.)
      call pc_put_control ('ndisk'        , 0)
      call pc_put_control ('setup_only'   , .false.)
!
!      The following will cause this process not to be parrallel
!
      call pc_put_control ('parallel_safe', .false.)
!
!      The following will cause this process to be parrallel
!
!     call pc_put_control ('parallel_safe'        , .true.)
!     call pc_put_control ('pcps_send_mode'       ,'pcps_send_first_avail')
!     call pc_put_control ('pcps_receive_mode'    ,'pcps_receive_passthru')
!     call pc_put_control ('pcps_bunch_mode'      ,'pcps_bunch_trace_groups')
!     call pc_put_control ('pcps_send_eof_mode'   ,'pcps_send_all_eof')
!     call pc_put_control ('pcps_alt_send_mode'   ,'pcps_send_all')
!     call pc_put_control ('pcps_alt_receive_mode','pcps_receive_all_eof')
!     call pc_put_control ('pcps_resequence_mode' ,'pcps_resequence_traces')
!     call pc_put_control ('pcps_generator_mode'  ,'pcps_no_trace_gen')
!
! --> Add here any other parameter cache calls such as to set sensitivities.


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


! --> Insert code to initialize variables for execution or deallocate arrays
! --> which will be reallocated below.

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

! --> Insert code to allocate needed permanent memory.

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

! --> Insert code to initialize anything needed for actual execution of process.
      MAX_SAMPS = OBJ%NDPT
!
!      INITIALIZE FOR OPENING FILES
!
      LUN=6
!
! --> Insert code to allocate needed permanent memory.
!
!     Set up the work array     
!
      LWA1 = 0
!
!       Now Increase the size of the work buffer
!
! ..... IX_BETA  :  BETA values
      OBJ%IX_BETA = LWA1 + 1
      LWA1 = LWA1 + MAX_SAMPS
! ..... IX_TMT  :  Time trace buffer
      OBJ%IX_TMT = LWA1 + 1
      LWA1 = LWA1 + MAX_SAMPS
! ..... IX_WORK  :  A work array
      OBJ%IX_WORK = LWA1 + 1
      LWA1 = LWA1 + MAX_SAMPS
! ..... IX_WORK2  :  A 2nd work array
      OBJ%IX_WORK2 = LWA1 + 1
      LWA1 = LWA1 + MAX_SAMPS
!
!   Allocate your permanent memory like this:
!   Allocation errors will be reported to the parameter cache.
!  
      IF (LWA1.GT.0) THEN
        CALL MEM_ALLOC (OBJ%WA1, LWA1)
      END IF


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine trmo_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


! --> Insert code for parameter traps here.
! --> (EZCPS with the -t option will insert skeleton code for you)


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine trmo (obj,ntr,hd,tr)
      type(trmo_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

! --> Insert declarations of local variables.
!
      INTEGER CMP_IN, TRACE_TYPE
      INTEGER I1, IBETATR, IBTRC2, IDEAD, IE2, IERROR, INITFG, INMO_DIR
      INTEGER IOUTOPT, ISTRC, IT, ittyp_hno, IVTRC
      INTEGER IX_BETA, IX_TMT, IX_WORK, IX_WORK2, IT1
      INTEGER JERROR, LAST_CDP, MAX_SAMPS
      INTEGER NHDRS, NTRC_GATH_IN, NTRC_GATH_OUT, NUM_WAYS, NV
      REAL OFFST, SAMP_INT_IN, SOURCE_DETECT_DIST_IN, STATIC, STR_MUTE
!
      SAVE :: LAST_CDP
!
      DATA  IDEAD   /  0/
      DATA  ISTRC   /  1/
      DATA  IVTRC   / 51/
      DATA  IBETATR / 81/
      DATA  IBTRC2  /181/
      DATA  LAST_CDP/  0/

! --> Insert code for processing logic.
!
!     The following conditional call to the WRAPUP routine should be made
!     before returning from this subroutine:
!
      IF (NTR == NO_MORE_TRACES .OR. NTR == FATAL_ERROR) THEN
        CALL TRMO_WRAPUP(OBJ)
        RETURN
      END IF
!
!      set pointers into the work array
!
      ix_beta=obj%ix_beta
      ix_tmt=obj%ix_tmt
      ix_work=obj%ix_work
      ix_work2=obj%ix_work2
!
!      INITIALIZE
!      (obj%ndpt is number of sample values in a trace)
!      (obj%nwih is number of header words)
!      (obj%numtr is max number of traces input/output)
!      (obj%dt is trace sample interval)
!
      MAX_SAMPS=OBJ%NDPT
      NHDRS=OBJ%NWIH
      NTRC_GATH_IN=NTR
      SAMP_INT_IN=OBJ%DT*1000.0
!
      NUM_WAYS=OBJ%NUM_WAYS
      INITFG=OBJ%INITFG
      INMO_DIR=OBJ%INMO_DIR
      IOUTOPT=OBJ%IOUTOPT
      STR_MUTE=OBJ%STR_MUTE
      ITTYP_HNO=OBJ%TTYP_HNO
!
!      determine the trace type header number
!
      IF (ITTYP_HNO.LT.1 .OR. ITTYP_HNO.GT.NHDRS) THEN
        CALL PC_ERROR("Invalid header number for the trace type")
        RETURN
      END IF
!
!      initialize header values
!
      NTRC_GATH_IN=NTR
      DO I1=1,NTRC_GATH_IN
        CMP_IN = HD(HDR_CURRENT_GROUP,I1)
        IF (CMP_IN.EQ.(LAST_CDP+1)) GOTO 10
      END DO
!
!      DETERMINE IF DATA IS STACKED
!
!     IF (DATA_DESC_IN .EQ. 'STACK') THEN
!       IERROR = 270
!       CALL EMSTD ( VERSION, RNAME, MSGNAME, IERROR )
!     END IF
!
! ..... DETERMINE THE FLAG FOR 1-WAY OR 2-WAY NMO CORRECTION
!
   10 IF ( NUM_WAYS.NE.1 .AND. NUM_WAYS.NE.2 ) THEN
        CALL PC_ERROR("NUM_WAYS value is not recognized")
      END IF
!
! ..... MAKE SURE THE CDP NUMBER IS PRESENT
!
      IF ( CMP_IN.LE.0 )  THEN
        CALL PC_ERROR("It appears that geometry has not been assigned to &
          &this data set! (CDP not found in header).")
      END IF
!
! ..... MAKE SURE THE CDP NUMBER IS ONE MORE THAN THE LAST
!
      IF (INITFG.EQ.1) THEN
        LAST_CDP = CMP_IN
      ELSE IF ((LAST_CDP+1).NE.CMP_IN) THEN
!       CALL PC_ERROR("A CDP is missing!! The CDP number of ensemble should &
!         &increment by 1!!")
      END IF
!
! ..... CLEAR BETA DATA IF NOT NEEDED
!
      IF (NUM_WAYS.EQ.1 .AND. INITFG.EQ.1) THEN
        IE2=IX_BETA+MAX_SAMPS-1
        OBJ%WA1(IX_BETA:IE2)=0.0
      END IF
!
! ..... Get needed beta data
!
      IT=0
      DO I1 = 1, NTRC_GATH_IN
        TRACE_TYPE = HD(ITTYP_HNO, I1 )
!
! .....   Copy trace data into the beta array
        IF (TRACE_TYPE.EQ.IBETATR .OR. TRACE_TYPE.EQ.IBTRC2) THEN
          IE2=IX_BETA+MAX_SAMPS-1
          OBJ%WA1(IX_BETA:IE2)=TR(1:MAX_SAMPS,I1)
!          EXIT LOOP
          EXIT
        END IF
      ENDDO
!
! ..... RESET THE INITIALIZATION FLAG
!
      OBJ%INITFG=0
!
      JERROR   = 0
 
      NTRC_GATH_OUT = 0
 
      DO I1 = 1, NTRC_GATH_IN
!
        CMP_IN = HD( HDR_CURRENT_GROUP, I1 )

        TRACE_TYPE = HD( ITTYP_HNO, I1 )
 
        SOURCE_DETECT_DIST_IN = HD( HDR_OFFSET, I1 )
!
!*********************************************************************
!*******************  PROCESS TRACE NUMBER "I"  *******************
!*********************************************************************
!
!         LOOK ONLY AT SEISMIC TRACES
!
        IF (TRACE_TYPE.EQ.ISTRC .OR. TRACE_TYPE.EQ.IDEAD) THEN
!
!         Because CPS does not have a NA_STAT literal set static to zero 
!
          STATIC = 0.0
!
!
          IF (NUM_WAYS .EQ. 1) THEN
!......... WE ARE APPLYING 1-WAY NMO, GET HORIZ OFFSET FROM BOREHOLE
!......... TO THE SOURCE AND AND MULTIPLY BY 2 SO THAT THE 2-WAY
!......... TRAVEL-TIME IN NMO_APPLY.F WILL BE CORRECT.
            OFFST = SOURCE_DETECT_DIST_IN * 2.0
          ELSE
!......... NUMWAYS = 2 AND WE ARE DOING REGULAR SURFACE CDP STUFF
            OFFST = SOURCE_DETECT_DIST_IN
          ENDIF
!
! ..... Perform NMO on the input trace
          IF (INMO_DIR.EQ.1) THEN
            NV=MAX_SAMPS-1
            CALL TRMO_APPLY(TR(1:,I1),OBJ%WA1(IX_BETA:),OBJ%WA1(IX_WORK:), &
              OBJ%WA1(IX_WORK2:),STR_MUTE,OFFST,NV,SAMP_INT_IN,STATIC,NUM_WAYS)
! ..... else remove NMO from the input trace
          ELSE IF (INMO_DIR.EQ.-1) THEN
            NV=MAX_SAMPS-1
            CALL TRMO_UNAPPLY(TR(1:,I1),OBJ%WA1(IX_BETA:),OBJ%WA1(IX_WORK:), &
              OBJ%WA1(IX_WORK2:),OFFST,NV,SAMP_INT_IN,NUM_WAYS)
          END IF
!
        END IF
!------------------------------------------------------------------
!
        NTRC_GATH_OUT = NTRC_GATH_OUT + 1
      END DO
!
      LAST_CDP = CMP_IN
!
!      SAVE OUTPUT POINTER VALUES
      IF (IOUTOPT.EQ.1) THEN
!
!        JUST OUTPUT SEISMIC & DEAD TRACES IF OPTION WANTED
        IT1=0
        DO I1 = 1,NTRC_GATH_IN
          TRACE_TYPE = HD( ITTYP_HNO, I1 )
          IF (TRACE_TYPE.EQ.ISTRC .OR. TRACE_TYPE.EQ.IDEAD) THEN
            IT1=IT1+1
!
            IF (IT1.LT.I1) THEN
              TR(1:MAX_SAMPS,IT1)=TR(1:MAX_SAMPS,I1)
              HD(1:NHDRS,IT1)=HD(1:NHDRS,I1)
            END IF
          END IF
        END DO
!
!        RESET THE # OF DATA TRACES
        NTRC_GATH_OUT = IT1
      END IF
!
      NTR=NTRC_GATH_OUT
!
      IF ( JERROR .NE. 0 )   THEN
         IERROR = JERROR
      END IF
!

      end subroutine trmo

!------------------------------------------------------------------------------
!         SUBROUTINE TRMO_APPLY(TRACE,  BETA,   TRC,     SAMP_NMO, STRETCH, &
!                               OFFST,  NUMSMP, SAMPRAT, STATIC,   NUM_WAYS)
!------------------------------------------------------------------------------
! 
!       Description:
!               Applies 3-term moveout to a trace.
! 
!       Input Arguments:
!               BETA    - Beta array (NUMSMP long)
!               SCT     - 4-th order moveout factor  array (NUMSMP long)
!               TRC     - Temporary trace array (NUMSMP long)
!               SAMP_NMO- Temporary trace sample pointer array (NUMSMP long)
!               STRETCH - NMO maximum stretch factor
!               OFFST   - Offset distance
!               NUMSMP  - Number of samples in trace
!               SAMPRAT - Sample rate of trace
!               STATIC  - Static in milliseconds to be applied to the trace
!                          as part of the NMO process (effectively before NMO)
!               NUM_WAYS- 1-way NMO correction (VSP) or 2-way NMO for CDP data
! 
!       Input/Output Arguments:
!               TRACE   - Array of trace values (NUMSMP long)
! 
!       Original code by S.R. Bridges
!       Modified by D.E. Diller, July 12, 1989
!       Modified by B.N. Fuller, May 8, 1991, addition of 1-way NMO
!       Modified by Rutt Bridges, 27 Aug, 1991, added 8 point sinc interpolation
!
!       Revised  by D. Corrigan  12/08/93  To adopt nmo_apply.f to apply
!                                          abnormal moveout to trace
!
!       Revised  by D. Corrigan  06/23/94  To convert to ProMAX 5.0
!  
!  
!         Revised    07/05/94   D. Corrigan
!                               Change model of nonhyperbolic moveout to:
!                             
!                                 T0**2 + (X/V)**2*(1 - EPS*(X/XMAX)**2)
!  
!         Revised    02/26/96   D Corrigan
!                               Change way the stretch mute is applied
!  
!  
!         Revised    06/12/97   D. Corrigan
!                               Change model of nonhyperbolic moveout to:
!                             
!                                 TN2 = T0**2 + (X/V)**2
!                                 TE2 = TN2 + 2*ETA*(X/V)**2
!  
!                                 T**2 = TN2 - 2*ETA*(X/V)**4/TE2
!  
!         Revised    10/31/97   D Corrigan
!                               Correct problem when x=0 and t=0
!  
!         Revised    07/31/98   D Corrigan
!                               Correct problem when x<0 and t=0
!  
!------------------------------------------------------------------------------
        SUBROUTINE TRMO_APPLY(TRACE,  BETA,   TRC,     SAMP_NMO, STRETCH,  &
                              OFFST,  NUMSMP, SAMPRAT, STATIC,   NUM_WAYS)
!
      IMPLICIT NONE
      REAL             ,INTENT(INOUT) :: TRACE(:)    ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(IN)    :: BETA(:)     ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(OUT)   :: TRC(:)      ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(OUT)   :: SAMP_NMO(:) ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(IN)    :: STRETCH     ! ARGUMENTS 
      REAL             ,INTENT(IN)    :: OFFST       ! ARGUMENTS
      INTEGER          ,INTENT(IN)    :: NUMSMP      ! ARGUMENTS
      REAL             ,INTENT(IN)    :: SAMPRAT     ! ARGUMENTS
      REAL             ,INTENT(IN)    :: STATIC      ! ARGUMENTS
      INTEGER          ,INTENT(IN)    :: NUM_WAYS    ! ARGUMENTS
!
        INTEGER I1, I2
        INTEGER N, ISAMP1, NPTS
!
        REAL DTT,SLX,T02,STS
        REAL TN2,TE2,SAMPLE,TVAL

        REAL TNMO, STRETCH_MAX, OSSQRD, TIME
 
! ..... pre-compute offset squared & initialize the minimum sample index

        OSSQRD = (OFFST * OFFST)/4.0
 
! ..... compute the sample indexes at which NMO was applied
  
        STS  = STATIC/1000.
        ISAMP1 = 1
        STRETCH_MAX  = STRETCH/100.

! ..... Handle t=0 separately

        TRC(1)      = TRACE(1)
        TIME = SAMPRAT
        DO 100 N = 1, NUMSMP
 
! ......... save the TRACE array in TRC and zero TRACE

            T02      = TIME**2
            TRC(N)   = TRACE(N)
            TRACE(N) = 0.0
 
! ......... compute (x/v)**2

            SLX    = OSSQRD*BETA(N)
            TN2    = T02 + SLX
            TE2    = TN2
            TNMO   = SQRT(TN2)
            DTT    = TNMO - TIME
            IF (ABS(DTT).GT.STRETCH_MAX*TIME) ISAMP1 = N + 1
 
! ......... we want one-way NMO correction, so divide the difference
! ......... between TIME and TNMO by 2 and add that to TIME to
! ......... obtain the arrival time at the downhole phone

            IF (NUM_WAYS.EQ.1) TNMO = TIME + ( (TNMO - TIME) / 2.0 )
 
! ......... note that NMO index is computed relative to sample 0 (NOT 1!) since
!            we'll be using a 'C' routine to interpolate the values

            SAMP_NMO(N) = ( TNMO - STS ) / SAMPRAT - 1.0
 
! ......... increment the time variable

            TIME = TIME + SAMPRAT
100     CONTINUE
 
! ..... no action (TRACE was zeroed) if all samples are excessively stretched
!        otherwise, use a 7 point sinc interpolators ('C' code)

        NPTS = NUMSMP - ISAMP1
        IF (NPTS.GT.0) THEN
          DO I1=1,NPTS
            I2=ISAMP1+I1-1
            SAMPLE=SAMP_NMO(I2)
            CALL TRUEVALUE(TRC(1:), SAMPLE, NUMSMP, TVAL)
            TRACE(I2)=TVAL
          END DO
        END IF
!
        RETURN
        END SUBROUTINE TRMO_APPLY
 
!------------------------------------------------------------------------------
!         SUBROUTINE TRMO_UNAPPLY(TRACE,  BETA,   SAMP_INDEX, SAMP_NMO, &
!                                 OFFST,  NUMSMP, SAMPRAT,    NUM_WAYS)
!------------------------------------------------------------------------------
!  
!       Description:
!               Inverses (unapplies) a trace's previously applied 3-term moveout
!  
!       Input Arguments:
!             BETA      - Beta array (NUMSMP long)
!             SAMP_INDEX- Temporary trace sample pointer array (NUMSMP long)
!             SAMP_NMO  - Temporary trace sample pointer array (NUMSMP long)
!             OFFST     - Offset distance
!             NUMSMP    - Number of samples in trace
!             SAMPRAT   - Sample rate of trace
!             NUM_WAYS  - 1-way NMO correction (VSP) or 2-way NMO for CDP data
!  
!       Input/Output Arguments:
!             TRACE     - Array of trace values (NUMSMP long)
!  
!       Original code by S.R. Bridges, August 3, 1990.
!       Modified by B.N. Fuller, May 8, 1991, added 1-way NMO
!       Modified by Rutt Bridges, 27 Aug, 1991, added 8 point sinc interpolation
!
!       Revised  by D. Corrigan  12/08/93  To adopt nmo_apply.f to apply
!                                          abnormal moveout to trace
!  
!         Revised    07/05/94   D. Corrigan
!                               Change model of nonhyperbolic moveout to:
!                             
!                                 T0**2 + (X/V)**2*(1 - EPS*(X/XMAX)**2)
!  
!         Revised    03/19/96   D. Corrigan
!                               Change model of nonhyperbolic moveout to:
!                             
!                                 TN2 = T0**2 + (X/V)**2
!                                 TE2 = TN2 + 2*ETA*(X/V)**2
!  
!                                 T**2 = TN2 - 2*ETA*(X/V)**4/TE2
!  
!  
!         Revised    10/31/97   D Corrigan
!                               Correct problem when x=0 and t=0
!  
!
!------------------------------------------------------------------------------
 
      SUBROUTINE TRMO_UNAPPLY(TRACE,  BETA,   SAMP_INDEX, SAMP_NMO, &
                              OFFST,  NUMSMP, SAMPRAT,    NUM_WAYS)
!
      IMPLICIT NONE
      REAL             ,INTENT(INOUT) :: TRACE(:)      ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(IN)    :: BETA(:)       ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(OUT)   :: SAMP_INDEX(:) ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(OUT)   :: SAMP_NMO(:)   ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(IN)    :: OFFST         ! ARGUMENTS
      INTEGER          ,INTENT(IN)    :: NUMSMP        ! ARGUMENTS
      REAL             ,INTENT(IN)    :: SAMPRAT       ! ARGUMENTS
      INTEGER          ,INTENT(IN)    :: NUM_WAYS      ! ARGUMENTS
!
      INTEGER N, I, I1, I2, IT1, IT2, ISAMP1, NPTS, IST
!  
      REAL SLX,T02
      REAL TE2,TN2

      REAL OSSQRD, T_START
      REAL TIME, TNMO, RT1, RT2, R_INTERP, SAMP_MIN
!
!        INITIALIZE ARRAY
!
        SAMP_INDEX(1:NUMSMP)=0.0
 
! ..... pre-compute offset squared & initialize the minimum sample index
       
        OSSQRD = (OFFST * OFFST)/4.0
        SAMP_MIN = NUMSMP
 
! ..... compute the sample indexes at which NMO was applied

        TIME = SAMPRAT
        DO 100 N = 1, NUMSMP
 
! ......... compute the millisecond velocity and NMO time
 
            T02    = TIME**2
            SLX    = OSSQRD*BETA(N)
            TN2    = T02 + SLX
            TE2    = TN2
            TNMO   = SQRT(TN2)

! ......... we want one-way NMO correction, so divide the difference
! ......... between TIME and TNMO by 2; add that value to TIME to
! ......... obtain the arrival time at the downhole phone

            IF (NUM_WAYS .EQ. 1 ) TNMO = TIME + ( (TNMO - TIME) / 2.0 )
 
! ......... note that NMO index is computed relative to sample 0 (NOT 1!) since
!            we'll be using a 'C' routine to interpolate the values

            SAMP_NMO(N) = TNMO / SAMPRAT
 
! ......... increment the time

            TIME = TIME + SAMPRAT
 
100     CONTINUE
 
! ..... loop thru all samples and set the index sample values
        IST=0
        DO 300 N = 1, NUMSMP-1
 
! ......... set the positions (reals) and the sample indexes for the samples
!            to be interpolated
            RT1 = SAMP_NMO(N)
            IT1 = INT( RT1 + 1.0 )
            RT2 = SAMP_NMO(N+1)
            IT2 = INT( RT2 )
 
! ......... if IT1 is greater than the last sample, we're through
            IF (IT1.GE.NUMSMP) THEN
              IF (IST.NE.0) GOTO 301
! ......... set the trace indexes needed for interpolation
            ELSE IF (IT1.LE.IT2) THEN
               IF (IST.EQ.0) IST=N
               SAMP_MIN = AMIN1( SAMP_MIN, RT1 )
               R_INTERP = 1.0 / ( RT2 - RT1 )
 
! ............ note that NMO index is computed relative to sample 0 (NOT 1!)
!               since we'll be using a 'C' routine to interpolate the values
               T_START = N - 1.0 + ( IT1 - RT1 ) * R_INTERP
               SAMP_INDEX(IT1) = T_START
 
! ............ additional samples may lie between IT1 and IT2
               IF (IT1.LT.IT2) THEN
                  DO 200 I = 1, 100
                     IF (IT1+I.LE.IT2) THEN
                        SAMP_INDEX(IT1+I) = T_START + FLOAT(I) * R_INTERP
                     ELSE
                        GOTO 300
                     ENDIF
 200              CONTINUE
               ENDIF
            ENDIF
 
 300     CONTINUE
 
! ...... save the trace to be interpolated into SAMP_NMO (temp scratch array)
 301     SAMP_NMO(1:NUMSMP)=TRACE(1:NUMSMP)
 
! ...... compute the first affected sample
         ISAMP1 = INT( SAMP_MIN )
 
! ...... if ISAMP1 is greater than the trace length, clear the trace and RETURN
         IF (ISAMP1.GE.NUMSMP) THEN
            TRACE(1:NUMSMP)=0.0
            RETURN
         ENDIF
 
! ...... use 8 point sinc interpolators ('C' code)
         IF (ISAMP1.GT.1)  TRACE(1:ISAMP1-1)=0.0
         NPTS = NUMSMP - ISAMP1 + 1
         DO I1=1,NPTS
           I2=ISAMP1+I1-1
           CALL TRUEVALUE(SAMP_NMO, SAMP_INDEX(I2), NUMSMP, TRACE(I2))
         END DO
!
         RETURN
         END SUBROUTINE TRMO_UNAPPLY


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine trmo_wrapup (obj)
      type(trmo_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

! --> Insert any required wrapup code here, including wrapups of
! --> internally-called processes.

      end subroutine trmo_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module trmo_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

