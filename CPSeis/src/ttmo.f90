!<CPS_v1 type="PROCESS"/>
!!------------------------------- ttmo.f90 ---------------------------------!!
!!------------------------------- ttmo.f90 ---------------------------------!!
!!------------------------------- ttmo.f90 ---------------------------------!!

! --> Edit the line below as appropriate:

        ! other files are:  ttmo_crou.c  ttmo_frou.f90  ttmo.h

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
! Name       : TTMO
! Category   : velocity_analysis
! Written    : 2003-09-10   by: Michael Ried
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Three Term Moveout
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! --> Insert description information here.
! For layered media characterized by large vertical velocity gradients and/or
! vertical anisotropy, the moveout on reflected events may deviate from the
! hyperbolic assumption.  In order to "flatten" events in such media, the
! deviations from hyperbolic moveout need to be calculated.  ALAMO uses a
! simple fourth-order formula (a three-term function) to characterize these
! deviations.  The second order term in this formula only requires a velocity
! field; however, the fourth order term requires a velocity field and an eta
! field.  ALAMO is used to calculate the eta field.  The eta field and
! velocity field is used by TTMO (three-term moveout) to flatten the
! non-hyperbolic events.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! The input seismic data should not have moveout applied.  The input velocity
! field should be derived from a velocity analysis performed on the relatively
! short offset portion (where the deviations from hyperbolic moveout are
! relatively small) of the CDP gathers. 
!
! Applying a very mild smoothing filter with KA (Komba Analysis) to the eta
! field may improve moveout results (TTMO) and will reduce the stretching and
! compression of the seismic wavelet on the far offsets.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Velocity Trace (Needed if not using a velocity file)
! ETA trace (From ALAMO - Not used if using an file with ETA data in it)
! Seismic Traces
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! Seismic Traces
! All input traces (If selected)
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
!    4    HDR_CURRENT_CHANNEL        Made sure each trace is numbered
!    6    HDR_OFFSET                 Input into Calculations
!   49    HDR_USER_49                Input/Output trace type **
!
! **(Trace Types: 0=Dead trace,1=Seismic trace,51=Velocity trace,70=ETA trace)
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
!006. 2006-10-16  D. Glover     Added NULLIFY statements for Intel compiler.
!  5. 2006-01-10  B. Menger     Removed Unused Variables.
!  4. 2005-01-31  Michael Ried  Allowed input to continue if no output
!  3. 2003-12-17  Michael Ried  CDPs are not required to be consecutive
!  2. 2003-11-12  Michael Ried  Fixed a Problem with the Inverse moveout
!  1. 2003-09-10  Michael Ried  Initial version.
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
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
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
!<NS TTMO Process/NC=80>
!                       TTMO - 3-term Moveout
!`----------------------------------------------------------------------------
!      Direction for moveout application[/R]    DIRECT=`CCCCCCCCCCCCCCCCCCCCCC
!                Stretch mute percentage[/R]  STR_MUTE=`FFFFFFFFFFFFFFFFFFFFFF
! Apply remaining static during moveout?[/R]     LSTAT=`KKK
! Use Eta field for a 3-term correction?[/R]    C3TOPT=`KKK
!`----------------------------------------------------------------------------
!`----------------------------------------------------------------------------
!       Select the Velocity Input Option[/R]    VELOPT=`CCCCCCCCCCCCCCCCCCCCCC
!                Velocity Parameter file[/R]
!Select VELNAME[VELNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!         [VELNAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!`----------------------------------------------------------------------------
!`----------------------------------------------------------------------------
!            Select the ETA Input Option[/R]    ETAOPT=`CCCCCCCCCCCCCCCCCCCCCC
!                     ETA Parameter File[/R]
!Select ETANAME[ETANAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!         [ETANAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!`----------------------------------------------------------------------------
!`----------------------------------------------------------------------------
!      Select the type of data to output[/R]    OUTOPT=`CCCCCCCCCCCCCCCCCCCCCC
!`----------------------------------------------------------------------------
!<PARMS ETANAME[/ML=128/XST]>
!<PARMS VELNAME[/ML=128/XST]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="C3TOPT">
!<Tip> -->Choose whether to use Eta field for a 3-term correction</Tip>
! Default = --> YES
! Allowed = --> YES
! Allowed = --> NO
! --> If 'YES', a 3-term correction will be performed
!</Help>
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
!<Help KEYWORD="ETANAME">
!<Tip> --> Use the file selection dialog to choose a ETA parameter file.</Tip>
! Default = --> NONE
! Allowed = --> A file name
! --> Eta parameter file
!</Help>
!
!<Help KEYWORD="ETANAME_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> --> Status of the ETA Parameter file.</Tip>
! Default = --> NONE
! Allowed = --> Status information
! --> Eta parameter file status information
!</Help>
!
!<Help KEYWORD="ETAOPT">
!<Tip> -->Select an ETA input option</Tip>
! Default = --> ETA trace data
! Allowed = --> ETA trace data
! Allowed = --> ETA parameter file
! --> ETA trace data -- Input ETA trace data
! --> ETA parameter file -- Input ETA parameter file
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
! Allowed = --> ALL INPUT TRACES
! --> Select the type of data to output
!</Help>
!
!<Help KEYWORD="SELECT_ETANAME">
!<Tip> --> Use the file selection dialog to choose a ETA parameter file.</Tip>
!</Help>
!
!<Help KEYWORD="SELECT_VELNAME">
!<Tip> --> Use the file selection dialog to choose a Velocity parm file.</Tip>
!</Help>
!
!<Help KEYWORD="STR_MUTE">
!<Tip> --> Enter a stretch mute percentage</Tip>
! Default = --> 200.0
! Allowed = --> Real
! --> Any sample strectched more than this perectage will be automatically
!     muted
!</Help>
!
!<Help KEYWORD="VELNAME">
!<Tip> --> Use the file selection dialog to choose a Velocity parm file.</Tip>
! Default = --> NONE
! Allowed = --> A file name
! --> SELECT Velocity parameter file
!</Help>
!
!<Help KEYWORD="VELNAME_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> --> Status of the velocity Parameter file.</Tip>
! Default = --> NONE
! Allowed = --> Status information
! --> Velocity parameter file status information
!</Help>
!
!<Help KEYWORD="VELOPT">
!<Tip> -->Choose a Velocity Option. Select a velocity file or a velocity</Tip>
! Default = --> Velocity table
! Allowed = --> Velocity table
! Allowed = --> Velocity trace
! --> Velocity table -- Select a velocity parameter file
! --> Velocity trace -- Input velocity trace data
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module ttmo_module
      use grid_module            ! if you need the grid transformation.
      use named_constants_module
      use mem_module
      use nmo_module             ! Creating a velocity trace
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.
      use pc_module
      use permtfile_module

! --> Insert here any other modules used by this process.

      implicit none
      private
      public :: ttmo_create
      public :: ttmo_initialize
      public :: ttmo_update
      public :: ttmo_delete
      public :: ttmo            ! main trace processing routine.
      public :: ttmo_wrapup

      character(len=100),public,save :: TTMO_IDENT = &
'$Id: ttmo.f90,v 1.6 2006/10/17 13:45:49 Glover prod sps $'

      character(len=FILENAME_LENGTH)  :: etaname
      character(len=FILENAME_LENGTH)  :: velname


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: ttmo_struct

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

        logical                    :: c3topt
        character(len=21)          :: direct
        logical                    :: lstat
        character(len=128)         :: etaname
        character(len=21)          :: etaopt
        character(len=21)          :: outopt
        real                       :: str_mute
        character(len=128)         :: velname
        character(len=21)          :: velopt
        integer                    :: i3topt
        integer                    :: initfg
        integer                    :: inmo_dir
        integer                    :: istat
        integer                    :: ietaopt
        integer                    :: ioutopt
        integer                    :: ivelopt
        integer                    :: num_ways

        integer                    :: ix_eta
        integer                    :: ix_sct
        integer                    :: ix_tmt
        integer                    :: ix_work
        integer                    :: ix_work2

! --> Insert any other needed variables or pointers here.
   type(pathchoose_struct),pointer :: pathchoose1
   type(pathchoose_struct),pointer :: pathchoose2
   type(permtfile_struct),pointer  :: id_efile
   type(nmo_struct),pointer   :: nmo

        real              ,pointer :: wa1(:)  ! work array

      end type ttmo_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


! --> Include any required interfaces here.


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(ttmo_struct),pointer,save :: object    ! needed for traps.

! --> Insert here any data declarations needed by this process.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine ttmo_create (obj)
      type(ttmo_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in ttmo_create")

! --> Nullify any additional pointers in the OBJ data structure here.
      nullify(obj%wa1)   ! must be done for all pointers.
      nullify(obj%nmo)
      nullify (obj%id_efile) ! jpa
      nullify (obj%pathchoose1) ! jpa
      nullify (obj%pathchoose2) ! jpa

      call pathchoose_create (obj%pathchoose1, 'etaname'  , '.trc')
      call pathchoose_create (obj%pathchoose2, 'velname', '.vel')

      call ttmo_initialize (obj)
      end subroutine ttmo_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine ttmo_delete (obj)
      type(ttmo_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking
  
      call pathchoose_delete (obj%pathchoose1)
      call pathchoose_delete (obj%pathchoose2)

      call ttmo_wrapup (obj)


! --> Deallocate any additional pointers in the OBJ data structure here.

      if (associated(obj%wa1)) deallocate(obj%wa1)
      if (associated(obj%nmo)) call nmo_delete(obj%nmo)

!     close the eta file and the semblance file
      if (obj%ietaopt .eq. 2) then
        call permtfile_close(obj%id_efile)
      end if

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in ttmo_delete")
      end subroutine ttmo_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine ttmo_initialize (obj)
      type(ttmo_struct),intent(inout) :: obj       ! arguments

! --> Change the default values below as needed

      obj%c3topt       = .TRUE.
      obj%direct       = 'Forward'
      obj%lstat        = .TRUE.
      obj%etaname      = 'NONE'
      obj%etaopt       = 'ETA trace data '
      obj%outopt       = 'Seismic traces only  '
      obj%str_mute     = 200.0
      obj%velname      = 'NONE'
      obj%velopt       = 'Velocity table'
      obj%i3topt       = 1
      obj%inmo_dir     = 1
      obj%istat        = 1
      obj%ietaopt      = 1
      obj%ioutopt      = 1
      obj%ivelopt      = 1

      obj%etaname      = PATHCHECK_EMPTY
      obj%velname      = PATHCHECK_EMPTY

      call ttmo_update (obj)
      end subroutine ttmo_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine ttmo_update (obj)
      type(ttmo_struct),intent(inout),target :: obj             ! arguments

! --> Insert code to declare all required local variables.
      integer istat, ierr, ietaopt, inmo_dir, ivelopt, ipn
      integer lun, lwa1, max_samps
      logical lstat, lereq, lrhist, lvreq
      

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

! --> Delete any of the globals below that are not needed:

      ipn = pc_get_ipn()
      obj%ipn = ipn

      call pc_get_global ('numtr'   , obj%numtr)
      call pc_get_global ('gathered', obj%gathered)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)
      call pc_get_global ('grid'    , obj%grid)

      call pc_get('C3TOPT  ', obj%c3topt)
      call pc_get('DIRECT  ', obj%direct)
      call pc_get('ETANAME ', obj%etaname)
      call pc_get('ETAOPT  ', obj%etaopt)
      call pc_get('LSTAT   ', obj%lstat)
      call pc_get('OUTOPT  ', obj%outopt)
      call pc_get('STR_MUTE', obj%str_mute)
      call pc_get('VELNAME ', obj%velname)
      call pc_get('VELOPT  ', obj%velopt)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!
! ..... Change options into integer values
!
      IF (OBJ%C3TOPT) THEN
        OBJ%I3TOPT=1
      ELSE
        OBJ%I3TOPT=0
      END IF
!
      IF (OBJ%DIRECT.EQ.'Inverse              ') THEN
        OBJ%INMO_DIR=-1
      ELSE
        OBJ%INMO_DIR=1
      END IF
!
      IF (LSTAT) THEN
        OBJ%ISTAT=1
      ELSE
        OBJ%ISTAT=0
      END IF
!
      IF (OBJ%ETAOPT.EQ.'ETA parameter file   ') THEN
        OBJ%IETAOPT=2
      ELSE
        OBJ%IETAOPT=1
      END IF
      IF (OBJ%I3TOPT.EQ.0) OBJ%IETAOPT=0
!
      IF (OBJ%OUTOPT.EQ.'All input traces     ') THEN
        OBJ%IOUTOPT=2
      ELSE
        OBJ%IOUTOPT=1
      END IF
!
      IF (OBJ%VELOPT.EQ.'Velocity trace       ') THEN
        OBJ%IVELOPT=2
      ELSE
        OBJ%IVELOPT=1
      END IF
!
!      Initialize needed variables
!
      INMO_DIR = OBJ%INMO_DIR
      ISTAT = OBJ%ISTAT
      IETAOPT = OBJ%IETAOPT
      IVELOPT = OBJ%IVELOPT
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
!
      IF (IETAOPT.EQ.2) THEN
        LEREQ=.TRUE.
        if (pathchoose_update (obj%pathchoose1, obj%etaname)) return
        call pathcheck("etaname"  , obj%etaname, &
                       '.trc', lereq, show=PATHCHECK_INFO_OUTPUT)
      ELSE
        LEREQ=.FALSE.
      END IF
!
      IF (IVELOPT.EQ.2) THEN
        LVREQ=.FALSE.
      ELSE
        LVREQ=.TRUE.
        if (pathchoose_update (obj%pathchoose2, obj%velname)) return
        call pathcheck("velname", obj%velname, &
                       'vel', lvreq, show=PATHCHECK_INFO_INPUT)
      END IF


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


! --> Insert code to call internal processes to verify process parameters.
      if (ivelopt.eq.1) then
        call pc_clear
  
        call pc_print (' ')
        call pc_print ('TTMO: internally calling process NMO:')
  
        call pc_put_global  ('OPT_NMO', 'STK_VEL')
        call pc_put_global  ('PATHNAME', obj%velname)
        call pc_put_process  ('OPT_NMO', 'STK_VEL')
        call pc_put_process  ('PATHNAME', obj%velname)

        if (associated(obj%nmo)) then
           call nmo_update (obj%nmo)
        else
           call nmo_create (obj%nmo)
        end if
!
        call pc_print_process_cards
        call pc_print (' ')
!
        call pc_restore
      end if

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
      call pc_put("etaname" , obj%etaname)
      call pc_put("velname" , obj%velname)
!
      IF (IETAOPT.EQ.2) THEN
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('ETANAME',        .TRUE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('select_ETANAME', .TRUE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('ETANAME_INFO',   .TRUE.)
      ELSE
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('ETANAME',        .FALSE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('select_ETANAME', .FALSE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('ETANAME_INFO',   .FALSE.)
      END IF
!
      IF (IVELOPT.EQ.2) THEN
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('VELNAME',        .FALSE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('Select_VELNAME', .FALSE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('VELNAME_INFO',   .FALSE.)
      ELSE
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('VELNAME',        .TRUE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('Select_VELNAME', .TRUE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('VELNAME_INFO',   .TRUE.)
      END IF

      call pc_put_options_field('DIRECT ', (/'Forward              ',          &
        'Inverse              '/) )
      call pc_put_options_field('ETAOPT ', (/'ETA trace data       ',          &
        'ETA parameter file   '/) )
      call pc_put_options_field('OUTOPT ', (/'Seismic traces only  ',          &
        'All input traces     '/) )
      call pc_put_options_field('VELOPT ', (/'Velocity table       ',          &
        'Velocity trace       '/) )

! --> Delete any of the globals below that have not changed:

      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)

      call pc_put('C3TOPT               ', obj%c3topt)
      call pc_put('DIRECT               ', obj%direct)
      call pc_put('LSTAT                ', obj%lstat)
      call pc_put('ETANAME              ', obj%etaname)
      call pc_put('ETAOPT               ', obj%etaopt)
      call pc_put('OUTOPT               ', obj%outopt)
      call pc_put('STR_MUTE             ', obj%str_mute)
      call pc_put('VELNAME              ', obj%velname)
      call pc_put('VELOPT               ', obj%velopt)

! --> Change the control defaults below as appropriate:

      call pc_put_control ('ntapes'       , 0)
      call pc_put_control ('need_label'   , .false.)
      call pc_put_control ('twosets'      , .false.)
      call pc_put_control ('nscratch'     , 0)
      call pc_put_control ('nstore'       , 0)
      call pc_put_control ('iftd'         , .false.)
      call pc_put_control ('ndisk'        , 0)
      call pc_put_control ('setup_only'   , .false.)

! --> Change the need request because there might be no output traces
      call pc_put_control ('need_request' , .true.)
!
!      The following will cause this process not to be parrallel
!
!     call pc_put_control ('parallel_safe', .false.)
!
!      The following will cause this process to be parrallel
!
      call pc_put_control ('parallel_safe'        , .true.)
      call pc_put_control ('pcps_send_mode'       ,'pcps_send_first_avail')
      call pc_put_control ('pcps_receive_mode'    ,'pcps_receive_passthru')
      call pc_put_control ('pcps_bunch_mode'      ,'pcps_bunch_trace_groups')
      call pc_put_control ('pcps_send_eof_mode'   ,'pcps_send_all_eof')
      call pc_put_control ('pcps_alt_send_mode'   ,'pcps_send_all')
      call pc_put_control ('pcps_alt_receive_mode','pcps_receive_all_eof')
      call pc_put_control ('pcps_resequence_mode' ,'pcps_resequence_traces')
      call pc_put_control ('pcps_generator_mode'  ,'pcps_no_trace_gen')
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
!        OPEN THE ETA FILE FOR READING
!
      IF (OBJ%IETAOPT.EQ.2) THEN
        LRHIST=.FALSE.
        CALL PERMTFILE_OPEN_READ(OBJ%ID_EFILE,OBJ%ETANAME,OBJ%NWIH,OBJ%NDPT, &
          OBJ%TSTRT,OBJ%DT,LUN,IERR,LRHIST)
      END IF
!
! --> Insert code to allocate needed permanent memory.
!
!     Set up the work array     
!
      LWA1 = 0
!
!       Now Increase the size of the work buffer
!
! ..... IX_ETA  :  ETA values
      OBJ%IX_ETA = LWA1 + 1
      LWA1 = LWA1 + MAX_SAMPS
! ..... IX_SCT  :  4th order moveout factor array
      OBJ%IX_SCT = LWA1 + 1
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


      end subroutine ttmo_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


! --> Insert code for parameter traps here.
! --> (EZCPS with the -t option will insert skeleton code for you)


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine ttmo (obj,ntr,hd,tr)
      type(ttmo_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

! --> Insert declarations of local variables.

      DOUBLE PRECISION, DIMENSION(SIZE(HD,1),1) :: HDI ! Input Header

      REAL, DIMENSION(SIZE(TR,1),1) :: VELTR  ! The velocity trace
!
      INTEGER CMP_IN, DATA_TRACE_TYPE_IN
      INTEGER I1, ICDP, IDEAD, IE2, IERROR, IETAOPT, IETATR
      INTEGER INITFG, INMO_DIR, IOUTOPT, ISAMP, ISTRC, IT, IVELOPT, IVTRC
      INTEGER IX_ETA, IX_SCT, IX_TMT, IX_WORK, IX_WORK2, IT1
      INTEGER JERROR, LAST_CDP, MAX_SAMPS
      INTEGER NHDRS, NTRC_GATH_IN, NTRC_GATH_OUT, NTR_VEL, NUM_WAYS, NV
      REAL ETA, OFFSET, SAMP_INT_IN, SOURCE_DETECT_DIST_IN, STATIC, STR_MUTE
!
      REAL ZERO
!
      SAVE :: LAST_CDP
!
      DATA  IDEAD  / 0 /
      DATA  ISTRC  / 1 /
      DATA  IVTRC  /51 /
      DATA  IETATR /70 /
      DATA  ZERO /0.0 /
!
! --> Insert code for processing logic.
!
!     The following conditional call to the WRAPUP routine should be made
!     before returning from this subroutine:
!
      IF (NTR == NO_MORE_TRACES .OR. NTR == FATAL_ERROR) THEN
        CALL TTMO_WRAPUP(OBJ)
        RETURN
      END IF
!
!      set pointers into the work array
!
      ix_eta=obj%ix_eta
      ix_sct=obj%ix_sct
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
      IETAOPT=OBJ%IETAOPT
      INITFG=OBJ%INITFG
      INMO_DIR=OBJ%INMO_DIR
      IOUTOPT=OBJ%IOUTOPT
      IVELOPT=OBJ%IVELOPT
      STR_MUTE=OBJ%STR_MUTE
!
!      initialize header values
!
      CMP_IN = HD(HDR_CURRENT_GROUP,1)
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
      IF ( NUM_WAYS.NE.1 .AND. NUM_WAYS.NE.2 ) THEN
        CALL PC_ERROR("NUM_WAYS value is not recognized")
      END IF
!
! ..... MAKE SURE THE CDP NUMBER IS PRESENT
!
      ICDP = CMP_IN
      IF ( ICDP.LE.0 )  THEN
        CALL PC_ERROR("It appears that geometry has not been assigned to &
          &this data set! (CDP not found in header).")
      END IF
!
! ..... INITIALIZE THE LAST CDP NUMBER
!
      IF (INITFG.EQ.1) THEN
        LAST_CDP = ICDP
      END IF
!
! ..... Read the velocity file
!
      IF (IVELOPT.EQ.1) THEN
        NTR_VEL=1
        CALL NMO(OBJ%NMO,NTR_VEL,HD,VELTR)
!
! ..... Get needed velocity data
!
      ELSE IF (IVELOPT.EQ.2) THEN
        IT=0
        DO I1 = 1, NTRC_GATH_IN
          DATA_TRACE_TYPE_IN = HD(HDR_USER_49, I1 )
!
! .....   Copy trace data into the velocity array
          IF (DATA_TRACE_TYPE_IN.EQ.IVTRC) THEN
            VELTR(1:MAX_SAMPS,1)=TR(1:MAX_SAMPS,I1)
!            EXIT LOOP
            EXIT
          END IF
        ENDDO
      END IF
!
! ..... CLEAR ETA DATA IF NOT NEEDED
!
      IF (NUM_WAYS.EQ.1 .AND. INITFG.EQ.1) THEN
        IE2=IX_ETA+MAX_SAMPS-1
        OBJ%WA1(IX_ETA:IE2)=0.0
      ELSE IF (IETAOPT.EQ.0) THEN
        IE2=IX_ETA+MAX_SAMPS-1
        OBJ%WA1(IX_ETA:IE2)=0.0
      END IF
!
! ..... Read the eta file
!
      IF (NUM_WAYS.EQ.2) THEN
        IF (IETAOPT.EQ.2) THEN
          CALL PERMTFILE_READ(OBJ%ID_EFILE,HDI(1:,1),OBJ%WA1(IX_ETA:),JERROR)
!
! ..... Get needed eta data
!
        ELSE IF (IETAOPT.EQ.1) THEN
          IT=0
          DO I1 = 1, NTRC_GATH_IN
            DATA_TRACE_TYPE_IN = HD(HDR_USER_49, I1 )
!
! .....   Copy trace data into the velocity array
            IF (DATA_TRACE_TYPE_IN.EQ.IETATR) THEN
              IE2=IX_ETA+MAX_SAMPS-1
              OBJ%WA1(IX_ETA:IE2)=TR(1:MAX_SAMPS,I1)
!              EXIT LOOP
              EXIT
            END IF
          ENDDO
        END IF
      END IF

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

        DATA_TRACE_TYPE_IN = HD( HDR_USER_49, I1 )
 
        SOURCE_DETECT_DIST_IN = HD( HDR_OFFSET, I1 )
!
!*********************************************************************
!*******************  PROCESS TRACE NUMBER "I"  *******************
!*********************************************************************
!
!         LOOK ONLY AT SEISMIC TRACES
!
        IF (DATA_TRACE_TYPE_IN.EQ.ISTRC .OR.  &
            DATA_TRACE_TYPE_IN.EQ.IDEAD) THEN
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
            OFFSET = SOURCE_DETECT_DIST_IN * 2.0
          ELSE
!......... NUMWAYS = 2 AND WE ARE DOING REGULAR SURFACE CDP STUFF
            OFFSET = SOURCE_DETECT_DIST_IN
          ENDIF
!
! ..... Compute the array required for the 4-th order correction
          DO ISAMP = 1,MAX_SAMPS
            ETA   = OBJ%WA1(IX_ETA+ISAMP-1)/100.
            OBJ%WA1(IX_SCT+ISAMP-1) = ETA
          ENDDO
!
! ..... Perform NMO on the input trace
          IF (INMO_DIR.EQ.1) THEN
            NV=MAX_SAMPS-1
            CALL TTMO_APPLY(TR(1:,I1),VELTR(1:,1),OBJ%WA1(IX_SCT:),    &
              OBJ%WA1(IX_WORK:),OBJ%WA1(IX_WORK2:),STR_MUTE,OFFSET,NV, &
              SAMP_INT_IN,STATIC,NUM_WAYS)
! ..... else remove NMO from the input trace
          ELSE IF (INMO_DIR.EQ.-1) THEN
            NV=MAX_SAMPS-1
            CALL TTMO_UNAPPLY(TR(1:,I1),VELTR(1:,1),obj%wa1(IX_SCT:),     &
              OBJ%WA1(IX_WORK:),OBJ%WA1(IX_WORK2:),OFFSET,NV,SAMP_INT_IN, &
              NUM_WAYS)
          END IF
        END IF
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
          DATA_TRACE_TYPE_IN = HD( HDR_USER_49, I1 )
          IF (DATA_TRACE_TYPE_IN.EQ.ISTRC .OR. &
              DATA_TRACE_TYPE_IN.EQ.IDEAD) THEN
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
!      GET MORE TRACES EVEN IF THERE ARE NOT OUTPUT TRACES
!
      IF (NTR == 0) THEN
        NTR = NEED_TRACES
      END IF
!
      IF ( JERROR .NE. 0 )   THEN
         IERROR = JERROR
      END IF
!
      END SUBROUTINE TTMO

!------------------------------------------------------------------------------
!         SUBROUTINE TTMO_APPLY( TRACE, VNMO, SCT, TRC, SAMP_NMO, STRETCH,
!            OFFSET, NUMSMP, SAMPRAT, STATIC, NUM_WAYS )
!------------------------------------------------------------------------------
! 
!       Description:
!               Applies 3-term moveout to a trace.
! 
!       Input Arguments:
!               VNMO    - Normal move-out velocity array (NUMSMP long)
!               SCT     - 4-th order moveout factor  array (NUMSMP long)
!               TRC     - Temporary trace array (NUMSMP long)
!               SAMP_NMO- Temporary trace sample pointer array (NUMSMP long)
!               STRETCH - NMO maximum stretch factor
!               OFFSET  - Offset distance
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
        SUBROUTINE TTMO_APPLY( TRACE, VNMO, SCT, TRC, SAMP_NMO, &
           STRETCH,OFFSET, NUMSMP, SAMPRAT, STATIC, NUM_WAYS )
!
      IMPLICIT NONE
      REAL             ,INTENT(INOUT) :: TRACE(:)    ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(IN)    :: VNMO(:)     ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(IN)    :: SCT(:)      ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(OUT)   :: TRC(:)      ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(OUT)   :: SAMP_NMO(:) ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(IN)    :: STRETCH     ! ARGUMENTS 
      REAL             ,INTENT(IN)    :: OFFSET      ! ARGUMENTS
      INTEGER          ,INTENT(IN)    :: NUMSMP      ! ARGUMENTS
      REAL             ,INTENT(IN)    :: SAMPRAT     ! ARGUMENTS
      REAL             ,INTENT(IN)    :: STATIC      ! ARGUMENTS
      INTEGER          ,INTENT(IN)    :: NUM_WAYS    ! ARGUMENTS
!
        INTEGER I1, I2
        INTEGER N, ISAMP1, NPTS
!
        REAL DTT,SLX,SRS,T02,STS
        REAL TN2,TE2,SL2,SAMPLE,TVAL

        REAL TNMO, STRETCH_MAX, OSSQRD, TIME
 
! ..... pre-compute offset squared & initialize the minimum sample index

        OSSQRD = OFFSET * OFFSET
 
! ..... compute the sample indexes at which NMO was applied
  
        SRS  = SAMPRAT/1000.
        STS  = STATIC/1000.
        ISAMP1 = 1
        STRETCH_MAX  = STRETCH/100.

! ..... Handle t=0 separately

        TRC(1)      = TRACE(1)
        TIME = SRS
        DO 100 N = 1, NUMSMP
 
! ......... save the TRACE array in TRC and zero TRACE

            T02      = TIME**2
            TRC(N)   = TRACE(N)
            TRACE(N) = 0.0
 
! ......... compute (x/v)**2

            SLX    = OSSQRD/VNMO(N)**2
            TN2    = T02 + SLX
            SL2    = 2.*SCT(N)*SLX
            TE2    = TN2 + SL2
            TNMO   = SQRT( TN2 - SL2*SLX/TE2 )
            DTT    = TNMO - TIME
            IF( DTT.GT.STRETCH_MAX*TIME ) ISAMP1 = N + 1
 
! ......... we want one-way NMO correction, so divide the difference
! ......... between TIME and TNMO by 2 and add that to TIME to
! ......... obtain the arrival time at the downhole phone

            IF (NUM_WAYS.EQ.1) TNMO = TIME + ( (TNMO - TIME) / 2.0 )
 
! ......... note that NMO index is computed relative to sample 0 (NOT 1!) since
!            we'll be using a 'C' routine to interpolate the values

            SAMP_NMO(N) = ( TNMO - STS ) / SRS - 1.0
 
! ......... increment the time variable

            TIME = TIME + SRS
 
100     CONTINUE
 
! ..... no action (TRACE was zeroed) if all samples are excessively stretched
!        otherwise, use a 7 point sinc interpolators ('C' code)

        NPTS = NUMSMP - ISAMP1
        IF (NPTS.GT.0) THEN
          DO I1=1,NPTS
            I2=ISAMP1+I1-1
            SAMPLE=SAMP_NMO(I2)
            CALL TRUEVALUE(TRC, SAMPLE, NUMSMP, TVAL)
            TRACE(I2)=TVAL
          END DO
        END IF
!
        RETURN
        END SUBROUTINE TTMO_APPLY
 
!------------------------------------------------------------------------------
!         SUBROUTINE TTMO_UNAPPLY( TRACE, VNMO, SCT, SAMP_INDEX, SAMP_NMO, &
!              OFFSET, NUMSMP, SAMPRAT, NUM_WAYS)
!------------------------------------------------------------------------------
!  
!       Description:
!               Inverses (unapplies) a trace's previously applied 3-term moveout
!  
!       Input Arguments:
!             VNMO      - Normal move-out velocity array (NUMSMP long)
!             SCT       - 4-th order moveout factor  array (NUMSMP long)
!             SAMP_INDEX- Temporary trace sample pointer array (NUMSMP long)
!             SAMP_NMO  - Temporary trace sample pointer array (NUMSMP long)
!             OFFSET    - Offset distance
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
 
      SUBROUTINE TTMO_UNAPPLY( TRACE, VNMO, SCT, SAMP_INDEX, &
        SAMP_NMO,OFFSET, NUMSMP, SAMPRAT, NUM_WAYS)
!
      IMPLICIT NONE
      REAL             ,INTENT(INOUT) :: TRACE(:)      ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(IN)    :: VNMO(:)       ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(IN)    :: SCT(:)        ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(OUT)   :: SAMP_INDEX(:) ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(OUT)   :: SAMP_NMO(:)   ! ARGUMENTS (LEN=NUMSMP)
      REAL             ,INTENT(IN)    :: OFFSET        ! ARGUMENTS
      INTEGER          ,INTENT(IN)    :: NUMSMP        ! ARGUMENTS
      REAL             ,INTENT(IN)    :: SAMPRAT       ! ARGUMENTS
      INTEGER          ,INTENT(IN)    :: NUM_WAYS      ! ARGUMENTS
!
      INTEGER N, I, I1, I2, IT1, IT2, ISAMP1, NPTS, IST
!  
      REAL SRS,SLX,T02
      REAL TE2,TN2,SL2

      REAL OSSQRD, T_START
      REAL TIME, TNMO, RT1, RT2, R_INTERP, SAMP_MIN
!
!        INITIALIZE ARRAY
!
        SAMP_INDEX(1:NUMSMP)=0.0
 
! ..... pre-compute offset squared & initialize the minimum sample index
       
        SRS    = SAMPRAT/1000.
        OSSQRD = OFFSET * OFFSET
        SAMP_MIN = NUMSMP
 
! ..... compute the sample indexes at which NMO was applied

        TIME = SRS
        DO 100 N = 1, NUMSMP
 
! ......... compute the millisecond velocity and NMO time
 
            T02    = TIME**2
            SLX    = OSSQRD/VNMO(N)**2
            TN2    = T02 + SLX
            SL2    = 2.*SCT(N)*SLX
            TE2    = TN2 + SL2
            TNMO   = SQRT( TN2 - SL2*SLX/TE2 )

! ......... we want one-way NMO correction, so divide the difference
! ......... between TIME and TNMO by 2; add that value to TIME to
! ......... obtain the arrival time at the downhole phone

            IF (NUM_WAYS .EQ. 1 ) TNMO = TIME + ( (TNMO - TIME) / 2.0 )
 
! ......... note that NMO index is computed relative to sample 0 (NOT 1!) since
!            we'll be using a 'C' routine to interpolate the values

            SAMP_NMO(N) = TNMO / SRS
 
! ......... increment the time

            TIME = TIME + SRS
 
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
         END SUBROUTINE TTMO_UNAPPLY


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine ttmo_wrapup (obj)
      type(ttmo_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

! --> Insert any required wrapup code here, including wrapups of
! --> internally-called processes.

      end subroutine ttmo_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module ttmo_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

