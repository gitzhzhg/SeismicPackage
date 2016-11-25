!<CPS_v1 type="PROCESS"/>
!!------------------------------- tsmute.f90 ---------------------------------!!
!!------------------------------- tsmute.f90 ---------------------------------!!
!!------------------------------- tsmute.f90 ---------------------------------!!

! --> Edit the line below as appropriate:

        ! other files are:

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
! Name       : TSMUTE
! Category   : velocity_analysis
! Written    : 2003-11-07   by: Michael Ried
! Revised    : 2007-11-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Muting of the top salt
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! TSMUTE (Top Salt Mute) is used to apply surgical muting to remove head waves
! and converted shear waves that are generated at interfaces with large
! interval velocity contrasts.
!
! Surgical muting begins at the critical offset.  The top and bottom slopes
! are used to control how rapidly the surgical mutes expand beyond the critical
! offset.
!
! The critical offset is calculated using the following information: 
!   a) An average velocity model
!   b) The depth of the horizon with the large interval velocity contrast
!      (e.g., top salt) 
!   c) The interval velocity of the medium under the horizon (e.g., salt
!      velocity). 
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! Use the MASKER process to put a horizon (e.g., top salt) into the trace
! headers. Use the TSMUTE menu to specify the location of the header word
! containing the horizon depth value.
!
! An average velocity trace can be attached to each Common Image Gather (CIG)
! that is input into TSMUTE.  Group the velocity trace with its respective CIG
! gather by using the following sequence in a CPS flow: 
!   TRIN (seismic) -> GATHER -> TRIN (velocity)
!
! The average velocity trace must have a value of 51 in header word 49.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Velocity Trace
! Seismic Traces
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! All input traces will be outputted
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
!    6    HDR_OFFSET                 Input into Calculations
!   48    HDR_USER_48                Header word number for top salt
!                                    (Hwd#48 is the default header word #)
!   49    HDR_USER_49                Input/Output trace type **
!                                    (Hwd#49 is the default header word #)
!
! **(Trace Types: 0=Dead trace,1=Seismic trace,51=Input Velocity trace)
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
!  6. 2007-11-29  Stoeckley     Remove unused reference to memman.
!005. 2006-10-16  D. Glover     Added NULLIFY statements for Intel compiler.
!  4. 2006-01-03  Michael Ried  Sample rate < 1 assumed to be in kilofeet
!  3. 2005-02-21  Michael Ried  Increased the max # of reference refactor nos
!  2. 2005-02-01  Michael Ried  Allowed process to read a modspec file 
!  1. 2003-11-07  Michael Ried  Initial version.
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
!  None provided.
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
!<NS tsmute Process/NC=80>
!                       TSMUTE - Muting of the top salt
!`------------------------------------------------------------------------------
!      Header word number for trace type[/R]  TTYP_HNO=`FFFFFFFFFFFFFFFFFFFF
!                       Velocity of salt[/R]  SALT_VEL=`FFFFFFFFFFFFFFFFFFFF
!                 Minimum offset to mute[/R]  MUTE_MIN=`FFFFFFFFFFFFFFFFFFFF
!      Change in offset of the top slope[/R]   TOP_OFF=`FFFFFFFFFFFFFFFFFFFF
!       Change in depth of the top slope[/R]   TOP_DEP=`FFFFFFFFFFFFFFFFFFFF
!   Change in offset of the bottom slope[/R]   BOT_OFF=`FFFFFFFFFFFFFFFFFFFF
!    Change in depth of the bottom slope[/R]   BOT_DEP=`FFFFFFFFFFFFFFFFFFFF
!                      Mute taper length[/R]     TAPER=`FFFFFFFFFFFFFFFFFFFF
!             Select the Velocity Option[/R]    VELOPT=`CCCCCCCCCCCCCCCCCCCC
!                          Velocity file[/R]
!Select VELNAME[VELNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!         [VELNAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                    Refractor Horizon #[/R]   REF_HOR=`FFFFFFFFFFFFFFFFFFFF
!        Header word number for top salt[/R]  SALT_HNO=`FFFFFFFFFFFFFFFFFFFF
!`------------------------------------------------------------------------------
!<PARMS VELNAME[/ML=128/XST]>
!<PARMS VELNAME_INFO[/ML=128/XST]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="BOT_DEP">
!<Tip> -->Enter the change in depth of the bottom slope</Tip>
! Default = --> 2000
! Allowed = --> A real value
! --> The change in depth of the bottom slope
!</Help>
!
!<Help KEYWORD="BOT_OFF">
!<Tip> -->Enter the change in offset of the bottom slope</Tip>
! Default = --> 10000
! Allowed = --> A real value
! --> The change in offset of the bottom slope
!</Help>
!
!<Help KEYWORD="MUTE_MIN">
!<Tip> -->Enter the minimum offset to mute</Tip>
! Default = --> 6000
! Allowed = --> A real value
! --> The minimum offset to mute
!</Help>
!
!<Help KEYWORD="TAPER">
!<Tip> -->Enter the mute taper length</Tip>
! Default = --> 200
! Allowed = --> A real value
! --> The mute taper length
!</Help>
!
!<Help KEYWORD="REF_HOR">
!<Tip> -->Enter the refractor horizon number</Tip>
! Default = --> 2 
! Allowed = --> A number
! --> The refractor horizon number
!</Help>
!
!<Help KEYWORD="SALT_HNO">
!<Tip> -->Enter the header word number for the top salt</Tip>
! Default = --> 48
! Allowed = --> A Header word number
! --> Header word number for the top salt
!</Help>
!
!<Help KEYWORD="SALT_VEL">
!<Tip> -->Enter the velocity of the salt</Tip>
! Default = --> 14000
! Allowed = --> A real value
! --> The velocity of the salt
!</Help>
!
!<Help KEYWORD="SELECT_VELNAME" TYPE= "DISPLAY_ONLY">
!<Tip> --> Use the file selection dialog to choose a Velocity file.</Tip>
!</Help>
!
!<Help KEYWORD="TOP_DEP">
!<Tip> -->Enter the change in depth of the top slope</Tip>
! Default = --> 2000
! Allowed = --> A real value
! --> The change in depth of the top slope
!</Help>
!
!<Help KEYWORD="TOP_OFF">
!<Tip> -->Enter the change in offset of the top slope</Tip>
! Default = --> 10000
! Allowed = --> A real value
! --> The change in offset of the top slope
!</Help>
!
!<Help KEYWORD="TTYP_HNO">
!<Tip> -->Enter the header word number for the trace type</Tip>
! Default = --> 49
! Allowed = --> A Header word number
! --> Header word number for the trace type
!</Help>
!
!<Help KEYWORD="VELNAME">
!<Tip> -->Use a file selection dialog to choose an Velocity file</Tip>
! Default = --> NONE
! Allowed = --> A file name
! --> The velocity file
!</Help>
!
!<Help KEYWORD="VELNAME_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> --> Status of the velocity file.</Tip>
! Default = --> NONE
! Allowed = --> Status information
! --> Velocity file status information
!</Help>
!
!<Help KEYWORD="VELOPT">
!<Tip> -->Select the velocity option</Tip>
! Default = --> Velocity trace input
! Allowed = --> Velocity trace input
! Allowed = --> Velocity trace file
! Allowed = --> Velocity file
! Allowed = --> Modspec file
! --> Velocity trace input -- velocity trace data (TTYP_HNO=51) used
!     for variable window analysis (average velocities)
! --> Velocity trace file -- velocity trace file (TTYP_HNO=51) used
!     for variable window analysis (average velocities)
! --> Velocity File -- CPS velocity file (average velocities)
! --> Modspec File -- Modspec depth file (interval velocities)
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module tsmute_module
      use pc_module
      use named_constants_module
      use grid_module            ! if you need the grid transformation.
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.
      use rantfile_module        ! velocity trace file
      use velterp_module         ! CPS or Modspec velocity file

! --> Insert here any other modules used by this process.

      implicit none
      private
      public :: tsmute_create
      public :: tsmute_initialize
      public :: tsmute_update
      public :: tsmute_delete
      public :: tsmute            ! main trace processing routine.
      public :: tsmute_wrapup

      character(len=100),public,save :: TSMUTE_IDENT = &
'$Id: tsmute.f90,v 1.6 2007/11/30 13:55:20 Stoeckley beta sps $'

      character(len=FILENAME_LENGTH)  :: velname


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: tsmute_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.

! --> Below are commonly used globals - edit or remove as appropriate:

        integer                        :: ipn      ! process number.
        integer                        :: numtr    ! max number of input traces.
        logical                        :: gathered ! whether properly gathered.
        integer                        :: nwih     ! number of header words.
        integer                        :: ndpt     ! number of trace samples.
        real                           :: tstrt !time of 1st trace sample (sec).
        real                           :: dt    !trace sample interval (sec).
        type(grid_struct)              :: grid     ! grid transform.

        real                           :: bot_dep
        real                           :: bot_off
        integer                        :: ivelopt
        integer                        :: mxorig
        real                           :: mute_min
        real                           :: taper
        real                           :: ref_hor
        real                           :: salt_hno
        real                           :: salt_vel
        real                           :: top_dep
        real                           :: top_off
        real                           :: ttyp_hno
        real                           :: velbias
        real                           :: velscale
        integer                        :: velsmode
        character(len=FILENAME_LENGTH) :: velname
        character(len=21)              :: velopt
        integer                        :: x_hno
        integer                        :: y_hno

! --> Insert any other needed variables or pointers here.
        type(pathchoose_struct),pointer :: pathchoose1
        type(rantfile_struct),pointer   :: id_vtfile
        type(velterp_struct),pointer    :: id_vfile


! --> Insert any other needed variables or pointers here.

      end type tsmute_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


! --> Include any required interfaces here.


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                    ,save :: lunprint  ! unit number for printing.
      type(tsmute_struct),pointer,save :: object    ! needed for traps.

! --> Insert here any data declarations needed by this process.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine tsmute_create (obj)
      type(tsmute_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in tsmute_create")

      nullify (obj%pathchoose1)
      nullify (obj%id_vtfile) ! jpa
      nullify (obj%id_vfile) ! jpa

      call pathchoose_create (obj%pathchoose1, 'velname' , 'vel')


! --> Nullify any additional pointers in the OBJ data structure here.

      call tsmute_initialize (obj)
      end subroutine tsmute_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine tsmute_delete (obj)
      type(tsmute_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      call tsmute_wrapup (obj)


! --> Deallocate any additional pointers in the OBJ data structure here.
      if (associated(obj%pathchoose1)) call pathchoose_delete(obj%pathchoose1)

! --> close the velocity file
!
      if (obj%ivelopt.eq.3) then
        call rantfile_delete(obj%id_vtfile)
      elseif (obj%ivelopt.ge.4) then
        call velterp_delete(obj%id_vfile)
      end if

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in tsmute_delete")
      end subroutine tsmute_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine tsmute_initialize (obj)
      type(tsmute_struct),intent(inout) :: obj       ! arguments

! --> Change the default values below as needed

      obj%bot_dep      = 2000.0
      obj%bot_off      = 10000.0
      obj%ivelopt      = 2
      obj%mute_min     = 6000.0
      obj%mxorig       = 1000.0
      obj%taper        = 200.0
      obj%ref_hor      = 2.0
      obj%salt_hno     = 48.0
      obj%salt_vel     = 14000.0
      obj%top_dep      = 2000.0
      obj%top_off      = 10000.0
      obj%ttyp_hno     = 49.0
      obj%velbias      = 0.0
      obj%velscale     = 1.0
      obj%velsmode     = 1
      obj%velname      = PATHCHECK_EMPTY
      obj%velopt       = 'Velocity trace input'
      obj%x_hno        = 7
      obj%y_hno        = 8

      call tsmute_update (obj)
      end subroutine tsmute_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine tsmute_update (obj)
      type(tsmute_struct),intent(inout),target :: obj             ! arguments
!
      character cmsg*128, veltype*4
!
      integer ivelopt, lun, mxorig, norder
!
      logical lvreq, lerror
!
      real vsr

! --> Insert code to declare all required local variables.

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      if (pathchoose_update (obj%pathchoose1, obj%velname)) return


! --> Delete any of the globals below that are not needed:

      obj%ipn = pc_get_ipn()

      call pc_get_global ('numtr'   , obj%numtr)
      call pc_get_global ('gathered', obj%gathered)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)
      call pc_get_global ('grid'    , obj%grid)

      call pc_get('BOT_DEP ', obj%bot_dep)
      call pc_get('BOT_OFF ', obj%bot_off)
      call pc_get('MUTE_MIN', obj%mute_min)
      call pc_get('REF_HOR ', obj%ref_hor)
      call pc_get('SALT_HNO', obj%salt_hno)
      call pc_get('SALT_VEL', obj%salt_vel)
      call pc_get('TAPER   ', obj%taper)
      call pc_get('TOP_DEP ', obj%top_dep)
      call pc_get('TOP_OFF ', obj%top_off)
      call pc_get('TTYP_HNO', obj%ttyp_hno)
      call pc_get('VELNAME' , obj%velname)
      call pc_get('VELOPT'  , obj%velopt)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


! --> Insert code to verify process parameters here (and/or in traps).
!
! ..... Change options into integer values
!
      if (obj%velopt.eq.'Velocity trace input') then
        obj%ivelopt=2
      elseif (obj%velopt.eq.'Velocity trace file ') then
        obj%ivelopt=3
      elseif (obj%velopt.eq.'Modspec file        ') then
        obj%ivelopt=5
      else
        obj%ivelopt=4
      end if
!
!      Initialize needed variables
!
      ivelopt=obj%ivelopt
!
!      determine if velocity file is needed
!
      if (ivelopt.ge.3) then
        lvreq=.true.
      else
        lvreq=.false.
      end if
!
!      get velocity file
!
      if (ivelopt.eq.3) then
        call pathcheck('velname', obj%velname, &
                       '.trc', lvreq, show=PATHCHECK_INFO_INPUT)
      elseif (ivelopt.ge.4) then
        call pathcheck('velname', obj%velname, &
                       'vel', lvreq, show=PATHCHECK_INFO_INPUT)
      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


! --> Insert code to call internal processes to verify process parameters.


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put('velname' , obj%velname)
!
      if (ivelopt.eq.2) then
        call pc_put_sensitive_field_flag ('velname',        .false.)
        call pc_put_sensitive_field_flag ('select_velname', .false.)
        call pc_put_sensitive_field_flag ('SALT_HNO      ', .true.)
        call pc_put_sensitive_field_flag ('REF_HOR       ', .false.)
      else if (ivelopt.eq.5) then
        call pc_put_sensitive_field_flag ('velname',        .true.)
        call pc_put_sensitive_field_flag ('select_velname', .true.)
        call pc_put_sensitive_field_flag ('SALT_HNO      ', .false.)
        call pc_put_sensitive_field_flag ('REF_HOR       ', .true.)
      else
        call pc_put_sensitive_field_flag ('velname',        .true.)
        call pc_put_sensitive_field_flag ('select_velname', .true.)
        call pc_put_sensitive_field_flag ('SALT_HNO      ', .true.)
        call pc_put_sensitive_field_flag ('REF_HOR       ', .false.)
      end if
!
!
      call pc_put_options_field('VELOPT',(/'Velocity trace input',           &
        'Velocity trace file ','Velocity file       ','Modspec file        '/) )

! --> Delete any of the globals below that have not changed:

      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('gathered', obj%gathered)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)
      call pc_put_global ('grid'    , obj%grid)

      call pc_put('BOT_DEP ', obj%bot_dep)
      call pc_put('BOT_OFF ', obj%bot_off)
      call pc_put('MUTE_MIN', obj%mute_min)
      call pc_put('TAPER   ', obj%taper)
      call pc_put('REF_HOR',  obj%ref_hor)
      call pc_put('SALT_HNO', obj%salt_hno)
      call pc_put('SALT_VEL', obj%salt_vel)
      call pc_put('TOP_DEP ', obj%top_dep)
      call pc_put('TOP_OFF ', obj%top_off)
      call pc_put('TTYP_HNO', obj%ttyp_hno)
      call pc_put('VELNAME' , obj%velname)
      call pc_put('VELOPT'  , obj%velopt)

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
      call pc_put_control ('parallel_safe', .false.)
!
      call pc_put_visible_flag ('velname_info', lvreq )
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

!
!     SET NEEDED VARIABLES
!
      ivelopt=obj%ivelopt
      mxorig=obj%mxorig
      lun=6

      if (lvreq) then
! --> call rantfile to open a velocity trace file
        if (ivelopt.eq.3) then
          ! average velocities
          call rantfile_create(obj%id_vtfile, obj%velname, obj%nwih, &
            obj%ndpt, 0.0, obj%dt, obj%x_hno, obj%y_hno, lun, lerror, cmsg)
          if (lerror) call pc_error(cmsg)
! --> call velterp to open a modspec or CPS velocity file
        else if (ivelopt.ge.4) then
          norder = 2
          ! if CPS velocity file then just get average velocities
          ! if modspec file then convert from interval to average velocities
          veltype = 'VZAV'
          vsr = obj%dt
!           Put sample rate in Meters if the sample rate is kilometers
!           Put sample rate in Feet if the sample rate is kilofeet
!           (A sample rate less than one assumes kilometers or kilofeet)
          if (vsr.lt.1.0) then
            vsr = obj%dt*1000.0
          end if

          call velterp_create(obj%id_vfile, obj%velname, obj%ndpt, vsr,     &
            obj%velbias, obj%velscale, obj%velsmode, obj%x_hno, obj%y_hno,  &
            lerror, cmsg, norder, veltype)
          if (lerror) call pc_error(cmsg)
        end if
      end if

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine tsmute_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


! --> Insert code for parameter traps here.
! --> (EZCPS with the -t option will insert skeleton code for you)


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine tsmute (obj,ntr,hd,tr)
      type(tsmute_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

! --> Insert declarations of local variables.
      real, dimension(size(tr,1)) :: veltr  ! the velocity trace
!
      character cmsg*128
!
      double precision source_detect_dist_in
      double precision, dimension(size(hd,1)) :: velhd ! The vel headers
      integer data_trace_type_in
      integer i1, i2, ib, ibm, idead, ie, iem, ip, iref_hor
      integer isalt_hno, istrc, itp, its, itsflg, ittyp_hno
      integer ivelopt, ivtno, ivtrc, len, ltaper, max_samps
      integer nhdrs, norig, ntrc_gath_in, x_hno, y_hno
      logical lchange, lerror
      real bot_dep, bot_mute, bot_off, hc, mute_min, mxorig, rprec
      real salt_vel, samp_int_in, taper, top_dep, top_mute, top_off, ts
      real xcoord, ycoord
!
      real torig(1000)
!
      data  idead  / 0 /
      data  istrc  / 1 /
      data  ivtrc  /51 /
!
! --> Insert code for processing logic.
!
!      The following conditional call to the WRAPUP routine should be made
!      before returning from this subroutine:
!
      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
        call tsmute_wrapup(obj)
        return
      end if
!
!      Intialize
!
      bot_dep=obj%bot_dep
      bot_off=obj%bot_off
      iref_hor=obj%ref_hor
      isalt_hno=obj%salt_hno
      ittyp_hno=obj%ttyp_hno
      ivelopt=obj%ivelopt
      mute_min=obj%mute_min
      mxorig=obj%mxorig
      salt_vel=obj%salt_vel
      top_dep=obj%top_dep
      top_off=obj%top_off
      taper=obj%taper
      x_hno=obj%x_hno
      y_hno=obj%y_hno
!
!      initialize
!      (obj%ndpt is number of sample values in a trace)
!      (obj%nwih is number of header words)
!      (obj%dt is trace sample interval)
!
      max_samps=obj%ndpt
      nhdrs=obj%nwih
      samp_int_in=obj%dt*1000.0
      ntrc_gath_in=ntr
!
!      determine the trace type header number
!
      if (ittyp_hno.lt.1 .or. ittyp_hno.gt.nhdrs) then
        call pc_error("Invalid header number for the trace type")
        return
      end if
!
!        determine the top salt
!
      if (ivelopt.ne.5) then
        if (isalt_hno.ge.1 .and. isalt_hno.le.nhdrs) then
          ts = hd(isalt_hno,1)
        else
          call pc_error("Invalid header number for the top salt")
          return
        end if
      end if
!
! ..... set the taper length
!
      ltaper = taper/samp_int_in
      if (ltaper.le.1) ltaper=0
!
! .... Get needed velocity data
!
      if (ivelopt.eq.2) then
        veltr(1:max_samps)=0.0
        ivtno=0
        do i1 = 1, ntrc_gath_in
          data_trace_type_in = hd(ittyp_hno, i1 )
!
! ........   Copy trace data into the velocity array
          if (data_trace_type_in.eq.ivtrc) then
            ivtno=i2
            veltr(1:max_samps)=tr(1:max_samps,i1)
!            exit loop
            exit
          end if
        enddo

! ......   Display error if velocity trace not found
        if (ivtno.eq.0) then
          call pc_error ("Unable to find the velocity trace!!  TTYP_HNO=51")
        end if
!
! ...... Read the velocity trace file
!
      else if (ivelopt.eq.3) then
!
        xcoord = hd(x_hno, 1)
        ycoord = hd(y_hno, 1)
        call rantfile_find(obj%id_vtfile, xcoord, ycoord, velhd, veltr, &
          lerror, cmsg);
        if (lerror) then
           call pc_error(cmsg)
           return
        end if
!
! ..... Read the CPS or Modspec velocity file
!
      else if (ivelopt.ge.4) then
        xcoord = hd(x_hno, 1)
        ycoord = hd(y_hno, 1)
        call velterp_find(obj%id_vfile, xcoord, ycoord, veltr, lchange, &
          norig, torig)
!
!        make sure the refractor horizon is valid
!
        if (iref_hor.ge.1 .and. iref_hor.le.norig) then
          ts = torig(iref_hor)
        else if (iref_hor.gt.mxorig) then
          call pc_error("Reference refactor number must be 1000 or less")
        else
          call pc_error("Invalid reference refactor number")
          return
        end if
      end if
!
      do i1 = 1, ntrc_gath_in
!
        source_detect_dist_in = hd( hdr_offset, i1 )
        data_trace_type_in = hd( ittyp_hno, i1 )
 
!         determine top salt value
        if (ivelopt.ge.4) then
          ts = torig(iref_hor)
          its=(ts/samp_int_in)+1
        else
          ts = hd(isalt_hno,i1)
          its=(ts/samp_int_in)+1
        end if
!
!*********************************************************************
!*******************  PROCESS TRACE NUMBER "I"  *******************
!*********************************************************************
!
!         look only at seismic traces
!
        if (data_trace_type_in.eq.istrc .or.  &
            data_trace_type_in.eq.idead) then
!
!          determine offset to start muting
!
          itsflg=0
          if (its.ge.1 .and. its.le.max_samps) then
            hc=2.0*ts*(veltr(its)/sqrt((salt_vel**2)-(veltr(its)**2)))
            itsflg=1
          end if
!
!           make sure offset to mute is greater than minimum
!
          if (hc.lt.mute_min) hc=mute_min
!
!           determine the top of mute
!
          if (source_detect_dist_in.ge.hc) then
            top_mute=ts-(top_dep/top_off)*(abs(source_detect_dist_in)-hc)
            ibm=(top_mute/samp_int_in)+1
            if (ibm.lt.1) ibm=1
            if (ibm.gt.max_samps) itsflg=0
!           if (ibm.lt.1 .or. ibm.gt.max_samps) itsflg=0
!
!           determine the bottom of mute
!
            bot_mute=ts+(bot_dep/bot_off)*(abs(source_detect_dist_in)-hc)
            iem=(bot_mute/samp_int_in)+1
            if (iem.gt.max_samps) iem=max_samps
            if (iem.lt.1) itsflg=0
!           if (iem.lt.1 .or. iem.gt.max_samps) itsflg=0
          else
            itsflg=0
          end if
!
!          taper the window
!
          if (ltaper.gt.0 .and. itsflg.eq.1) then
            do i2=1,ltaper
              itp=i2
              ip=ibm-ltaper+i2
              if (ip.ge.1) then
                rprec=float(ltaper-itp)/float(ltaper)
                tr(ip,i1)=tr(ip,i1)*rprec
              end if
            end do
!
            ib=iem
            ie=iem+ltaper-1
            if (ie.gt.max_samps) ie=max_samps
            len=ie-ib+1
            if (len.gt.1) then
              do i2=1,len
                itp=i2-1
                rprec=float(itp)/float(ltaper)
                ip=ib+i2-1
                tr(ip,i1)=tr(ip,i1)*rprec
              end do
            end if
          end if
!
!          zero out trace values to be muted
!
          if (itsflg.eq.1) then
            if (iem.ge.ibm) then
              tr(ibm:iem,i1)=0.0
            end if
          end if
        end if
!------------------------------------------------------------------
!
      end do
!
      end subroutine tsmute


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine tsmute_wrapup (obj)
      type(tsmute_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

! --> Insert any required wrapup code here, including wrapups of
! --> internally-called processes.

      end subroutine tsmute_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module tsmute_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

