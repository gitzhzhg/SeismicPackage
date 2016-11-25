!<CPS_v1 type="PROCESS"/>
!!------------------------------- alamo.f90 ---------------------------------!!
!!------------------------------- alamo.f90 ---------------------------------!!
!!------------------------------- alamo.f90 ---------------------------------!!

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
! Name       : ALAMO
! Category   : velocity_analysis
! Written    : 2003-04-22   by: Michael Ried
! Revised    : 2007-11-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Analysis of Long array moveout
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! For layered media characterized by large vertical velocity gradients and/or
! velocity anisotropy, the moveout on reflected events may deviate from the
! hyperbolic assumption.  In order to "flatten" events in such media, the
! deviations from hyperbolic moveout need to be calculated.  ALAMO uses a
! simple fourth order (3-Term) formula to characterize these deviations. 
!
!  T**2 = T0**2 + (X/Vnmo)**2 - 2*ETA*(1/T0**2)*(X/Vnmo)**4
!
! After ALAMO is used to automatically calculate the ETA field, then 3-Term
! Moveout can be used to apply the velocity and ETA fields to the seismic
! data in order to flatten the events.
!
! ALAMO calculates an ETA volume by scanning a range of ETA values and then
! picking the ETA value with the largest semblance.  The picked ETA volume
! varies as a function of location and time.
!
! The following traces are output from ALAMO:
!   1. ETA traces   ( Header 49 = 70 )
!
! The above traces are output from ALAMO for each input CDP gather.
!
! See the ADVICE FOR USERS section for a sample work flow.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! The input seismic data to ALAMO should NOT have moveout applied.  The input
! velocity field should be derived from a velocity analysis performed on the
! relatively short offset portion (where the deviations from hyperbolic moveout
! are relatively small) of the CDP gathers.  This input velocity field will 
! normally be the smoothed version of the velocities that were computed using
! the AVOVIT process.  
!
! Like AVOVIT, ALAMO is a 2D algorithm and a semblance based approach. 
! It operates, in the case of 3D data, on a single inline at a time. 
! Because of this, there is the possibility of creating crossline "jitter"
! in the resulting ETA field.  Since ALAMO outputs a ETA function at each
! CDP location, high frequency variations in the ETA field can even occur in
! the inline direction depending on parameter selection and data quality. 
!
! Because of these inherent problems, it is highly recommended to apply some 
! amount of smoothing to the output ETA traces before they are used to help
! flatten CDP gathers. 
!
! In addition, application of a very mild smoothing filter (in space and/or
! time) to the ETA field may improve moveout results and will reduce the
! stretching and compression of the seismic wavelet on the far offsets.  The
! MIX process can be used to accomplish this smoothing.
!
! Sample ALAMO work flow
!
! TRIN - Input prestack CDP gathers with NO normal moveout correction applied.
! Data conditioning processes can be applied here if necessary.
! GATHER
! ALAMO
! SELECT - Select ETA traces for output (HW 49 = 70)
! TROT
! 
! A window length of 80 ms is a reasonable starting point.  You don't want the
! window to be so small that too few events are included within the window.
! The algorithm does track event curvature.  Like AVOVIT, ALAMO uses a constant
! window length for all times.  An 80 ms window length may be fine at shallower
! times, but may be ridiculous at 6 seconds.  Set your window start and end
! times appropriately.
!
! There are different types of weighting that can be done within ALAMO to edit
! the ETA field.  The defaults are a reasonable starting point.  
! However, one should always output an ETA field with no editing applied for
! comparison.  Note that this editing should not be a substitute for 
! smoothing the ETA field.  A sample flow to smooth the ETA field is simply
!
! TRIN - ETA traces from ALAMO 
! MIX
! TROT - Output smoothed ETA traces
! 
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Velocity Trace (Needed if not using a velocity file)
! Seismic Traces (No normal moveout correction applied)
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! Velocity trace (If selected)
! Semblance trace (If selected)
! Complex Semblance Analysis traces (If selected)
! ETA trace (If selected)
! Seismic traces
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
! NUMTR     max number of traces input/output       # of traces will vary
! GATHERED  whether traces are a legitimate gather  Input & Output in gathers
! NWIH      number of words in trace header         None
! NDPT      number of sample values in trace        None
! TSTRT     starting time on trace                  None
! DT        trace sample interval                   None
! GRID      grid transformation structure           None
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
! **(Trace Types: 0=Dead trace,1=Seismic trace,51=Velocity trace,70=ETA trace,
!    71=Semblance trace, 72=Zero or separator trace)
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
!  5. 2006-11-14  D. Glover     Added NULLIFY statements for Intel compiler.
!  4. 2006-01-17  B. Menger     Removed Unused Variables.
!  3. 2005-01-31  Michael Ried  New changes to ALAMO:
!                               -Allowed for parrellel processors
!                               -It calls velterp/rantfile to get velocity data
!                               -Seperated screen parameters not used much
!                               -Changed parameter units ms->sec
!                               -Updated documentation
!                               -Set new default values for screen parameters
!  2. 2003-09-03  Michael Ried  New changes to ALAMO:
!                               -Looks for .vel velocity files
!                               -Eliminated the ETA output gather option
!                               -Headers are defaulted to trace headers
!  1. 2003-04-22  Michael Ried  Initial version.
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
!<NS Main_Menu/NC=80>
!                       CPS_ALAMO - Analysis of Long Array Moveout
!`--------------------------------------------------------------------------
!            Beginning Time for Analysis[/R]   S_TIME=`FFFFFFFFFFFFFFFFFFFF
!               Ending Time for Analysis[/R]   E_TIME=`FFFFFFFFFFFFFFFFFFFF
!             Window Length for Analysis[/R]    L_WIN=`FFFFFFFFFFFFFFFFFFFF
!          Window increment for Analysis[/R]    I_WIN=`FFFFFFFFFFFFFFFFFFFF
!                     Taper Length (sec)[/R]  L_TAPER=`FFFFFFFFFFFFFFFFFFFF
!            Maximum Stretch (%) Allowed[/R]  STR_MAX=`FFFFFFFFFFFFFFFFFFFF
!                            Minimum ETA[/R]  ETA_MIN=`FFFFFFFFFFFFFFFFFFFF
!                            Maximum ETA[/R]  ETA_MAX=`FFFFFFFFFFFFFFFFFFFF
!                          ETA Increment[/R]  ETA_INC=`FFFFFFFFFFFFFFFFFFFF
!`--------------------------------------------------------------------------
!`--------------------------------------------------------------------------
!       Select the Velocity Input Option[/R]   VELOPT=`CCCCCCCCCCCCCCCCCCCCC
!                          Velocity file[/R]
!Select VELNAME[VELNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!         [VELNAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!`--------------------------------------------------------------------------
!`--------------------------------------------------------------------------
!      Select the ETA Output File Option[/R]   ETAOPT=`CCCCCCCCCCCCCCCCCCCCC
!                               ETA File[/R]
!Select ETANAME[ETANAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!         [ETANAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!       Use Semblances to edit the field[/R]    EDITO=`CCCCCCCCCCCCCCCCCCCC
!     Maximum Interpolation Length (sec)[/R]   RI_MAX=`FFFFFFFFFFFFFFFFFFFF
!  Extrapolation Type (Beginning/Ending)[/R]    EXTRP=`CCCCCCCCCCCCCCCCCCCC
!          Semblance (%) Threshold Value[/R]  SMB_MIN=`FFFFFFFFFFFFFFFFFFFF
!   Exponent to Apply to Semblance Ratio[/R]  SMB_EXP=`FFFFFFFFFFFFFFFFFFFF
!Smoothing filter to edit Eta field type[/R]    MFLTO=`CCCCCCCCCCCCCCCCCCCC
!      Median or Mean filter length(sec)[/R]     FLEN=`FFFFFFFFFFFFFFFFFFFF
!`--------------------------------------------------------------------------
!<NS Diagnostic_Outputs/NC=80>
!              Additional Output Options[/R]    AOUTO=`CCCCCCCCCCCCCCCCCCCCC
!                Output Velocity Traces?[/R]    VTOPT=`KKK
!`--------------------------------------------------------------------------
!     Select the Output Semblance Option[/R]   SIGOPT=`CCCCCCCCCCCCCCCCCCCCC
!                  Output Semblance File[/R]
!Select SIGNAME[SIGNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!         [SIGNAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!`--------------------------------------------------------------------------
!<PARMS ETANAME[/ML=128/XST]>
!<PARMS SIGNAME[/ML=128/XST]>
!<PARMS VELNAME[/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="AOUTO">
!<Tip> -->Choose an additional output option</Tip>
! Default = --> No added outputs
! Allowed = --> No added outputs
! Allowed = --> Semblance analysis
! Allowed = --> Seismic traces
! --> No added outputs -- No additional output traces wanted
! --> Semblance analysis -- Output Comprehensive SEMBLANCE Analysis traces
! --> Seismic traces -- Output seismic traces
!</Help>
!
!<Help KEYWORD="EDITO">
!<Tip> -->Choose the type of eta field editing</Tip>
! Default = --> Semblance weighting
! Allowed = --> Interpolation
! Allowed = --> Semblance weighting
! Allowed = --> Zero edit
! Allowed = --> No Editing
! --> Interpolation -- Edit values by Interpolating
! --> Semblance weighting -- Edit values using semblance weighting
! --> Zero edit -- Eta values not satisfying the semblance threshold are
!     set to a small value
! --> No Editing -- Do not use semblances to edit eta fields
!</Help>
!
!<Help KEYWORD="ETANAME">
!<Tip> --> Use the file selection dialog to choose a ETA file.</Tip>
! Default = --> NONE
! Allowed = --> A file name
! --> Eta file
!</Help>
!
!<Help KEYWORD="ETANAME_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> --> Status of the ETA file.</Tip>
! Default = --> NONE
! Allowed = --> Status information
! --> Eta file status information
!</Help>
!
!<Help KEYWORD="ETAOPT">
!<Tip> -->Select the ETA output file option</Tip>
! Default = --> ETA trace data
! Allowed = --> ETA trace data
! Allowed = --> ETA file
! --> ETA trace data -- Output ETA trace data
! --> ETA file -- Select an output ETA file
!</Help>
!
!<Help KEYWORD="ETA_INC">
!<Tip> -->Choose increment in parameter eta (decimal)</Tip>
! Default = --> 0.005
! Allowed = --> A Real value
! --> Eta increment (decimal)
!</Help>
!
!<Help KEYWORD="ETA_MAX">
!<Tip> -->Choose maximum anellipticity parameter eta (decimal)</Tip>
! Default = --> 0.2
! Allowed = --> A Real value
! --> Maximum eta (decimal)
!</Help>
!
!<Help KEYWORD="ETA_MIN">
!<Tip> -->Choose minimum anellipticity parameter eta (decimal)</Tip>
! Default = --> 0.0
! Allowed = --> A Real value
! --> Minimum eta (decimal)
!</Help>
!
!<Help KEYWORD="EXTRP">
!<Tip> -->Choose the type of extrapolation at the beginning and ending</Tip>
! Default = --> Semblance weighting
! Allowed = --> Semblance weighting
! Allowed = --> Zero edit
! --> Type of extrapolation at the beginning and ending
!</Help>
!
!<Help KEYWORD="E_TIME">
!<Tip> -->Choose end time (sec)</Tip>
! Default = --> 3.0
! Allowed = --> A Real value
! --> Ending time for analyis (sec)
!</Help>
!
!<Help KEYWORD="FLEN">
!<Tip> -->Choose the median or mean filter length (sec)</Tip>
! Default = --> 0.08
! Allowed = --> A Real value
! --> Median or mean filter length (sec)
!</Help>
!
!<Help KEYWORD="I_WIN">
!<Tip> -->Choose analysis start time increment (sec)</Tip>
! Default = --> 0.02
! Allowed = --> A Real value
! --> Window increment for analysis (sec)
!</Help>
!
!<Help KEYWORD="L_TAPER">
!<Tip> -->Choose Taper length to apply on analysis window (sec)</Tip>
! Default = --> 0.012
! Allowed = --> A Real value
! --> Taper length (sec)
!</Help>
!
!<Help KEYWORD="L_WIN">
!<Tip> -->Choose analysis gate length (sec)</Tip>
! Default = --> 0.08
! Allowed = --> A Real value
! --> Window length for analysis (sec)
!</Help>
!
!<Help KEYWORD="MFLTO">
!<Tip> -->Choose when to perform a median or mean smoothing filter.</Tip>
! Default = --> Mean after
! Allowed = --> No filter
! Allowed = --> Median before
! Allowed = --> Median after
! Allowed = --> Mean before
! Allowed = --> Mean after
! --> No filter -- Do not apply a median or mean smoothing filter to the data
! --> Median before -- Apply median filter before the eta field edit
! --> Median after -- Apply median filter after the eta field edit
! --> Mean before -- Apply mean filter before the eta field edit
! --> Mean after -- Apply mean filter after the eta field edit
!</Help>
!
!<Help KEYWORD="RI_MAX">
!<Tip> -->Determine the maximum length to perform an interpolation</Tip>
! Default = --> 0.2
! Allowed = --> A Real value
! --> Maximum Interpolation length (sec)
!</Help>
!
!<Help KEYWORD="SELECT_ETANAME">
!<Tip> --> Use the file selection dialog to choose a ETA file.</Tip>
!</Help>
!
!<Help KEYWORD="SELECT_SIGNAME">
!<Tip> --> Use the file selection dialog to choose a SEMBLANCE file.</Tip>
!<Tip> Choose OUTPUT_FILE using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_VELNAME">
!<Tip> --> Use the file selection dialog to choose a Velocity file.</Tip>
!</Help>
!
!<Help KEYWORD="SIGNAME_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> --> Status of the Semblance file.</Tip>
! Default = --> NONE
! Allowed = --> Status information
! --> Semblance file status information
!</Help>
!
!<Help KEYWORD="SIGNAME">
!<Tip> --> Use the file selection dialog to choose a SEMBLANCE file.</Tip>
! Default = --> NONE
! Allowed = --> A File Name
! --> Output SEMBLANCE file
!</Help>
!
!<Help KEYWORD="SIGOPT">
!<Tip> -->Choose an output SEMBLANCE file Option</Tip>
! Default = --> No SEMBLANCE output
! Allowed = --> No SEMBLANCE output
! Allowed = --> SEMBLANCE trace data
! Allowed = --> SEMBLANCE file
! --> No SEMBLANCE output -- Do not output SEMBLANCE data
! --> SEMBLANCE trace data -- Output SEMBLANCE trace data
! --> SEMBLANCE file -- Select an output SEMBLANCE file
!</Help>
!
!<Help KEYWORD="SMB_EXP">
!<Tip> -->Choose a semblance exponenent used to edit eta values</Tip>
! Default = --> 2.0
! Allowed = --> A Real value
! --> Exponent to apply to semblance ratio
!</Help>
!
!<Help KEYWORD="SMB_MIN">
!<Tip> --> Choose minimum semblance threshold value in percent</Tip>
! Default = --> 15.0
! Allowed = --> A Real value
! --> Semblance (%) threshold value
!</Help>
!
!<Help KEYWORD="STR_MAX">
!<Tip> --> Maximum stretch (%) allowed</Tip>
! Default = --> 80.0
! Allowed = --> A Real value
! --> Maximum stretch (%) allowed
!</Help>
!
!<Help KEYWORD="S_TIME">
!<Tip> --> Choose start time (sec)</Tip>
! Default = --> 0.2
! Allowed = --> A Real value
! --> Beginning time for analyis (sec)
!</Help>
!
!<Help KEYWORD="VELNAME">
!<Tip> --> Use the file selection dialog to choose a velocity file.</Tip>
! Default = --> NONE
! Allowed = --> A file name
! --> SELECT a velocity file
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
!<Tip> -->Choose a Velocity Option. Select a CPS velocity file or a trace</Tip>
! Default = --> Velocity trace input
! Allowed = --> CPS Velocity file
! Allowed = --> Velocity trace input
! Allowed = --> Velocity trace file
! --> CPS velocity file -- Select a CPS velocity file
! --> Velocity trace input -- A velocity trace follows the input seismic data
! --> Velocity trace file -- Select a trace file
!</Help>
!
!<Help KEYWORD="VTOPT">
!<Tip> -->If 'Yes', the velocity trace data will be outputted.</Tip>
! Default = --> NO
! Allowed = --> YES
! Allowed = --> NO
! --> Output velocity traces?
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module alamo_module
      use ap1_module
      use grid_module            ! if you need the grid transformation.
      use named_constants_module
      use mem_module
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.
      use pc_module
      use permtfile_module
      use rantfile_module
      use velterp_module

! --> Insert here any other modules used by this process.
      implicit none
      private
      public :: alamo_create
      public :: alamo_initialize
      public :: alamo_update
      public :: alamo_delete
      public :: alamo            ! main trace processing routine.
      public :: alamo_wrapup

      public :: alamo_ed_eta
      public :: alamo_meanf
      public :: alamo_medf
      public :: alamo_sava3tc
      public :: alamo_tdata

      character(len=100),public,save :: ALAMO_IDENT = &
'$Id: alamo.f90,v 1.6 2007/11/30 13:55:16 Stoeckley beta sps $'

      character(len=FILENAME_LENGTH)  :: etaname
      character(len=FILENAME_LENGTH)  :: signame
      character(len=FILENAME_LENGTH)  :: velname


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: alamo_struct

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

        character(len=21)          :: aouto
        character(len=21)          :: edito
        character(len=128)         :: etaname
        character(len=21)          :: etaopt
        real                       :: eta_inc
        real                       :: eta_max
        real                       :: eta_min
        character(len=21)          :: extrp
        real                       :: e_time
        real                       :: flen
        real                       :: i_win
        real                       :: l_taper
        real                       :: l_win
        character(len=21)          :: mflto
        real                       :: ri_max
        character(len=128)         :: signame
        character(len=21)          :: sigopt
        real                       :: smb_exp
        real                       :: smb_min
        real                       :: str_max
        real                       :: s_time
        character(len=128)         :: velname
        character(len=21)          :: velopt
        logical                    :: vtopt

        integer                    :: iaouto
        integer                    :: iedito
        integer                    :: iextrp
        integer                    :: ietaopt
        integer                    :: imflto
        integer                    :: iflen
        integer                    :: inc
        integer                    :: inf
        integer                    :: isigopt
        integer                    :: ist
        integer                    :: ivelopt
        integer                    :: ivtopt
        integer                    :: lop
        integer                    :: lwin
        integer                    :: ltaper
        integer                    :: nana
        integer                    :: n_eta
        integer                    :: nop
        integer                    :: nwin
        real                       :: velbias
        real                       :: velscale
        integer                    :: velsmode
        integer                    :: x_hno
        integer                    :: y_hno

        integer                    :: ix_tc
        integer                    :: ix_ms
        integer                    :: ix_sl
        integer                    :: ix_et
        integer                    :: ix_sx
        integer                    :: ix_s0
        integer                    :: ix_nn
        integer                    :: ix_sm
        integer                    :: ix_0n
        integer                    :: ix_eta
        integer                    :: ix_op
        integer                    :: ix_s1
        integer                    :: ix_csa
        integer                    :: ix_bm
        integer                    :: ix_bm2
        integer                    :: ix_bm3
        integer                    :: ix_tmt
        integer                    :: ix_smt
        integer                    :: ix_ett
        integer                    :: ix_edt
        integer                    :: ix_temp

! --> Insert any other needed variables or pointers here.
   type(pathchoose_struct),pointer :: pathchoose1
   type(pathchoose_struct),pointer :: pathchoose2
   type(pathchoose_struct),pointer :: pathchoose3
   type(permtfile_struct),pointer   :: id_sfile
   type(permtfile_struct),pointer   :: id_efile
   type(rantfile_struct),pointer    :: id_vtfile
   type(velterp_struct),pointer     :: id_vfile

        real              ,pointer :: wa1(:) ! work array #1
        real              ,pointer :: wa2(:) ! work array #2
        integer           ,pointer :: iwa(:) ! integer work array

      end type alamo_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


! --> Include any required interfaces here.


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(alamo_struct),pointer,save :: object    ! needed for traps.

! --> Insert here any data declarations needed by this process.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine alamo_create (obj)
      type(alamo_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in alamo_create")

! --> Nullify any additional pointers in the OBJ data structure here.
      nullify(obj%wa1)   ! must be done for all pointers.
      nullify(obj%wa2)
      nullify(obj%iwa)
      nullify (obj%pathchoose1) ! jpa
      nullify (obj%pathchoose2) ! jpa
      nullify (obj%pathchoose3) ! jpa
      nullify (obj%id_sfile) ! jpa
      nullify (obj%id_efile) ! jpa
      nullify (obj%id_vtfile) ! jpa
      nullify (obj%id_vfile) ! jpa

      call pathchoose_create (obj%pathchoose1, 'etaname'  , '.trc')
      call pathchoose_create (obj%pathchoose2, 'signame' , '.trc')
      call pathchoose_create (obj%pathchoose3, 'velname', '.vel')

      call alamo_initialize (obj)
      end subroutine alamo_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine alamo_delete (obj)
      type(alamo_struct),pointer :: obj      ! arguments
      integer                   :: ierr      ! for error checking
!      
      call pathchoose_delete (obj%pathchoose1)
      call pathchoose_delete (obj%pathchoose2)
      call pathchoose_delete (obj%pathchoose3)

      call alamo_wrapup (obj)


! --> Deallocate any additional pointers in the OBJ data structure here.

      if (associated(obj%wa1)) deallocate(obj%wa1)
      if (associated(obj%wa2)) deallocate(obj%wa2)
      if (associated(obj%iwa)) deallocate(obj%iwa)

!      close the eta file and the semblance file
      if (obj%ietaopt .eq. 2) then
        call permtfile_close(obj%id_efile)
      end if
      if (obj%isigopt .eq. 2) then
        call permtfile_close(obj%id_sfile)
      end if
      if (obj%ivelopt.eq.1) then
        call velterp_delete(obj%id_vfile)
      else if (obj%ivelopt.eq.3) then
        call rantfile_delete(obj%id_vtfile)
      end if

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in alamo_delete")
      end subroutine alamo_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine alamo_initialize (obj)
      type(alamo_struct),intent(inout) :: obj       ! arguments

! --> Change the default values below as needed

      obj%aouto        = 'No Added Outputs'
      obj%edito        = 'Semblance weighting'
      obj%etaname      = 'NONE'
      obj%etaopt       = 'ETA trace data'
      obj%eta_inc      = 0.005
      obj%eta_max      = 0.2
      obj%eta_min      = 0.0
      obj%extrp        = 'Semblance weighting'
      obj%e_time       = 3.0
!
!      Calculate the end time
!
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('dt'      , obj%dt)
      obj%e_time = obj%tstrt + (obj%ndpt-1)*obj%dt
!
      obj%flen         = 0.08
      obj%i_win        = 0.02
      obj%l_taper      = 0.012
      obj%l_win        = 0.08
      obj%mflto        = 'Mean after'
      obj%ri_max       = 0.2
      obj%signame      = 'NONE'
      obj%sigopt       = 'No SEMBLANCE output '
      obj%smb_exp      = 2.0
      obj%smb_min      = 15.0
      obj%str_max      = 80.0
      obj%s_time       = 0.2
      obj%velname      = 'NONE'
      obj%velopt       = 'Velocity trace input '
      obj%vtopt        = .FALSE.

      obj%iaouto       = 1
      obj%iedito       = 2
      obj%iextrp       = 0
      obj%ietaopt      = 1
      obj%imflto       = 5
      obj%iflen        = 0
      obj%inc          = 1
      obj%inf          = 5
      obj%isigopt      = 0
      obj%ist          = 1
      obj%ivelopt      = 2
      obj%ivtopt       = 0
      obj%lop          = 10
      obj%lwin         = 1
      obj%ltaper       = 0
      obj%nana         = 1
      obj%n_eta        = 1
      obj%nop          = 101
      obj%nwin         = 1
      obj%velbias      = 0.0
      obj%velscale     = 1.0
      obj%velsmode     = 1
      obj%x_hno        = 7
      obj%y_hno        = 8

      obj%ix_tc        = 1
      obj%ix_ms        = 1
      obj%ix_sl        = 1
      obj%ix_et        = 1
      obj%ix_sx        = 1
      obj%ix_s0        = 1
      obj%ix_nn        = 1
      obj%ix_sm        = 1
      obj%ix_0n        = 1
      obj%ix_eta       = 1
      obj%ix_op        = 1
      obj%ix_s1        = 1
      obj%ix_csa       = 1
      obj%ix_bm        = 1
      obj%ix_bm2       = 1
      obj%ix_bm3       = 1
      obj%ix_tmt       = 1
      obj%ix_smt       = 1
      obj%ix_ett       = 1
      obj%ix_edt       = 1
      obj%ix_temp      = 1

      obj%etaname      = PATHCHECK_EMPTY
      obj%signame      = PATHCHECK_EMPTY
      obj%velname      = PATHCHECK_EMPTY

      call alamo_update (obj)
      end subroutine alamo_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine alamo_update (obj)
      type(alamo_struct),intent(inout),target :: obj             ! arguments

      CHARACTER  :: CMSG*128, CWHIST*7
      integer :: IAOUTO,IEDITO, IERR, IET, IETAOPT, IFLEN, IMFLTO, INC, INF, IPN
      integer :: ISIGOPT, IST, IVELOPT, IVTOPT
      integer :: LIWA, LOP, LTAPER, L_TAPER, LUN, LWA1, LWA2, LWIN
      integer :: MAX_SAMPS, MRECS
      integer :: NANA, NBITS, N_ETA, NOP, NTVALS, NUMTR_IN, NUMTR_OUT  
      integer :: NVALS, NWIN
      logical :: LEREQ, LERROR, LSREQ, LVREQ
      real :: ETA_INC, ETA_MAX, ETA_MIN, E_TIME, FLEN, I_WIN, L_WIN
      real :: SAMP_INT_IN, SMB_MIN, S_TIME

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      if (pathchoose_update (obj%pathchoose1, obj%etaname)) return
      if (pathchoose_update (obj%pathchoose2, obj%signame)) return
      if (pathchoose_update (obj%pathchoose3, obj%velname)) return

! --> Delete any of the globals below that are not needed:

      obj%ipn = pc_get_ipn()

      call pc_get_global ('numtr'   , obj%numtr)
      call pc_get_global ('gathered', obj%gathered)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)
      call pc_get_global ('grid'    , obj%grid)

      call pc_get('AOUTO  ', obj%aouto)
      call pc_get('EDITO  ', obj%edito)
      call pc_get('ETANAME', obj%etaname)
      call pc_get('ETAOPT ', obj%etaopt)
      call pc_get('ETA_INC', obj%eta_inc)
      call pc_get('ETA_MAX', obj%eta_max)
      call pc_get('ETA_MIN', obj%eta_min)
      call pc_get('EXTRP  ', obj%extrp)
      call pc_get('E_TIME ', obj%e_time)
      call pc_get('FLEN   ', obj%flen)
      call pc_get('I_WIN  ', obj%i_win)
      call pc_get('L_TAPER', obj%l_taper)
      call pc_get('L_WIN  ', obj%l_win)
      call pc_get('MFLTO  ', obj%mflto)
      call pc_get('RI_MAX ', obj%ri_max)
      call pc_get('SIGNAME', obj%signame)
      call pc_get('SIGOPT ', obj%sigopt)
      call pc_get('SMB_EXP', obj%smb_exp)
      call pc_get('SMB_MIN', obj%smb_min)
      call pc_get('STR_MAX', obj%str_max)
      call pc_get('S_TIME ', obj%s_time)
      call pc_get('VELNAME', obj%velname)
      call pc_get('VELOPT ', obj%velopt)
      call pc_get('VTOPT  ', obj%vtopt)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


! --> Insert code to verify process parameters here (and/or in traps).
!
! ..... Change options into integer values
!
      IF (OBJ%VELOPT.EQ.'Velocity trace input ') THEN
        OBJ%IVELOPT=2
      ELSE IF (OBJ%VELOPT.EQ.'Velocity trace file  ') THEN
        OBJ%IVELOPT=3
      ELSE
        OBJ%IVELOPT=1
      END IF
!
      IF (OBJ%VTOPT) THEN
        OBJ%IVTOPT=1
      ELSE
        OBJ%IVTOPT=0
      END IF
!
      IF (OBJ%SIGOPT.EQ.'Semblance trace data ') THEN
        OBJ%ISIGOPT=1
      ELSE IF (OBJ%SIGOPT.EQ.'Semblance file       ') THEN
        OBJ%ISIGOPT=2
      ELSE
        OBJ%ISIGOPT=0
      END IF
!
      IF (OBJ%ETAOPT.EQ.'ETA file             ') THEN
        OBJ%IETAOPT=2
      ELSE
        OBJ%IETAOPT=1
      END IF
!
      IF (OBJ%EDITO.EQ.'Interpolation        ') THEN
        OBJ%IEDITO=1
      ELSE IF (OBJ%EDITO.EQ.'Semblance weighting  ') THEN
        OBJ%IEDITO=2
      ELSE IF (OBJ%EDITO.EQ.'Zero edit            ') THEN
        OBJ%IEDITO=3
      ELSE
        OBJ%IEDITO=4
      END IF
!
      IF (OBJ%IEDITO.EQ.1) THEN
        IF (OBJ%EXTRP.EQ.'Semblance weighting  ') THEN
          OBJ%IEXTRP=1
        ELSE
          OBJ%IEXTRP=2
        END IF
      ELSE
        OBJ%IEXTRP=0
      END IF
!
      IF (OBJ%MFLTO.EQ.'Median before        ') THEN
        OBJ%IMFLTO=2
      ELSE IF (OBJ%MFLTO.EQ.'Median after        ') THEN
        OBJ%IMFLTO=3
      ELSE IF (OBJ%MFLTO.EQ.'Mean before         ') THEN
        OBJ%IMFLTO=4
      ELSE IF (OBJ%MFLTO.EQ.'Mean after          ') THEN
        OBJ%IMFLTO=5
      ELSE
        OBJ%IMFLTO=1
      END IF
!
      IF (OBJ%AOUTO.EQ.'Semblance analysis   ') THEN
        OBJ%IAOUTO=2
      ELSE IF (OBJ%AOUTO.EQ.'Seismic traces       ') THEN
        OBJ%IAOUTO=3
      ELSE
        OBJ%IAOUTO=1
      END IF
!
!      Initialize needed variables
!
      IAOUTO=OBJ%IAOUTO
      IEDITO=OBJ%IEDITO
      IETAOPT=OBJ%IETAOPT
      IMFLTO=OBJ%IMFLTO
      ISIGOPT=OBJ%ISIGOPT
      IVELOPT=OBJ%IVELOPT
      IVTOPT=OBJ%IVTOPT
      ETA_MAX = OBJ%ETA_MAX*100.0
      ETA_MIN = OBJ%ETA_MIN*100.0
      ETA_INC = OBJ%ETA_INC*100.0
!
!      determine the number of output traces
!
      NUMTR_IN = OBJ%NUMTR
      NUMTR_OUT = 0
      IF (IVTOPT.EQ.1) THEN
        NUMTR_OUT=NUMTR_OUT+1
      END IF
      IF (ISIGOPT.EQ.1) THEN
        NUMTR_OUT=NUMTR_OUT+1
      END IF
      IF (IETAOPT.EQ.1) THEN
        NUMTR_OUT=NUMTR_OUT+1
      END IF
      IF (IAOUTO.EQ.2) THEN
        N_ETA = 1+ (ETA_MAX-ETA_MIN)/ETA_INC
        NUMTR_OUT = NUMTR_OUT+N_ETA+1
      ELSE IF (IAOUTO.EQ.3) THEN
        NUMTR_OUT = NUMTR_OUT+NUMTR_IN+1
      END IF
!       change max number of traces if necessary
      IF (NUMTR_OUT.GT.NUMTR_IN) THEN
        OBJ%NUMTR=NUMTR_OUT
      END IF
!
      IF (IETAOPT.EQ.2) THEN
        LEREQ=.TRUE.
      ELSE
        LEREQ=.FALSE.
      END IF
!
      IF (ISIGOPT.EQ.2) THEN
        LSREQ=.TRUE.
      ELSE
        LSREQ=.FALSE.
      END IF
!
      call pathcheck ("etaname"  , obj%etaname, &
                      '.trc', lereq, show=PATHCHECK_INFO_OUTPUT)
 
      call pathcheck ("signame" , obj%signame, &
                      '.trc', lsreq, show=PATHCHECK_INFO_OUTPUT)
 !
      IF (IVELOPT.EQ.1) THEN
        LVREQ=.TRUE.
        call pathcheck ("velname", obj%velname, &
                        '.vel', lvreq, show=PATHCHECK_INFO_INPUT)
      ELSE IF (IVELOPT.EQ.3) THEN
        LVREQ=.TRUE.
        call pathcheck ("velname", obj%velname, &
                        '.trc', lvreq, show=PATHCHECK_INFO_INPUT)
      END IF
!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!       initialize 
      LUN=6

! --> call velterp to open a CPS velocity file
      if (ivelopt.eq.1) then
        call velterp_create(obj%id_vfile, obj%velname, obj%ndpt, obj%dt, &
          obj%velbias, obj%velscale, obj%velsmode, obj%x_hno, obj%y_hno, &
          lerror, cmsg)
! --> call rantfile to open a velocity trace file
      else if (ivelopt.eq.3) then
        call rantfile_create(obj%id_vtfile, obj%velname, obj%nwih, obj%ndpt, &
          0.0, obj%dt, obj%x_hno, obj%y_hno, lun, lerror, cmsg)
      end if

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
      call pc_put("etaname" , obj%etaname)
      call pc_put("signame" , obj%signame)
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
      IF (ISIGOPT.EQ.2) THEN
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('SIGNAME',        .TRUE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('select_SIGNAME', .TRUE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('SIGNAME_INFO',   .TRUE.)
      ELSE
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('SIGNAME',        .FALSE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('select_SIGNAME', .FALSE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('SIGNAME_INFO',   .FALSE.)
      END IF
!
      IF (IVELOPT.EQ.2) THEN
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('VELNAME',        .FALSE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('select_VELNAME', .FALSE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('VELNAME_INFO',   .FALSE.)
      ELSE
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('VELNAME',        .TRUE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('select_VELNAME', .TRUE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('VELNAME_INFO',   .TRUE.)
      END IF
!
      IF (IEDITO.EQ.1) THEN
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('RI_MAX',        .TRUE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('EXTRP',         .TRUE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('SMB_MIN',       .TRUE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('SMB_EXP',       .FALSE.)
      ELSE IF (IEDITO.EQ.2) THEN
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('RI_MAX',        .FALSE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('EXTRP',         .FALSE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('SMB_MIN',       .TRUE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('SMB_EXP',       .TRUE.)
      ELSE IF (IEDITO.EQ.3) THEN
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('RI_MAX',        .FALSE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('EXTRP',         .FALSE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('SMB_MIN',       .TRUE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('SMB_EXP',       .FALSE.)
      ELSE
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('RI_MAX',        .FALSE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('EXTRP',         .FALSE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('SMB_MIN',       .FALSE.)
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('SMB_EXP',       .FALSE.)
      END IF
!
      IF (IMFLTO.EQ.1) THEN
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('FLEN',        .FALSE.)
      ELSE
        CALL PC_PUT_SENSITIVE_FIELD_FLAG ('FLEN',        .TRUE.)
      END IF
!
      call pc_put_options_field('AOUTO ', (/'No added outputs     ',           &
        'Semblance analysis   ','Seismic traces       '/) )
      call pc_put_options_field('EDITO ', (/'Interpolation        ',           &
        'Semblance weighting  ','Zero edit            ',                       &
        'No Editing           '/) )
      call pc_put_options_field('ETAOPT', (/'ETA trace data       ',           &
        'ETA file             '/) )
      call pc_put_options_field('EXTRP ', (/'Semblance weighting  ',           &
        'Zero edit            '/) )
      call pc_put_options_field('MFLTO ', (/'No filter            ',           &
        'Median before        ','Median after         ',                       &
        'Mean before          ','Mean after           '/) )
      call pc_put_options_field('SIGOPT', (/'No SEMBLANCE output  ',           &
        'Semblance trace data ','Semblance file       '/) )
      call pc_put_options_field('VELOPT', (/'CPS velocity file    ',           &
        'Velocity trace input ','Velocity trace file  '/) )

! --> Delete any of the globals below that have not changed:

      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)

      call pc_put('AOUTO  ', obj%aouto)
      call pc_put('EDITO  ', obj%edito)
      call pc_put('ETANAME', obj%etaname)
      call pc_put('ETAOPT ', obj%etaopt)
      call pc_put('ETA_INC', obj%eta_inc)
      call pc_put('ETA_MAX', obj%eta_max)
      call pc_put('ETA_MIN', obj%eta_min)
      call pc_put('EXTRP  ', obj%extrp)
      call pc_put('E_TIME ', obj%e_time)
      call pc_put('FLEN   ', obj%flen)
      call pc_put('I_WIN  ', obj%i_win)
      call pc_put('L_TAPER', obj%l_taper)
      call pc_put('L_WIN  ', obj%l_win)
      call pc_put('MFLTO  ', obj%mflto)
      call pc_put('RI_MAX ', obj%ri_max)
      call pc_put('SIGNAME', obj%signame)
      call pc_put('SIGOPT ', obj%sigopt)
      call pc_put('SMB_EXP', obj%smb_exp)
      call pc_put('SMB_MIN', obj%smb_min)
      call pc_put('STR_MAX', obj%str_max)
      call pc_put('S_TIME ', obj%s_time)
      call pc_put('VELNAME', obj%velname)
      call pc_put('VELOPT ', obj%velopt)
      call pc_put('VTOPT  ', obj%vtopt)

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
! -------------------------------------------------------------
      MAX_SAMPS = OBJ%NDPT
!
!      INITIALIZE FOR OPENING FILES
!
      IPN=0
      NBITS=32
      MRECS=1000
      CWHIST='ALL'
!
!        OPEN THE SEMBLANCE FILE FOR WRITING
!
      IF (OBJ%ISIGOPT.EQ.2) THEN
        CALL PERMTFILE_OPEN_WRITE(OBJ%ID_SFILE,OBJ%SIGNAME,OBJ%NWIH,OBJ%NDPT, &
          OBJ%TSTRT,OBJ%DT,LUN,IERR,IPN,NBITS,MRECS,CWHIST)
      END IF
!
!        OPEN THE ETA FILE FOR WRITING
!
      IF (OBJ%IETAOPT.EQ.2) THEN
        CALL PERMTFILE_OPEN_WRITE(OBJ%ID_EFILE,OBJ%ETANAME,OBJ%NWIH,OBJ%NDPT, &
          OBJ%TSTRT,OBJ%DT,LUN,IERR,IPN,NBITS,MRECS,CWHIST)
      END IF
!
!     SET NEEDED VARIABLES
!
      E_TIME = OBJ%E_TIME*1000.0
      FLEN = OBJ%FLEN*1000.0
      IAOUTO=OBJ%IAOUTO
      IEDITO=OBJ%IEDITO
      IMFLTO=OBJ%IMFLTO
      I_WIN = OBJ%I_WIN*1000.0
      L_TAPER = OBJ%L_TAPER*1000.0
      L_WIN = OBJ%L_WIN*1000.0
      SAMP_INT_IN = OBJ%DT*1000.0
      S_TIME = OBJ%S_TIME*1000.0
      SMB_MIN = OBJ%SMB_MIN
!
!     PERFORM INITIALIZATION
!
      IST       =  S_TIME/SAMP_INT_IN + 1.0
!     IET       =  E_TIME/SAMP_INT_IN + 1.0
      IET       =  E_TIME/SAMP_INT_IN + 20.0
      IET       =  MIN0( IET,MAX_SAMPS )
!
      LWIN      =  L_WIN/SAMP_INT_IN
      LWIN      =  2*(LWIN/2) + 1
      INC       =  I_WIN/SAMP_INT_IN
      NWIN      =  1 + (IET-IST-LWIN+1)/INC
      SMB_MIN   = SMB_MIN/100.0
!
      OBJ%IST = IST
      OBJ%LWIN = LWIN
      OBJ%INC = INC
      OBJ%NWIN = NWIN
      OBJ%SMB_MIN = SMB_MIN
!
! ..... Set interpolation parameters
      LOP   = 10
      NOP   = 101
      INF   = 5
      OBJ%LOP   = LOP
      OBJ%NOP   = NOP
      OBJ%INF   = INF
! ..... Determine number of ETA values
      N_ETA   = 1+ (ETA_MAX-ETA_MIN)/ETA_INC
      NANA    = N_ETA
      OBJ%NANA    = NANA
      OBJ%N_ETA   = N_ETA
!       Default the max interpolation length
      IF (IEDITO.NE.1) THEN
        OBJ%RI_MAX = 0.0
      END IF
!       Default the semblance exponent
      IF (IEDITO.NE.2) THEN
        OBJ%SMB_EXP = 2.0
      END IF
!       Default the minimum semblance
      IF (IEDITO.EQ.4) THEN
        OBJ%SMB_MIN = 0.0
      END IF
!       Determine the median or mean filter length
      IFLEN=0
      IF (IMFLTO.NE.1) THEN
        IFLEN = FLEN/SAMP_INT_IN
        IF (IFLEN.GT.0) THEN
          IF (MOD(IFLEN,2).EQ.0) IFLEN=IFLEN+1
        END IF
      END IF
      OBJ%IFLEN=IFLEN
!
! ..... SET THE TAPER LENGTH
!
      LTAPER = L_TAPER/SAMP_INT_IN
      IF (LTAPER.LE.1) THEN
        LTAPER=0
      ELSE IF ((LTAPER*2).GT.LWIN) THEN
        LTAPER=0
        CALL PC_ERROR("The Taper length is too large for the specified &
          &window length for analysis. So tapering of the data was not &
          &performed.")
      END IF
      OBJ%LTAPER=LTAPER
!
! --> Insert code to allocate needed permanent memory.
!
!     Set up the work array     
!
      LWA1 = 0
      LWA2 = 0
      LIWA = 0
!      IX_TC  :  Center times for windows
      OBJ%IX_TC = LWA1 + 1
      LWA1 = LWA1 + NWIN
!      IX_MS  :  Maximum stretch times for windows
      OBJ%IX_MS = LWA1 + 1
      LWA1 = LWA1 + NWIN
!      IX_SL  :  Sloth (1./v**2) values
      OBJ%IX_SL = LWA1 + 1
      LWA1 = LWA1 + NWIN
!      IX_ET  :  Isotropic eta values
      OBJ%IX_ET = LWA1 + 1
      LWA1 = LWA1 + NWIN
!      IX_SX  :  Semblance maxima for each window
      OBJ%IX_SX = LWA1 + 1
      LWA1 = LWA1 + NWIN
!      IX_S0  :  Number of traces for each (tc,vel,dta)
      OBJ%IX_S0 = LWA1 + 1
      LWA1 = LWA1 + NWIN*NANA
! ..... IX_NN    -   Summed energies for each (tc,vel,dta)
      OBJ%IX_NN = LWA1 + 1
      LWA1 = LWA1 + NWIN*NANA
! ..... IX_SM    -   Semblance matrix for each window
      OBJ%IX_SM = LWA1 + 1
      LWA1 = LWA1 + NWIN*NANA
! ..... IX_0N    -   Summed traces for each (tc,vel,dta)
      OBJ%IX_0N = LWA1 + 1
      LWA1 = LWA1 + LWIN*NWIN*NANA

! Use a 2nd work array
! ..... IX_ETA :  Magnitude eta values
      OBJ%IX_ETA = LWA2 + 1
      LWA2 = LWA2 + NANA
! ..... IX_OP  :  Interpolation operators
      OBJ%IX_OP = LWA2 + 1
      LWA2 = LWA2 + NOP*LOP
! ..... IX_S1    -   Interpolated trace buffer
      OBJ%IX_S1 = LWA2 + 1
      LWA2 = LWA2 + INF*MAX_SAMPS
! ..... IX_CSA    -   Complex Semblance Analysis
      OBJ%IX_CSA = 1
      IF (IAOUTO.EQ.2) THEN
        OBJ%IX_CSA = LWA2 + 1
        LWA2 = LWA2 + (N_ETA+1)*MAX_SAMPS
      END IF
! ..... IX_BM    -   Buffer for finding medians
      IF (IMFLTO.EQ.2 .OR. IMFLTO.EQ.3) THEN
        OBJ%IX_BM = LWA2 + 1
        LWA2 = LWA2 + IFLEN
 
! Use an Integer work array
        OBJ%IX_BM2 = LIWA + 1
        LIWA = LIWA + IFLEN
!
        OBJ%IX_BM3 = LIWA + 1
        LIWA = LIWA + IFLEN
      END IF

! ..... IX_TMT   -   Time trace buffer
      OBJ%IX_TMT = LWA2 + 1
      LWA2 = LWA2 + MAX_SAMPS
! ..... IX_SMT   -   Semblance trace buffer
      OBJ%IX_SMT = LWA2 + 1
      LWA2 = LWA2 + MAX_SAMPS
! ..... IX_ETT   -   ETA trace buffer
      OBJ%IX_ETT = LWA2 + 1
      LWA2 = LWA2 + MAX_SAMPS
! ..... IX_EDT   -   edit trace buffer
      OBJ%IX_EDT = LWA2 + 1
      LWA2 = LWA2 + MAX_SAMPS
! ..... IX_TEMP  -   memory for a temporary work area
      OBJ%IX_TEMP = LWA2 + 1
      NTVALS=(LOP+2)*4
!
      NVALS=MAX_SAMPS+LOP
      IF (NTVALS.LE.NVALS) NTVALS=NVALS
!
      NVALS=LWIN
      IF (NTVALS.LE.NVALS) NTVALS=NVALS
!
      NVALS=(NWIN+2)*3.0
      IF (NTVALS.LE.NVALS) NTVALS=NVALS
      LWA2 = LWA2+NTVALS
!
!   Allocate your permanent memory like this:
!   Allocation errors will be reported to the parameter cache.
!  
      IF (LWA1.GT.0) THEN
        CALL MEM_ALLOC (OBJ%WA1, LWA1)
      END IF
!
      IF (LWA2.GT.0) THEN
        CALL MEM_ALLOC (OBJ%WA2, LWA2)
      END IF
!
      IF (LIWA.GT.0) THEN
        CALL MEM_ALLOC (OBJ%IWA, LIWA)
      END IF
!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine alamo_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


! --> Insert code for parameter traps here.
! --> (EZCPS with the -t option will insert skeleton code for you)


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine alamo (obj,ntr,hd,tr)
      type(alamo_struct),intent(inout) :: obj                   ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

! --> Insert declarations of local variables.
!                                                   _________________
!                                                  |                 |
!                                                  | Local Variables |
!                                                  |_________________|
!
      REAL, DIMENSION(:), ALLOCATABLE :: RSEIS
      DOUBLE PRECISION, DIMENSION(SIZE(HD,1),SIZE(HD,2)) :: HDO ! Output Headers
      DOUBLE PRECISION, DIMENSION(SIZE(HD,1)) :: VELHD  ! The vel trace headers
      REAL, DIMENSION(SIZE(TR,1)) :: VELTR  ! The velocity trace
!
      double precision ::         SOURCE_DETECT_DIST_IN, TC 
!
      CHARACTER CMSG*128
!
      integer :: CMP_IN, DATA_TRACE_TYPE_IN
      integer :: I, I1, I2, IAFLG        , IANA, IAOUTO, IB, IBYTES, IC 
      integer :: IDEAD
      integer :: IE, IE1, IE2     , IEDITO, IERR, IETATR, IETAOPT, IEXTRP 
      integer ::        IFLEN   
      integer :: IMAX, IMFLTO, INC, INF, INO         , IP   
      integer :: IS_A, IS_R, IS_G, IS_F, IS1, IS2, IS3, ISIGOPT, ISIGTR
      integer :: IST, IST1, ISTRC
      integer :: IT, ITC, ITP, ITX
      integer :: IX_0N, IX_BM, IX_BM2, IX_BM3, IX_CSA, IX_EDT, IX_ET
      integer :: IX_ETT, IX_ETA, IX_MS, IX_NN, IX_OP, IX_S1, IX_SL, IX_SM
      integer :: IX_SMT, IX_S0, IX_SX, IX_TEMP, IX_TC, IX_TMT
      integer :: IVELOPT, IVT1, IVTOPT, IVTRC, IWIN, IX_ST, IZERTR
      integer :: JERROR, JST, JX_0N, JX_NN, JX_S0
      integer :: KBYTES, KX_0N, KX_NN, KX_S0, KX_SM, KX_SX
      integer :: LOP, LTAPER, LTOT, LWRK_ST, LWIN, MWIN
      integer :: NANA, NBYTES, N_ETA, NHDRS, NO, NSEIS
      integer :: NTRC_GATH_IN, NTRC_GATH_OUT         , NTR_VEL 
      integer ::     NVALS, NWIN 
      integer :: MAX_SAMPS, TRACE_NUM_IN, YES
!
      logical :: LERROR
!
      real :: RI_MAX, SMB_EXP, SMB_MIN
!
      CHARACTER ETANAME*21

      real ::        DD     , DEN, DTMX, DTX, ETA, ETA_INC, ETA_MIN, FRNYQ  
      real ::       RPREC 
      real :: SAMP_INT_IN, SL, SLX, SRI, SRS, STR_MAX, STS   
      real :: TC2, TX, TX2, TXX, TY2, X, XCOORD, XX, YCOORD

!
      integer ::        X_HNO, Y_HNO 
!


!
      DATA  IDEAD  / 0 /
      DATA  ISTRC  / 1 /
      DATA  IVTRC  /51 /
      DATA  IETATR /70 /
      DATA  ISIGTR /71 /
      DATA  IZERTR /72 /
!
      DATA  YES /1 /
      DATA  NO  /0 /
!
! --> Insert code for processing logic.
!
!     The following conditional call to the WRAPUP routine should be made
!     before returning from this subroutine:
!
      IF (NTR == NO_MORE_TRACES .OR. NTR == FATAL_ERROR) THEN
        CALL ALAMO_WRAPUP(OBJ)
        RETURN
      END IF
!
!      Set the allocate flag
      IAFLG = YES
!
      IST1=0
      IVT1=0
!
!      set pointers into the work array
!
      IX_TC  = OBJ%IX_TC
      IX_MS  = OBJ%IX_MS
      IX_SL  = OBJ%IX_SL
      IX_ET  = OBJ%IX_ET
      IX_SX  = OBJ%IX_SX
      IX_S0  = OBJ%IX_S0
      IX_NN  = OBJ%IX_NN
      IX_SM  = OBJ%IX_SM
      IX_0N  = OBJ%IX_0N
      IX_ETA = OBJ%IX_ETA
      IX_OP  = OBJ%IX_OP
      IX_S1  = OBJ%IX_S1
      IX_CSA = OBJ%IX_CSA
      IX_BM  = OBJ%IX_BM
      IX_BM2 = OBJ%IX_BM2
      IX_BM3 = OBJ%IX_BM3
      IX_TMT = OBJ%IX_TMT
      IX_SMT = OBJ%IX_SMT
      IX_ETT = OBJ%IX_ETT
      IX_EDT = OBJ%IX_EDT
      IX_TEMP = OBJ%IX_TEMP
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
      ETA_MIN = OBJ%ETA_MIN*100.0
      ETA_INC = OBJ%ETA_INC*100.0
      ETANAME = OBJ%ETANAME
      IAOUTO=OBJ%IAOUTO
      IEDITO=OBJ%IEDITO
      IEXTRP=OBJ%IEXTRP
      IETAOPT=OBJ%IETAOPT
      IFLEN=OBJ%IFLEN
      IMFLTO=OBJ%IMFLTO
      INC=OBJ%INC
      INF=OBJ%INF
      ISIGOPT=OBJ%ISIGOPT
      IST=OBJ%IST
      IVELOPT=OBJ%IVELOPT
      IVTOPT=OBJ%IVTOPT
      LOP=OBJ%LOP
      LWIN=OBJ%LWIN
      LTAPER=OBJ%LTAPER
      NANA=OBJ%NANA
      N_ETA=OBJ%N_ETA
      RI_MAX=OBJ%RI_MAX*1000.0
      SMB_EXP=OBJ%SMB_EXP
      SMB_MIN=OBJ%SMB_MIN
      STR_MAX=OBJ%STR_MAX
      X_HNO=OBJ%X_HNO
      Y_HNO=OBJ%Y_HNO
!
      NWIN=OBJ%NWIN
      NBYTES=4
!
! ..... Put center times and max. stretch factors in WBUF
!
      SRS  = SAMP_INT_IN/1000.
      IC   = IST + LWIN/2
      DO IWIN = 1,NWIN
        TC = SRS*(IC-1)
        DTMX = TC*(STR_MAX/100.)
        OBJ%WA1(IX_TC+IWIN-1) = TC
        OBJ%WA1(IX_MS+IWIN-1) = DTMX
        IC = IC + INC
      ENDDO
!
! ..... Store interpolation operators in the work buffer
      FRNYQ  = .7
      IS_R = IX_TEMP
      IS_G = IS_R + LOP + 2
      IS_F = IS_G + LOP + 2
      IS_A = IS_F + LOP + 2
      CALL AP1_COFGEN1(LOP, FRNYQ, OBJ%WA2(IS_R:), OBJ%WA2(IS_G:), &
        OBJ%WA2(IS_F:), OBJ%WA2(IS_A:), OBJ%WA2(IX_OP:))
!
! ..... Interpolation controls
!
      SRS   = SAMP_INT_IN/1000.
      STS   = SRS*(IST-1)
      INF   = 5
      SRI   = SRS/FLOAT(INF)
      LTOT  = MAX_SAMPS - IST + 1
      IS2   = IX_TEMP
!
! ..... Allocate memory for seismic trace data
      IT=0
      NSEIS=0
      IX_ST=1
      IF (IAOUTO.EQ.3) THEN
 
        IF ( IAFLG .EQ. YES)  THEN
          LWRK_ST=MAX_SAMPS*NTRC_GATH_IN
          IBYTES = NBYTES * LWRK_ST
 
          ALLOCATE (RSEIS(LWRK_ST), STAT=IERR)
 
          IF (IERR .NE. 0)  THEN
            KBYTES = ( IBYTES + 1023) / 1024
            CALL PC_ERROR("Error encountered while trying to allocate %i &
              &kbytes of memory.  You must have more memory available to &
              &run this process.  The index for the allocation request was &
              &IX_ST",KBYTES)
            IAFLG = NO
          END IF
!
! ..... Save seismic data
          DO I = 1,NTRC_GATH_IN
            DATA_TRACE_TYPE_IN = HD(HDR_USER_49, I)
            IF (DATA_TRACE_TYPE_IN.EQ.ISTRC .OR.  &
                DATA_TRACE_TYPE_IN.EQ.IDEAD) THEN
               IT=IT+1
               IB=(IT-1)*MAX_SAMPS+IX_ST
               IE=IB+MAX_SAMPS-1
               RSEIS(IB:IE)=TR(1:MAX_SAMPS,I)
            END IF
          ENDDO
 
          NSEIS=IT
        END IF
      END IF
!
! ..... Read the CPS velocity file
!
      IF (IVELOPT.EQ.1) THEN
!
        NTR_VEL=1
        XCOORD = HD(X_HNO, 1)
        YCOORD = HD(Y_HNO, 1)
        CALL VELTERP_FIND(OBJ%ID_VFILE, XCOORD, YCOORD, VELTR);
!
! ..... Get needed velocity data
!
      ELSE IF (IVELOPT.EQ.2) THEN
        IT=0
        DO I = 1, NTRC_GATH_IN
          DATA_TRACE_TYPE_IN = HD(HDR_USER_49, I )
!
! .....   Copy trace data into the velocity array
          IF (DATA_TRACE_TYPE_IN.EQ.IVTRC) THEN
            IVT1=I
            VELTR(1:MAX_SAMPS)=TR(1:MAX_SAMPS,I)
!            EXIT LOOP
            GOTO 10
          END IF
        ENDDO
!
! ..... Read the velocity trace file
!
      ELSE IF (IVELOPT.EQ.3) THEN
!
        NTR_VEL=1
        XCOORD = HD(X_HNO, 1)
        YCOORD = HD(Y_HNO, 1)
        CALL RANTFILE_FIND(OBJ%ID_VTFILE, XCOORD, YCOORD, VELHD, VELTR, &
          LERROR, CMSG);
        IF (LERROR) RETURN
      END IF
  10  CONTINUE
!
! ..... Setup arrays of eta to scan
      CALL ALAMO_SAVA3TC(NWIN,OBJ%WA1(IX_TC:),SRS,VELTR,OBJ%WA1(IX_SL:),&
        N_ETA,ETA_MIN,ETA_INC,OBJ%WA2(IX_ETA:))
!
! ..... Zero arrays
!
      IE2=IX_TMT+MAX_SAMPS-1
      OBJ%WA2(IX_TMT:IE2)=0.0
      IE2=IX_SMT+MAX_SAMPS-1
      OBJ%WA2(IX_SMT:IE2)=0.0
      IE2=IX_ETT+MAX_SAMPS-1
      OBJ%WA2(IX_ETT:IE2)=0.0
      IE2=IX_EDT+MAX_SAMPS-1
      OBJ%WA2(IX_EDT:IE2)=0.0
!
! ..... Zero arrays
!
      IE2=IX_S0+(NWIN*NANA)-1
      OBJ%WA1(IX_S0:IE2)=0.0
      IE2=IX_NN+(NWIN*NANA)-1
      OBJ%WA1(IX_NN:IE2)=0.0
      IE2=IX_0N+(NWIN*NANA*LWIN)-1
      OBJ%WA1(IX_0N:IE2)=0.0
      IE2=IX_SX+NWIN-1
      OBJ%WA1(IX_SX:IE2)=0.0
!
      TXX  = SRS*(MAX_SAMPS-2-LWIN/2)
!
!
 
      JERROR   = 0
!
      NTRC_GATH_OUT = 0
!
      DO I = 1, NTRC_GATH_IN
!
!                                         ___________________________
!                                        |                           |
!                                        |  GET NUMERICAL LITERALS   |
!                                        | FROM THE "HD" ARRAY       |
!                                        |___________________________|
 
        CMP_IN = HD( HDR_CURRENT_GROUP, I )

        TRACE_NUM_IN = HD( HDR_CURRENT_CHANNEL, I )
 
        DATA_TRACE_TYPE_IN = HD( HDR_USER_49, I )
 
        SOURCE_DETECT_DIST_IN = HD( HDR_OFFSET, I )
!
!*********************************************************************
!*******************  PROCESS TRACE NUMBER "I"  *******************
        IF (DATA_TRACE_TYPE_IN.EQ.ISTRC .OR. DATA_TRACE_TYPE_IN.EQ.IDEAD) THEN
!          SET THE FIRST SEISMIC TRACE NUMBER
          IF (IST1.EQ.0) IST1 = I
!
          X = SOURCE_DETECT_DIST_IN / 1000.0 
          XX = X*X
!
! ..... Interpolate trace
!
          CALL AP1_TRINT(TR(IST:,I),LTOT,OBJ%WA2(IX_OP:),LOP,INF, &
            OBJ%WA2(IX_S1:),OBJ%WA2(IS2:))
!
! ----------------------------------------------------------------
        
!
! ..... Loop on analysis parameters
!
          DO IANA = 1,NANA
!
            ETA  = OBJ%WA2(IX_ETA+IANA-1)
!
! ----------------------------------------------------------------
!
! ..... Loop on analysis windows
!
            JX_S0  = IX_S0 + NWIN*(IANA-1)
            JX_NN  = IX_NN + NWIN*(IANA-1)
            JX_0N  = IX_0N + NWIN*LWIN*(IANA-1)
            DO IWIN = 1,NWIN
!
              SL  = OBJ%WA1(IX_SL+IWIN-1)
              TC  = OBJ%WA1(IX_TC+IWIN-1)
              TC2 = TC*TC
              ITC = TC/SRS + 1.01
              DTX = OBJ%WA1(IX_MS+IWIN-1)
              SLX = SL*XX
 
              TX2  = TC2 + SLX
              TY2  = TC2 + SLX*(1.+2*ETA)
              TX2  = TX2 - 2*ETA*SLX*SLX/TY2
!
! ..... Check stretch for this window
!
              IF(   TX2.LE.TC2 ) GO TO 300
              TX   = SQRT( TX2 )
              IF( TX-TC.GT.DTX ) GO TO 300
              IF(    TX.GT.TXX ) GO TO 300
!
! ..... New I1 computation
!
              ITX = (TX-STS)/SRI
              I1  = IX_S1 + ITX - INF*(LWIN/2)
              IF (I1.LT.IX_S1) I1=IX_S1
!
! ..... Extract window
!
              IE1=I1+(LWIN-1)*INF
              IE2=IS2+LWIN-1 
              OBJ%WA2(IS2:IE2)=OBJ%WA2(I1:IE1:INF)
!
!              Taper the window
!
              IF (LTAPER.GT.0) THEN
                DO I2=1,LTAPER
                  ITP=I2
                  RPREC=FLOAT(ITP-1)/FLOAT(LTAPER-1)
                  IP=IS2+I2-1
                  OBJ%WA2(IP)=OBJ%WA2(IP)*RPREC
                END DO
!
                IB=LWIN-LTAPER+1
                DO I2=IB,LWIN
                  ITP=I2-IB+1
                  RPREC=FLOAT(LTAPER-ITP)/FLOAT(LTAPER-1)
                  IP=IS2+I2-1
                  OBJ%WA2(IP)=OBJ%WA2(IP)*RPREC
                END DO
              END IF
!
! ..... Extract window and update summed trace, summed energy
!
              KX_S0  = JX_S0 + IWIN - 1
              KX_NN  = JX_NN + IWIN - 1
              KX_0N  = JX_0N + LWIN*(IWIN-1)
!
              OBJ%WA1(KX_S0) = OBJ%WA1(KX_S0) + 1.
              IE1=IS2+LWIN-1
              DD=DOT_PRODUCT(OBJ%WA2(IS2:IE1),OBJ%WA2(IS2:IE1))
              OBJ%WA1(KX_NN) = OBJ%WA1(KX_NN) + DD
              IE1=IS2+LWIN-1
              IE2=KX_0N+LWIN-1
              OBJ%WA1(KX_0N:IE2)=OBJ%WA2(IS2:IE1)+OBJ%WA1(KX_0N:IE2)
!
  300         CONTINUE
!
            ENDDO
          ENDDO
        END IF
      END DO
!
!  ---------------------------------------------------
!
! ..... Semblance computation
 
      DO IANA = 1,NANA
!
! ..... Loop on analysis windows
!
        JX_S0  = IX_S0 + NWIN*(IANA-1)
        JX_NN  = IX_NN + NWIN*(IANA-1)
        JX_0N  = IX_0N + NWIN*LWIN*(IANA-1)
        DO IWIN = 1,NWIN
!
          KX_S0  = JX_S0 + IWIN - 1
          KX_NN  = JX_NN + IWIN - 1
          KX_0N  = JX_0N + LWIN*(IWIN-1)
          KX_SX  = IX_SX + IWIN - 1
          KX_SM  = IX_SM + NANA*(IWIN-1) + IANA - 1
!
          IE1=KX_0N+LWIN-1
          IE2=IS2+LWIN-1
          OBJ%WA2(IS2:IE2)=OBJ%WA1(KX_0N:IE1)
          IE1=IS2+LWIN-1
          DD=DOT_PRODUCT(OBJ%WA2(IS2:IE1),OBJ%WA2(IS2:IE1))
          DEN  = OBJ%WA1(KX_S0)*OBJ%WA1(KX_NN)
!
          IF( DEN.NE.0. ) THEN
            DD = DD/DEN
          ELSE
            DD = 0.
          ENDIF
!
          OBJ%WA1(KX_SM) = DD
          OBJ%WA1(KX_SX) = MAX(OBJ%WA1(KX_SX),DD)
        ENDDO
      ENDDO
!
!  ---------------------------------------------------
!
! ..... Replace data traces with semblance information
!
      JST  = IST + (LWIN-INC)/2
      IS1 = IX_TEMP
      IS2  = IS1 + NWIN + 2
      IS3  = IS2 + NWIN + 2
!
      SRS  = SAMP_INT_IN/1000.
!
      CALL ALAMO_TDATA(IVTOPT,ISIGOPT,IETAOPT,IAOUTO,VELTR,          &
        RSEIS(IX_ST:), OBJ%WA1(IX_TC:),NWIN,OBJ%WA2(IX_ETA:),N_ETA,  &
        OBJ%WA1(IX_SM:),OBJ%WA1(IX_SX:),SMB_MIN,MAX_SAMPS,NSEIS,     &
        SAMP_INT_IN,JST,INC,MWIN,OBJ%WA2(IS1:),OBJ%WA2(IS2:),        &
        OBJ%WA2(IS3:),TR,OBJ%WA2(IX_TMT:),OBJ%WA2(IX_SMT:),          &
        OBJ%WA2(IX_ETT:),OBJ%WA2(IX_CSA:))
!
!       PERFORM MEDIAN FILTER BEFORE EDITING WITH SEMBLANCE DATA
!
      IF (IMFLTO.EQ.2) THEN
        NVALS=NWIN*INC
        CALL ALAMO_MEDF(JST,NVALS,IFLEN,OBJ%WA2(IX_BM:),OBJ%IWA(IX_BM2:), &
          OBJ%IWA(IX_BM3:),OBJ%WA2(IX_ETT:), OBJ%WA2(IX_EDT:))
        IE1=IX_EDT+MAX_SAMPS-1
        IE2=IX_ETT+MAX_SAMPS-1
        OBJ%WA2(IX_ETT:IE2)=OBJ%WA2(IX_EDT:IE1)
!
!       PERFORM MEAN FILTER BEFORE EDITING WITH SEMBLANCE DATA
!
      ELSE IF (IMFLTO.EQ.4) THEN
        NVALS=NWIN*INC
        CALL ALAMO_MEANF(JST,NVALS,IFLEN,OBJ%WA2(IX_ETT:),OBJ%WA2(IX_EDT:))
        IE1=IX_EDT+MAX_SAMPS-1
        IE2=IX_ETT+MAX_SAMPS-1
        OBJ%WA2(IX_ETT:IE2)=OBJ%WA2(IX_EDT:IE1)
      END IF
!
!      EDIT ETA TRACE DATA WITH SEMBLANCE DATA
!
      IMAX = RI_MAX/SAMP_INT_IN
      CALL ALAMO_ED_ETA(IEDITO,NWIN,SMB_MIN,SMB_EXP,IMAX,IEXTRP,MAX_SAMPS, &
        SAMP_INT_IN,JST,INC,OBJ%WA2(IX_SMT:),OBJ%WA2(IX_ETT:),OBJ%WA2(IX_EDT:))
      IE1=IX_EDT+MAX_SAMPS-1
      IE2=IX_ETT+MAX_SAMPS-1
      OBJ%WA2(IX_ETT:IE2)=OBJ%WA2(IX_EDT:IE1)
!
!      PERFORM MEDIAN FILTER AFTER EDITING WITH SEMBLANCE DATA
!
      IF (IMFLTO.EQ.3) THEN
        NVALS=NWIN*INC
        CALL ALAMO_MEDF(JST,NVALS,IFLEN,OBJ%WA2(IX_BM:),OBJ%IWA(IX_BM2:), &
          OBJ%IWA(IX_BM3:),OBJ%WA2(IX_ETT:), OBJ%WA2(IX_EDT:))
        IE1=IX_EDT+MAX_SAMPS-1
        IE2=IX_ETT+MAX_SAMPS-1
        OBJ%WA2(IX_ETT:IE2)=OBJ%WA2(IX_EDT:IE1)
!
!      PERFORM MEAN FILTER AFTER EDITING WITH SEMBLANCE DATA
!
      ELSE IF (IMFLTO.EQ.5) THEN
        NVALS=NWIN*INC
        CALL ALAMO_MEANF(JST,NVALS,IFLEN,OBJ%WA2(IX_ETT:),OBJ%WA2(IX_EDT:))
        IE1=IX_EDT+MAX_SAMPS-1
        IE2=IX_ETT+MAX_SAMPS-1
        OBJ%WA2(IX_ETT:IE2)=OBJ%WA2(IX_EDT:IE1)
      END IF
!
!*********************************************************************
      IF (IST1.EQ.0) IST1=1
      IT=1
!      SET VECTOR TRACE HEADER INFORMATION
      IF (IVTOPT.EQ.1) THEN
!        COPY HEADERS FROM THE VELOCITY TRACE OR THE FIRST SEISMIC TRACE
        IF (IVT1.EQ.0) THEN
          INO=IST1
        ELSE
          INO=IVT1
        END IF
        HDO(1:NHDRS, IT) = HD(1:NHDRS, INO)
        HDO(HDR_CURRENT_CHANNEL, IT) = INO
        HDO(HDR_USER_49, IT) = IVTRC
        IT=IT+1
      END IF
!      SET SEMBLANCE TRACE HEADER INFORMATION
      IF (ISIGOPT.EQ.1) THEN
        INO=IST1+IT-1
        HDO(1:NHDRS, IT) = HD(1:NHDRS, INO)
        HDO(HDR_CURRENT_CHANNEL, IT) = IT
        HDO(HDR_USER_49, IT) = ISIGTR
        HDO(HDR_OFFSET, IT) = 0
        IT=IT+1
      END IF
!      SET ETA TRACE DATA AND HEADER INFORMATION
      IF (IETAOPT.EQ.1) THEN
        INO=IST1+IT-1
        IE1=IX_ETT+MAX_SAMPS-1
        TR(1:MAX_SAMPS,IT)=OBJ%WA2(IX_ETT:IE1) / 100.0
!
        HDO(1:NHDRS, IT) = HD(1:NHDRS, INO)
        HDO(HDR_CURRENT_CHANNEL, IT) = IT
        HDO(HDR_USER_49, IT) = IETATR
        HDO(HDR_OFFSET, IT) = 0
        IT=IT+1
      END IF
!
!      SET COMPLEX SEMBLANCE ANALYSIS TRACE HEADER INFORMATION
!
      IF (IAOUTO.EQ.2) THEN
        IB=IT
        IE=N_ETA+IB
        INO=0
        DO I=IB,IE
!           MAKE SURE TRACE TYPE IS VALID
  810     IF (INO.GE.1 .AND. INO.LE.NTRC_GATH_IN) THEN
            DATA_TRACE_TYPE_IN = HD(HDR_USER_49, INO )
            IF (DATA_TRACE_TYPE_IN.NE.ISTRC .AND. &
                DATA_TRACE_TYPE_IN.NE.IDEAD) THEN
              INO=INO+1
              GOTO 810
            END IF
          END IF
!
          IF (INO.GT.NTRC_GATH_IN) THEN
            HDO(1:NHDRS, IT) = HD(1:NHDRS, NTRC_GATH_IN)
            HDO(HDR_CURRENT_CHANNEL, IT) = I-IB
            HDO(HDR_USER_49, IT) = ISTRC
            INO=INO+1
          ELSE IF (INO.EQ.0) THEN
            INO=IST1
            HDO(1:NHDRS, IT) = HD(1:NHDRS, INO)
            HDO(HDR_CURRENT_CHANNEL, IT) = 1
            HDO(HDR_USER_49, IT) = IZERTR
            HDO(HDR_OFFSET, IT) = 0
            INO=IST1
          ELSE
            HDO(1:NHDRS, IT) = HD(1:NHDRS, INO)
            HDO(HDR_CURRENT_GROUP, IT) = HD(HDR_CURRENT_GROUP, INO)
            HDO(HDR_OFFSET, IT)=HD(HDR_OFFSET,INO)
!
            HDO(HDR_CURRENT_CHANNEL, IT) = I-IB
            HDO(HDR_USER_49, IT) = ISTRC
            INO=INO+1
          END IF
!
          IT=IT+1
        END DO
!      SET OUTPUT SEISMIC TRACE HEADER INFORMATION
      ELSE IF (IAOUTO.EQ.3) THEN
        IB=IT
        IE=NTRC_GATH_IN+IB
        INO=0
        DO I=IB,IE
!           MAKE SURE TRACE TYPE IS VALID
  820     IF (INO.GE.1 .AND. INO.LE.NTRC_GATH_IN) THEN
            DATA_TRACE_TYPE_IN = HD(HDR_USER_49, INO)
            IF (DATA_TRACE_TYPE_IN.NE.ISTRC .AND. &
                DATA_TRACE_TYPE_IN.NE.IDEAD) THEN
              INO=INO+1
              GOTO 820
            END IF
          END IF
!
          IF (INO.GT.NTRC_GATH_IN) THEN
            HDO(1:NHDRS, IT) = HD(1:NHDRS, NTRC_GATH_IN)
            HDO(HDR_CURRENT_CHANNEL, IT) = I-IB
            HDO(HDR_USER_49, IT) = ISTRC
            INO=INO+1
          ELSE IF (INO.EQ.0) THEN
            INO=IST1
            HDO(1:NHDRS, IT) = HD(1:NHDRS, INO)
            HDO(HDR_CURRENT_CHANNEL, IT) = 1
            HDO(HDR_USER_49, IT) = IZERTR
            HDO(HDR_OFFSET, IT) = 0
            INO=IST1
          ELSE
            HDO(1:NHDRS, IT) = HD(1:NHDRS, INO)
            HDO(HDR_CURRENT_GROUP, IT) = HD(HDR_CURRENT_GROUP, INO)
            HDO(HDR_OFFSET, IT)=HD(HDR_OFFSET, INO)
!
            HDO(HDR_CURRENT_CHANNEL, IT) = INO
            HDO(HDR_USER_49, IT) = ISTRC
            INO=INO+1
          END IF
!
          IT=IT+1
        END DO
!
!       FREE THE SEISMIC TRACES MEMORY
!
        DEALLOCATE(RSEIS)
      END IF
!
      NTRC_GATH_OUT = IT-1
!
!      WRITE SEMBLANCE INFORMATION TO A SEPARATE FILE
!
      IF (ISIGOPT.EQ.2) THEN
        IE1=IX_SMT+MAX_SAMPS-1
        TR(1:MAX_SAMPS,IT)=OBJ%WA2(IX_SMT:IE1)
        INO=IST1
        HDO(1:NHDRS, IT) = HD(1:NHDRS, INO)
        HDO(HDR_CURRENT_CHANNEL, IT) = 1
        HDO(HDR_USER_49, IT) = ISIGTR
        HDO(HDR_OFFSET, IT) = 0
        CALL PERMTFILE_WRITE(OBJ%ID_SFILE,HDO(1:,IT),TR(1:,IT),JERROR)
        IT=IT+1
      END IF
!
!      SET ETA INFORMATION FOR A SEPARATE FILE
!
      IF (IETAOPT.EQ.2) THEN
        IE1=IX_ETT+MAX_SAMPS-1
        TR(1:MAX_SAMPS,IT)=OBJ%WA2(IX_ETT:IE1) / 100.0
!
        INO=IST1
        HDO(1:NHDRS, IT) = HD(1:NHDRS, INO)
        HDO(HDR_CURRENT_CHANNEL, IT) = 1
        HDO(HDR_USER_49, IT) = IETATR
        HDO(HDR_OFFSET, IT) = 0
        CALL PERMTFILE_WRITE(OBJ%ID_EFILE,HDO(1:,IT),TR(1:,IT),JERROR)
        IT=IT+1
      END IF
!      SAVE HEADERS
      NTR = NTRC_GATH_OUT
      HD = HDO
!
!      GET MORE TRACES EVEN IF THERE ARE NOT OUTPUT TRACES
!
      IF (NTR == 0) THEN
        NTR = NEED_TRACES
      END IF
!
      IF ( JERROR .NE. 0 )   THEN
         IERR = JERROR
      END IF
!
      RETURN
      END SUBROUTINE ALAMO

!!------------------------------- alamo_ed_eta------------------------------!!
!!------------------------------- alamo_ed_eta------------------------------!!
!!------------------------------- alamo_ed_eta------------------------------!!
!                                                  
!               SUBROUTINE ALAMO_ED_ETA
!                                                 
!  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!                                                  
      SUBROUTINE ALAMO_ED_ETA(IEDO,NW,SMIN,SE,IMAX,IEX,NS, &
                              SR,IST,INC,SMTRC,ETTRC,      &
                              D)
!
      IMPLICIT NONE
      integer          ,INTENT(IN)  :: IEDO      ! ARGUMENTS
      integer          ,INTENT(IN)  :: NW        ! ARGUMENTS
      real             ,INTENT(IN)  :: SMIN      ! ARGUMENTS
      real             ,INTENT(IN)  :: SE        ! ARGUMENTS
      integer          ,INTENT(IN)  :: IMAX      ! ARGUMENTS
      integer          ,INTENT(IN)  :: IEX       ! ARGUMENTS
      integer          ,INTENT(IN)  :: NS        ! ARGUMENTS
      real             ,INTENT(IN)  :: SR        ! ARGUMENTS
      integer          ,INTENT(IN)  :: IST       ! ARGUMENTS
      integer          ,INTENT(IN)  :: INC       ! ARGUMENTS
      real             ,INTENT(IN)  :: SMTRC(:)  ! ARGUMENTS
      real             ,INTENT(IN)  :: ETTRC(:)  ! ARGUMENTS
      real             ,INTENT(OUT) :: D(:)      ! ARGUMENTS
!
!                                                 
!         Edit ETA trace data and put into the 'data' traces
!
!                ARGUMENTS (INPUT)
!
!             -   IEDO   -  Edit eta field option
!                           (1=Interpolation,2=Semblance weighting,
!                            4=Mark values,5=none)
!             -   NW     -  NUMBER OF WINDOWS 
!             -   SMIN   -  Min. semblance for screened epsilon trace
!             -   SE     -  Exponent for weighting of semblance ratio
!             -   IMAX   -  Maximum interpolation length
!             -   IEX    -  Type of extrapolation at beginning & end
!                           (1=Semblance weighting,2=Zero edit)
!             -   NS     -  Number of samples in D
!             -   SR     -  Sample rate
!             -   IST    -  Start sample for analysis
!             -   INC    -  Sample increment for analysis
!             -   SMTRC  -  Semblance trace
!             -   ETTRC  -  input Eta trace
!
!                Arguments (output)
!
!             -   D  -  output Eta trace
!
! --------------------------------------------------
!
      integer ::   INTFG
      integer ::   I,I1,I2,I3,IP1,IP2
!                                                             
      real ::      EVAL,EVAL1,EVAL2,SMN,SVAL
      real ::      T1,T2,T3,TOLR,ZEDIT
!
      ZEDIT=10.0**(-10)
      TOLR=0.00001
!
!      edited each of the eta values in all windows
!
      SMN=100.*SMIN
      INTFG=0
      I1 = IST
      I2 = IST + INC*NW -1
      IP1 = 0
!
      DO I = I1,I2
        SVAL=SMTRC(I)
        EVAL=ETTRC(I)
!
        IF (SVAL.GE.SMN) THEN
!
          D(I) = EVAL
!
!
!          UNSET THE INTERPOLATE FLAG IF THE INTERVAL TOO LARGE
          IF (INTFG.EQ.1) THEN
            IF ((I-IP1).GT.IMAX) INTFG=0
!
!            UNSET INTERPOLATE FLAG IF AT THE START
            IF ((IP1+1).EQ.IST) THEN
              INTFG=0
              IP1=0
            END IF
          END IF
!
!          INTERPOLATE IF FLAG IS SET
          IF (INTFG.EQ.1) THEN
            EVAL1=ETTRC(IP1)
            T1=(IP1-1)*SR
!
            EVAL2=EVAL
            T2=(I-1)*SR
            IP1=IP1+1
            IP2=I-1
            DO I3 = IP1,IP2
              T3=(I3-1)*SR
              IF (EVAL2.GE.EVAL1) THEN
                D(I3)=EVAL1+(T3-T1)/(T2-T1)*(EVAL2-EVAL1)
              ELSE
                D(I3)=EVAL1-(T3-T1)/(T2-T1)*(EVAL1-EVAL2)
              END IF
!              MAKE SURE ZERO IS NOT USED
              IF (ABS(D(I3)-0.0).LE.TOLR) THEN
                D(I3)=ZEDIT
              ENDIF
            ENDDO
            INTFG=0
          END IF
!        use semblance to edit the eta field
        ELSE IF (IEDO.EQ.1) THEN
          D(I)=ZEDIT
          IF (INTFG.NE.1) THEN
            IP1=I-1
            INTFG=1
          END IF
!
          IF ((IP1+1).EQ.IST) THEN
            IF (IEX.EQ.1) THEN
              D(I) = (SVAL/SMN)*EVAL
            ELSE
              D(I)=ZEDIT
            END IF
          END IF
        ELSE IF (IEDO.EQ.2) THEN
          D(I) = ((SVAL/SMN)**SE)*EVAL
        ELSE IF (IEDO.EQ.3) THEN
          D(I)=ZEDIT
        ELSE
          D(I) = EVAL
        ENDIF
!        MAKE SURE ZERO IS NOT USED
        IF (ABS(D(I)-0.0).LE.TOLR) THEN
          D(I)=ZEDIT
        ENDIF
      ENDDO
!
!          UNSET INTERPOLATE FLAG IF INTERVAL TOO LARGE
      IF (INTFG.EQ.1) THEN
        IF ((I2-IP1).GT.IMAX) INTFG=0
      END IF
!
!      Use Semblance weighting if interpolation flag is still set
!
      IF (INTFG.EQ.1) THEN
        IP1=IP1+1
        IP2=I2
        DO I3 = IP1,IP2
          SVAL=SMTRC(I3)
          EVAL=ETTRC(I3)
          IF (IEX.EQ.1) THEN
            D(I3) = (SVAL/SMN)*EVAL
          ELSE
            D(I3)=ZEDIT
          END IF
!          MAKE SURE ZERO IS NOT USED
          IF (ABS(D(I3)-0.0).LE.TOLR) THEN
            D(I3)=ZEDIT
          ENDIF
        ENDDO
!
        INTFG=0
      END IF
!
      RETURN                                                  
      END SUBROUTINE ALAMO_ED_ETA
!!------------------------------- alamo_meanf-------------------------------!!
!!------------------------------- alamo_meanf-------------------------------!!
!!------------------------------- alamo_meanf-------------------------------!!
!
      SUBROUTINE ALAMO_MEANF(IST,NVALS,NFILT,XI,XO)
!
      IMPLICIT NONE
      integer          ,INTENT(IN)    :: IST   ! ARGUMENTS
      integer          ,INTENT(IN)    :: NVALS ! ARGUMENTS
      integer          ,INTENT(INOUT) :: NFILT ! ARGUMENTS
      real             ,INTENT(IN)    :: XI(:) ! ARGUMENTS
      real             ,INTENT(OUT)   :: XO(:) ! ARGUMENTS
!
!     PURPOSE:
!       Perform mean filter on an array
!
      integer :: I, I1, IB, ICOUNT, IE, IEV, IMIDP
      real :: RCOUNT, RT, XMEAN
!
!      Make sure filter length is odd
!
      IF (MOD(NFILT,2).EQ.0) NFILT=NFILT+1
!
!      Perform filter on each value
!
      IEV=IST+NVALS-NFILT
      DO I=IST,IEV
        IB=I
        IMIDP=IB+(NFILT/2)
        IE=IB+NFILT-1
!
!         DETERMINE THE MEAN
!
        ICOUNT=0
        RT=0.0
        DO I1=IB,IE
          ICOUNT=I1-IB+1
          RT = RT + XI(I1)
        ENDDO
        RCOUNT = ICOUNT
        XMEAN = RT/RCOUNT
!
!        Reset beginning of array to the mean
!
        IF (IB.EQ.IST) THEN
          DO I1=IB,IMIDP
            XO(I1)=XMEAN
          END DO
!
!        or reset end of array to the mean
!
        ELSE IF (IE.EQ.(IST+NVALS-1)) THEN
          DO I1=IMIDP,IE
            XO(I1)=XMEAN
          END DO
!
!        otherwise reset the middle point to the mean
!
        ELSE
          XO(IMIDP)=XMEAN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE ALAMO_MEANF
!!------------------------------- alamo_med---------------------------------!!
!!------------------------------- alamo_med---------------------------------!!
!!------------------------------- alamo_med---------------------------------!!
!
      SUBROUTINE ALAMO_MED(X,ICOUNT,IUPS,ILOWS,XMED)
!
      IMPLICIT NONE
      real             ,INTENT(INOUT) :: X(:)     ! ARGUMENTS
      integer          ,INTENT(IN)    :: ICOUNT   ! ARGUMENTS
      integer          ,INTENT(INOUT) :: IUPS(:)  ! ARGUMENTS
      integer          ,INTENT(INOUT) :: ILOWS(:) ! ARGUMENTS
      real             ,INTENT(OUT)   :: XMED     ! ARGUMENTS
!
!     PURPOSE:
!       FIND THE MEDIAN
!
      integer :: I, ILB, ITOP, IUB, J, MID
      real :: TEMP
!
!S     SORT THE DATA IN ASCENDING ORDER
!S     FIND THE MEDIAN
!
!      PARTITION EXCHANGE SORT 
!
      ITOP=1
      ILOWS(ITOP)=1
      IUPS(ITOP)=ICOUNT
!
   10 IF (ITOP.NE.0) THEN
        ILB=ILOWS(ITOP)
        IUB=IUPS(ITOP)
        ITOP=ITOP-1
   20   IF (IUB.GT.ILB) THEN
          I=ILB
          J=IUB
          TEMP=X(I)
   30     IF(TEMP.LT.X(J))THEN
            J=J-1
            GOTO 30
          END IF
          IF (J.LE.I) THEN
            X(I)=TEMP
            GOTO 50
          ELSE 
            X(I)=X(J)
            I=I+1
          ENDIF
   40     IF (X(I).LT.TEMP) THEN
            I=I+1
            GOTO 40
          END IF
          IF (J.GT.I) THEN
            X(J)=X(I)
            J=J-1 
            GOTO 30
          ELSE
            X(J)=TEMP
            I=J
          ENDIF
   50     ITOP=ITOP+1
          IF (I-ILB.LT.IUB-I) THEN
            ILOWS(ITOP)=I+1
            IUPS(ITOP)=IUB
            IUB=I-1
          ELSE
            ILOWS(ITOP)=ILB
            IUPS(ITOP)=I-1
            ILB=I+1
          ENDIF
        GOTO 20
        ENDIF
      GOTO 10
      ENDIF 
! 
!      FIND THE MEDIAN 
!
      IF (MOD(ICOUNT,2).EQ.0) THEN
        MID=.5*ICOUNT
        XMED=.5*(X(MID)+X(MID+1))
      ELSE
        XMED=X((ICOUNT/2)+1)  
      END IF
!
      RETURN
      END SUBROUTINE ALAMO_MED

!!------------------------------- alamo_medf--------------------------------!!
!!------------------------------- alamo_medf--------------------------------!!
!!------------------------------- alamo_medf--------------------------------!!
!
      SUBROUTINE ALAMO_MEDF(IST,NVALS,NFILT,BUFM,IBUFU,IBUFL,XI,XO)
!
      IMPLICIT NONE
      integer          ,INTENT(IN)    :: IST      ! ARGUMENTS
      integer          ,INTENT(IN)    :: NVALS    ! ARGUMENTS
      integer          ,INTENT(INOUT) :: NFILT    ! ARGUMENTS
      real             ,INTENT(INOUT) :: BUFM(:)  ! ARGUMENTS
      integer          ,INTENT(INOUT) :: IBUFU(:) ! ARGUMENTS
      integer          ,INTENT(INOUT) :: IBUFL(:) ! ARGUMENTS
      real             ,INTENT(IN)    :: XI(:)    ! ARGUMENTS
      real             ,INTENT(OUT)   :: XO(:)    ! ARGUMENTS
!
!     PURPOSE:
!       Perform median filter on an array
!
      integer :: I, I1, IB, IE, IEV, IMIDP, INO
      real :: XMED
!
!      Make sure filter length is odd
!
      IF (MOD(NFILT,2).EQ.0) NFILT=NFILT+1
!
!      Perform filter on each value
!
      IEV=IST+NVALS-NFILT
      DO I=IST,IEV
        IB=I
        IMIDP=IB+(NFILT/2)
        IE=IB+NFILT-1
!
!         Determine values to find median
!
        DO I1=IB,IE
          INO=I1-IB+1
          BUFM(INO)=XI(I1)
        ENDDO
!
!        Determine the median of values
!
        CALL ALAMO_MED(BUFM,NFILT,IBUFU,IBUFL,XMED)
!
!        Reset beginning of array to the median
!
        IF (IB.EQ.IST) THEN
          DO I1=IB,IMIDP
            XO(I1)=XMED
          END DO
!
!        or reset end of array to the median
!
        ELSE IF (IE.EQ.(IST+NVALS-1)) THEN
          DO I1=IMIDP,IE
            XO(I1)=XMED
          END DO
!
!        otherwise reset the middle point to the median
!
        ELSE
          XO(IMIDP)=XMED
        END IF
      END DO
!
      RETURN
      END SUBROUTINE ALAMO_MEDF
!!------------------------------- alamo_sava3tc-----------------------------!!
!!------------------------------- alamo_sava3tc-----------------------------!!
!!------------------------------- alamo_sava3tc-----------------------------!!
!  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<          
!                                                                               
!               SUBROUTINE ALAMO_SAVA3TC
!                                                                               
!  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<         
!                                                                               
      SUBROUTINE ALAMO_SAVA3TC( NWIN,TC,SRS,VEL,SL,         &
                                N_EPS,EPS_MIN,EPS_INC,EPS )
!
      IMPLICIT NONE
      integer          ,INTENT(IN)    :: NWIN     ! ARGUMENTS
      real             ,INTENT(IN)    :: TC(:)    ! ARGUMENTS
      real             ,INTENT(IN)    :: SRS      ! ARGUMENTS
      real             ,INTENT(IN)    :: VEL(:)   ! ARGUMENTS
      real             ,INTENT(OUT)   :: SL(:)    ! ARGUMENTS
      integer          ,INTENT(IN)    :: N_EPS    ! ARGUMENTS
      real             ,INTENT(IN)    :: EPS_MIN  ! ARGUMENTS
      real             ,INTENT(IN)    :: EPS_INC  ! ARGUMENTS
      real             ,INTENT(OUT)   :: EPS(:)   ! ARGUMENTS (LEN=N_EPS)
!                                                                               
!                                                                               
! ---------------------------------------------------------------------         
!                                                                               
!       Revised  08/18/95    Remove restriction that the epsilon range
!                            be symmetric about 0
!
!       Revised  08/18/95    Generate sloth at window centers using a full
!                            velocity function (vel(t)) instead of TV-pairs
!
!       Revised  03/18/96    Change name from SAA_VA3TC to A_SAVA3TC
!
! ---------------------------------------------------------------------         
!                                                                               
!         Fill arrays of sloths (1/v**2) to be scanned
!                                                                               
!                 ARGUMENTS 
!                                                                               
!         Argument    Type   I/O   Usage
!
!           NWIN       I4     I    Number of windows
!           TC         R4     I    Window center times (s)
!           SRS        R4     I    Sample rate (s)
!           VEL        R4     I    Velocity function
!           SL         R4     O    Sloth (1/v**2) at window centers
!                                  Units are (s/km)**2 or (s/kft)**2
!           N_EPS      I4     I    Number of epsilons to scan (%)
!           EPS_MIN    R4     I    Minimum epsilon
!           EPS_INC    R4     I    Epsilon increment
!           EPS        R4     O    Array of epsilons for each scan
!                                                                               
! ---------------------------------------------------------------------         
!                                                                               
      integer ::   IWIN,IVEL
      integer ::   IEPS                                                       
!                                                                               
      real ::      VELT
      real ::      EPST
!
! ---------------------------------------------------------------------         
!                                                                               
!         Determine sloth at each window center
!
      DO IWIN = 1,NWIN
          IVEL = TC(IWIN)/SRS + 2
          VELT = VEL(IVEL)/1000.
          SL(IWIN) = 1./(VELT**2)
      ENDDO                                                                     
!                                                                               
!                                                                               
! ---------------------------------------------------------------------         
!                                                                               
!         Set up array of epsilon to scan
!                                                                               
      DO IEPS = 1,N_EPS
          EPST      = EPS_MIN +  EPS_INC*(IEPS-1)
          EPS(IEPS) = EPST/100.
      ENDDO
!                                                                               
! ---------------------------------------------------------------------         
!                                                                               
!     RETURN                                                                    
      END SUBROUTINE ALAMO_SAVA3TC
!!------------------------------- alamo_tdata-------------------------------!!
!!------------------------------- alamo_tdata-------------------------------!!
!!------------------------------- alamo_tdata-------------------------------!!
!                                                  
!               SUBROUTINE ALAMO_TDATA
!                                                 
!  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!                                                  
      SUBROUTINE ALAMO_TDATA(IVTO,ISIGO,IETAO,IAOO,VEL, &
                             SEIS,TC,NW,EPS,NE,         &
                             S,SX,SMIN,NS,NST,          &
                             SR,IST,INC,MW,TT,ET,       &
                             SG,D,TMTRC,SMTRC,          &
                             ETTRC, CSA)
!
      IMPLICIT NONE
      integer          ,INTENT(IN)    :: IVTO     ! ARGUMENTS
      integer          ,INTENT(IN)    :: ISIGO    ! ARGUMENTS
      integer          ,INTENT(IN)    :: IETAO    ! ARGUMENTS
      integer          ,INTENT(IN)    :: IAOO     ! ARGUMENTS
      real             ,INTENT(IN)    :: VEL(:)   ! ARGUMENTS (LEN=NS)
      real             ,INTENT(IN)    :: SEIS(:)  ! ARGUMENTS (LEN=NS*NST)
      real             ,INTENT(IN)    :: TC(:)    ! ARGUMENTS (LEN=NW)
      integer          ,INTENT(IN)    :: NW       ! ARGUMENTS
      real             ,INTENT(IN)    :: EPS(:)   ! ARGUMENTS (LEN=NE)
      integer          ,INTENT(IN)    :: NE       ! ARGUMENTS
      real             ,INTENT(IN)    :: S(:)     ! ARGUMENTS (LEN=NE*NW)
      real             ,INTENT(IN)    :: SX(:)    ! ARGUMENTS (LEN=NW)
      real             ,INTENT(IN)    :: SMIN     ! ARGUMENTS
      integer          ,INTENT(IN)    :: NS       ! ARGUMENTS
      integer          ,INTENT(IN)    :: NST      ! ARGUMENTS
      real             ,INTENT(IN)    :: SR       ! ARGUMENTS
      integer          ,INTENT(IN)    :: IST      ! ARGUMENTS
      integer          ,INTENT(IN)    :: INC      ! ARGUMENTS
      integer          ,INTENT(OUT)   :: MW       ! ARGUMENTS
      real             ,INTENT(OUT)   :: TT(:)    ! ARGUMENTS (LEN=MW)
      real             ,INTENT(OUT)   :: ET(:)    ! ARGUMENTS (LEN=MW)
      real             ,INTENT(OUT)   :: SG(:)    ! ARGUMENTS (LEN=MW)
      real             ,INTENT(OUT)   :: D(:,:)   ! ARGUMENTS (LEN=NS,IT+NE+4)
      real             ,INTENT(OUT)   :: TMTRC(:) ! ARGUMENTS (LEN=NS)
      real             ,INTENT(OUT)   :: SMTRC(:) ! ARGUMENTS (LEN=NS)
      real             ,INTENT(OUT)   :: ETTRC(:) ! ARGUMENTS (LEN=NS)
      real             ,INTENT(OUT)   :: CSA(:)   ! ARGUMENTS (LEN=NS*NE+4)
!
!                                                 
!         Put ETA trace data, semblance trace data, semblance matrix,
!            maximum semblance and epsilon at maximum semblance
!            into the 'data' traces
!
!                ARGUMENTS (INPUT)
!
!             -   IVTO   -  Output velocity traces (IVTO=1)
!             -   ISIGO  -  Output semblance trace data option (ISIGO=1)
!             -   IETAO  -  Output eta trace data option (IETAO=1)
!             -   IAOO   -  Additional output options
!                           (1=none,2=Comprehensive seismic analysis,
!                            3=Seismic traces)
!             -   VEL    -  Velocity trace data
!             -   SEIS   -  Seismic trace data
!             -   TC     -  TIME AT WINDOW CENTER (S)
!             -   NW     -  NUMBER OF WINDOWS 
!             -   EPS    -  Array of epsilons for each scan
!             -   NE     -  NUMBER OF ETAS 
!             -   S      -  SEMBLANCE MATRIX
!             -   SX     -  SEMBLANCE MAXIMUM FOR EACH WINDOW
!             -   SMIN   -  Min. semblance for screened epsilon trace
!             -   NS     -  Number of samples in D
!             -   NST    -  Number of seismic traces
!             -   SR     -  Sample rate
!             -   IST    -  Start sample for analysis
!             -   INC    -  Sample increment for analysis
!
!                Arguments (output)
!
!             -   MW     -  Number of output points
!             -   TT     -  Times
!             -   ET     -  Etas
!             -   SG     -  Semblances
!             -   D      -  D(NS,IT+NE+4)
!             -   TMTRC  -  Time trace
!             -   SMTRC  -  Semblance trace
!             -   ETTRC  -  Eta trace
!             -   CSA    -  CSA(NS*NE+4)
!
!            Organization:
!
!            D(*,IT)        : velocity trace data (IT=1)
!            D(*,IT)        : semblance trace data (IT=1 or 2)
!            D(*,IT)        : eta values (IT=1,2, or 3 if IETAO=1)
!              
!            TMTRC(NS)      : Time trace (NS)
!            SMTRC(NS)      : Semblance trace (NS)
!            ETTRC(NS)      : Eta trace (NS)
!              
!            CSA(NS*NE)     : comprehensive semblance analysis (NE)
!              ...
!            CSA(NS*(NE+1)) : zero trace
!
! --------------------------------------------------
!
      integer ::   IT, ITS1,NTD
      integer ::   IE,IEL,IEN,INO,IW
      integer ::   I,I1,I1L,I1N,I2,I2L,I2N,IMID,IMIDL,IMIDN,IS1,IS2
      integer ::   INTFG, IWL,IWN, NVALS
!                                                             
      real ::      DTC,EPN,EPT,EPTL,EPTN,ETT,EVAL
      real ::      SMB,SMBL,SMBN,SVAL
      real ::      SMN,SM1,SM2,SMT,SXI
      real ::      T0,TE,TIME,TMID,TMIDL,TMIDN
!                                                             
! ----------------------------------------------------------         
!                                                             
!      Initialize D
!
      NTD=0
      IF (IVTO.EQ.1) NTD=NTD+1
      IF (ISIGO.EQ.1) NTD=NTD+1
      IF (IETAO.EQ.1) NTD=NTD+1
      IF (IAOO.EQ.2) NTD=NTD+NE+1
      IF (IAOO.EQ.3) NTD=NTD+NST+1
!
      D(1:NS,1:NTD)=0.0
!       set the velocity trace data
      IT=1
      IF (IVTO.EQ.1) THEN
        D(1:NS,IT)=VEL(1:NS)
        IT=IT+1
      END IF
!                                                             
!      For each window, put (t,eta for max. semb) in arrays
!
      MW  = 0
      DTC = TC(2) - TC(1)
      T0  = TC(1) - DTC
      IF (T0.GT.0.) THEN
        MW  = MW + 1
        TT(MW) = 1000.*T0
        ET(MW) = 0.
        SG(MW) = 0.
      ENDIF
      DO IW = 2,NW-1
        SXI = SX(IW)
        IF( SXI.LT.SMIN ) GO TO 300
        IF( SX(IW-1).GT.SXI .OR. SX(IW+1).GT.SXI ) GO TO 300
!
        MW  = MW + 1
        DO IE = 1,NE
          INO=(IW-1)*NE+IE
          IF( SX(IW).EQ.S(INO) ) GO TO 200
        ENDDO
        IE = 1
  200   CONTINUE
!
        ETT = EPS(IE)
        ET(MW)  =  100.*ETT
        SG(MW)  =  100.*SX(IW)
        TT(MW)  = 1000.*TC(IW)
  300   CONTINUE
      ENDDO
!
      ET(MW)  =  100.*ETT
      MW     = MW + 1
      TE     = TC(NW) + DTC
      TT(MW) = 1000.*TE
      ET(MW) = 0.
      SG(MW) = 0.
!                                                             
!      Determine the trace to put the semblance values
!
      IF (ISIGO.EQ.1) THEN
       ITS1=IT
       IT=IT+1
      END IF
!                                                             
!      Save a trace to put the eta values
!
      IF (IETAO.EQ.1) IT=IT+1
!
!      For each window, put in D:
!          semblance (eps), max semblance, eps at max., screened epsilon
!
      INTFG=0
      EPN  = -100.*EPS(NE)
      SMN  =  100.*SMIN
      DO IW = 1,NW
        I1  = IST + INC*(IW-1)
        I2  = I1  + INC - 1
        IMID=(I1+I2)/2
!       TMID=IMID*SR
        TMID=(IMID-1)*SR
!
        SMB = 100.*SX(IW)
!
        DO IE = 1,NE
          INO=(IW-1)*NE+IE
          IF (SX(IW).EQ.S(INO)) GO TO 400
        ENDDO
        IE = 1
  400   CONTINUE
        EPT  = 100.*EPS(IE)
!
        IF (IW.GT.1) THEN
          I1L=IST + INC*(IW-2)
          I2L=I1L + INC - 1
          IMIDL=(I1L+I2L)/2
!         TMIDL=IMIDL*SR
          TMIDL=(IMIDL-1)*SR
          SMBL=SX(IW-1)*100.0
!
          DO IEL = 1,NE
            INO=(IW-2)*NE+IEL
            IF (SX(IW-1).EQ.S(INO)) GO TO 410
          ENDDO
          IEL = 1
  410     CONTINUE
          IWL=IW-1
          EPTL = 100.*EPS(IEL)
        END IF
!
        IF (IW.LT.NW) THEN
          I1N=IST + INC*IW
          I2N=I1N + INC - 1
          IMIDN=(I1N+I2N)/2
!         TMIDN=IMIDN*SR
          TMIDN=(IMIDN-1)*SR
          SMBN=SX(IW+1)*100.0
!
          DO IEN = 1,NE
            INO=IW*NE+IEN
            IF (SX(IW+1).EQ.S(INO)) GO TO 420
          ENDDO
          IEN = 1
  420     CONTINUE
          IWN=IW+1
          EPTN = 100.*EPS(IEN)
        END IF
!
        IF (IW.EQ.1 .OR. IW.EQ.NW) THEN
          SMBL=SMB
          SMBN=SMB
          EPTL=EPT
          EPTN=EPT
!         TMIDL=I1*SR
          TMIDL=(I1-1)*SR
!         TMIDN=I2*SR
          TMIDN=(I2-1)*SR
        END IF
!
        DO I = I1,I2
!         TIME=I*SR
          TIME=(I-1)*SR
          TMTRC(I)=TIME
!
          IF (I.EQ.IMID) THEN
            SVAL=SMB
          ELSE IF (I.LT.IMID) THEN
            IF (SMBL.LE.SMB) THEN
              SVAL=SMBL+(TIME-TMIDL)/(TMID-TMIDL)*(SMB-SMBL)
            ELSE
              SVAL=SMBL-(TIME-TMIDL)/(TMID-TMIDL)*(SMBL-SMB)
            END IF
          ELSE IF (I.GT.IMID) THEN
            IF (SMB.LE.SMBN) THEN
              SVAL=SMB+(TIME-TMID)/(TMIDN-TMID)*(SMBN-SMB)
            ELSE
              SVAL=SMB-(TIME-TMID)/(TMIDN-TMID)*(SMB-SMBN)
            END IF
          END IF
!
          SMTRC(I)=SVAL
          IF (ISIGO.EQ.1) D(I,ITS1)=SVAL
!
          IF (I.EQ.IMID) THEN
            EVAL=EPT
          ELSE IF (I.LT.IMID) THEN
            IF (EPTL.LE.EPT) THEN
              EVAL=EPTL+(TIME-TMIDL)/(TMID-TMIDL)*(EPT-EPTL)
            ELSE
              EVAL=EPTL-(TIME-TMIDL)/(TMID-TMIDL)*(EPTL-EPT)
            END IF
          ELSE IF (I.GT.IMID) THEN
            IF (EPT.LE.EPTN) THEN
              EVAL=EPT+(TIME-TMID)/(TMIDN-TMID)*(EPTN-EPT)
            ELSE
              EVAL=EPT-(TIME-TMID)/(TMIDN-TMID)*(EPT-EPTN)
            END IF
          END IF
!
          ETTRC(I)=EVAL
        ENDDO
      ENDDO
!      determine the comprehensive assemblance analysis (if wanted)
      IF (IAOO.EQ.2) THEN
        IS1=1
        IS2=NE
        DO IE = IS1,IS2
          SM1  = 0.
          I1   = MAX0(1,IST-INC/2)
          I2   = IST + INC/2
          DO IW = 1,NW+1
            IF( IW.LE.NW ) THEN
              INO=(IW-1)*NE+IE
              SM2  = 100.*S(INO)
            ELSE
              SM2  = 0.
            ENDIF
            DO I = I1,I2
              SMT     = SM2*(I-I1) + SM1*(I2-I)
              INO=NS*(IE-1)+I
              CSA(INO) = SMT/(I2-I1)
              D(I,IE+IT) = CSA(INO)
            ENDDO
            I1  = I2 + 1
            I2  = MIN0(NS,I1+INC-1)
            SM1 = SM2
          ENDDO
        ENDDO
!
        IT=IT+NE+1
!       set the seismic trace data
      ELSE IF (IAOO.EQ.3) THEN
        IT=IT+1
        NVALS=NS*NST
        D(1:NVALS,IT)=SEIS(1:NVALS)
        IT=IT+NST+1
      END IF
!                                                             
      RETURN                                                  
      END SUBROUTINE ALAMO_TDATA

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine alamo_wrapup (obj)
      type(alamo_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine alamo_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module alamo_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

