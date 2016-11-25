!<CPS_v1 type="PROCESS"/>
!!----------------------------- masker.f90 ---------------------------------!!
!!----------------------------- masker.f90 ---------------------------------!!
!!----------------------------- masker.f90 ---------------------------------!!


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
! Name       : MASKER                            (mask traces)
! Category   : miscellaneous
! Written    : 2003-06-19   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Mask seismic traces by setting masked values to nil.
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This process masks seismic traces by setting masked values to nil, or to
! any other user-specified value.
!
! The masked regions are defined by horizon files which are in modspec
! or static file format.
!
! The top, bottom, and internal portions of seismic traces can be masked.
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
! This process is a single-trace process.
! Input traces can arrive in any order, singly or in bunches.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process outputs the same traces it inputs, with masked values set to
! the value of parameter MASKING_VALUE (which by default is NIL).
!
!          WARNING: SINCE OUTPUT TRACES MIGHT CONTAIN NIL VALUES,
!                   THESE TRACES MUST NOT BE COMPRESSED.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NWIH      number of header words                  used but not changed.
! NDPT      number of sample values in trace        used but not changed.
! TSTRT     starting time on trace                  used but not changed.
! DT        trace sample interval                   used but not changed.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#             Description                       Action taken
! ----             -----------                       ------------
! HDR_FLAG         flag identifying traces to mask   used but not changed.
! 25               largest absolute value            reset as necessary.
! 2                head mute index                   reset as necessary.
! 64               tail mute index                   reset as necessary.
!
! In addition, header words containing the X and Y coordinates of the
! traces will be used.  These header words are specified in the modspec file
! (or the static files) used to specify the masked regions.
!
! If HORIZONS_TO_HEADERS is true, new user defined header words are created
! to contain the horizon times for all horizons.  These are headers HDR_FIRST
! through HDR_LAST.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!005. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!  4. 2005-01-31  Stoeckley  Add parameter MASKING_VALUE.
!  3. 2004-03-09  Stoeckley  Add parameters for shifting horizons and tapering
!                             mutes; set the mute headers when muting; add
!                             NO MASKING to the masking mode options; add
!                             parallel-safe control parameters; add parameter
!                             HORIZONS_TO_HEADERS; remove parameters referring
!                             to flattening horizons.
!  2. 2003-07-28  Stoeckley  Add GUI information field modspec_nhor; fix
!                             problem with reading modspec files; add third
!                             separate static file or horizon for flattening;
!                             add ability to choose which horizons on modspec
!                             file to use; change default for FILETYPE to
!                             MODSPEC FILE; change FILETYPE kombobox to a
!                             combobox.
!  1. 2003-06-19  Stoeckley  Initial version.
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
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH        >0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
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
!
!<NS Masker/NC=80>
!                         Mask Seismic Traces
!
! HDR_FLAG~~~~~~~~~=`II        [/L]Header word denoting flagged traces.
! MASKING_MODE~~~~~=`CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! MASKING_VALUE~~~~=`FFFFFFFF  [/L](blank field means a NIL value)
! FILETYPE~~~~~~~~~=`CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! TOP_SHIFT~~~~~~~~=`FFFFFFFF  [/L]Constant to add to top horizon time (sec).
! BOTTOM_SHIFT~~~~~=`FFFFFFFF  [/L]Constant to add to bottom horizon time (sec).
! TOP_TAPER~~~~~~~~=`FFFFFFFF  [/L]Taper applied to top horizon masking (sec).
! BOTTOM_TAPER~~~~~=`FFFFFFFF  [/L]Taper applied to bottom horizon masking (sec).
!
! HORIZONS_TO_HEADERS=`KKK    HDR_FIRST=`XX   HDR_LAST =`XX
!
! `----------------------------------------------------------------------------
!  Select MODSPEC[MODSPEC]~~~~~~~`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                        [MODSPEC_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                        [MODSPEC_NHOR]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                        TOP_MODSPEC_HORIZON~~~~~~~=`II
!                        BOTTOM_MODSPEC_HORIZON~~~~=`II
! `----------------------------------------------------------------------------
!
! `--------------------------------------------------------------------------------
!  Select TOP_HORIZON[TOP_HORIZON]~~~~~~~`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                            [TOP_HORIZON_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!  Select BOTTOM_HORIZON[BOTTOM_HORIZON]~~~~`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                            [BOTTOM_HORIZON_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! `--------------------------------------------------------------------------------
!
!<PARMS MODSPEC                [/XST/ML=140]>
!<PARMS TOP_HORIZON            [/XST/ML=140]>
!<PARMS BOTTOM_HORIZON         [/XST/ML=140]>
!<PARMS MODSPEC_INFO           [/XST/ML=140]>
!<PARMS MODSPEC_NHOR           [/XST/ML=140]>
!<PARMS TOP_HORIZON_INFO       [/XST/ML=140]>
!<PARMS BOTTOM_HORIZON_INFO    [/XST/ML=140]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!
!<Help KEYWORD="FILETYPE">
!<Tip> Type of horizon file (or files) to read in. </Tip>
! Default = MODSPEC FILE
! Allowed = MODSPEC FILE     (single modspec file containing all horizons)
! Allowed = STATIC FILES     (each horizon in a separate "static" file)
!
! If FILETYPE is "MODSPEC FILE", MODSPEC must be specified.  This file must
! contain the top and/or bottom horizon to use for masking.
!
! If FILETYPE is "STATIC FILES", TOP_HORIZON and/or BOTTOM_HORIZON must be
! specified.  These files must be in CPS static file format.
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="MODSPEC_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of MODSPEC. </Tip>
!</Help>
!
!<Help KEYWORD="MODSPEC_NHOR" TYPE= "DISPLAY_ONLY">
!<Tip> Number of horizons on MODSPEC file. </Tip>
!</Help>
!
!<Help KEYWORD="TOP_HORIZON_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of TOP_HORIZON. </Tip>
!</Help>
!
!<Help KEYWORD="BOTTOM_HORIZON_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of BOTTOM_HORIZON. </Tip>
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="SELECT_MODSPEC">
!<Tip> Choose a MODSPEC file using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_TOP_HORIZON">
!<Tip> Choose a TOP_HORIZON using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_BOTTOM_HORIZON">
!<Tip> Choose a BOTTOM_HORIZON using a file selection dialog box. </Tip>
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="TOP_MODSPEC_HORIZON">
!<Tip> Modspec horizon number to use for top horizon. </Tip>
! Default = none
! Allowed = any horizon on the modspec file.
!</Help>
!
!<Help KEYWORD="BOTTOM_MODSPEC_HORIZON">
!<Tip> Modspec horizon number to use for bottom horizon. </Tip>
! Default = none
! Allowed = any horizon on the modspec file.
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="MODSPEC">
!<Tip> Pathname for a modspec file containing a set of horizons. </Tip>
! Default = NONE
! Allowed = char
!
! If MASKING_MODE is MASK ABOVE, data above the TOP_MODSPEC_HORIZON in the
! modspec file will be masked.
!
! If MASKING_MODE is MASK BELOW, data below the BOTTOM_MODSPEC_HORIZON in the
! modspec file will be masked.
!
! If MASKING_MODE is MASK ABOVE AND BELOW, data above the TOP_MODSPEC_HORIZON
! in the modspec file, and below the BOTTOM_MODSPEC_HORIZON in the modspec
! file, will be masked.
!
! If MASKING_MODE is MASK BETWEEN, data below the TOP_MODSPEC_HORIZON in the
! modspec file, and above the BOTTOM_MODSPEC_HORIZON in the modspec file,
! will be masked, but only where the TOP_MODSPEC_HORIZON is above the
! BOTTOM_MODSPEC_HORIZON.
!
! If MASKING_MODE is NO MASKING, no masking will be performed.
!
! This file is required if FILETYPE is "MODSPEC FILE".
! This file is not used if FILETYPE is "STATIC FILES".
!</Help>
!
!
!<Help KEYWORD="TOP_HORIZON">
!<Tip> Pathname for top horizon in static file format. </Tip>
! Default = NONE
! Allowed = char
!
! If MASKING_MODE is MASK ABOVE AND BELOW or MASK ABOVE, data above this
! TOP_HORIZON will be masked.
!
! If MASKING_MODE is MASK BETWEEN, data below this TOP_HORIZON and above the
! BOTTOM HORIZON will be masked, but only where the top horizon is above
! the bottom horizon.
!
! This file is not used if FILETYPE is "MODSPEC FILE".
!</Help>
!
!
!<Help KEYWORD="BOTTOM_HORIZON">
!<Tip> Pathname for bottom horizon in static file format. </Tip>
! Default = NONE
! Allowed = char
!
! If MASKING_MODE is MASK ABOVE AND BELOW or MASK BELOW, data below this
! BOTTOM_HORIZON will be masked.
!
! If MASKING_MODE is MASK BETWEEN, data below the TOP_HORIZON and above this
! BOTTOM HORIZON will be masked, but only where the top horizon is above
! the bottom horizon.
!
! This file is not used if FILETYPE is "MODSPEC FILE".
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="MASKING_MODE">
!<Tip> How to mask the traces. </Tip>
! Default = MASK ABOVE AND BELOW
! Allowed = MASK ABOVE AND BELOW
! Allowed = MASK ABOVE
! Allowed = MASK BELOW
! Allowed = MASK BETWEEN
! Allowed = NO MASKING
!
! If MASKING_MODE is MASK ABOVE AND BELOW:
!
!   All data will be set to MASKING_VALUE (default NIL) above top horizon.
!   All data will be set to MASKING_VALUE (default NIL) below bottom horizon.
!
! If MASKING_MODE is MASK ABOVE:
!
!   All data will be set to MASKING_VALUE (default NIL) above top horizon.
!
! If MASKING_MODE is MASK BELOW:
!
!   All data will be set to MASKING_VALUE (default NIL) below bottom horizon.
!
! If MASKING_MODE is MASK BETWEEN:
!
!   All data will be set to MASKING_VALUE (default NIL) between the top
!   horizon and the bottom horizon (both must be specified), but only where
!   the top horizon is above the bottom horizon.
!
! If MASKING_MODE is NO MASKING:
!
!   No data will be reset.
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="MASKING_VALUE">
!<Tip> Value to put into masked areas of the traces. </Tip>
! Default = blank
! Allowed = any value (or leave blank to use a NIL value)
!
! WARNING: Setting MASKING_VALUE to something different from NIL or zero may
! have unintended consequences if you are using a mute taper.  If this is
! the case, be sure to set TOP_TAPER and BOTTOM_TAPER to zero.
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 0-NWIH   (normally 0 or 48-55 or 65-NWIH)
!
! If HDR_FLAG = 0, then all traces are masked.  Otherwise, only traces with
! a flag (non-zero number) found in header word HDR_FLAG are masked.
!
! If HORIZONS_TO_HEADERS is YES, all traces receive the horizon times in
! their header words regardless of the flag found in header word HDR_FLAG.
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="HORIZONS_TO_HEADERS">
!<Tip> Whether to insert horizon times into trace headers. </Tip>
! Default = NO  
! Allowed = YES or NO
!
! If this parameter is YES, new user defined header words will be created
! to contain the horizon times for all horizons on the modspec (or static)
! files.  All horizons found on the files will be used regardless of whether
! they are also being used for masking.
!</Help>
!
!
!<Help KEYWORD="HDR_FIRST">
!<tip> First user-defined header word for storing horizon times. </tip>
!
! New header words from HDR_FIRST through HDR_LAST are created when
! HORIZONS_TO_HEADERS is YES.
!</Help>
!
!
!<Help KEYWORD="HDR_LAST">
!<tip> Last user-defined header word for storing horizon times. </tip>
!
! New header words from HDR_FIRST through HDR_LAST are created when
! HORIZONS_TO_HEADERS is YES.
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="TOP_SHIFT">
!<Tip> Constant to add to top horizon time (seconds). </Tip>
! Default = 0.0
! Allowed = real
!
! A positive value adds time (shifts the horizon down).
! A negative value subtracts time (shifts the horizon up).
!
! When masking above the top horizon, TOP_SHIFT should normally be set
! to zero or to a negative value.
!
! When masking between the top and bottom horizons, TOP_SHIFT should
! normally be set to a zero or positive value.
!
! Note that this parameter must be specified in SECONDS.
!</Help>
!
!
!<Help KEYWORD="BOTTOM_SHIFT">
!<Tip> Constant to add to bottom horizon time (seconds). </Tip>
! Default = 0.0
! Allowed = real
!
! A positive value adds time (shifts the horizon down).
! A negative value subtracts time (shifts the horizon up).
!
! When masking below the bottom horizon, BOTTOM_SHIFT should normally be set
! to zero or to a positive value.
!
! When masking between the top and bottom horizons, BOTTOM_SHIFT should
! normally be set to a zero or negative value.
!
! Note that this parameter must be specified in SECONDS.
!</Help>
!
!                     ++++++++++++++++++++++++++++
!
!<Help KEYWORD="TOP_TAPER">
!<Tip> Length of taper to apply when masking at the top horizon (sec). </Tip>
! Default = 0.06
! Allowed = real >= 0.0
!
! WARNING: Setting MASKING_VALUE to something different from NIL or zero may
! have unintended consequences if you are using a mute taper.  If this is
! the case, be sure to set TOP_TAPER and BOTTOM_TAPER to zero.
!
! Note that this parameter must be specified in SECONDS.
!</Help>
!
!
!<Help KEYWORD="BOTTOM_TAPER">
!<Tip> Length of taper to apply when masking at the bottom horizon (sec). </Tip>
! Default = 0.06
! Allowed = real >= 0.0
!
! WARNING: Setting MASKING_VALUE to something different from NIL or zero may
! have unintended consequences if you are using a mute taper.  If this is
! the case, be sure to set TOP_TAPER and BOTTOM_TAPER to zero.
!
! Note that this parameter must be specified in SECONDS.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module masker_module
      use pc_module
      use named_constants_module
      use kadabra_module
      use lav_module
      use mth_module
      use pathcheck_module
      use pathchoose_module
      use statclasses_module
      use statclass_module
      use dbug_module

      implicit none
      private
      public :: masker_create
      public :: masker_initialize
      public :: masker_update
      public :: masker_delete
      public :: masker            ! main trace processing routine.
      public :: masker_wrapup

      character(len=100),public,save :: MASKER_IDENT = &
'$Id: masker.f90,v 1.5 2006/10/17 13:45:45 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: masker_struct

        private
        logical            :: skip_wrapup                 ! wrapup flag.

        integer            :: nwih_input                  ! global
        integer            :: nwih_output                 ! global
        integer            :: ndpt                        ! global
        real               :: tstrt                       ! global
        real               :: dt                          ! global

        character(len=40)  :: filetype                    ! process parameter
        integer            :: hdr_flag                    ! process parameter
        character(len=40)  :: masking_mode                ! process parameter
        real               :: masking_value               ! process parameter
        logical            :: horizons_to_headers         ! process parameter
        integer            :: top_modspec_horizon         ! process parameter
        integer            :: bottom_modspec_horizon      ! process parameter
        real               :: top_shift                   ! process parameter
        real               :: bottom_shift                ! process parameter
        real               :: top_taper                   ! process parameter
        real               :: bottom_taper                ! process parameter

     character(len=FILENAME_LENGTH) :: modspec            ! process parameter
     character(len=FILENAME_LENGTH) :: top_horizon        ! process parameter
     character(len=FILENAME_LENGTH) :: bottom_horizon     ! process parameter

        type(pathchoose_struct) ,pointer :: modspec_pathchoose
        type(pathchoose_struct) ,pointer :: top_pathchoose
        type(pathchoose_struct) ,pointer :: bottom_pathchoose
        type(statclasses_struct),pointer :: statclasses
        integer                          :: nhorizons_modspec
        integer                          :: top_horizon_number
        integer                          :: bottom_horizon_number
        integer                          :: nclass
        type(statclass_struct),pointer   :: top_statclass  
        type(statclass_struct),pointer   :: bottom_statclass

      end type masker_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                    ,save :: lunprint  ! unit number for printing.
      type(masker_struct),pointer,save :: object    ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine masker_create (obj)
      type(masker_struct),pointer :: obj       ! arguments
      integer                     :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) then
           call pc_error ("Unable to allocate obj in masker_create")
           return
      end if

      nullify (obj%modspec_pathchoose) ! jpa
      nullify (obj%top_pathchoose) ! jpa
      nullify (obj%bottom_pathchoose) ! jpa
      nullify (obj%top_statclass) ! jpa
      nullify (obj%bottom_statclass) ! jpa

  call pathchoose_create (obj%modspec_pathchoose   , 'modspec'     , 'modspec')
  call pathchoose_create (obj%top_pathchoose       , 'top_horizon'       , '*')
  call pathchoose_create (obj%bottom_pathchoose    , 'bottom_horizon'    , '*')

      nullify (obj%statclasses)

      call masker_initialize (obj)
      end subroutine masker_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine masker_delete (obj)
      type(masker_struct),pointer :: obj            ! arguments
      integer                     :: ierr           ! for error checking

      call masker_wrapup (obj)

      call pathchoose_delete  (obj%modspec_pathchoose)
      call pathchoose_delete  (obj%top_pathchoose)
      call pathchoose_delete  (obj%bottom_pathchoose)
      call statclasses_delete (obj%statclasses)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) &
             call pc_warning ("error deallocating obj in masker_delete")
      end subroutine masker_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine masker_initialize (obj)
      type(masker_struct),intent(inout) :: obj            ! arguments

      obj%hdr_flag                   = 0
      obj%masking_mode               = 'MASK ABOVE AND BELOW'
      obj%masking_value              = FNIL
      obj%horizons_to_headers        = .false.
      obj%filetype                   = 'MODSPEC FILE'
      obj%modspec                    = PATHCHECK_EMPTY
      obj%top_horizon                = PATHCHECK_EMPTY
      obj%bottom_horizon             = PATHCHECK_EMPTY
      obj%top_modspec_horizon        = INIL
      obj%bottom_modspec_horizon     = INIL
      obj%top_shift                  = 0.0
      obj%bottom_shift               = 0.0
      obj%top_taper                  = 0.06
      obj%bottom_taper               = 0.06

      obj%nhorizons_modspec = 0

      call masker_update (obj)
      end subroutine masker_initialize


!!-------------------------- start of update -------------------------------!!
!!-------------------------- start of update -------------------------------!!
!!-------------------------- start of update -------------------------------!!


      subroutine masker_update (obj)
      type(masker_struct),intent(inout),target :: obj             ! arguments
      logical                                  :: whoops,mspec    ! local
      logical                                  :: top,bottom      ! local
      logical                                  :: m_top,m_bottom  ! local
      logical                                  :: s_top,s_bottom  ! local
      character(len=80)                        :: msg             ! local
      character(len=FILENAME_LENGTH)           :: filenames(2)    ! local
      integer                                  :: nclass          ! local
      character(len=80)                        :: modspec_nhor    ! local
      character(len=FILENAME_LENGTH)           :: modspec_keep    ! local
      character(len=30)                        :: msg2,msg3       ! local
      integer                                  :: hdr_first       ! local
      integer                                  :: hdr_last        ! local
      logical                                  :: s_top2          ! local
      logical                                  :: s_bottom2       ! local
      logical                                  :: mspec2          ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!---------------------------- read parameters -----------------------------!!
!!---------------------------- read parameters -----------------------------!!
!!---------------------------- read parameters -----------------------------!!


      modspec_keep = obj%modspec

 if(pathchoose_update(obj%modspec_pathchoose   ,obj%modspec           )) return
 if(pathchoose_update(obj%top_pathchoose       ,obj%top_horizon       )) return
 if(pathchoose_update(obj%bottom_pathchoose    ,obj%bottom_horizon    )) return

      call pc_get_global ('nwih'    , obj%nwih_input)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)

      call pc_get ('hdr_flag'                  , obj%hdr_flag              )
      call pc_get ('masking_value'             , obj%masking_value         )
      call pc_get ('horizons_to_headers'       , obj%horizons_to_headers   )
      call pc_get ('modspec'                   , obj%modspec               )
      call pc_get ('top_horizon'               , obj%top_horizon           )
      call pc_get ('bottom_horizon'            , obj%bottom_horizon        )
      call pc_get ('top_modspec_horizon'       , obj%top_modspec_horizon   )
      call pc_get ('bottom_modspec_horizon'    , obj%bottom_modspec_horizon)
      call pc_get ('top_shift'                 , obj%top_shift             )
      call pc_get ('bottom_shift'              , obj%bottom_shift          )
      call pc_get ('top_taper'                 , obj%top_taper             )
      call pc_get ('bottom_taper'              , obj%bottom_taper          )

      call kadabra_get_put_options ('filetype',obj%filetype,   &
                                      (/'MODSPEC FILE ',       &
                                        'STATIC FILES '/))

      call kadabra_get_put_options ('masking_mode',obj%masking_mode,  &
                                          (/'MASK ABOVE AND BELOW ',  &
                                            'MASK ABOVE           ',  &
                                            'MASK BELOW           ',  &
                                            'MASK BETWEEN         ',  &
                                            'NO MASKING           '/))


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      obj%top_taper    = max(obj%top_taper   , 0.0)
      obj%bottom_taper = max(obj%bottom_taper, 0.0)

      if (obj%hdr_flag < 0 .or. obj%hdr_flag > obj%nwih_input) then
           call pc_warning ('bad HDR_FLAG number - reset to 0')
           obj%hdr_flag = 0
      end if

 call pathcheck ('modspec'       ,obj%modspec       ,show=PATHCHECK_INFO_INPUT)
 call pathcheck ('top_horizon'   ,obj%top_horizon   ,show=PATHCHECK_INFO_INPUT)
 call pathcheck ('bottom_horizon',obj%bottom_horizon,show=PATHCHECK_INFO_INPUT)

      if (obj%modspec /= modspec_keep) then
            if (obj%modspec == PATHCHECK_EMPTY) then
                 obj%nhorizons_modspec = 0
            else
                 obj%nhorizons_modspec = &
                        statclasses_num_modspec_layers (obj%modspec,lunprint)
            end if
      end if

      modspec_nhor = trim(string_ii2ss(obj%nhorizons_modspec))// &
                            ' horizons on modspec file'

      if (pc_verify_end() .and. obj%filetype == 'STATIC FILES') then
           select case (obj%masking_mode)
             case ('MASK ABOVE AND BELOW')
                if (obj%top_horizon    == PATHCHECK_EMPTY .and. &
                    obj%bottom_horizon == PATHCHECK_EMPTY) then
                  call pc_error ('TOP_HORIZON and BOTTOM_HORIZON must be&
                                 & specified when masking above and below')
                end if
             case ('MASK ABOVE')
                if (obj%top_horizon    == PATHCHECK_EMPTY) then
                  call pc_error ('TOP_HORIZON must be&
                                 & specified when masking above')
                end if
             case ('MASK BELOW')
                if (obj%bottom_horizon == PATHCHECK_EMPTY) then
                  call pc_error ('BOTTOM_HORIZON must be&
                                 & specified when masking below')
                end if
             case ('MASK BETWEEN')
                if (obj%top_horizon    == PATHCHECK_EMPTY .or. &
                    obj%bottom_horizon == PATHCHECK_EMPTY) then
                  call pc_error ('TOP_HORIZON and BOTTOM_HORIZON must both be&
                                 & specified when masking between')
                end if
             case ('NO MASKING')
                continue
             case default
                call pc_error ('Illegal masking mode',obj%masking_mode)
           end select
      end if

      if (pc_verify_end() .and. obj%filetype == 'MODSPEC FILE') then
           if (obj%modspec == PATHCHECK_EMPTY) then
                call pc_error ('the MODSPEC file must be specified')
           end if
      end if

      mspec     = (obj%filetype == 'MODSPEC FILE')
      top       = (obj%masking_mode /= 'MASK BELOW' .and. &
                   obj%masking_mode /= 'NO MASKING')
      bottom    = (obj%masking_mode /= 'MASK ABOVE' .and. &
                   obj%masking_mode /= 'NO MASKING')
      m_top     =      mspec .and. top  
      m_bottom  =      mspec .and. bottom
      s_top     = .not.mspec .and. top 
      s_bottom  = .not.mspec .and. bottom
      s_top2    = .not.mspec .and. (obj%horizons_to_headers .or. top)
      s_bottom2 = .not.mspec .and. (obj%horizons_to_headers .or. bottom)
      mspec2    = mspec .and. (top .or. bottom .or. obj%horizons_to_headers)

      if (pc_verify_end() .and. m_top) then
           if (obj%top_modspec_horizon == INIL .or. &
               obj%top_modspec_horizon < 1     .or. &
               obj%top_modspec_horizon > obj%nhorizons_modspec) then
                  call pc_error ('invalid TOP_MODSPEC_HORIZON')
           end if
      end if

      if (pc_verify_end() .and. m_bottom) then
           if (obj%bottom_modspec_horizon == INIL .or. &
               obj%bottom_modspec_horizon < 1     .or. &
               obj%bottom_modspec_horizon > obj%nhorizons_modspec) then
                  call pc_error ('invalid BOTTOM_MODSPEC_HORIZON')
           end if
      end if

      if (pc_verify_end()) then
      if (obj%masking_value /= FNIL .and. obj%masking_value /= 0.0) then
      if (obj%top_taper /= 0.0 .or. obj%bottom_taper /= 0.0) then
           call pc_warning ('Setting MASKING_VALUE to something different &
                            &from NIL or zero')
           call pc_warning ('may have unintended consequences if you are &
                            &using a mute taper.')
           call pc_warning ('If this is the case, be sure to set TOP_TAPER &
                            &and BOTTOM_TAPER to zero.')
      end if
      end if
      end if

      select case (obj%filetype)
        case ('STATIC FILES')
            nclass = 0
            if (obj%top_horizon        /= PATHCHECK_EMPTY) nclass = nclass + 1
            if (obj%bottom_horizon     /= PATHCHECK_EMPTY) nclass = nclass + 1
        case ('MODSPEC FILE')
            nclass = obj%nhorizons_modspec
        case default
           call pc_error ('Illegal filetype',obj%filetype)
      end select

      if (obj%horizons_to_headers .and. nclass > 0) then
           obj%nwih_output = obj%nwih_input + nclass
           hdr_first       = obj%nwih_input + 1
           hdr_last        = obj%nwih_output
      else
           obj%nwih_output = obj%nwih_input
           hdr_first       = INIL
           hdr_last        = INIL
      end if


!!---------------------------- write parameters ----------------------------!!
!!---------------------------- write parameters ----------------------------!!
!!---------------------------- write parameters ----------------------------!!


      call pc_put_global ('nwih'    , obj%nwih_output)

      call pc_put ('hdr_flag'                  , obj%hdr_flag              )
      call pc_put ('masking_value'             , obj%masking_value         )
      call pc_put ('horizons_to_headers'       , obj%horizons_to_headers   )
      call pc_put ('modspec'                   , obj%modspec               )
      call pc_put ('top_horizon'               , obj%top_horizon           )
      call pc_put ('bottom_horizon'            , obj%bottom_horizon        )
      call pc_put ('top_modspec_horizon'       , obj%top_modspec_horizon   )
      call pc_put ('bottom_modspec_horizon'    , obj%bottom_modspec_horizon)
      call pc_put ('top_shift'                 , obj%top_shift             )
      call pc_put ('bottom_shift'              , obj%bottom_shift          )
      call pc_put ('top_taper'                 , obj%top_taper             )
      call pc_put ('bottom_taper'              , obj%bottom_taper          )

      call pc_put_gui_only ('modspec_nhor' , modspec_nhor)
      call pc_put_gui_only ('hdr_first'    , hdr_first)
      call pc_put_gui_only ('hdr_last'     , hdr_last)

      call pc_put_control('PARALLEL_SAFE'        ,.true.)
      call pc_put_control('PCPS_SEND_MODE'       ,'PCPS_SEND_FIRST_AVAIL')
      call pc_put_control('PCPS_RECEIVE_MODE'    ,'PCPS_RECEIVE_PASSTHRU')
      call pc_put_control('PCPS_BUNCH_MODE'      ,'PCPS_BUNCH_TRACE_GROUPS')
      call pc_put_control('PCPS_SEND_EOF_MODE'   ,'PCPS_SEND_ALL_EOF')
      call pc_put_control('PCPS_ALT_SEND_MODE'   ,'PCPS_SEND_ALL')
      call pc_put_control('PCPS_ALT_RECEIVE_MODE','PCPS_RECEIVE_ALL_EOF')

      call pc_put_sensitive_field_flag ('modspec'                   , mspec2)
      call pc_put_sensitive_field_flag ('select_modspec'            , mspec2)
  !!  call pc_put_sensitive_field_flag ('modspec_info'              , mspec2)
  !!  call pc_put_sensitive_field_flag ('modspec_nhor'              , mspec2)
      call pc_put_sensitive_field_flag ('top_modspec_horizon'       , m_top)
      call pc_put_sensitive_field_flag ('bottom_modspec_horizon'    , m_bottom)
      call pc_put_sensitive_field_flag ('top_horizon'               , s_top2)
      call pc_put_sensitive_field_flag ('select_top_horizon'        , s_top2)
  !!  call pc_put_sensitive_field_flag ('top_horizon_info'          , s_top2)
      call pc_put_sensitive_field_flag ('bottom_horizon'            , s_bottom2)
      call pc_put_sensitive_field_flag ('select_bottom_horizon'     , s_bottom2)
  !!  call pc_put_sensitive_field_flag ('bottom_horizon_info'       , s_bottom2)


!!------------------------- prepare for execution --------------------------!!
!!------------------------- prepare for execution --------------------------!!
!!------------------------- prepare for execution --------------------------!!


      call statclasses_delete  (obj%statclasses)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      call statclasses_create &
            (obj%statclasses,obj%nwih_input,obj%ndpt,obj%tstrt,obj%dt,lunprint)


      select case (obj%filetype)

        case ('STATIC FILES')

            obj%top_horizon_number        = INIL
            obj%bottom_horizon_number     = INIL
            nclass = 0
            if (obj%top_horizon /= PATHCHECK_EMPTY) then
                 nclass = nclass + 1
                 filenames(nclass) = obj%top_horizon
                 if (s_top) obj%top_horizon_number = nclass
            end if
            if (obj%bottom_horizon /= PATHCHECK_EMPTY) then
                 nclass = nclass + 1
                 filenames(nclass) = obj%bottom_horizon
                 if (s_bottom) obj%bottom_horizon_number = nclass
            end if

            call statclasses_read_static_files &
                           (obj%statclasses,filenames,nclass,whoops,msg)
            if (whoops) call pc_error (msg)

        case ('MODSPEC FILE')

            call statclasses_read_modspec_file &
                                (obj%statclasses,obj%modspec,whoops,msg)
            if (whoops) call pc_error (msg)
            obj%top_horizon_number        = obj%top_modspec_horizon
            obj%bottom_horizon_number     = obj%bottom_modspec_horizon
            nclass = obj%nhorizons_modspec

        case default

           call pc_error ('Illegal filetype',obj%filetype)

      end select

      obj%nclass = statclasses_num_classes(obj%statclasses)
      if (obj%nclass /= nclass) then
           call pc_error &
              ('Inconsistency in number of horizons returned by STATCLASSES')
      end if

      obj%top_statclass    => statclasses_fetch_statclass &
                                (obj%statclasses,obj%top_horizon_number)
      obj%bottom_statclass => statclasses_fetch_statclass &
                                (obj%statclasses,obj%bottom_horizon_number)

      if (associated(obj%top_statclass)) then
           call statclass_supply_mute_taper (obj%top_statclass, obj%top_taper)
      end if
      if (associated(obj%bottom_statclass)) then
           call statclass_supply_mute_taper &
                                      (obj%bottom_statclass, obj%bottom_taper)
      end if

  msg2 = ' '
  msg3 = ' '
  if (top)    msg2 = 'needed but not available'
  if (bottom) msg3 = 'needed but not available'
  if (top    .and. associated(obj%top_statclass))    msg2(8:) = 'and available'
  if (bottom .and. associated(obj%bottom_statclass)) msg3(8:) = 'and available'

  if (obj%top_horizon_number        == INIL) obj%top_horizon_number        = 0
  if (obj%bottom_horizon_number     == INIL) obj%bottom_horizon_number     = 0

  print *, 'number of horizons = ',obj%nclass
  print *, 'top horizon        = ',obj%top_horizon_number       ,'  ',msg2
  print *, 'bottom horizon     = ',obj%bottom_horizon_number    ,'  ',msg3

      if (top .and. .not.associated(obj%top_statclass)) then
           call pc_error ('no top horizon information when needed')
      end if

      if (bottom .and. .not.associated(obj%bottom_statclass)) then
           call pc_error ('no bottom horizon information when needed')
      end if


!!----------------------------- finish update ------------------------------!!
!!----------------------------- finish update ------------------------------!!
!!----------------------------- finish update ------------------------------!!


      end subroutine masker_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine masker (obj,ntr,hd,tr)
      type(masker_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(inout) :: ntr                   ! arguments
      double precision   ,intent(inout) :: hd(:,:)               ! arguments
      real               ,intent(inout) :: tr(:,:)               ! arguments
      integer                           :: itr,indx              ! local
      type(statclass_struct),pointer    :: statclass             ! local

      call dbug_set_message ('masker (entering)')

      do itr = 1,ntr

        if (obj%horizons_to_headers) then
             do indx = 1,obj%nclass
                 statclass => statclasses_fetch_statclass &
                                             (obj%statclasses,indx)
                 hd(obj%nwih_input+indx,itr) = 0.001 * &
                          statclass_get_static_value (statclass,hd(:,itr))
             end do
        end if

        if (obj%hdr_flag > 0) then
             if (hd(obj%hdr_flag,itr) == 0.0d0) cycle
        end if

        call dbug_set_message ('masker '//obj%masking_mode)

        select case (obj%masking_mode)

             case ('MASK ABOVE AND BELOW')

                  call statclass_apply_top_mute                          &
                            (obj%top_statclass,hd(:,itr),tr(:,itr),      &
                             obj%masking_value, 1000.0 * obj%top_shift)

                  call statclass_apply_bottom_mute                          &
                            (obj%bottom_statclass,hd(:,itr),tr(:,itr),      &
                             obj%masking_value, 1000.0 * obj%bottom_shift)

             case ('MASK ABOVE')

                  call statclass_apply_top_mute                          &
                            (obj%top_statclass,hd(:,itr),tr(:,itr),      &
                             obj%masking_value, 1000.0 * obj%top_shift)

             case ('MASK BELOW')

                  call statclass_apply_bottom_mute                          &
                            (obj%bottom_statclass,hd(:,itr),tr(:,itr),      &
                             obj%masking_value, 1000.0 * obj%bottom_shift)

             case ('MASK BETWEEN')

                  call statclass_apply_inside_mute                   &
                            (obj%top_statclass,obj%bottom_statclass, &
                             hd(:,itr),tr(:,itr), obj%masking_value, &
                             1000.0 * obj%top_shift,                 &
                             1000.0 * obj%bottom_shift)

             case ('NO MASKING')

                  continue

        end select

        call dbug_set_message ('masker (after masking_mode select case)')

      end do

      call dbug_set_message ('masker (after ntr loop)')

      if (ntr > 0) call lav_set_hdr (hd,tr,obj%ndpt,ntr)
      call dbug_set_message ('masker (exiting)')

      end subroutine masker


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine masker_wrapup (obj)
      type(masker_struct),intent(inout) :: obj              ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine masker_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module masker_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

