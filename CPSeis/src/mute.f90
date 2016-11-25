!<CPS_v1 type="PROCESS"/>
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
!                        C P S   P R O C E S S
!
! Name       : MUTE
! Category   : amplitude_mod
! Written    : 1986-04-08   by: Mike Howard
! Revised    : 2009-01-29   by: Bill Menger
! Maturity   : beta
! Purpose    : Kill undesirable samples at top or bottom of trace.
! Portability: No known limitations.
! Parallel   : Yes
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! 
! Nominal Head Mute Operation
!
! For all options other than the SET options, MUTE will zero all trace samples
! with a time less than the specified mute time.  It will then taper the sample
! values using a cosine taper of 0.0 at the mute time, tapering up to 1.0 at
! (mute time + LEN_TAPER).  Finally, it will set the head mute header word to
! the index of the first non-zero sample.
!
!
! Nominal Tail Mute Operation
!
! For all options other than the SET options, MUTE will zero all trace samples
! with a time greater than the specified mute time.  It will then taper the
! sample values using a cosine taper of 0.0 at the mute time, tapering up to
! 1.0 at (mute time - LEN_TAPER).  Finally, it will set the tail mute header
! word to the index of the last non-zero sample.
!
!
! Definition of Mute Header Word Values
!
! The head/tail mute header word values are defined as the "index of the
! first/last desirable sample in the trace," where "index" of a sample means
! the  sequential number of the sample in the trace such that the first sample
! in the trace, whose time is TSTRT, has an index of 1.  (This means that mute
! header word values are referenced to TSTRT, not zero time.)
!
!
! Mute Header Word Values for Dead Traces
!
! For dead traces, header word 25 is set to 0.0.
!
!
! SET and RESTORE Options
!
! The SET options set the head/tail mute header word values to the index of
! the first/last non-zero trace sample value.  (See OPT_SFNZV Option, below,
! for an exception to this.)  The SET option does not zero any sample values.
!
! The RESTORE options function the same as the nominal mute operation except
! that the mute time is the time associated with the mute header word values
! and the mute header word values are not reset.  RESTORE options can be used
! to reapply a previous mute.
!
!
! OPT_SFNZV Option
!
! If you want to set the mute header word values without regard to whether
! there is a dead section in the trace, set OPT_SFNZV = N.  This will override
! the normal operation of setting head/tail mute header word values to the
! index of the first/last non-zero sample.  (OPT_SFNZV = Y gives normal
! operation.)
!
!
! FILE Options
!
! The format for mute files was changed in 1995 to allow operation for 3D data.
! Even though the new format is the only one being written now, MUTE will
! recognize the old format and use it properly for 2D data.
!
! In the new format, offsets will be considered negative: (a) if header words
! used are 6,46,0 and header 47 is less than header 46; or (b) if header words
! used are 6,33,34 and header 35 is less than header 33.
!
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                         ADVICE FOR USERS
!
! 
! Mute Header Word Values and Operation of TSEL and SHFT
!
! If the TSEL process is used to change TSTRT, it will change the mute header
! words also, since mute header word values are referenced to TSTRT.  If TSTRT
! increases/decreases by n samples, the head and tail mute header words will
! decrease/increase by n.
!
! A bulk shift of a trace, such as a static shift made by the SHFT process, will
! change the mute header word values since the first/last desirable sample time
! changes but TSTRT does not.  Thus, if the first/last desirable sample time
! increases/decreases by n samples, the head and tail mute headers will
! increase/decrease by n.
! 
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                       TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This process can alter input traces.
! This process outputs the same traces as it receives (possibly altered).
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                   GLOBAL PARAMETERS USED OR CHANGED
!
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
! NWIH     number of words in header             used but not changed
!
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!         HDR_FLAG                   flag word
! 2       head mute header word      possibly used or changed
! 6       offset                     used but not changed
! 64      tail mute header word      possibly used or changed
! 25      trace sample LAV           recomputed for every trace processed
! HDR_X   arbitrary header word      for grid and offset varying mute
! HDR_Y   arbitrary header word      for grid and offset varying mute 
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY
!
!     Date        Author    Description
!     ----        ------    -----------
! 55. 2009-01-29  Bill Menger Nullified more pointers,fixed compound if test to
!                             work with Intel compiler.
! 54. 2007-12-04  Stoeckley Do not try to deallocate the mute_pointer when it
!                            had not been allocated, and move the deallocation
!                            from the delete routine to the wrapup routine.
! 53. 2007-11-27  Stoeckley Change mute_pointer from integer to CPOINTER, so
!                            the file option will work in SeisSpace.  On 64-bit
!                            machines an integer is too small to store a C
!                            pointer.
! 52. 2006-08-24  D. Glover Added NULLIFY statements for Intel compiler.
! 51. 2006-06-06  Stoeckley Add call to pc_register_array_names for SeisSpace.
!050. 2006-01-10  B. Menger   Removed Unused Variables.
! 49. 2005-01-17  Baumel    Enable VEL_MUTE = 0.0 (the default value!) to work
!                            correctly (equivalent to infinite velocity = zero
!                            slowness) for VEL_HEAD and VEL_TAIL options.
! 48. 2002-05-06  Vunderink Added parallel control parameters
! 47. 2002-04-22  Stoeckley Set head mute to bottom of trace when mute goes
!                            below end of trace.
! 46. 2001-10-18  Stoeckley Move trap subroutine to different location in code
!                            to make the intel compiler happy; also remove
!                            call to pc_verify_scalar.
! 45. 2001-08-24  Stoeckley Add file selection box and file status message.
! 44. 2001-04-04  Stoeckley Change wrapup flag; allow #headers > 64.
! 43. 2000-08-30  O'Brien   Implemented inter_module to fix OT_3D operations
! 42. 2000-08-23  O'Brien   Adopted one trap per header word in place of one
!                             trap for all header words.
!                           HDR_X now defaults to HDR_MIDPOINT_XGRID (7)
!                           HDR_Y now defaults to HDR_MIDPOINT_YGRID (8)
! 41. 2000-08-17  O'Brien   Fixed typo X_VALEUS --> X_VALUES in pc_put() call
! 40. 2000-08-10  O'Brien   Documentation update to accomodate changes to
!                             header word naming convention... variable names
!                             made to be consistent with new convention.
!                           Rearranged OFFset,TIME sorts in update to properly
!                             handle 3-D parameter lists.
!                           Added logic in mute_list_consistency() to remove
!                             rows of FNIL from lists.
! 39. 2000-06-14  O'Brien   Fixed bug in OFF and TIME traps
! 38. 2000-05-25  O'Brien   Reworked trapping routines for linked lists
!                           Allow true OT_3D_HEAD and OT_3D_TAIL muting
! 37. 2000-05-17  O'Brien   Implemented EzGUI Layout
! 36. 2000-03-02  O'Brien   Fixed bug in velocity mute option.
! 35. 2000-02-23  O'Brien   Reworked several GUI traps.
! 34. 2000-02-08  O'Brien   Various adjustments for GUI response.
! 33. 2000-02-01  O'Brien   Revised document regarding dead trace headers.
! 32. 2000-01-31  O'Brien   Removed FILENAME_LENGTH parameter as it's now
!                             available through pathcheck_module
! 31. 2000-01-27  O'Brien   Reorganized calls to pc_get_global and use
!                             pc_put_process in place of pc_put where
!                             appropriate.
! 30. 2000-01-20  O'Brien   Revised logic in traps for OPT_SFNZV
!                             and PATHNAME. Added LAV calculation after
!                             muting in accord with new "process
!                             responsibilities" document.
! 29. 1999-12-30  O'Brien   Adjusted comment in Revision history item 27.
! 28. 1999-12-29  O'Brien   Brought xml tags up to date
! 27. 1999-12-08  O'Brien   Added RCS Identifyer variable.
!                           Fixed time registration in offset-time mode
! 26. 1999-10-15  O'Brien   Properly allocate offset array
! 25. 1999-09-13  O'Brien   Updates for conformance with new pc_module
! 24. 1999-08-31  O'Brien   Full f90 conversion.
! 23. 1998-11-10  Vunderink Begin using the f90 compiler.
! 22. 1998-04-27  Vunderink Added HWFNZ parameter for options 8 and 9.
! 21. 1998-03-04  Vunderink Fixed bug in options 8 and 9 in using T0.
! 20. 1995-01-03  Stoeckley Add new type of 3D mute file.
! 19. 1992-07-16  Peterson  When OFF(1) is greater than OFF(NOFF), invert
!                           OFF and TIME arrays for easier interpolation.
! 18. 1992-03-10  Peterson  Correct options 8 and 9 to utilize T0.
! 17. 1992-02-27  Stoeckley Allow offsets on mute file comment cards to
!                           be in decreasing order.
! 16. 1992-02-19  Troutt    Add logic for new header word 64 which contains
!                           the tail mute index. HW64 represents the index
!                           of the last "desirable" value in the trace.
!                           This update includes the addition of options
!                           10 and 11 plus the setting of HW64 for options
!                           4,5,6, and 9.
! 15. 1992-02-17  Troutt    Fixed a bug with options 8 and 9 (mutes
!                           from a file) which previously disabled the
!                           cosine taper for these options.
! 14. 1992-01-16  Stoeckley Replace call to STATREPL with STATREPY.
! 13. 1992-01-10  Stoeckley Add option for tail mute times from a file,
!                           plus replace nils on mute file by interpolated
!                           values, plus add capability to use mute file
!                           with comment card indirect addressing, plus
!                           add automatic optional negative offsets when
!                           using a mute file.
! 12. 1991-10-24  Peterson  Add option for head mute times from a file.
! 11. 1991-06-10  Peterson  Correction in SUBROUTINE MUTEAPL to COMMON
!                           /MUTEP1/.   Had left out new NHF parameter.
! 10. 1991-06-03  Peterson  Added parameter to selectivly mute by header
!                           word.  HF#
!  9. 1990-06-26  Howard    Make internally callable.
!  8. 1990-03-15  Troutt    Add checks at setup for LIN and BAS increasing
!  7. 1988-09-23  Ball      NWIH and NWPT Conversion
!  6. 1988-06-01  Baumel    Add OPT=7; make conventions more like Conseis.
!  5. 1988-04-22  Baumel    Put in CPSPRT calls.
!  4. 1987-04-30  Howard    Fix GETS call in OPT=3.
!  3. 1987-04-08  Hanson    Add NCODE for history records.
!  2. 1986-07-15  Baumel    Add options 4,5,6
!  1. 1986-04-08  Howard    Original version.
!
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
!                    SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                   SPECIFIC CALLING CHARACTERISTICS
!
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
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
!                  ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                 ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                          PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS MUTE Process/NC=80>
!
!Kill undesirable samples at top or bottom of trace.
!
!HDR_FLAG=`II OPT_MUTE=`CCCCCCCCC
! 
!COMMENT= `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!LEN_TAPER=`FFFFFFF HDR_OFF=`II VEL_MUTE=~~`FFFFFFF TIM_ADD=`FFFFFFF
!
!HDR_X=~~`II      HDR_Y=`II     HDR_INTERP=`II      OPT_SFNZV=`CC
!
!Select PATHNAME[PATHNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                [PATHNAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!OFF         TIME        X_VALUES    Y_VALUES 
!`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!<PARMS COMMENT[/ML=128/XST]>
!<PARMS PATHNAME[/ML=128/XST]>
!<PARMS OFF_ARRAYSET[/XST/YST]>
!</gui_def>



!<HelpSection>

!<Help KEYWORD="SELECT_PATHNAME">
!<Tip> Choose PATHNAME using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME. </Tip>
!</Help>
!
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0 
! Allowed = 0 - NWIH
!
! If HDR_FLAG = 0, then all traces will be processed.  Otherwise, only traces 
! with a flag set in header word HDR_FLAG will be processed.  
!
!</Help>
!
!<Help KEYWORD="OPT_MUTE">
!<Tip> Type of operation for the mute process. </Tip>
! Default = FILE_HEAD
! Allowed = FILE_HEAD   Head mute using time values in mute file.
! Allowed = FILE_TAIL   Tail mute using time values in mute file.
! Allowed = OT_HEAD     Head mute by offset and time.
! Allowed = OT_TAIL     Tail mute by offset and time.
! Allowed = OT_3D_HEAD  Head mute by offset, time and grid.
! Allowed = OT_3D_TAIL  Tail mute by offset, time and grid.
! Allowed = VEL_HEAD    Head mute by velocity method.
! Allowed = VEL_TAIL    Tail mute by velocity method.
! Allowed = SET_HEAD    Set head mute header value from trace sample values.
! Allowed = SET_TAIL    Set tail mute header value from trace sample values.
! Allowed = REST_HEAD   Restore head mute from head mute header value.
! Allowed = REST_TAIL   Restore tail mute from tail mute header value.
!</Help>
!
!<Help KEYWORD="COMMENT">
!<Tip> One line comment for history file. </Tip>
! Default = blank
! Allowed = char 
!</Help>
!
!<Help KEYWORD="LEN_TAPER">
!<Tip> Length of cosine taper, in seconds. </Tip>
! Default = 0.060
! Allowed = real>=0.0  
!</Help>
!
!<Help KEYWORD="HDR_OFF">
!<Tip> Header word designating offset. </Tip>
! Default = 6
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="VEL_MUTE">
!<Tip> Velocity that defines mute function slope for velocity mute method.</Tip>
! Default = 0.0
! Allowed = real>=0.0
!
! Mute time = (offset / VEL_MUTE) + TIM_ADD.
!
! Setting VEL_MUTE = 0.0 is equivalent to infinite velocity = zero slowness.
!</Help>
!
!<Help KEYWORD="TIM_ADD">
!<Tip> Additional mute time for velocity mute method. </Tip>
! Default = 0.0
! Allowed = real
!
! Mute time = (offset / VEL_MUTE) + TIM_ADD.
!
!</Help>
!
!<Help KEYWORD="OFF">
!<Tip> Array of increasing offsets for offset-time mute. </Tip>
! Default =  -
! Allowed = real (array)
!</Help>
!
!<Help KEYWORD="TIME">
!<Tip> Array of mute time values for offset-time mute. </Tip>
! Default =  -
! Allowed = real (array)
!</Help>
!
!<Help KEYWORD="HDR_X">
!<Tip> Header word for arbitrary x coordinate. </Tip>
! Default = 7
! Allowed = 0 - NWIH
! Header word for arbitrary x coordinate to use in specifying grid and offset
! varying mute.
!</Help>
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word for arbitrary y coordinate. </Tip>
! Default = 8
! Allowed = 0 - NWIH
! Header word for arbitrary y coordinate to use in specifying grid and offset
! varying mute.
!</Help>
!
!<Help KEYWORD="X_VALUES">
!<Tip> Array of HDR_X values for spatially varying mute. </Tip>
! Default = 0
! Allowed = real (array)
! Array of HDR_X values for specifying spatially varying mute.
!</Help>
!
!<Help KEYWORD="Y_VALUES">
!<Tip> Array of HDR_Y values for spatially varying mute.</Tip>
! Default = 0
! Allowed = real (array)
! Array of HDR_Y values for specifying spatially varying mute.
!</Help>
!
!<Help KEYWORD="HDR_INTERP">
!<Tip> Header used for interpolation in a mute by file option.</Tip>
! Default = 0
! Allowed = 0 - NWIH
! User specified header word for interpolation in a mute by file option. If
! HDR_INTERP = 0 then use the header word specified in the mute file (this is
! the recommended choice.)
!</Help>
!
!<Help KEYWORD="OPT_SFNZV">
!<Tip> Option whether to set mute header to the first non-zero value.</Tip>
! Default = YES
! Allowed = YES/NO
! If OPT_SFNZV = YES, then set the head/tail mute header word to the index of
! the first/last non-zero sample in the trace (normal operation).
! If OPT_SFNZV = NO, then set the head/tail mute header word to the index of
! the first sample after/before the specified mute time.
!</Help>
!
!<Help KEYWORD="PATHNAME">
!<Tip> Pathname for the mute file to be used by the MUTE process. </Tip>
! Default = NONE
! Allowed = char*60
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module mute_module

      use pc_module
      use named_constants_module
      use sort_module
      use lav_module
      use string_module
      use getlun_module
      use rcpfile_module
      use pathcheck_module
      use pathchoose_module
      use sizeof_module
      use interp_module

      implicit none

      private
      public :: mute_create   
      public :: mute_initialize
      public :: mute_update  
      public :: mute_delete
      public :: mute            ! main execution (trace processing) routine.
      public :: mute_wrapup

      character(len=100),public,save :: MUTE_IDENT = &
             '$Id: mute.f90,v 1.54 2007/12/05 15:05:53 Stoeckley beta sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: mute_struct
        private
        logical            :: skip_wrapup   ! wrapup flag

      ! parameters
        integer            :: hdr_flag      ! header word to key on for muting
        character(len=10)  :: opt_mute      ! mute option
        character(len=80)  :: comment       ! comment card for history
        real               :: len_taper     ! length of the cos taper (seconds)
        integer            :: hdr_off       ! header word with offset value
        real               :: vel_mute      ! muting velocity
        real               :: tim_add       ! zero-offset time for vel mute
        real,      pointer :: offset(:)     ! offset array for offset-time mute
        real,      pointer :: time(:)       ! time array for offset-time mute
        integer            :: hdr_x         ! header word for X coord.
        integer            :: hdr_y         ! header word for Y coord.
        real,      pointer :: x_values(:)   ! array of X coordinates
                                            !   for spacially vaying mute
        real,      pointer :: y_values(:)   ! array of Y coordinates
                                            !   for spacially vaying mute
        character(len=3)   :: opt_sfnzv     ! option for setting/not setting
                                            !   the header mute words to the
                                            !   first non-zero value
        character(len=FILENAME_LENGTH) :: pathname
                                            ! name of mute file including path

      ! globals
        integer            :: nwih          ! number of words in header array
        integer            :: ndpt          ! number of points per trace
        real               :: dt            ! sample interval
        real               :: tstrt         ! time of first sample

      ! dependent variables
        integer            :: n_off         ! number of offsets (and times)
        integer            :: n_tim         ! number of times (and offsets)
        integer            :: nx            ! number of X values
        integer            :: ny            ! number of Y values
        integer            :: nlist         ! size of linked lists
        type(CPOINTER)     :: mute_pointer  ! pointer to mute file
        integer            :: ntaper        ! number of samples in vector taper
        real               :: slowness_mute ! 1./vel_mute
        real,      pointer :: taper(:)      ! a vector with taper weights
        real,      pointer :: ttab1d(:)     ! 1D a vector with 3-D mute times
        real,      pointer :: ttab3d(:,:,:) ! 3D a vector with 3-D mute times
        character(len=240) :: localfile     ! the local name of the mute file
        integer,dimension(60) :: Cfileid     ! local file id from C routines
        type(pathchoose_struct),pointer :: dialog

      end type mute_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(mute_struct),pointer,save :: object           ! needed for traps.
      character(len=10),save         :: mute_options(12) ! used for traps
      integer                        :: mute_nopt=12     ! used for GUI menu

      data mute_options/'FILE_HEAD',  'FILE_TAIL',  'OT_HEAD',   'OT_TAIL',  &
                        'OT_3D_HEAD', 'OT_3D_TAIL', 'VEL_HEAD',  'VEL_TAIL', &
                        'SET_HEAD',   'SET_TAIL',   'REST_HEAD', 'REST_TAIL'/

      contains


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


      subroutine mute_trap (keyword)

      implicit none

      character(len=*)   :: keyword                  ! arguments


      integer            :: j, n_here, nlist, il          ! local
      integer,allocatable:: isrt_indx(:)                  ! local
!------------------------------------------------------

      select case (keyword)

        case ('HDR_FLAG')
          if (object%hdr_flag<0 .or. object%hdr_flag>object%nwih) then
            call pc_warning &
              ('MUTE Warning: HDR_FLAG not in range 0 to ',object%nwih, &
               'Resetting to default: 0')
            object%hdr_flag = 0
          endif 


        case ('HDR_X')
          if ( index(object%opt_mute,'3D') > 0 ) then
            if (object%hdr_x<1 .or. object%hdr_x>object%nwih) then
              call pc_warning &
                ('MUTE Warning: HDR_X  not in range 1 to ',object%nwih, &
                 'Resetting to default: HDR_MIDPOINT_XGRID (7)')
              object%hdr_x = HDR_MIDPOINT_XGRID
            endif 
          endif 


        case ('HDR_Y')
          if ( index(object%opt_mute,'3D') > 0 ) then
            if (object%hdr_y<1 .or. object%hdr_y>object%nwih) then
              call pc_warning &
                ('MUTE Warning: HDR_Y  not in range 1 to ',object%nwih, &
                 'Resetting to default: HDR_MIDPOINT_YGRID (8)')
              object%hdr_y = HDR_MIDPOINT_YGRID
            endif 
          endif 


        case ('HDR_OFF')
          if ( index(object%opt_mute,'OT') > 0 ) then
            if (object%hdr_off<1 .or. object%hdr_off>object%nwih) then
              call pc_warning &
                ('MUTE Warning: HDR_OFF  not in range 1 to ',object%nwih, &
                 'Resetting to default: HDR_OFFSET (6)')
              object%hdr_off = HDR_OFFSET
            endif 
          endif 


        case ('OPT_MUTE')
          call string_to_upper(object%opt_mute)
          if ( all(mute_options/=object%opt_mute)) then
            call pc_warning('Invalid mute operation. '//object%opt_mute)
            call pc_info('Resetting to default: FILE_HEAD.')
            object%opt_mute = 'FILE_HEAD'
          endif


        case ('OPT_SFNZV')
          call string_to_upper(object%opt_sfnzv)
          if (object%opt_sfnzv(1:1).ne.'Y' .and. &
              object%opt_sfnzv(1:1).ne.'N') then
            call pc_info("Mute: OPT_SFNZV being reset to default value 'YES'")
            object%opt_sfnzv = 'YES'
          endif


        case ('OFF_TIME')
          nlist = object%nlist  ! For convenience in avoiding line wraps

          ! Check that info is truely 1-D. 3-D info might confuse sorts so
          ! warn users.
          if ( any(object%x_values(1:nlist)/=object%x_values(1)) .or. &
               any(object%y_values(1:nlist)/=object%y_values(1)) ) then
            call pc_warning &
              ('MUTE warning: 3-D X,Y information can confuse the sort order &
               & of OFFset,TIME lists when OPT_MUTE == ',object%opt_mute)
          endif

          ! Sort the OFFset,TIME,X,Y arrays by increasing OFFset,X,Y
          allocate(isrt_indx(nlist))
          isrt_indx(1:nlist) = (/(j,j=1,nlist)/)
          call sort_insisort(object%offset,  isrt_indx,1,nlist)
          call sort_insisort(object%x_values,isrt_indx,1,nlist)
          call sort_insisort(object%y_values,isrt_indx,1,nlist)
          object%offset  (1:nlist) = object%offset  (isrt_indx(1:nlist))
          object%time    (1:nlist) = object%time    (isrt_indx(1:nlist))
          object%x_values(1:nlist) = object%x_values(isrt_indx(1:nlist))
          object%y_values(1:nlist) = object%y_values(isrt_indx(1:nlist))
          deallocate(isrt_indx)


        case ('PATHNAME')
          if ( index(object%opt_mute,'FILE') > 0 ) then
            if (adjustl(object%pathname) == PATHCHECK_EMPTY) then
              call pc_error('MUTE Error: Invalid PATHNAME.')
              call pc_error('Parameter PATHNAME is required for &
                            &OPT_MUTE = ',object%opt_mute)
            endif
          endif


        case ('ARRAY_SIZES')
          if ( index(object%opt_mute,'OT')>0 .and. &
               (object%n_off==0 .or. object%n_tim==0) ) then
            call pc_error('MUTE: For OPT_MUTE = ',object%opt_mute, &
                          'OFF and TIME arrays must be specified.')
          endif
          if ( index(object%opt_mute,'3D')>0 .and. &
               (object%nx==0 .or. object%ny==0) ) then
            call pc_error('MUTE: For OPT_MUTE = ',object%opt_mute, &
                          'X_VALUES and Y_VALUES arrays must be specified.')
          endif
          if ( index(object%opt_mute,'3D')>0 .and.  &
               (object%n_off /= object%n_tim .or.   &
                object%n_off /= object%nx    .or.   &
                object%n_off /= object%ny)        ) then
            call pc_error('MUTE: For OPT_MUTE = ',object%opt_mute, &
                          'OFF, TIME, X_VALUES and Y_VALUES arrays must &
                          &have the same number of elements.')
          endif


        case ('OT_ARRAYSET_END')
          ! Find the last valid OFFset,TIME pair for the first X,Y pair
          n_here = 1
          do
            if( n_here == object%n_off .or. &
                (object%x_values(n_here) /= object%x_values(n_here+1)) .or. &
                (object%y_values(n_here) /= object%y_values(n_here+1)) ) exit
            n_here = n_here+1
            cycle
          enddo
          object%nlist = n_here

          ! Verify that TIMEs match OFFsets
          do il = 1, object%nlist
            if( (object%offset(il)==FNIL .and. object%time(il)/=FNIL) .or. &
                (object%offset(il)/=FNIL .and. object%time(il)==FNIL) ) then
              call pc_error &
                ('MUTE: The OFFset and TIME lists must have matched pairs. &
                 &Check index',il)
              call pc_jump_array_element('OFF',il)
              exit
            endif
            cycle
          enddo


        case ('OT_3D_ARRAYSET_END')
          do il = 1,object%nlist
            if ( object%offset(il) == FNIL ) then
              call pc_error('MUTE: Array element',il, &
                            ' is missing from the OFFset array.')
              call pc_jump_array_element('OFF',il)
              exit
            endif
            if ( object%time(il) == FNIL ) then
              call pc_error('MUTE: Array element',il, &
                            ' is missing from the TIME array.')
              call pc_jump_array_element('TIME',il)
              exit
            endif
            if ( object%x_values(il) == FNIL ) then
              call pc_error('MUTE: Array element',il, &
                            ' is missing from the X_VALUES array.')
              call pc_jump_array_element('X_VALUES',il)
              exit
            endif
            if ( object%y_values(il) == FNIL ) then
              call pc_error('MUTE: Array element',il, &
                            ' is missing from the Y_VALUES array.')
              call pc_jump_array_element('Y_VALUES',il)
              exit
            endif
          enddo

        case default

      end select
      return

      end subroutine mute_trap


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine mute_create (obj)
      implicit none
      type(mute_struct),pointer :: obj       ! arguments


      nullify  (obj)
      allocate (obj)

      nullify(obj%offset)               ! Nullify all pointers like this.
      nullify(obj%time)
      nullify(obj%x_values)
      nullify(obj%y_values)
      nullify(obj%taper)
      nullify(obj%ttab1d)
      nullify(obj%ttab3d)
      nullify (obj%dialog) ! jpa

      call pathchoose_create (obj%dialog, 'pathname', 'mute')
      call mute_initialize   (obj)
      return
      end subroutine mute_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine mute_delete (obj)
      implicit none
      type(mute_struct),pointer :: obj       ! arguments

      call mute_wrapup (obj)

! Make sure ALL POINTERS in your parameter structure are deallocated
      if(associated(obj%offset))     deallocate (obj%offset)
      if(associated(obj%time))       deallocate (obj%time)
      if(associated(obj%x_values))   deallocate (obj%x_values)
      if(associated(obj%y_values))   deallocate (obj%y_values)
      if(associated(obj%taper))      deallocate (obj%taper)
      if(associated(obj%ttab1d))     deallocate (obj%ttab1d)
      if(associated(obj%ttab3d))     deallocate (obj%ttab3d)

      call pathchoose_delete (obj%dialog)

      deallocate(obj)

      return
      end subroutine mute_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine mute_initialize (obj)
      implicit none
      type(mute_struct),pointer :: obj       ! arguments

! Initialize user controlable scalar variables
      obj%hdr_flag   = 0
      obj%opt_mute   = 'FILE_HEAD'
      obj%len_taper  = 0.06
      obj%comment    = ''
      obj%hdr_off    = HDR_OFFSET
      obj%vel_mute   = 0.0
      obj%tim_add    = 0.0
      obj%hdr_x      = HDR_MIDPOINT_XGRID
      obj%hdr_y      = HDR_MIDPOINT_YGRID
      obj%opt_sfnzv  = 'YES'
      obj%pathname   = ''

! Initialize structure globals
      obj%nwih = INIL
      obj%ndpt = INIL
      obj%dt   = FNIL

! Initialize all the rest
      obj%n_off = 0
      obj%n_tim = 0
      obj%nx    = 0
      obj%ny    = 0
      obj%nlist = 0
      obj%ntaper       = 0
      obj%localfile    = ""
      obj%Cfileid      = 0

      call mute_update (obj)

      return
      end subroutine mute_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine mute_update (obj)

      implicit none

      type(mute_struct),target :: obj                ! arguments

      integer            :: ier1 ! local
      integer            ::   j ! local
      integer            :: update_state             ! local
      real               :: fac1                     ! local

      integer            :: nstore, nscratch         ! local
      integer            :: lun, ier                 ! local
      character(len=3)   :: need_label,need_request  ! local
      character(len=3)   :: twosets                  ! local

      integer   :: SIZEOF_INT
      integer   :: SIZEOF_REAL

      SIZEOF_INT  = sizeof(1)
      SIZEOF_REAL = sizeof(1.0)

      nullify(object)
      object => obj         ! needed for traps.
      obj%skip_wrapup = .true.

      update_state = pc_get_update_state()


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      if (pathchoose_update(obj%dialog, obj%pathname)) return

      call pc_register_array_names ("off_arrayset", (/  &
                                    "off     ",         &
                                    "time    ",         &
                                    "x_values",         &
                                    "y_values" /))

      call pc_get_global('NWIH',  obj%nwih )
      call pc_get_global('NDPT',  obj%ndpt )
      call pc_get_global('DT',    obj%dt   )
      call pc_get_global('TSTRT', obj%tstrt)

      call pc_get ('HDR_FLAG',   obj%hdr_flag,   mute_trap)
      call pc_get ('OPT_MUTE',   obj%opt_mute,   mute_trap)
      call pc_get ('LEN_TAPER',  obj%len_taper,  mute_trap)
      call pc_get ('COMMENT',    obj%comment,    mute_trap)
      call pc_get ('HDR_OFF',    obj%hdr_off,    mute_trap)
      call pc_get ('VEL_MUTE',   obj%vel_mute,   mute_trap)
      call pc_get ('TIM_ADD',    obj%tim_add,    mute_trap)
      call pc_get ('HDR_X',      obj%hdr_x,      mute_trap)
      call pc_get ('HDR_Y',      obj%hdr_y,      mute_trap)
      call pc_get ('OPT_SFNZV',  obj%opt_sfnzv,  mute_trap)
      call pc_get ('PATHNAME',   obj%pathname )

      call pc_alloc ('OFF',      obj%offset,     obj%n_off)
      call pc_alloc ('TIME',     obj%time,       obj%n_tim)
      call pc_alloc ('X_VALUES', obj%x_values,   obj%nx   )
      call pc_alloc ('Y_VALUES', obj%y_values,   obj%ny   )

   
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

  !   if ( pc_verify_scalar('PATHNAME') ) then
        call pathcheck (KEYWORD='PATHNAME', PATHNAME=obj%pathname, &
                        EXT='', REQUIRED=.false., SCREEN='MUTE',   &
                        show=PATHCHECK_INFO_INPUT)
  !   endif

! Lists of offset time parameters should be consistent because when accessed
! from the front end, users can only alter values up to the known size of
! each list
      if ( index(obj%opt_mute,'OT') > 0 ) call mute_list_consistency (obj)

      if (obj%opt_mute(1:7)=='OT_HEAD' .or. obj%opt_mute(1:7)=='OT_TAIL') then
        call mute_trap ('OFF_TIME')
      endif

! Run the traps needed when GUI users assert the parameters are "OK".
      if ( pc_verify_end() ) then
        call mute_trap ('PATHNAME')
        if ( index(obj%opt_mute,'FILE') > 0 ) then
          call pathcheck (KEYWORD='PATHNAME', PATHNAME=obj%pathname, &
                          EXT='', REQUIRED=.true., SCREEN='MUTE')
        endif
        call mute_trap ('ARRAY_SIZES')
        if (obj%opt_mute(1:7)=='OT_HEAD' .or. obj%opt_mute(1:7)=='OT_TAIL') &
          call mute_trap ('OT_ARRAYSET_END')
        if ( index(obj%opt_mute,'3D') > 0 ) &
          call mute_trap ('OT_3D_ARRAYSET_END')
      endif

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field ('OPT_MUTE', mute_options, mute_nopt)
      call pc_put_options_field ('OPT_SFNZV', (/'YES','NO '/), 2)

      call pc_put ('HDR_FLAG',   obj%hdr_flag  )
      call pc_put ('OPT_MUTE',   obj%opt_mute  )
      call pc_put ('LEN_TAPER',  obj%len_taper )
      call pc_put ('COMMENT',    obj%comment   )
      call pc_put ('HDR_OFF',    obj%hdr_off   )
      call pc_put ('VEL_MUTE',   obj%vel_mute  )
      call pc_put ('TIM_ADD',    obj%tim_add   )
      call pc_put ('HDR_X',      obj%hdr_x     )
      call pc_put ('HDR_Y',      obj%hdr_y     )
      call pc_put ('OPT_SFNZV',  obj%opt_sfnzv )
      call pc_put ('PATHNAME',   obj%pathname  )

      call pc_put ('OFF',      obj%offset,   obj%nlist)
      call pc_put ('TIME',     obj%time,     obj%nlist)
      call pc_put ('X_VALUES', obj%x_values, obj%nlist)
      call pc_put ('Y_VALUES', obj%y_values, obj%nlist)

!!----------------------- end session trap ---------------------------------!!
!!----------------------- end session trap ---------------------------------!!
!!----------------------- end session trap ---------------------------------!!

      need_label   = 'NO'
      need_request = 'NO'
      twosets      = 'NO'

      nstore   = 0
      nscratch = 0

      if (obj%opt_mute/='SET_HEAD' .and. obj%opt_mute/='SET_TAIL') &
        nstore = nstore + (obj%len_taper/obj%dt)*SIZEOF_REAL
      
      if ( index(obj%opt_mute,'3D') > 0 ) then
        nstore   = nstore  + obj%nlist * 2 * SIZEOF_REAL ! ttab estimate
        nscratch = nscratch+ obj%nlist * 4 * SIZEOF_REAL ! tmp copies of lists
        nscratch = nscratch+ obj%nlist * SIZEOF_REAL     ! for sorting lists
        nscratch = nscratch+ obj%nlist * SIZEOF_INT      ! for sorting lists
      endif

      call pc_put_control ('nstore',             nstore) 
      call pc_put_control ('nscratch',         nscratch) 
      call pc_put_control ('need_label',     need_label) 
      call pc_put_control ('need_request', need_request) 
      call pc_put_control ('twosets',           twosets) 

      call pc_put_control ('PARALLEL_SAFE'        ,.true.)
      call pc_put_control ('PCPS_SEND_MODE'       ,'PCPS_SEND_FIRST_AVAIL')
      call pc_put_control ('PCPS_RECEIVE_MODE'    ,'PCPS_RECEIVE_PASSTHRU')
      call pc_put_control ('PCPS_BUNCH_MODE'      ,'PCPS_BUNCH_TRACE_GROUPS')
      call pc_put_control ('PCPS_SEND_EOF_MODE'   ,'PCPS_SEND_ALL_EOF')
      call pc_put_control ('PCPS_ALT_SEND_MODE'   ,'PCPS_SEND_ALL')
      call pc_put_control ('PCPS_ALT_RECEIVE_MODE','PCPS_RECEIVE_ALL_EOF')

!!----------------------- set GUI sensitivity flags ------------------------!!
!!----------------------- set GUI sensitivity flags ------------------------!!
!!----------------------- set GUI sensitivity flags ------------------------!!

      if ( update_state == PC_GUI .or. update_state == PC_FRONTEND ) then
        call mute_set_sensitivities(obj)
      endif

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


! verify that execution is warrented
      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

! Open a mutefile as needed.
      obj%localfile = "%MUTE."//trim(string_ii2ss(pc_get_ipn()))
      if (obj%opt_mute(1:4) == 'FILE') then
        call rcpfile(obj%localfile,obj%pathname,'GET')
        call getlun (lun)
        open(unit=lun, file=obj%localfile, status='OLD', iostat=ier)
        if (ier /= 0) then
          call pc_error('MUTE: Unable to open local mute file ')
        else
          ! Update filename that fortran sees
          ! wmm -- you need enough room in localfile to hold the resolved pathname of the "lun".
          inquire(lun, name=obj%localfile,iostat=ier)
          close(lun,status='KEEP') 
          ! Convert char to hollerith and terminate for clean handoff to C.
          call string_cc2hh (obj%localfile, obj%Cfileid)
          ! mutefile_wrapper_open() is a C function.
          call mutefile_wrapper_open (obj%Cfileid, obj%mute_pointer)
        endif
      endif

! build the array of taper weights upon execution.
      if (associated(obj%taper)) deallocate(obj%taper)
      if (obj%len_taper > 0.0) then
        obj%ntaper = obj%len_taper/obj%dt - 0.000001
        if (obj%ntaper > 0) then
          allocate(obj%taper(obj%ntaper), stat=ier1)
          if (ier1 /= 0) call pc_error ('Eror in MUTE: &
            &Could not allocate ',obj%ntaper,' words of memory')
          fac1 = PI*obj%dt/2.0/obj%len_taper
          do j = 1, obj%ntaper
            obj%taper(j) = sin(fac1*j)**2
          enddo
          call pc_info('MUTE set up to use ',obj%ntaper,' point taper.')
        endif
      else
        obj%ntaper = 0
      endif

! Need to fill ttab3d for 3D muting with time/offset params
      if ( index(obj%opt_mute,'3D') > 0 ) call mute_create_ttab (obj)

! Put these new pieces of info into the parameter cache for the process
! but not for the GUI.
      call pc_put_process('ntaper', obj%ntaper)
      if ( obj%ntaper > 0 ) then
        call pc_put_process('taper', obj%taper, obj%ntaper)
      endif
      if ( associated(obj%ttab1d) .and. obj%nx*obj%ny > 0 ) then
        call pc_put_process('ttab', obj%ttab1d, obj%n_off*obj%nx*obj%ny)
      endif


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine mute_update



!*******************************************************************************
! Subroutine to ensure consistent offset parameter list length
!*******************************************************************************
      subroutine mute_list_consistency (obj)

      implicit none

      type(mute_struct) :: obj
      integer           :: i_out,ilist
      real, allocatable :: rtemp(:)
!------------------------------------------------------------------------------
      obj%nlist = max(obj%n_off, obj%n_tim, obj%ny, obj%nx)

      allocate(rtemp(obj%nlist))

      if ( obj%n_off < obj%nlist ) then
        rtemp(:) = FNIL
        if (associated(obj%offset)) then
          rtemp(1:obj%n_off) = obj%offset(1:obj%n_off)
          deallocate(obj%offset)
        endif
        allocate(obj%offset(obj%nlist))
        obj%offset(1:obj%nlist) = rtemp(1:obj%nlist)
        obj%n_off = obj%nlist
      endif

      if ( obj%n_tim < obj%nlist ) then
        rtemp(:) = FNIL
        if (associated(obj%time)) then
          rtemp(1:obj%n_tim) = obj%time(1:obj%n_tim)
          deallocate(obj%time)
        endif
        allocate(obj%time(obj%nlist))
        obj%time(1:obj%nlist) = rtemp(1:obj%nlist)
        obj%n_tim = obj%nlist
      endif

      if ( obj%nx < obj%nlist ) then
        rtemp(:) = FNIL
        if (associated(obj%x_values)) then
          rtemp(1:obj%nx) = obj%x_values(1:obj%nx)
          deallocate(obj%x_values)
        endif
        allocate(obj%x_values(obj%nlist))
        obj%x_values(1:obj%nlist) = rtemp(1:obj%nlist)
        obj%nx = obj%nlist
      endif

      if ( obj%ny < obj%nlist ) then
        rtemp(:) = FNIL
        if (associated(obj%y_values)) then
          rtemp(1:obj%ny) = obj%y_values(1:obj%ny)
          deallocate(obj%y_values)
        endif
        allocate(obj%y_values(obj%nlist))
        obj%y_values(1:obj%nlist) = rtemp(1:obj%nlist)
        obj%ny = obj%nlist
      endif

      deallocate(rtemp)

      ! Remove indices with all FNILs
      i_out = 0
      do ilist = 1,obj%nlist
        if ( object%offset  (ilist) == FNIL .and. &
             object%time    (ilist) == FNIL .and. &
             object%x_values(ilist) == FNIL .and. &
             object%y_values(ilist) == FNIL      ) cycle
        i_out = i_out+1
        object%offset  (i_out) = object%offset  (ilist)
        object%time    (i_out) = object%time    (ilist)
        object%x_values(i_out) = object%x_values(ilist)
        object%y_values(i_out) = object%y_values(ilist)
      enddo
      object%n_off = i_out
      object%n_tim = i_out
      object%nx    = i_out
      object%ny    = i_out
      obj%nlist    = i_out

      return
      end subroutine mute_list_consistency


!*******************************************************************************
! Subroutine to handle all the GUI sensitivity settings during update.
!*******************************************************************************
      subroutine mute_set_sensitivities(obj)
      implicit none
      type(mute_struct) :: obj       ! arguments
!----------------------------------------------------------------

! First establish the default values
      call pc_put_sensitive_field_flag ('HDR_FLAG',   .true. )
      call pc_put_sensitive_field_flag ('OPT_MUTE',   .true. )
      call pc_put_sensitive_field_flag ('COMMENT',    .true. )
      call pc_put_sensitive_field_flag ('LEN_TAPER',  .true. )
      call pc_put_sensitive_field_flag ('HDR_OFF',    .false.)
      call pc_put_sensitive_field_flag ('VEL_MUTE',   .false.)
      call pc_put_sensitive_field_flag ('TIM_ADD',    .false.)
      call pc_put_sensitive_arrayset_flag ('OFF_ARRAYSET', .false.)
      call pc_put_sensitive_array_flag ('OFF',        .false.)
      call pc_put_sensitive_array_flag ('TIME',       .false.)
      call pc_put_sensitive_array_flag ('X_VALUES',   .false.)
      call pc_put_sensitive_array_flag ('Y_VALUES',   .false.)
      call pc_put_sensitive_field_flag ('HDR_X',      .false.)
      call pc_put_sensitive_field_flag ('HDR_Y',      .false.)
      call pc_put_sensitive_field_flag ('HDR_INTERP', .true. )
      call pc_put_sensitive_field_flag ('OPT_SFNZV',  .true. )
      call pc_put_sensitive_field_flag ('PATHNAME',   .true. )
      call pc_put_sensitive_field_flag ('SELECT_PATHNAME', .true. )
      call pc_put_sensitive_field_flag ('PATHNAME_INFO',   .true. )

! Now reset some based on opt_mute
      select case (obj%opt_mute)

        case ('FILE_HEAD','FILE_TAIL')
          ! This uses the default values that were set just above.

        case ('OT_HEAD','OT_TAIL')
          call pc_put_sensitive_field_flag ('HDR_OFF',    .true. )
          call pc_put_sensitive_arrayset_flag ('OFF_ARRAYSET', .true. )
          call pc_put_sensitive_array_flag ('OFF',        .true. )
          call pc_put_sensitive_array_flag ('TIME',       .true. )
          call pc_put_sensitive_array_flag ('X_VALUES',   .false.)
          call pc_put_sensitive_array_flag ('Y_VALUES',   .false.)
          call pc_put_sensitive_field_flag ('HDR_INTERP', .false.)
          call pc_put_sensitive_field_flag ('PATHNAME',   .false.)
          call pc_put_sensitive_field_flag ('SELECT_PATHNAME', .false. )
          call pc_put_sensitive_field_flag ('PATHNAME_INFO',   .false. )

        case ('OT_3D_HEAD','OT_3D_TAIL')
          call pc_put_sensitive_field_flag ('HDR_OFF',    .true. )
          call pc_put_sensitive_arrayset_flag ('OFF_ARRAYSET', .true. )
          call pc_put_sensitive_array_flag ('OFF',        .true. )
          call pc_put_sensitive_array_flag ('TIME',       .true. )
          call pc_put_sensitive_array_flag ('X_VALUES',   .true. )
          call pc_put_sensitive_array_flag ('Y_VALUES',   .true. )
          call pc_put_sensitive_field_flag ('HDR_X',      .true. )
          call pc_put_sensitive_field_flag ('HDR_Y',      .true. )
          call pc_put_sensitive_field_flag ('HDR_INTERP', .false.)
          call pc_put_sensitive_field_flag ('PATHNAME',   .false.)
          call pc_put_sensitive_field_flag ('SELECT_PATHNAME', .false. )
          call pc_put_sensitive_field_flag ('PATHNAME_INFO',   .false. )

        case ('VEL_HEAD','VEL_TAIL')
          call pc_put_sensitive_field_flag ('HDR_OFF',    .true. )
          call pc_put_sensitive_field_flag ('VEL_MUTE',   .true. )
          call pc_put_sensitive_field_flag ('TIM_ADD',    .true. )
          call pc_put_sensitive_field_flag ('HDR_INTERP', .false.)
          call pc_put_sensitive_field_flag ('PATHNAME',   .false.)
          call pc_put_sensitive_field_flag ('SELECT_PATHNAME', .false. )
          call pc_put_sensitive_field_flag ('PATHNAME_INFO',   .false. )

        case ('SET_HEAD','SET_TAIL')
          call pc_put_sensitive_field_flag ('LEN_TAPER',  .false.)
          call pc_put_sensitive_field_flag ('HDR_INTERP', .false.)
          call pc_put_sensitive_field_flag ('PATHNAME',   .false.)
          call pc_put_sensitive_field_flag ('SELECT_PATHNAME', .false. )
          call pc_put_sensitive_field_flag ('PATHNAME_INFO',   .false. )

        case ('REST_HEAD','REST_TAIL')
          call pc_put_sensitive_field_flag ('HDR_INTERP', .false.)
          call pc_put_sensitive_field_flag ('PATHNAME',   .false.)
          call pc_put_sensitive_field_flag ('SELECT_PATHNAME', .false. )
          call pc_put_sensitive_field_flag ('PATHNAME_INFO',   .false. )

      end select

      return
      end subroutine mute_set_sensitivities


!*******************************************************************************
! Subroutine to process linked lists, establishing a consistent set of offsets
! at each X,Y location and setting up the table of times ttab().
!*******************************************************************************
      subroutine mute_create_ttab (obj)

      implicit none

      type(mute_struct) :: obj

      integer             :: ilist, ioff, ix, iy
      integer             :: j, ier=0

      ! Temporary copies of input/output lists
      real, allocatable    :: tmp_off(:)
      real, allocatable    :: tmp_tim(:)
      real, allocatable    :: tmp_x(:)
      real, allocatable    :: tmp_y(:)

      ! Used for sorting lists
      integer, allocatable :: indices(:)
      real, allocatable    :: rtmp(:)

      ! Used for grid completion
      integer, allocatable :: grid_template(:,:)
      integer              :: iptr, inode
      real                 :: w1, w2

      ! Used for time/offset interpolation
      integer              :: iptr1,iptr2
      real                 :: delta_t, delta_o
      real                 :: slope, bias
!------------------------------------------------------------------------------

! First get the non-nil length for each parameter list
      obj%n_off = count( obj%offset(:)   /= FNIL )
      obj%n_tim = count( obj%time(:)     /= FNIL )
      obj%ny    = count( obj%y_values(:) /= FNIL )
      obj%nx    = count( obj%x_values(:) /= FNIL )

! Now establish the maximum list length
      obj%nlist = max(obj%n_off, obj%n_tim, obj%ny, obj%nx)

! Verify the existence of parameter lists
      if (obj%nlist == 0) then
        call pc_error("MUTE: OFF,TIME,X,Y parameters are absent. FATAL.")
        ier=-1
      endif

! Check the lists for length consistency
      if (obj%n_off /= 0 .and. obj%n_off /= obj%nlist) then
        call pc_error('MUTE: Parameter OFF has an inconsistent &
                      &list length')
        ier=-1
      endif

      if (obj%n_tim /= 0 .and. obj%n_tim /= obj%nlist) then
        call pc_error('MUTE: Parameter TIME has an inconsistent &
                      &list length')
        ier=-1
      endif

      if (obj%nx /= 0 .and. obj%nx /= obj%nlist) then
        call pc_error('MUTE: Parameter X_VALUES has an &
                      &inconsistent list length')
        ier=-1
      endif

      if (obj%ny /= 0 .and. obj%ny /= obj%nlist) then
        call pc_error('MUTE: Parameter Y_VALUES has an &
                      &inconsistent list length')
        ier=-1
      endif


! Return if something has caused a change in ier.
      if (ier /= 0) return

! If we made it this far, we'll be processing the lists. This means that
! we should allocate workspace for offset/time manipulation.
      allocate( tmp_off(obj%nlist) );   tmp_off = FNIL
      allocate( tmp_tim(obj%nlist) );   tmp_tim = FNIL
      allocate( tmp_x(obj%nlist)   );   tmp_x   = FNIL
      allocate( tmp_y(obj%nlist)   );   tmp_y   = FNIL

      tmp_off(1:obj%n_off) = obj%offset(1:obj%n_off)
      tmp_tim(1:obj%n_tim) = obj%time(1:obj%n_tim)
      tmp_x(1:obj%nx)      = obj%x_values(1:obj%nx)
      tmp_y(1:obj%ny)      = obj%y_values(1:obj%ny)

      if (associated(obj%offset))    deallocate (obj%offset)
      if (associated(obj%time))      deallocate (obj%time)
      if (associated(obj%y_values))  deallocate (obj%y_values)
      if (associated(obj%x_values))  deallocate (obj%x_values)

      ! Allocate and fill a vector of indices
      allocate(indices(obj%nlist)); indices(:) = (/(j,j=1,obj%nlist)/)

      ! Sort on x_values and count them, then allocate and fill the
      ! permanent x_values vector
      call sort_insisort(tmp_x,indices,1,obj%nlist)
      obj%nx= 1 + count(tmp_x(indices(2:obj%nlist))  /= &
                        tmp_x(indices(1:obj%nlist-1))  )
      allocate(obj%x_values(obj%nx))
      obj%x_values(1) = tmp_x(indices(1))
      if ( obj%nx > 1 ) then
        obj%x_values(2:)= pack(tmp_x(indices(2:obj%nlist)), &
                               MASK=(tmp_x(indices(2:obj%nlist))/= &
                                     tmp_x(indices(1:obj%nlist-1)) ))
      endif

      ! Sort on y_values and count them, then allocate and fill the
      ! permanent y_values vector
      call sort_insisort(tmp_y,indices,1,obj%nlist)
      obj%ny= 1 + count(tmp_y(indices(2:obj%nlist))  /= &
                        tmp_y(indices(1:obj%nlist-1))  )
      allocate(obj%y_values(obj%ny))
      obj%y_values(1) = tmp_y(indices(1))
      if ( obj%ny > 1 ) then
        obj%y_values(2:)= pack(tmp_y(indices(2:obj%nlist)), &
                               MASK=(tmp_y(indices(2:obj%nlist))/= &
                                     tmp_y(indices(1:obj%nlist-1)) ))
      endif

      ! Sort on offset and count them, then allocate and fill the
      ! permanent offset vector
      call sort_insisort(tmp_off,indices,1,obj%nlist)
      obj%n_off= 1 + count(tmp_off(indices(2:obj%nlist))  /= &
                           tmp_off(indices(1:obj%nlist-1))  )
      allocate(obj%offset(obj%n_off))
      obj%offset(1) = tmp_off(indices(1))
      if ( obj%n_off > 1 ) then
        obj%offset(2:)= pack(tmp_off(indices(2:obj%nlist)), &
                             MASK=(tmp_off(indices(2:obj%nlist))/= &
                                   tmp_off(indices(1:obj%nlist-1)) ))
      endif

      ! Now that we have sizes, allocate and initialize a 3D version of ttab
      allocate(obj%ttab3d(obj%n_off,obj%nx,obj%ny))
      obj%ttab3d(:,:,:) = FNIL

      ! Allocate a local grid template to keep track of parmeter presence
      allocate(grid_template(obj%nx,obj%ny))
      grid_template = 0

      ! Scan through the lists to fill ttab3d().
      do ilist = 1,obj%nlist
        ! Find the X,Y,OFFSET indices for this value in the list
        do ix = 1,obj%nx
          if(tmp_x(ilist) == obj%x_values(ix)) exit
        enddo
        do iy = 1,obj%ny
          if(tmp_y(ilist) == obj%y_values(iy)) exit
        enddo
        do ioff = 1,obj%n_off
          if(tmp_off(ilist) == obj%offset(ioff)) exit
        enddo
        ! Keep the grid template up to date
        grid_template(ix,iy) = grid_template(ix,iy) + 1
        ! Set the ttab3d value
        obj%ttab3d(ioff,ix,iy) = tmp_tim(ilist)
      enddo

   ! Now ensure that each offset has an associated time value at each
   ! X,Y location.
     
      ! Loop over Y and X looking for non-zero grid_template()
      ! When found, interpolate/extrapolate missing time values
      do iy = 1,obj%ny
        do ix = 1,obj%nx

          ! When a zero is found, cycle
          if ( grid_template(ix,iy) == 0 ) cycle

          ! if only one value, replicate
          if ( grid_template(ix,iy) == 1 ) then
            do ioff = 1,obj%n_off
              if ( obj%ttab3d(ioff,ix,iy) /= FNIL ) then
                obj%ttab3d(1:obj%n_off,ix,iy) = obj%ttab3d(ioff,ix,iy)
                exit
              endif
            enddo
            cycle
          endif

          ! making it this far means there must be muliple time/offset
          ! values at this location. interpolate/extrapolate linearly
          do ioff = 1,obj%n_off

            if ( obj%ttab3d(ioff,ix,iy) == FNIL ) then

              ! get the first index for interpolation
              if ( ioff == 1 ) then
                do iptr1 = ioff+1,obj%n_off
                  if ( obj%ttab3d(iptr1,ix,iy) /= FNIL ) exit
                enddo
              else
                iptr1 = ioff-1
              endif

              ! get the second index for interpolation
              do iptr2 = iptr1+1,obj%n_off
                if ( obj%ttab3d(iptr2,ix,iy) /= FNIL ) exit
              enddo

              ! adjust the index pointers if iptr2 is .gt. n_off
              iptr2 = min(iptr2,obj%n_off)
              if ( iptr2==obj%n_off .and. obj%ttab3d(iptr2,ix,iy)==FNIL ) then
                iptr2 = iptr1
                iptr1 = iptr1 - 1
              endif

              ! set up the line parameters
              delta_t = obj%ttab3d(iptr2,ix,iy) - obj%ttab3d(iptr1,ix,iy)
              delta_o = obj%offset(iptr2) - obj%offset(iptr1)
              slope = delta_t/delta_o
              bias  = obj%ttab3d(iptr1,ix,iy) - slope*obj%offset(iptr1)

              ! interpolate a value at this ioff
              obj%ttab3d(ioff,ix,iy) = slope*obj%offset(ioff) + bias

            endif

          enddo ! Loop over n_off
        enddo ! Loop over nx
      enddo ! Loop over ny


   ! Next interpolate parameters onto grid nodes where they are absent.
   ! Interpolation is in the Y direction only, as done in process tsvf

      ! Loop over Y and X looking for zeros in grid_template()
      do iy = 1,obj%ny
        do ix = 1,obj%nx

          ! When a zero is found, do the interpolation
          if ( grid_template(ix,iy) == 0 ) then

            ! Scan for the next node with parameters filled in.
            do iptr = ix+1,obj%nx
              if ( grid_template(iptr,iy) > 0 ) exit
            enddo
            iptr = min(iptr,obj%nx)

            ! Special cases occur at the ends of the X list where
            ! the nearest neighbor is replicated.
            ! First handle replication at begining of X list
            if (ix == 1) then
              do ioff = 1,obj%n_off
                obj%ttab3d(ioff,1:iptr-1,iy) = obj%ttab3d(ioff,iptr,iy)
              enddo

            ! Detect if this is an empty node at the end of the x_values list
            ! and handle that special case.
            elseif ( iptr==obj%nx .and. grid_template(iptr,iy)==0 ) then
              do ioff = 1,obj%n_off
                obj%ttab3d(ioff,ix:iptr,iy) = obj%ttab3d(ioff,ix-1,iy)
              enddo

            ! Empty nodes needs to be interpolated from nodes with parameters.
            elseif ( grid_template(iptr,iy) > 0 ) then
              ! Loop over adjacent empty nodes
              do inode = ix,iptr-1
                ! Establish weights for the nodes with params filled in.
                w2 =  ( obj%x_values(inode)-obj%x_values(ix-1) ) &
                     /( obj%x_values(iptr )-obj%x_values(ix-1) )
                w1 = 1.0 - w2
                ! Loop over filters interpolating all the parameters.
                do ioff = 1,obj%n_off
                  obj%ttab3d(ioff,inode,iy) = &
                     w1*obj%ttab3d(ioff,ix-1,iy)+w2*obj%ttab3d(ioff,iptr,iy)
                enddo ! Loop over n_off
              enddo ! Loop over empty grid_template nodes
            endif
          endif
        enddo ! Loop over nx
      enddo ! Loop over ny

      ! Interpolation of parameters onto unoccupied grid nodes is complete.

      ! Now set up ttab1d and reallocate time() to the size it needs to be
      ! to hold interpolated ttab functions

      obj%nlist = obj%n_off*obj%nx*obj%ny

      if ( associated(obj%ttab1d) )  deallocate(obj%ttab1d)
      allocate(obj%ttab1d(obj%nlist))

      ! Reshape ttab3d() array to ttab1d ()
      obj%ttab1d = reshape(obj%ttab3d,(/obj%nlist/))

      allocate(obj%time(obj%n_off))

      ! Clean up all the temporary space allocated
      if ( allocated(grid_template)) deallocate(grid_template)
      if ( allocated(indices) ) deallocate(indices)
      if ( allocated(rtmp)    ) deallocate(rtmp)
      if ( allocated(tmp_off) ) deallocate(tmp_off)
      if ( allocated(tmp_tim) ) deallocate(tmp_tim)
      if ( allocated(tmp_x)   ) deallocate(tmp_x)
      if ( allocated(tmp_y)   ) deallocate(tmp_y)

      ! all done
      return

      end subroutine mute_create_ttab


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine mute(obj, ntr, hd, tr)

      implicit none

      type(mute_struct)             :: obj                ! arguments
      integer,         intent(in)   :: ntr                ! arguments
      real,            intent(inout):: tr(:,:)            ! arguments
      double precision,intent(inout):: hd(:,:)            ! arguments

      integer              :: k,j                         ! local
      integer              :: j1, j2, ji                  ! local
      integer              :: imut, mhw                   ! local
      integer              :: i1,i2                       ! local
      real                 :: fmut, fac, time             ! local
      real                 :: x_here, y_here, off_here    ! local
      real                 :: delta_time,del_tim_here     ! local
      real                 :: delta_offset,del_off_here   ! local
      real                 :: time_bias                   ! local
      real, allocatable    :: t_interp(:)                 ! local

      real                 :: mutefile_wrapper_get        ! function

!-----------------------------------------------
! Bail out early if no more traces
      if (ntr == NO_MORE_TRACES) then
        call mute_wrapup(obj)
        return
      endif

! Branch to the piece of code that handles the desired option
      select case (obj%opt_mute)

      case ('REST_HEAD','REST_TAIL')

! REST_HEAD and REST_TAIL apply head/tail mutes from header words
!        HDR_TOP_MUTE and HDR_BOTTOM_MUTE

        do k = 1, ntr
!  only mute flagged trace
          if (obj%hdr_flag > 0 .and. obj%hdr_flag <= obj%nwih ) then
            if (hd(obj%hdr_flag,k)==0.0) cycle
          endif
          if (obj%opt_mute == 'REST_HEAD') then
            imut = nint(hd(HDR_TOP_MUTE,k)) - 1
          else
            imut = nint(hd(HDR_BOTTOM_MUTE,k)) + 1
!  this hw may not have been set.
            if (imut <= 1) imut = obj%ndpt
          endif
          call mute_apply (obj, imut, hd(:,k), tr(:,k))
        enddo


      case ('VEL_HEAD','VEL_TAIL')

! VEL_HEAD and VEL_TAIL are velocity head/tail mutes
!    Mute time is hd(hdr_off)/vel_mute+tim_add

        obj%slowness_mute = 0.0
        if (obj%vel_mute /= 0.0) obj%slowness_mute = 1.0/obj%vel_mute
        do k = 1, ntr
!  only mute flagged trace
          if (obj%hdr_flag > 0 .and. obj%hdr_flag <= obj%nwih ) then
            if (hd(obj%hdr_flag,k)==0.0) cycle
          endif
          imut = nint( ( hd(obj%hdr_off,k)*obj%slowness_mute &
                        +obj%tim_add - obj%tstrt )           &
                      / obj%dt ) + 1
          call mute_apply (obj, imut, hd(:,k), tr(:,k))
        enddo


      case ('OT_HEAD','OT_TAIL')

! OT_HEAD and OT_TAIL perform head/tail mutes by hd(hdr_off) and time

        do k = 1, ntr 
!  only mute flagged trace
          if (obj%hdr_flag > 0 .and. obj%hdr_flag <= obj%nwih ) then
            if (hd(obj%hdr_flag,k)==0.0) cycle
          endif

!  check ends of offset function
          if (hd(obj%hdr_off,k) <= obj%offset(1)) then
            imut = nint((obj%time(1)-obj%tstrt)/obj%dt) + 1
          elseif (hd(obj%hdr_off,k) >= obj%offset(obj%n_off)) then
            imut = nint((obj%time(obj%n_off)-obj%tstrt)/obj%dt) + 1
          else

!  interpolate offset function
            do j = 2, obj%n_off
              if (hd(obj%hdr_off,k) >= obj%offset(j)) cycle
              exit
            enddo

            delta_time   = obj%time(j)-obj%time(j-1)
            delta_offset = obj%offset(j)-obj%offset(j-1)
            del_off_here = hd(obj%hdr_off,k)-obj%offset(j-1)
            del_tim_here = delta_time*del_off_here/delta_offset
            time_bias    = obj%time(j-1) - obj%tstrt

            imut = 1+nint( (time_bias + del_tim_here) / obj%dt )
          endif

          call mute_apply (obj, imut, hd(:,k), tr(:,k))

        enddo


      case ('OT_3D_HEAD','OT_3D_TAIL')

! OT_3D_HEAD/TAIL interpolate head/tail mute offset functions on an X,Y grid.

!  get space for times interpolated at trace XY coordinates
        if (allocated(t_interp))   deallocate(t_interp)
        allocate(t_interp(obj%n_off))

        do k = 1, ntr 
!  only mute flagged trace
          if (obj%hdr_flag > 0 .and. obj%hdr_flag <= obj%nwih ) then
            if (hd(obj%hdr_flag,k)==0.0) cycle
          endif

!  intepolate times at the XY coordinate of this trace
          x_here = real(hd(obj%hdr_x,k))
          y_here = real(hd(obj%hdr_y,k))
          call interp_2d_var_lin_real &
                 (obj%x_values, obj%y_values, obj%nx, obj%ny, &
                  obj%ttab3d, obj%n_off, x_here, y_here, t_interp)

!  intepolate the time at the offset of this trace
          off_here = real(hd(obj%hdr_off,k))
          if (off_here <= obj%offset(1)) then
            time = t_interp(1)
          elseif (off_here >= obj%offset(obj%n_off)) then
            time = t_interp(obj%n_off)
          else
            call interp_1d_lin (obj%offset, off_here, i1,i2, fac)
            time = t_interp(i1)*(1-fac) + t_interp(i2)*fac
          endif
          
          imut = nint((time-obj%tstrt)/obj%dt) + 1 
          call mute_apply (obj, imut, hd(:,k), tr(:,k)) 

        enddo 


      case ('SET_HEAD','SET_TAIL')

!  SET_HEAD and SET_TAIL reset HDR_TOP_MUTE and HDR_BOTTOM_MUTE to match
!  the first and last non-zero data samples

        if (obj%opt_mute == 'SET_HEAD') then 
          mhw = HDR_TOP_MUTE 
          j1 = 1
          j2 = obj%ndpt
          ji = 1
        else 
          mhw = HDR_BOTTOM_MUTE 
          j1 = obj%ndpt
          j2 = 1
          ji = -1
        endif 

        do k = 1, ntr
          ! only mute flagged trace 
          if (obj%hdr_flag > 0 .and. obj%hdr_flag <= obj%nwih ) then
            if (hd(obj%hdr_flag,k)==0.0) cycle
          endif
          do j = j1, j2, ji 
            if (tr(j,k) == 0.) cycle  
            exit  
          enddo 
          hd(mhw,k) = j
          ! zero trace if mute header words indicated that it is dead.
          if (hd(HDR_TOP_MUTE,k) >= hd(HDR_BOTTOM_MUTE,k)) then
            tr(:,k) = 0.0
          endif
        enddo 


      case ('FILE_HEAD','FILE_TAIL')

! FILE_HEAD and FILE_TAIL apply head/tail mutes from a disk file

!  process a group of traces.
        do k = 1, ntr 
!  only mute flagged trace 
          if (obj%hdr_flag > 0 .and. obj%hdr_flag <= obj%nwih ) then
            if (hd(obj%hdr_flag,k)==0.0) cycle
          endif
          fmut = mutefile_wrapper_get(obj%mute_pointer,hd(1,k)) 
          imut = (fmut-obj%tstrt)/obj%dt
          call mute_apply (obj, imut, hd(:,k), tr(:,k)) 
        enddo 

      end select

! finish up by recomputing trace LAV values
      call lav_set_hdr (hd, tr, obj%ndpt, ntr)

      return
      end subroutine mute

 
 
!!--------------------- working subroutines --------------------------------!!
!!--------------------- working subroutines --------------------------------!!
!!--------------------- working subroutines --------------------------------!!
!-----------------------------------------------------------------------------
!  apply the mute to a single trace
!-----------------------------------------------------------------------------
      subroutine mute_apply(obj, imut, hd, tr) 

      implicit none

      type(mute_struct)              :: obj                ! arguments
      integer,         intent(in)    :: imut               ! arguments
      double precision,intent(inout) :: hd(:)              ! arguments
      real,            intent(inout) :: tr(:)              ! arguments

      integer                        :: j                  ! local
!-----------------------------------------------
      select case (obj%opt_mute)

      case('REST_HEAD','VEL_HEAD','OT_HEAD','OT_3D_HEAD','FILE_HEAD') !head mute

        tr(:min(imut,obj%ndpt)) = 0.0 

        tr(imut+max(1,1-imut):min(obj%ntaper,obj%ndpt-imut)+imut) =      &
               obj%taper(max(1,1-imut):min(obj%ntaper,obj%ndpt-imut))    &
             * tr(imut+max(1,1-imut):min(obj%ntaper,obj%ndpt-imut)+imut) 

        if (imut < obj%ndpt) then
          if (obj%opt_sfnzv == 'NO') then 
            hd(HDR_TOP_MUTE) = max(imut + 1,1) 
          else 
            do j = max(imut+1,1), obj%ndpt 
              if (tr(j) == 0.0) cycle  
              exit  
            enddo 
            hd(HDR_TOP_MUTE) = j 
          endif 
          ! zero trace if mute header words indicated that it is dead.
          if (hd(HDR_TOP_MUTE) >= hd(HDR_BOTTOM_MUTE)) then
            tr(:) = 0.0
          endif
        else
          hd(HDR_TOP_MUTE) = obj%ndpt 
          tr(:) = 0.0
        endif 


      case('REST_TAIL','VEL_TAIL','OT_TAIL','OT_3D_TAIL','FILE_TAIL') !tail mute

        tr(max(imut,1):obj%ndpt) = 0.0 

        tr(imut-max(1,imut-obj%ndpt):imut-min(obj%ntaper,imut-1):(-1)) =     &
               obj%taper(max(1,imut-obj%ndpt):min(obj%ntaper,imut-1))        &
             * tr(imut-max(1,imut-obj%ndpt):imut-min(obj%ntaper,imut-1):(-1)) 

        if (imut > 1) then
          if (obj%opt_sfnzv == 'NO') then 
            hd(HDR_BOTTOM_MUTE) = min(obj%ndpt,imut - 1) 
          else 
            do j = min(obj%ndpt,imut-1), 1, -1 
              if (tr(j) == 0.0) cycle  
              exit  
            enddo 
            hd(HDR_BOTTOM_MUTE) = j 
          endif 
          ! zero trace if mute header words indicated that it is dead.
          if (hd(HDR_TOP_MUTE) >= hd(HDR_BOTTOM_MUTE)) then
            tr(:) = 0.0
          endif
        endif 
      
      end select

      return  
      end subroutine mute_apply 


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine mute_wrapup (obj)
      implicit none
      type(mute_struct) :: obj       ! arguments

      if(obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      if (obj%opt_mute(1:4) == 'FILE') call mutefile_wrapper_kill (obj%mute_pointer)

      call pc_print ('MUTE:  Wrapup complete.')
      return
      end subroutine mute_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module mute_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

