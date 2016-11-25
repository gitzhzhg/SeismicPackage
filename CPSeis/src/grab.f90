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
!                         C P S   P R O C E S S
!
! Name       : GRAB     (GRAB static values.)
! Category   : statics
! Written    : 1989-10-12   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Copy values from a header word to a static file, or vice versa.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! GRAB copies values from a trace header word to a static file (BUILD options)
! or copies values from a static file to a trace header word (SET options).
!
! Both traces and file entries are labeled by HDR_A and HDR_B.  Individual
! traces are associated with corresponding entries in the static file when both
! the traces and the file entries are labeled by common values of HDR_A and
! HDR_B.
!
! All trace and file entry associations require identical labeling by HDR_A and
! HDR_B except for SET_VAL (the nearest value in the file is used) and SET_NIL
! (a nil value is used if there is not an exact labeling match).
!
! Two sets of Label Header Words:
! For BUILD options, when HDR_C and HDR_D are also specified, the trace value
! is used twice (once for each set of headers).
!
! For SET options, when HDR_C and HDR_D are also specified in the static file,
! the sum of the two corresponding values will be put into the trace header.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! The trace and static file label parameters, HDR_A and HDR_B, are used to set
! up a correspondence between input trace header word HDR_GRAB and identically
! labeled static file entries.  These header words are entirely unrestricted and
! the user should specify whatever header words best suit the problem at hand.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process does not alter input traces (BUILD options).
! This process does alter input trace header word values (SET options).
! This process outputs the same traces as it receives (possibly altered).
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NWIH     number of words in trace header       used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!         HDR_FLAG                   flag header word
!         HDR_GRAB                   header word corresponding to file entries
!         HDR_A - HDR_D              header words labeling traces and files
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!023. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
! 22. 2003-12-09  Stoeckley  Fix bug where SET_INTERP mode put nils into header;
!                             change SET_INTERP to interpolate between static
!                             file grid points.
! 21. 2002-09-09  Stoeckley  Change to use the MTH module for binning.
! 20. 2002-04-17  Stoeckley  Add error check while writing the file.
! 19. 2002-02-12  Stoeckley  Add file selection popup, status line, and
!                             message line; populate parameters from file
!                             and make them insensitive when reading a file;
!                             remove unused parameters from GUI; add option
!                             SET_INTERP.
! 18. 2001-12-10  Selzler    Change "(*)" arrays to "(:)" for Intel compiler.
! 17. 2001-02-13  Selzler    Changed wrapup logic to use skip_wrapup.
! 16. 2000-10-05  Selzler    Fixed gui def for new ezgui conventions.
! 15. 2000-07-07  Selzler    Fixed problems found by CPS Fortran Code Review.
! 14. 2000-03-28  Selzler    Improved GUI support
! 13. 2000-03-01  Selzler    Change to use STATIO and STATUTIL instead of
!                             STATICS_UTIL (by Stoeckley).
! 12. 2000-02-02  Selzler    Added support for GUI and general cleanup.
! 11. 1999-11-19  Selzler    Added RCS "Id" strings to tag executeable.
! 10. 1999-09-13  Selzler    Updated skip_wrapup and print_lun usage.
!  9. 1999-08-23  Selzler    Conversion to f90.
!  8. 1998-11-13  Vunderink  Begin using the f90 compiler.
!  7. 1997-06-16  Vunderink  Add SET2 option.
!  6. 1994-07-22  Troutt     Add BUILD4 and BUILD5 options.
!  5. 1994-02-11  Troutt     Add error checks for HPALLO! calls.
!  4. 1991-06-12  Stoeckley  Add MODE=BUILD2 and MODE=BUILD3.
!  3. 1990-10-23  Peterson   Include error and abort arguments on calls to
!                             HPALLOC and HPDEALLC.
!  2. 1990-05-25  Stoeckley  Convert to call several STATUTIL primitives.
!  1. 1989-10-12  Stoeckley  First version implemented.
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
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS GRAB Process/NC=80>
!     Copy values from a header word to a static file, or vice versa.
!
! OPT_MODE=`CCCCCCCCC
!
! Select PATHNAME [PATHNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [PATHNAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                  [PATHNAME_MESS]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! HDR_GRAB=`II    HDR_FLAG=`II    
!
! HDR_A= `II
! A_INIT=`FFFFFFFFFFF  A_INC=`FFFFFFFFFFF  A_LAST=`FFFFFFFFFFF  A_TOT=`IIIIII
!
! HDR_B= `II    
! B_INIT=`FFFFFFFFFFF  B_INC=`FFFFFFFFFFF  B_LAST=`FFFFFFFFFFF  B_TOT=`IIIIII
!
! HDR_C= `II  [/L](goes with HDR_A)
!
! HDR_D= `II  [/L](goes with HDR_B)
!
!<PARMS PATHNAME     [/ML=128/XST]>
!<PARMS PATHNAME_INFO[/ML=128/XST]>
!<PARMS PATHNAME_MESS[/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="OPT_MODE">
!<Tip> Option of type of build or set operation. </Tip>
! Default = BUILD_AVE
! Allowed = BUILD_AVE  (Static file value is AVERAGE of header word values.)
! Allowed = BUILD_SUM  (Static file value is SUM of header word values.)
! Allowed = BUILD_OCC  (Static file value is 1 if header bin is OCCUPIED.)
! Allowed = BUILD_MAX  (Static file value is MAXIMUM of header word values.)
! Allowed = BUILD_MIN  (Static file value is MINIMUM of header word values.)
! Allowed = SET_NEAR   (Set header word to NEAREST value from a static file.)
! Allowed = SET_NIL    (Set header word value NIL if there is no file entry.)
! Allowed = SET_INTERP (Set header word to INTERPOLATED value from static file.)
!
! The BUILD options write a static file in which the static values are
! determined from the values of header word HDR_GRAB for the input traces.
! The static file entries are labeled by the trace values of HDR_A and HDR_B.
!
! In the SET options the HDR_GRAB header word for each trace is set to a value
! determined from file entries labeled the same as the values of HDR_A and
! HDR_B for the trace.
!
! BUILD_AVE
! For option BUILD_AVE, a static file is created in which each "static"
! value is the AVERAGE of the contents of the HDR_GRAB header word
! for the input traces.  The static file entries are labeled by the trace
! values of HDR_A and HDR_B.  If HDR_C and HDR_D  are also specified, the trace
! is used twice (once for each set of headers).  If a trace does not fall
! within the range of the static file, it is not used.
!
! BUILD_SUM
! Option BUILD_SUM is identical to BUILD_AVE, except that the created static
! file contains the SUM rather than the average of the contents of the header
! word.  This variation has only specialized uses and will normally not be used.
!
! BUILD_OCC
! Option BUILD_OCC is similar to BUILD_SUM, except that the values in the
! created static file are set to 1 if one or more traces have the same label as
! the file entry, and to 0 otherwise.  This variation has only specialized
! uses and will normally not be used.
!
! BUILD_MAX
! Option BUILD_MAX is identical to BUILD_AVE, except that the created static
! file contains the MAXIMUM value rather than the average of the contents of
! the header word.  This variation has only specialized uses and will normally
! not be used.  Header values of zero will not be used (a value of zero in the
! GRAB file indicates that no nonzero values were found with the same label as
! the file entry).
!
! BUILD_MIN
! Option BUILD_MIN is identical to BUILD_AVE, except that the created static
! file contains the MINIMUM value rather than the average of the contents of
! the header word.  This variation has only specialized uses and will normally
! not be used.  Header values of zero will not be used (a value of zero in the
! GRAB file indicates that no nonzero values were found with the same label as
! the file entry).
!
! SET_NEAR
! For option SET_NEAR, the HDR_GRAB header word for each trace is set to the
! value found in the static file labeled the same as the values of HDR_A and
! HDR_B for the trace.  The NEAREST value in the file is used (even if it is
! nil) -- no interpolation is performed.  If the static file specifies two sets
! of headers, the sum of the two corresponding values will be placed into the
! trace header.  The static file is extrapolated if necessary.
!
! SET_NIL
! Option SET_NIL is identical to SET_NEAR except that if no static file entry
! is labeled the same as the values of HDR_A and  HDR_B for the trace, a
! NIL is used -- instead of nearest value or extrapolation.  This variation has
! only specialized uses and will normally not be used.
!
! SET_INTERP
! Option SET_INTERP is identical to SET_NEAR except that if no static file
! entry is labeled the same as the values of HDR_A and  HDR_B for the trace,
! an interpolated value is used (by replacing NILs with interpolated values,
! and then interpolating between adjacent values or extrapolating as neeeded).
!</Help>
!
!<Help KEYWORD="PATHNAME_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME. </Tip>
!</Help>
!
!<Help KEYWORD="PATHNAME_MESS" TYPE= "DISPLAY_ONLY">
!<Tip> Further information about input PATHNAME. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_PATHNAME">
!<Tip> Choose PATHNAME using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="PATHNAME">
!<Tip> Pathname for the static file to write (BUILD) or read (SET). </Tip>
! Default = NONE
! Allowed = char*60
! The static file will be written to (or read from) the sub-directory specified
! by PATHNAME.  PATHNAME must include the desired filename for the static file.
!</Help>
!
!<Help KEYWORD="HDR_GRAB">
!<Tip> Header word for values corresponding to static file values. </Tip>
! Default = 40
! Allowed = 1 - NWIH
! Header word from which static (or other) values will be taken (BUILD options),
! or into which the values will be placed (SET options).  The default
! corresponds to the shift-post header word.
!</Help>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 1 - NWIH
! If HDR_FLAG = 0, then all traces will be used.  Otherwise, only traces with
! a flag set in header word HDR_FLAG will be used.
!</Help>
!
!<Help KEYWORD="HDR_A">
!<Tip> HDR_A and HDR_B label traces and static files. </Tip>
! Default = 7
! Allowed = 1 - NWIH
! HDR_A and HDR_B label traces and static files.  Traces and static files are
! associated when they have common labels.
!</Help>
!
!<Help KEYWORD="HDR_B">
!<Tip> HDR_A and HDR_B label traces and static files. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! HDR_A and HDR_B label traces and static files.  Traces and static files are
! associated when they have common labels.  If HDR_B = 0, it is not used.
!</Help>
!
!<Help KEYWORD="HDR_C">
!<Tip> HDR_C and HDR_D are the second pair of labels. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! HDR_C and HDR_D are the second pair of labels for traces and static files.
! Traces and static files are associated when they have common labels.  If
! HDR_C = 0, it is not used.
!</Help>
!
!<Help KEYWORD="HDR_D">
!<Tip> HDR_C and HDR_D are the second pair of labels. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! HDR_C and HDR_D are the second pair of labels for traces and static files.
! Traces and static files are associated when they have common labels.  If
! HDR_D = 0, it is not used.
!</Help>
!
!<Help KEYWORD="A_INIT">
!<Tip> Initial value in HDR_A. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="B_INIT">
!<Tip> Initial value in HDR_B. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="A_INC">
!<Tip> Increment for HDR_A value. </Tip>
! Default = 1.0
! Allowed = real>0.0
!</Help>
!
!<Help KEYWORD="B_INC">
!<Tip> Increment for HDR_B value. </Tip>
! Default = 1.0
! Allowed = real>0.0
!</Help>
!
!
!<Help KEYWORD="A_LAST">
!<Tip> Last value in HDR_A. </Tip>
! Default = 1.0
! Allowed = real>=A_INIT
!</Help>
!
!<Help KEYWORD="B_LAST">
!<Tip> Last value in HDR_B. </Tip>
! Default = 1.0
! Allowed = real>=B_INIT
!</Help>
!
!
!<Help KEYWORD="A_TOT">
!<Tip> Total number of values in HDR_A. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="B_TOT">
!<Tip> Total number of values in HDR_B. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module grab_module
      use pc_module
      use named_constants_module
      use getlun_module
      use statutil_module
      use statio_module
      use pathcheck_module
      use pathchoose_module
      use pattern_module
      use mth_module
      use pjar_module
      implicit none
      private
      public :: grab_create     ! uses the parameter cache.
      public :: grab_initialize
      public :: grab_update     ! uses the parameter cache.
      public :: grab_delete
      public :: grab            ! main execution (trace processing) routine.
      public :: grab_wrapup


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: grab_struct
      private
        logical                    :: skip_wrapup   ! wrapup flag

        character(len=10)          :: opt_mode      ! process parameter.
        character(len=FILENAME_LENGTH) :: pathname  ! process parameter.
        integer                    :: hdr_grab      ! process parameter.
        integer                    :: hdr_flag      ! process parameter.
        integer                    :: hdr_a         ! process parameter.
        integer                    :: hdr_b         ! process parameter.
        integer                    :: hdr_c         ! process parameter.
        integer                    :: hdr_d         ! process parameter.
        real                       :: a_init        ! process parameter.
        real                       :: b_init        ! process parameter.
        real                       :: a_inc         ! process parameter.
        real                       :: b_inc         ! process parameter.
        real                       :: a_last        ! process parameter.
        real                       :: b_last        ! process parameter.
        integer                    :: a_tot         ! process parameter.
        integer                    :: b_tot         ! process parameter.

        integer                    :: nwih          ! global parameter

        type(pathchoose_struct) ,pointer         :: pathchoose  ! dependent
        real             ,pointer,dimension(:,:) :: svalue      ! dependent
        integer          ,pointer,dimension(:,:) :: scount      ! dependent
        character(len=80),pointer,dimension(:)   :: cards       ! dependent
        integer                                  :: ncard       ! dependent
        integer                                  :: lun         ! dependent
        character(len=120)                       :: mess        ! dependent
      end type grab_struct

!!??? debug support
!integer                    :: debug_lun     ! dependent parameter

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(grab_struct),pointer,save :: object      ! needed for traps.

      character(len=100),public :: grab_ident = &
        "$Id: grab.f90,v 1.23 2006/10/17 13:45:44 Glover prod sps $"

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine grab_create (obj)
      implicit none
      type(grab_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%svalue)
      nullify (obj%scount)
      nullify (obj%cards)
      nullify (obj%pathchoose) ! jpa

!!??? debug support
!call getlun(debug_lun)

      call pathchoose_create   (obj%pathchoose, 'pathname'  , '*')
      call grab_initialize (obj)

      return
      end subroutine grab_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine grab_delete (obj)
      implicit none
      type(grab_struct),pointer :: obj       ! arguments

!!??? debug support
!open(debug_lun, file='/usr/app/user/selzlrl/grab/grab.debug', &
!  status='unknown', position='append')
!write(debug_lun,*) 'GRAB_DELETE: enter subroutine'
!close (debug_lun)

      call grab_wrapup (obj)

      call pathchoose_delete  (obj%pathchoose)

      if (associated(obj%svalue)) deallocate (obj%svalue)
      if (associated(obj%scount)) deallocate (obj%scount)
      if (associated(obj%cards)) deallocate (obj%cards)

!!??? debug support
!open(debug_lun, file='/usr/app/user/selzlrl/grab/grab.debug', &
!  status='unknown', position='append')
!write(debug_lun,*) 'GRAB_DELETE: subroutine return (almost)'
!close (debug_lun)

      deallocate(obj)

      return
      end subroutine grab_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine grab_initialize (obj)
      implicit none
      type(grab_struct),pointer :: obj       ! arguments

!!??? debug support
!open(debug_lun, file='/usr/app/user/selzlrl/grab/grab.debug', &
!  status='unknown', position='append')
!write(debug_lun,*) 'GRAB_INITIALIZE: enter subroutine'
!close (debug_lun)

      obj%opt_mode = 'BUILD_AVE'
      obj%pathname = PATHCHECK_EMPTY
      obj%hdr_grab = 40
      obj%hdr_flag = 0
      obj%hdr_a = 7
      obj%hdr_b = 0
      obj%hdr_c = 0
      obj%hdr_d = 0
      obj%a_init = 1.0
      obj%b_init = 1.0
      obj%a_inc = 1.0
      obj%b_inc = 1.0
      obj%a_last = 1.0
      obj%b_last = 1.0
      obj%a_tot = 1
      obj%b_tot = 1

      obj%lun  = -1
      obj%mess = ' '

!!??? debug support
!open(debug_lun, file='/usr/app/user/selzlrl/grab/grab.debug', &
!  status='unknown', position='append')
!write(debug_lun,*) 'GRAB_INITIALIZE: call grab_update'
!close (debug_lun)

      call grab_update (obj)

      return
      end subroutine grab_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine grab_update (obj)
      implicit none
      type(grab_struct),target       :: obj                        ! arguments
      logical                        :: build_mode                 ! local
      logical                        :: verify                     ! local
      integer                        :: ier1,ier2                  ! local
      integer                        :: status                     ! local
      integer                        :: state                      ! local
      character(len=8)               :: file_type                  ! local
      character(len=80)              :: msg                        ! local
      character(len=FILENAME_LENGTH) :: pathkeep                   ! local
      logical                        :: buildkeep                  ! local
      integer                        :: hdr_a,hdr_b,hdr_c,hdr_d    ! local
      real                           :: a_init,b_init,a_inc,b_inc  ! local
      integer                        :: a_tot,b_tot                ! local
      character(len=40)              :: secname                    ! local
      type(pjar_struct),pointer      :: pjar                       ! local

      nullify (pjar) ! jpa

      object => obj          ! needed for traps.
      obj%skip_wrapup = .true.

      state = pc_get_update_state()

      if(state == PC_FRONTEND .or. state == PC_BACKEND) then
        verify = .true.
      else
        verify = .false.
      end if

!!??? debug support
!open(debug_lun, file='/usr/app/user/selzlrl/grab/grab.debug', &
!  status='unknown', position='append')
!select case(state)
!case (PC_FRONTEND)
!write(debug_lun,*) 'GRAB_UPDATE: enter subroutine, state= PC_FRONTEND'
!case (PC_GUI)
!write(debug_lun,*) 'GRAB_UPDATE: enter subroutine, state= PC_GUI'
!case (PC_BACKEND)
!write(debug_lun,*) 'GRAB_UPDATE: enter subroutine, state= PC_BACKEND'
!case (PC_EXECUTE)
!write(debug_lun,*) 'GRAB_UPDATE: enter subroutine, state= PC_EXECUTE'
!case default
!write(debug_lun,*) 'GRAB_UPDATE: enter subroutine, state= <illegal>'
!end select
!close (debug_lun)

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      pathkeep  = obj%pathname
      buildkeep = (obj%opt_mode(1:1) == 'B')

      if (pathchoose_update(obj%pathchoose, obj%pathname)) return

      obj%lun = pc_get_lun()

      call pc_get_global ('nwih', obj%nwih)

      call pc_get('opt_mode', obj%opt_mode)
      call string_to_upper(obj%opt_mode)
      call pc_get('pathname', obj%pathname)
      call pc_get('hdr_grab', obj%hdr_grab)
      call pc_get('hdr_flag', obj%hdr_flag)
      call pc_get('hdr_a', obj%hdr_a)
      call pc_get('hdr_b', obj%hdr_b)
      call pc_get('hdr_c', obj%hdr_c)
      call pc_get('hdr_d', obj%hdr_d)
      call pc_get('a_init', obj%a_init)
      call pc_get('b_init', obj%b_init)
      call pc_get('a_inc', obj%a_inc)
      call pc_get('b_inc', obj%b_inc)
      call pc_get('a_last', obj%a_last)
      call pc_get('b_last', obj%b_last)
      call pc_get('a_tot', obj%a_tot)
      call pc_get('b_tot', obj%b_tot)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      build_mode = .false.

      select case (obj%opt_mode)
      case ('BUILD_AVE')
        build_mode = .true.
      case ('BUILD_SUM')
        build_mode = .true.
      case ('BUILD_OCC')
        build_mode = .true.
      case ('BUILD_MAX')
        build_mode = .true.
      case ('BUILD_MIN')
        build_mode = .true.
      case ('SET_NEAR')
      case ('SET_NIL')
      case ('SET_INTERP')
      case default
        call pc_error('OPT_MODE must be BUILD_AVE, BUILD_SUM, &
          &BUILD_OCC, BUILD_MAX, BUILD_MIN, SET_NEAR, SET_NIL, or SET_INTERP')
        obj%opt_mode = 'BUILD_AVE'
      end select

      if (build_mode) then
           call pathcheck ('pathname', obj%pathname, '',    &
                           required=.true., status=status,  &
                           show=PATHCHECK_INFO_OUTPUT)
      else
           call pathcheck ('pathname', obj%pathname, '',    &
                           required=.true., status=status,  &
                           show=PATHCHECK_INFO_INPUT)
      end if

      if (build_mode) then
           obj%mess = 'HDR, INIT, INC, LAST, and TOT parameters&
                                      & will be used to write the file'
      else if (status /= PATHCHECK_VALID) then
           obj%mess = 'when the file is specified, HDR, INIT, INC, LAST,&
                                  & and TOT will be reset to match the file'
      else if ((obj%pathname /= pathkeep) .or. &
               (build_mode .neqv. buildkeep)) then

           secname = 'static'
           call pjar_create         (pjar)
           call statio_read_header  (obj%pathname,pjar,secname,ier1,msg)
           call pjar_choose_section (pjar,secname)
           call pjar_get            (pjar, 'nhx' , hdr_a )
           call pjar_get            (pjar, 'nhy' , hdr_b )
           call pjar_get            (pjar, 'nhx2', hdr_c )
           call pjar_get            (pjar, 'nhy2', hdr_d )
           call pjar_get            (pjar, 'x1'  , a_init)
           call pjar_get            (pjar, 'y1'  , b_init)
           call pjar_get            (pjar, 'xinc', a_inc )
           call pjar_get            (pjar, 'yinc', b_inc )
           call pjar_get            (pjar, 'nx'  , a_tot )
           call pjar_get            (pjar, 'ny'  , b_tot )
           call pjar_delete         (pjar)

           if (ier1 == STATIO_OK) then

                if (obj%hdr_a  /= hdr_a  .or. &
                    obj%hdr_b  /= hdr_b  .or. &
                    obj%hdr_c  /= hdr_c  .or. &
                    obj%hdr_d  /= hdr_d  .or. &
                    obj%a_init /= a_init .or. &
                    obj%b_init /= b_init .or. &
                    obj%a_inc  /= a_inc  .or. &
                    obj%b_inc  /= b_inc  .or. &
                    obj%a_tot  /= a_tot  .or. &
                    obj%b_tot  /= b_tot) then

                     obj%hdr_a  = hdr_a
                     obj%hdr_b  = hdr_b
                     obj%hdr_c  = hdr_c
                     obj%hdr_d  = hdr_d
                     obj%a_init = a_init
                     obj%b_init = b_init
                     obj%a_inc  = a_inc
                     obj%b_inc  = b_inc
                     obj%a_tot  = a_tot
                     obj%b_tot  = b_tot

                     status = pattern_stop2('GRAB:', verify,                   &
                                obj%a_init, obj%a_inc, obj%a_last, obj%a_tot,  &
                                'A_INIT', 'A_INC', 'A_LAST', 'A_TOT',          &
                                .true., .true., .false., .true.,               &
                                inc_min=0.0)

                     status = pattern_stop2('GRAB:', verify,                   &
                                obj%b_init, obj%b_inc, obj%b_last, obj%b_tot,  &
                                'B_INIT', 'B_INC', 'B_LAST', 'B_TOT',          &
                                .true., .true., .false., .true.,               &
                                inc_min=0.0)
           
                     obj%mess = 'HDR, INIT, INC, LAST, and TOT parameters&
                                      & changed to values found on file'
                else
                     obj%mess = 'HDR, INIT, INC, LAST, and TOT parameters&
                                      & match the values found on file'
                end if

           else if (msg == 'input file not found') then

               obj%mess =  ' '
               obj%mess = 'when the file is found, HDR, INIT, INC, LAST,&
                                  & and TOT will be reset to match the file'
           end if

      end if

      if(obj%hdr_grab < 1 .or. obj%hdr_grab > obj%nwih) then
        call pc_error( 'HDR_GRAB < 1 or > NWIH')
        obj%hdr_grab = 40
      end if

      if(obj%hdr_flag < 0 .or. obj%hdr_flag > obj%nwih) then
        call pc_error( 'HDR_FLAG < 0 or > NWIH')
        obj%hdr_flag = 0
      end if

      if(obj%hdr_a < 1 .or. obj%hdr_a > obj%nwih) then
        call pc_error( 'HDR_A < 1 or > NWIH')
        obj%hdr_a = 7
      end if

      if(obj%hdr_b < 0 .or. obj%hdr_b > obj%nwih) then
        call pc_error( 'HDR_B < 0 or > NWIH')
        obj%hdr_b = 0
      end if

      if(obj%hdr_c < 0 .or. obj%hdr_c > obj%nwih) then
        call pc_error( 'HDR_C < 0 or > NWIH')
        obj%hdr_c = 0
      end if

      if(obj%hdr_d < 0 .or. obj%hdr_d > obj%nwih) then
        call pc_error( 'HDR_D < 0 or > NWIH')
        obj%hdr_d = 0
      end if

      status = pattern_stop2('GRAB:', verify, &
        obj%a_init, obj%a_inc, obj%a_last, obj%a_tot, &
        'A_INIT', 'A_INC', 'A_LAST', 'A_TOT', &
        pc_verify_scalar('a_init'), pc_verify_scalar('a_inc'), &
        pc_verify_scalar('a_last'), pc_verify_scalar('a_tot'), &
        inc_min=0.0)

      if(obj%hdr_b > 0) then
        status = pattern_stop2('GRAB:', verify, &
          obj%b_init, obj%b_inc, obj%b_last, obj%b_tot, &
          'B_INIT', 'B_INC', 'B_LAST', 'B_TOT', &
          pc_verify_scalar('b_init'), pc_verify_scalar('b_inc'), &
          pc_verify_scalar('b_last'), pc_verify_scalar('b_tot'), &
          inc_min=0.0)
      end if


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field('opt_mode', &
        (/ "BUILD_AVE ", "BUILD_SUM ", "BUILD_OCC ", "BUILD_MAX ", &
           "BUILD_MIN ", "SET_NEAR  ", "SET_NIL   ", "SET_INTERP" /), 8)
      call pc_put('opt_mode', obj%opt_mode)
      call pc_put('pathname', obj%pathname)
      call pc_put('hdr_grab', obj%hdr_grab)
      call pc_put('hdr_flag', obj%hdr_flag)
      call pc_put('hdr_a', obj%hdr_a)
      call pc_put('hdr_b', obj%hdr_b)
      call pc_put('hdr_c', obj%hdr_c)
      call pc_put('hdr_d', obj%hdr_d)
      call pc_put('a_init', obj%a_init)
      call pc_put('b_init', obj%b_init)
      call pc_put('a_inc', obj%a_inc)
      call pc_put('b_inc', obj%b_inc)
      call pc_put('a_last', obj%a_last)
      call pc_put('b_last', obj%b_last)
      call pc_put('a_tot', obj%a_tot)
      call pc_put('b_tot', obj%b_tot)

      call pc_put_sensitive_field_flag ('hdr_a' , build_mode)
      call pc_put_sensitive_field_flag ('hdr_b' , build_mode)
      call pc_put_sensitive_field_flag ('hdr_c' , build_mode)
      call pc_put_sensitive_field_flag ('hdr_d' , build_mode)
      call pc_put_sensitive_field_flag ('a_init', build_mode)
      call pc_put_sensitive_field_flag ('b_init', build_mode)
      call pc_put_sensitive_field_flag ('a_inc' , build_mode)
      call pc_put_sensitive_field_flag ('b_inc' , build_mode)
      call pc_put_sensitive_field_flag ('a_last', build_mode)
      call pc_put_sensitive_field_flag ('b_last', build_mode)
      call pc_put_sensitive_field_flag ('a_tot' , build_mode)
      call pc_put_sensitive_field_flag ('b_tot' , build_mode)

      call pc_put_gui_only ('pathname_mess' , obj%mess)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!!??? debug support
!open(debug_lun, file='/usr/app/user/selzlrl/grab/grab.debug', &
!  status='unknown', position='append')
!write(debug_lun,*) 'GRAB_UPDATE: prepare for execution'
!close (debug_lun)


      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      call pc_print (obj%mess)

      call pc_alloc_process_cards (obj%cards,obj%ncard)

      mode_choice: IF (obj%opt_mode == 'BUILD_AVE') THEN
        allocate(obj%svalue(obj%a_tot,obj%b_tot), stat=ier1)
        allocate(obj%scount(obj%a_tot,obj%b_tot), stat=ier2)

        if(ier1 /= 0 .or. ier2 /= 0) then
          call pc_error('svalue/scount memory allocation failed')
        else
          CALL statutil_bld1 (obj%a_tot, obj%b_tot, obj%svalue, obj%scount)
        end if
      ELSE IF (obj%opt_mode == 'BUILD_SUM' .OR. & ! mode_choice
               obj%opt_mode == 'BUILD_OCC' .OR. &
               obj%opt_mode == 'BUILD_MAX' .OR. &
               obj%opt_mode == 'BUILD_MIN') THEN
        allocate(obj%svalue(obj%a_tot,obj%b_tot), stat=ier1)

        if(ier1 /= 0) then
          call pc_error('svalue memory allocation failed')
        else
          obj%svalue = 0.
        end if
      ELSE ! mode_choice
        CALL statio_read_file (obj%pathname, file_type, &
          obj%hdr_a, obj%hdr_b, obj%hdr_c, obj%hdr_d, &
          obj%a_init, obj%b_init, obj%a_inc, &
          obj%b_inc, obj%a_tot, obj%b_tot, obj%svalue, &
          ier1, msg, lunprint = obj%lun)
        IF (ier1 /= STATIO_OK) then
            call pc_error (msg)
        else if (obj%opt_mode == 'SET_INTERP') then
            call statutil_rep_nilx (obj%a_tot, obj%b_tot, obj%svalue)
        end if
      ENDIF mode_choice


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine grab_update

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine grab (obj,ntr,hd,tr)
      implicit none
      type(grab_struct)               :: obj                    ! arguments
      integer         ,intent(inout)  :: ntr                    ! arguments
      double precision,intent(inout)  :: hd(:,:)                ! arguments
      real            ,intent(inout)  :: tr(:,:)                ! arguments
      logical                         :: error                  ! local

      integer :: ntr_do  ! local

      IF (ntr <= 0) THEN
        ! WRAPUP PROCESSING.
        call grab_wrapup(obj,error)
        if (error) ntr = FATAL_ERROR
        return
      end if

      DO ntr_do = 1, ntr
        ! SKIP TRACES THAT ARE NOT FLAGGED.
        IF (obj%hdr_flag > 0 .and. &
          HD(obj%hdr_flag,ntr_do) == 0.) CYCLE

        ! BUILD STATIC FILE FROM HEADER WORD, OR VICE VERSA.
        IF (obj%opt_mode == 'BUILD_AVE') THEN
          CALL statutil_bld2 (HD(:obj%nwih,ntr_do), &
            real(HD(obj%hdr_grab,ntr_do)), &
            obj%svalue, obj%scount, &
            obj%hdr_a, obj%hdr_b, obj%hdr_c, obj%hdr_d, &
            obj%a_init, obj%b_init, obj%a_inc, obj%b_inc, &
            obj%a_tot, obj%b_tot)
        ELSE IF (obj%opt_mode == 'BUILD_SUM' .OR. &
                 obj%opt_mode == 'BUILD_OCC' .OR. &
                 obj%opt_mode == 'BUILD_MAX' .OR. &
                 obj%opt_mode == 'BUILD_MIN') THEN
          CALL grab_build_sum (HD(:obj%nwih,ntr_do), &
            real(HD(obj%hdr_grab,ntr_do)), obj%svalue, obj%opt_mode, &
            obj%hdr_a, obj%hdr_b, obj%hdr_c, obj%hdr_d, &
            obj%a_init, obj%b_init, obj%a_inc, obj%b_inc, &
            obj%a_tot, obj%b_tot)
        ELSE IF (obj%opt_mode == 'SET_NIL') THEN
          HD(obj%hdr_grab,ntr_do) = statutil_get3(HD(:obj%nwih,ntr_do), &
            obj%svalue, &
            obj%hdr_a,obj%hdr_b,obj%hdr_c,obj%hdr_d, &
            obj%a_init,obj%b_init,obj%a_inc,obj%b_inc,&
            obj%a_tot,obj%b_tot)
        ELSE IF (obj%opt_mode == 'SET_NEAR') THEN
          HD(obj%hdr_grab,ntr_do) = statutil_get1(HD(:obj%nwih,ntr_do), &
            obj%svalue, &
            obj%hdr_a,obj%hdr_b,obj%hdr_c,obj%hdr_d, &
            obj%a_init,obj%b_init,obj%a_inc,obj%b_inc,&
            obj%a_tot,obj%b_tot)
        ELSE   !  IF (obj%opt_mode == 'SET_INTERP') THEN
          HD(obj%hdr_grab,ntr_do) = statutil_get2(HD(:obj%nwih,ntr_do), &
            obj%svalue, &
            obj%hdr_a,obj%hdr_b,obj%hdr_c,obj%hdr_d, &
            obj%a_init,obj%b_init,obj%a_inc,obj%b_inc,&
            obj%a_tot,obj%b_tot)
        ENDIF
      END DO

      RETURN
      end subroutine grab


!!--------------------------- grab_build_sum -------------------------------!!
!!--------------------------- grab_build_sum -------------------------------!!
!!--------------------------- grab_build_sum -------------------------------!!

      SUBROUTINE grab_build_sum(HD, static, svalue, opt_mode, &
        hdr_a, hdr_b, hdr_c, hdr_d, a_init, b_init, &
        a_inc, b_inc, a_tot, b_tot)
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: hdr_a
      INTEGER , INTENT(IN) :: hdr_b
      INTEGER , INTENT(IN) :: hdr_c
      INTEGER , INTENT(IN) :: hdr_d
      INTEGER , INTENT(IN) :: a_tot
      INTEGER , INTENT(IN) :: b_tot
      REAL , INTENT(IN) :: static
      REAL , INTENT(IN) :: a_init
      REAL , INTENT(IN) :: b_init
      REAL , INTENT(IN) :: a_inc
      REAL , INTENT(IN) :: b_inc
      CHARACTER(len=*) , INTENT(IN) :: opt_mode
      double precision , INTENT(IN) :: HD(:)
      REAL , INTENT(INOUT) :: svalue(a_tot,b_tot)

      INTEGER :: a_idx, b_idx
!-----------------------------------------------
!     STATIC FILE UPDATE ROUTINE.
!     A REPLACEMENT FOR ROUTINE statutil_bld2 (FROM THE STATUTIL PRIMITIVE)
!        FOR USE WITH opt_mode=BUILD2, BUILD3, BUILD4, OR BUILD5.
!     STATIC GROUND POSITION IS DETERMINED FROM HEADER WORDS (hdr_a,hdr_b)
!        AND ALSO FROM HEADER WORDS (hdr_c,hdr_d).
!     TRACES OUTSIDE THE RANGE OF THE STATIC FILE ARE NOT USED.
!     hdr_a IS ALWAYS GREATER THAN ZERO.
!     hdr_b,hdr_c,hdr_d CAN BE ZERO.
!----------DIMENSION STATEMENTS.
!----------GET X INDEX.
      a_idx = mth_bin_number (a_init, a_inc, real(HD(hdr_a)))

      IF (a_idx>=1 .AND. a_idx<=a_tot) THEN
        ! GET Y INDEX.
        b_idx = 1

        IF (hdr_b > 0) THEN
          b_idx = mth_bin_number (b_init, b_inc, real(HD(hdr_b)))

          IF (b_idx<1 .OR. b_idx>b_tot) GO TO 600
        ENDIF

        ! UPDATE THE STATIC ARRAY.
        SELECT CASE (opt_mode)                       !sum
        CASE ('BUILD_SUM')
          svalue(a_idx,b_idx) = svalue(a_idx,b_idx) + static
        CASE ('BUILD_OCC')                          !flag
          svalue(a_idx,b_idx) = 1.
        CASE ('BUILD_MAX')                          !max
          IF (static /= 0.0) THEN
            IF (svalue(a_idx,b_idx) /= 0.0) THEN
              svalue(a_idx,b_idx) = MAX(svalue(a_idx,b_idx),static)
            ELSE
              svalue(a_idx,b_idx) = static
            ENDIF
          ENDIF
        CASE ('BUILD_MIN')                          !min
          IF (static /= 0.0) THEN
            IF (svalue(a_idx,b_idx) /= 0.0) THEN
              svalue(a_idx,b_idx) = MIN(svalue(a_idx,b_idx),static)
            ELSE
              svalue(a_idx,b_idx) = static
            ENDIF
          ENDIF
        END SELECT
      ENDIF

  600 CONTINUE

      IF (hdr_c == 0) RETURN

      ! GET X2 INDEX.
      a_idx = mth_bin_number (a_init, a_inc, real(HD(hdr_c)))

      IF (a_idx<1 .OR. a_idx>a_tot) RETURN

      ! GET Y2 INDEX.
      b_idx = 1

      IF (hdr_d > 0) THEN
        b_idx = mth_bin_number (b_init, b_inc, real(HD(hdr_d)))

        IF (b_idx<1 .OR. b_idx>b_tot) RETURN
      ENDIF

      ! UPDATE THE STATIC ARRAY.
      mode_choice: SELECT CASE (opt_mode)                         !sum
      CASE ('BUILD_SUM')
        svalue(a_idx,b_idx) = svalue(a_idx,b_idx) + static
      CASE ('BUILD_OCC')                            !flag
        svalue(a_idx,b_idx) = 1.
      CASE ('BUILD_MAX')                            !max
        IF (static /= 0.0) THEN
          IF (svalue(a_idx,b_idx) /= 0.0) THEN
            svalue(a_idx,b_idx) = MAX(svalue(a_idx,b_idx),static)
          ELSE
            svalue(a_idx,b_idx) = static
          ENDIF
        ENDIF
      CASE ('BUILD_MIN')                            !min
        IF (static /= 0.0) THEN
          IF (svalue(a_idx,b_idx) /= 0.0) THEN
            svalue(a_idx,b_idx) = MIN(svalue(a_idx,b_idx),static)
          ELSE
            svalue(a_idx,b_idx) = static
          ENDIF
        ENDIF
      END SELECT mode_choice

      RETURN

      END SUBROUTINE grab_build_sum


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine grab_wrapup (obj,error)
      implicit none
      type(grab_struct)            :: obj       ! arguments
      logical,optional,intent(out) :: error     ! arguments
      integer                      :: ier       ! local
      character(len=80)            :: msg       ! local

!! 1999-08-25 Randall L. Selzler
!! "kards" KLUDGE for following internal compiler error in absoft
!! tmod-2612 tmod: INTERNAL
!!   Internal: IR xlate; SOURCE line: 1088, file: ../grab.f90;
!!      INTERNAL site: 16768, tag: 0, val: -1
!! error on line 11043, source file line 1089, invalid identifier after handle
!! 2000-01-21 RLS... confirmed that the problem still exists.

      character(len=80), pointer, dimension(:):: kards

      if (present(error)) error = .false.

      if(obj%skip_wrapup) return
      obj%skip_wrapup = .true.

kards => obj%cards

      IF (obj%opt_mode == 'BUILD_AVE') THEN
        CALL statutil_bld3 (obj%a_tot, obj%b_tot, obj%svalue, obj%scount)

        CALL statio_write_file (obj%pathname, 'GRAB', &
          obj%hdr_a, obj%hdr_b, obj%hdr_c, obj%hdr_d, &
          obj%a_init, obj%b_init, obj%a_inc, obj%b_inc, &
          obj%a_tot, obj%b_tot, obj%svalue, &
          ier, msg, kards, obj%ncard, 'GRAB', obj%lun)
        IF (ier /= STATIO_OK) then
            call pc_error (msg)
            if (present(error)) error = .true.
        ENDIF

      ELSE IF (obj%opt_mode == 'BUILD_SUM' .OR. &
               obj%opt_mode == 'BUILD_OCC' .OR. &
               obj%opt_mode == 'BUILD_MAX' .OR. &
               obj%opt_mode == 'BUILD_MIN') THEN
        CALL statio_write_file (obj%pathname, 'GRAB', &
          obj%hdr_a, obj%hdr_b, obj%hdr_c, obj%hdr_d, &
          obj%a_init, obj%b_init, obj%a_inc, obj%b_inc, &
          obj%a_tot, obj%b_tot, obj%svalue, &
          ier, msg, kards, obj%ncard, 'GRAB', obj%lun)
        IF (ier /= STATIO_OK) then
            call pc_error (msg)
            if (present(error)) error = .true.
        ENDIF
      ENDIF

      write(obj%lun, *) 'GRAB:  WE ARE FINISHED'

      return
      end subroutine grab_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module grab_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
