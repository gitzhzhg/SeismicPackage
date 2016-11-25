!<CPS_v1 type="PROCESS"/>
!!------------------------------- spti.f90 ---------------------------------!!
!!------------------------------- spti.f90 ---------------------------------!!
!!------------------------------- spti.f90 ---------------------------------!!

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
! Name       : SPTI    (Spline Trace Interpolator)
! Category   : transforms
! Written    : 1994-02-15   by: Douglas Hanson
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : 2D horizontal trace interpolation using a sinc function.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! SPTI is a 2D trace interpolator that uses a horizontal sinc operator to form
! the new (output) traces.  Only the interpolated traces are output; input
! traces are NOT passed through.
!
! SPTI uses a very simple algorithm that calculates a sample value for the
! interpolated trace from samples at the same time on nearby traces.  These
! nearby samples are weighted by a sinc function and summed to form the new
! sample on the interpolated trace.  This sinc function operator extends LEN_OP
! input traces on each side of the interpolated trace (the operator length is
! reduced on one side for interpolated traces near the ends of the input 2D
! line).
!
! Each 2D input line, as defined by HDR_SLOW, is interpolated separately.
!
!
! Advantages of SPTI
!
! SPTI is fast and allows the user to set up an arbitrary 2D array of output
! interpolated traces (not necessarily restricted to interpolated traces half
! way between two adjacent input traces).
!
!
! Disadvantage of SPTI
!
! The algorithm used by SPTI is based on the assumption that all events have
! zero dip.
!
!
! Output Trace Array
!
! Location of traces in the interpolated trace array is defined by the
! parameters FAST_VAL_OUT and FAST_INC_OUT.  Interpolated traces occupy all
! locations in the interpolated trace array that do not extend beyond the first
! and last trace in the input line.  SPTI will not create interpolated traces
! beyond the range of input trace locations.
!
! For example, if the input data falls on the HDR_FAST values 0, 1, 2, ..., but
! you set FAST_VAL_OUT = 0.2 and FAST_INC_OUT=.5, then the output traces will
! fall on .2, .7, 1.2, ....
!
! Muting Option
!
! If OPT_MUTE = YES, mute aggressively.  The top mute is the maximum value
! from the two neighboring traces and the bottom mute is the minimum.
!
! If OPT_MUTE = NO, mute conservatively.  The top mute is the minimum value
! from the two neighboring traces and the bottom mute is the maximum.
!
! The output trace mute header words are always reset.
! Samples outside the mute range are zeroed.
!
! Header Words for Interpolated Traces
!
! Header words 1, 3, 4 and 25 are set as follows:
!
!      hdr 1     sequential output trace number
!
!      hdr 2     top mute
!
!      hdr 3     sequential output line number (starting with 1)
!
!      hdr 4     sequential output trace number within the output line
!
!      hdr 25    LAV
!
!      hdr 48    Set to -98 for interpolated traces.
!
!      hdr 64    bottom mute
!
! All other output trace header words are the average of the header words of
! the nearest neighbor input trace on both sides of the interpolated trace.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
! SPTI should be used only when it is necessary to interpolate traces that are
! not located half way between two adjacent input traces.  If you need to
! create interpolated traces that do lie half way between two adjacent input
! traces, then FXTI should be used.
!
! Be sure to set FAST_INC_OUT negative if the input data is sorted so that
! HDR_FAST values decrease.
!
!
! Example
!
! Consider an example of interpolating a 3D data set in the direction
! associated with increasing header word 7.  For this example we have 21 lines
! with 101 traces each, sorted into inline order.  Header word 7 values within
! each line range from 0 to 100 incrementing by 1.  The header word 8 values of
! the lines range from 0 to 21 incrementing by 1.  We want to interpolate the
! data from a header word 7 spacing of 1 to .3 and we want the output traces to
! fall on locations .2 , .5 , .8 , ....  We verify that the input data are
! sorted in order of header word 8 than header word 7 and then run SPTI with the
! following parameter values:
!
! HDR_FAST = 7, HDR_SLOW = 8, LEN_OP = 10, FAST_INC_IN = 1., FAST_INC_OUT = .3,
! FAST_VAL_OUT = 1.1, SLOW_VAL_IN = 0., SLOW_INC_IN = 1.
!
! This should give us 21 lines of output traces with the following header word
! 7 pattern:  0.2, 0.5, 0.8, 1.1, 1.4, 1.7, 2.0, 2.3, 2.6, 2.9, 3.2,
! 3.5, ..., 98.9, 99.2, 99.5, 99.8.
!
! If any input line were shorter, the actual output traces for that line would
! be restricted to the range of that particular line.
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
!
! This process requires traces to be input one at a time.
!
! Process requires traces to be input in sort order as defined by HDR_FAST and
! HDR_SLOW.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
!
! This process outputs one trace at a time.
!
! Only the new interpolated traces are output.  Input traces are NOT passed
! through.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       used but not changed
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 1       Sequential Trace Count     Renumbered.
! 2       Head mute index            reset
! 25      LAV                        reset
! 48      created trace indicator    set to -98
! 64      Tail mute index            reset
! HDR_FAST User-defined header word  Used to interpolation within a line.
! HDR_SLOW User-defined header word  Used to distinguishing between lines.
! ALL     all output trace headers   Changed, linear interpolation from nearest
!                                    input traces
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                               REVISION HISTORY
!     Date       Author      Description
!     ----       ------      -----------
!022. 2006-06-20  B. Menger   Removed Unused Variables.
! 21. 2004-05-24 Menger      Fixed to only change traces that are created.
! 20. 2004-04-20 Menger      Add hdr#48 set to -98
! 19. 2002-06-03 Goodger     Change default for LEN_OP to 2.
! 18. 2002-02-05 Selzler     Removed extraneous "~" from GUI layout.
! 17. 2001-11-15 Selzler     Change "(*)" arrays to "(:)" for Intel compiler
! 16. 2001-10-10 Selzler     Added OPT_MUTE parameter and functionality.
! 15. 2001-09-26 Selzler     Eliminate FSE from GUI and force value to 1.0
! 14. 2001-07-11 Selzler     Correct latent bug (floating point round off
!                            error when headers are constant).
! 13. 2001-05-10 Selzler     Incorporated CIB's newdoc changes.
! 12. 2001-05-08 Selzler     Changed write(6 to write(print_lun
!                            and set LAV
! 11. 2001-05-04 Selzler     Conversion to new CPS
! 10. 1999-05-11 Hanson      Modify special case of FSE = 0.
!  9. 1998-11-11 Goodger     Begin using fortran90 compiler.
!  8. 1998-09-24 Hanson      Use getn_parm to get the number of parameters.
!  7. 1998-09-02 Hanson      Add FSE weighting coefficient.
!  6. 1998-08-27 Hanson      Restrict mutes to the trace range.
!  5. 1998-08-26 Hanson      Add parameter PRINT for print level.
!  4. 1998-08-25 Hanson      Fix header word comp for mutes and hdr_fast.
!  3. 1998-08-04 Vunderink   Fix bug in calculation of ix_out_2 and out_trc_max
!  2. 1994-06-02 Hanson      Fix ix_ter_2 > ix_mem_2 bug in spti_inp
!  1. 1994-02-15 Hanson      Original version
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
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NEED_REQUEST   true     whether this process ever needs to request traces.
! NEED_LABEL     true     whether this process needs a label.
! TWOSETS        true     whether this process needs two trace/header arrays.
!
! Upon input, NTR must have one of these values:
!  NTR == 1              means to process the input trace.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR == 1              if this process is outputting a trace.
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
!
!<gui_def>
!<NS spti Process/NC=80>
!
! HDR_FAST~~~~~~=`III             HDR_SLOW~~~~~=`III
!
! FAST_INC_IN~~~=`FFFFFFFFFFF     SLOW_INC_IN~~=`FFFFFFFFFFF
!
! FAST_VAL_OUT~~=`FFFFFFFFFFF     SLOW_VAL_IN~~=`FFFFFFFFFFF
!
! FAST_INC_OUT~~=`FFFFFFFFFFF
!
!
! PRINT_LEVEL~~~=`CCCCC         LEN_OP=`III       OPT_MUTE=`CCC
!
!
!
!</gui_def>
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!
!<HelpSection>
!
!
!<Help KEYWORD="LEN_OP">
!<Tip> Half length of spline interpolation operator in trace units. </Tip>
! Default = 2
! Allowed = 1 =< int =< 25
! Number of input traces to use on each side of the output trace location for
! the interpolation calculation.
!</Help>
!
!<Help KEYWORD="HDR_FAST">
!<Tip> Header word for the more rapidly changing input trace coordinate. </Tip>
! Default = 7
! Allowed = 1 - NWIH
! Input data should be sorted so that HDR_FAST is the header word for the more
! rapidly changing input trace coordinate, that is, the value of HDR_FAST
! defines the position of input traces within a line.
!
! Output traces are interpolated from the input in the direction defined by
! increasing (or decreasing) values of HDR_FAST.
!</Help>
!
!<Help KEYWORD="HDR_SLOW">
!<Tip> Header word for the more slowly changing input trace coordinate. </Tip>
! Default = 8
! Allowed = 1 - NWIH
! Input data should be sorted so that HDR_SLOW is the header word for the more
! slowly changing input trace coordinate, that is, the value of HDR_SLOW
! defines input trace lines.
!
! Output traces are interpolated independently for each input line.
!</Help>
!
!<Help KEYWORD="SLOW_VAL_IN">
!<Tip> HDR_SLOW value for any input line. </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="SLOW_INC_IN">
!<Tip> Increment of HDR_SLOW value between input lines. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="FAST_INC_IN">
!<Tip> Increment of HDR_FAST value between input traces. </Tip>
! Default = 1.0
! Allowed = real, non-zero required
! Increment of HDR_FAST value between input traces (in the direction of
! interpolation).
!
! Be sure to set the polarity of FAST_INC_IN correctly!  If the value of
! HDR_FAST increases as traces are input, then set FAST_INC_IN > 0.0; if the
! value of HDR_FAST decreases as traces are input, then set FAST_INC_IN < 0.0.
!</Help>
!
!<Help KEYWORD="FAST_VAL_OUT">
!<Tip> HDR_FAST value for any output trace. </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="FAST_INC_OUT">
!<Tip> Increment of HDR_FAST value between output traces. </Tip>
! Default = 1.0
! Allowed = real, non-zero required
! Increment of HDR_FAST value between output traces (in the direction of
! interpolation).
!</Help>
!
!<Help KEYWORD="PRINT_LEVEL">
!<Tip> Verbosity of printout. </Tip>
! Default = NO
! Allowed = NO
! Allowed = BRIEF
! Allowed = YES
!
! If PRINT_LEVEL = NO, then there is no diagnostic printout.
!
! If PRINT_LEVEL = BRIEF, then about 10 lines of information for first the 10
! lines will be printed followed by about 1 line for each subsequent line.
!
! If PRINT_LEVEL = YES, then about 10 lines of information for each line will
! be printed.
!</Help>
!
!<Help KEYWORD="OPT_MUTE">
!<Tip> Whether to restore the interpolated mute or compute a new mute. </Tip>
! Default = YES
! Allowed = YES/NO   
!
! If OPT_MUTE = YES, mute aggressively.  The top mute is the maximum value
! from the two neighboring traces and the bottom mute is the minimum.
!
! If OPT_MUTE = NO, mute conservatively.  The top mute is the minimum value
! from the two neighboring traces and the bottom mute is the maximum.
!
! The output trace mute header words are always reset.
! Samples outside the mute range are zeroed.
!</Help>
!
!</HelpSection>
!
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module spti_module
      use pc_module
      use lav_module
      use named_constants_module
      use mem_module
      implicit none
      private
      public :: spti_create
      public :: spti_initialize
      public :: spti_update
      public :: spti_delete
!<execute_only>
      public :: spti            ! main execution (trace processing) routine.
      public :: spti_wrapup
!</execute_only>

      character(len=100),public,save :: spti_IDENT = &
'$Id: spti.f90,v 1.22 2006/06/20 13:12:08 Menger prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: spti_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.
        integer                    :: hdr_fast         ! header word containing
                                                       ! input x coordinate.
        integer                    :: hdr_slow         ! header word containing
                                                       ! input y coordinate.
        integer                    :: len_op           ! half length of spline
                                                       ! interpolation operator
                                                       ! scaled by fast_inc_in
        real                       :: fast_inc_in      ! input  trace x spacing
        double precision           :: fast_inc_out     ! output trace x spacing
        real                       :: fast_val_out     ! output trace x coord
        real                       :: slow_val_in      ! input  trace y coord
        real                       :: slow_inc_in      ! input  trace y spacing

        character(len=5)           :: print_level      ! print verbosity.
                                                       ! YES, print about 10
                                                       !   lines per
                                                       !   interpolation group.
                                                       ! BRIEF, print only the
                                                       !   first 10 groups.
                                                       ! NO, inhibit printing.
        logical                     :: opt_mute        ! Mute aggressively.
! GLOBAL PARAMETERS
        integer                     :: ndpt            ! Num Data Points
        integer                     :: nwih            ! Num Word In Header
! DEPENDENT PARAMETERS
        integer                     :: in_trc_cnt      ! Input trace count
        integer                     :: in_trc_total    ! Input trace cnt total
        integer                     :: out_trc_cnt     ! Output trace count
        integer                     :: out_trc_total   ! Output trace cnt total
        integer                     :: n_group         !???
        integer                     :: nx_mem          ! Num traces in memory
        integer                     :: ipass           !???
        integer                     :: n_inp           !???
        integer                     :: jt_out          !???
        integer                     :: ix_mem_1        !???
        integer                     :: ix_mem_2        !???
        integer                     :: ix_out_1        !???
        integer                     :: ix_out_2        !???
        integer                     :: x_dir           ! hdr_fast direction.
                                                       ! +1 iff increasing.
                                                       ! -1 iff decreasing.
        real                        :: x_interpolate   ! fast_inc_in * len_op
        double precision            :: x_out_0         !???
        double precision            :: x_inp_1         !???
        double precision            :: y_inp_1         !???
        double precision            :: x_inp_2         !???
        double precision            :: y_inp_2         !???

        double precision,dimension(:,:,:),pointer :: h_out  ! header array
                                                       ! D1= nwih
                                                       ! D2= nx_mem
                                                       ! D3= 2
        real,dimension(:,:),pointer :: d_out           ! Sample array
                                                       ! D1= ndpt
                                                       ! D2= nx_mem
        real,dimension(:,:),pointer :: w_out           ! Sample array
                                                       ! D1= ndpt
                                                       ! D2= nx_mem
      end type spti_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(spti_struct),pointer,save :: object      ! needed for traps.

      character(len=5),dimension(3),parameter :: print_level_options = &
        (/'YES  ','BRIEF','NO   '/)

      integer :: print_lun       ! Fortran print LUN

      integer :: hdr48_set_value = -98     ! set hdr 48 to this value
      integer :: spti_revision_number = 21 ! set to rev number above

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine spti_create (obj)
      implicit none
      type(spti_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify(obj%h_out)
      nullify(obj%d_out)
      nullify(obj%w_out)

      call spti_initialize (obj)

      return
      end subroutine spti_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine spti_delete (obj)
      implicit none
      type(spti_struct),pointer :: obj       ! arguments

!<execute_only>
      call spti_wrapup (obj)
!</execute_only>

      call mem_free(obj%h_out)
      call mem_free(obj%d_out)
      call mem_free(obj%w_out)

      deallocate(obj)
      return
      end subroutine spti_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine spti_initialize (obj)
      implicit none
      type(spti_struct),intent(inout) :: obj       ! arguments

      obj%hdr_fast = 7
      obj%hdr_slow = 8
      obj%len_op = 2
      obj%fast_inc_in = 1.0
      obj%fast_val_out = 0.0
      obj%fast_inc_out = 1.0
      obj%fast_val_out = 0.0
      obj%slow_val_in = 0.0
      obj%slow_inc_in = 1.0
      obj%slow_inc_in = 1.0
      obj%print_level = 'NO'
      obj%opt_mute = .true.

      obj%ndpt = 0
      obj%nwih = 64

      obj%in_trc_cnt = 0
      obj%in_trc_total = 0
      obj%out_trc_cnt = 0
      obj%out_trc_total = 0
      obj%n_group = 0
      obj%nx_mem = 0
      obj%ipass = 1
      obj%n_inp = 0
      obj%jt_out = 0
      obj%ix_mem_1 = 0
      obj%ix_mem_2 = 0
      obj%ix_out_1 = 0
      obj%ix_out_2 = 0
      obj%x_dir = 0
      obj%x_interpolate = 0.0
      obj%x_out_0 = 0.0
      obj%x_inp_1 = 0.0
      obj%y_inp_1 = 0.0
      obj%x_inp_2 = 0.0
      obj%y_inp_2 = 0.0

      print_lun = -1

      call spti_update (obj)
      return
      end subroutine spti_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine spti_update (obj)
      implicit none
      type(spti_struct),intent(inout),target :: obj             ! arguments

      integer :: state
      logical :: verify
      integer :: numtr

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

      state = pc_get_update_state()

      if(state == PC_FRONTEND .or. state == PC_BACKEND) then
        verify = .true.
      else
        verify = .false.
      end if

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_get_global('ndpt', obj%ndpt)
      call pc_get_global('nwih', obj%nwih)

      call pc_get_global ('numtr'   , numtr)     ! maximum number of traces.

      call pc_get ('hdr_fast', obj%hdr_fast)
      call pc_get ('hdr_slow', obj%hdr_slow)
      call pc_get ('len_op', obj%len_op)
      call pc_get ('fast_inc_in', obj%fast_inc_in)
      call pc_get ('fast_inc_out', obj%fast_inc_out)
      call pc_get ('fast_val_out', obj%fast_val_out)
      call pc_get ('slow_val_in', obj%slow_val_in)
      call pc_get ('slow_inc_in', obj%slow_inc_in)

      call pc_get ('print_level', obj%print_level)
      call string_to_upper (obj%print_level)
      call pc_get ('opt_mute', obj%opt_mute)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if(numtr > 1) then
        call pc_error('UNGATHER needed to limit input NUMTR to 1')
      end if

      if(obj%hdr_fast < 1 .or. obj%hdr_fast > obj%nwih) then
        call pc_error('invalid HDR_FAST, < 1 or > NWIH')
         obj%hdr_fast = 7
      end if

      if(obj%hdr_slow < 1 .or. obj%hdr_slow > obj%nwih) then
        call pc_error('invalid HDR_SLOW, < 1 or > NWIH')
         obj%hdr_slow = 8
      end if

      if(obj%len_op < 1 .or. obj%len_op > 25) then
        call pc_error('invalid LEN_OP, 1 <= int <= 25, required')
        obj%len_op = 10
      end if

      if(obj%fast_inc_in == 0.0 .or. obj%fast_inc_in == FNIL) then
        call pc_error('invalid FAST_INC_IN, non-zero required')
        obj%fast_inc_in = 1.0
      end if

      if(obj%fast_inc_out == 0.0 .or. obj%fast_inc_out == DNIL) then
        call pc_error('invalid FAST_INC_OUT, non-zero required')
        obj%fast_inc_out = 1.0
      end if

      obj%fast_inc_out = sign(obj%fast_inc_out, dble(obj%fast_inc_in))

      if(obj%fast_val_out == FNIL) then
        call pc_error('invalid FAST_VAL_OUT, value required')
        obj%fast_val_out = 0.0
      end if

      if(obj%slow_val_in == FNIL) then
        call pc_error('invalid SLOW_VAL_IN, value required')
        obj%slow_val_in = 0.0
      end if

      if(obj%slow_inc_in <= 0.0 .or. obj%slow_inc_in == FNIL) then
        call pc_error('invalid SLOW_INC_IN, real > 0.0, required')
        obj%slow_inc_in = 1.0
      end if

      if(all(print_level_options /= obj%print_level)) then
        call pc_error( &
          'Invalid PRINT_LEVEL value. Valid values are YES, BRIEF, NO')
        obj%print_level = 'NO'
      end if

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put ('hdr_fast', obj%hdr_fast)
      call pc_put ('hdr_slow', obj%hdr_slow)

      call pc_put ('len_op', obj%len_op)
      call pc_put ('fast_inc_in', obj%fast_inc_in)
      call pc_put ('fast_inc_out', obj%fast_inc_out)
      call pc_put ('fast_val_out', obj%fast_val_out)
      call pc_put ('slow_val_in', obj%slow_val_in)
      call pc_put ('slow_inc_in', obj%slow_inc_in)

      call pc_put_options_field('print_level', print_level_options, 3)
      call pc_put ('print_level', obj%print_level)
      call pc_put ('opt_mute', obj%opt_mute)

      call pc_put_control ('need_request', .true.)
      call pc_put_control ('need_label'  , .true.)
      call pc_put_control ('twosets'     , .true.)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      obj%x_interpolate = obj%len_op * abs(obj%fast_inc_in)

      obj%x_dir = sign(1.0,obj%fast_inc_in)

      obj%nx_mem = int(2*obj%x_interpolate/abs(obj%fast_inc_out))

      if (obj%nx_mem*abs(obj%fast_inc_out) .ne. 2*obj%x_interpolate) then
        obj%nx_mem = obj%nx_mem + 2     ! no. out traces in mem
      else
        obj%nx_mem = obj%nx_mem + 1     ! no. out traces in mem
      end if

      write(print_lun, '(/,'' SPTI setup'' &
       &,/,'' SPTI Revision: '',I3,'' (HDR#48 set to '',I7,'')''     &
       &,/,'' header word defining interpolation bins   ='',I2 &
       &,/,'' header word defining interpolation groups ='',I2 &
       &,/,'' input trace direction fast direction      ='',I2 &
       &,/,'' spline interpolator half length           ='',I4 &
       &,/,'' interpolation distance obj%x_interpolate  ='',F12.2 &
       &,/,'' input trace spacing fast_inc_in           ='',F12.2 &
       &,/,'' ouput trace spacing fast_inc_out          ='',F12.2 &
       &,/,'' x bin center fast_val_out                 ='',F12.2 &
       &,/,'' y bin center slow_val_in                  ='',F12.2 &
       &,/,'' input trace group spacing slow_inc_in     ='',F12.2 &
       &,/,'' number of input traces kept in memory obj%nx_mem='',I8)') &
        spti_revision_number,hdr48_set_value, &
        obj%hdr_fast, obj%hdr_slow, obj%x_dir, &
        obj%len_op, obj%x_interpolate, obj%fast_inc_in, obj%fast_inc_out, &
        obj%fast_val_out, obj%slow_val_in, obj%slow_inc_in, obj%nx_mem

      call mem_alloc (obj%h_out,  obj%nwih, 2, obj%nx_mem)
      call mem_alloc (obj%d_out,  obj%ndpt, obj%nx_mem)
      call mem_alloc (obj%w_out,  obj%ndpt, obj%nx_mem)

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

      print_lun = pc_get_lun()

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine spti_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine spti (obj,ntr,hdi,tri,hdo,tro)
      implicit none
      type(spti_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      !--- Add intent inout to  handle resetting the input headers
      double precision ,intent(inout) :: hdi(:,:)               ! arguments
      real             ,intent(in)    :: tri(:,:)               ! arguments
      double precision ,intent(inout) :: hdo(:,:)               ! arguments
      real             ,intent(inout) :: tro(:,:)               ! arguments

      integer :: ierr

      ierr = 0

      !--- we need to be able to detect when a trace was added, so here we
      !--- will reset header # 48 to header #1's value, which is defined to
      !--- always be unique.  We add a test in spti_out that will detect
      !--- if the value in hdr # 48 is equal to either of the values being
      !--- interpolated from, and if so, this trace has not been inserted
      !--- and will not have hdr 48 changed from the old hdr 1 value.  All
      !--- interpolated traces will have hdr 48 modified to -98 nominally,
      !--- a value defined in hdr48_set_value in the code above.

      !--- BUT, we don't want to remove a value in hdi(48) that is non-zero
      !--- so we will save if non-zero

      where (hdi(48,:) == 0d0 )
        hdi(48,:) = hdi(1,:)
      end where

      !--- done resetting header, now spti can continue...

      !  look at the current status and decide what to do
      call spti_status (obj%ipass, ntr, obj%n_inp, &
        obj%out_trc_cnt, obj%out_trc_total)


      !  if input mode put this trace in buffer
      !  note this may mean we need to ouput data also
      if (obj%ipass == 1) then
        call spti_inp (obj%print_level, ierr, ntr, obj%nwih, obj%ndpt, &
          hdi, tri, obj%h_out, obj%d_out, obj%w_out, &
          obj%hdr_fast, obj%hdr_slow, obj%x_dir, obj%x_interpolate, &
          obj%fast_inc_out, obj%fast_inc_in, obj%fast_val_out, &
          obj%slow_val_in, obj%slow_inc_in, &
          obj%nx_mem, obj%n_inp, obj%ipass, obj%in_trc_cnt, &
          obj%in_trc_total, obj%jt_out, obj%out_trc_cnt, obj%out_trc_total, &
          obj%n_group, obj%ix_mem_1, obj%ix_mem_2, obj%ix_out_1, obj%ix_out_2, &
          obj%x_out_0, obj%x_inp_1, obj%y_inp_1, obj%x_inp_2, obj%y_inp_2)
      end if

      if (ierr /= 0) then
        ntr = FATAL_ERROR
        return
      end if

      !  pass out a trace
      if (obj%ipass == 2) then
        call spti_out (ntr, obj%n_group, obj%ix_out_2, obj%jt_out, &
          obj%out_trc_cnt, obj%out_trc_total, obj%hdr_fast, obj%hdr_slow, &
          obj%nx_mem, obj%x_out_0, obj%fast_val_out, &
          obj%fast_inc_out, &
          obj%nwih, obj%ndpt, obj%h_out, obj%d_out, obj%w_out, hdo, tro, &
          obj%opt_mute)
      end if


      !  all done with this pass
      if (obj%ipass == 1) then                       ! data input
        ntr = NEED_TRACES
        return
      end if

      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
           call spti_wrapup (obj)
      end if

      return

      end subroutine spti

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine spti_wrapup (obj)
      implicit none
      type(spti_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return

      obj%skip_wrapup = .true.

      return
      end subroutine spti_wrapup

!</execute_only>

!!---------------------------- spti_status ---------------------------------!!
!!---------------------------- spti_status ---------------------------------!!
!!---------------------------- spti_status ---------------------------------!!

!<execute_only>

      subroutine spti_status(ipass, ntr, n_inp, it_out, nt_out)
!  determine the status of what should be done
      implicit none

      integer , intent(inout) :: ipass
      integer , intent(out) :: ntr
      integer , intent(in) :: n_inp
      integer , intent(in) :: it_out
      integer , intent(in) :: nt_out

      if (ipass == 2) then
        if (it_out < nt_out) then
          ntr = 1
        else
          if (n_inp == 0) then
            ntr = 0
            ipass = 3
          else
            ntr = n_inp
            ipass = 1
          endif
        endif
      endif

      return
      end subroutine spti_status

!</execute_only>

!!------------------------------ spti_inp ----------------------------------!!
!!------------------------------ spti_inp ----------------------------------!!
!!------------------------------ spti_inp ----------------------------------!!

!<execute_only>

      subroutine spti_inp(print_level, ierr, ntr, nwih, ndpt, hdi, tri, &
        h_out, d_out, w_out, hdr_fast, hdr_slow, x_dir, x_interpolate, &
        fast_inc_out, fast_inc_in, fast_val_out, slow_val_in, &
        slow_inc_in, nx_mem, n_inp, ipass, &
        it_inp, nt_inp, jt_out, it_out, nt_out, n_group, ix_mem_1, ix_mem_2, &
        ix_out_1, ix_out_2, x_out_0, x_inp_1, y_inp_1, x_inp_2, y_inp_2)
!  input the next data trace

      implicit none

      integer , intent(out) :: ierr
      integer , intent(in) :: ntr
      integer  :: nwih
      integer  :: ndpt
      integer , intent(in) :: hdr_fast
      integer , intent(in) :: hdr_slow
      integer  :: x_dir
      integer , intent(in) :: nx_mem
      integer , intent(out) :: n_inp
      integer , intent(out) :: ipass
      integer , intent(inout) :: it_inp
      integer , intent(inout) :: nt_inp
      integer , intent(out) :: jt_out
      integer  :: it_out
      integer , intent(inout) :: nt_out
      integer , intent(inout) :: n_group
      integer  :: ix_mem_1
      integer  :: ix_mem_2
      integer , intent(out) :: ix_out_1
      integer , intent(out) :: ix_out_2
      real  :: x_interpolate
      double precision  :: fast_inc_out
      real  :: fast_inc_in
      real , intent(in) :: fast_val_out
      real , intent(in) :: slow_val_in
      real , intent(in) :: slow_inc_in
      double precision  :: x_out_0
      double precision  :: x_inp_1
      double precision , intent(inout) :: y_inp_1
      double precision , intent(inout) :: x_inp_2
      double precision , intent(inout) :: y_inp_2
      character(len=*) , intent(in) :: print_level
      double precision  :: hdi(nwih)
      real  :: tri(ndpt)
      double precision  :: h_out(nwih,2,nx_mem)
      real  :: d_out(ndpt,nx_mem)
      real  :: w_out(ndpt,nx_mem)

      integer :: ix_out_0, i, ix_mem, ix_ter_1, ix_ter_2
      integer :: id_out, imute_1, imute_2
      double precision :: x_out_2, x_inp_0, y_inp_0, x_mem, dx_0, dx_1, dx_2

      ierr = 0

      n_inp = ntr

!  if done entering data write out current buffer and proceed to migration
      if (ntr == 0) then

        ipass = 2
        ix_out_1 = ix_mem_1
        ix_out_2 = 1 + int((x_inp_2 - x_out_0)/fast_inc_out)
        x_out_2 = (ix_out_2 - 1)*fast_inc_out + x_out_0

        if (x_dir>0 .and. x_out_2>x_inp_2 .or. &
          x_dir<0 .and. x_out_2<x_inp_2) then
          ix_out_2 = ix_out_2 - 1
        end if

        x_out_2 = (ix_out_2 - 1)*fast_inc_out + x_out_0

        nt_out = nt_out + max(0,ix_out_2 - ix_out_1 + 1)
        ipass = 2

        write(print_lun, '( &
         & /,'' SPTI has detected the end of data input'' &
         &,/,'' total number of groups               ='',I8 &
         &,/,'' total number of input  traces        ='',I8 &
         &,/,'' total number of output traces        ='',I8 &
         &,/,'' number of input  traces in this group='',I8 &
         &,/,'' number of output traces in this group='',I8 &
         &,/,'' flushing output traces ix_out        ='',I4,1X,I4 &
         &,/,'' first output x='',F12.2 &
         &  ,'' first  input x='',F12.2,'' y='',F12.2 &
         &,/,''  last output x='',F12.2 &
         &  ,''  last  input x='',F12.2,'' y='',F12.2)') &
          n_group, it_inp, nt_out, it_inp - nt_inp, ix_out_2, &
          ix_out_1, ix_out_2, x_out_0, x_inp_1, y_inp_1, &
          x_out_2, x_inp_2, y_inp_2

      else                              ! if (ntr .eq. 0) then

!  SPTI can only handle one input trace at a time
        if (n_inp > 1) go to 991

!  if this is the first input trace of a group set some inital counters
        if (it_inp == nt_inp) then
          jt_out = 0
          n_group = n_group + 1
          x_inp_1 = hdi(hdr_fast)       ! first input x location this group
          y_inp_1 = hdi(hdr_slow)       ! first input y location this group
          x_inp_2 = x_inp_1             ! last input x location this group
          y_inp_2 = y_inp_1             ! last input x location this group

!  the first output bin location is defined by x_inp_1
          ix_out_0 = int((x_inp_1 - fast_val_out)/fast_inc_out) + 1
          x_out_0 = (ix_out_0 - 1)*fast_inc_out + fast_val_out

          if (x_dir>0 .and. x_out_0<x_inp_1) ix_out_0 = ix_out_0 + 1

          if (x_dir<0 .and. x_out_0>x_inp_1) ix_out_0 = ix_out_0 - 1

          x_out_0 = (ix_out_0 - 1)*fast_inc_out + fast_val_out

!  initialize the locations in memory for output traces
          call spti_x_limits (x_dir, x_interpolate, fast_inc_out, x_out_0, &
            x_inp_1, ix_mem_1, ix_mem_2)

!      print'(/,'' SPTI initalizing traces in memory.''
!     1,'' ix_mem_1='',i8,'' ix_mem2='',i8)',ix_mem_1,ix_mem_2

!  copy the input header to the output headers intitialize output values
          d_out = 0.0
          w_out = 0.0
          do i = 1, 2
            do ix_mem = 1, nx_mem
              h_out(:,i,ix_mem) = hdi
            end do
          end do

! dwh 08-26-98 add print level test
          if (print_level(1:1)=='B' .or. print_level(1:1)=='Y') then
            write(print_lun, '( &
             & '' SPTI has detected the start of group number='',I8 &
             &,'' at trace number ='',I8)') &
              n_group, it_inp + 1
          end if

          if (print_level(1:1)=='B' .and. &
            n_group<=10 .or. print_level(1:1)=='Y') then
            write(print_lun, '( &
             & '' first output x='',F12.2 &
             &,'' input x='',F12.2,'' y='',F12.2)') &
              x_out_0, x_inp_1, y_inp_1
          end if

        endif                                    ! if (it_inp .eq. nt_inp) then

        x_inp_0 = hdi(hdr_fast)                  ! current x location
        y_inp_0 = hdi(hdr_slow)                  ! current y location

!  determine if this is a different interpolation group
!  if so output all traces in buffer

        if (int(abs(y_inp_0 - slow_val_in)/slow_inc_in) /= &
            int(abs(y_inp_1 - slow_val_in)/slow_inc_in)) then

!  limit the output traces to the range of input traces
          ix_out_1 = ix_mem_1
          ix_out_2 = 1 + int((x_inp_2 - x_out_0)/fast_inc_out)
          x_out_2 = (ix_out_2 - 1)*fast_inc_out + x_out_0

          if (x_dir>0 .and. x_out_2>x_inp_2 .or. &
            x_dir<0 .and. x_out_2<x_inp_2) then
            ix_out_2 = ix_out_2 - 1
          end if

          x_out_2 = (ix_out_2 - 1)*fast_inc_out + x_out_0

          nt_out = nt_out + max(0,ix_out_2 - ix_out_1 + 1)
          ipass = 2

          if (print_level(1:1)=='B' .and. &
            n_group<=10 .or. print_level(1:1)=='Y') then
            write(print_lun, '( &
             & /,'' SPTI has detected a group change'' &
             &,/,'' total number of groups               ='',I8,'' + 1'' &
             &,/,'' total number of input  traces        ='',I8,'' + 1'' &
             &,/,'' total number of output traces        ='',I8 &
             &,/,'' number of input  traces in this group='',I8 &
             &,/,'' number of output traces in this group='',I8 &
             &,/,'' flushing output traces ix_out        ='',I4,1X,I4 &
             &,/,'' first output x='',F12.2 &
             &  ,'' first  input x='',F12.2,'' y='',F12.2 &
             &,/,''  last output x='',F12.2 &
             &  ,''  last  input x='',F12.2,'' y='',F12.2 &
             &,/,''             '',12X &
             &  ,''  this  input x='',F12.2,'' y='',F12.2)') &
              n_group, it_inp, nt_out, it_inp - nt_inp, &
              ix_out_2, ix_out_1, ix_out_2, x_out_0, x_inp_1, &
              y_inp_1, x_out_2, x_inp_2, y_inp_2, x_inp_0, y_inp_0
          end if

          nt_inp = it_inp

          return

        endif               ! if (int(abs(y_inp_0-slow_val_in)/slow_inc_in)

!  if the trace x direction has been established and this violates that
        if (x_inp_0/=x_inp_1 .and. &
          x_dir/=nint(sign(dble(1.),x_inp_0-x_inp_1))) go to 992

!  determine the interpolation limits for this trace
        call spti_x_limits (x_dir, x_interpolate, fast_inc_out, x_out_0, &
          x_inp_0, ix_ter_1, ix_ter_2)

!  output interpolated traces this input trace does not contribute to
        if (ix_ter_1 > ix_mem_1) then

          ix_out_1 = ix_mem_1
          ix_out_2 = ix_ter_1 - 1
          nt_out = nt_out + max(0,ix_out_2 - ix_out_1 + 1)
          ipass = 2

!          print'(/,'' SPTI has detected completed output traces''
!     1,/,'' passing out traces ix_out='',i4,1x,i4
!     1,'' ter='',i4,1x,i4,'' mem='',i4,1x,i4
!     1,/,'' it_inp='',i8,'' it_out='',i8,'' nt_out='',i8)'
!     1,ix_out_1,ix_out_2,ix_ter_1,ix_ter_2,ix_mem_1,ix_mem_2
!     1,it_inp,it_out,nt_out

          ix_mem_1 = ix_ter_1
          ix_mem_2 = ix_ter_2

          return

        else if (ix_ter_2 > ix_mem_2) then  ! if (ix_ter_1 .gt. ix_mem_1)

!      print'(/,'' SPTI changing the traces in memory''
!     1,/,'' ix_mem_1='',i8,'' ix_mem_2='',i8
!     1,/,'' ix_ter_1='',i8,'' ix_ter_2='',i8)'
!     1,ix_mem_1,ix_mem_2,ix_ter_1,ix_ter_2

          ix_mem_2 = max(ix_mem_2,ix_ter_2)

        endif                               ! if (ix_ter_1 .gt. ix_mem_1) then

        x_inp_2 = x_inp_0
        y_inp_2 = y_inp_0
        it_inp = it_inp + 1

!      print'(/,'' SPTI adding input trace '',i4,'' to output traces''
!     1,i4,1x,i4)',it_inp,ix_mem_1,ix_mem_2

!  add this input trace to each of the output traces
        do ix_mem = ix_mem_1, ix_mem_2

          x_mem = (ix_mem - 1)*fast_inc_out + x_out_0  ! output trace location
          id_out = mod(ix_mem - 1,nx_mem) + 1    ! output memory location

          dx_0 = x_mem - x_inp_0                 ! distance input to output
          dx_1 = x_mem - h_out(hdr_fast,1,id_out)! distance to current left
          dx_2 = x_mem - h_out(hdr_fast,2,id_out)! distance to current right

! dwh 08-27-98 restrict the mute values to the trace range
          imute_1 = max(1,min(ndpt,nint(hdi(2))))
          imute_2 = max(1,min(ndpt,nint(hdi(64))))

!  if this input location is closer on the left replace the header values
          if (dx_0>=0. .and. dx_1<0. .or. dx_0>=0. .and. &
            dx_1>0. .and. abs(dx_0)<abs(dx_1) .or. dx_0<=0. .and. &
            dx_1<0. .and. abs(dx_0)<abs(dx_1)) then
            h_out(:,1,id_out) = hdi
          end if

!  if this input location is closer on the right replace the header values
          if (dx_0<=0. .and. dx_2>0. .or. dx_0>=0. .and. &
            dx_2>0. .and. abs(dx_0)<abs(dx_2) .or. dx_0<=0. .and. &
            dx_2<0. .and. abs(dx_0)<abs(dx_2)) then
            h_out(:,2,id_out) = hdi
          end if

          call spti_interpolate (x_interpolate, fast_inc_in, &
            x_inp_0, x_mem, imute_1, imute_2, ndpt, tri, &
            d_out(1,id_out), w_out(1,id_out))

        end do

      endif                                      ! if (ntr .eq. 0) then

      return


  991 continue
      write(print_lun, '( &
       &   '' error in SPTI number of input traces='',I8 &
       &,/,'' SPTI can only accept one input trace at a time'')') ntr
      go to 999

  992 continue
      write(print_lun, '( &
       &   '' error in spti_inp'' &
       &,/,'' this trace is not in the correct order it should be sorted'' &
       &,/,'' x_dir='',I8,'' x_data='',I8 &
       &,/,'' x_inp_1='',F12.2,'' y_inp_1='',F12.2 &
       &,/,'' x_inp_0='',F12.2,'' y_inp_0='',F12.2)') &
        x_dir, nint(sign(dble(1.),x_inp_0 - x_inp_1)), x_inp_1, &
        y_inp_1, x_inp_0, y_inp_0

  999 continue
      write(print_lun, '('' error in spti_inp'')')
      ierr = -1

      return

      end subroutine spti_inp

!</execute_only>

!!--------------------------- spti_x_limits --------------------------------!!
!!--------------------------- spti_x_limits --------------------------------!!
!!--------------------------- spti_x_limits --------------------------------!!

!<execute_only>

      subroutine spti_x_limits(x_dir, x_interpolate, fast_inc_out, x_out_0, &
        x_inp_2, ix_out_1, ix_out_2)
!  determine the interpolation limits for an input trace

      implicit none

      integer , intent(in) :: x_dir
      integer , intent(out) :: ix_out_1
      integer , intent(out) :: ix_out_2
      real , intent(in) :: x_interpolate
      double precision , intent(in) :: fast_inc_out
      double precision , intent(in) :: x_out_0
      double precision , intent(in) :: x_inp_2

      real :: x_out_1, x_out_2

!  interpolation limits for this trace
      if (x_dir > 0) then
        x_out_1 = max(x_out_0,x_inp_2 - x_interpolate)
        x_out_2 = x_inp_2 + x_interpolate
      else
        x_out_1 = min(x_out_0,x_inp_2 + x_interpolate)
        x_out_2 = x_inp_2 - x_interpolate
      endif

      ix_out_1 = max(1,1 + int((x_out_1 - x_out_0)/fast_inc_out))
      ix_out_2 = max(1,1 + int((x_out_2 - x_out_0)/fast_inc_out))

!      print'(/,'' spti_x_limits x_dir='',i2,'' x_interpolate='',f10.2
!     1,/,'' x_out_0='',f10.2,'' x_inp_2='',f10.2
!     1,/,'' x_out_1='',f10.2,'' x_out_2='',f10.2
!     1,/,'' ix_out_1='',i8,'' ix_out_2='',i8)'
!     1,x_dir,x_interpolate,x_out_0,x_inp_2
!     1,x_out_1,x_out_2,ix_out_1,ix_out_2

      return
      end subroutine spti_x_limits

!</execute_only>

!!------------------------------ spti_out ----------------------------------!!
!!------------------------------ spti_out ----------------------------------!!
!!------------------------------ spti_out ----------------------------------!!

!<execute_only>

      subroutine spti_out(ntr, n_group, ix_out_2, jt_out, it_out, nt_out, &
        hdr_fast, hdr_slow, nx_mem, x_out_0, fast_val_out, fast_inc_out, &
        nwih, ndpt, h_out, d_out, w_out, hdo, tro, opt_mute)
!  output next trace

      implicit none

      integer , intent(out) :: ntr
      integer , intent(in) :: n_group
      integer , intent(in) :: ix_out_2
      integer , intent(inout) :: jt_out
      integer , intent(inout) :: it_out
      integer , intent(in) :: nt_out
      integer , intent(in) :: hdr_fast
      integer  :: hdr_slow
      integer , intent(in) :: nx_mem
      integer  :: nwih
      integer  :: ndpt
      double precision , intent(in) :: x_out_0
      real  :: fast_val_out
      double precision , intent(in) :: fast_inc_out
      double precision  :: h_out(nwih,2,nx_mem)
      real  :: d_out(ndpt,nx_mem)
      real  :: w_out(ndpt,nx_mem)
      double precision , intent(out) :: hdo(nwih)
      real  :: tro(ndpt)
      logical , intent(in) :: opt_mute

      integer ::    ix_out, id_out 
      double precision :: x_out, dx_h_out, f1, f2

      ntr = 1
      it_out = it_out + 1
      jt_out = jt_out + 1
      ix_out = ix_out_2 - nt_out + it_out
      id_out = mod(ix_out - 1,nx_mem) + 1

! dwh 08-25-98 the original code had fast_val_out in the spti_out call
! and should have had x_out_0

      x_out = (ix_out - 1)*fast_inc_out + x_out_0
      dx_h_out = h_out(hdr_fast,2,id_out) - h_out(hdr_fast,1,id_out)

      if (dx_h_out == 0.) dx_h_out = 1.

      f2 = max(dble(0.),min(dble(1.), &
        (x_out - h_out(hdr_fast,1,id_out))/dx_h_out))
      f1 = 1. - f2

!  copy trace
!     --- put inside where statement (wmm) --- tro = 0.

      where (w_out(:ndpt,id_out) /= 0.)
        tro(:ndpt) = d_out(:ndpt,id_out)/w_out(:ndpt,id_out)
      else where
        tro(:ndpt) = 0.0
      end where

!  copy header
      where(h_out(:nwih,1,id_out) == h_out(:nwih,2,id_out))
        ! This prevents floating point round off errors from 
        ! messing up constant header values.
        hdo(:nwih) = h_out(:nwih,1,id_out)
      else where
        hdo(:nwih) = f1*h_out(:nwih,1,id_out) + f2*h_out(:nwih,2,id_out)
      end where

      hdo(hdr_fast) = x_out
      hdo(1) = it_out
      hdo(3) = n_group
      hdo(4) = jt_out
!     -- --------------------------------------------------------------
!     -- wmm added the next line to set header 48 to the hdr48_set_value 
!     -- only when trace was interpolated. (hdo was interpolated above
!     -- and should have had sequential trace numbers in #48 temporarily
!     -- which would have not been equal, therefore it would have been
!     -- interpolated using the "else where" above.  This is how I am
!     -- discriminating between interpolated traces and original ones.
      if(hdo(48) /= h_out(48,1,id_out) .and. &
         hdo(48) /= h_out(48,2,id_out) ) hdo(48) = hdr48_set_value
!     -- --------------------------------------------------------------

! dwh 08-25-98   set the output top and bottom mutes to the
!   min and max input mutes respectively
! dwh 08-25-98       hdo(2) = max(h_out(2,1,id_out),h_out(2,2,id_out))
! dwh 08-25-98       hdo(64) = min(h_out(64,1,id_out),h_out(64,2,id_out))

      if(opt_mute) then
        hdo(2) = max(h_out(2,1,id_out),h_out(2,2,id_out))
        hdo(64) = min(h_out(64,1,id_out),h_out(64,2,id_out))
      else
        hdo(2) = min(h_out(2,1,id_out),h_out(2,2,id_out))
        hdo(64) = max(h_out(64,1,id_out),h_out(64,2,id_out))
      end if

      if(hdo(2) <= hdo(64)) then
        tro(:int(hdo(2))-1) = 0.0
        tro(int(hdo(64))+1:) = 0.0

        ! set header LAV (dead trace flag)
        call lav_set_hdr(hdo(:), tro(:), ndpt)
      else
        ! dead trace
        hdo(64) = hdo(2)
        hdo(hdr_lav) = 0.0
        tro = 0.0
      end if

!  reset header,trace to zero
      h_out(:,:,id_out) = 0.0
      d_out(:,id_out) = 0.0
      w_out(:,id_out) = 0.0

!      print'('' SPTI passing out trace '',i4,'' of '',i4
!     1,'' x='',f12.2,'' y='',f12.2,'' trmax='',g15.9)'
!     1,it_out,nt_out,hdo(hdr_fast),hdo(hdr_slow),hdo(25)

      return
      end subroutine spti_out

!</execute_only>

!!-------------------------- spti_interpolate ------------------------------!!
!!-------------------------- spti_interpolate ------------------------------!!
!!-------------------------- spti_interpolate ------------------------------!!

!<execute_only>

      subroutine spti_interpolate(x_interpolate, fast_inc_in, x_inp, x_out, &
        imute_1, imute_2, n, d_inp, d_out, w_out)
!  sum a weighted input trace into an output trace

      implicit none

      integer , intent(in) :: imute_1
      integer , intent(in) :: imute_2
      integer , intent(in) :: n
      real , intent(in) :: x_interpolate
      real , intent(in) :: fast_inc_in
      double precision , intent(in) :: x_inp
      double precision , intent(in) :: x_out
      real , intent(in) :: d_inp(n)
      real , intent(inout) :: d_out(n)
      real , intent(inout) :: w_out(n)


      real :: pi, dx_min, dx_max, dx_int, dx_ham, weight

      pi = acos(-1.)
      dx_min = pi*x_interpolate/fast_inc_in*1.E-6     ! min separation
      dx_max = pi*x_interpolate/fast_inc_in           ! max separation
      dx_int = pi*(x_out - x_inp)/fast_inc_in         ! vector separation
      dx_ham = pi*(x_out - x_inp)/x_interpolate  ! hamming cosine

! compute interpolator weight (sinc windowed with Hamming window)
      if (abs(dx_int) < dx_min) then
        weight = 1.
      else if (abs(dx_int) > dx_max) then
        weight = 0.
      else                          ! if     (abs(dx_int) .lt. dx_min) then
        weight = (0.54 + 0.46*cos(dx_ham))*sin(dx_int)/dx_int
      endif                         ! if     (abs(dx_int) .lt. dx_min) then

!      print'('' x_inp='',f10.2,'' x_out='',f10.2,'' weight='',f10.4)'
!     1,x_inp,x_out,weight

!  sum the seighted input vector into the output vector
      if (weight /= 0.) then
        d_out(imute_1:imute_2) = d_out(imute_1:imute_2) + &
          weight*d_inp(imute_1:imute_2)
        w_out(imute_1:imute_2) = w_out(imute_1:imute_2) + weight
      endif

      return
      end subroutine spti_interpolate

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module spti_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
