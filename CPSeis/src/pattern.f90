!<CPS_v1 type="PRIMITIVE"/>
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
!                        C P S   P R I M I T I V E
!
! Name       : pattern  (do-skip PATTERN verification)
! Category   : miscellaneous
! Written    : 1999-09-24   by: Randy L. Selzler
! Revised    : 2002-02-14   by: Randy Selzler, Data-Warp, Inc.
! Maturity   : production   2002-03-04
! Purpose    : Support parameter patterns (BEG, END, INC, TOT and variations).
!   Convenience routines to verify values and rationalize physical coordinates
!   and logical indexes.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!
!              *NEW*      GENERAL DESCRIPTION
!
!  pattern_stop2  Function should be used when verifying parameters
!                 utilizing "END", "MAX" or "LAST" keyword style.
!                 Example: BEG_x,   END_x, INC_x, TOT_x.
!                 Example: MIN_x,   MAX_x, INC_x, TOT_x.
!                 Example: INIT_x, LAST_x, INC_x, TOT_x.
!
!  pattern_len2   Function should be used when verifying parameters
!                 utilizing "LEN"" keyword style.
!                 Example: BEG_x,  LEN_x, INC_x, TOT_x.
!                 Example: MIN_x,  LEN_x, INC_x, TOT_x.
!                 Example: INIT_x, LEN_x, INC_x, TOT_x.
!
!              *OLD*      GENERAL DESCRIPTION  *** DEPRECATED *** AVOID ***
!
!  THE *NEW* SUBROUTINES "pattern_stop2" and "pattern_len2"
!  ARE NOW PREFERRED BECAUSE THE API IS MORE INTUITIVE THAN THE FOLLOWING.
!
!  The following table summarizes the convenience subroutines for patterns.
!  Patterns are sequences described by start, stop, increment and count.
!  The routines provide a standard for update-trap processing (range checks,
!  user messages and implicit changes of redundant information).
!
!    PATTERN VERIFY API    |      | OPTIONAL  |
!    COMBINATIONS          |VALUE | MIN | MAX |
!    ----------------------+------+-----+-----+
!    1    BEG ? MIN ? INIT | real |-inf |+inf |
!    2a   END ? MAX ? LAST | real |-inf |+inf |
!    2b   LEN              | real |  0  |+inf |
!    3    INC              | real |-inf |+inf |
!    4    TOT              | int  |  0  |+inf |
!    ----------------------+------+-----+-----+
!
!  For example, the TSLC process (Time SLiCe) has a crossline axis
!  described by CRL_INIT, CRL_LAST, CRL_INC and CRL_TOT).
!  The increment and total number of points must be greater than zero.
!  If the user changes CRL_LAST, then CRL_TOT must be implicitly computed.
!  The following code fragments from TSLC illustrates PATTERN routines:
!
!    TSLC_UPDATE gets parameters like this...
!      call pc_get('CRL_LAST', obj%crl_last, TSLC_CRL_PATTERN_TRAP)
!
!    Subroutine TSLC_CRL_PATTERN_TRAP contains this...
!        call pattern_init_last(keyword, object%inl_init, object%inl_last, &
!          object%inl_inc, object%inl_tot, msg, &
!          inc_min= tiny(obj%inl_inc), tot_min = 1)
!        if(msg(1:1) /= ' ') then
!          call pc_error(msg)
!          call pc_jump_field(keyword)
!        end if
!
!  Update precedence for rationalizing redundant information:
!    1) TOT is updated, if END/MAX/LAST/LEN changes.
!    2) END/MAX/LAST/LEN is updated, if BEG/MIN/INIT, INC or TOT changes.
!
! PATTERN_VERIFY:
!
!  A special version (actually the original version) called pattern_verify
!  is provided for use by the select process.
!  The do-skip parameters used by select are taken from 4 elements of
!  one array keyword.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
!-------------------------------------------------------------------------------
!</trace_io_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!               *NEW*      CALLING SEQUENCE
!
!   o                     i      i
! status = pattern_stop2 (msg, verify, &
!             b         b        b         b
!         start_val, inc_val, stop_val, total_val, &
!             i         i        i         i
!         start_key, inc_key, stop_key, total_key, &
!             i         i        i         i
!         start_ver, inc_ver, stop_ver, total_ver, &
!            opt       opt      opt       opt
!             i         i        i         i
!         start_min, inc_min, stop_min, total_min, &
!            opt       opt      opt       opt
!             i         i        i         i
!         start_max, inc_max, stop_max, total_max)
!
!
!   o                     i      i
! status = pattern_len2 (msg, verify, &
!             b         b        b         b
!         start_val, inc_val, len_val, total_val, &
!             i         i        i         i
!         start_key, inc_key, len_key, total_key, &
!             i         i        i         i
!         start_ver, inc_ver, len_ver, total_ver, &
!            opt       opt      opt       opt
!             i         i        i         i
!         start_min, inc_min, len_min, total_min, &
!            opt       opt      opt       opt
!             i         i        i         i
!         start_max, inc_max, len_max, total_max)
!
! integer            pattern_stop2  = Return value from function.
! integer            pattern_len2   = Return value from function.
!                                     non-zero if an error is detected.
!
! character(len=*)           msg    = Prefix for error messages.
!                                     Typical value is "<process name>:".
! logical                    verify = True if parameters should be verified.
!                                     Typically "verify" should be .true. when
!                                     the function pc_get_update_state()
!                                     returns PC_FRONTEND or PC_BACKEND.
!
! real                       start_val = BEG/MIN/INIT parameter value 
! real                       inc_val   = INC increment parameter value 
! real                       stop_val  = END/MAX/LAST parameter value 
! real                       len_val   = LEN length parameter value 
! integer                    total_val = TOT total parameter value
!                                        These are typically the values
!                                        manipulated by pc_put and pc_get.
!
! character(len=*)           start_key  = START/MIN/INIT keyword
! character(len=*)           inc_key    = INC increment keyword
! character(len=*)           stop_key   = END/MAX/LAST keyword
! character(len=*)           len_key    = END/MAX/LAST keyword
! character(len=*)           total_key  = TOT total keyword
!                                        These are typically the keywords
!                                        manipulated by pc_put and pc_get.
!
! logical                    start_ver = BEG/MIN/INIT verify flag 
! logical                    inc_ver   = INC increment verify flag 
! logical                    stop_ver  = END/MAX/LAST verify flag 
! logical                    len_ver   = LEN length verify flag 
! logical                    total_ver = TOT total verify flag
!                                        These are typically the values
!                                        returned by the function
!                                        pc_verify_scalar("<keyword>").
!
! real                       start_min = BEG/MIN/INIT parameter minimum 
!                                        The default is -huge(real).
! real                       inc_min   = INC increment parameter minimum 
!                                        The STOP default is -huge(real).
!                                        The LEN  default and minimum is zero.
! real                       stop_min  = END/MAX/LAST parameter minimum 
!                                        The default is -huge(real).
! real                       len_min   = LEN length parameter minimum 
!                                        The default and minimum is zero.
! integer                    total_min = TOT total parameter minimum
!                                        The default=1 and minimum=0.
!                                        If total_min is explicitly set to
!                                        zero, then verification is inhibited
!                                        if the actual total_val is zero.
!
! real                       start_max = BEG/MIN/INIT parameter maximum 
! real                       inc_max   = INC increment parameter maximum 
! real                       stop_max  = END/MAX/LAST parameter maximum 
!                                        The default is +huge(real).
! real                       len_max   = LEN length parameter maximum 
!                                        The default and maximum are huge.
! integer                    total_max = TOT total parameter maximum
!                                        The default and maximum are huge.
!
!               *OLD*      CALLING SEQUENCE  *** DEPRECATED *** AVOID ***
!
!                         i      b    b     b     b     o
! call pattern_beg_end  (mkey,  rbeg, rend, rinc, rtot, msg, &
!   strt_min, strt_max, stop_min, stop_max, inc_min, inc_max, tot_min, tot_max)
!      opt       opt       opt       opt      opt      opt      opt      opt
!
!                         i     b     b     b     b     o
! call pattern_beg_len  (mkey,  rbeg, rlen, rinc, rtot, msg, &
!   strt_min, strt_max, len_min, len_max, inc_min, inc_max, tot_min, tot_max)
!      opt       opt      opt      opt      opt      opt      opt      opt
!
!                         i     b     b     b     b     o
! call pattern_min_max  (mkey,  rmin, rmax, rinc, rtot, msg, &
!   strt_min, strt_max, stop_min, stop_max, inc_min, inc_max, tot_min, tot_max)
!      opt       opt       opt       opt      opt      opt      opt      opt
!
!                         i     b     b     b     b     o
! call pattern_min_len  (mkey,  rmin, rlen, rinc, rtot, msg, &
!   strt_min, strt_max, len_min, len_max, inc_min, inc_max, tot_min, tot_max)
!      opt       opt      opt      opt      opt      opt      opt      opt
!
!                         i     b     b     b     b     o
! call pattern_init_last(mkey, rinit,rlast, rinc, rtot, msg, &
!   strt_min, strt_max, stop_min, stop_max, inc_min, inc_max, tot_min, tot_max)
!      opt       opt       opt       opt      opt      opt      opt      opt
!
!                         i     b     b     b     b     o
! call pattern_init_len (mkey, rinit, rlen, rinc, rtot, msg, &
!   strt_min, strt_max, len_min, len_max, inc_min, inc_max, tot_min, tot_max)
!      opt       opt      opt      opt      opt      opt      opt      opt
!
!  The preceeding public subroutines set up various arguments and call a
!  private subroutine (pattern_stop or pattern_len).  Set up outline:
!    1) Initialize a table of keywords, based upon the value of mkey.
!       For example, if patterrn_beg_end is called with mkey "CRL_INC",
!       the table contains "CRL_BEG", "CRL_END", "CRL_INC" and "CRL_TOT".
!       The table is used to construct meaningful error messages.
!    2) Detect optional arguments and create explicit limits.  Example:
!       IF(PRESENT(STRT_MIN)) THEN
!         STRT2_MIN = STRT_MIN
!       ELSE
!         STRT2_MIN= - HUGE(STRT2_MIN)
!       END IF
!    3) call the private subroutine based upon the public entry point.
!       A) call pattern_stop, if public call was pattern_beg_end,
!         pattern_min_max or pattern_init_last.
!       B) call pattern_len, if public call was pattern_beg_len,
!         pattern_min_len or pattern_init_len.
!
!
!
!                     i      i      b      b     b     b     o
! call pattern_stop (ikey, table, rstrt, rstop, rinc, rtot, msg, &
!   strt_min, strt_max, stop_min, stop_max, inc_min, inc_max, tot_min, tot_max)
!       i         i         i         i        i        i        i        i
!
!                    i      i      b     b     b     b     o
! call pattern_len (ikey, table, rstrt, rlen, rinc, rtot, msg, &
!   strt_min, strt_max, len_min, len_max, inc_min, inc_max, tot_min, tot_max)
!       i         i        i        i        i        i        i        i
!
! character(len=*)           mkey     = Keyword provided by trap call
!
! character(len=*)           table   = table of all 4 keywords
! integer                    ikey    = index of changed mkey in table
!                                      1, iff mkey = BEG/MIN/INIT
!                                      2, iff mkey = END/MAX/LAST/LEN
!                                      3, iff mkey = INC
!                                      4, iff mkey = TOT
!
! real                       rbeg    = Beginning physical coordinate.
! real                       rend    = Ending    physical coordinate.
!
! real                       rmin    = minimum physical coordinate.
! real                       rmax    = maximum physical coordinate.
!
! real                       rinit   = initial physical coordinate.
! real                       rlast   = last    physical coordinate.
!
! real                       rlen    = Length = abs(rend - rbeg)
!
! real                       rinc    = Pattern increment in physical units.
!
! integer                    rtot    = Total number of pattern points.
!
! character(len=*)           msg     = Error message (1:1 is blank if OK).
!
! real                       strt_min = Start minimum
! real                       strt_max = Start maximum
!
! real                       stop_min = Stop minimum
! real                       stop_max = Stop maximum
!
! real                       len_min = Length minimum
! real                       len_max = Length maximum
!
! real                       inc_min = Increment minimum
! real                       inc_max = Increment maximum
!
! real                       tot_min = Total minimum
! real                       tot_max = Total maximum
!
!
! PATTERN_VERIFY:
!                  o                      i    i
!                valid = pattern_verify (cnt, msg,
!                     i          i         i        i
!                  old_start, old_inc, old_stop, old_cnt,
!                     b          b         b        b
!                  new_start, new_inc, new_stop, new_cnt)
!
! logical pattern_verify = function return value
! integer cnt = number of valid new parameters (counting left to right)
!             <  3, warning msg, 3 or 4 new args required
!             == 3, compute new_cnt from new_start, new_inc and new_stop.
!                   recompute implicit new_stop.
!             == 4, compute one of new_start, new_inc, new_stop
!                   or new_cnt, depending upon what changed.
!                   recompute implicit new parameters.
! character(len=*)              msg =       --> prefix for messages
! real                    old_start =       --> old start coordinate
! real                    old_inc   =       --> old increment between points
! real                    old_stop  =       --> old stop coordinate
! integer                 old_cnt   =       --> old count (number of points)
! real                    new_start =       --> new start coordinate
! real                    new_inc   =       --> new increment between points
! real                    new_stop  =       --> new stop coordinate
! integer                 new_cnt   =       --> new count (number of points)
!
! see "select" process for example usage
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 10. 2002-03-04  Selzler    Improved priority of total computation.
!  9. 2001-03-13  Selzler    Corrected closing header_word_doc tag.
!  8. 2000-07-07  Selzler    Fixed problems found by CPS Fortran Code Review.
!  7. 2000-05-17  Selzler    Increased tolerance in STOP2 and LEN2
!  6. 2000-03-28  Selzler    Corrected bug in STOP2 and LEN2
!  5. 2000-03-14  Selzler    Created STOP2 and LEN2 primitives.
!  4. 2000-01-25  Selzler    Clean up trailing blanks and block labels
!  3. 1999-11-23  Selzler    merged pattern_verify into this primitive
!  2. 1999-11-19  Selzler    Added RCS "Id" strings to tag executeable
!  1. 1999-09-24  Selzler    Initial version.
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

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!
!-------------------------------------------------------------------------------
!</programming_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!

      module pattern_module
      use pc_module
      use named_constants_module
      use mth_module
      use string_module
      implicit none

      private
      public :: pattern_beg_end
      public :: pattern_beg_len
      public :: pattern_min_max
      public :: pattern_min_len
      public :: pattern_init_last
      public :: pattern_init_len

      public :: pattern_stop
      public :: pattern_len

      public :: pattern_verify

      public :: pattern_stop2
      public :: pattern_len2

!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      integer, parameter :: KEY_MAX = 16

      character(len=100),public :: pattern_ident = &
        "$Id: pattern.f90,v 1.10 2002/02/28 22:06:39 Selzler prod sps $"

      contains

!!------------------------ pattern_beg_end -------------------------------!!
!!------------------------ pattern_beg_end -------------------------------!!
!!------------------------ pattern_beg_end -------------------------------!!

subroutine pattern_beg_end (mkey,  rbeg, rend, rinc, rtot, msg, &
  strt_min, strt_max, stop_min, stop_max, inc_min, inc_max, tot_min, tot_max)
  implicit none

  character(len=*) :: mkey       ! argument
  real :: rbeg                  ! argument
  real :: rend                  ! argument
  real :: rinc                  ! argument
  integer :: rtot               ! argument
  character(len=*) :: msg       ! argument

  real, optional :: strt_min    ! argument
  real, optional :: strt_max    ! argument
  real, optional :: stop_min    ! argument
  real, optional :: stop_max    ! argument
  real, optional :: inc_min     ! argument
  real, optional :: inc_max     ! argument
  integer, optional :: tot_min  ! argument
  integer, optional :: tot_max  ! argument

  real :: strt2_min    ! local
  real :: strt2_max    ! local
  real :: stop2_min    ! local
  real :: stop2_max    ! local
  real :: inc2_min     ! local
  real :: inc2_max     ! local
  integer :: tot2_min  ! local
  integer :: tot2_max  ! local

  integer :: key_len   ! local
  integer :: ikey      ! local

  character(len=KEY_MAX) :: ukey                 ! local
  character(len=KEY_MAX), dimension(4) :: table  ! local

  ukey = mkey
  call string_to_upper(ukey)
  key_len = len_trim(ukey)

  if(key_len < 4) then
    msg = "pattern_beg_end: keyword too short, value=" // ukey
    return
  else if(key_len >= KEY_MAX) then
    msg = "pattern_beg_end: keyword too long, value=" &
      // ukey(:min(KEY_MAX,key_len))
    return
  else if(ukey(1:4) == 'BEG_' .or. &
          ukey(1:4) == 'END_' .or. &
          ukey(1:4) == 'INC_' .or. &
          ukey(1:4) == 'TOT_') then

     select case(ukey(1:1))
     case ("B")  ! Begin
       ikey = 1
     case ("E")  ! End
       ikey = 2
     case ("I")  ! Increment
       ikey = 3
     case ("T")  ! Total
       ikey = 4
     end select

     table(1) = "BEG" // ukey(4:)
     table(2) = "END" // ukey(4:)
     table(3) = "INC" // ukey(4:)
     table(4) = "TOT" // ukey(4:)
  else if(ukey(key_len-3:key_len) == '_BEG' .or. &
          ukey(key_len-3:key_len) == '_END' .or. &
          ukey(key_len-3:key_len) == '_INC' .or. &
          ukey(key_len-3:key_len) == '_TOT') then

     select case(ukey(key_len-2:key_len-2))
     case ("B")  ! Begin
       ikey = 1
     case ("E")  ! End
       ikey = 2
     case ("I")  ! Increment
       ikey = 3
     case ("T")  ! Total
       ikey = 4
     end select

     table(1) = ukey(:key_len-3) // "BEG"
     table(2) = ukey(:key_len-3) // "END"
     table(3) = ukey(:key_len-3) // "INC"
     table(4) = ukey(:key_len-3) // "TOT"
  else
    msg = "pattern_beg_end: keyword not recognized, value=" &
      // ukey(:min(KEY_MAX,key_len))
    return
  end if

  if(present(strt_min)) then
    strt2_min = strt_min
  else
    strt2_min = - huge(strt2_min)
  end if

  if(present(strt_max)) then
    strt2_max = strt_max
  else
    strt2_max = + huge(strt2_max)
  end if

  if(present(stop_min)) then
    stop2_min = stop_min
  else
    stop2_min = - huge(stop2_min)
  end if

  if(present(stop_max)) then
    stop2_max = stop_max
  else
    stop2_max = + huge(stop2_max)
  end if

  if(present(inc_min)) then
    inc2_min = inc_min
  else
    inc2_min = - huge(inc2_min)
  end if

  if(present(inc_max)) then
    inc2_max = inc_max
  else
    inc2_max = + huge(inc2_max)
  end if

  if(present(tot_min)) then
    tot2_min = tot_min
  else
    tot2_min = 0
  end if

  if(present(tot_max)) then
    tot2_max = tot_max
  else
    tot2_max = + huge(tot2_max)
  end if

  call pattern_stop(ikey, table, rbeg, rend, rinc, rtot, msg, &
    strt2_min, strt2_max, &
    stop2_min, stop2_max, &
    inc2_min, inc2_max, &
    tot2_min, tot2_max)

  return
  end subroutine pattern_beg_end

!!------------------------ pattern_beg_len -------------------------------!!
!!------------------------ pattern_beg_len -------------------------------!!
!!------------------------ pattern_beg_len -------------------------------!!

subroutine pattern_beg_len (mkey,  rbeg, rlen, rinc, rtot, msg, &
  strt_min, strt_max, len_min, len_max, inc_min, inc_max, tot_min, tot_max)
  implicit none

  character(len=*) :: mkey       ! argument
  real :: rbeg                  ! argument
  real :: rlen                  ! argument
  real :: rinc                  ! argument
  integer :: rtot               ! argument
  character(len=*) :: msg       ! argument

  real, optional :: strt_min    ! argument
  real, optional :: strt_max    ! argument
  real, optional :: len_min     ! argument
  real, optional :: len_max     ! argument
  real, optional :: inc_min     ! argument
  real, optional :: inc_max     ! argument
  integer, optional :: tot_min  ! argument
  integer, optional :: tot_max  ! argument

  real :: strt2_min    ! local
  real :: strt2_max    ! local
  real :: len2_min     ! local
  real :: len2_max     ! local
  real :: inc2_min     ! local
  real :: inc2_max     ! local
  integer :: tot2_min  ! local
  integer :: tot2_max  ! local

  integer :: key_len   ! local
  integer :: ikey      ! local

  character(len=KEY_MAX) :: ukey    ! local
  character(len=KEY_MAX), dimension(4) :: table  ! local

  ukey = mkey
  call string_to_upper(ukey)
  key_len = len_trim(ukey)

  if(key_len < 4) then
    msg = "pattern_beg_len: keyword too short, value=" // ukey
    return
  else if(key_len >= KEY_MAX) then
    msg = "pattern_beg_len: keyword too long, value=" &
      // ukey(:min(KEY_MAX,key_len))
    return
  else if(ukey(1:4) == 'BEG_' .or. &
          ukey(1:4) == 'LEN_' .or. &
          ukey(1:4) == 'INC_' .or. &
          ukey(1:4) == 'TOT_') then

     select case(ukey(1:1))
     case ("B")  ! Begin
       ikey = 1
     case ("L")  ! Length
       ikey = 2
     case ("I")  ! Increment
       ikey = 3
     case ("T")  ! Total
       ikey = 4
     end select

     table(1) = "BEG" // ukey(4:)
     table(2) = "LEN" // ukey(4:)
     table(3) = "INC" // ukey(4:)
     table(4) = "TOT" // ukey(4:)
  else if(ukey(key_len-3:key_len) == '_BEG' .or. &
          ukey(key_len-3:key_len) == '_LEN' .or. &
          ukey(key_len-3:key_len) == '_INC' .or. &
          ukey(key_len-3:key_len) == '_TOT') then

     select case(ukey(key_len-2:key_len-2))
     case ("B")  ! Begin
       ikey = 1
     case ("L")  ! Length
       ikey = 2
     case ("I")  ! Increment
       ikey = 3
     case ("T")  ! Total
       ikey = 4
     end select

     table(1) = ukey(:key_len-3) // "BEG"
     table(2) = ukey(:key_len-3) // "LEN"
     table(3) = ukey(:key_len-3) // "INC"
     table(4) = ukey(:key_len-3) // "TOT"
  else
    msg = "pattern_beg_len: keyword not recognized, value=" &
      // ukey(:min(KEY_MAX,key_len))
    return
  end if

  if(present(strt_min)) then
    strt2_min = strt_min
  else
    strt2_min = - huge(strt2_min)
  end if

  if(present(strt_max)) then
    strt2_max = strt_max
  else
    strt2_max = + huge(strt2_max)
  end if

  if(present(len_min)) then
    len2_min = len_min
  else
    len2_min = 0.0
  end if

  if(present(len_max)) then
    len2_max = len_max
  else
    len2_max = + huge(len2_max)
  end if

  if(present(inc_min)) then
    inc2_min = inc_min
  else
    inc2_min = - huge(inc2_min)
  end if

  if(present(inc_max)) then
    inc2_max = inc_max
  else
    inc2_max = + huge(inc2_max)
  end if

  if(present(tot_min)) then
    tot2_min = tot_min
  else
    tot2_min = 0
  end if

  if(present(tot_max)) then
    tot2_max = tot_max
  else
    tot2_max = + huge(tot2_max)
  end if

  call pattern_len(ikey, table, rbeg, rlen, rinc, rtot, msg, &
    strt2_min, strt2_max, &
    len2_min, len2_max, &
    inc2_min, inc2_max, &
    tot2_min, tot2_max)

  return
  end subroutine pattern_beg_len

!!------------------------ pattern_min_max -------------------------------!!
!!------------------------ pattern_min_max -------------------------------!!
!!------------------------ pattern_min_max -------------------------------!!

subroutine pattern_min_max (mkey,  rmin, rmax, rinc, rtot, msg, &
  strt_min, strt_max, stop_min, stop_max, inc_min, inc_max, tot_min, tot_max)
  implicit none

  character(len=*) :: mkey       ! argument
  real :: rmin                  ! argument
  real :: rmax                  ! argument
  real :: rinc                  ! argument
  integer :: rtot               ! argument
  character(len=*) :: msg       ! argument

  real, optional :: strt_min    ! argument
  real, optional :: strt_max    ! argument
  real, optional :: stop_min    ! argument
  real, optional :: stop_max    ! argument
  real, optional :: inc_min     ! argument
  real, optional :: inc_max     ! argument
  integer, optional :: tot_min  ! argument
  integer, optional :: tot_max  ! argument

  real :: strt2_min    ! local
  real :: strt2_max    ! local
  real :: stop2_min    ! local
  real :: stop2_max    ! local
  real :: inc2_min     ! local
  real :: inc2_max     ! local
  integer :: tot2_min  ! local
  integer :: tot2_max  ! local

  integer :: key_len   ! local
  integer :: ikey      ! local

  character(len=KEY_MAX) :: ukey    ! local
  character(len=KEY_MAX), dimension(4) :: table  ! local

  ukey = mkey
  call string_to_upper(ukey)
  key_len = len_trim(ukey)

  if(key_len < 4) then
    msg = "pattern_min_max: keyword too short, value=" // ukey
    return
  else if(key_len >= KEY_MAX) then
    msg = "pattern_min_max: keyword too long, value=" &
      // ukey(:min(KEY_MAX,key_len))
    return
  else if(ukey(1:4) == 'MIN_' .or. &
          ukey(1:4) == 'MAX_' .or. &
          ukey(1:4) == 'INC_' .or. &
          ukey(1:4) == 'TOT_') then

     select case(ukey(2:2))
     case ("I")  ! mInimum
       ikey = 1
     case ("A")  ! mAximum
       ikey = 2
     case ("N")  ! iNcrement
       ikey = 3
     case ("O")  ! tOtal
       ikey = 4
     end select

     table(1) = "MIN" // ukey(4:)
     table(2) = "MAX" // ukey(4:)
     table(3) = "INC" // ukey(4:)
     table(4) = "TOT" // ukey(4:)
  else if(ukey(key_len-3:key_len) == '_MIN' .or. &
          ukey(key_len-3:key_len) == '_MAX' .or. &
          ukey(key_len-3:key_len) == '_INC' .or. &
          ukey(key_len-3:key_len) == '_TOT') then

     select case(ukey(key_len:key_len))
     case ("N")  ! miNimum
       ikey = 1
     case ("X")  ! maXimum
       ikey = 2
     case ("C")  ! inCrement
       ikey = 3
     case ("T")  ! toTal
       ikey = 4
     end select

     table(1) = ukey(:key_len-3) // "MIN"
     table(2) = ukey(:key_len-3) // "MAX"
     table(3) = ukey(:key_len-3) // "INC"
     table(4) = ukey(:key_len-3) // "TOT"
  else
    msg = "pattern_min_max: keyword not recognized, value=" &
      // ukey(:min(KEY_MAX,key_len))
    return
  end if

  if(present(strt_min)) then
    strt2_min = strt_min
  else
    strt2_min = - huge(strt2_min)
  end if

  if(present(strt_max)) then
    strt2_max = strt_max
  else
    strt2_max = + huge(strt2_max)
  end if

  if(present(stop_min)) then
    stop2_min = stop_min
  else
    stop2_min = - huge(stop2_min)
  end if

  if(present(stop_max)) then
    stop2_max = stop_max
  else
    stop2_max = + huge(stop2_max)
  end if

  if(present(inc_min)) then
    inc2_min = inc_min
  else
    inc2_min = - huge(inc2_min)
  end if

  if(present(inc_max)) then
    inc2_max = inc_max
  else
    inc2_max = + huge(inc2_max)
  end if

  if(present(tot_min)) then
    tot2_min = tot_min
  else
    tot2_min = 0
  end if

  if(present(tot_max)) then
    tot2_max = tot_max
  else
    tot2_max = + huge(tot2_max)
  end if

  call pattern_stop(ikey, table, rmin, rmax, rinc, rtot, msg, &
    strt2_min, strt2_max, &
    stop2_min, stop2_max, &
    inc2_min, inc2_max, &
    tot2_min, tot2_max)

  return
  end subroutine pattern_min_max

!!------------------------ pattern_min_len -------------------------------!!
!!------------------------ pattern_min_len -------------------------------!!
!!------------------------ pattern_min_len -------------------------------!!

subroutine pattern_min_len (mkey,  rmin, rlen, rinc, rtot, msg, &
  strt_min, strt_max, len_min, len_max, inc_min, inc_max, tot_min, tot_max)
  implicit none

  character(len=*) :: mkey       ! argument
  real :: rmin                  ! argument
  real :: rlen                  ! argument
  real :: rinc                  ! argument
  integer :: rtot               ! argument
  character(len=*) :: msg       ! argument

  real, optional :: strt_min    ! argument
  real, optional :: strt_max    ! argument
  real, optional :: len_min     ! argument
  real, optional :: len_max     ! argument
  real, optional :: inc_min     ! argument
  real, optional :: inc_max     ! argument
  integer, optional :: tot_min  ! argument
  integer, optional :: tot_max  ! argument

  real :: strt2_min    ! local
  real :: strt2_max    ! local
  real :: len2_min     ! local
  real :: len2_max     ! local
  real :: inc2_min     ! local
  real :: inc2_max     ! local
  integer :: tot2_min  ! local
  integer :: tot2_max  ! local

  integer :: key_len   ! local
  integer :: ikey      ! local

  character(len=KEY_MAX) :: ukey    ! local
  character(len=KEY_MAX), dimension(4) :: table  ! local

  ukey = mkey
  call string_to_upper(ukey)
  key_len = len_trim(ukey)

  if(key_len < 4) then
    msg = "pattern_min_len: keyword too short, value=" // ukey
    return
  else if(key_len >= KEY_MAX) then
    msg = "pattern_min_len: keyword too long, value=" &
      // ukey(:min(KEY_MAX,key_len))
    return
  else if(ukey(1:4) == 'MIN_' .or. &
          ukey(1:4) == 'LEN_' .or. &
          ukey(1:4) == 'INC_' .or. &
          ukey(1:4) == 'TOT_') then

     select case(ukey(1:1))
     case ("M")  ! Minimu
       ikey = 1
     case ("L")  ! Length
       ikey = 2
     case ("I")  ! Increment
       ikey = 3
     case ("T")  ! Total
       ikey = 4
     end select

     table(1) = "MIN" // ukey(4:)
     table(2) = "LEN" // ukey(4:)
     table(3) = "INC" // ukey(4:)
     table(4) = "TOT" // ukey(4:)
  else if(ukey(key_len-3:key_len) == '_MIN' .or. &
          ukey(key_len-3:key_len) == '_LEN' .or. &
          ukey(key_len-3:key_len) == '_INC' .or. &
          ukey(key_len-3:key_len) == '_TOT') then

     select case(ukey(key_len-2:key_len-2))
     case ("I")  ! mInimum
       ikey = 1
     case ("E")  ! lEngth
       ikey = 2
     case ("N")  ! iNcrement
       ikey = 3
     case ("O")  ! tOtal
       ikey = 4
     end select

     table(1) = ukey(:key_len-3) // "MIN"
     table(2) = ukey(:key_len-3) // "LEN"
     table(3) = ukey(:key_len-3) // "INC"
     table(4) = ukey(:key_len-3) // "TOT"
  else
    msg = "pattern_min_len: keyword not recognized, value=" &
      // ukey(:min(KEY_MAX,key_len))
    return
  end if

  if(present(strt_min)) then
    strt2_min = strt_min
  else
    strt2_min = - huge(strt2_min)
  end if

  if(present(strt_max)) then
    strt2_max = strt_max
  else
    strt2_max = + huge(strt2_max)
  end if

  if(present(len_min)) then
    len2_min = len_min
  else
    len2_min = 0.0
  end if

  if(present(len_max)) then
    len2_max = len_max
  else
    len2_max = + huge(len2_max)
  end if

  if(present(inc_min)) then
    inc2_min = inc_min
  else
    inc2_min = - huge(inc2_min)
  end if

  if(present(inc_max)) then
    inc2_max = inc_max
  else
    inc2_max = + huge(inc2_max)
  end if

  if(present(tot_min)) then
    tot2_min = tot_min
  else
    tot2_min = 0
  end if

  if(present(tot_max)) then
    tot2_max = tot_max
  else
    tot2_max = + huge(tot2_max)
  end if

  call pattern_len(ikey, table, rmin, rlen, rinc, rtot, msg, &
    strt2_min, strt2_max, &
    len2_min, len2_max, &
    inc2_min, inc2_max, &
    tot2_min, tot2_max)

  return
  end subroutine pattern_min_len

!!------------------------ pattern_init_last -------------------------------!!
!!------------------------ pattern_init_last -------------------------------!!
!!------------------------ pattern_init_last -------------------------------!!

subroutine pattern_init_last (mkey,  rinit, rlast, rinc, rtot, msg, &
  strt_min, strt_max, stop_min, stop_max, inc_min, inc_max, tot_min, tot_max)
  implicit none

  character(len=*) :: mkey       ! argument
  real :: rinit                 ! argument
  real :: rlast                 ! argument
  real :: rinc                  ! argument
  integer :: rtot               ! argument
  character(len=*) :: msg       ! argument

  real, optional :: strt_min    ! argument
  real, optional :: strt_max    ! argument
  real, optional :: stop_min    ! argument
  real, optional :: stop_max    ! argument
  real, optional :: inc_min     ! argument
  real, optional :: inc_max     ! argument
  integer, optional :: tot_min  ! argument
  integer, optional :: tot_max  ! argument

  real :: strt2_min    ! local
  real :: strt2_max    ! local
  real :: stop2_min    ! local
  real :: stop2_max    ! local
  real :: inc2_min     ! local
  real :: inc2_max     ! local
  integer :: tot2_min  ! local
  integer :: tot2_max  ! local

  integer :: key_len      ! local
  integer :: ikey         ! local
  integer :: under_score  ! local

  character(len=KEY_MAX) :: ukey    ! local
  character(len=KEY_MAX), dimension(4) :: table  ! local

  ukey = mkey
  call string_to_upper(ukey)
  key_len = len_trim(ukey)

  if(key_len < 5) then
    msg = "pattern_init_last: keyword too short, value=" // ukey
    return
  else if(key_len >= KEY_MAX) then
    msg = "pattern_init_last: keyword too long, value=" &
      // ukey(:min(KEY_MAX,key_len))
    return
  else if(ukey(1:5) == 'INIT_' .or. &
          ukey(1:5) == 'LAST_' .or. &
          ukey(1:4) == 'INC_' .or. &
          ukey(1:4) == 'TOT_') then

     select case(ukey(3:3))
     case ("I")  ! inItial
       ikey = 1
     case ("S")  ! laSt
       ikey = 2
     case ("C")  ! inCrement
       ikey = 3
     case ("T")  ! toTal
       ikey = 4
     end select

     under_score = index(ukey, "_")

     table(1) = "INIT" // ukey(under_score:)
     table(2) = "LAST" // ukey(under_score:)
     table(3) = "INC" // ukey(under_score:)
     table(4) = "TOT" // ukey(under_score:)
  else if(ukey(key_len-4:key_len) == '_INIT' .or. &
          ukey(key_len-4:key_len) == '_LAST' .or. &
          ukey(key_len-3:key_len) == '_INC' .or. &
          ukey(key_len-3:key_len) == '_TOT') then

     select case(ukey(key_len-1:key_len-1))
     case ("I")  ! inItial
       ikey = 1
     case ("S")  ! laSt
       ikey = 2
     case ("N")  ! iNcrement
       ikey = 3
     case ("O")  ! tOtal
       ikey = 4
     end select

     under_score = index(ukey, "_", .true.)

     table(1) = ukey(:under_score) // "INIT"
     table(2) = ukey(:under_score) // "LAST"
     table(3) = ukey(:under_score) // "INC"
     table(4) = ukey(:under_score) // "TOT"
  else
    msg = "pattern_init_last: keyword not recognized, value=" &
      // ukey(:min(KEY_MAX,key_len))
    return
  end if

  if(present(strt_min)) then
    strt2_min = strt_min
  else
    strt2_min = - huge(strt2_min)
  end if

  if(present(strt_max)) then
    strt2_max = strt_max
  else
    strt2_max = + huge(strt2_max)
  end if

  if(present(stop_min)) then
    stop2_min = stop_min
  else
    stop2_min = - huge(stop2_min)
  end if

  if(present(stop_max)) then
    stop2_max = stop_max
  else
    stop2_max = + huge(stop2_max)
  end if

  if(present(inc_min)) then
    inc2_min = inc_min
  else
    inc2_min = - huge(inc2_min)
  end if

  if(present(inc_max)) then
    inc2_max = inc_max
  else
    inc2_max = + huge(inc2_max)
  end if

  if(present(tot_min)) then
    tot2_min = tot_min
  else
    tot2_min = 0
  end if

  if(present(tot_max)) then
    tot2_max = tot_max
  else
    tot2_max = + huge(tot2_max)
  end if

  call pattern_stop(ikey, table, rinit, rlast, rinc, rtot, msg, &
    strt2_min, strt2_max, &
    stop2_min, stop2_max, &
    inc2_min, inc2_max, &
    tot2_min, tot2_max)

  return
  end subroutine pattern_init_last

!!------------------------ pattern_init_len -------------------------------!!
!!------------------------ pattern_init_len -------------------------------!!
!!------------------------ pattern_init_len -------------------------------!!

subroutine pattern_init_len (mkey,  rinit, rlen, rinc, rtot, msg, &
  strt_min, strt_max, len_min, len_max, inc_min, inc_max, tot_min, tot_max)
  implicit none

  character(len=*) :: mkey       ! argument
  real :: rinit                 ! argument
  real :: rlen                  ! argument
  real :: rinc                  ! argument
  integer :: rtot               ! argument
  character(len=*) :: msg       ! argument

  real, optional :: strt_min    ! argument
  real, optional :: strt_max    ! argument
  real, optional :: len_min     ! argument
  real, optional :: len_max     ! argument
  real, optional :: inc_min     ! argument
  real, optional :: inc_max     ! argument
  integer, optional :: tot_min  ! argument
  integer, optional :: tot_max  ! argument

  real :: strt2_min    ! local
  real :: strt2_max    ! local
  real :: len2_min     ! local
  real :: len2_max     ! local
  real :: inc2_min     ! local
  real :: inc2_max     ! local
  integer :: tot2_min  ! local
  integer :: tot2_max  ! local

  integer :: key_len      ! local
  integer :: ikey         ! local
  integer :: under_score  ! local

  character(len=KEY_MAX) :: ukey    ! local
  character(len=KEY_MAX), dimension(4) :: table  ! local

  ukey = mkey
  call string_to_upper(ukey)
  key_len = len_trim(ukey)

  if(key_len < 4) then
    msg = "pattern_init_len: keyword too short, value=" // ukey
    return
  else if(key_len >= KEY_MAX) then
    msg = "pattern_init_len: keyword too long, value=" &
      // ukey(:min(KEY_MAX,key_len))
    return
  else if(ukey(1:5) == 'INIT_' .or. &
          ukey(1:4) == 'LEN_' .or. &
          ukey(1:4) == 'INC_' .or. &
          ukey(1:4) == 'TOT_') then

     select case(ukey(3:3))
     case ("I")  ! inIt
       ikey = 1
     case ("N")  ! leNgth
       ikey = 2
     case ("C")  ! inCrement
       ikey = 3
     case ("T")  ! toTal
       ikey = 4
     end select

     under_score = index(ukey, "_")

     table(1) = "INIT" // ukey(under_score:)
     table(2) = "LEN" // ukey(under_score:)
     table(3) = "INC" // ukey(under_score:)
     table(4) = "TOT" // ukey(under_score:)
  else if(ukey(key_len-4:key_len) == '_INIT' .or. &
          ukey(key_len-3:key_len) == '_LEN' .or. &
          ukey(key_len-3:key_len) == '_INC' .or. &
          ukey(key_len-3:key_len) == '_TOT') then

     select case(ukey(key_len-1:key_len-1))
     case ("I")  ! inItial
       ikey = 1
     case ("E")  ! lEngth
       ikey = 2
     case ("N")  ! iNcrement
       ikey = 3
     case ("O")  ! tOtal
       ikey = 4
     end select

     under_score = index(ukey, "_", .true.)

     table(1) = ukey(:under_score) // "INIT"
     table(2) = ukey(:under_score) // "LEN"
     table(3) = ukey(:under_score) // "INC"
     table(4) = ukey(:under_score) // "TOT"
  else
    msg = "pattern_init_len: keyword not recognized, value=" &
      // ukey(:min(KEY_MAX,key_len))
    return
  end if

  if(present(strt_min)) then
    strt2_min = strt_min
  else
    strt2_min = - huge(strt2_min)
  end if

  if(present(strt_max)) then
    strt2_max = strt_max
  else
    strt2_max = + huge(strt2_max)
  end if

  if(present(len_min)) then
    len2_min = len_min
  else
    len2_min = 0.0
  end if

  if(present(len_max)) then
    len2_max = len_max
  else
    len2_max = + huge(len2_max)
  end if

  if(present(inc_min)) then
    inc2_min = inc_min
  else
    inc2_min = - huge(inc2_min)
  end if

  if(present(inc_max)) then
    inc2_max = inc_max
  else
    inc2_max = + huge(inc2_max)
  end if

  if(present(tot_min)) then
    tot2_min = tot_min
  else
    tot2_min = 0
  end if

  if(present(tot_max)) then
    tot2_max = tot_max
  else
    tot2_max = + huge(tot2_max)
  end if

  call pattern_len(ikey, table, rinit, rlen, rinc, rtot, msg, &
    strt2_min, strt2_max, &
    len2_min, len2_max, &
    inc2_min, inc2_max, &
    tot2_min, tot2_max)

  return
  end subroutine pattern_init_len

!!------------------------ pattern_stop -------------------------------!!
!!------------------------ pattern_stop -------------------------------!!
!!------------------------ pattern_stop -------------------------------!!

subroutine pattern_stop (ikey, table,  rstrt, rstop, rinc, rtot, msg, &
  strt_min, strt_max, stop_min, stop_max, inc_min, inc_max, tot_min, tot_max)
  implicit none

  integer :: ikey               ! argument
  character(len=*), dimension(4) :: table  ! argument
  real :: rstrt                 ! argument
  real :: rstop                 ! argument
  real :: rinc                  ! argument
  integer :: rtot               ! argument
  character(len=*) :: msg       ! argument

  real :: strt_min    ! argument
  real :: strt_max    ! argument
  real :: stop_min    ! argument
  real :: stop_max    ! argument
  real :: inc_min     ! argument
  real :: inc_max     ! argument
  integer :: tot_min  ! argument
  integer :: tot_max  ! argument

  real :: new_rstop
  integer :: new_rtot
  character(len=16) :: limit_str  ! local
  character(len=16) :: value_str  ! local

  msg = " "  ! assume no error, until refuted

  if(ikey == 2) then
    ! END/MAX/LAST has changed, update TOTal (and tweak END/MAX/LAST)
    new_rtot = 1 + nint((rstop - rstrt) / rinc)
    new_rstop = rstrt + (new_rtot - 1) * rinc ! tweak END/MAX/LAST
  else
    ! BEG/MIN/INIT, INC or TOTal has changed, update END/MAX/LAST
    new_rtot = rtot
    new_rstop = rstrt + (rtot - 1) * rinc
  end if

  ! check for fundamental limit errors

  if(rstrt < strt_min) then
    call string_ff2cc(rstrt, value_str)
    call string_ff2cc(strt_min, limit_str)

    msg = "pattern: " // trim(table(1)) // " (" // trim(value_str) &
      // ") is less than minimum (" // trim(limit_str) // ")"
  else if(rstrt > strt_max) then
    call string_ff2cc(rstrt, value_str)
    call string_ff2cc(strt_max, limit_str)

    msg = "pattern: " // trim(table(1)) // " (" // trim(value_str) &
      // ") is greater than maximum (" // trim(limit_str) // ")"
  else if(rstop < stop_min) then
    call string_ff2cc(rstop, value_str)
    call string_ff2cc(stop_min, limit_str)

    msg = "pattern: " // trim(table(2)) // " (" // trim(value_str) &
      // ") is less than minimum (" // trim(limit_str) // ")"
  else if(rstop > stop_max) then
    call string_ff2cc(rstop, value_str)
    call string_ff2cc(stop_max, limit_str)

    msg = "pattern: " // trim(table(2)) // " (" // trim(value_str) &
      // ") is greater than maximum (" // trim(limit_str) // ")"
  else if(rinc < inc_min) then
    call string_ff2cc(rinc, value_str)
    call string_ff2cc(inc_min, limit_str)

    msg = "pattern: " // trim(table(3)) // " (" // trim(value_str) &
      // ") is less than minimum (" // trim(limit_str) // ")"
  else if(rinc > inc_max) then
    call string_ff2cc(rinc, value_str)
    call string_ff2cc(inc_max, limit_str)

    msg = "pattern: " // trim(table(3)) // " (" // trim(value_str) &
      // ") is greater than maximum (" // trim(limit_str) // ")"
  else if(rtot < tot_min) then
    call string_ii2cc(rtot, value_str)
    call string_ii2cc(tot_min, limit_str)

    msg = "pattern: " // trim(table(4)) // " (" // trim(value_str) &
      // ") is less than minimum (" // trim(limit_str) // ")"
  else if(rtot > tot_max) then
    call string_ii2cc(rtot, value_str)
    call string_ii2cc(tot_max, limit_str)

    msg = "pattern: " // trim(table(4)) // " (" // trim(value_str) &
      // ") is greater than maximum (" // trim(limit_str) // ")"
  end if

  if(msg(1:1) == ' ') then
    rtot = new_rtot
    rstop = new_rstop
  end if

  return
  end subroutine pattern_stop

!!------------------------ pattern_len -------------------------------!!
!!------------------------ pattern_len -------------------------------!!
!!------------------------ pattern_len -------------------------------!!

subroutine pattern_len (ikey, table,  rstrt, rlen, rinc, rtot, msg, &
  strt_min, strt_max, len_min, len_max, inc_min, inc_max, tot_min, tot_max)
  implicit none

  integer :: ikey               ! argument
  character(len=*), dimension(4) :: table  ! argument
  real :: rstrt                 ! argument
  real :: rlen                  ! argument
  real :: rinc                  ! argument
  integer :: rtot               ! argument
  character(len=*) :: msg       ! argument

  real :: strt_min    ! argument
  real :: strt_max    ! argument
  real :: len_min    ! argument
  real :: len_max    ! argument
  real :: inc_min     ! argument
  real :: inc_max     ! argument
  integer :: tot_min  ! argument
  integer :: tot_max  ! argument

  real :: new_rlen
  integer :: new_rtot
  character(len=16) :: limit_str  ! local
  character(len=16) :: value_str  ! local

  msg = " "  ! assume no error, until refuted

  if(ikey == 2) then
    ! LEN has changed, update TOTal (and tweak LEN)
    new_rtot = 1 + abs(nint(rlen / rinc))
    new_rlen = abs((new_rtot - 1) * rinc) ! tweak LEN
  else
    ! BEG/MIN/INIT, INC or TOTal has changed, update LEN
    new_rtot = rtot
    new_rlen = abs((rtot - 1) * rinc)
  end if

  ! check for fundamental limit errors

  if(rstrt < strt_min) then
    call string_ff2cc(rstrt, value_str)
    call string_ff2cc(strt_min, limit_str)

    msg = "pattern: " // trim(table(1)) // " (" // trim(value_str) &
      // ") is less than minimum (" // trim(limit_str) // ")"
  else if(rstrt > strt_max) then
    call string_ff2cc(rstrt, value_str)
    call string_ff2cc(strt_max, limit_str)

    msg = "pattern: " // trim(table(1)) // " (" // trim(value_str) &
      // ") is greater than maximum (" // trim(limit_str) // ")"
  else if(rlen < len_min) then
    call string_ff2cc(rlen, value_str)
    call string_ff2cc(len_min, limit_str)

    msg = "pattern: " // trim(table(2)) // " (" // trim(value_str) &
      // ") is less than minimum (" // trim(limit_str) // ")"
  else if(rlen > len_max) then
    call string_ff2cc(rlen, value_str)
    call string_ff2cc(len_max, limit_str)

    msg = "pattern: " // trim(table(2)) // " (" // trim(value_str) &
      // ") is greater than maximum (" // trim(limit_str) // ")"
  else if(rinc < inc_min) then
    call string_ff2cc(rinc, value_str)
    call string_ff2cc(inc_min, limit_str)

    msg = "pattern: " // trim(table(3)) // " (" // trim(value_str) &
      // ") is less than minimum (" // trim(limit_str) // ")"
  else if(rinc > inc_max) then
    call string_ff2cc(rinc, value_str)
    call string_ff2cc(inc_max, limit_str)

    msg = "pattern: " // trim(table(3)) // " (" // trim(value_str) &
      // ") is greater than maximum (" // trim(limit_str) // ")"
  else if(rtot < tot_min) then
    call string_ii2cc(rtot, value_str)
    call string_ii2cc(tot_min, limit_str)

    msg = "pattern: " // trim(table(4)) // " (" // trim(value_str) &
      // ") is less than minimum (" // trim(limit_str) // ")"
  else if(rtot > tot_max) then
    call string_ii2cc(rtot, value_str)
    call string_ii2cc(tot_max, limit_str)

    msg = "pattern: " // trim(table(4)) // " (" // trim(value_str) &
      // ") is greater than maximum (" // trim(limit_str) // ")"
  end if

  if(msg(1:1) == ' ') then
    rtot = new_rtot
    rlen = new_rlen
  end if

  return
  end subroutine pattern_len

!!-------------------------- pattern_verify -------------------------------!!
!!-------------------------- pattern_verify -------------------------------!!
!!-------------------------- pattern_verify -------------------------------!!

      function pattern_verify( cnt, msg, &
        old_start, old_inc, old_stop, old_cnt, &
        new_start, new_inc, new_stop, new_cnt)

      logical                               :: pattern_verify  ! return value
      integer, intent(in)                   :: cnt        ! argument
      character(len=*), intent(in)          :: msg        ! argument
      real, intent(in)                      :: old_start  ! argument
      real, intent(in)                      :: old_inc    ! argument
      real, intent(in)                      :: old_stop   ! argument
      integer, intent(in)                   :: old_cnt    ! argument
      real, intent(inout)                   :: new_start  ! argument
      real, intent(inout)                   :: new_inc    ! argument
      real, intent(inout)                   :: new_stop   ! argument
      integer, intent(inout)                :: new_cnt    ! argument

      pattern_verify = .false.  ! assume invalid, until refuted

      if(cnt < 3) then
        call pc_warning(msg // &
          ', 3 of 4 required (start, increment, stop, count)')
      else if(cnt == 3 .or. &
        0 /= mth_compare(old_start, new_start) .or. &
        0 /= mth_compare(old_inc, new_inc) .or. &
        0 /= mth_compare(old_stop, new_stop)) then

        ! compute new_cnt, given other parameters

        if(new_inc == 0.0 .and. new_start /= new_stop) then
          call pc_error(msg // ', increment must be non-zero')
        else
          if(new_inc == 0.0) then
            new_cnt = 1
          else
            new_cnt = anint((new_stop - new_start) / new_inc)

            if(new_cnt < 0) then
              ! positive count required
              new_cnt = 1 - new_cnt
              new_inc = - new_inc
              call pc_warning(msg // ', reversing sign of increment')
            else
              new_cnt = 1 + new_cnt
            end if
          endif

          pattern_verify = .true.
        end if
      else if(0 /= mth_compare(old_start, new_start) .or. &
        0 /= mth_compare(old_inc, new_inc) .or. &
        old_cnt /= new_cnt) then

        ! compute new_stop, given other parameters

        if(new_inc == 0.0) then
          call pc_error(msg // ', increment must be non-zero')
        else if(new_cnt < 1) then
          call pc_error(msg // ', count/number must be greater than zero')
        else
          new_stop = new_start + (new_cnt - 1) * new_inc
          pattern_verify = .true.
        end if
      else if(0 /= mth_compare(old_start, new_start) .or. &
        0 /= mth_compare(old_stop, new_stop) .or. &
        old_cnt /= new_cnt) then

        ! compute new_inc, given other parameters

        if(new_cnt < 1) then
          call pc_error(msg // ', count/number must be greater than zero')
        else
          if(new_cnt > 1) then
            new_inc = (new_stop - new_start) / (new_cnt - 1)
          end if

          pattern_verify = .true.
        end if
      else if(0 /= mth_compare(old_stop, new_stop) .or. &
        0 /= mth_compare(old_inc, new_inc) .or. &
        old_cnt /= new_cnt) then

        ! compute new_start, given other parameters

        if(new_inc == 0.0) then
          call pc_error(msg // ', increment must be non-zero')
        else if(new_cnt < 1) then
          call pc_error(msg // ', count/number must be greater than zero')
        else
          new_start = new_stop - (new_cnt - 1) * new_inc
          pattern_verify = .true.
        end if
      else if(0 == mth_compare((new_cnt - 1) * new_inc, &
        (new_stop - new_start))) then
        pattern_verify = .true.
      else
        call pc_error(msg // ', inconsistant start, inc, stop, count')
      end if

      return
      end function pattern_verify

!!-------------------------- pattern_stop2 -----------------------------!!
!!-------------------------- pattern_stop2 -----------------------------!!
!!-------------------------- pattern_stop2 -----------------------------!!

  function pattern_stop2(msg, verify, &
    start_val, inc_val, stop_val, total_val, &
    start_key, inc_key, stop_key, total_key, &
    start_ver, inc_ver, stop_ver, total_ver, &
    start_min, inc_min, stop_min, total_min, &
    start_max, inc_max, stop_max, total_max)

  integer :: pattern_stop2       ! return value
  character(len=*) :: msg        ! argument
  logical :: verify              ! argument
  real ::    start_val           ! argument
  real ::    inc_val             ! argument
  real ::    stop_val            ! argument
  integer :: total_val           ! argument
  character(len=*) :: start_key  ! argument
  character(len=*) :: inc_key    ! argument
  character(len=*) :: stop_key   ! argument
  character(len=*) :: total_key  ! argument
  logical :: start_ver           ! argument
  logical :: inc_ver             ! argument
  logical :: stop_ver            ! argument
  logical :: total_ver           ! argument
  real, optional ::    start_min ! argument
  real, optional ::    inc_min   ! argument
  real, optional ::    stop_min  ! argument
  integer, optional :: total_min ! argument
  real, optional ::    start_max ! argument
  real, optional ::    inc_max   ! argument
  real, optional ::    stop_max  ! argument
  integer, optional :: total_max ! argument

  real :: start_min2, inc_min2, stop_min2  ! local
  integer :: total_min2                    ! local

  real :: start_max2, inc_max2, stop_max2  ! local
  integer :: total_max2                    ! local

  double precision :: double_total  ! local

  real, parameter :: tiny_inc = 1e-20 ! parameter
  real :: test, epsilon

  !! assume success
  pattern_stop2 = 0

  !!****************************
  !!***  default min values  ***
  !!****************************

  if(present(start_min)) then
    start_min2 = start_min
  else
    start_min2 = - huge(start_min2)
  end if

  if(present(inc_min)) then
    inc_min2 = inc_min
  else
    inc_min2 = - huge(inc_min2)
  end if

  if(present(stop_min)) then
    stop_min2 = stop_min
  else
    stop_min2 = - huge(stop_min2)
  end if

  if(present(total_min)) then
    total_min2 = max(0, total_min)
  else
    total_min2 = 1
  end if

  !!****************************
  !!***  default max values  ***
  !!****************************

  if(present(start_max)) then
    start_max2 = start_max
  else
    start_max2 = + huge(start_max2)
  end if

  if(present(inc_max)) then
    inc_max2 = inc_max
  else
    inc_max2 = + huge(inc_max2)
  end if

  if(present(stop_max)) then
    stop_max2 = stop_max
  else
    stop_max2 = + huge(stop_max2)
  end if

  if(present(total_max)) then
    total_max2 = total_max
  else
    total_max2 = + huge(total_max2)
  end if

  !!***********************************************
  !!***  Recompute values as needed.            ***
  !!***    total_val (highest precedence)       ***
  !!***    stop_val                             ***
  !!***    inc_val   (lowest precedence)        ***
  !!***    start_val (FNIL changed to zero iff  ***
  !!***      stop_val or inc_val is updated.    ***
  !!***********************************************

  if(.not. total_ver .and. &
    ((inc_val > + tiny_inc .and. start_val <= stop_val) .or. &
     (inc_val < - tiny_inc .and. start_val >= stop_val)) .and. &
    (inc_val > + tiny_inc .or. inc_val < - tiny_inc) .and. &
    inc_val /= FNIL .and. &
    start_val /= FNIL .and. stop_val /= FNIL) then
    ! attempt to recompute total

    ! Double precision float is needed to preserve 32 bit integer values
    double_total = nint(1.0 + &
      (real(stop_val,  kind(double_total)) - &
       real(start_val, kind(double_total))) / inc_val)

    if(double_total >= total_min2 .and. double_total <= total_max2) then
      total_val = double_total

      ! rationalize the stop value
      stop_val = start_val + real(inc_val, kind(double_total)) * (total_val - 1)
    end if
  else if(.not. stop_ver .and. &
    (inc_val > + tiny_inc .or. inc_val < - tiny_inc) .and. &
    inc_val /= FNIL .and. &
    total_val > 0) then
    ! recompute stop

    if(start_val == FNIL) start_val = 0.0

    stop_val = start_val + real(inc_val, kind(double_total)) * (total_val - 1)
  else if(.not. inc_ver .and. &
    stop_val /= FNIL .and. &
    total_val > 1) then
    ! recompute increment

    if(start_val == FNIL) start_val = 0.0

    inc_val = (real(stop_val,  kind(double_total)) - &
               real(start_val, kind(double_total))) / (total_val - 1)
  else if(total_val == 0 .and. &
    0 >= total_min2 .and. 0 <= total_max2) then
    ! force stop_val to be consistant with a zero total
    stop_val = FNIL
  else if(verify .and. total_val >= total_min2) then
    test = start_val + inc_val * (total_val - 1)
    epsilon = 0.49999 * abs(inc_val)

    if(stop_val < test - epsilon .or. stop_val > test + epsilon) then
      call pc_error(msg // ' inconsistant ' // start_key // ', ' // &
        inc_key // ', ' // stop_key // ', ' // total_key)
      pattern_stop2 = 1
    end if
  end if

  !!*********************************
  !!***  Verify values as needed  ***
  !!*********************************

  if(verify .and. (total_min2 > 0 .or. total_val > 0)) then
    if(start_val == FNIL) then
      call pc_error(msg // ' ' // start_key // ' must be defined')
      pattern_stop2 = 1
    else if(start_val < start_min2) then
      call pc_error(msg // ' ' // start_key // ' is less than ', start_min2)
      pattern_stop2 = 1
    else if(start_val > start_max2) then
      call pc_error(msg // ' ' // start_key // ' is greater than ', start_max2)
      pattern_stop2 = 1
    end if
  
    if(inc_val == FNIL) then
      call pc_error(msg // ' ' // inc_key // ' must be defined')
      pattern_stop2 = 1
    else if(inc_val < inc_min2) then
      call pc_error(msg // ' ' // inc_key // ' is less than ', inc_min2)
      pattern_stop2 = 1
    else if(inc_val > inc_max2) then
      call pc_error(msg // ' ' // inc_key // ' is greater than ', inc_max2)
      pattern_stop2 = 1
    end if
  
    if(stop_val == FNIL) then
      call pc_error(msg // ' ' // stop_key // ' must be defined')
      pattern_stop2 = 1
    else if(stop_val < stop_min2) then
      call pc_error(msg // ' ' // stop_key // ' is less than ', stop_min2)
      pattern_stop2 = 1
    else if(stop_val > stop_max2) then
      call pc_error(msg // ' ' // stop_key // ' is greater than ', stop_max2)
      pattern_stop2 = 1
    end if
  
    if(total_val == FNIL) then
      call pc_error(msg // ' ' // total_key // ' must be defined')
      pattern_stop2 = 1
    else if(total_val < total_min2) then
      call pc_error(msg // ' ' // total_key // ' is less than ', total_min2)
      pattern_stop2 = 1
    else if(total_val > total_max2) then
      call pc_error(msg // ' ' // total_key // ' is greater than ', total_max2)
      pattern_stop2 = 1
    end if
  end if

  return
  end function pattern_stop2
  
!!-------------------------- pattern_len2 -----------------------------!!
!!-------------------------- pattern_len2 -----------------------------!!
!!-------------------------- pattern_len2 -----------------------------!!

  function pattern_len2(msg, verify, &
    start_val, inc_val, len_val, total_val, &
    start_key, inc_key, len_key, total_key, &
    start_ver, inc_ver, len_ver, total_ver, &
    start_min, inc_min, len_min, total_min, &
    start_max, inc_max, len_max, total_max)

  integer :: pattern_len2        ! return value
  character(len=*) :: msg        ! argument
  logical :: verify              ! argument
  real ::    start_val           ! argument
  real ::    inc_val             ! argument
  real ::    len_val             ! argument
  integer :: total_val           ! argument
  character(len=*) :: start_key  ! argument
  character(len=*) :: inc_key    ! argument
  character(len=*) :: len_key    ! argument
  character(len=*) :: total_key  ! argument
  logical :: start_ver           ! argument
  logical :: inc_ver             ! argument
  logical :: len_ver             ! argument
  logical :: total_ver           ! argument
  real, optional ::    start_min ! argument
  real, optional ::    inc_min   ! argument
  real, optional ::    len_min   ! argument
  integer, optional :: total_min ! argument
  real, optional ::    start_max ! argument
  real, optional ::    inc_max   ! argument
  real, optional ::    len_max   ! argument
  integer, optional :: total_max ! argument

  real :: start_min2, inc_min2, len_min2   ! local
  integer :: total_min2                    ! local

  real :: start_max2, inc_max2, len_max2   ! local
  integer :: total_max2                    ! local

  double precision :: double_total  ! local

  real, parameter :: tiny_inc = 1e-20 ! parameter
  real :: test, epsilon

  !! assume success
  pattern_len2 = 1

  !!****************************
  !!***  default min values  ***
  !!****************************

  if(present(start_min)) then
    start_min2 = start_min
  else
    start_min2 = - huge(start_min2)
  end if

  if(present(inc_min)) then
    inc_min2 = max(0.0,inc_min)
  else
    inc_min2 = 0.0
  end if

  if(present(len_min)) then
    len_min2 = max(0.0,len_min)
  else
    len_min2 = 0.0
  end if

  if(present(total_min)) then
    total_min2 = max(0, total_min)
  else
    total_min2 = 1
  end if

  !!****************************
  !!***  default max values  ***
  !!****************************

  if(present(start_max)) then
    start_max2 = start_max
  else
    start_max2 = + huge(start_max2)
  end if

  if(present(inc_max)) then
    inc_max2 = inc_max
  else
    inc_max2 = + huge(inc_max2)
  end if

  if(present(len_max)) then
    len_max2 = len_max
  else
    len_max2 = + huge(len_max2)
  end if

  if(present(total_max)) then
    total_max2 = total_max
  else
    total_max2 = + huge(total_max2)
  end if

  !!***********************************************
  !!***  Recompute values as needed.            ***
  !!***    total_val (highest precedence)       ***
  !!***    len_val                              ***
  !!***    inc_val   (lowest precedence)        ***
  !!***    start_val (FNIL changed to zero iff  ***
  !!***      len_val or inc_val is updated.     ***
  !!***********************************************

  if(.not. total_ver .and. &
    (inc_val > + tiny_inc .or. inc_val < - tiny_inc) .and. &
    inc_val /= FNIL .and. &
    start_val /= FNIL .and. len_val /= FNIL) then
    ! attempt to recompute total

    ! Double precision float is needed to preserve 32 bit integer values
    double_total = nint(1.0 + &
      real((len_val), kind(double_total)) / inc_val)

    if(double_total >= total_min2 .and. double_total <= total_max2) then
      total_val = double_total

      if(start_val /= FNIL) then
        ! rationalize the len value
        len_val = real(inc_val, kind(double_total)) * (total_val - 1)
      end if
    end if
  else if(.not. len_ver .and. &
    (inc_val > + tiny_inc .or. inc_val < - tiny_inc) .and. &
    inc_val /= FNIL .and. &
    total_val > 0) then
    ! recompute len

    if(start_val == FNIL) start_val = max(0.0, start_min2)

    len_val = real(inc_val, kind(double_total)) * (total_val - 1)
  else if(.not. inc_ver .and. &
    len_val /= FNIL .and. &
    total_val > 1) then
    ! recompute increment

    if(start_val == FNIL) start_val = max(0.0, start_min2)

    inc_val = real(len_val, kind(double_total)) / (total_val - 1)
  else if(total_val == 0 .and. &
    0 >= total_min2 .and. 0 <= total_max2) then
    ! force len_val to be consistant with a zero total
    len_val = FNIL
  else if(verify .and. total_val >= total_min2) then
    test = inc_val * (total_val - 1)
    epsilon = 0.49999 * abs(inc_val)

    if(len_val < test - epsilon .or. len_val > test + epsilon) then
      call pc_error(msg // ' inconsistant ' // start_key // ', ' // &
        inc_key // ', ' // len_key // ', ' // total_key)
      pattern_len2 = 1
    end if
  end if

  !!*********************************
  !!***  Verify values as needed  ***
  !!*********************************

  if(verify .and. (total_min2 > 0 .or. total_val > 0)) then
    if(start_val == FNIL) then
      call pc_error(msg // ' ' // start_key // ' must be defined')
      pattern_len2 = 1
    else if(start_val < start_min2) then
      call pc_error(msg // ' ' // start_key // ' is less than ', start_min2)
      pattern_len2 = 1
    else if(start_val > start_max2) then
      call pc_error(msg // ' ' // start_key // ' is greater than ', start_max2)
      pattern_len2 = 1
    end if
  
    if(inc_val == FNIL) then
      call pc_error(msg // ' ' // inc_key // ' must be defined')
      pattern_len2 = 1
    else if(inc_val < inc_min2) then
      call pc_error(msg // ' ' // inc_key // ' is less than ', inc_min2)
      pattern_len2 = 1
    else if(inc_val > inc_max2) then
      call pc_error(msg // ' ' // inc_key // ' is greater than ', inc_max2)
      pattern_len2 = 1
    end if
  
    if(len_val == FNIL) then
      call pc_error(msg // ' ' // len_key // ' must be defined')
      pattern_len2 = 1
    else if(len_val < len_min2) then
      call pc_error(msg // ' ' // len_key // ' is less than ', len_min2)
      pattern_len2 = 1
    else if(len_val > len_max2) then
      call pc_error(msg // ' ' // len_key // ' is greater than ', len_max2)
      pattern_len2 = 1
    end if
  
    if(total_val == FNIL) then
      call pc_error(msg // ' ' // total_key // ' must be defined')
      pattern_len2 = 1
    else if(total_val < total_min2) then
      call pc_error(msg // ' ' // total_key // ' is less than ', total_min2)
      pattern_len2 = 1
    else if(total_val > total_max2) then
      call pc_error(msg // ' ' // total_key // ' is greater than ', total_max2)
      pattern_len2 = 1
    end if
  end if

  return
  end function pattern_len2
  
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module pattern_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

