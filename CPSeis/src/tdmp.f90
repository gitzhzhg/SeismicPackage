!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 1999-09-21. />

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
! Name       : TDMP  (Trace DuMP)
! Category   : diagnostics
! Written    : 1986-04-08   by: Mike Howard
! Revised    : 2011-06-09   by: B. Menger
! Maturity   : production
! Purpose    : Dump (print) headers and/or trace samples of selected traces.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! TDMP dumps (prints) header word values for input traces and optionally dumps
! input trace sample values.  Options are provided for trace selection,
! selection of header words to dump, number of columns to use in dumping each
! header and whether to dump trace samples as well as headers, among others.
!
! Headers selected to be dumped have the number of columns to be used in the
! dump shown in the HEADERS array.  Zero entries in HEADERS correspond to
! headers not being dumped.
!
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
! If the user frequently finds that none of the default header selections is
! appropriate, then storing a user default for TDMP may be indicated.
!
! To avoid wrap-around in TDUMP display rows, It may be desirable to split the
! desired headers among two or more TDUMP processes in the job.
!
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
! This process does not alter input traces.
! This process outputs the same traces as it receives.
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NWIH     number of words in trace header       may be used
! NDPT     number of sample values in trace      may be used
!
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! all     all                        any or all may be dumped
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!      Date       Author     Description
!      ----       ------     -----------
!  35  2011-06-09 B. Menger  Removed fortran I/O and replaced with cpsio.
!034. 2006-06-12  B. Menger   Removed Unused Variables.
!  33. 2001-11-05 Selzler    Corrected initialized size of headers array.
!  32. 2001-08-16 Selzler    Eliminated fixed upper limit on hdr_size and
!                            dependence upon HDR_SIZE_KLUDGE
!  31. 2000-12-11 Selzler    Changed wrapup logic to use skip_wrapup
!  30. 2000-10-04 Selzler    Right justified column headers per user request
!  29. 2000-09-27 Selzler    Right justified printout per user request
!  28. 2000-07-07 Selzler    Fixed problems found by CPS Fortran Code Review.
!  27. 2000-03-27 Selzler    Changed default for USER mode
!  26. 2000-02-10 Selzler    Corrected bug in wrapup logic
!  25. 2000-02-09 Selzler    synchronized source with CIB's latest newdoc.
!  24. 2000-02-07 Selzler    improved gui support
!  23. 2000-02-02 Selzler    Added support for GUI and general cleanup
!  22. 2000-01-19 Selzler    improved tabular format and removed KLUDGE
!  21. 1999-12-07 Selzler    Fixed bug with HDR_SIZE intialization
!  20. 1999-11-30 Selzler    Fixed bug with dump_lun wrapup
!  19. 1999-11-20 Selzler    Added RCS "Id" strings to tag executeable
!  18. 1999-09-14 Selzler    Work around absoft bug (HDR_SIZE_KLUDGE)
!  17. 1999-09-13 Selzler    Updated skip_wrapup and print_lun usage
!  16. 1999-08-26 Selzler    Bug fix, "ROUND" constant values
!  15. 1999-08-23 Selzler    change headers to double precision
!  14. 1999-08-05 Selzler    Bug fixes.
!  13. 1999-07-26 Selzler    Conversion to fortran90 compiler.
!  12. 1998-11-18 Goodger    Begin using the fortran90 compiler.
!  11. 1995-02-21 Goodger    Add parameter HF#.
!  10. 1990-05-15 Howard     Make internally callable.
!   9. 1989-09-13 Baumel     Print asterisks when number doesn't fit.
!   8. 1989-02-25 Howard     Add multi-line dumps.
!   7. 1988-10-16 Howard     Add trace value dump.
!   6. 1988-10-11 Howard     Add user selected output format.
!   5. 1988-09-26 Howard     NWIH conversion.
!   4. 1988-09-26 Howard     Change to ISKP,NDO,NSKP,TOT
!   3. 1988-04-22 Baumel     Add CPSPRT calls.
!   2. 1987-04-28 Baumel     Write to history file.
!   1. 1986-04-08 Howard     Original version.
!
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
!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS TDMP Process/NC=80>
!
!         Dump (print) headers and/or trace samples of selected traces.
!
!HDR_FLAG=`IIIII
!
!SKIP_INIT=`IIIIIIIIII  NUM_DO=`IIIIIIIIII NUM_SKIP=`IIIIIIIIII TOT_DO=`IIIIIIIIII
!
!OPT_HDR=~~`CCCCC       ROUND= `CC
!
!    Header Values Array
!
!       HEADERS
!       `IIIIIIIIII
!       `IIIIIIIIII
!       `IIIIIIIIII
!       `IIIIIIIIII
!       `IIIIIIIIII
!       `IIIIIIIIII
!       `IIIIIIIIII
!       `IIIIIIIIII
!       `IIIIIIIIII
!       `IIIIIIIIII
!
!NUM_SAM=`FFFFFFFFFFF
!<PARMS HEADERS[/XST/YST]>
!</gui_def>
!<HelpSection>

!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! If HDR_FLAG = 0, then all traces are dumped.  Otherwise, only traces with
! a flag set in header word HDR_FLAG are dumped.
!
!</Help>

!<Help KEYWORD="SKIP_INIT">
!<Tip> Number of traces to skip initially in the DO-SKIP selection. </Tip>
! Default = 0
! Allowed = int>=0
!
! The DO-SKIP trace selection method consists of initially skipping SKIP_INIT
! traces, then sequentially processing ("doing") NUM_DO consecutive traces
! and skipping NUM_SKIP consecutive traces until TOT_DO total traces are
! processed.
!
!</Help>
!
!<Help KEYWORD="NUM_DO">
!<Tip> Number of traces to dump at a time in the DO-SKIP selection. </Tip>
! Default = 1
! Allowed = int>0
!
! The DO-SKIP trace selection method consists of initially skipping SKIP_INIT
! traces, then sequentially processing ("doing") NUM_DO consecutive traces
! and skipping NUM_SKIP consecutive traces until TOT_DO total traces are
! processed.
!
!</Help>
!
!<Help KEYWORD="NUM_SKIP">
!<Tip> Number of traces to skip at a time in the DO-SKIP selection. </Tip>
! Default = 0
! Allowed = int>=0
!
! The DO-SKIP trace selection method consists of initially skipping SKIP_INIT
! traces, then sequentially processing ("doing") NUM_DO consecutive traces
! and skipping NUM_SKIP consecutive traces until TOT_DO total traces are
! processed.
!
!</Help>
!
!<Help KEYWORD="TOT_DO">
!<Tip> Total number of traces to dump in the DO-SKIP selection. </Tip>
! Default = 10
! Allowed = int>0
!
! The DO-SKIP trace selection method consists of initially skipping SKIP_INIT
! traces, then sequentially processing ("doing") NUM_DO consecutive traces
! and skipping NUM_SKIP consecutive traces until TOT_DO total traces are
! processed.
!
!</Help>
!
!<Help KEYWORD="OPT_HDR">
!<Tip> Option for choice of headers to dump (including user-specified). </Tip>
! Default = USER
! Allowed = USER   (User specifies headers to dump in HEADERS array.)
! Allowed = BRIEF  (Brief dump - takes 72 columns)
! Allowed = 2D     (2D-oriented header selection)
! Allowed = GRID   (Grid headers only)
! Allowed = SURVEY (Survey headers only)
! Allowed = STAT   (Statics headers)
! Allowed = SYS    (38 important headers)
!
! OPT_HDR determines which pre-defined selection of headers is dumped or
! whether the user must specify the headers to be dumped in the HEADERS array.
!
! Headers associated with each default selections can be viewed in the HEADERS
! array by choosing the desired default in OPT_HDR.
!
!</Help>
!
!<Help KEYWORD="ROUND">
!<Tip> Option whether to round header values to whole numbers. </Tip>
! Default = NO   (Dependent upon OPT_HDR choice)
! Allowed = YES  (Round header values to whole numbers for dump.)
! Allowed = NO   (Dump actual floating point header values.)
!</Help>
!
!<Help KEYWORD="HEADERS">
!<Tip> Array of header words selected for header dump. </Tip>
! Default = user-specified headers
! Allowed = int>=0 for each header
! Users must specify a non-zero number of columns to be used for each header
! word to be dumped.  Header words not being dumped are indicated by "0" number
! of columns to use.
!
! The dump will use one column more than the indicated number for each header
! dumped.  If floating point values are used, one column will be used by the
! decimal point.
!
! Choices of OPT_HDR other than USER cause the default selections of headers
! to be displayed in the HEADERS array.
!</Help>
!
!<Help KEYWORD="NUM_SAM">
!<Tip> Number of trace sample values to be dumped. </Tip>
! Default = 0
! Allowed = NWPT>=real>=0
! NUM_SAM trace sample values will be dumped, starting with the first non-zero
! trace sample.  (This is in addition to the header dump specified.)
!
! If NUM_SAM = 0 then no trace samples are dumped.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module tdmp_module
      use pc_module
      use mem_module
      use named_constants_module
      use string_module
      use cpsio_module
      use getlun_module
      implicit none
      private
      public :: tdmp_create     ! uses the parameter cache.
      public :: tdmp_initialize
      public :: tdmp_update     ! uses the parameter cache.
      public :: tdmp_delete
!<execute_only>
      public :: tdmp            ! main execution (trace processing) routine.
      public :: tdmp_wrapup
!</execute_only>


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: tdmp_struct
      private
      logical                    :: skip_wrapup   ! dependent parameter

      character(len=6)           :: opt_hdr          ! process parameter
      integer                    :: hdr_flag         ! process parameters.
      integer                    :: skip_init        ! process parameters.
      integer                    :: num_do           ! process parameters.
      integer                    :: num_skip         ! process parameters.
      integer                    :: tot_do           ! process parameters.
      integer                    :: num_sam          ! process parameters.
      character(len=3)           :: round            ! process parameters.
      integer,dimension(:),pointer :: headers        ! process parameters.
                                                     ! D1 = hdr_size

      integer                    :: nwih             ! globals.
      integer                    :: ndpt             ! globals.

      integer                    :: hdr_size         ! maximum hdr to dump
      integer                    :: dump_lun         ! dependent parameters.
                                    ! default = provided by getlun
                                    !       >= 0, temporary file LUN
      integer                    :: n_cnt            ! dependent parameters.
                                    ! default = 0
                                    ! valid >= 0
                                    ! trace skip control counter
      integer                    :: n_out ! dependent parameters.
                                    ! default = 0
                                    ! valid >= 0
                                    ! Number of traces actually printed.
      end type tdmp_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(tdmp_struct),pointer,save :: object      ! needed for traps.

      integer, parameter :: MAX_LINE = 132 ! Maximum number of characters
                                           ! (columns) in one line

      integer :: print_lun = 0             ! state variable
                                           ! = pc_get_lun()

      character(len=100),public :: tdmp_ident = &
        "$Id: tdmp.f90,v 1.34 2006/06/12 13:03:57 Menger prod sps $"

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine tdmp_create (obj)
      implicit none
      type(tdmp_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify(obj%headers)

      call tdmp_initialize (obj)

      return
      end subroutine tdmp_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine tdmp_delete (obj)
      implicit none
      type(tdmp_struct),pointer :: obj       ! arguments

!<execute_only>
      call tdmp_wrapup (obj)
!</execute_only>

      call mem_free(obj%headers)

      deallocate(obj)

      return
      end subroutine tdmp_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine tdmp_initialize (obj)
      implicit none
      type(tdmp_struct),pointer :: obj       ! arguments

      call pc_get_global ('nwih'  , obj%nwih)
      call pc_get_global ('ndpt'  , obj%ndpt)

      obj%opt_hdr = 'USER'
      obj%hdr_flag = 0
      obj%skip_init = 0
      obj%num_do = 1
      obj%num_skip = 0
      obj%tot_do = 10
      obj%num_sam = 0
      obj%round = 'NO'

      call mem_alloc(obj%headers, obj%nwih)

      obj%hdr_size = obj%nwih

      ! reset all headers to INIL (empty).
      obj%headers = INIL

!! The following were the default headers used in the pre-2000 CPS
!!    obj%headers(1) = 6
!!    obj%headers(2) = 4
!!    obj%headers(3) = 6
!!    obj%headers(4) = 3
!!    obj%headers(5) = 4
!!    obj%headers(6) = 5
!!    obj%headers(7) = 4
!!    obj%headers(8) = 4
!!    obj%headers(9) = 3
!!    obj%headers(10) = 3
!!    obj%headers(11) = 7
!!    obj%headers(12) = 7
!!    obj%headers(13) = 3
!!    obj%headers(14) = 7
!!    obj%headers(15) = 7
!!    obj%headers(16) = 3
!!    obj%headers(17) = 7
!!    obj%headers(18) = 7
!!    obj%headers(19) = 3
!!    obj%headers(20) = 3
!!    obj%headers(21) = 3
!!    obj%headers(22) = 3
!!    obj%headers(23) = 3

      obj%ndpt = 0
      obj%dump_lun = -1
      obj%n_cnt = 0
      obj%n_out = 0

      call tdmp_update (obj)

      return
      end subroutine tdmp_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine tdmp_update (obj)
      implicit none
      type(tdmp_struct),target,intent(inout) :: obj     ! arguments

      integer :: nwih_do, status          ! local
      integer :: state                    ! local
      character(len=MAX_LINE) :: msg      ! message string
      character(len=6) :: opt_hdr_save    ! previous value of opt_hdr

      object => obj         ! needed for traps.
      obj%skip_wrapup = .true.

!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!

      call pc_get_global ('nwih'  , obj%nwih)
      call pc_get_global ('ndpt'  , obj%ndpt)

      call pc_get ('hdr_flag', obj%hdr_flag)
      call pc_get ('skip_init', obj%skip_init)
      call pc_get ('num_do', obj%num_do)
      call pc_get ('num_skip', obj%num_skip)
      call pc_get ('tot_do', obj%tot_do)
      call pc_get ('num_sam', obj%num_sam)
      call pc_get ('round', obj%round)
      call string_to_upper(obj%round)
      opt_hdr_save = obj%opt_hdr
      call pc_get ('opt_hdr', obj%opt_hdr)
      call string_to_upper(obj%opt_hdr)

      state = pc_get_update_state()

      call pc_put_minsize_array('headers',1)
      call pc_put_maxsize_array('headers',obj%nwih)

      opt_hdr_choice: &
      if(obj%opt_hdr .eq. 'USER') then
        if(state == PC_GUI .and. &
          pc_verify_scalar('opt_hdr') .and. &
          .not. pc_verify_scalar('hdr_flag')) then
          ! Note: regarding an obscure programming trick...
          ! The "if(... .not. pc_verify_scalar('hdr_flag')) then"
          ! allows the values to be forced when the GUI's OPT_HDR
          ! is changed by the user but prevents it from being
          ! forced when the parameters are copied by the GUI from the
          ! old process list (one process at a time, not "COPY ALL").

          call mem_alloc(obj%headers, obj%nwih)

          obj%hdr_size = obj%nwih

          ! reset all headers to INIL (empty).
          obj%headers = INIL
        else
          call pc_alloc('headers', obj%headers, obj%hdr_size)
        end if
      else if(obj%opt_hdr .eq. 'BRIEF') then ! opt_hdr_choice
        obj%round = 'YES'
        obj%hdr_size = 35
        call mem_alloc(obj%headers,obj%hdr_size)
        obj%headers = INIL
        obj%headers(1) = 6
        obj%headers(2) = 4
        obj%headers(3) = 5
        obj%headers(4) = 4
        obj%headers(5) = 4
        obj%headers(6) = 5
        obj%headers(7) = 5
        obj%headers(9) = 4
        obj%headers(10) = 4
        obj%headers(25) = 8
        obj%headers(33) = 5
        obj%headers(35) = 5
      else if(obj%opt_hdr .eq. '2D') then ! opt_hdr_choice
        obj%round = 'NO'
        obj%hdr_size = 37
        call mem_alloc(obj%headers,obj%hdr_size)
        obj%headers = INIL
        obj%headers(1) = 6
        obj%headers(2) = 4
        obj%headers(3) = 5
        obj%headers(4) = 4
        obj%headers(5) = 4
        obj%headers(6) = 5
        obj%headers(7) = 5
        obj%headers(9) = 4
        obj%headers(10) = 4
        obj%headers(11) = 7
        obj%headers(14) = 7
        obj%headers(17) = 7
        obj%headers(19) = 5
        obj%headers(25) = 8
        obj%headers(33) = 5
        obj%headers(35) = 5
        obj%headers(37) = 5
      else if(obj%opt_hdr .eq. 'GRID') then ! opt_hdr_choice
        obj%round = 'NO'
        obj%hdr_size = 47
        call mem_alloc(obj%headers,obj%hdr_size)
        obj%headers = INIL
        obj%headers(1) = 6
        obj%headers(2) = 4
        obj%headers(3) = 5
        obj%headers(4) = 4
        obj%headers(5) = 4
        obj%headers(6) = 5
        obj%headers(7) = 5
        obj%headers(8) = 5
        obj%headers(9) = 4
        obj%headers(10) = 4
        obj%headers(19) = 4
        obj%headers(25) = 8
        obj%headers(33) = 5
        obj%headers(34) = 5
        obj%headers(35) = 5
        obj%headers(36) = 5
        obj%headers(37) = 5
        obj%headers(38) = 5
        obj%headers(46) = 5
        obj%headers(47) = 5
      else if(obj%opt_hdr .eq. 'SURVEY') then ! opt_hdr_choice
        obj%round = 'NO'
        obj%hdr_size = 25
        call mem_alloc(obj%headers,obj%hdr_size)
        obj%headers = INIL
        obj%headers(1) = 6
        obj%headers(2) = 4
        obj%headers(3) = 5
        obj%headers(4) = 4
        obj%headers(5) = 4
        obj%headers(6) = 5
        obj%headers(7) = 5
        obj%headers(8) = 5
        obj%headers(9) = 4
        obj%headers(10) = 4
        obj%headers(11) = 7
        obj%headers(12) = 7
        obj%headers(13) = 4
        obj%headers(14) = 7
        obj%headers(15) = 7
        obj%headers(16) = 4
        obj%headers(17) = 7
        obj%headers(18) = 7
        obj%headers(19) = 4
        obj%headers(25) = 8
      else if(obj%opt_hdr .eq. 'STAT') then ! opt_hdr_choice
        obj%round = 'YES'
        obj%hdr_size = 47
        call mem_alloc(obj%headers,obj%hdr_size)
        obj%headers = INIL
        obj%headers(1) = 6
        obj%headers(2) = 4
        obj%headers(3) = 4
        obj%headers(4) = 3
        obj%headers(5) = 4
        obj%headers(6) = 5
        obj%headers(7) = 4
        obj%headers(8) = 4
        obj%headers(9) = 4
        obj%headers(10) = 3
        obj%headers(19) = 4
        obj%headers(33) = 4
        obj%headers(34) = 4
        obj%headers(35) = 4
        obj%headers(36) = 4
        obj%headers(37) = 4
        obj%headers(38) = 4
        obj%headers(39) = 4
        obj%headers(40) = 4
        obj%headers(41) = 4
        obj%headers(42) = 4
        obj%headers(43) = 4
        obj%headers(44) = 4
        obj%headers(45) = 4
        obj%headers(46) = 4
        obj%headers(47) = 4
      else if(obj%opt_hdr .eq. 'SYS') then ! opt_hdr_choice
        obj%round = 'NO'
        obj%hdr_size = 47
        call mem_alloc(obj%headers,obj%hdr_size)
        obj%headers = INIL
        obj%headers(1) = 6
        obj%headers(2) = 5
        obj%headers(3) = 5
        obj%headers(4) = 5
        obj%headers(5) = 7
        obj%headers(6) = 7
        obj%headers(7) = 5
        obj%headers(8) = 5
        obj%headers(9) = 5
        obj%headers(10) = 5
        obj%headers(11) = 7
        obj%headers(12) = 7
        obj%headers(13) = 7
        obj%headers(14) = 7
        obj%headers(15) = 7
        obj%headers(16) = 7
        obj%headers(17) = 7
        obj%headers(18) = 7
        obj%headers(19) = 7
        obj%headers(20) = 7
        obj%headers(21) = 7
        obj%headers(22) = 7
        obj%headers(23) = 7
        obj%headers(25) = 8
        obj%headers(33) = 5
        obj%headers(34) = 5
        obj%headers(35) = 5
        obj%headers(36) = 5
        obj%headers(37) = 5
        obj%headers(38) = 5
        obj%headers(39) = 4
        obj%headers(40) = 4
        obj%headers(41) = 4
        obj%headers(42) = 4
        obj%headers(43) = 4
        obj%headers(44) = 4
        obj%headers(45) = 4
        obj%headers(46) = 5
        obj%headers(47) = 5
      else ! opt_hdr_choice
         obj%opt_hdr = opt_hdr_save
         call pc_error( &
           'OPT_HDR must be USER, BRIEF, 2D, GRID, SURVEY, STAT or SYS')
      end if opt_hdr_choice

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if(obj%hdr_flag < 0 .or. obj%hdr_flag > obj%nwih) then
        call pc_error( 'HDR_FLAG < 0 or > NWIH')
        obj%hdr_flag = 0
      end if

      if(obj%skip_init < 0) then
        call pc_error('SKIP_INIT < 0')
        obj%skip_init = 0
      end if

      if(obj%num_do < 1) then
        call pc_error('NUM_DO < 1')
        obj%num_do = 1
      end if

      if(obj%num_skip < 0) then
        call pc_error('NUM_SKIP < 0')
        obj%num_skip = 0
      end if

      if(obj%tot_do < 1) then
        call pc_error('TOT_DO < 1')
        obj%tot_do = 10
      end if

      if(obj%num_sam < 0) then
        call pc_error('NUM_SAM < 0')
        obj%num_sam = 0
      else if(obj%num_sam > obj%ndpt) then
        call pc_error('NUM_SAM > NPDT')
        obj%num_sam = obj%ndpt
      end if

      select case (obj%round(1:1))
      case ('Y')
        obj%round = 'YES'
      case ('N')
        obj%round = 'NO'
      case default
        call pc_error('ROUND must be "YES" (nearest integer) or "NO"')
        obj%round = 'YES'
      end select

      do nwih_do = 1, obj%hdr_size
        if(obj%headers(nwih_do) == INIL) then
          cycle
        else if(obj%headers(nwih_do) < 0) then
          write(msg, '("HD(",I4,") column width < 0")') nwih_do
          call pc_warning(msg)
          obj%headers(nwih_do) = INIL
        else if(obj%headers(nwih_do) > 22) then
          write(msg, '("HD(",I4,") column width > 22")') nwih_do
          call pc_warning(msg)
          obj%headers(nwih_do) = 22
        end if
      end do

!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!


      call pc_put ('hdr_flag', obj%hdr_flag)
      call pc_put ('skip_init', obj%skip_init)
      call pc_put ('num_do', obj%num_do)
      call pc_put ('num_skip', obj%num_skip)
      call pc_put ('tot_do', obj%tot_do)
      call pc_put ('num_sam', obj%num_sam)

      call pc_put_options_field('opt_hdr', &
        (/ "USER  ", "BRIEF ", "2D    ", "GRID  ", "SURVEY", "STAT  ", &
          "SYS   " /), 7)
      call pc_put ('opt_hdr', obj%opt_hdr)

      call pc_put_options_field('round', (/ "YES", "NO " /), 2)
      call pc_put ('round', obj%round)

      call pc_put ('headers', obj%headers, obj%hdr_size)

      if(obj%opt_hdr == 'USER') then
        call pc_put_sensitive_array_flag('headers', .true.)
        call pc_put_sensitive_array_flag('round', .true.)
      else
        call pc_put_sensitive_array_flag('headers', .false.)
        call pc_put_sensitive_array_flag('round', .false.)
      end if

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      print_lun = pc_get_lun()

      ! attempt to get logical unit for temporary dump file
      !call getlun(obj%dump_lun, status)
      status=0

      if (status == 0) then
         status=cpsio_open('.tdmp_tmp','w+',obj%dump_lun,.true.)
         !open(unit = obj%dump_lun, action = 'READWRITE', iostat = status, &
         !  status = 'SCRATCH', recl = MAX_LINE)
         if(status /= 0) then
           write(msg, '("iostat=",I4,", opening scratch dump file")')
           call pc_error(msg)
           return
         end if
      else
        call pc_error( &
          'getlun failed, can not open scratch dump file')
        return
      end if

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine tdmp_update

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

      subroutine tdmp (obj,ntr,hd,tr)
      implicit none
      type(tdmp_struct)    ,intent(inout)  :: obj      ! arguments
      integer              ,intent(inout)  :: ntr      ! arguments
      double precision     ,intent(in)     :: hd(:,:)  ! arguments
      real                 ,intent(in)     :: tr(:,:)  ! arguments

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: ntr_do, n_column, nwih_do, samp_do, INZ, line_do,status   
      CHARACTER(LEN=MAX_LINE) :: LINE

      IF (ntr <= 0) then
        call tdmp_wrapup(obj)

        return
      end if

      ! tdmp called with one or more traces
      ntr_loop: DO ntr_do = 1, ntr
        IF (obj%n_out >= obj%tot_do) EXIT  ! max number of traces reached

        obj%n_cnt = obj%n_cnt + 1

        IF (obj%n_cnt < obj%skip_init + obj%n_out + &
          (obj%n_out / obj%num_do) * obj%num_skip + 1) CYCLE  ! skip window

        IF (obj%hdr_flag /= 0) THEN
          ! dump trace, iff flagged header word is non-zero

          IF(hd(obj%hdr_flag,ntr_do) == 0.0) CYCLE ! not flagged

        ENDIF

        LINE = ' '
        n_column = 1

        nwih_loop: DO nwih_do = 1, obj%hdr_size
          IF (obj%headers(nwih_do) > 0) THEN
            ! one or more columns reserved for header

            IF (n_column + obj%headers(nwih_do) > MAX_LINE) THEN
              ! line overflow, flush current line
              !WRITE (obj%dump_lun, '(A)') LINE
              status = cpsio_write(obj%dump_lun,line)
              if(status /= 0 ) write(print_lun,'(A,I4,A)')'Error Writing to scratch file. Error=',status,'.'
              LINE = ' '
              n_column = 1
            ENDIF

            IF (obj%round(1:1) == 'Y') THEN
              ! round float header to nearest integer and dump
              CALL string_ii2cc (NINT(hd(nwih_do,ntr_do)), &
                LINE(n_column + 1 : n_column + obj%headers(nwih_do)))
            ELSE
              ! dump header as float value
              CALL string_dd2cc (hd(nwih_do,ntr_do), &
                LINE(n_column + 1 : n_column + obj%headers(nwih_do)))
            ENDIF

            ! Right justify the result
            LINE(n_column + 1 : n_column + obj%headers(nwih_do)) = adjustr( &
            LINE(n_column + 1 : n_column + obj%headers(nwih_do)))

            n_column = n_column + obj%headers(nwih_do) + 1
          ENDIF

        END DO nwih_loop

        ! flush the header line
        !WRITE (obj%dump_lun, '(A)') LINE
        status = cpsio_write(obj%dump_lun,line)
        if(status /= 0 ) write(print_lun,'(A,I4,A)')'Error Writing to scratch file. Error=',status,'.'
        IF (obj%num_sam > 0) THEN
          ! dump samples values, starting with first non-zero value

          ! inlined INZ = ISRCHNE(obj%ndpt,TR(1:obj%ndpt,ntr_do),1,0.)
          DO INZ = 1, obj%ndpt
            IF (TR(INZ, ntr_do) /= 0.0) EXIT
          END DO

          IF (INZ <= obj%ndpt) THEN
            ! dump one or more non-zero sample values

            line_loop: DO line_do = 1, (obj%num_sam - 1) / 10 + 1
              ! maximum of 10 samples per line
              LINE = ' '
              WRITE (LINE(2:6), '(I4,'':'')') INZ

              DO samp_do = 1, 10
                IF (INZ > obj%ndpt) EXIT

                CALL string_ff2cc (TR(INZ,ntr_do), &
                   LINE(8 + (samp_do - 1) * 9 : 15 + (samp_do - 1) * 9))

                ! Right justify the result
                LINE(8 + (samp_do - 1)*9 : 15 + (samp_do - 1)*9) = adjustr( &
                LINE(8 + (samp_do - 1)*9 : 15 + (samp_do - 1)*9))

                INZ = INZ + 1
              END DO

              ! flush the sample line
              !WRITE (obj%dump_lun, '(A)') LINE
              status = cpsio_write(obj%dump_lun,line)
              if(status /= 0 ) write(print_lun,'(A,I4,A)')'Error Writing to scratch file. Error=',status,'.'
            END DO line_loop
          ENDIF
        ENDIF

        obj%n_out = obj%n_out + 1
      END DO ntr_loop

      return
      end subroutine tdmp

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine tdmp_wrapup (obj)
      implicit none
      type(tdmp_struct),intent(inout) :: obj       ! arguments

      integer :: n_column              ! local
      integer :: nwih_do               ! local
      integer :: status                ! local
      CHARACTER(LEN=MAX_LINE) :: LINE  ! local

      if(obj%skip_wrapup) return
      obj%skip_wrapup = .true.
      if(obj%dump_lun >= 0) then
        ! copy temporary dump file to standard out

        WRITE (print_lun, "('1  TDMP  TRACES ',4I9)") &
           obj%skip_init, obj%num_do, obj%num_skip, obj%tot_do
        !REWIND obj%dump_lun
        call cpsio_rewind(obj%dump_lun)
        LINE = ' '
        n_column = 1

        ! print tabular column headers (header word number)
        nwih_loop: DO nwih_do = 1, obj%hdr_size
          IF (obj%headers(nwih_do) > 0) THEN
            IF (n_column + obj%headers(nwih_do) > MAX_LINE) THEN
              WRITE (print_lun, '(A)') LINE
              LINE = ' '
              n_column = 1
            ENDIF

            CALL string_ii2cc(nwih_do, &
              LINE(n_column + 1 : n_column + obj%headers(nwih_do)))

            LINE(n_column + 1 : n_column + obj%headers(nwih_do)) = adjustr( &
            LINE(n_column + 1 : n_column + obj%headers(nwih_do)))

            n_column = n_column + obj%headers(nwih_do) + 1
          ENDIF
        END DO nwih_loop

        WRITE(print_lun, '(A)') LINE

        DO
          !READ (obj%dump_lun, '(A)', IOSTAT = STATUS) LINE
          status = cpsio_read(obj%dump_lun,line) 
          IF (STATUS .ne. 0) exit
          WRITE(print_lun, '(A)') LINE
        END DO

        !close(unit = obj%dump_lun)
         status = cpsio_close(obj%dump_lun)
        

        obj%dump_lun = -1
      end if

      return
      end subroutine tdmp_wrapup

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module tdmp_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

