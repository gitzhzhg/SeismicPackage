!<CPS_v1 type="PROCESS"/>
!!------------------------------- slice.f90 --------------------------------!!
!!------------------------------- slice.f90 --------------------------------!!
!!------------------------------- slice.f90 --------------------------------!!

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
! Name       : SLICE  (SLICE through headers or time values)
! Category   : miscellaneous
! Written    : 1995-04-10   by: Scott Michell
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Characterize traces falling into each bin of a 2D bin array.
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
! SLICE sets up a 2D array of bins, usually defined by header words 7 and 8,
! makes a measurement on the traces that fall into each bin and separately
! reports the measurement on each bin in a columnar ASCII file.  The
! measurements available are: maximum value, minimum value, average value, sum
! of values and count of traces falling into the bin.  Each SLICE process can
! make as many as 100 measurements on trace samples and/or 100 header word
! values.
!
! Each row in the output ASCII file corresponds to an occupied bin.  The first
! two columns identify the bin (HDR_X and HDR_Y values) and are followed by one
! column for each requested measurement.  The ASCII file can contain up to 12
! space delimited columns.  Entries in the ASCII file are made only for
! occupied bins; no entries are made in the ASCII file for unoccupied bins.
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
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
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       used but not changed
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
!         HDR_FLAG                   flagword
!         HDR_X, HDR_Y               identify bins
!         HEADERS                    header words whose values are measured
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!016. 2006-10-16  D. Glover    Added NULLIFY statements for Intel compiler.
! 15. 2006-01-10  B. Menger    Removed Unused Variables.
! 14. 2004-01-07  K. Goodger   Use memman primitive for memory management.
! 13. 2002-09-16  K. Goodger   Use mth module for binning.
! 12. 2002-03-19  R. Selzler   Eliminate cio and simply use Fortran write,
!                              to avoid issues with cio extents.
! 11. 2001-08-30  K. Goodger   Increase extent size on cio file to maximum. 
! 10. 2001-02-19  Brad Kruse   Report #293, Make METHOD easier to fill in, 
!                              change PATHNAME error to occur only when leaving
!                              screen "SLICE".
! 9. 2001-01-23   Brad Kruse   Conversion to new CPS
! 8. 1998-11-03   Goodger    Change pogun to po4001 in slice_crou.c
! 7. 1998-11-11   Goodger    Begin using fortran90 compiler.
! 6. 1996-03-01   Michell    Fix _netinfo format change in slice_dispose
!                            Add support for rcp (see rcpxfr2) see notes
!                            Fix MAX bug for all negative inputs
!                            Modified slice_dispose
! 5. 1995-10-02   Michell    Fix indexing to make sure that only traces
!                            that fall within the grid definition are
!                            used.
! 4. 1995-09-25   Troutt     Initialize NARRAY=0 before call to DCODE.
! 3. 1995-08-14   Michell    Fix index check so values less than the grid
!                            definition are properly excluded.
! 2. 1995-04-13   Michell    Fix slice_po.  Remove some Debug statements.
! 1. 1995-04-10   Michell    Original Version
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
!
!
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
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
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
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
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!-------------------------------------------------------------------------------
!                    ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
! Some assumptions:
!   1. Number of bins (obj%x_tot * obj%y_tot) should be 2-3 million or less
!   2. 32-bit data accumulation should be sufficient
!   3. All bin data will be held in static arrays, rather than using a file.
!   4. A maximum of 100 header values and 100 time slices (obj%num_headers
!      + obj%num_times) will be accumulated for each X-Y bin.  The multiple
!      of bins and bin values should be kept smaller than 4 million.
!   5. Outlier HDR_X and HDR_Y values (bin numbers fall outside 1-X_TOT,
!      or 1 - Y_TOT) will be disregarded.  This is a possible future area
!      to capture information that may be useful in detecting problems in
!      a data set.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!
!<gui_def>
!<NS SLICE Process/NC=80/NR=28>
!
!                  SLICE through headers or time values Process
!          Characterize traces falling into each bin of a 2D bin array.
!
!    HDR_FLAG=`IIIIII    
!
!    HDR_X~~= `IIIIII           HDR_Y~~= `IIIIII
!    X_INIT = `FFFFFFFFFFFF     Y_INIT = `FFFFFFFFFFFF
!    X_INC~~= `FFFFFFFFFFFF     Y_INC~~= `FFFFFFFFFFFF
!    X_LAST = `FFFFFFFFFFFF     Y_LAST = `FFFFFFFFFFFF     
!    X_TOT~~= `IIIIIIII         Y_TOT~~= `IIIIIIII
!
!    BINS~~~= [BN]`IIIIIII / [SN]`IIIIIIII
!
!    HEADERS [HM]METHOD         TIMES   [TM]METHOD            METHODS
!    `III~~~~`SSSSSS~~~         `FFFFFFF`SSSSSS~~~        a.  a) Average
!    `III~~~~`SSSSSS~~~         `FFFFFFF`SSSSSS~~~        b.  x) Maximum
!    `III~~~~`SSSSSS~~~         `FFFFFFF`SSSSSS~~~        c.  n) Minimum
!    `III~~~~`SSSSSS~~~         `FFFFFFF`SSSSSS~~~        d.  r) Range
!    `III~~~~`SSSSSS~~~         `FFFFFFF`SSSSSS~~~        e.  s) Sum
!    `III~~~~`SSSSSS~~~         `FFFFFFF`SSSSSS~~~
!
!    Output file: 
!    PATHNAME = `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!    Output file data line format:
!    BIN_DATA_FORMAT = `CCCCCCCCCCC
!
!<PARMS TIMES_ARRAYSET[/XST/YST]>
!<PARMS HEADERS_ARRAYSET[/XST/YST]>
!<PARMS HM[HEADERS_METHOD]>
!<PARMS TM[TIMES_METHOD/ML=8/XST]>
!<PARMS PATHNAME[/ML=128/XST]>
!<PARMS BN[BIN_COUNT/EN/SN/XSF/YSF]>
!<PARMS SN[STORE_COUNT/EN/SN/XSF/YSF]>
!</gui_def>
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!
!<HelpSection>
!
!<Help KEYWORD="PATHNAME">
!<Tip> Pathname for the columnar, space delimited, ASCII output file. </Tip>
! Default = -
! Allowed = char
! Each row in the output ASCII file corresponds to an occupied bin.  The first
! two columns identify the bin (HDR_X and HDR_Y values).  The third column is
! the count of traces in the bin.  These three columns are followed by one
! column for each requested measurement (two columns for RANGE).  The ASCII 
! file can contain up to 43 space delimited columns.  Entries in the ASCII 
! file are made only for occupied bins; no entries are made in the ASCII file 
! for unoccupied bins.
!
! See the BIN_DATA_FORMAT field for information about the alternate, verbose
! listing of bin data -- FORMATTED.  The one line per bin format described 
! above is the UNFORMATTED option.
!</Help>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! If HDR_FLAG = 0, then all traces are used by SLICE.  Otherwise, only traces
! with a flag set in header word HDR_FLAG are used by SLICE.
!</Help>
!
!<Help KEYWORD="HEADERS">
!<Tip> Array of header words whose values will be measured. </Tip>
! Default = -
! Allowed = 1 - NWIH (linked array 100)
!</Help>
!
!<Help KEYWORD="HEADERS_METHOD">
!<Tip> Type of measurement to use. </Tip>
! Default = RANGE
! Allowed = AVERAGE   (Average of values encountered in bin.)
! Allowed = MAXIMUM   (Maximum of values encountered in bin.)
! Allowed = MINIMUM   (Minimum of values encountered in bin.)
! Allowed = RANGE     (Maximum and minimum of values encountered in bin.)
! Allowed = SUM       (Sum of values encountered in bin.)
!
! HEADERS_METHOD entries are linked to corresponding entries
! of the HEADERS list.
!</Help>
!
!<Help KEYWORD="TIMES">
!<Tip> Array of times for trace samples to be measured. </Tip>
! Default = -
! Allowed = real (linked array 100)
!</Help>
!
!<Help KEYWORD="TIMES_METHOD">
!<Tip> Type of measurement to use. </Tip>
! Default = RANGE
! Allowed = AVERAGE   (Average of values encountered in bin.)
! Allowed = MAXIMUM   (Maximum of values encountered in bin.)
! Allowed = MINIMUM   (Minimum of values encountered in bin.)
! Allowed = RANGE     (Maximum and minimum of values encountered in bin.)
! Allowed = SUM       (Sum of values encountered in bin.)
!
! TIMES_METHOD entries are linked to corresponding entries in the TIMES list.
!</Help>
!
!<Help KEYWORD="HDR_X">
!<Tip> Header word defining x coordinate for bins. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="X_INIT">
!<Tip> Value of HDR_X for the first bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="X_INC">
!<Tip> Increment of HDR_X between bins. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="X_LAST">
!<Tip> Value of HDR_X for the last bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="X_TOT">
!<Tip> Total number of bins in the x-direction. </Tip>
! Default = 1
! Allowed = int
!</Help>
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word defining y coordinate for bins. </Tip>
! Default = 8
! Allowed = 0 - NWIH
! Set HDR_Y = 0 for 2D work.
!</Help>
!
!<Help KEYWORD="Y_INIT">
!<Tip> Value of HDR_Y for the first bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="Y_INC">
!<Tip> Increment of HDR_Y between bins. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="Y_LAST">
!<Tip> Value of HDR_Y for the last bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="Y_TOT">
!<Tip> Total number of bins in the y-direction. </Tip>
! Default = 1
! Allowed = int
!</Help>
!
!<Help KEYWORD="BIN_DATA_FORMAT">
!<Tip> Print style of output data. </Tip>
! Default = UNFORMATTED
! Allowed = FORMATTED
! Allowed = UNFORMATTED
!
! The output file consists of a header, detailing the 
! date run, the job name, and selected parameters, and a list
! of data gathered.
!
! UNFORMATTED -- listing is one bin per line, with each line
!                starting with the bin X and Y coordinates,
!                followed by the count of traces found in 
!                that bin, followed by data collected.
!                Header values collected follow the x-y-count
!                columns, then the trace time data.
! FORMATTED   -- The same data is listed, in the same order,
!                as for the UNFORMATTED listing.  
!                The X-Y-Count columns are printed on the first
!                line for the bin, followed by the remaining data. 
!                Each collected value is printed indented, 
!                one value per line. 
!</Help>
!
!<Help KEYWORD="BIN_COUNT" TYPE="DISPLAY_ONLY">
!<Tip> Total number of bins in the x-y grid. </Tip>
! Default = -
! Allowed = integer
!</Help>
!
!<Help KEYWORD="STORE_COUNT" TYPE="DISPLAY_ONLY">
!<Tip> Total number of values stored for the x-y grid. </Tip>
! Default = -
! Allowed = integer
!</Help>
!
!</HelpSection>
!
!-------------------------------------------------------------------------------



!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!

module slice_module
  !
  ! - Referenced modules
  !
  use pathcheck_module  
  use pc_module
  use named_constants_module
  use memman_module          
  use mth_module
  use getlun_module
  !
  implicit none
  !
  private
  !
  public :: slice_create
  public :: slice_initialize
  public :: slice_update
  public :: slice_delete
  !
!<execute_only>
  public :: slice            ! main execution (trace processing) routine.
  public :: slice_wrapup
!</execute_only>

  character(len = 100), public, save :: SLICE_IDENT = &
    '$Id: slice.f90,v 1.16 2006/10/17 13:45:47 Glover prod sps $'


  !!------------------------ parameter structure ---------------------------!!
  !!------------------------ parameter structure ---------------------------!!
  !!------------------------ parameter structure ---------------------------!!

  integer, parameter, public :: max_hdr_slices    = 100
  integer, parameter, public :: max_time_slices    = 100
  integer, parameter, public :: job_name_len = 15
  !
  type,public :: slice_struct
    !
    private
    !
    logical                   :: skip_wrapup         ! wrapup flag.
    !
    ! - Process parameters
    !      Available from the parameter cache
    !
    character (len=FILENAME_LENGTH) :: pathname            ! pc - char
    character (len=11)              :: bin_data_format

    integer                         :: hdr_flag            ! pc - 0 - NWIH
    !
    integer                         :: num_headers
    integer                         :: headers        (max_hdr_slices) 
                                                 ! pc - 1 - NWIH (linked array)
    character (len = 7)             :: headers_method (max_hdr_slices) 
                                                 ! Was m1
                                                 ! pc - AVERAGE, RANGE,     &
                                                 !     MAXIMUM, MINIMUM, SUM
    !
    integer                         :: num_times
    integer                         :: sample_times (max_time_slices) 
    real                            :: times        (max_time_slices) 
                                                 ! pc - real (linked array)
    character (len = 7)             :: times_method (max_time_slices) 
                                                 ! Was m1
                                                 ! pc - AVERAGE, RANGE,     &
                                                 !     MAXIMUM, MINIMUM, SUM
    !
    integer                         :: out_lun
    integer                         :: num_ranges
    integer                         :: num_bins
    integer                         :: num_flagged_traces
    integer                         :: num_traces
    character (len = job_name_len)  :: job_name
    !
    integer                         :: hdr_x               ! pc - 1 - NWIH
    integer                         :: hdr_y               ! pc - 0 - NWIH
    integer                         :: x_tot               ! pc - int
    integer                         :: y_tot               ! pc - int
    real                            :: x_inc               ! pc - real > 0.0
    real                            :: x_init              ! pc - real
    real                            :: x_last              ! pc - real
    real                            :: y_inc               ! pc - real > 0.0
    real                            :: y_init              ! pc - real
    real                            :: y_last              ! pc - real
    !
    ! - Common globals
    !
    integer                         :: ndpt             ! Common globals, ndpt
    integer                         :: nwih             ! Common globals, nwih
    real                            :: dt               ! Common globals, dt
    real                            :: tstrt            ! Common globals, tstrt
    !
    double precision, pointer       :: bin_accum (:, :, :)
    integer,          pointer       :: bin_count (:, :)
    !
  end type slice_struct


  !!----------------------------- interfaces -------------------------------!!
  !!----------------------------- interfaces -------------------------------!!
  !!----------------------------- interfaces -------------------------------!!

  interface slice_write_item
    module procedure slice_write_item_dp
    module procedure slice_write_item_r
    module procedure slice_write_item_i
    module procedure slice_write_item_c
  end interface 

  !!------------------------------- data -----------------------------------!!
  !!------------------------------- data -----------------------------------!!
  !!------------------------------- data -----------------------------------!!


  type(slice_struct),pointer,save :: object      ! needed for traps.
  !
  character (len = 7), parameter :: AVERAGE = 'AVERAGE'
  character (len = 7), parameter :: MAXIMUM = 'MAXIMUM'
  character (len = 7), parameter :: MINIMUM = 'MINIMUM'
  character (len = 7), parameter :: RANGE   = 'RANGE'
  character (len = 7), parameter :: SUM     = 'SUM'

  character (len = 11), parameter :: FORMATTED   = 'FORMATTED'
  character (len = 11), parameter :: UNFORMATTED = 'UNFORMATTED'

contains


  !!------------------------------ create ----------------------------------!!
  !!------------------------------ create ----------------------------------!!
  !!------------------------------ create ----------------------------------!!


  subroutine slice_create (obj)
    !
    ! - Arguments
    !
    type (slice_struct), pointer :: obj       ! arguments
    !
    ! - Begin slice_create
    !
    allocate (obj)
    nullify (obj%bin_accum) ! jpa
    nullify (obj%bin_count) ! jpa
    !
    call memman_nullify(obj%bin_accum,"bin_accum")
    call memman_nullify(obj%bin_count,"bin_count")
    !
    call slice_initialize (obj)
    !
  end subroutine slice_create


  !!------------------------------- delete ---------------------------------!!
  !!------------------------------- delete ---------------------------------!!
  !!------------------------------- delete ---------------------------------!!


  subroutine slice_delete (obj)
    !
    ! - Arguments
    !
    type (slice_struct), pointer :: obj       ! arguments
    !
    ! - Begin slice_delete
    !
!<execute_only>
    call slice_wrapup (obj)
!</execute_only>
    !
    call memman_deallocate(obj%bin_accum)
    call memman_deallocate(obj%bin_count)
!rev14    if (associated (obj%bin_accum)) deallocate (obj%bin_accum)
!rev14    if (associated (obj%bin_count)) deallocate (obj%bin_count)
    !
    deallocate(obj)
    !
  end subroutine slice_delete


  !!----------------------------- initialize -------------------------------!!
  !!----------------------------- initialize -------------------------------!!
  !!----------------------------- initialize -------------------------------!!


  subroutine slice_initialize (obj)
    !
    ! - Arguments
    !
    type(slice_struct),intent(inout) :: obj       ! arguments
    !
    ! - Begin slice_initialize
    !
    obj%pathname = pathcheck_empty
    obj%bin_data_format = UNFORMATTED
    obj%hdr_flag = 0 
    obj%num_headers = 0
    obj%headers = 0
    obj%headers_method = RANGE
    obj%num_times = 0.0
    obj%times = 0.0
    obj%times_method = RANGE
    obj%out_lun = -1
    obj%num_ranges = 0
    obj%num_bins = 0
    obj%hdr_x = 7
    obj%x_init = 1.0
    obj%x_inc = 1.0
    obj%x_last = 1.0
    obj%x_tot = 1
    obj%hdr_y = 8
    obj%y_init = 1.0
    obj%y_inc = 1.0
    obj%y_last = 1.0
    obj%y_tot = 1
    !
    obj%num_flagged_traces = 0
    obj%num_traces = 0
    obj%job_name = ' '
    !
    call slice_update (obj)
    !
  end subroutine slice_initialize


  !!-------------------------- start of update -----------------------------!!
  !!-------------------------- start of update -----------------------------!!
  !!-------------------------- start of update -----------------------------!!


  subroutine slice_update (obj)
    !
    ! - Arguments
    !
    type (slice_struct), intent (inout), target :: obj             ! arguments
    !
    ! - Local variables
    !
    character (len = 128) :: my_line
    integer :: bin_count
    integer :: i
    integer :: nh1
    integer :: nh2
    integer :: nt1
    integer :: nt2
    integer :: status
    integer :: store_count
    real :: stop_time
    !
    ! - Begin slice_update
    !
    object => obj               ! needed for traps.
    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


    !!------------------------ read parameters -----------------------------!!
    !!------------------------ read parameters -----------------------------!!
    !!------------------------ read parameters -----------------------------!!


    call pc_register_array_names ('headers_arrayset',    &
                                  (/'HEADERS       ', 'HEADERS_METHOD' /), 2)
    call pc_register_array_names ('times_arrayset',      &
                                  (/'TIMES       ',   'TIMES_METHOD' /),   2)

    call pc_get_jdata (keyword = 'JOBNAME',    &
                       scalar  = obj%job_name)

    call pc_get_global ('nwih',  obj%nwih)     ! number of header words.
    call pc_get_global ('ndpt',  obj%ndpt)     ! number of trace samples.
    call pc_get_global ('dt',    obj%dt)       ! trace sample interval (sec).
    call pc_get_global ('tstrt', obj%tstrt)    !time of 1st trace 


    call pc_get (keyword = 'PATHNAME',        scalar = obj%pathname)
    call pc_get (keyword = 'BIN_DATA_FORMAT', scalar = obj%bin_data_format)
    call string_to_upper (obj%bin_data_format)
    !
    call pc_get (keyword = 'HDR_FLAG', scalar = obj%hdr_flag)
    !
    nh1 = obj%num_headers
    nh2 = nh1
    call pc_get (keyword   = 'HEADERS',        &
                 array     = obj%headers,    &
                 nelements = nh1,              &
                 etrap     = slice_headers_element_trap)
    call pc_get (keyword   = 'HEADERS_METHOD',        &
                 array     = obj%headers_method,    &
                 nelements = nh2,                     &
                 etrap     = slice_method_element_trap)
    !
    nt1 = obj%num_times
    nt2 = nt1
    call pc_get (keyword   = 'TIMES',        &
                 array     = obj%times,    &
                 nelements = nt1,            &
                 etrap     = slice_times_element_trap)
    call pc_get (keyword   = 'TIMES_METHOD',        &
                 array     = obj%times_method,    &
                 nelements = nt2,                   &
                 etrap     = slice_method_element_trap)
    !
    call pc_get (keyword = 'HDR_X',  scalar = obj%hdr_x)
    call pc_get (keyword = 'X_INIT', scalar = obj%x_init)
    call pc_get (keyword = 'X_INC',  scalar = obj%x_inc)
    call pc_get (keyword = 'X_LAST', scalar = obj%x_last)
    call pc_get (keyword = 'X_TOT',  scalar = obj%x_tot)
    call pc_get (keyword = 'HDR_Y',  scalar = obj%hdr_y)
    call pc_get (keyword = 'Y_INIT', scalar = obj%y_init)
    call pc_get (keyword = 'Y_INC',  scalar = obj%y_inc)
    call pc_get (keyword = 'Y_LAST', scalar = obj%y_last)
    call pc_get (keyword = 'Y_TOT',  scalar = obj%y_tot)


    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!

    !
    ! - Check PATHNAME
    !
    call pathcheck (keyword  = 'pathname',       &
                    pathname = obj%pathname,   &
                    ext      = 'slc',            &
                    status   = status)

  check_pathname:   &
    if (status == path_unspecified) then   ! char
      !
      if ((pc_verify_screen (keyword = "slice")         &
          .and. (pc_get_update_state () == pc_gui))     &
          .or. pc_verify_scalar (keyword = 'pathname')) then
        call pc_error (msg1 = 'PATHNAME (',            &
                       var1 = trim (obj%pathname),   &
                       msg2 = ') was not specified.')
      end if
      !
    else if (status == path_invalid) then check_pathname  ! char
      !
      call pc_error (msg1 = 'Bad value for PATHNAME (',   &
                     var1 = trim (obj%pathname),        &
                     msg2 = ')')
      !
    end if check_pathname

    !
    ! - Check BIN_DATA_FORMAT
    !
  check_bin_data_format:   &
    if ((obj%bin_data_format /= FORMATTED)    &
        .and. (obj%bin_data_format /= UNFORMATTED)) then 
      !
      call pc_error (msg1 = 'BIN_DATA_FORMAT (',            &
                     var1 = trim (obj%bin_data_format),   &
                     msg2 = ') must be FORMATTED or UNFORMATTED.')
      obj%bin_data_format = UNFORMATTED
      !
    end if check_bin_data_format
    !
    ! - Check HDR_FLAG
    !
  check_hdr_flag:   &
    if ((obj%hdr_flag < 0) .or. (obj%hdr_flag > obj%nwih)) then  ! 0-NWIH
      !
      call pc_error (msg1 = 'Bad value for HDR_FLAG (',   &
                     var1 = obj%hdr_flag,   &
                     msg2 = ').  Must be 0-',   &
                     var2 = obj%nwih,                &
                     msg3 = '.  Setting to default 0')
      !
      obj%hdr_flag = 0
      !
    end if check_hdr_flag

    !
    ! - Check HEADERS
    !
    if (nh1 == nh2) then
      obj%num_headers = nh1
    else
      !
      call pc_error (msg1 = 'Error. Number of HEADERS (',   &
                     var1 = nh1,                            &
                     msg2 = ') and HEADER METHODS (',       &
                     var2 = nh2,                            &
                     msg3 = ') does not match.')
      obj%num_headers = min (a1 = nh1, a2 = nh2)
      !
    end if
    !
    if (obj%num_headers > max_hdr_slices) then
      !
      call pc_error (msg1 = 'Error. Number of HEADERS (',   &
                     var1 = obj%num_headers,              &
                     msg2 = ') is greater than limit (',    &
                     var2 = max_hdr_slices,                      &
                     msg3 = ').  Setting to limit.')
      obj%num_headers = max_hdr_slices
      !
    end if
    !
  loop_thru_headers:   &
    do i = 1, obj%num_headers
      !
    check_headers:   &
      if ((obj%headers (i) < 0) .or. (obj%headers (i) > obj%nwih)) then
        !
        call pc_error (msg1 = 'Bad value for HEADERS (',            &
                       var1 = obj%headers (i),                    &
                       msg2 = ').  Must be between 0 and NWIH (',   &
                       var2 = obj%nwih,                           &
                       msg3 = ').')
        !
      end if check_headers
      !
      call slice_check_method (method     = obj%headers_method (i),    &
                               num_ranges = obj%num_ranges,            &
                               label      = 'HEADERS_METHOD',            &
                               iter       = i)
      !
    end do loop_thru_headers

    !
    ! - Check TIMES
    !
    if (nt1 == nt2) then
      obj%num_times = nt1
    else
      !
      call pc_error (msg1 = 'Error. Number of TIMES (',     &
                     var1 = nt1,                            &
                     msg2 = ') and TIME METHODS (',         &
                     var2 = nt2,                            &
                     msg3 = ') does not match.')
      obj%num_times = min (a1 = nt1, a2 = nt2)
      !
    end if
    !
    if (obj%num_times > max_time_slices) then
      !
      call pc_error (msg1 = 'Error. Number of TIMES (',     &
                     var1 = obj%num_times,                &
                     msg2 = ') is greater than limit (',    &
                     var2 = max_time_slices,                      &
                     msg3 = ').  Setting to limit.')
      obj%num_times = max_time_slices
      !
    end if
    !

    stop_time = obj%tstrt + real (obj%ndpt - 1) * obj%dt
    !
  loop_thru_times:   &
    do i = 1, obj%num_times
      !
    check_times:   &
      if ((obj%times (i) < obj%tstrt)     &
          .or. (obj%times (i) > stop_time)) then
        !
        write (my_line, *) 'Value for TIMES (', i, ') falls outside trace: ', &
                           obj%tstrt, ' - ', stop_time
        call pc_warning (msg1 = trim (my_line))
        !
        obj%sample_times (i) = 0
        !
      else
        !
        obj%sample_times (i) = (obj%times (i) - obj%tstrt) / obj%dt
        !
      end if check_times
      !
      !
      call slice_check_method (method     = obj%times_method (i),    &
                               num_ranges = obj%num_ranges,          &
                               label      = 'TIMES_METHOD',            &
                               iter       = i)
      !
    end do loop_thru_times

    !
    ! - Check HDR_X
    !
  check_hdr_x:   &
    if ((obj%hdr_x < 1) .or. (obj%hdr_x > obj%nwih)) then  ! 1-NWIH
      !
      call pc_error (msg1 = 'Bad value for HDR_X (',   &
                     var1 = obj%hdr_x,               &
                     msg2 = ').  Must be 1-',          &
                     var2 = obj%nwih,                &
                     msg3 = '.  Setting to default 7')
      obj%hdr_x  = 7
      !
    end if check_hdr_x

    !
    ! - Check X_INIT
    !     obj%x_init is not checked
    !

    !
    ! - Check X_INC
    !
  check_x_inc:   &
    if (obj%x_inc <= 0.0) then   ! real > 0.0
      !
      call pc_error (msg1 = 'Bad value for X_INC (',   &
                     var1 = obj%x_inc,   &
                     msg2 = ').  Must be greater than zero (0.0).')
      !
      obj%x_inc = 1.0
      !
    end if check_x_inc

    !
    ! - Check X_LAST & X_TOT
    !   
    if (pc_verify_scalar (keyword = 'X_TOT')) then
      !
    check_x_tot:   &
      if (obj%x_tot < 1) then   ! int
        !
        call pc_error (msg1 = 'Bad value for X_TOT (',   &
                       var1 = obj%x_tot,   &
                       msg2 = ').  Must be >= 1.  Resetting X_INC.')
        !
        if (obj%x_last == obj%x_init) then
          obj%x_tot = 1
        else if (obj%x_last < obj%x_init) then
          obj%x_last = obj%x_init
          obj%x_inc  = 1.0
          obj%x_tot  = 1
        else
          obj%x_tot  = 2
        end if
        !
      end if check_x_tot
      !
      obj%x_last = real (obj%x_tot - 1) * obj%x_inc + obj%x_init
      !
    else 
      !
      obj%x_tot = nint ((obj%x_last - obj%x_init) / obj%x_inc) + 1
      !
    end if

    !
    ! - Check HDR_Y
    !
  check_hdr_y:   &
    if ((obj%hdr_y < 0) .or. (obj%hdr_y > obj%nwih)) then  ! 0-NWIH
      !
      call pc_error (msg1 = 'Bad value for HDR_Y (',   &
                     var1 = obj%hdr_y,   &
                     msg2 = ').  Must be 0-',   &
                     var2 = obj%nwih,                &
                     msg3 = '.  Setting to default 8.')
      !
      obj%hdr_y = 8
      !
    end if check_hdr_y

    !
    ! - Check Y_INIT
    !   The value of Y_INIT is not checked.
    !

    !
    ! - Check Y_INC
    !
  check_y_inc:   &
    if (obj%y_inc <= 0.0) then   ! real > 0.0
      !
      call pc_error (msg1 = 'Bad value for Y_INC (',   &
                     var1 = obj%y_inc,   &
                     msg2 = ').  Must be >= zero (0.0).  Setting to 1.0')
      !
      obj%y_inc = 1.0
      !
    end if check_y_inc
    !
    ! - Check Y_LAST & Y_TOT
    !   
    if (pc_verify_scalar (keyword = 'Y_TOT')) then
      !
    check_y_tot:   &
      if (obj%y_tot < 1) then   ! int
        !
        call pc_error (msg1 = 'Bad value for Y_TOT (',   &
                       var1 = obj%y_tot,   &
                       msg2 = ').  Must be >= 1.  Resetting Y_INC.')
        !
        if (obj%y_last == obj%y_init) then
          obj%y_tot = 1
        else if (obj%y_last < obj%y_init) then
          obj%y_last = obj%y_init
          obj%y_inc  = 1.0
          obj%y_tot  = 1
        else
          obj%y_tot  = 2
        end if
        !
      end if check_y_tot
      !
      obj%y_last = real (obj%y_tot - 1) * obj%y_inc + obj%y_init
      !
    else 
      !
      obj%y_tot = nint ((obj%y_last - obj%y_init) / obj%y_inc) + 1
      !
    end if

    !!------------------- call processes internally ------------------------!!
    !!------------------- call processes internally ------------------------!!
    !!------------------- call processes internally ------------------------!!


    !!------------------------ write parameters ----------------------------!!
    !!------------------------ write parameters ----------------------------!!
    !!------------------------ write parameters ----------------------------!!


    call pc_put_control ('ntapes',       0)             ! default 0
    call pc_put_control ('need_request', .false.)       ! default false
    call pc_put_control ('need_label',   .false.)       ! default false
    call pc_put_control ('twosets',      .false.)       ! default false
    call pc_put_control ('nscratch',     0)             ! default 0
    call pc_put_control ('nstore',       0)             ! default 0
    call pc_put_control ('iftd',         .false.)       ! default false
    call pc_put_control ('ndisk',        0)             ! default 0
    call pc_put_control ('setup_only',   .false.)       ! default .false.
    !
    call pc_put_minsize_arrayset ('headers_arrayset', 1)
    call pc_put_maxsize_arrayset ('headers_arrayset', max_hdr_slices)
    !
    call pc_put_minsize_arrayset ('times_arrayset',   1)
    call pc_put_maxsize_arrayset ('times_arrayset',   max_time_slices)



    call pc_put (keyword = 'PATHNAME', scalar = obj%pathname)
    !
    call pc_put_options_field (keyword  = 'BIN_DATA_FORMAT',    &
                               options  = (/ UNFORMATTED,       &
                                             FORMATTED /),      &
                               noptions = 2)
    call pc_put (keyword = 'BIN_DATA_FORMAT', scalar = obj%bin_data_format)
    !
    call pc_put (keyword = 'HDR_FLAG', scalar = obj%hdr_flag)
    !
    nh1 = obj%num_headers
    nh2 = nh1
    call pc_put (keyword   = 'HEADERS',        &
                 array     = obj%headers,    &
                 nelements = nh1)
    call pc_put (keyword   = 'HEADERS_METHOD',        &
                 array     = obj%headers_method,    &
                 nelements = nh2)
    !
    nt1 = obj%num_times
    nt2 = nt1
    call pc_put (keyword   = 'TIMES',        &
                 array     = obj%times,    &
                 nelements = nt1)
    call pc_put (keyword   = 'TIMES_METHOD',        &
                 array     = obj%times_method,    &
                 nelements = nt2)
    !
    call pc_put (keyword = 'HDR_X',  scalar = obj%hdr_x)
    call pc_put (keyword = 'X_INIT', scalar = obj%x_init)
    call pc_put (keyword = 'X_INC',  scalar = obj%x_inc)
    call pc_put (keyword = 'X_LAST', scalar = obj%x_last)
    call pc_put (keyword = 'X_TOT',  scalar = obj%x_tot)
    call pc_put (keyword = 'HDR_Y',  scalar = obj%hdr_y)
    call pc_put (keyword = 'Y_INIT', scalar = obj%y_init)
    call pc_put (keyword = 'Y_INC',  scalar = obj%y_inc)
    call pc_put (keyword = 'Y_LAST', scalar = obj%y_last)
    call pc_put (keyword = 'Y_TOT',  scalar = obj%y_tot)
    !
    bin_count = obj%x_tot * obj%y_tot
    !
    call pc_put (keyword = 'BIN_COUNT',   scalar = bin_count)
    !
    obj%num_bins = obj%num_headers + obj%num_times + obj%num_ranges
    store_count    = bin_count * obj%num_bins
    !
    call pc_put (keyword = 'STORE_COUNT', scalar = store_count)


    !!--------------------- prepare for execution --------------------------!!
    !!--------------------- prepare for execution --------------------------!!
    !!--------------------- prepare for execution --------------------------!!


!<execute_only>

    if (pc_do_not_process_traces()) return
    obj%skip_wrapup = .false.

    ! attempt to get logical unit for output file
    call getlun(obj%out_lun, status)
    call pc_warning("slice:DEBUG: out_lun=",obj%out_lun)

    if (status == 0) then
       open(unit = obj%out_lun, action = 'WRITE', iostat = status, &
         file = obj%pathname)
!???     status = 'REPLACE', recl=132)
!???     status = 'REPLACE', recl = MAX_LINE)
       if(status /= 0) then
         call pc_error('slice: Fortran open iostat=',status, &
           ', file=' // obj%pathname)
         return
       end if
       call pc_warning("slice:DEBUG: open status=",status)
    else
      call pc_error('getlun failed, can not open Fortran print file')
      return
    end if
    !
    call memman_allocate(obj%bin_accum,obj%num_bins,obj%x_tot,obj%y_tot,status,&
                         "bin_accum")
    if(status.ne.0)then
      call pc_error('SLICE-->Unable to allocate bin_accum')
      return
    endif
    call memman_allocate(obj%bin_count,obj%x_tot,obj%y_tot,status,"bin_count")
    if(status.ne.0)then
      call pc_error('SLICE-->Unable to allocate bin_count')
      return
    endif
!rev14    call mem_alloc (parray3 = obj%bin_accum,    &
!                    n1      = obj%num_bins,     &
!                    n2      = obj%x_tot,        &
!                    n3      = obj%y_tot)
!    call mem_alloc (parray2 = obj%bin_count,    &
!                    n1      = obj%x_tot,        &
!rev14                    n2      = obj%y_tot)

    if (pc_do_not_process_traces()) return   ! in case of allocation errors.

    obj%bin_accum          = 0.0
    obj%bin_count          = 0
    obj%num_flagged_traces = 0
    obj%num_traces         = 0

!</execute_only>


    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!


    !
  end subroutine slice_update


  !!------------------------------- traps ----------------------------------!!
  !!------------------------------- traps ----------------------------------!!
  !!------------------------------- traps ----------------------------------!!

  !
  ! INDX is a Fortran-style index (=1 for the first array element).
  ! ACTION is PC_INSERT or PC_REMOVE or PC_MODIFY.
  ! INDX refers to the array element inserted or removed or modified.
  !
  subroutine  slice_method_element_trap (keyword,indx,action)
    !
    ! - Arguments
    !
    character(len=*), intent (in) :: keyword           ! arguments
    integer,          intent (in) :: indx              ! arguments
    integer,          intent (in) :: action            ! arguments
    !
    ! - Begin  slice_method_element_trap
    !
    if (action /= PC_REMOVE) then
      if (keyword == 'HEADERS_METHOD') then
        if (object%headers_method (indx) == '       ') then
          object%headers_method (indx) = RANGE
        end if
      else if (keyword == 'TIMES_METHOD') then
        if (object%times_method (indx) == '       ') then
          object%times_method (indx) = RANGE
        end if
      else
        call pc_error ('slice_headers_meth_element_trap: Unknown action: '   &
                        // keyword)
      end if
    end if
    !
  end subroutine slice_method_element_trap


  subroutine  slice_headers_element_trap (keyword,indx,action)
    !
    ! - Arguments
    !
    character(len=*), intent (in) :: keyword           ! arguments
    integer,          intent (in) :: indx              ! arguments
    integer,          intent (in) :: action            ! arguments
    !
    ! - Begin  slice_headers_element_trap
    !
    if (action /= PC_REMOVE) then
      if (object%headers (indx) < 1) then
        object%headers (indx) = 1
      else if (object%headers (indx) > HDR_BOTTOM_MUTE) then
        object%headers (indx) = HDR_BOTTOM_MUTE
      end if
    end if
    !
  end subroutine slice_headers_element_trap


  subroutine  slice_times_element_trap (keyword,indx,action)
    !
    ! - Arguments
    !
    character(len=*), intent (in) :: keyword           ! arguments
    integer,          intent (in) :: indx              ! arguments
    integer,          intent (in) :: action            ! arguments
    !
    ! - Local variables
    !
    real :: end_time
    !
    ! - Begin  slice_times_element_trap
    !
    if (action /= PC_REMOVE) then
      if (object%times (indx) < object%tstrt) then
        object%times (indx) = object%tstrt
      else 
        end_time = object%tstrt + (object%ndpt - 1) * object%dt
        if (object%times (indx) > end_time) then
          object%times (indx) = end_time
        end if
      end if
    end if
    !
  end subroutine slice_times_element_trap

  !!------------------------- slice_check_method ---------------------------!!
  !!------------------------- slice_check_method ---------------------------!!
  !!------------------------- slice_check_method ---------------------------!!

  subroutine slice_check_method (method, num_ranges, label, iter)
    !
    ! - Arguments
    !
    character (len = *), intent (inout) :: method
    integer,             intent (inout) :: num_ranges
    character (len = *), intent (in)    :: label
    integer,             intent (in)    :: iter
    !
    ! - Begin slice_check_method
    !
    call string_to_upper (method)
    !
    select case (method)
    case ('D', 'R', 'RA', 'RAN', RANGE) 
      num_ranges = num_ranges + 1
      method     = RANGE
    case (AVERAGE, MAXIMUM, MINIMUM, SUM) 
    case ('A      ', 'AV     ', 'AVE    ') 
      method = AVERAGE
    case ('B      ', 'X      ', 'MA     ', 'MX     ', 'MAX    ')
      method = MAXIMUM
    case ('C      ', 'N      ', 'MI     ', 'MN     ', 'MIN    ')
      method = MINIMUM
    case ('E      ', 'S      ', 'SU     ') 
      method = SUM
    case default 
      call pc_error (msg1 = 'Bad value for ' // label // ' (',   &
                     var1 = iter,                                &
                     msg2 = '), ' // trim (method))
      method = RANGE
      !
    end select 
    !
  end subroutine slice_check_method


  !!--------------------------- main execution -----------------------------!!
  !!--------------------------- main execution -----------------------------!!
  !!--------------------------- main execution -----------------------------!!


!<execute_only>

  subroutine slice (obj,ntr,hd,tr)
    !
    ! - Arguments
    !
    type (slice_struct), intent (inout) :: obj                    ! arguments
    integer,             intent (inout) :: ntr                    ! arguments
    double precision,    intent (inout) :: hd (:,:)               ! arguments
    real,                intent (inout) :: tr (:,:)               ! arguments
    !
    ! - Local variables
    !
    character (len = 7) :: method
    double precision :: value
    integer :: i
    integer :: j
    integer :: jt
    integer :: k
    integer :: x_bin_num
    integer :: y_bin_num

    !
    ! - Begin slice
    !
  loop_thru_ntr:    &
    do i = 1, ntr
      !
      ! - Skip traces that are not flagged.
      !
      obj%num_traces = obj%num_traces + 1
      !
      if (obj%hdr_flag > 0) then
         if (hd (obj%hdr_flag, i) == 0.0d0) cycle loop_thru_ntr
      end if
      !
      obj%num_flagged_traces = obj%num_flagged_traces + 1
      !
      x_bin_num=mth_bin_number(dble(obj%x_init),dble(obj%x_inc),hd(obj%hdr_x,i))

!rev12      x_bin_num = nint ((hd (obj%hdr_x, i)    &
!rev12                         - obj%x_init)        &
!rev12                         / obj%x_inc)         &
!rev12                  + 1
      if (x_bin_num < 1) then
        cycle loop_thru_ntr
      else if (x_bin_num > obj%x_tot) then
        cycle loop_thru_ntr
      end if
      !
      if (obj%hdr_y > 0) then
        y_bin_num=mth_bin_number(dble(obj%y_init),dble(obj%y_inc),&
                                 hd(obj%hdr_y,i))

!rev12        y_bin_num = nint ((hd (obj%hdr_y, i)    &
!rev12                           - obj%y_init)        &
!rev12                           / obj%y_inc)         &
!rev12                  + 1
        if (y_bin_num < 1) then
          cycle loop_thru_ntr
        else if (y_bin_num > obj%y_tot) then
          cycle loop_thru_ntr
        end if
        !
      else
        y_bin_num = 1
      end if
      !
      obj%bin_count(x_bin_num, y_bin_num)=obj%bin_count(x_bin_num, y_bin_num)+1
        
      k = 1
      !
    loop_thru_slices:   &
      do j = 1, obj%num_headers + obj%num_times
        !
        if (j <= obj%num_headers) then
          !
          value  = hd (obj%headers        (j), i)
          method =     obj%headers_method (j)
          !
        else
          !
          jt = j - obj%num_headers
          !
          if (obj%sample_times (jt) > 0) then
            value = dble (tr (obj%sample_times (jt), i))
          else
            cycle loop_thru_slices
          end if
          !
          method = obj%times_method (jt)
          !
        end if
        !
        select case (method)

        case (AVERAGE, SUM)
          obj%bin_accum (k, x_bin_num, y_bin_num)     &
            = obj%bin_accum (k, x_bin_num, y_bin_num) + value

        case (MINIMUM)
          if (obj%bin_count (x_bin_num, y_bin_num) == 1) then
            obj%bin_accum (k, x_bin_num, y_bin_num) = value
          else
            obj%bin_accum (k, x_bin_num, y_bin_num)                   &
              = min (a1 = obj%bin_accum (k, x_bin_num, y_bin_num),    &
                     a2 = value)
          end if

        case (MAXIMUM)
          if (obj%bin_count (x_bin_num, y_bin_num) == 1) then
            obj%bin_accum (k, x_bin_num, y_bin_num) = value
          else
            obj%bin_accum (k, x_bin_num, y_bin_num)                   &
              = max (a1 = obj%bin_accum (k,x_bin_num, y_bin_num),    &
                     a2 = value)
          end if

        case (RANGE)
          if (obj%bin_count (x_bin_num, y_bin_num) == 1) then
            obj%bin_accum (k, x_bin_num, y_bin_num) = value
            k = k + 1
            obj%bin_accum (k, x_bin_num, y_bin_num) = value
          else
            obj%bin_accum (k, x_bin_num, y_bin_num)                   &
              = min (a1 = obj%bin_accum (k, x_bin_num, y_bin_num),    &
                     a2 = value)
            k = k + 1
            obj%bin_accum (k, x_bin_num, y_bin_num)                   &
              = max (a1 = obj%bin_accum (k, x_bin_num, y_bin_num),    &
                     a2 = value)
          end if

        case default
          call pc_error ('slice: Cannot interpret mode '    &
                         // obj%headers_method (j))
          ntr = FATAL_ERROR
          return
        end select
        !
        k = k + 1
        !
      end do loop_thru_slices
      !
    end do loop_thru_ntr
    !
    if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
      call slice_wrapup (obj)
    end if
    !
  end subroutine slice

!</execute_only>


  !!------------------------------- wrapup ---------------------------------!!
  !!------------------------------- wrapup ---------------------------------!!
  !!------------------------------- wrapup ---------------------------------!!


!<execute_only>

  subroutine slice_wrapup (obj)
    !
    ! - Arguments
    !
    type (slice_struct), intent (inout) :: obj       ! arguments
    !
    ! - Local variables
    !
    character (len =  7) :: method
    character (len =  8) :: my_date
    character (len = 10) :: my_time
    character (len = 20) :: type
    character (len = 25 * (obj%num_bins + 2)) :: datastr
    integer :: i

    integer :: j
    integer :: jt
    integer :: k
    integer :: num_binned_traces
    integer :: num_bins
    integer :: x
    integer :: y
    real    :: xloc
    real    :: yloc
    !
    ! - Begin slice_wrapup
    !
    call pc_warning("slice:DEBUG: entering slice_wrapup")
    if (obj%skip_wrapup) return
    obj%skip_wrapup = .true.
    call pc_warning("slice:DEBUG: processing slice_wrapup")
    !
    write(obj%out_lun,'(a)') "#                    SLICE file "
    write(obj%out_lun,'(a)') "#                    Job " // obj%job_name
    write(obj%out_lun,'(a)') "# "
    !
    call date_and_time (date = my_date, time = my_time)
    !
    write(obj%out_lun,'(a)') "#    Run date: "  &
       // my_date (1:4) // '-'   &
       // my_date (5:6) // '-'   &
       // my_date (7:8)
    write(obj%out_lun,'(a)') "#    Run time: " &
      // my_time (1:2) // ':'    &
      // my_time (3:4) // ':'    &
      // my_time (5:6)
    write(obj%out_lun,'(a)') "#    File:     " // trim (obj%pathname)
    write(obj%out_lun,'(a)') "# "
    !
    write(obj%out_lun,'(a)') "# ----------------------"
    write(obj%out_lun,'(a)') "# - Type of extraction -"
    write(obj%out_lun,'(a)') "# ----------------------"
    write(obj%out_lun,'(a)') "# "
    !
    write(obj%out_lun,'(a,i4,a,i4)') &
      "# Grid Headers -- X ", obj%hdr_x, "   Y ", obj%hdr_y
    !
    write(obj%out_lun,'(a)') "# "
    !
    write(obj%out_lun,'(a)') "#        Headers       Extraction "
    write(obj%out_lun,'(a)') "#       Extracted         Mode    "
    write(obj%out_lun,'(a)') "#       ---------      ---------- "
    !
    do i = 1, obj%num_headers
      write(obj%out_lun,'(a,i4,a,i8,9x,a)') &
        "#  ", i, ". ", obj%headers(i), obj%headers_method (i)
    end do
    !
    write(obj%out_lun,'(a)') "# "
    write(obj%out_lun,'(a)') "#         Times        Extraction "
    write(obj%out_lun,'(a)') "#       Extracted         Mode    "
    write(obj%out_lun,'(a)') "#       ---------      ---------- "
    write(obj%out_lun,'(a)') "# "
    !
    do i = 1, obj%num_times
      write(obj%out_lun,'(a,i4,a,f8.2,9x,a)') &
        "#  ", i, ". ", obj%times(i), trim(obj%times_method (i))
    end do
    !
    write(obj%out_lun,'(a)') "# "
    write(obj%out_lun,'(a)') "# ----------------------------"
    write(obj%out_lun,'(a)') "# Data format: " // obj%bin_data_format
    write(obj%out_lun,'(a)') "# "
    write(obj%out_lun,'(a)') "# ----------------------------"
    !
    num_binned_traces = 0
    num_bins          = 0
    !
    ! - List the bin details
    !
    yloc = obj%y_init
    !
  loop_y:    &
    do y = 1, obj%y_tot
      !
      xloc = obj%x_init
      !
    loop_x:    &
      do x = 1, obj%x_tot
        !
        !
        if (obj%bin_count (x, y) == 0) cycle loop_x
        !
        xloc    = obj%x_init     &
                  + (real (x - 1) * obj%x_inc)
        datastr = ' '
        !
        if (obj%bin_data_format == FORMATTED) then
          call slice_write_item (item = "X",   data_line = datastr)
          call slice_write_item (item = xloc,  data_line = datastr)
          call slice_write_item (item = "  Y", data_line = datastr)
          call slice_write_item (item = yloc,  data_line = datastr)
          !
          call slice_write_item (item      = "  Count",    &
                                 data_line = datastr)
          call slice_write_item (item      = obj%bin_count (x, y),    &
                                 data_line = datastr)
          write(obj%out_lun,'(a)') trim (datastr)
          datastr = ' '
        else
          call slice_write_item (item = xloc, data_line = datastr)
          call slice_write_item (item = yloc, data_line = datastr)
          !
          call slice_write_item (item      = obj%bin_count (x, y),    &
                                 data_line = datastr)
        end if
        !
        k                 = 1
        num_binned_traces = num_binned_traces + obj%bin_count (x, y)
        num_bins          = num_bins + 1
        !
      loop_thru_slices:   &
        do j = 1, obj%num_headers + obj%num_times
          !
          type = ' '
          !
          if (j <= obj%num_headers) then
            method = obj%headers_method (j)
            call slice_write_item (item = ' Header',         data_line = type)
            call slice_write_item (item = obj%headers (j), data_line = type)
          else
            jt = j - obj%num_headers
            method = obj%times_method (jt)
            call slice_write_item (item = ' Time',          data_line = type)
            call slice_write_item (item = obj%times (jt), data_line = type)
          end if
          !
          select case (method)
  
          case (AVERAGE)
            call slice_write_item     &
                   (item      = obj%bin_accum (k, x, y)    &
                                / dble (obj%bin_count (x, y)),    &
                    data_line = datastr)

          case (SUM, MINIMUM, MAXIMUM)
            call slice_write_item (item      = obj%bin_accum (k, x, y),    &
                                   data_line = datastr)

          case (RANGE)
            call slice_write_item (item      = obj%bin_accum (k, x, y),    &
                                   data_line = datastr)
            k = k + 1
            call slice_write_item (item      = obj%bin_accum (k, x, y),    &
                                   data_line = datastr)

          case default
            call pc_error ('slice: Cannot interpret mode '    &
                           // obj%headers_method (j))
            return
          end select
          !
          if (obj%bin_data_format == FORMATTED) then
            write(obj%out_lun,'(a)') "      " // trim (datastr) // "   " &
              // trim (method) // trim (type)
            datastr = ' '
          end if
          !
          k = k + 1
          !
        end do loop_thru_slices
        !
        write(obj%out_lun,'(a)') trim (datastr)
        !
      end do loop_x
      !
      yloc = yloc + obj%y_inc
      !
    end do loop_y
    !
    write(obj%out_lun,'(a)') " "

    close(unit=obj%out_lun)
    !
    ! - Report stats
    !
    call pc_print ('SLICE: Read   ', obj%num_traces, ' traces.')
    if (obj%hdr_flag > 0) then
      call pc_print ('SLICE: Found  ', obj%num_flagged_traces,    &
                     ' flagged traces.')
    end if
    call pc_print ('SLICE: Binned ', num_binned_traces,    &
                   ' traces, in ', num_bins, ' bins.')
    call pc_print ('SLICE: Grid is ', obj%x_tot,    &
                   ' X by ', obj%y_tot, 'Y.')
    call pc_print ('SLICE:         ', obj%x_tot * obj%y_tot,' total bins.')
    call pc_print ('SLICE: Results written to file:')
    call pc_print ('SLICE:   ' // trim (obj%pathname))
    !
  end subroutine slice_wrapup

  !!-------------------------- slice_write_item ----------------------------!!
  !!-------------------------- slice_write_item ----------------------------!!
  !!-------------------------- slice_write_item ----------------------------!!


  subroutine slice_write_item_dp (item, data_line)
    !
    ! - Arguments
    !
    double precision,    intent (in)    :: item
    character (len = *), intent (inout) :: data_line
    !
    ! - Local variables
    !
    character (len = 24) :: datastr

    integer :: i
    integer :: k
    integer :: start
    !
    ! - Begin slice_write_item_dp
    !
    write (datastr, "(f24.6)") item
    datastr = adjustl (string = datastr)
    !
    start = len_trim (data_line) + 1     ! Pad each field with one space
    k = 1
    !
    do i = start + 1, start + len_trim (string = datastr)
      !
      data_line (i:i) = datastr (k:k)
      k               = k + 1
      !
    end do
    !
  end subroutine slice_write_item_dp


  subroutine slice_write_item_r (item, data_line)
    !
    ! - Arguments
    !
    real,                intent (in)    :: item
    character (len = *), intent (inout) :: data_line
    !
    ! - Local variables
    !
    character (len = 24) :: datastr

    integer :: i
    integer :: k
    integer :: start
    !
    ! - Begin slice_write_item_r
    !
    write (datastr, "(f24.6)") item
    datastr = adjustl (string = datastr)
    !
    start = len_trim (data_line) + 1     ! Pad each field with one space
    k = 1
    !
    do i = start + 1, start + len_trim (string = datastr)
      !
      data_line (i:i) = datastr (k:k)
      k               = k + 1
      !
    end do
    !
  end subroutine slice_write_item_r


  subroutine slice_write_item_i (item, data_line)
    !
    ! - Arguments
    !
    integer,             intent (in)    :: item
    character (len = *), intent (inout) :: data_line
    !
    ! - Local parameters
    !
    character (len = 12) :: datastr

    integer :: i
    integer :: k
    integer :: start
    !
    ! - Begin slice_write_item_i
    !
    write (datastr, "(i12)") item
    datastr = adjustl (string = datastr)
    !
    start = len_trim (data_line) + 1     ! Pad each field with one space
    k = 1
    !
    do i = start + 1, start + len_trim (string = datastr)
      !
      data_line (i:i) = datastr (k:k)
      k               = k + 1
      !
    end do
    !
  end subroutine slice_write_item_i


  subroutine slice_write_item_c (item, data_line)
    !
    ! - Arguments
    !
    character (len = *), intent (in)    :: item
    character (len = *), intent (inout) :: data_line
    !
    ! - Local variables
    !

    integer :: i
    integer :: k
    integer :: start
    !
    ! - Begin slice_write_item_c
    !
    start = len_trim (data_line) + 1     ! Pad each field with one space
    k = 1
    !
    do i = start + 1, start + len_trim (string = item)
      !
      data_line (i:i) = item (k:k)
      k               = k + 1
      !
    end do
    !
  end subroutine slice_write_item_c

!</execute_only>


  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!


end module slice_module


!!---------------------------------- end -----------------------------------!!
!!---------------------------------- end -----------------------------------!!
!!---------------------------------- end -----------------------------------!!

