!<CPS_v1 type="PRIMITIVE"/>
!!------------------------- histogram.f90 --------------------------------!!
!!------------------------- histogram.f90 --------------------------------!!
!!------------------------- histogram.f90 --------------------------------!!

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
! Name       : histogram
! Category   : math
! Written    : 2000-06-07   by: Randy L. Selzler, Data-Warp, Inc.
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Accumulate and optionally print histogram.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! Accumulate a histogram from a window that passes over the input data.
! The results can be returned as values or printed to a logical unit.
!
! The center and range of bins are dynamically computed,
! if they are not explicitly defined when histogram_create is called.
! If dynamically computed, the histogram is only an approximation.
! The caller can control the quality and expense of the approximation
! by specifying max_dev, min_sub, max_sub and/or log_slots.
!
! If the median and/or range is known apriori, they should be specified.
! Otherwise the defaults are recommended for histogram_create arguments,
! except possibly for the number of bins in the final result.
! This is because knowledge of the distribution (a histogram) is
! required to choose good values for the center and range.
!
! Perhaps the worst case for dynamic computation of the distribution
! is when the median or standard deviation changes dramaticly half
! way through the input data.
!
! If the histogram is printed, the caller can control the size and
! orientation of the displayed graph.
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
!                          CALLING SEQUENCE
!
! histogram_create must be called first to get a histogram object.
! histogram_compute must be called one or more times to accumulate input data.
! histogram_print, dump or result is called to obtain histogram results.
! histogram_quantile estimates the data value at a fractional population.
! histogram_clear may be called to purge previous input for a fresh start.
! histogram _delete should be called to release memory.
!
! At least two "live" input points must be input to define a histogram.
! "Live" implies non-zero points, if "ignore"  zero values is true.
! The compute subroutine is overloaded to accept either vectors or scalars.
! Intermediate histograms may be obtained while data is being accumulated.
! Print and result histograms are normalized as specified by create.
! Dump is used to see the raw un-normalized histogram (for debugging).
!
!                             opt    opt     opt    opt
!                        o     i      i       i      i
! call histogram_create(obj, slots, center, range, ignore,
!                         opt      opt      opt      opt      opt
!                          i        i        i        i        i
!                       min_dev, max_dev, min_sub, max_sub, log_slots)
!
!                         b     i
! call histogram_compute(obj, vector)
!
!                         b     i
! call histogram_compute(obj, scalar)
!
!                                  opt     opt   opt     opt        opt
!                       b    i      l       i     i       i          i
! call histogram_print(obj, lun, verbose, nchar, title, ordinate, abscissa)
!
!                                  opt     opt   opt     opt        opt
!                       b    i      l       i     i       i          i
! call histogram_dump (obj, lun, verbose, nchar, title, ordinate, abscissa)
!
!                              opt     opt      opt     opt    opt
!                         b     o       o        o       o      o
! call histogram_result (obj, result, median, average, r_min, r_max,
!               opt     opt    opt    opt    opt    opt     opt      opt
!                o       o      o      o      o      o       o        o
!              center, range, total, zeros, n_min, n_max, n_below, n_above)
!
!                        i       i
! value = histogram_quantile (obj, quantile)
!
!                        b
! call histogram_clear(obj)
!
!                        b
! call histogram_delete(obj)
!
!
! histogram_struct pointer   obj       = histogram structure object pointer.
!
! integer                    slots     = number of bins in final histogram.
!                                        If specified in histogram_create,
!                                        it must be greater than 0.
!                                        If not specified, the default is 65.
!
! real                       center    = central value for the histogram.
!                                        If specified in create, the center
!                                        of the histogram is forced to it.
!                                        If not specified, the median is
!                                        estimated and used for the center.
!
!                                        Note: when the center equals the
!                                        data's median, the maximum number
!                                        of points are binned for a given
!                                        range.
!
! real                       range     = width of final histogram as measured
!                                        between extreme edges of bins.
!                                        If specified in histogram_create,
!                                        it must be greater than 0.0.
!                                        If not specified, it is computed
!                                        (min_dev * standard_deviation).
!
!                                        Note: this range is divided into
!                                        bins (slots count) of equal size.
!
! logical                    ignore    = .true. iff input points with a zero
!                                        value should be ignored.
!                                        If not specified, default is .false..
!
! real                       min_dev   = minimum range (final histogram width),
!                                        in units of one standard deviation.
!                                        If specified in histogram_create,
!                                        it must be greater than 0.
!                                        If not specified, it defaults to 3.
!
!                                        If range is not specified in the
!                                        histogram_create call, then min_dev
!                                        is used to dynamically compute range.
!
!                                        Note: if the data has gaussian
!                                        distribution, then one standard
!                                        deviation from the median is the
!                                        inflection point for the curve
!                                        (transistion from concave to convex).
!
!                                        Note: 3 standard deviations captures
!                                        most data in a typical histogram.
!
! real                       max_dev   = maximum range (trial histogram width),
!                                        in units of one standard deviation.
!                                        If specified in create, it must be
!                                        greater than or equal to min_dev.
!                                        If not specified, it defaults to 10
!                                        times the min_dev.
!
!                                        Note: larger values of max_dev allow
!                                        the median to drift or the standard
!                                        deviation to increase before the
!                                        trial histogram is renormalized.
!
! integer                    min_sub   = minimum sub sampling for trial.
!                                        If specified in histogram_create,
!                                        it must be greater than 0.
!                                        If not specified, it defaults to 10.
!
!                                        Note: larger values of min_sub
!                                        increase the accuracy of the median
!                                        estimated from the trial histogram.
!
! integer                    max_sub   = maximum sub sampling for trial.
!                                        If specified in create, it must be
!                                        greater than or equal to min_sub.
!                                        If not specified, it defaults to 10
!                                        times the min_sub.
!
!                                        Note: larger values of max_sub allow
!                                        the standard deviation to decrease
!                                        before renormalizing the trial.
!
! integer                    log_slots = number of logarithmic bins in the
!                                        trial histogram beyond max_dev.
!                                        If specified in histogram_create,
!                                        it must be greater than 0.
!                                        If not specified, it defaults to 100.
!
!                                        Note:These bins characterize data
!                                        that lies outside of max_dev.
!                                        Larger values of log_slots increase
!                                        trial histogram accuracy when the
!                                        data distribution changes dramatically
!                                        during input.
!
!                                        Note: if center and range are both
!                                        specified in the histogram_create,
!                                        then the trial histogram is never
!                                        renormalized and some statistics
!                                        are exact instead of approximated.
!                                        If either center, range or both are
!                                        not specified, then min_dev, max_dev,
!                                        min_sub and max_sub determine when
!                                        the trial histogram is renormalized.
!
!                                        Note: Larger values improve accuracy,
!                                        but increase memory requirements.
!
! real                       scalar    = input data for histogram analysis.
!                                        One point only (convenience routine).
!
! real, dimension(:)         vector    = input data for histogram analysis.
!                                        Number of points = size(vector).
!
! logical                    verbose   = Verbose statistics if .true.
!                                        Default is .false.
!
! integer                    lun       = print Fortran logical unit number.
!
! integer                    nchar     = character length of ordinate bars.
!                                        Default is 50.
!                                        Minimum is 10, maximum is 100.
!
! character(len=*)           title     = printed title for graph.
!                                        Default is "<undefined>"
!
! character(len=*)           ordinate  = printed label for ordinate.
!                                        Default is "Number of points in bin."
!
! character(len=*)           abscissa  = printed label for abscissa.
!                                        Default is "Center of bin"
!
! real, dimension(:), pointer result   = histogram output value (estimated).
!                                        The nominal value for size(result)
!                                        is defined by slots.
!
!                                        Note: the caller is responsible for
!                                        deallocating result memory as needed.
!
!                                        Note: estimates are "exact", if
!                                        center and range were specified
!                                        when histogram_create was called.
!
! real                       median    = median value (approximate).
!                                        Optimum center for histogram.
!
! real                       average   = mean value (exact).
!
! real                       r_min     = minimum value (exact).
!
! real                       r_max     = maximum value (exact).
!
! double                     total     = number of points analyzed (exact).
!                                        Excludes zeros iff "ignore" is true.
!
! double                     zeros     = count of zero input points (exact).
!                                        Accumulated iff "ignore" is true.
!
! double                     n_max     = maximum number of data points
!                                        falling within one bin (estimated).
!
! double                     n_min     = minimum number of data points
!                                        falling within one bin (estimated).
!
! double                     n_below   = number of data points less than
!                                        minimum histogram bin (estimated).
!
! double                     n_above   = number of points greater than
!                                        maximum histogram bin (estimated).
!
! real                       value     = estimated value, given a quantile
!
! real                       quantile  = quantile within histogram population.
!                                        Valid range for argument is 0.0 to 1.0.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! Use the defaults, except possibly for slots (bin count) in create.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!007. 2006-09-18  D. Glover    Added NULLIFY statements for Intel compiler.
!  6. 2006-01-10  B. Menger    Removed Unused Variables.
!  5. 2005-01-17  Goodger      Added a check for "almost" zero in the
!                              relocation subroutine.  The pop variable
!                              was a very small negative number rather
!                              than zero, and was causing the routine to
!                              get into an infinite loop.
!  4. 2001-04-30  Selzler      Corrected inout on argument declaration
!  3. 2001-03-13  Selzler      Corrected closing header_word_doc tag.
!  2. 2000-08-02  Selzler      Initial release
!  1. 2000-06-07  Selzler      Initial version.
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
!                      SPECIAL COMPILING REQUIREMENTS
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! An over-sampled trial histogram is created while data is input.
! The trial histogram is created with an assumed median and deviation.
! A running estimate is made of the distribution as more data is input.
! If the estimate drifts far from the nominal median and deviation that was
! assumed for the trial, then it must be renormalized to maintain accuracy.
! Normalization uses linear interpolation to construct a new trial histogram
! from the previous one.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! The trial histogram is accumulated in the bin array.
! It is over sampled and covers a broader range than the
! final histogram requested by the histogram_create caller.
!
! The trial histogram is divided into three sections: upper, middle, lower.
! The bins in the middle section have an equal linear spacing.
! The final histogram falls within the linear middle section.
! The upper and lower sections have a logarithmic spacing.
! They capture outliers when the distribution varies radically.
!
! Diagram of number line and relation to histogram terminology.
!
!                      +infinity
!                          |
! +huge(double precision) =|=
!                          |
!     +HUGE_REAL ==========|== upper limit for dynamic range
!                          |   bin(upper_max), upper overflow
!          huge_upper =====|== r_max + delta
!                          |   ... upper logarithmic bins
!                          |   bin(upper_min)
!          upper_edge =====|====== center + max_dev / 2
!                          |       bin(middle_max)
!                          |       ... middle linear bins
!                  center =|========== center of histogram
!                          |       ... middle linear bins
!                          |       bin(middle_min)
!          lower_edge =====|====== center - max_dev / 2
!                          |   bin(lower_max)
!                          |   ... lower logarithmic bins
!          huge_lower =====|== r_min - delta
!                          |   bin(lower_min), lower overflow
!     -HUGE_REAL ==========|== lower limit for dynamic range
!                          |
! -huge(double precision) =|=
!                          |
!                      -infinity
!
!-------------------------------------------------------------------------------
!</programming_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!

      module histogram_module
      use ameq_module
      use pc_module
      use named_constants_module
      use mem_module
      implicit none

      private
      public :: histogram_create
      public :: histogram_compute
      public :: histogram_print
      public :: histogram_dump
      public :: histogram_result
      public :: histogram_quantile
      public :: histogram_delete
      public :: histogram_clear

      character(len=100),public,save :: histogram_IDENT = &
       '$Id: histogram.f90,v 1.7 2006/09/18 13:32:49 Glover prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!

      real :: REAL_PROTO

      integer, parameter          :: MIN_NCHAR = 10
      integer, parameter          :: MAX_NCHAR = 100
      integer, parameter          :: NOMINAL_NCHAR = 50
      integer, parameter          :: NOMINAL_SLOTS = 65
      real, parameter             :: NOMINAL_MIN_DEV = 3.0
      real, parameter             :: NOMINAL_MAX_DEV_RATIO = 10.0
      integer, parameter          :: NOMINAL_MIN_SUB = 10
      real, parameter             :: NOMINAL_MAX_SUB_RATIO = 10.0
      integer, parameter          :: NOMINAL_LOG_SLOTS = 100
      double precision, parameter :: HUGE_REAL = huge(REAL_PROTO)

      ! should be a parameter, but fortran does not understand
      ! that expression only containing constants are also constant
      double precision            :: HUGE_REAL_SPACING
                                     ! == 8 * spacing(HUGE_REAL)

      type,public :: histogram_struct
      private
      logical :: explicit_slots      ! .true. iff defined in history_create
      logical :: explicit_center     ! .true. iff defined in history_create
      logical :: explicit_range      ! .true. iff defined in history_create
      logical :: explicit_exact      ! .true. iff center and range are .true.

      logical          :: ignore     ! .true. iff zero points are ignored

      double precision :: min_dev    ! final histogram range factor
      double precision :: max_dev    ! trial histogram range factor
      double precision :: min_sub    ! minimum bin sub-sampling
      double precision :: max_sub    ! maximum bin sub-sampling
      integer          :: log_slots  ! bin count for logrithmic ranges.
                                     ! Count includes the overflow bin.

      integer          :: slots      ! final bin count
      double precision :: center     ! final center of histogram
      double precision :: range      ! final range at min_dev

      integer          :: bin_slots         ! == number of bin slots (total)
      integer          :: upper_slots       ! == number of upper  log    bins
      integer          :: middle_slots      ! == number of middle linear bins
      integer          :: lower_slots       ! == number of lower  log    bins

      integer          :: upper_max         ! == middle_max + upper_slots
      integer          :: upper_min         ! == middle_max + 1

      integer          :: middle_max        ! == lower_max + middle_slots
      integer          :: middle_min        ! == lower_max + 1

      integer          :: lower_max         ! == lower_slots
      integer          :: lower_min         ! == 1

      integer          :: normalized        ! histogram normalization count

      double precision :: min_range         ! middle_range * min_sub / max_sub
      double precision :: huge_edge         ! Limit on upper and lower edge.
                                            ! Reserves dynamic range for
                                            ! logrithmic bins on either side
                                            ! of linear center bins.
      double precision :: huge_upper        ! lower bound of bin(upper_max).
      double precision :: huge_lower        ! upper bound of bin(lower_min).

      double precision, dimension(:), pointer :: bin  ! Histogram bin array.
                                            ! Each bin may be populated by
                                            ! zero or more data points.
                                            ! Fractional points are allowed.
                                            ! The points are assumed to be
                                            ! uniformly distributed within it.

      double precision :: upper_range       ! == HUGE_REAL  - upper_edge
      double precision :: upper_size        ! logarithmic bin size
      double precision :: upper_overflow    ! overflow bin size
      double precision :: upper_edge        ! linear & logarithmic boundary
      double precision :: upper_base        ! where log(edge - base) >= 1.0.

      double precision :: middle_range      ! == upper_edge - lower_edge
      double precision :: middle_size       ! linear bin size

      double precision :: lower_range       ! == lower_edge - HUGE_REAL
      double precision :: lower_size        ! logarithmic bin size
      double precision :: lower_overflow    ! overflow bin size
      double precision :: lower_edge        ! linear & logarithmic boundary
      double precision :: lower_base        ! where log(base - edge) >= 1.0.

      double precision :: sum               ! used to compute average (mean)

      double precision :: median            ! estimated data median.
                                            ! Needed to estimate optimum
                                            ! center for histogram relocation.
      integer          :: median_bin        ! bin number containing median.
                                            ! Bin population may be zero.
                                            ! Needed to estimate median.
      double precision :: median_rank       ! rank of median within median_bin.
                                            ! 0 <= rank <= bin population.
                                            ! Needed to estimate median.

      double precision :: variance          ! estimated variance about center
      double precision :: std_dev           ! estimated standard deviation
                                            ! std_dev == sqrt(variance)

      double precision :: r_min             ! minimum value used
      double precision :: r_max             ! maximum value used

      double precision :: total             ! number of points analyzed.
                                            ! Excludes zeros if "ignored".
                                            ! Initialization pending if < 2.0.
      double precision :: tally             ! number of points binned
      double precision :: zeros             ! number of zero input points.
                                            ! Accumulated iff "ignored".

      double precision :: first             ! value of first  sample used
      double precision :: second            ! value of second sample used
                                            ! Needed for initialization.

      end type histogram_struct

!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

      interface histogram_compute
        module procedure histogram_compute_vector
        module procedure histogram_compute_scalar
      end interface

!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      contains

!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!

      subroutine histogram_create (obj, slots, center, range, ignore, &
        min_dev, max_dev, min_sub, max_sub, log_slots)
      implicit none
      type(histogram_struct),pointer :: obj    ! arguments
      integer,optional,intent(in)    :: slots
      real,optional,intent(in)       :: center
      real,optional,intent(in)       :: range
      logical,optional,intent(in)    :: ignore
      real,optional,intent(in)       :: min_dev
      real,optional,intent(in)       :: max_dev
      integer,optional,intent(in)    :: min_sub
      integer,optional,intent(in)    :: max_sub
      integer,optional,intent(in)    :: log_slots

      ! initialize constant parameters
      HUGE_REAL_SPACING = 8.0 * spacing(HUGE_REAL)

      nullify(obj)

      allocate (obj)

      nullify (obj%bin)

      ! For each optional argument, test for its presents and
      ! validate the caller's value or default it.
      ! If any errors are detected, return a null object.

      if(present(slots)) then
        if(slots <= 0) then
          deallocate(obj)
          nullify(obj)
          return
        end if

        obj%explicit_slots = .true.
        obj%slots = slots
      else
        obj%explicit_slots = .false.
        obj%slots = NOMINAL_SLOTS
      end if

      if(present(log_slots)) then
        if(log_slots < 1) then
          deallocate(obj)
          nullify(obj)
          return
        end if

        obj%log_slots = log_slots
      else
        obj%log_slots = NOMINAL_log_slots
      end if

      if(present(ignore)) then
        obj%ignore = ignore
      else
        obj%ignore = .false.
      end if

      if(present(min_dev)) then
        if(min_dev <= 0.0) then
          deallocate(obj)
          nullify(obj)
          return
        end if

        obj%min_dev = min_dev
      else
        obj%min_dev = NOMINAL_MIN_DEV
      end if

      if(present(max_dev)) then
        if(max_dev < obj%min_dev) then
          deallocate(obj)
          nullify(obj)
          return
        end if

        obj%max_dev = max_dev
      else
        obj%max_dev = obj%min_dev * NOMINAL_MAX_DEV_RATIO
      end if

      if(present(min_sub)) then
        if(min_sub <= 0) then
          deallocate(obj)
          nullify(obj)
          return
        end if

        obj%min_sub = min_sub
      else
        obj%min_sub = NOMINAL_MIN_SUB
      end if

      if(present(max_sub)) then
        if(max_sub < obj%min_sub) then
          deallocate(obj)
          nullify(obj)
          return
        end if

        obj%max_sub = max_sub
      else
        obj%max_sub = obj%min_sub * NOMINAL_MAX_SUB_RATIO
      end if

      ! min_dev and max_dev MUST already be defined
      if(present(range)) then
        if(range <= 0.0 .or. &
          range > HUGE_REAL * (obj%min_dev / obj%max_dev)) then
          deallocate(obj)
          nullify(obj)
          return
        end if

        obj%explicit_range = .true.
        obj%range = range
      else
        obj%explicit_range = .false.
        obj%range = 0.0
      end if

      ! Allocate and zero memory for the obj bins.
      call histogram_alloc(obj)

      ! huge_edge is defined by histogram_alloc call
      if(present(center)) then
        if(center > + obj%huge_edge - 0.5 * obj%range .or. &
           center < - obj%huge_edge + 0.5 * obj%range) then
          ! Explicit center is too close to +/- HUGE_REAL.
          deallocate(obj%bin)
          deallocate(obj)
          nullify(obj)
          return
        end if

        obj%explicit_center = .true.
        obj%center = center
      else
        obj%explicit_center = .false.
        obj%center = 0.0
      end if

      if(obj%explicit_center .and. obj%explicit_range) then
        obj%explicit_exact = .true.
      else
        obj%explicit_exact = .false.
      end if

      ! Zero point counters
      obj%total = 0.0
      obj%tally = 0.0
      obj%zeros = 0.0

      obj%normalized = 0

      ! Meaningful values can not be defined for these values
      ! until values for the first two input points are actually seen.
      obj%upper_range = 0.0
      obj%upper_size = 0.0
      obj%upper_edge = 0.0
      obj%upper_base = 0.0

      obj%middle_range = 0.0
      obj%middle_size = 0.0

      obj%lower_range = 0.0
      obj%lower_size = 0.0
      obj%lower_edge = 0.0
      obj%lower_base = 0.0

      obj%sum = 0.0
      obj%median = 0.0
      obj%median_bin = 0
      obj%median_rank  = 0.0

      obj%variance = 0.0
      obj%std_dev = 0.0
      obj%r_min = 0.0
      obj%r_max = 0.0

      obj%first = 0.0
      obj%second = 0.0

      return
      end subroutine histogram_create

!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!

      subroutine histogram_delete (obj)
      implicit none
      type(histogram_struct),pointer :: obj       ! arguments

      call mem_free(obj%bin)

      deallocate(obj)

      return
      end subroutine histogram_delete

!!----------------------------- clear -------------------------------------!!
!!----------------------------- clear -------------------------------------!!
!!----------------------------- clear -------------------------------------!!

      subroutine histogram_clear (obj)
      implicit none
      type(histogram_struct) :: obj       ! arguments

      ! Zero point counters.
      obj%normalized = 0.0
      obj%total = 0.0
      obj%tally = 0.0
      obj%zeros = 0.0

      obj%bin = 0.0

      return
      end subroutine histogram_clear

!!--------------------- histogram_alloc ------------------------!!
!!--------------------- histogram_alloc ------------------------!!
!!--------------------- histogram_alloc ------------------------!!

      subroutine histogram_alloc(obj)
      implicit none
      type(histogram_struct),intent(inout) :: obj       ! arguments

      obj%huge_edge = HUGE_REAL - obj%log_slots * HUGE_REAL_SPACING

      ! Compute the number of slots in upper, middle and lower trial ranges.
      obj%upper_slots = obj%log_slots
      obj%middle_slots = obj%max_sub * obj%slots
      obj%lower_slots = obj%log_slots

      ! Compute number of slots in all trial bins (linear and logrithmic).
      obj%bin_slots = obj%upper_slots + obj%middle_slots + obj%lower_slots

      ! Compute the bin array subscripts corresponding to trial ranges.
      obj%lower_min  = 1
      obj%lower_max  = obj%lower_slots

      obj%middle_min = obj%lower_slots + 1
      obj%middle_max = obj%lower_slots + obj%middle_slots

      obj%upper_min  = obj%lower_slots + obj%middle_slots + 1
      obj%upper_max  = obj%lower_slots + obj%middle_slots + obj%lower_slots

      ! Allocate the bin array containing upper, middle and lower slots.
      nullify(obj%bin)
      call mem_alloc(obj%bin, obj%bin_slots)

      obj%bin = 0.0

      return
      end subroutine histogram_alloc

!!--------------------- histogram_index ------------------------!!
!!--------------------- histogram_index ------------------------!!
!!--------------------- histogram_index ------------------------!!

      function histogram_index(obj, sample)
      implicit none
      type(histogram_struct),intent(inout) :: obj       ! arguments
      double precision :: sample  ! argument

      ! Compute index (rounded toward the center) associated with sample.

      integer :: histogram_index

      if(sample > obj%upper_edge) then
        ! Sample falls within logarithmic upper bins.
        histogram_index = max(obj%lower_min, min(obj%upper_max, &
          obj%upper_min + int(log(sample - obj%upper_base) / obj%upper_size)))
      else if(sample < obj%lower_edge) then
        ! Sample falls within logarithmic lower bins.
        histogram_index = min(obj%upper_max, max(obj%lower_min, &
          obj%lower_max - int(log(obj%lower_base - sample) / obj%lower_size)))
      else
        ! Sample falls within linear middle bins.
        if(sample < obj%center) then
          histogram_index = min(obj%middle_max, &
            obj%middle_min + int((sample - obj%lower_edge) / obj%middle_size))
        else
          histogram_index = max(obj%middle_min, &
            obj%middle_max - int((obj%upper_edge - sample) / obj%middle_size))
        end if
      end if

      return

      end function histogram_index

!!--------------------- histogram_compute_scalar ------------------------!!
!!--------------------- histogram_compute_scalar ------------------------!!
!!--------------------- histogram_compute_scalar ------------------------!!

      subroutine histogram_compute_scalar (obj, scalar)
      implicit none
      type(histogram_struct),intent(inout) :: obj       ! arguments
      real, intent(in) :: scalar

      real, dimension(1) :: vector

      vector(1) = scalar

      call histogram_compute_vector(obj, vector)

      return
      end subroutine histogram_compute_scalar

!!--------------------- histogram_compute_vector ------------------------!!
!!--------------------- histogram_compute_vector ------------------------!!
!!--------------------- histogram_compute_vector ------------------------!!

      subroutine histogram_compute_vector (obj, vector)
      implicit none
      type(histogram_struct),intent(inout) :: obj       ! arguments
      real, dimension(:), intent(in) :: vector

      ! Note: opportunity for improved accuracy (not implemented yet).
      !
      ! Histograms suffer from a catch 22.
      ! They are computed to determine data distribution, but knowledge
      ! of the distribution is required to compute good histograms.
      !
      ! This problem can be reduced by preconditioning the histogram.
      ! The raw value for the first N non-zero points can be saved
      ! while computing a histogram upon them.
      ! If N = bin_slots, then preconditioning quality is proportional
      ! to the quality of the final histogram and the memory requirements
      ! for the raw data (real) is 50% of the bins (double precision).
      !
      ! The distribution determined for these initial points can be
      ! used to select parameters for a new histogram, recomputed from
      ! the raw points that were saved.

      integer :: sample_count
      integer :: start, do_sample, bin_index,knt=0
      double precision :: sample, bin_pop              

      sample_count = size(vector)

      if(obj%total < 2.0) then
        ! Histogram initialization is still pending.
        call histogram_initialize(obj, vector, start)
      else
        start = 1
      end if

      sample_loop: &
      do do_sample = start, sample_count
        sample = vector(do_sample)

        if(sample == 0.0) then
          if(obj%ignore) then
            obj%zeros = obj%zeros + 1.0
            cycle sample_loop
          end if
        end if

        obj%sum = obj%sum + sample  ! needed to compute average (mean)
        obj%total = obj%total + 1.0
        obj%tally = obj%tally + 1.0

        obj%r_min = min(obj%r_min, sample)
        obj%r_max = max(obj%r_max, sample)

        bin_index = histogram_index(obj, sample)

        ! Increase the bin population by 1.0
        obj%bin(bin_index) = obj%bin(bin_index) + 1.0

        !*******************************************
        !***  Estimate median rank within population
        !*******************************************

        bin_pop = obj%bin(obj%median_bin)

        median_drift: &
        if(bin_index < obj%median_bin) then
          ! Move median half of one point lower.
          obj%median_rank = obj%median_rank - 0.5

          if(obj%median_rank < 0.0) then
            ! Median shifts downward into another bin.
            call histogram_median_dec(obj, obj%median_rank)
          end if
        else if(bin_index > obj%median_bin) then
          ! Move median half of one point higher.
          obj%median_rank = obj%median_rank + 0.5

          if(obj%median_rank > bin_pop) then
            ! Median shifts upward into another bin.
            call histogram_median_inc(obj, obj%median_rank - bin_pop)
          end if
        else
          ! Move median half of one point toward center of median_bin
          if(obj%median_rank < 0.5 * bin_pop) then
            obj%median_rank = obj%median_rank + 0.5
          else
            obj%median_rank = obj%median_rank - 0.5
          end if
        end if median_drift

        bin_pop = obj%bin(obj%median_bin)

        ! Note: if median_inc (median_dec) does not overflow (underflow)
        ! then this division should be okay as is.
        obj%median = histogram_sample(obj, obj%median_bin, &
          obj%median_rank / obj%bin(obj%median_bin))

        !*****************************************************
        !***  Estimate the distribution given this data point.
        !*****************************************************

        ! Note: optimization opportunity (not implemented yet).
        !
        ! Efficiency could be increased by limiting histogram_estimate calls.
        ! For example, it could be called for the first 1e5 points
        ! (approximately 100 seismic traces), then every 1/10 point
        ! for the next 1e6 points, then every 1/100 points ... etc.
        !
        ! Profiling execution suggest four routines dominate execution time:
        ! compute_vector (biggest), estimate, index, and sample (smallest).
        ! The first target for optimizing should be compute_vector itself.
        call histogram_estimate(obj, sample)
      end do sample_loop

      return
      end subroutine histogram_compute_vector

!!--------------------- histogram_initialize -------------------------------!!
!!--------------------- histogram_initialize -------------------------------!!
!!--------------------- histogram_initialize -------------------------------!!

      subroutine histogram_initialize (obj, vector, start)
      implicit none
      type(histogram_struct), intent(inout) :: obj       ! arguments
      real, dimension(:), intent(in) :: vector
      integer, intent(out) :: start

      integer :: sample_count
      integer :: index_first, index_second
      double precision :: value
      double precision :: center
      double precision :: range
      double precision :: median_std_dev
      double precision :: half_middle_size

      !*********************************************
      !***  Capture the first two input points.  ***
      !*********************************************

      sample_count = size(vector)

      if(obj%total == 0.0) then
        if(obj%ignore) then
          ! find first non-zero sample in vector
          first_loop: &
          do start = 1, sample_count
            if(vector(start) /= 0.0) then
              exit first_loop
            end if

            obj%zeros = obj%zeros + 1
          end do first_loop
        else
          ! use first sample, whether its zero or not.
          start = 1
        end if

        if(start > sample_count) return

        value = vector(start)
        obj%first = value

        start = start + 1
        obj%total = 1.0
        obj%tally = 1.0
      else
        start = 1
      end if

      if(obj%ignore) then
        ! find second non-zero sample in vector
        second_loop: &
        do start = start, sample_count
          if(vector(start) /= 0.0) then
            exit second_loop
          end if

          obj%zeros = obj%zeros + 1
        end do second_loop
      end if

      if(start > sample_count) return

      value = vector(start)
      obj%second = value

      start = start + 1
      obj%total = 2.0
      obj%tally = 2.0

      !***************************************************************
      ! Initialize first trial histogram, given the first two samples.
      !***************************************************************

      obj%median = 0.5 * (obj%first + obj%second)

      if(obj%explicit_range) then
        range = obj%range
      else
        median_std_dev = sqrt((obj%first  - obj%median) ** 2 + &
                              (obj%second - obj%median) ** 2)

        if(median_std_dev == 0.0) then
          range = 2001.0  ! arbitrary
        else
          range = obj%min_dev * median_std_dev
        end if
      end if

      if(obj%explicit_center) then
        center = obj%center
      else
        half_middle_size = 0.5 * range / obj%middle_slots

        if(obj%median > - half_middle_size .and. &
           obj%median < + half_middle_size) then
          ! round center to exactly zero, natural position
          center = 0.0
        else
          center = obj%median
        end if
      end if

      obj%r_min = min(obj%first, obj%second)
      obj%r_max = max(obj%first, obj%second)

      obj%sum = obj%first + obj%second

      obj%variance = (obj%first  - center) ** 2 + &
                     (obj%second - center) ** 2

      obj%std_dev = sqrt(obj%variance)

      ! setup trial histogram bins based upon a nominal center and range
      call histogram_setup(obj, center, range)

      ! insert the first two values for input points already seen
      index_first  = histogram_index(obj, obj%first)

      obj%bin(index_first) = obj%bin(index_first) + 1

      index_second = histogram_index(obj, obj%second)

      obj%bin(index_second) = obj%bin(index_second) + 1

      ! synthesize a median estimate
      if(index_second < index_first) then
        obj%median_bin = index_second
        obj%median_rank = 1.0
      else
        obj%median_bin = index_first
        obj%median_rank = 1.0
      end if

      return
      end subroutine histogram_initialize

!!------------------ histogram_setup -------------------------------!!
!!------------------ histogram_setup -------------------------------!!
!!------------------ histogram_setup -------------------------------!!

      subroutine histogram_setup (obj, center, range)
      implicit none
      type(histogram_struct),intent(inout) :: obj       ! arguments
      double precision, intent(in) :: center
      double precision, intent(in) :: range

      !*************************************************************
      !***  Setup trial histogram bins, given center and range.  ***
      !*************************************************************

      double precision :: min_max_range, huge_delta

      ! The trial histogram is over sampled and covers a broader range
      ! than the final histogram requested by the histogram_create caller.
      ! This improves the accuracy of the final histogram, while limiting
      ! the cost (memory and execution) of computing it.

      obj%range = min(range, HUGE_REAL * (obj%min_dev / obj%max_dev))

      obj%center = min(max(center, 0.5 * obj%range - obj%huge_edge), &
                                   obj%huge_edge - 0.5 * obj%range)

      obj%middle_range = obj%range * obj%max_dev / obj%min_dev

      obj%middle_size = obj%middle_range / obj%middle_slots

      ! boundary between linear and logrithmic bins.
      obj%upper_edge = obj%center + 0.5 * obj%middle_range
      obj%lower_edge = obj%center - 0.5 * obj%middle_range

      obj%upper_range = HUGE_REAL - obj%upper_edge
      obj%lower_range = obj%lower_edge + HUGE_REAL

      ! The logrithmic base is approximately the edge value, such that
      ! log(upper_sample - upper_base) is approximately zero and
      ! log(lower_base - lower_sample) is approximately zero.
      obj%upper_base = obj%upper_edge - max(spacing(obj%upper_edge), dble(1.0))
      obj%lower_base = obj%lower_edge + max(spacing(obj%lower_edge), dble(1.0))

      obj%min_range = (obj%middle_range * obj%min_sub) / obj%max_sub

      ! Computing a good value for trial upper_size and lower_size is tricky.
      !
      ! Most points (> 90%) should fall within the middle linear bins.
      ! The upper and lower logrithmic bins catch the outliers.
      ! If the outliers are true rogue points, a poor approximation is okay.
      ! If they are the precursor to a developing trend in subsequent points,
      ! a good approximation is highly desireable.
      !
      ! By definition, no points below r_min, nor above r_max have arrived.
      ! Subsequent points may extend these limits, but it seems silly to
      ! waste (zero population) logrithmic bins until they do.
      ! If they do arrive, the min and max typically extend gradually.
      ! A few logrithmic bins of the nominal size beyond the current
      ! r_min and r_max values will probably capture them.
      !
      ! The edge of the nominal size logrithmic bins is defined by
      ! huge_upper and huge_lower (r_max + delta and r_min - delta).
      ! Any points beyond these limits fall into an overflow bin.
      ! Hopefully the trial overflow bins remain empty or nearly so.
      !
      ! The final histogram is smaller than the trial histogram.
      ! It is not oversampled and the linear bin range is not extended.
      ! Upper and lower bins are limited to overflow (log_slots == 1).
      ! They should contain less than 10% of the population if all is well.

      if(obj%log_slots == 1) then
        ! overflow bin, but no nominal sized bins
        obj%huge_upper = obj%upper_edge
        obj%huge_lower = obj%lower_edge

        ! size of overflow bins
        obj%upper_overflow = obj%upper_range
        obj%lower_overflow = obj%lower_range

        ! Nominal size of logrithmic bins is used to compute index
        obj%upper_size = obj%upper_range
        obj%lower_size = obj%lower_range
      else
        ! overflow bin plus one or more nominal size bins
        min_max_range = max(obj%r_max, obj%upper_edge) - &
                        min(obj%r_min, obj%lower_edge)

        huge_delta = 1e+3 * min_max_range

        ! boundary between overflow and (zero or more) logrithmic bins.
        obj%huge_upper = min(+ HUGE_REAL - HUGE_REAL_SPACING, &
          obj%upper_edge + huge_delta)
        obj%huge_lower = max(- HUGE_REAL + HUGE_REAL_SPACING, &
          obj%lower_edge - huge_delta)

        ! size of overflow bins, needed to relocate population
        obj%upper_overflow = HUGE_REAL - obj%huge_upper
        obj%lower_overflow = obj%huge_lower - HUGE_REAL

        ! Nominal size of logrithmic bins (excluding the overflow bins).
        obj%upper_size = log(obj%huge_upper - obj%upper_edge) / &
          (obj%upper_slots - 1)
        obj%lower_size = log(obj%lower_edge - obj%huge_lower) / &
          (obj%lower_slots - 1)
      end if

      return
      end subroutine histogram_setup

!!------------------ histogram_median_inc -------------------------------!!
!!------------------ histogram_median_inc -------------------------------!!
!!------------------ histogram_median_inc -------------------------------!!

      subroutine histogram_median_inc (obj, excess)
      implicit none
      type(histogram_struct),intent(inout) :: obj       ! arguments
      double precision, intent(in) :: excess

      integer :: j
      double precision :: group, pop

      group = excess

      ! Drift median estimate into higher bin
      do j = obj%median_bin + 1, obj%bin_slots, +1
        pop = obj%bin(j)

        if(pop >= group) then
          obj%median_bin = j
          obj%median_rank = pop - group
          return
        end if

        group = group - pop
      end do

      call pc_error("Histogram: median increment overflow.")

      ! Distribution is poorly binned.
      obj%median_bin = obj%upper_max
      obj%median_rank = obj%bin(obj%upper_max)

      return
      end subroutine histogram_median_inc

!!------------------ histogram_median_dec -------------------------------!!
!!------------------ histogram_median_dec -------------------------------!!
!!------------------ histogram_median_dec -------------------------------!!

      subroutine histogram_median_dec (obj, excess)
      implicit none
      type(histogram_struct),intent(inout) :: obj       ! arguments
      double precision, intent(in) :: excess

      integer :: j
      double precision :: group, pop

      group = excess

      ! Decrement median estimate into next non-empty bin
      do j = obj%median_bin - 1, 1, -1
        pop = obj%bin(j)

        group = group + pop

        if(group >= 0) then
          obj%median_bin = j
          obj%median_rank = group
          return
        end if
      end do

      call pc_error("Histogram: median decrement underflow.")

      ! Distribution is poorly binned.
      obj%median_bin = obj%lower_min
      obj%median_rank = 0.0

      return
      end subroutine histogram_median_dec

!!------------------ histogram_estimate -------------------------------!!
!!------------------ histogram_estimate -------------------------------!!
!!------------------ histogram_estimate -------------------------------!!

      subroutine histogram_estimate (obj, sample)
      implicit none
      type(histogram_struct),intent(inout) :: obj       ! arguments
      double precision :: sample

      double precision :: center
      double precision :: range
      double precision :: half_range

      !**********************************************
      !***  Estimate the variance of the distribution
      !**********************************************
      ! The optimum center for a histogram is the data median, assuming
      ! the histogram range is fixed and accurate binning is desired.
      !
      ! Given the mathematical definition of variance about the center,
      ! one can derive an explicit expression for the variance
      ! of N+1 points, as a function of the variance for the first N points
      ! and the value of the new N+1 point.
      !
      ! An explicit expression for the variance about the median (instead
      ! of the center) is more difficult or perhaps impossible.
      !
      ! This scheme assumes the center is near the median so that
      ! binning is accurate and the variance about the center is
      ! approximately the same as the variance about the median.
      !
      ! Its unknown how sensitive this expression is to round off error.

      obj%variance = ((obj%total - 2.0) * obj%variance + &
        (sample - obj%center) ** 2) / (obj%total - 1.0)

      !************************************
      !***  Estimate the standard deviation
      !************************************
      obj%std_dev = sqrt(obj%variance)

      if(.not. obj%explicit_exact .and. obj%std_dev > 0.0) then
        ! Compare the estimated median and standard deviation to the
        ! nominal values used for the trial histogram.
        if(obj%explicit_center) then
          center = obj%center
        else
          center = obj%median
        end if

        if(obj%explicit_range) then
          range = obj%range
        else
          range = obj%min_dev * obj%std_dev
        end if

        half_range =  0.5 * range

        !********************************************
        !***  Normalize the trial histogram as needed
        !********************************************
        if(obj%explicit_range) then
          ! The range is frozen, only the center can shift.
          if(obj%upper_edge < center + half_range .or. &
             obj%lower_edge > center - half_range) then
            ! The median has varied significantly from the
            ! nominal values assumed for the current trial.
            call histogram_normalize(obj, center, range)
          end if
        else if(obj%explicit_center) then
          ! The center is frozen, only the range can vary.
          if(range < obj%min_range) then
            ! The standard deviation has varied significantly from the
            ! nominal values assumed for the current trial.
            call histogram_normalize(obj, center, range)
          end if
        else if(obj%upper_edge < center + half_range .or. &
                obj%lower_edge > center - half_range .or. &
                range < obj%min_range) then
          ! The median and/or standard deviation has varied significantly
          ! from the nominal values assumed for the current trial.
          call histogram_normalize(obj, center, range)
        end if
      end if

      return
      end subroutine histogram_estimate

!!------------------ histogram_quantile -------------------------------!!
!!------------------ histogram_quantile -------------------------------!!
!!------------------ histogram_quantile -------------------------------!!

      function histogram_quantile(obj, quantile)
      implicit none
      real :: histogram_quantile
      type(histogram_struct),intent(in) :: obj       ! arguments
      real :: quantile

      ! Estimate the data value at a given fractional population

      integer :: j
      double precision :: group, pop

      if(quantile <= 0.0) then
        histogram_quantile = obj%r_min
      else if(quantile >= 1.0) then
        histogram_quantile = obj%r_max
      else
        group = quantile * obj%tally

        ! Drift estimate from lower to higher bin
        do j = 1, obj%bin_slots
          pop = obj%bin(j)

          if(pop >= group) then
            ! normal return
            histogram_quantile = histogram_sample(obj, j, group / pop)
            return
          end if

          group = group - pop
        end do

        ! numerical round off problem
        histogram_quantile = obj%r_max
      end if

      return
      end function histogram_quantile

!!------------------ histogram_sample -------------------------------!!
!!------------------ histogram_sample -------------------------------!!
!!------------------ histogram_sample -------------------------------!!

      function histogram_sample(obj, bin_index, ratio)
      implicit none
      double precision :: histogram_sample
      type(histogram_struct),intent(in) :: obj       ! arguments
      integer, intent(in) :: bin_index
      double precision, intent(in) :: ratio

      ! Compute sample value associated with bin_index and ratio.

      if(bin_index > obj%middle_max) then
        ! Index falls within upper logarithmic bins.
        if(bin_index >= obj%upper_max) then
          ! upper overflow bin
          histogram_sample = 0.5 * (obj%huge_upper + HUGE_REAL)
        else
          ! nominal size upper logrithmic bin
          histogram_sample = obj%upper_base + exp(obj%upper_size * &
            (bin_index - obj%upper_min + ratio))
        end if
      else if(bin_index < obj%middle_min) then
        ! Index falls within lower logarithmic bins.
        if(bin_index <= obj%lower_min) then
          ! lower overflow bin
          histogram_sample = 0.5 * (obj%huge_lower - HUGE_REAL)
        else
          ! nominal size lower logrithmic bin
          histogram_sample = obj%lower_base - exp(obj%lower_size * &
            (obj%middle_min - bin_index - ratio))
        end if
      else
        ! Index falls within middle linear bins.
        histogram_sample = obj%lower_edge + obj%middle_size * &
          (bin_index - obj%middle_min + ratio)
      end if

      return
      end function histogram_sample

!!------------------ histogram_ratio -------------------------------!!
!!------------------ histogram_ratio -------------------------------!!
!!------------------ histogram_ratio -------------------------------!!

      function histogram_ratio(obj, bin_index, sample)
      implicit none
      double precision :: histogram_ratio
      type(histogram_struct),intent(in) :: obj       ! arguments
      integer, intent(in) :: bin_index
      double precision, intent(in) :: sample

      ! Compute ratio corresponding to sample referenced from bin_index bin.

      double precision :: sub_ratio

      if(bin_index > obj%middle_max) then
        ! Index falls within upper logarithmic bins.
        if(sample <= obj%upper_base) then
          ! avoid taking log of a non-positive number
          sub_ratio = 0.0
        else if(sample <= obj%huge_upper) then
          ! nominal size upper logrithmic bin
          sub_ratio = log(sample - obj%upper_base) / obj%upper_size
        else
          ! upper overflow bin
          sub_ratio = (sample - obj%huge_upper) / (obj%upper_overflow)
        end if

        histogram_ratio = obj%upper_min - bin_index + sub_ratio
      else if(bin_index < obj%middle_min) then
        ! Index falls within lower logarithmic bins.
        if(sample >= obj%lower_base) then
          ! avoid taking log of a non-positive number
          sub_ratio = 0.0
        else if(sample >= obj%huge_lower) then
          ! nominal size lower logrithmic bin
          sub_ratio = log(obj%lower_base - sample) / obj%lower_size
        else
          ! lower overflow bin
          sub_ratio = (obj%huge_lower - sample) / (obj%lower_overflow)
        end if

        histogram_ratio = obj%middle_min - bin_index - sub_ratio
      else
        ! Index falls within middle linear bins.
        histogram_ratio = obj%middle_min - bin_index + &
          (sample - obj%lower_edge) / obj%middle_size
      end if

      return
      end function histogram_ratio

!!------------------ histogram_relocate -------------------------------!!
!!------------------ histogram_relocate -------------------------------!!
!!------------------ histogram_relocate -------------------------------!!

      subroutine histogram_relocate (old, new)
      implicit none
      type(histogram_struct),intent(in) :: old       ! arguments
      type(histogram_struct),intent(inout) :: new       ! arguments

      ! Relocate the population from the old bins to the new.

      integer :: new_index
      double precision :: new_edge_value, new_tally, new_variance

      integer :: old_index
      integer,save :: knt=0
      double precision :: old_rank, old_edge_rank

      double precision :: sample, group, pop
      logical :: almost

      new%bin = 0.0

      if(old%total < 2.0) then
        ! special case, can't histogram less than two points
        new%tally = 0.0
        new%variance = 0.0
        new%median_bin = new%bin_slots / 2
        new%median_rank = 0.0

        return
      end if

      new_tally = 0.0

      bin_loop: &
      do old_index = old%lower_min, old%upper_max
        pop = old%bin(old_index)
        almost=ameq(pop,0.0,0.0000001)
        if(almost)pop=0.0

        old_rank = 0.0

        ! Relocate population in sequential groups,
        ! where a group includes old_rank through old_edge_rank.
        rank_loop: &
        do while(old_rank < pop)
          sample = histogram_sample(old, old_index, old_rank / pop)

          new_index = histogram_index(new, sample)

          new_edge_value = histogram_sample(new, new_index, dble(1.0))

          if(new_edge_value <= sample .and. new_index < new%upper_max) then
            ! new_index needs to be rounded up, not down.
            new_index = new_index + 1

            new_edge_value = histogram_sample(new, new_index, dble(1.0))
          end if

          old_edge_rank = pop * min(dble(1.0), &
            histogram_ratio(old, old_index, new_edge_value))

          ! relocated one group (from old_rank up to old_edge_rank).
          if(old_edge_rank <= old_rank) then
            ! numerical round off limitation.
            old_edge_rank = pop
          end if

          ! relocated one group (from old_rank up to old_edge_rank).
          group = old_edge_rank - old_rank

          new_tally = new_tally + group

          new%bin(new_index) = new%bin(new_index) + group

          ! increment the loop counter
          old_rank = old_edge_rank
        end do rank_loop
      end do bin_loop

      new%tally = new_tally

      ! Recompute the estimated variance
      new_variance = 0.0

      variance_loop: &
      do new_index = new%lower_min, new%upper_max
        pop = new%bin(new_index)

        if(pop > 0.0) then
          ! Assume all points for this bin are located at the center
          sample = histogram_sample(new, new_index, dble(0.5))

          new_variance = new_variance + pop * (new%center - sample) ** 2
        end if
      end do variance_loop

      new%variance = new_variance

      ! Relocate the estimated median.
      new%median_bin = histogram_index(new, new%median)
      new%median_rank = new%bin(new%median_bin) * &
        histogram_ratio(new, new%median_bin, new%median)

      return
      end subroutine histogram_relocate

!!------------------ histogram_normalize -------------------------------!!
!!------------------ histogram_normalize -------------------------------!!
!!------------------ histogram_normalize -------------------------------!!

      subroutine histogram_normalize (obj, center, range)
      implicit none
      type(histogram_struct),intent(inout) :: obj       ! arguments
      double precision, intent(in) :: center
      double precision, intent(in) :: range

      ! Normalize the old histogram to a new trial distribution.
      ! Relocate the population from the old bins to the new.

      type(histogram_struct), pointer :: old



      ! count the number of normalizations applied to histogram.
      obj%normalized = obj%normalized + 1

      ! save description of the old trial histogram
      nullify(old)
      allocate (old)

      old = obj

      ! Allocate and zero memory for the replacement bins.
      call histogram_alloc(obj)

      ! Setup boundaries for new trial histogram.
      call histogram_setup (obj, center, range)

      ! Relocate sample points from old trial to new obj.
      call histogram_relocate(old, obj)

    ! !??? development aid
    ! if(old%tally /= obj%tally) then
    !   call pc_warning("histogram: normalization changed tally from ", &
    !     old%tally, " to ", obj%tally)
    ! end if

      ! Cleanup storage.
      call mem_free(old%bin)

      deallocate(old)

      return
      end subroutine histogram_normalize

!!------------------ histogram_result_tmp -------------------------------!!
!!------------------ histogram_result_tmp -------------------------------!!
!!------------------ histogram_result_tmp -------------------------------!!

      subroutine histogram_result_tmp (obj, tmp)
      implicit none
      type(histogram_struct),intent(in) :: obj       ! arguments
      type(histogram_struct),pointer :: tmp       ! arguments

      ! Normalize the trial histogram to a "result" distribution.

      double precision ::         half_middle_size 

      ! copy description of the trial histogram
      nullify(tmp)
      allocate (tmp)

      tmp = obj

      ! Force "result" style constraints on the binning.
      tmp%max_dev = tmp%min_dev
      tmp%min_sub = 1
      tmp%max_sub = 1
      tmp%log_slots = 1

      ! Allocate and zero memory for the new bins.
      call histogram_alloc(tmp)

      if(.not. tmp%explicit_range) then
        if(tmp%std_dev > 0.0) then
          ! nominal case, data contains some variation
          tmp%range = tmp%min_dev * tmp%std_dev
        else if(tmp%sum > 0.0) then
          ! special case, non-zero values that are all equal
          tmp%range = 0.02 * tmp%sum / tmp%total
        else
          ! special special case, all values are all zero
          tmp%range = 2.0  ! arbitrary
        end if
      end if

      if(.not. tmp%explicit_center) then
        half_middle_size = 0.5 * tmp%range / tmp%middle_slots

        if(tmp%median > - half_middle_size .and. &
           tmp%median < + half_middle_size) then
          ! round center to exactly zero
          tmp%center = 0.0
        else
          tmp%center = tmp%median
        end if
      end if

      ! Setup boundaries for new tmp histogram.
      call histogram_setup (tmp, tmp%center, tmp%range)

      ! Relocate sample points from old trial to new obj.
      call histogram_relocate(obj, tmp)

    ! ??? development aid
    ! if(tmp%tally /= obj%tally) then
    !   call pc_warning("histogram: output changed tally from ", &
    !     obj%tally, " to ", tmp%tally)
    ! end if

      return
      end subroutine histogram_result_tmp

!!------------------ histogram_result -------------------------------!!
!!------------------ histogram_result -------------------------------!!
!!------------------ histogram_result -------------------------------!!

      subroutine histogram_result (obj, result, median, average, r_min, r_max,&
        center, range, total, zeros, n_min, n_max, n_below, n_above)
      implicit none
      type(histogram_struct),intent(in) :: obj       ! arguments
      real, dimension(:), optional,pointer :: result
      real, optional, intent(out) :: median
      real, optional, intent(out) :: average
      real, optional, intent(out) :: r_min
      real, optional, intent(out) :: r_max
      real, optional, intent(out) :: center
      real, optional, intent(out) :: range
      double precision, optional, intent(out) :: total
      double precision, optional, intent(out) :: zeros
      double precision, optional, intent(out) :: n_min
      double precision, optional, intent(out) :: n_max
      double precision, optional, intent(out) :: n_below
      double precision, optional, intent(out) :: n_above

      ! Normalize the trial histogram to a "result" distribution.
      ! Return values for optional arguments that are actually present.

      type(histogram_struct), pointer :: tmp

      nullify (tmp) ! jpa

      ! Normalize the trial histogram to a "result" distribution.
      call histogram_result_tmp(obj, tmp)

      ! Return values for optional arguments that are actually present.

      if(present(result)) then
        call mem_alloc(result, tmp%slots)

        result = tmp%bin(2:int(tmp%bin(tmp%slots + 1)))
      end if

      if(present(median)) then
        median = tmp%median
      end if

      if(present(average)) then
        average = tmp%sum / tmp%total
      end if

      if(present(r_min)) then
        r_min = tmp%r_min
      end if

      if(present(r_max)) then
        r_max = tmp%r_max
      end if

      if(present(center)) then
        center = tmp%center
      end if

      if(present(range)) then
        range = tmp%range
      end if

      if(present(total)) then
        total = tmp%total
      end if

      if(present(zeros)) then
        zeros = tmp%zeros
      end if

      if(present(n_min)) then
        n_min = minval(tmp%bin)
      end if

      if(present(n_max)) then
        n_max = maxval(tmp%bin)
      end if

      if(present(n_below)) then
        n_below = tmp%bin(1)
      end if

      if(present(n_above)) then
        n_above = tmp%bin(tmp%bin_slots)
      end if

      ! Cleanup storage.
      call mem_free(tmp%bin)

      deallocate(tmp)

      return
      end subroutine histogram_result

!!------------------ histogram_print_bar -------------------------------!!
!!------------------ histogram_print_bar -------------------------------!!
!!------------------ histogram_print_bar -------------------------------!!

      subroutine histogram_print_bar (lun, nchar, bar, &
        index, mode, coord, n_max, value)
      implicit none
      integer, intent(in) :: lun
      integer, intent(in) :: nchar
      character(len=*), intent(in) :: bar
      integer, intent(in) :: index
      character(len=*), intent(in) :: mode
      real, intent(in) :: coord
      double precision, intent(in) :: n_max
      double precision, intent(in) :: value

      ! Print one bar on histogram graph.

      integer length

      if(value <= 0.0) then
        length = 0
      else
        length = min(nchar, MAX_NCHAR, nint(0.5 + nchar * value / n_max))
      end if

      write(lun, " (i5,' ',a6,' ',g12.4,' ',a)") &
        index, mode, coord, bar(:length)

      return
      end subroutine histogram_print_bar

!!------------------ histogram_dump -------------------------------!!
!!------------------ histogram_dump -------------------------------!!
!!------------------ histogram_dump -------------------------------!!

      subroutine histogram_dump (tmp, lun, verbose, nchar, &
        title, ordinate, abscissa)
      implicit none
      type(histogram_struct),intent(in) :: tmp       ! arguments
      integer, intent(in) :: lun
      logical, optional, intent(in) :: verbose
      integer, optional, intent(in) :: nchar
      character(len=*), optional, intent(in) :: title
      character(len=*), optional, intent(in) :: ordinate
      character(len=*), optional, intent(in) :: abscissa

      ! Print histogram results.

      logical :: verbose2
      integer :: nchar2
      character(len=64) :: title2
      character(len=32) :: ordinate2
      character(len=32) :: abscissa2

      character(len=MAX_NCHAR), parameter :: ticks = &
       "         1         2         3         4         5&
       &         6         7         8         9         0"

      character(len=MAX_NCHAR), parameter :: clicks = &
       "12345678901234567890123456789012345678901234567890&
       &12345678901234567890123456789012345678901234567890"

      character(len=MAX_NCHAR), parameter :: stars = &
       "**************************************************&
       &**************************************************"

      character(len=MAX_NCHAR) :: marker
      integer :: mark, mark_cnt, mark_idx

      integer :: j
      double precision :: average, std_deviations
      double precision :: n_min, n_max
      double precision :: n_above, n_linear, n_below
      real :: coord
      character(len=6) :: mode

      if(tmp%total < 2.0) then
        write(lun, *) "histogram: not printed, less than 2 input points"
        return
      end if

      ! For each optional argument, test for its presents and
      ! validate the caller's value or default it.

      if(present(verbose)) then
        verbose2 = verbose
      else
        verbose2 = .false.
      end if

      nchar2 = NOMINAL_NCHAR

      if(present(nchar)) then
        if(nchar >= MIN_NCHAR .and. nchar <= MAX_NCHAR) then
          nchar2 = nchar
        end if
      end if

      if(present(title)) then
        title2 = title
      else
        title2 = "<undefined>"
      end if

      if(present(ordinate)) then
        ordinate2 = ordinate
      else
        ordinate2 = "Bin population"
      end if

      if(present(abscissa)) then
        abscissa2 = abscissa
      else
        abscissa2 = "Center of bin"
      end if

      if(tmp%total < 1.0) then
        write(lun, *) "histogram: not printed, no input points"
        return
      end if

      average = tmp%sum / tmp%total

      if(tmp%std_dev /= 0.0) then
        std_deviations = tmp%middle_range / tmp%std_dev
      else
        std_deviations = 0.0
      end if

      n_min = minval(tmp%bin)
      n_max = maxval(tmp%bin)
      n_above = sum(tmp%bin(tmp%upper_min:tmp%upper_max))
      n_linear = sum(tmp%bin(tmp%middle_min:tmp%middle_max))
      n_below = sum(tmp%bin(tmp%lower_min:tmp%lower_max))

      write(lun, *)

      write(lun, "(a)") stars

      write(lun, "(a,a)") &
        " Histogram Title= ", title2

      write(lun, "(a)") stars

      write(lun, *)

      verbose_choice: &
      if(verbose2) then
        ! verbose is TRUE !
        write(lun, "(a,i5)") &
          " Renormalized,  steps= ", tmp%normalized

        write(lun, "(a,g12.3,a,g12.3,a,g12.3)") &
          " Data stats,   median= ", tmp%median, &
          "    std dev= ", tmp%std_dev, &
          " average= ", average

        write(lun, "(a,g12.3,a,g12.3)") &
          " Data value,      min= ", tmp%r_min, &
          "        max= ", tmp%r_max

        write(lun, "(a,g12.3,a,g12.3)") &
          " Point count,   total= ", tmp%total, &
          "      tally= ", tmp%tally

        if(tmp%ignore) then
          write(lun, "(a,g12.3,a,g12.3,a)") &
            " Point count,   total= ", tmp%total, &
            "      zeros= ", tmp%zeros, &
            " (ignored)"
        else
          write(lun, "(a,g12.3)") &
            " Point count,   total= ", tmp%total
        end if


        write(lun, "(a,g12.3,a,g12.3,a,g12.3)") &
          " Linear bins,  center= ", tmp%center, &
          " edge upper= ", tmp%upper_edge, &
          "   lower= ", tmp%lower_edge

        write(lun, "(a,g12.3,a,g12.3,a,g12.3)") &
          " Range,        middle= ", tmp%range, &
          "      upper= ", tmp%upper_range, &
          "   lower= ", tmp%lower_range

        write(lun, "(a,g12.3,a,g12.3,a,g12.3)") &
          " Edge base,     upper= ", tmp%upper_base, &
          "      lower= ", tmp%lower_base

        write(lun, "(a,g12.3,a,g12.3)") &
          " Linear bins, max dev= ", tmp%middle_range, &
          "    std dev= ", std_deviations

        write(lun, "(a,i5,7x,a,i5,7x,a,i5)") &
          " Bin count,    middle= ", tmp%middle_slots, &
          "      upper= ", tmp%log_slots, &
          "   lower= ", tmp%log_slots

        write(lun, "(a,g12.3,a,g12.3,a,g12.3)") &
          " Bin size,     middle= ", tmp%middle_size, &
          "      upper= ", tmp%upper_size, &
          "   lower= ", tmp%lower_size

        write(lun, "(a,g12.3,a,g12.3,a,g12.3)") &
          " Bin populatn, linear= ", n_linear, &
          "      upper= ", n_above, &
          "   lower= ", n_below

        write(lun, "(a,g12.3,a,g12.3)") &
          " Bin population   min= ", n_min, &
          "        max= ", n_max
      else verbose_choice
        ! verbose is FALSE !
        write(lun, "(a,g12.3,a,g12.3)") &
          " Data value,      min= ", tmp%r_min, &
          "        max= ", tmp%r_max

        if(tmp%ignore) then
          write(lun, "(a,g12.3,a,g12.3,a)") &
            " Point count,   total= ", tmp%total, &
            "      zeros= ", tmp%zeros, &
            " (ignored)"
        else
          write(lun, "(a,g12.3)") &
            " Point count,   total= ", tmp%total
        end if

        write(lun, "(a,g12.3,a,g12.3,a,g12.3)") &
          " Linear bins,  center= ", tmp%center, &
          " edge upper= ", tmp%upper_edge, &
          "   lower= ", tmp%lower_edge

        write(lun, "(a,g12.3,a,g12.3,a,g12.3)") &
          " Bin populatn, linear= ", n_linear, &
          "      upper= ", n_above, &
          "   lower= ", n_below
      end if verbose_choice

      write(lun, *)

      write(lun, "(a,a)") &
        " Abscissa= ", abscissa2

      ! Print the graph
      mode = " "

      call histogram_print_bar(lun, nchar2, clicks, &
        0, mode, real(+HUGE_REAL), n_max, n_max)

      coord = 0.5 * (tmp%huge_upper + HUGE_REAL)
      mode = "log"
      call histogram_print_bar(lun, nchar2, stars, &
        tmp%upper_max, mode, coord, n_max, tmp%bin(tmp%upper_max))

      graph_upper: &
      do j = tmp%upper_max - 1, tmp%lower_min + 1, -1
        coord = histogram_sample(tmp, j, dble(0.5))

        if(j > tmp%middle_max .or. j < tmp%middle_min) then
          mode = "Log"
        else
          mode = "Linear"
        end if

        call histogram_print_bar(lun, nchar2, stars, &
          j, mode, coord, n_max, tmp%bin(j))
      end do graph_upper

      coord = 0.5 * (tmp%huge_lower - HUGE_REAL)
      mode = "log"
      call histogram_print_bar(lun, nchar2, stars, &
        tmp%lower_min, mode, coord, n_max, tmp%bin(tmp%lower_min))

      mode = " "

      call histogram_print_bar(lun, nchar2, clicks, &
        0, mode, real(-HUGE_REAL), n_max, n_max)

      ! create population marks for ordinate (allow 15 char per mark)
      mark_cnt = min(5, nchar2 / 15)

      if(mark_cnt >= 1) then
        marker = " "

        do mark = 1, mark_cnt
          mark_idx = (mark * nchar2) / mark_cnt

          marker(mark_idx:mark_idx) = "|"
        end do

        call histogram_print_bar(lun, nchar2, marker, &
          0, mode, 0.0, n_max, n_max)

        do mark = 1, mark_cnt
          mark_idx = (mark * nchar2) / mark_cnt

          write(marker(mark_idx - 14:mark_idx), "(3x,g12.4)") &
            dble(mark * n_max / mark_cnt)
        end do

        call histogram_print_bar(lun, nchar2, marker, &
          0, mode, 0.0, n_max, n_max)
      end if

      write(lun, "(a,a)") &
        " Ordinate= ", ordinate2

      ! marker values for

      write(lun, *)

      return
      end subroutine histogram_dump

!!------------------ histogram_print -------------------------------!!
!!------------------ histogram_print -------------------------------!!
!!------------------ histogram_print -------------------------------!!

      subroutine histogram_print (obj, lun, verbose, nchar, &
        title, ordinate, abscissa)
      implicit none
      type(histogram_struct),intent(in) :: obj       ! arguments
      integer, intent(in) :: lun
      logical, optional, intent(in) :: verbose
      integer, optional, intent(in) :: nchar
      character(len=*), optional, intent(in) :: title
      character(len=*), optional, intent(in) :: ordinate
      character(len=*), optional, intent(in) :: abscissa

      ! Print histogram results.

      type(histogram_struct),pointer :: tmp

      logical :: verbose2
      integer :: nchar2
      character(len=64) :: title2
      character(len=32) :: ordinate2
      character(len=32) :: abscissa2

      double precision :: n_above, n_linear, n_below

      nullify (tmp) ! jpa

      if(obj%total < 2.0) then
        write(lun, *) "histogram: not printed, less than 2 input points"
        return
      end if

      ! For each optional argument, test for its presents and
      ! validate the caller's value or default it.

      if(present(verbose)) then
        verbose2 = verbose
      else
        verbose2 = .false.
      end if

      nchar2 = NOMINAL_NCHAR

      if(present(nchar)) then
        if(nchar >= MIN_NCHAR .and. nchar <= MAX_NCHAR) then
          nchar2 = nchar
        end if
      end if

      if(present(title)) then
        title2 = title
      else
        title2 = "<undefined>"
      end if

      if(present(ordinate)) then
        ordinate2 = ordinate
      else
        ordinate2 = "Bin center"
      end if

      if(present(abscissa)) then
        abscissa2 = abscissa
      else
        abscissa2 = "Bin population"
      end if

      ! Normalize the trial histogram to a "result" distribution.
      call histogram_result_tmp(obj, tmp)

      ! Print histogram results.
      call histogram_dump (tmp, lun, verbose2, nchar2, &
        title2, ordinate2, abscissa2)

      if(verbose2) then
        n_above = sum(obj%bin(obj%upper_min:obj%upper_max))
        n_linear = sum(obj%bin(obj%middle_min:obj%middle_max))
        n_below = sum(obj%bin(obj%lower_min:obj%lower_max))

        write(lun, "(a,g12.3,a,g12.3,a,g12.3)") &
          " Original pop, linear= ", n_linear, &
          "      upper= ", n_above, &
          "   lower= ", n_below
      end if

      return
      end subroutine histogram_print

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module histogram_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

