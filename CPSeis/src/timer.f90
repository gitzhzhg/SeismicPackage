!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- timer.f90 --------------------------------!!
!!------------------------------- timer.f90 --------------------------------!!
!!------------------------------- timer.f90 --------------------------------!!

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
! Name       : TIMER 
! Category   : miscellaneous
! Written    : 1999-12-30   by: Brad Kruse
! Revised    : 2007-12-06   by: B. Menger
! Maturity   : beta
! Purpose    : Provide performance timing functions
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This timer package provides a simplified means of timing discrete events 
!   during the execution of software.  It is intended to serve both 
!   development and maintenance phases of development, and product interval
!   keeping.  Times are stated in seconds of wall clock time.
!
! Two rountines are provided to clear or reset one or all timers:
!    timer_clear_all -- Clears all timers to initial state, optionally will 
!                       modify the defaut (6) lun used by timer_report
!                       (NOTE: should only be called by main.f90 within CPS)
!    timer_clear     -- Clears one timer to initial (0 count, 0 time) state
!
! Routines provided to allocate and free a timer assigned to the development
!   and maintenance allotment:
!    timer_alloc -- Allocate a timer
!    timer_free  -- Release a timer
!
! Two routines are provided to start and stop the 'clock' for each timer:
!    timer_start -- Capture start time, increment timer trip count
!    timer_stop  -- Capture end time, accumulate elapsed time
!
! Routines provided to access accumulated information:
!    timer_fetch  -- Returns available statistics about a timer
!    timer_report -- Prints a 4-line report including stats from timer_fetch
!                    for a timer.
!
! Timers 1-249 are defined; however, their use should be assigned as:
!     1 - 199 -- main.f90 job timing for CPS; may be the same as the ipn
!   200 - 240 -- development and maintenance, non-product timing.
!   241 - 249 -- reserved for timer future, internal use
!
!-------------------------------------------------------------------------------
!</descript_doc>


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
!                              o
!    call timer_clear_all (default_lun)
!
!                        i
!    call timer_clear (timer)
!
!                        o      o
!    call timer_alloc (timer, status)
!
!                        i
!    call timer_start (timer)
!
!                        i
!    call timer_stop (timer)
!
!                      i      o         o         o         o
!    call timer_fetch (timer, cnt_time, min_time, max_time, avg_time,    &
!
!                      o         o         o        o-optional
!                      sum_time, var_time, sd_time, elapsed_time)
!
!                        i     i      o
!    call timer_report (timer, label, opt_lun)
!
!                       i      o
!    call timer_free (timer, status)
!
!
! character(len=*)   label       = User label for timer report.  Only the first
!                                  12 characters will be printed to the report. 
! integer            timer       = Timer ID, 1 to 249 (See descript_doc)
! integer, optional  default_lun = Establish as default lun for timer_report
!                                  (used for all timers).  Default is 6.
! integer, optional  opt_lun     = Optional lun for use for a specific report
!                                  of a specific timer.  Default is default_lun
! integer, optional  status      = 0 if successful, -1 if failure
! integer            cnt_time    = Number of times timer_start is called 
!                                  for a timer.
! real               min_time    = Smallest time interval for a timer.
! real               max_time    = Largest time interval for a timer.
! real               avg_time    = Average time interval for a timer.
! real               sum_time    = Accumulated time for a timer.
! real               var_time    = Variance in time for a timer.
! real               sd_time     = Standard deviation in time for a timer.
! real               elapsed_time= elapse time in seconds for a timer
!
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! All times are expressed in seconds.
!
! Expected sequence of usage:
!
!    use timer_module, only:     &
!          timer_alloc,          &
!          timer_clear,          &
!          timer_start,          &
!          timer_stop,           &
!          timer_report,         &
!          timer_free
!
!    integer :: my_timer
!
!    call timer_alloc (timer = my_timer)
!
!    call timer_start (timer = my_timer)
!    <block of code or subroutine call>
!    call timer_stop (timer = my_timer)
!    ...
!    <repeat as needed>
!    call timer_report (timer = my_timer, label = '<block name> ')
!    call timer_free (timer = my_timer)
!
! To reuse/reset an already allocated timer:
!     call timer_clear (timer = my_timer)
!
! Various means of report creation are supported; timer_report is one example
! of getting the timer values using timer_fetch, and formatting a simple
! report of values.
!
! NOTE:  Due to system clock granularity very short intervals may not
!        be measured correctly.  For instance, Solaris updates it's
!        'clock' every 10 milliseconds, then reports the clock to the
!        microsecond...
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
! 11. 2007-12-06  B. Menger    Replaced parameterized init of timer struct with
!                              a routine for ifort.
! 10. 2006-06-05  B. Menger    Removed Unused Variables.
!  9. 2002-02-04  C C Burch    Fix bug in calculating elapsed wall clock time
!  8. 2001-11-02  C C Burch    Added optional elapse time argument to fetch
!  7. 2001-10-23  Stoeckley    Fix format TEXT_FMT which the new sun compiler
!                               did not like (replace x with 1x).
!  6. 2001-08-27  C C Burch    Retain in_use status in clear routines 
!  5. 2001-07-17  Vunderink    Added routines timer_alloc and timer_free.
!  4. 2000-06-27  Brad Kruse   Correct divide-by-zero error in timer_fetch.
!  3. 2000-06-16  Brad Kruse   Update Maturity from raw to production
!  2. 2000-01-26  Brad Kruse   Corrected collection of system time
!  1. 2000-01-04  Brad Kruse   Initial version.
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
! No special requirements.
!
! However, because of the way that TIMER should operate, that is, in 
! evaluating system and software performance, timer.f90 should be 
! compiled fully optimized (-O3) for best results.
!
! Also, GETSYS should also be compiled optimized (-O3) since TIMER relies
! on GETSYS for efficient and accurate timing values.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!
!  Running sums are gathered to later compute variance and standard deviation.
!
! var = sum ((x(i) - x_mean)** 2) / (n - 1)
! sd  = sqrt (var)
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!
! The precision and accuracy of this routine are determined by the 
! implementation of cgetsys_time (getrusage).  Accuracy and precision
! of the time values should be expected to vary somewhat from platform
! to platform.
!
! Below is a simple test driver:
! program main
!   use timer_module
!   use unix_module
!
!   implicit none
!
!   integer :: i
!
!   call timer_clear(1)
!   call timer_start(1)
!   call unix_sleep(2)
!   call timer_stop(1)
!   call timer_report(1,"timer 1-wait")
!
!   call timer_clear(2)
!   call timer_start(2)
!   call timer_clear(3)
!
!   do i=1, 100000
!    call timer_start(3)
!    call timer_stop(3)
!   enddo
!
!   call timer_stop(2)
!   call timer_report(2,"Timer 2-loop")
!   call timer_report(3,"Timer 3-loop")
!   
!   stop
!
! end program main
!
!-------------------------------------------------------------------------------
!</programming_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


module timer_module
  use unix_module
  !
  implicit none
  private
  !
  public :: timer_alloc
  public :: timer_clear
  public :: timer_clear_all
  public :: timer_fetch
  public :: timer_report
  public :: timer_start
  public :: timer_stop
  public :: timer_free

  character (len = 100), public, save :: TIMER_IDENT =    &
       '$Id: timer.f90,v 1.11 2007/12/07 15:25:25 Menger beta sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!

  type :: timer_struct              
    integer          :: count
    logical          :: running
    logical          :: in_use
    double precision :: start_utime
    double precision :: start_stime
    double precision :: max_utime
    double precision :: min_utime
    double precision :: accum_utime
    double precision :: max_stime
    double precision :: min_stime
    double precision :: accum_stime
    double precision :: accum_time
    double precision :: accum_time2
    double precision :: first_time
    double precision :: last_time
    double precision :: start_etime
    double precision :: accum_etime
  end type timer_struct


  !!----------------------------- data ------------------------------------!!
  !!----------------------------- data ------------------------------------!!
  !!----------------------------- data ------------------------------------!!

  integer,             parameter :: max_timers       = 249
  integer,             parameter :: min_alloc_timers = 200
  integer,             parameter :: max_alloc_timers = 240
  !
  integer, save :: lun = 6
  type (timer_struct), dimension (0:max_timers), save :: timers

contains

  subroutine timer_init(t)
    type(timer_struct), intent(inout) :: t
    t%count          =0
    t%running        =.false.
    t%in_use         =.false.
    t%start_utime    =0.0d0
    t%start_stime    =0.0d0
    t%max_utime    =0.0d0
    t%min_utime    =1.0d37
    t%accum_utime    =0.0d0
    t%max_stime    =0.0d0
    t%min_stime    =1.0d37
    t%accum_stime    =0.0d0
    t%accum_time    =0.0d0
    t%accum_time2    =0.0d0
    t%first_time    =0.0d0
    t%last_time    =0.0d0
    t%start_etime    =0.0d0
    t%accum_etime    =0.0d0
  end subroutine timer_init

  !!---------------------------- timer_alloc ------------------------------!!
  !!---------------------------- timer_alloc ------------------------------!!
  !!---------------------------- timer_alloc ------------------------------!!

  subroutine timer_alloc (timer,status)
    !
    ! - Arguments
    !
    integer, intent (out)           :: timer
    integer, intent (out), optional :: status
    !
    ! - Local Variables
    !
    integer :: i
    !
    ! - Begin timer_alloc
    !
    timer = -1
    if (present(status)) status = -1
    do i=min_alloc_timers,max_alloc_timers
      if (.not. timers(i)%in_use) then
        timer = i
        call timer_init(timers(timer))
        timers(timer)%in_use = .true.
        if (present(status)) status = 0
        exit
      endif
    enddo
    if (timer .eq. -1) then
      write (lun, *) '*** timer_alloc: Error -- all timers already allocated'
    endif
    !
  end subroutine timer_alloc


  !!-------------------------- timer_clear_all ----------------------------!!
  !!-------------------------- timer_clear_all ----------------------------!!
  !!-------------------------- timer_clear_all ----------------------------!!

  subroutine timer_clear_all (default_lun)
    !
    ! - Arguments
    !
    integer, intent (in), optional :: default_lun
    !
    ! - Local Variables
    !
    integer    :: timer
    logical    :: in_use
    !
    ! - Begin timer_clear_all
    !
    do timer=0,max_timers
      in_use=timers(timer)%in_use
      call timer_init(timers (timer))
      timers(timer)%in_use=in_use
    enddo
    !
    if (present (default_lun)) then
      if ((default_lun > 0) .and. (default_lun < 1000)) then
        lun = default_lun
      else
        write (lun, *) '*** timer_clear_all: Error -- bad default_lun (',    &
                       default_lun, ') -- using ', lun
      end if
    end if
    !
  end subroutine timer_clear_all


  !!---------------------------- timer_clear ------------------------------!!
  !!---------------------------- timer_clear ------------------------------!!
  !!---------------------------- timer_clear ------------------------------!!

  subroutine timer_clear (timer)
    !
    ! - Arguments
    !
    integer, intent (in) :: timer
    !
    ! - Local Variables
    !
    logical  :: in_use
    !
    ! - Begin timer_clear
    !
    if ((timer > max_timers) .or. (timer < 1)) then
      write (lun, *) '*** timer_clear: Error -- bad timer (', timer, ')'
    else
      in_use=timers(timer)%in_use
      call timer_init(timers (timer)) 
      timers(timer)%in_use=in_use
    end if
    !
    !
  end subroutine timer_clear


  !!---------------------------- timer_start ------------------------------!!
  !!---------------------------- timer_start ------------------------------!!
  !!---------------------------- timer_start ------------------------------!!

  subroutine timer_start (timer)
    !
    ! - Arguments
    !
    integer, intent (in) :: timer
    !
    ! - Local Variables
    !
    double precision  :: stime
    double precision  :: utime
    integer, external :: cgetsys_time
    integer           :: istat
    !
    ! - Begin timer_start
    !
    if ((timer > max_timers) .or. (timer < 1)) then
      write (lun, *) '*** timer_start: Error -- bad timer "', timer, '"'
    else
      timers (timer) % running = .true.
      timers (timer) % count = timers (timer) % count + 1
      istat = cgetsys_time (utime, stime)
      timers (timer) % start_utime = utime
      timers (timer) % start_stime = stime
      timers (timer) % start_etime = unix_wtime()
    end if
    !
  end subroutine timer_start


  !!---------------------------- timer_stop -------------------------------!!
  !!---------------------------- timer_stop -------------------------------!!
  !!---------------------------- timer_stop -------------------------------!!

  subroutine timer_stop (timer)
    !
    ! - Arguments
    !
    integer, intent (in) :: timer
    !
    ! - Local variables
    !
    double precision  :: stime
    double precision  :: utime
    double precision  :: elapsed_utime
    double precision  :: elapsed_stime
    double precision  :: elapsed_time
    integer, external :: cgetsys_time
    integer           :: istat
    !
    ! - Begin timer_stop
    !
    istat = cgetsys_time (utime, stime)
    !
    if ((timer > max_timers) .or. (timer < 1)) then
      write (lun, *) '*** timer_stop: Error -- bad timer (', timer, ')'
    else
      timers (timer) % running     = .false.
      !
      elapsed_utime                = utime - timers (timer) % start_utime
      timers (timer) % min_utime   = min (a1 = timers (timer) % min_utime,    &
                                          a2 = elapsed_utime)
      timers (timer) % max_utime   = max (a1 = timers (timer) % max_utime,    &
                                          a2 = elapsed_utime)
      timers (timer) % accum_utime = timers (timer) % accum_utime     &
                                     + elapsed_utime
      !
      elapsed_stime                = stime - timers (timer) % start_stime
      timers (timer) % min_stime   = min (a1 = timers (timer) % min_stime,    &
                                          a2 = elapsed_stime)
      timers (timer) % max_stime   = max (a1 = timers (timer) % max_stime,    &
                                          a2 = elapsed_stime)
      timers (timer) % accum_stime = timers (timer) % accum_stime     &
                                     + elapsed_stime

      elapsed_time                 = elapsed_utime + elapsed_stime
      timers (timer) % accum_time  = timers (timer) % accum_time     &
                                     + elapsed_time
      timers (timer) % accum_time2 = timers (timer) % accum_time2    &
                                     + elapsed_time ** 2
      !
      if (timers (timer) % count == 1) then
        timers (timer) % first_time = elapsed_time
      end if
      timers (timer) % last_time = elapsed_time

      timers(timer)%accum_etime = timers(timer)%accum_etime +  &
                                  unix_wtime(timers(timer)%start_etime)
      
    end if
    !
  end subroutine timer_stop


  !!---------------------------- timer_fetch ------------------------------!!
  !!---------------------------- timer_fetch ------------------------------!!
  !!---------------------------- timer_fetch ------------------------------!!

  subroutine timer_fetch (timer, cnt_time,    &
                          min_utime, max_utime, avg_utime, sum_utime,    &
                          min_stime, max_stime, avg_stime, sum_stime,    &
                          var_time, sd_time,    elapse_time)
    !
    ! - Arguments
    !
    integer,        intent (in)  :: timer
    integer,        intent (out) :: cnt_time
    real,           intent (out) :: min_utime
    real,           intent (out) :: max_utime
    real,           intent (out) :: avg_utime
    real,           intent (out) :: sum_utime
    real,           intent (out) :: min_stime
    real,           intent (out) :: max_stime
    real,           intent (out) :: avg_stime
    real,           intent (out) :: sum_stime
    real,           intent (out) :: var_time
    real,           intent (out) :: sd_time
    real, optional, intent (out) :: elapse_time
    !
    ! - Local variables
    !
    double precision :: d_avg
    double precision :: d_cnt
    !
    ! - Begin timer_fetch
    !
    if ((timer > max_timers) .or. (timer < 1)) then
      write (lun, *) '*** timer_start: Error -- bad timer (', timer, ')'
    else
      !
      if (timers (timer) % running) then
        call timer_stop (timer = timer)
      end if
      !
      cnt_time  = timers (timer) % count
      d_cnt     = dble (cnt_time)
      !
      min_utime = real (a = timers (timer) % min_utime)
      max_utime = real (a = timers (timer) % max_utime)
      sum_utime = real (a = timers (timer) % accum_utime)
      !
      min_stime = real (a = timers (timer) % min_stime)
      max_stime = real (a = timers (timer) % max_stime)
      sum_stime = real (a = timers (timer) % accum_stime)
      !
      if (cnt_time > 0) then
        avg_utime = real (a = timers (timer) % accum_utime / d_cnt)
        avg_stime = real (a = timers (timer) % accum_stime / d_cnt)
        !
        d_avg     = timers (timer) % accum_time / d_cnt
        var_time = d_avg ** 2                                               &
                   - 2.0d0 * d_avg * timers (timer) % accum_time / d_cnt    &
                   + timers (timer) % accum_time2 / d_cnt
        if (var_time > 0.0d0) then
          sd_time  = sqrt (x = var_time)
        else
          sd_time = 0.0d0
        end if
      else
        avg_utime = 0.0
        avg_stime = 0.0

        d_avg    = 0.0d0
        var_time = 0.0d0
        sd_time  = 0.0d0
      end if
      !
      if(present(elapse_time)) elapse_time = timers (timer)%accum_etime
      
    end if
    !
  end subroutine timer_fetch


  !!---------------------------- timer_report -----------------------------!!
  !!---------------------------- timer_report -----------------------------!!
  !!---------------------------- timer_report -----------------------------!!

  subroutine timer_report (timer, label, opt_lun)
    !
    ! - Arguments
    !
    integer, intent (in)             :: timer
    character (len = *), intent (in) :: label
    integer, intent (in), optional   :: opt_lun
    !
    ! - Local variables
    !
    character (len =    2) :: c_dig_text
    character (len =    3) :: t_text
    character (len =  500) :: text_fmt

    integer                :: cnt_time
    integer                :: my_lun
    real                   :: avg_stime
    real                   :: avg_utime
    real                   :: max_stime
    real                   :: max_utime
    real                   :: min_stime
    real                   :: min_utime
    real                   :: sd_time
    real                   :: sum_stime
    real                   :: sum_utime
    real                   :: var_time
    real                   :: elapse_time
    !
    ! - Begin timer_report
    !
    if ((timer > max_timers) .or. (timer < 1)) then
      write (lun, *) '*** timer_report: Error -- bad timer (', timer, ')'
      return
    end if
    !
    call timer_fetch (timer     = timer,        &
                      cnt_time  = cnt_time,     &
                      min_utime = min_utime,    &
                      max_utime = max_utime,    &
                      avg_utime = avg_utime,    &
                      sum_utime = sum_utime,    &
                      min_stime = min_stime,    &
                      max_stime = max_stime,    &
                      avg_stime = avg_stime,    &
                      sum_stime = sum_stime,    &
                      var_time  = var_time,     &
                      sd_time   = sd_time,      &
                      elapse_time=elapse_time)
    !
    if (present (opt_lun)) then
      if ((opt_lun > 0) .and. (opt_lun < 1000)) then
        my_lun = opt_lun
      else
        write (lun, *) '*** timer_report: Error -- bad opt_lun (', opt_lun,    &
                       ') -- using ', lun
        my_lun = lun
      end if
    else
      my_lun = lun
    end if
    !
    if (cnt_time > 0) then
      write (c_dig_text, '(i2.2)') 1 + int (log10 (real (cnt_time)))
    else 
      c_dig_text = '01'
    end if
    write (t_text, '(i3)') timer
    !
    text_fmt = '(" t: ", a,1x, a16, "  trips: ", i' // c_dig_text // ', /'    &
               // '"  usr min:", g12.5, "  max:", g12.5, '                    &
               // '"  avg:",   g12.5, "  tot:", g12.5 /'                      &
               // '"  sys min:", g12.5, "  max:",   g12.5, '                  &
               // '"  avg:",   g12.5, "  tot:", g12.5 /'                      &
               // '"  var:", g12.5, "  sd:", g12.5,'                          &
               // '"  t1:",  g12.5, "  tn:", g12.5 /'                         &
               // '"  elapse time:",g12.5 )'
    write (my_lun, text_fmt) trim (string = adjustl (t_text)),                &
                             label, cnt_time,                                 &
                             min_utime, max_utime, avg_utime, sum_utime,      &
                             min_stime, max_stime, avg_stime, sum_stime,      &
                             var_time, sd_time,                               &
                             timers (timer) % first_time,                     &
                             timers (timer) % last_time, elapse_time
    !
  end subroutine timer_report


  !!----------------------------- timer_free ------------------------------!!
  !!----------------------------- timer_free ------------------------------!!
  !!----------------------------- timer_free ------------------------------!!

  subroutine timer_free (timer,status)
    !
    ! - Arguments
    !
    integer, intent (in)              :: timer
    integer, intent (out)  , optional :: status
    !
    ! - Begin timer_alloc
    !
    if (timer > 0 .and. timer < max_timers) then
      if (timers(timer)%in_use) then
        call timer_init(timers(timer)) 
        if (present(status)) status = 0
      else
        if (present(status)) status = -1
        write (lun, *) '*** timer_free: Error -- not allocated (',timer, ')'
      endif
    else
      if (present(status)) status = -1
      write (lun, *) '*** timer_free: Error -- bad timer (', timer, ')'
    endif
    !
  end subroutine timer_free



  !!---------------------------- end of module ----------------------------!!
  !!---------------------------- end of module ----------------------------!!
  !!---------------------------- end of module ----------------------------!!


end module timer_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

