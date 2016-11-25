!<CPS_v1 type="PRIMITIVE"/>

!!------------------------------- trscan.f90 --------------------------------!!
!!------------------------------- trscan.f90 --------------------------------!!
!!------------------------------- trscan.f90 --------------------------------!!
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
! Name       : trscan
! Category   : main_prog
! Written    : 1988-05-25   by: Bob Baumel
! Revised    : 2006-06-12   by: B. Menger
! Maturity   : production
! Purpose    : Scan traces and print or return diagnostics.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION    
!
!  Subroutine trscan_print scans traces and prints a summary (number of
!  traces, LAV info, execution time.)  If TRSCAN_OPT is set to 'ADVANCED'
!  in the parameter cache or the optional histo_req argument is present
!  in the call to trscan_setup, an LAV histogram will be added to the summary.
!
!  Subroutine trscan_noprint can retrieve statistics gathered by a preceding
!  call to trscan_print.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS            
!
!
!-------------------------------------------------------------------------------
!</trace_io_doc>
!
!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name        Description                           Action taken
! ----        -----------                           ------------
! NDPT        number of sample values in trace      used but not changed
! TSTRT       starting time on trace                used but not changed
! DT          trace sample interval                 used but not changed
! TRSCAN_OPT  Request printing the LAV Histogram    job data
! 
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS    
! 
! Hwd#      Description                          Action taken
! ----      -----------                          ------------
! HDR_LAV   Largest Absolute Value Header word   Used but not changed
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE       
!
!                                          opt
!                            i     i        i
!        call trscan_setup (ipn, iwhen, histo_req)
!
!                              i   i  b   i     i       i
!        call trscan_print   (ipn, n, hd, tr, prname, iwhen)
!
!                              i     i      o        o        o        o
!        call trscan_noprint (ipn, iwhen, numtrc, numdead, alavmax, alavavg,
!                                o       o
!                             alavmed, tloop)
!
!
!
! integer           ipn       =  process number
! integer           iwhen     =  1 - Before this process; 2 - after this process
!                                0 - Unlabeled, for use by a process
! logical           histo_req =  .true. -- list the LAV histogram w/ the summary
! integer           n         =  number of traces in the hd and tr arrays
! double precision  hd        =  array of header values
! real              tr        =  array of trace values
! character(len=*)  prname    =  name of process associated with this call
! integer           numtrc    =  total number of traces
! integer           numdead   =  number of dead traces
! real              alavmax   =  maximum LAV
! real              alavagv   =  average LAV
! real              alavmed   =  median LAV
! real              tloop     =  CPU time (sec) cumulative in this loop
!
!-----------------------------------------------------------------------
!                                 NOTES
!
! 1. trscan_print may be called by either an individual process or the Fortran
!    main program, but trscan_noprint will normally be called only by an indiv-
!    idual process.  trscan_noprint returns statistics collected by trscan_print
!    up to a particular point in the job, but doesn't do any printing.
!
! 2. trscan_print, called with N=0, prints the same 6 numbers that trscan_
!    noprint returns to the calling program (and also some additional numbers).
!    trscan_print prints to both the "processing messages" portion of the
!    .rpt file and to the history file.
!
! 3. trscan_setup is called prior to start of "processing", to capture 
!    job data parameter cache values while they are valid.
!
! 4. Both trscan_print and trscan_noprint routines are called only from 
!    the "processing" portion of the job.  trscan_print sets itself up 
!    when called for the first time in a job with a particular
!    (IPN,IWHEN) pair, if trscan_setup was not called for that pair.
!
! 5. All of the statistics returned by trscan_print and trscan_noprint
!    will be zero unless trscan_print was previously called with a 
!    non-zero N (a.k.a. ntr) with the specified (IPN,IWHEN) pair.
!
! 6. trscan_setup dynamically allocates 400 words of memory for each distinct
!    (IPN,IWHEN) pair with which it is used.  IPN is limited to the range
!    of 1-maxipn (1 to 199)
!
! 7. Calculation of the MEDIAN is APPROXIMATE, based on binning of the
!    LAV's of the scanned traces.  There are 381 bins, covering values
!    from 10**(-38) to 10**(+38) in multiples of 10**(0.2).  Linear
!    interpolation within the bin containing the median yields a "most
!    probable" value for the median.  This binning uses 381 of the 400
!    memory words required by trscan as described in note 5 above.
!
! 8. In case the job terminates abnormally, the following module variables
!    will contain information from the most recent call to trscan_print
!    before the error.           
!           last_ipn
!           last_ndead
!           last_nthistrp
!           last_ntrace
!           last_ntrips
!
! 9. TRSCAN can supply extended STATISTICS by setting JOB DATA parameter
!    "TRSCAN_OPT" = "ADVANCED" in the parameter cache.  The 3-line summary
!    will be followed by a histogram showing the distribution of trace LAV's.  
!    TRSCAN prints a summary when called with Ntr = NO_MORE_TRACES
!
!10. The main.f90 (cfebld jobs) uses iwhen=1 ('BEFORE') or iwhen=2 ('AFTER'), 
!    for all processes.  It is recommended that processes use iwhen=0
!    when calling TRSCAN internally, to avoid clobbering job statistics.
!
!11. A summary LAV histogram can also be requested using the optional histo_req 
!    argument to trscan_setup.  If present, 'histo_req' will overide
!    the setting for TRSCAN_OPT for that instance of ipn, iwhen.
!
!12. If JOB_DATA parameter TRSCAN_OPT is set to 'CheckLAV' then the lav
!    of each trace will be verified by a call to lav_set_hdr.  The resulting
!    LAV of each trace is compared to the LAV value of the trace on arrival
!    at TRSCAN.  Differences between initial and final LAV values are counted 
!    and reported.  The preceding process should always have set the LAV
!    correctly, and this test will help to identify processes that do not.
!
!-----------------------------------------------------------------------
!
!
!                                i     i        o         o         o
!        call trscan_get_stats (ipn, iwhen, my_stats, my_imed, my_imed_lin)
!
!                                i     i        i         i         i
!        call trscan_sum_stats (ipn, iwhen, my_stats, my_imed, my_imed_lin)
!
!
! integer       ipn       =  process number
! integer       iwhen     =  1 - Before this process; 2 - after this process
!                            0 - Unlabeled, for use by a process
! integer       my_imed(histo_size) = array of instances of values
! integer       my_imed_lin(lin_histo_size) = array of instances of values
! type (stats)  my_stats  = job statistics structure.
!
! trscan_get_stats will fetch the structure 'my_stats' from system stats for 
! a given ipn, iwhen pair.  trscan_sum_stats will sum two
! sets of statistics for a given ipn, iwhen pair, with the results in system
! stats for that ipn, iwhen pair.
!     start_time:     The earlier start_time is kept
!     histo_flag:     Set if either my_stats value or System is set
!     check_lav_flag: Set if either my_stats value or System is set
!     ndead:          Summed
!     ndpt:           Set to my_stats if System is not set
!     ntrace:         Summed
!     ntrips:         Summed
!     reset_lav:      Summed
!     dt:             Set to my_stats if System is not set
!     lav_max:        Set to MAX of my_stats value and System
!     lav_sum:        Summed
!     tstrt:          Set to my_stats if System is not set
!
!-----------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
!032. 2006-06-12  B. Menger   Removed Unused Variables.
! 31. 2004-05-26  R Selzler   Nullified pointers before first use.
!                             Checked array allocation status.
!                             KLUDGE around "save" issue (compiler bug?).
! 30. 2001-04-30  Brad Kruse  Add trscan_get_stats and trscan_sum_stats to
!                             support multi-processor job statistics.
! 29. 2001-03-13  Brad Kruse  Correct header_word_doc tag
! 28. 2000-12-08  Brad Kruse  Bug report #210.  Added explicit initial value
!                             for a(ipn,iwhen)%p%tstrt, to resolve debugger
!                             detection of reference to an uninitialized value.
! 27. 2000-12-06  Brad Kruse  Requested change to LAV validation:  Add 
!                             'CheckLAV' to combo box for TRSCAN_OPT on JOB_DATA
!                             screen.  Presence of CheckLAV value for TRSCAN_OPT
!                             will trigger the LAV test, instead of disabling
!                             the test only when in PRODLIB
! 26. 2000-08-23  Brad Kruse  Correct maturity to 'production
!                             Correct bug report 8-18-00, histogram has
!                             an extra line.
! 25. 2000-05-16  Brad Kruse  Correct problem with integer overflow on
!                              linear histogram.  Improve test logic for
!                              LAV testing.
! 24. 2000-05-08  Brad Kruse  Remove commented-out code.
!                             Replace 'patch001' comment with logic to
!                             bypass LAV check when in prodlib.
!                             Correct scaling error, when largest value is
!                             in linear median bin (med_lin).
! 23. 2000-04-05  Brad Kruse  Correct Patch001, set the Largest Absolute 
!                              Value(LAV).  Detect and report unset LAVs.
!                              Add RCS Identifier
! 22. 2000-03-22  Brad Kruse  Replace deprecated 'history_write' with the
!                             functional 'hist_write'.
! 21. 2000-03-21  Brad Kruse  CPS Testing Report, dated 3/16/00
!                             Remove non-cray check and report.  Report same
!                             precision for max and avg LAV; increase precision
!                             for median LAV.
! 20. 2000-02-02  Brad Kruse  Renamed JOB DATA parameter LAV_HISTO to TRSCAN_OPT
!.                            Added optional argument histo_req to trscan_setup.
!.                            Replaced calls to 'wuhist' with 'history_write'.
! 19. 2000-01-31  Brad Kruse  Replaced IDIAGNOS with JOB DATA parameter 
!.                            LAV_HISTO.  Temporarily re-introduce setting 
!.                            LAV (Patch001).  (This patch restores rev. 17 
!.                            from below, while restoring the LAV function 
!.                            removed in rev. 16).  
! 18. 2000/01/21  Goodger     Revision 15 put back on system.  When LAV has
!.                            been added to the individual processes, we
!                             need to go back to revision 17 from RCS.
! 17. 2000-01-20  Brad Kruse  Updated documentation.  Removed reference to CPR
!.                            output file (now .rpt).  
! 16. 2000-01-18  Brad Kruse  Removed finding LAV of trace.  Removed setting
!.                            hd(HDR_LAV) from the actual LAV of the trace.
! 15. 2000-01-10  Brad Kruse  Optimized performance of finding LAV of trace.
! 14. 2000-01-07  Brad Kruse  Correct handling of 'Not A Number' LAV header
!                             values, added trscan_setup.  Added updating 
!                             trace header word HDR_LAV (25).
! 13. 2000-01-04  Brad Kruse  Restructure the way stats are allocated, and how
!                             the allocation is checked.  Some improvement in
!                             performance.
! 12. 1999-12-29  Brad Kruse  Removed several dead variables, reduced pointer
!                             de-referencing for performance, repackaged
!                             common code (trscan_eval_stats), 
!                             removed old XML tags.
! 11. 1999/12/21  Goodger     Remove use of cps_module.
! 10. 1999/09/15  Dorman      Convert to revised CPS system in Fortran 90
!  9. 1999/01/11  Goodger     Begin using the fortran90 compiler.
!  8. 1996/07/01  Vunderink   Moved RMASK from PARAMETER statment to fix
!                             Fortran 90 compile
!  7. 1990/06/07  M Howard    Change MAXIPN to 199.
!  6. 1989/11/10  Bob Baumel, Add extended statistics (IDIAGNOS=3) option.
!  5. 1989/03/04  Bob Baumel, Take out scan for illegal Cray data values.
!               Instead, assume non-Cray data when get flag HD(25)<0.
!  4. 1989/02/02  Bob Baumel, Print NDPT,DT,TSTRT for 1st 2 traces in
!               extended diagnostic mode.
!  3. 1989/01/31  Bob Baumel, Allocate memory dynamically, print expanded
!               diagnostics, make robust for illegal Cray data values.
!  2. 1988/09/29  JB Sinton,  NWIH conversion.
!  1. 1988/05/25  Bob Baumel, Original version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS                
!
! This process requires 8-byte (64-bit) word sizes for header values.
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS             
!
!
!-------------------------------------------------------------------------------
!</compile_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS          
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES                    
!
!  trscan_print and trscan_noprint replace TRSCAN and TRSCAN1 in the old CPS
!  system.  However, trscan_print and trscan_noprint are not called directly
!  by the CPS main program.  Instead, the CPS main program calls cps_trscan, 
!  which in turn calls trscan_print and/or trscan_noprint.
!
!  A suggested sequence for calling trscan_setup from a process to request 
!  an LAV histogram in the summary:
!
!    call trscan_setup (ipn = obj%ipn, iwhen = local_when, histo_req = .true.)
!
!  The hist_write history primitive requires that hist_init be called for
!  each ipn.  CPS_module performs this call for trscan; a non-CPS application
!  will also have to invoke hist_init for each unique ipn value.
!
!-------------------------------------------------------------------------------
!</programming_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!

    module trscan_module
      !
      ! - Module references
      !
      use pc_module
      !
      use named_constants_module, only:  &     
            HDR_SEQUENCE,           &    !  1
            HDR_TOP_MUTE,           &    !  2
            HDR_CURRENT_GROUP,      &    !  3
            HDR_CURRENT_CHANNEL,    &    !  4
            HDR_FOLD,               &    !  5
            HDR_OFFSET,             &    !  6
            HDR_MIDPOINT_XGRID,     &    !  7
            HDR_MIDPOINT_XLOC,      &    ! 17
            HDR_LAV                      ! 25
      !
      use getsys_module, only:   &
            getsys_utime
      !
      use lav_module, only:    &
            lav_set_hdr
      !
      use hist_module, only:   &
            hist_ok,           &
            hist_write
      !
      use string_module, only:   &
            string_to_upper

      !
      implicit none

      private
      public :: trscan_setup
      public :: trscan_print
      public :: trscan_noprint
      public :: trscan_get_stats
      public :: trscan_sum_stats

      character(len=100),public,save :: trscan_ident =    &
       '$Id: trscan.f90,v 1.32 2006/06/12 13:03:57 Menger prod sps $'

!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!

      integer, parameter :: maxipn = 199

      integer, parameter :: histo_bound_hi          = +191
      integer, parameter :: histo_bound_lo          = -189
      integer, parameter, public                                          &
                         :: histo_size              = histo_bound_hi      &
                                                      - histo_bound_lo    &
                                                      + 1
      real,    parameter :: histo_offset            = 190.0
      real,    parameter :: lin_histo_bin_size      = 0.1
      real,    parameter :: lin_histo_bin_size_inv  = 1.0 / lin_histo_bin_size
      real,    parameter :: half_lin_histo_bin_size = 0.5 * lin_histo_bin_size
      integer, parameter, public    &
                         :: lin_histo_size          = 100
      real,    parameter :: lin_histo_range_hi      = lin_histo_size   &
                                                      * lin_histo_bin_size
      type, public :: stats
        private
        double precision :: start_time
        logical          :: histo_flag
        logical          :: check_lav_flag
        integer          :: ndead
        integer          :: ndpt
        integer          :: ntrace
        integer          :: ntrips
        integer          :: reset_lav
        real             :: dt
        real             :: lav_max
        real             :: lav_sum
        real             :: tstrt
      end type stats
      !
      ! - To implement an array of pointers, it is necessary to define
      !   a derived type that contains a pointer: stats_pointer
      !
      type stats_pointer
        type(stats), pointer                :: p
        integer,     pointer, dimension (:) :: imed
        integer,     pointer, dimension (:) :: imed_lin
      end type stats_pointer
      !
      ! - Define an array of pointers:
      !
      ! KLUDGE 22 Mar 2004 R Selzler
      ! The previous version declared the "a" variable with "save".
      ! The Absoft 8.0 compiler with the latest quick fix applied
      ! on the special lx747 node (2.4.21 kernel, libc libc-2.3.2.so,
      ! libpthread-0.10.so) compiled okay, but execution seg faults.
      ! For some unknown reason, the Fortran "allocate" assigns -1 to
      ! address for stats_pointer, giving a seg fault when they are used.
      ! Simply removing the "save" makes the problem go away.
      ! Removing the "save" is NOT a fix, only a work around.
      type (stats_pointer), dimension (maxipn, 0:2)       :: a
      logical,              dimension (maxipn, 0:2), save :: a_init = .false.
      !
      ! - Refer to the ipn-th, iwhen-th instance of the pointer as
      !                    a(ipn,iwhen)%p
      !
      character (len=*), parameter, dimension (0:2) :: when     &
        = (/'       ', 'BEFORE ', ' AFTER '/)
      logical, save :: first_time_flag = .true.
      real,    save :: afact           = 0.460517018
      real,    save :: arecip          = 2.17147241
      integer, save :: lun             = 6
      !
      ! - These values were globally accessible.
      !   They are kept here for crash recovery
      !
      integer, save, public :: last_ipn
      integer, save, public :: last_ndead
      integer, save, public :: last_nthistrp
      integer, save, public :: last_ntrace
      integer, save, public :: last_ntrips

      contains

!!------------------------------ trscan_setup ------------------------------!!
!!------------------------------ trscan_setup ------------------------------!!
!!------------------------------ trscan_setup ------------------------------!!


      subroutine trscan_setup (ipn, iwhen, histo_req)
        !
        ! - Arguments
        !
        integer, intent (in)           :: ipn
        integer, intent (in)           :: iwhen
        logical, intent (in), optional :: histo_req
        !
        ! - Local variables
        !
        integer             :: ierr
        character (len = 8) :: histo_str
        integer :: i, j
        logical, save :: first_time
        data first_time /.true./
        !
        ! - Begin trscan_setup
        !
        if(first_time) then
            first_time = .false.
            do i=1,maxipn
                do j=0,2
                    nullify(a(i,j)%p)
                    nullify(a(i,j)%imed)
                    nullify(a(i,j)%imed_lin)
                end do
            end do
        end if

        if ((ipn < 1) .or. (ipn > maxipn)) then
          !
          call pc_error (msg1 = "trscan: Invalid ipn (",    &
                         var1 = ipn,                        &
                         msg2 = ") must be 1 <= ipn <= ",   &
                         var2 = maxipn)
          return
          !
        else if ((iwhen < 0) .or. (iwhen > 2)) then
          !
          call pc_error (msg1 = "trscan: Invalid iwhen (",    &
                         var1 = iwhen,                        &
                         msg2 = ") must be 0 <= iwhen <= 2")
          return
          !
        end if
        !
        ! - Allocate static storage for the stats
        !
        if (associated (a (ipn, iwhen)%imed)) deallocate (a (ipn, iwhen)%imed)
        allocate (a (ipn, iwhen)%imed(histo_size), stat=ierr)
          if (ierr /= 0) then
            call pc_error (msg1 ='Error allocating imed stats for ipn ',   &
                           var1 = ipn,                                &
                           msg2 = '   Allocate error ',               &
                           var2 = ierr)
            return
          end if
        !
        if (associated (a (ipn, iwhen)%imed_lin)) then
          deallocate (a (ipn, iwhen)%imed_lin)
        end if
        allocate (a (ipn, iwhen)%imed_lin(lin_histo_size), stat=ierr)
          if (ierr /= 0) then
            call pc_error (msg1 ='Error allocating imed_line stats for ipn ',&
                           var1 = ipn,                                &
                           msg2 = '   Allocate error ',               &
                           var2 = ierr)
            return
          end if
        !
        if (associated (a (ipn, iwhen)%p)) deallocate (a (ipn, iwhen)%p)
        allocate(a(ipn,iwhen)%p,stat=ierr)
          if (ierr /= 0) then
            call pc_error (msg1 ='Error allocating p stats for ipn ',   &
                           var1 = ipn,                                &
                           msg2 = '   Allocate error ',               &
                           var2 = ierr)
            return
          end if
        a_init (ipn,iwhen) = .true.
        !
        ! - Set a default ndpt in case there is not one defined,
        !   but then get the correct value.
        !
        a(ipn,iwhen)%p%ndpt = -1
        call pc_get_global ('NDPT', a(ipn,iwhen)%p%ndpt)
        if (a(ipn,iwhen)%p%ndpt == -1) then
          call pc_warning (msg1 = "TRScan -- Global NDPT is not set")
        end if
        !
        a(ipn,iwhen)%p%tstrt = 0.0
        call pc_get_global ('TSTRT', a(ipn,iwhen)%p%tstrt)
        !
        a(ipn,iwhen)%p%dt = -1.0
        call pc_get_global ('DT', a(ipn,iwhen)%p%dt)
        if (a(ipn,iwhen)%p%dt == -1.0) then
          call pc_warning (msg1 = "TRScan -- Global DT is not set")
        end if
        !
        ! - TRSCAN_OPT will be true or histo_req will be present if a process 
        !   wants to see the LAV histogram printed with the summary
        !
        a(ipn,iwhen)%p%histo_flag     = .false.
        !
        if (present (histo_req)) then
          !
          a(ipn,iwhen)%p%histo_flag = histo_req
          !
        end if
        !
        histo_str = ''
        !
        call pc_get_jdata (keyword = 'TRSCAN_OPT',    &
                           scalar  = histo_str)
        !
        call string_to_upper (histo_str)
        !
        if (histo_str == 'ADVANCED') then
          a(ipn,iwhen)%p%histo_flag = .true.
        end if
        !
        a(ipn,iwhen)%p%check_lav_flag = histo_str == 'CHECKLAV'
        !
        ! - These 'constants' must be calculated
        !
        afact  = log (x = 10.0) / 5.0
        arecip = 1.0 / afact
        lun    = pc_get_lun ()
        !
        ! - Initialize the data collection
        !
        a(ipn,iwhen)%imed         = 0
        a(ipn,iwhen)%imed_lin     = 0
        a(ipn,iwhen)%p%start_time = getsys_utime()
        a(ipn,iwhen)%p%lav_max    = 0.0
        a(ipn,iwhen)%p%lav_sum    = 0.0
        a(ipn,iwhen)%p%ntrace     = 0
        a(ipn,iwhen)%p%ndead      = 0
        a(ipn,iwhen)%p%ntrips     = 0
        a(ipn,iwhen)%p%reset_lav  = 0
        !
      end subroutine trscan_setup

!!------------------------------ trscan_print ------------------------------!!
!!------------------------------ trscan_print ------------------------------!!
!!------------------------------ trscan_print ------------------------------!!


      subroutine trscan_print (ipn, n, hd, tr, prname, iwhen)
        !
        ! - Arguments
        !
        integer,             intent (in)    :: ipn
        integer,             intent (in)    :: n
        double precision,    intent (inout) :: hd (:,:)
        real,                intent (in)    :: tr (:,:)
        character (len = *), intent (in)    :: prname
        integer,             intent (in)    :: iwhen
        !
        ! - Local variables
        !
        double precision, allocatable :: lav_in (:)
        double precision, allocatable :: lav_diff (:)
        double precision :: lav_max
        double precision :: lav_min
        !

        character (len =   7) :: ipn_str              ! local variable
        integer               :: i                    ! local variable
        integer               :: i2                   ! local variable
        integer               :: iloc                 ! local variable
        integer               :: intarg               ! local variable
        integer               :: ierr                 ! local variable


        logical               :: stats_exist          ! local variable
        real                  :: tlav                 ! local variable


        type (stats)          :: my_stats             ! local variable
        !
        ! - Begin trscan_print
        !
        if(n < 0 ) return
        allocate(lav_in(n),stat=ierr)
        if(ierr /= 0 ) then
            call pc_error (msg1 ='Error allocating lav_in for ipn ',   &
                           var1 = ipn,                                &
                           msg2 = '   Allocate error ',               &
                           var2 = ierr)
            return
          ! errors
        endif
        allocate(lav_diff(n),stat=ierr)
        if(ierr /= 0 ) then
            call pc_error (msg1 ='Error allocating lav_diff for ipn ',   &
                           var1 = ipn,                                &
                           msg2 = '   Allocate error ',               &
                           var2 = ierr)
            return
          ! errors
        endif
        ! from here on, go to 999 instead of return in order to deallocate lav_in, lav_diff
        if ((ipn < 1) .or. (ipn > maxipn)) then
          !
          call pc_error (msg1 = "trscan: Invalid ipn (",    &
                         var1 = ipn,                        &
                         msg2 = ") must be 1 <= ipn <= ",   &
                         var2 = maxipn)
          goto 999
          !
        else if ((iwhen < 0) .or. (iwhen > 2)) then
          !
          call pc_error (msg1 = "trscan: Invalid iwhen (",    &
                         var1 = iwhen,                        &
                         msg2 = ") must be 0 <= iwhen <= 2")
          goto 999
          !
        end if
        !
        stats_exist = a_init (ipn,iwhen)
        !
      Check_New_Stats:   &
        if(.not. stats_exist) then
          call pc_warning (msg1 = "trscan_setup was not called during "    &
                                  // "setup for ipn =",                    &
                           var1 = ipn,                                     &
                           msg2 = " and iwhen = ",                         &
                           var2 = iwhen)
          call trscan_setup (ipn   = ipn,    &
                             iwhen = iwhen)
        end if Check_New_Stats
        !
        my_stats = a(ipn,iwhen)%p
        !
      verify_traces_present:    &
        if (n > 0) then
          !
          if (my_stats%ntrips == 0) then 
            my_stats%start_time = getsys_utime()
          end if
          !
          my_stats%ntrips = my_stats%ntrips + 1
          !
          ! - This code sets trace LAV, and flags if the 
          !   setting is different than the initial setting.  
          !   The purpose is to detect when the preceding 
          !   process changes traces but neglects to set 
          !   HDR_LAV.
          !
          !   Because this check is used to identify 
          !   coding errors, it need not be run by users, 
          !   when running from the production library.
          !
        validate_lav_value:    &
          if (my_stats%check_lav_flag) then
            !
            lav_in = hd (HDR_LAV, 1:n)
            !
            call lav_set_hdr (hd   = hd,               &
                              tr   = tr,               &
                              ndpt = my_stats%ndpt,    &
                              ntr  = n)
            !
            lav_diff = hd (HDR_LAV, 1:n) - lav_in
            lav_min = minval (lav_diff)
            lav_max = maxval (lav_diff)
            !
            if ((abs (lav_min) > 0.0d0) .or. (abs (lav_max) > 0.0d0)) then
              my_stats%reset_lav = my_stats%reset_lav + 1
            end if
            !
          end if validate_lav_value
          !
        loop_thru_traces:    &
          do i = 1, n
            !
            my_stats%ntrace = my_stats%ntrace + 1
            tlav            = real (a = hd (HDR_LAV,i))
            !
            if(tlav > 0.0) then
              !
              my_stats%lav_max = max (a1 = my_stats%lav_max, a2 = tlav)
              my_stats%lav_sum = my_stats%lav_sum + tlav
              intarg           = nint (arecip * log (tlav) + histo_offset)
              iloc             = min (a1 = max (a1 = intarg, a2 = 1),    &
                                      a2 = histo_size)
              a(ipn,iwhen)%imed(iloc) = a(ipn,iwhen)%imed(iloc) + 1
              !
              if ((iloc >= 185) .and. (iloc <= 195)) then
                !
                ! - Scale to linear bin size and shift to bin ctr
                !
                intarg = nint ((tlav + half_lin_histo_bin_size)  &
                               * lin_histo_bin_size_inv)
                iloc             = min (a1 = max (a1 = intarg, a2 = 1),    &
                                        a2 = lin_histo_size)
                !
                a (ipn, iwhen)%imed_lin (iloc)     &
                  = a (ipn, iwhen)%imed_lin (iloc) + 1
                !
              end if
              !
            else
              !
              my_stats%ndead = my_stats%ndead + 1
              !
            end if
            !
          end do loop_thru_traces
          !
        else if (n == NO_MORE_TRACES) then verify_traces_present
          !
          if (stats_exist) then
            call trscan_print_summary (my_stats = my_stats,                 &
                                       imed     = a(ipn,iwhen)%imed,        &
                                       imed_lin = a(ipn,iwhen)%imed_lin,    &
                                       n        = n,                        &
                                       ipn      = ipn,                      &
                                       prname   = when (iwhen) // prname)
          end if
          !
        elseif (n /= NEED_TRACES) then verify_traces_present
          !
          ! - Panic exit -- as a diagnostic aid, dump all known
          !   stats packages.
          !
          write (lun, *) "n = FATAL_ERROR, dumping known stats"
          !
        loop_known_ipns:    &
          do i = 1, maxipn
            !
            write (ipn_str, '("IPN ", i3)') i
            !
            do i2 = 1, 2
              if (a_init (i,i2)) then  
                write (lun, *) 
                call trscan_print_summary (my_stats = a (i, i2)%p,           &
                                           imed     = a (i, i2)%imed,        &
                                           imed_lin = a (i, i2)%imed_lin,    &
                                           n        = 0,                     &
                                           ipn      = i,                     &
                                           prname   = when (i2) // ipn_str)
                !
              end if
            end do
            !
          end do loop_known_ipns
          !
          goto 999
          !
        end if verify_traces_present
        !
        last_ipn      = ipn
        last_ntrips   = my_stats%ntrips
        last_nthistrp = n
        last_ntrace   = my_stats%ntrace
        last_ndead    = my_stats%ndead
        !
        ! - Store the local stats in the static structure
        !
        a(ipn,iwhen)%p = my_stats 
999     continue
        deallocate(lav_in)
        deallocate(lav_diff)
        !
      end subroutine trscan_print

!!----------------------------- trscan_noprint -----------------------------!!
!!----------------------------- trscan_noprint -----------------------------!!
!!----------------------------- trscan_noprint -----------------------------!!

      subroutine trscan_noprint (ipn, iwhen, numtrc, numdead, alavmax,&
                                 alavavg, alavmed, tloop)

        integer, intent(in)  :: ipn, iwhen
        integer, intent(out) :: numtrc, numdead
        real,    intent(out) :: alavmax, alavavg, alavmed, tloop
        !








        !
        ! - Begin trscan_noprint
        !
        if ((ipn < 1) .or. (ipn > maxipn)) then
          !
          call pc_error (msg1 = "trscan: Invalid ipn (",    &
                         var1 = ipn,                        &
                         msg2 = ") must be 1 <= ipn <= ",   &
                         var2 = maxipn)
          return
          !
        else if ((iwhen < 0) .or. (iwhen > 2)) then
          !
          call pc_error (msg1 = "trscan: Invalid iwhen (",    &
                         var1 = iwhen,                        &
                         msg2 = ") must be 0 <= iwhen <= 2")
          return
          !
        end if
        !
        if (associated(a(ipn,iwhen)%p)) then
          !
          call trscan_eval_lav (my_stats = a(ipn,iwhen)%p,          &
                                imed     = a(ipn,iwhen)%imed,       &
                                imed_lin = a(ipn,iwhen)%imed_lin,   &
                                alavmax  = alavmax,                 &
                                alavavg  = alavavg,                 &
                                alavmed  = alavmed)
          !
          numtrc  = a(ipn,iwhen)%p%ntrace
          numdead = a(ipn,iwhen)%p%ndead
          tloop   = getsys_utime() - a(ipn,iwhen)%p%start_time
        else
          numtrc  = 0
          numdead = 0
          alavmax = 0.0
          alavavg = 0.0
          alavmed = 0.0
          tloop   = 0.0
        end if

      end subroutine trscan_noprint

!!---------------------------- trscan_eval_lav -----------------------------!!
!!---------------------------- trscan_eval_lav -----------------------------!!
!!---------------------------- trscan_eval_lav -----------------------------!!

      subroutine trscan_eval_lav (my_stats, imed, imed_lin,    &
                                  alavmax, alavavg, alavmed)
        !
        ! - Arguments
        !
        type(stats), intent (in)  :: my_stats 
        integer,     intent (in)  :: imed (histo_size)
        integer,     intent (in)  :: imed_lin (lin_histo_size)
        real,        intent (out) :: alavmax
        real,        intent (out) :: alavavg
        real,        intent (out) :: alavmed
        !
        ! - Local Variables
        !

        integer :: iloc     
        integer :: ncheck   
        integer :: ntot     
        integer :: num_live 
        real    :: acheck   
        real    :: adjacent_bins
        real    :: aloc     
        real    :: interp_bias
        real    :: near_bins
        !
        ! - Begin trscan_eval_lav
        !
        alavmax = my_stats%lav_max
        alavavg = 0.0
        alavmed = 0.0
        !
        num_live = my_stats%ntrace - my_stats%ndead
        !
        if (num_live > 0) then
          !
          alavavg = my_stats%lav_sum / num_live
          ncheck  = (num_live + 1) / 2
          ntot    = 0
          aloc    = 0.0
          !
          ! - First check to see if the median is on the linear histogram
          !
          if ((imed_lin (1) < ncheck)     &
              .and. (imed_lin (lin_histo_size) < ncheck)) then
            !
            do iloc = 1, lin_histo_size
              ntot = ntot + imed_lin (iloc)
              if(ntot >= ncheck) exit
            end do
            !
            ! - Interpolate within the bin depending on number of samples
            !   in adjacent bins
            !
            adjacent_bins = real (a = imed_lin (iloc+1) - imed_lin (iloc-1))
            near_bins     = real (a = imed_lin (iloc-1)     &
                                      + imed_lin (iloc)     &
                                      + imed_lin (iloc+1))
            interp_bias   = min (a1 = max (a1 = adjacent_bins / near_bins,    &
                                           a2 = -0.5),                        &
                                 a2 = 0.5)
            !
            alavmed       = (real (iloc) + interp_bias) * lin_histo_bin_size
            !
          else
            !
            acheck  = real (a = num_live + 1) / 2.0
            !
            do iloc = 1, histo_size
              ntot = ntot + imed(iloc)
              if(ntot >= ncheck) exit
            end do
            !
            aloc = iloc + 0.5 + (acheck-ntot-0.5) / imed(iloc)
            alavmed  = exp (x = afact * (aloc - histo_offset))
            !
          end if
          !
        end if
        !
      end subroutine trscan_eval_lav


!!-------------------------- trscan_print_summary --------------------------!!
!!-------------------------- trscan_print_summary --------------------------!!
!!-------------------------- trscan_print_summary --------------------------!!

      subroutine trscan_print_summary (my_stats, imed, imed_lin, n, ipn, prname)
        !
        ! - Arguments
        !
        type (stats), intent (inout)     :: my_stats
        integer, intent (inout)          :: imed (:)
        integer, intent (inout)          :: imed_lin (:)
        integer, intent (in)             :: ipn
        integer, intent (in)             :: n
        character (len = *), intent (in) :: prname
        !
        ! - Local parameters
        !
        integer, parameter :: stars_len = 52
        character (len = stars_len), parameter :: stars     &
          = '****************************************************'
        !
        ! - Local Variables
        !
        character (len = 132) :: card
        character (len = 132) :: format
        double precision      :: time
        integer               :: i
        integer               :: ifirst
        integer               :: first_bin_stop
        integer               :: last_bin_start
        integer               :: ifirst_lin
        integer               :: ilast
        integer               :: ilast_lin
        integer               :: iloc
        integer               :: ncheck
        integer               :: nmax
        integer               :: nmax1
        integer               :: nmax2
        integer               :: nstar
        integer               :: ntot
        integer               :: num_live
        real                  :: acheck
        real                  :: aloc
        real                  :: amaxlav
        real                  :: amedian
        real                  :: avglav
        real                  :: centval
        real                  :: cfact
        !
        ! - Begin trscan_print_summary
        !
        num_live = my_stats%ntrace - my_stats%ndead
        !
        if (num_live <= 0) then
          amaxlav = 0.0
          avglav  = 0.0
          amedian = 0.0
        else
          amaxlav  = my_stats%lav_max
          avglav   = my_stats%lav_sum / num_live
          acheck   = real (a = num_live + 1) / 2.0
          ncheck   = (num_live + 1) / 2
          ntot     = 0
          !
          do iloc = 1, histo_size
            ntot = ntot + imed (iloc)
            if(ntot >= ncheck) exit
          end do
          !
          aloc = iloc + 0.5 + (acheck - ntot - 0.5) / imed (iloc)
          amedian  = exp (x = afact * (aloc - histo_offset))
        end if
        !
        time = getsys_utime() - my_stats%start_time

        format = "(a16,'(IPN=',i3,'),',i12,' TRIPS THRU LOOP,',1pe12.5,"   &
                 // "' SEC IN LOOP')"
        write (card, format)  prname, ipn, my_stats%ntrips, time
        write (lun, '(a80)') card
        if (hist_write (ipn, card) /= hist_ok) then
          call pc_error ("CPS/TRSCAN: Error calling hist_write: ")
          call pc_error (trim (card))
        end if

        format = "(i16, ' TRACES, ',i12, ' DEAD', 4x,"    &
                 // "'MEDIAN LAV=', e13.6, ' (approx.)')"
        write (card, format) my_stats%ntrace, my_stats%ndead, amedian
        write (lun,'(a80)') card
        if (hist_write (ipn, card) /= hist_ok) then
          call pc_error ("CPS/TRSCAN: Error calling hist_write: ")
          call pc_error (trim (card))
        end if

        format = "(14x, 'MAX LAV=', e13.6, 4x, 'AVG LAV=', e13.6)"
        write (card,format)      amaxlav, avglav
        write (lun,'(a80)') card
        if (hist_write (ipn, card) /= hist_ok) then
          call pc_error ("CPS/TRSCAN: Error calling hist_write: ")
          call pc_error (trim (card))
        end if

        if (my_stats%reset_lav > 0) then
          format = "(14x, '*** LAV Reset by TRSCAN on ', i7, ' trips ***')"
          write (card,format)  my_stats%reset_lav
          write (lun,'(a80)') card
          if (hist_write (ipn, card) /= hist_ok) then
            call pc_error ("CPS/TRSCAN: Error calling hist_write: ")
            call pc_error (trim (card))
          end if
        end if

        if (.not. my_stats%histo_flag) then
          return
        end if

        if (my_stats%ntrace .eq. my_stats%ndead) then
          return
        end if
        !
        ! - Print the histogram
        !
        ifirst = histo_size
        ilast  = histo_size
        !
        do i = 1, histo_size
          if (imed (i) /= 0) then
            ifirst = i
            exit
          end if
        end do

        do i = histo_size, ifirst, -1
          if (imed (i) /= 0) then
            ilast = i
            exit
          end if
        end do
        !
        ifirst_lin = lin_histo_size + 1
        ilast_lin  = 0
        !
        first_bin_stop = 184
        last_bin_start = 196
        !
        if ((ifirst < last_bin_start) .and. (ilast > first_bin_stop)) then
          !
          do i = 1, lin_histo_size
            if (imed_lin (i) /= 0) then
              ifirst_lin = i
              exit
            end if
          end do
          !
          do i = lin_histo_size, 1, -1
            if (imed_lin (i) /= 0) then
              ilast_lin = i
              exit
            end if
          end do
          !
          nmax2 = maxval (array = imed_lin (ifirst_lin:ilast_lin), dim = 1)
          !
        else
          !
          nmax2 = 0
          !
          first_bin_stop = ilast
          last_bin_start = histo_size + 1
          !
        end if
        !
        nmax1 = maxval (array = imed (ifirst:ilast), dim = 1)
        nmax  = max (a1 = nmax1,    &
                     a2 = nmax2)
        !
        cfact = real   (a = stars_len) / real (a = nmax)
        write(lun, 401)
        !
        do iloc = ifirst, first_bin_stop
          centval = exp  (x = afact * real (a = iloc - 190))
          nstar   = nint (a = cfact * imed (iloc))
          if (nstar == 0)  then
            write (lun, 402) centval,imed(iloc)
          else
            write (lun, 402) centval,imed(iloc), stars(1:nstar)
          end if
        end do

        do iloc = ifirst_lin, ilast_lin
          centval = real (iloc) * lin_histo_bin_size
          nstar = nint (a = cfact * imed_lin (iloc))
          if (nstar == 0)  then
            write (lun, 402) centval,imed_lin (iloc)
          else
            write (lun, 402) centval,imed_lin (iloc), stars(1:nstar)
          end if
        end do

        do iloc = last_bin_start, ilast
          centval = exp (x = afact * real (a = iloc - 190))
          nstar = nint (a = cfact * imed (iloc))
          if (nstar == 0)  then
            write (lun, 402) centval,imed(iloc)
          else
            write (lun, 402) centval,imed(iloc), stars(1:nstar)
          end if
        end do
        write(lun, *)

   401  format(/3x,'BIN CENTER',5x,'# LAVS')
   402  format(1x,1pe12.3,i10,5x,a)

      end subroutine trscan_print_summary


!!---------------------------- trscan_get_stats -----------------------------!!
!!---------------------------- trscan_get_stats -----------------------------!!
!!---------------------------- trscan_get_stats -----------------------------!!

      subroutine trscan_get_stats (ipn, iwhen, my_stats, my_imed, my_imed_lin)

        integer,      intent(in)  :: ipn, iwhen
        integer,      intent(out) :: my_imed(histo_size)
        integer,      intent(out) :: my_imed_lin(lin_histo_size)
        type (stats), intent(out) :: my_stats 
        !
        logical :: stats_exist          ! local variable
        !
        ! - Begin trscan_get_stats
        !
        if ((ipn < 1) .or. (ipn > maxipn)) then
          !
          call pc_error (msg1 = "trscan: Invalid ipn (",    &
                         var1 = ipn,                        &
                         msg2 = ") must be 1 <= ipn <= ",   &
                         var2 = maxipn)
          return
          !
        else if ((iwhen < 0) .or. (iwhen > 2)) then
          !
          call pc_error (msg1 = "trscan: Invalid iwhen (",    &
                         var1 = iwhen,                        &
                         msg2 = ") must be 0 <= iwhen <= 2")
          return
          !
        end if
        !
        stats_exist = a_init (ipn,iwhen)
        !
      Check_New_Stats:   &
        if(.not. stats_exist) then
          call pc_warning (msg1 = "trscan_setup was not called during "    &
                                  // "setup for ipn =",                    &
                           var1 = ipn,                                     &
                           msg2 = " and iwhen = ",                         &
                           var2 = iwhen)
          call trscan_setup (ipn   = ipn,    &
                             iwhen = iwhen)
        end if Check_New_Stats
        !
        my_stats    = a (ipn, iwhen)%p
        my_imed     = a (ipn, iwhen)%imed
        my_imed_lin = a (ipn, iwhen)%imed_lin
        !
        !
      end subroutine trscan_get_stats


!!---------------------------- trscan_sum_stats ----------------------------!!
!!---------------------------- trscan_sum_stats ----------------------------!!
!!---------------------------- trscan_sum_stats ----------------------------!!

      subroutine trscan_sum_stats (ipn, iwhen, my_stats,    &
                                      my_imed, my_imed_lin)

        integer,      intent(in) :: ipn, iwhen
        integer,      intent(in) :: my_imed(histo_size)
        integer,      intent(in) :: my_imed_lin(lin_histo_size)
        type (stats), intent(in) :: my_stats 
        !
        integer :: i
        logical :: stats_exist          ! local variable
        !
        ! - Begin trscan_sum_stats
        !
        if ((ipn < 1) .or. (ipn > maxipn)) then
          !
          call pc_error (msg1 = "trscan: Invalid ipn (",    &
                         var1 = ipn,                        &
                         msg2 = ") must be 1 <= ipn <= ",   &
                         var2 = maxipn)
          return
          !
        else if ((iwhen < 0) .or. (iwhen > 2)) then
          !
          call pc_error (msg1 = "trscan: Invalid iwhen (",    &
                         var1 = iwhen,                        &
                         msg2 = ") must be 0 <= iwhen <= 2")
          return
          !
        end if
        !
        stats_exist = a_init (ipn,iwhen)
        !
      Check_New_Stats:   &
        if(.not. stats_exist) then
          call pc_warning (msg1 = "trscan_setup was not called during "    &
                                  // "setup for ipn =",                    &
                           var1 = ipn,                                     &
                           msg2 = " and iwhen = ",                         &
                           var2 = iwhen)
          call trscan_setup (ipn   = ipn,    &
                             iwhen = iwhen)
        end if Check_New_Stats
        !
        do i = 1, histo_size
          a (ipn, iwhen)%imed (i)     = a (ipn, iwhen)%imed (i) + my_imed (i)
        end do
        !
        do i = 1, lin_histo_size
          a (ipn, iwhen)%imed_lin (i) = a (ipn, iwhen)%imed_lin (i)    &
                                        + my_imed_lin (i)
        end do
        !
        a (ipn, iwhen)%p%start_time = max (a1 = a (ipn, iwhen)%p%start_time,  &
                                           a2 = my_stats%start_time)
        !
        if (my_stats%histo_flag) then
          a (ipn, iwhen)%p%histo_flag = my_stats%histo_flag
        end if
        !
        if (my_stats%check_lav_flag) then
          a (ipn, iwhen)%p%check_lav_flag = my_stats%check_lav_flag
        end if
        !
        a (ipn, iwhen)%p%ndead  = a (ipn, iwhen)%p%ndead  + my_stats%ndead
        !
        if (a (ipn, iwhen)%p%ndpt == 0) then
          a (ipn, iwhen)%p%ndpt = my_stats%ndpt
        end if
        !
        a (ipn, iwhen)%p%ntrace = a (ipn, iwhen)%p%ntrace + my_stats%ntrace
        a (ipn, iwhen)%p%ntrips = a (ipn, iwhen)%p%ntrips + my_stats%ntrips
        a (ipn, iwhen)%p%reset_lav = a (ipn, iwhen)%p%reset_lav    &
                                       + my_stats%reset_lav
        !
        if (a (ipn, iwhen)%p%dt == 0.0) then
          a (ipn, iwhen)%p%dt = my_stats%dt
        end if
        !
        a (ipn, iwhen)%p%lav_max = max (a1 = a (ipn, iwhen)%p%lav_max,    &
                                        a2 = my_stats%lav_max)
        a (ipn, iwhen)%p%lav_sum = a (ipn, iwhen)%p%lav_sum + my_stats%lav_sum
        !
        if (a (ipn, iwhen)%p%tstrt == 0.0) then
          a (ipn, iwhen)%p%tstrt = my_stats%tstrt
        end if
        !
      end subroutine trscan_sum_stats



!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

    end module trscan_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
