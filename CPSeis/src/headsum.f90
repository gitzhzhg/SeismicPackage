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
!!------------------------------ headsum.f90 ---------------------------------!!
!!------------------------------ headsum.f90 ---------------------------------!!
!!------------------------------ headsum.f90 ---------------------------------!!

!<brief_doc>
!-------------------------------------------------------------------------------
!                         C P S   P R O C E S S
!
! Name       : HEADSUM     (HEADer word SUMmary)
! Category   : Diagnostics
! Written    : 1992-09-28   by: Bill Troutt
! Revised    : 2010-01-08   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Summarize range of all header word values
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
! HEADSUM examines each header word of all the traces passing through the
! process.  It displays in the .rpt file for each header word:  the minimum and
! maximum values observed, the number of zeros and the minimum and maximum
! values of the increment between header values for consecutive traces.  (The
! minimum and maximum value calculation ignores zeros.)
!
! HEADSUM optionally calculates and displays a histogram of trace LAVs.
!
! HEADSUM has no effect on traces passing through the process.
! 
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! 
! HEADSUM will operate only on those traces passed by the header flag logic.
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
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
!
! Name      Description                           Action taken
! ----      -----------                           ------------
! NWIH      number of words in trace header       used but not changed
! NDPT      number of sample values in trace      used but not changed in trscan
! TSTRT     starting time on trace                used but not changed in trscan
! DT        trace sample interval                 used but not changed in trscan
!
!-------------------------------------------------------------------------------
!</global_doc>
!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! all     all header words           HEADSUM summarizes all header words
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 23. 2010-01-08  Stoeckley    Added a parameter to specify the number of header
!                              words to summarize, with a default of 64.  Needed
!                              because this module aborts in GeoPRO because
!                              a block of GeoPRO headers is added to the header
!                              word array.  This block contains formats that
!                              are incompatible with double precision numbers.
! 22. 2009-01-27  Bill Menger  Fixed an old bug in headsumstats that is compiler
!                              dependent.
! 21. 2006-06-20  B. Menger    Removed Unused Variables.
! 20. 2004-01-07  Goodger      Change use module only to use module.  Remove
!                              renaming of no_more_traces to satisfy the intel
!                              compiler.
! 19. 2001-10-11  SMCook       Minor gui_def change - set xStretch and yStretch
!                               false to correct appearance after CFE changes.
! 18. 2001-08-27  Selzler      Increase I2 format to I5 for hdr word number.
! 17. 2001-02-15  Brad Kruse   Change name for wrapup flag to SKIP_WRAPUP for
!                              clarity, and slightly change how it is used.
! 16. 2000-08-29  Brad Kruse   Correct memory leak, due to calling trscan_setup
!                              for GUI updates.  Reported by Donna Vunderink
!                              while debugging CFE memory leaks.
! 15. 2000-05-17  Brad Kruse   Investigate reported problem of multiple
!                              reports, when no traces have been processed
!                              before ntr goes to NO_MORE_TRACES.
!                              Added setting of control parameters and 
!                              obj%wrapped_up.
! 14. 2000-04-05  Kruse        Insert screen layout.
! 13. 2000-02-21  Kruse        Spelling error in Advice DOC
! 12. 2000-02-02  Kruse        Made options for HIST_LAV logical, to 
!.                             correct mishandling of combo box mixed-case
!.                             logical values.  Change to use TRSCAN_SETUP 
!.                             argument to request a histogram, rather than
!.                             JOB DATA parameters.
! 11. 2000-01-31  Kruse        Replace cps_trscan call with call to 
!.                             trscan_print.  Correct uninitialized arrays.
! 10  1999-12-30  Dorman       Get unit from parameter cache.  Remove old tags.
!  9. 1999-09-15  Dorman       Converted to revised CPS system and Fortran 90
!  8. 1998-11-18  Vunderink    Fixed bug in absolute minimum and maximum
!.                             increment logic.
!  7. 1998-11-13  Vunderink    Begin using the F90 compiler.
!  6. 1998-09-30  Vunderink    Added absolute minimum and maximum increment.
!  5. 1996-06-26  Goodger      Put variable NN in a save statement.
!  4. 1994-02-11  Troutt       Add error check for HPALLOC calls.
!  3. 1993-11-09  Troutt       Added parameter LAVS (YES,NO) which uses
!.                             TRSCAN to give histogram of LAVs.
!  2. 1992-10-01  Troutt       Added routine HEADSUMR and entry HEADSUMI
!.                             as copies of NUM2AFLR and NUM2ALFI with # of
!.                             significant digits increased from 7 to 8.
!  1. 1992-09-28  Troutt       Original version.
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
! Note that extrema calculation includes non-zero values only.
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
!<NS HEADSUM Process/NC=80/NR=12>
!
! Summarize range of all header word values.
!
! HDR_FLAG=`IIIIII     NUM_HEADERS_TO_SUMMARIZE=`IIIII
!
! Include an LAV histogram in the report: 
!             [HIST_LAV/XSF/YSF]`CC
!
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 1 - NWIH
! 
! If HDR_FLAG = 0, then all traces are summarized.  Otherwise, only traces with
! a flag set in header word HDR_FLAG are summarized.
! 
!</Help>
!
!<Help KEYWORD="NUM_HEADERS_TO_SUMMARIZE">
!<Tip> Number of header words to summarize. </Tip>
! Default = 64
! Allowed = 1 - NWIH
! 
! This parameter is needed because this module aborts in GeoPRO since
! a block of GeoPRO headers is added to the header word array.  This block
! contains formats that are incompatible with double precision numbers.
! Therefore no more than 64 header words should be summarized in GeoPRO.
!</Help>
!
!<Help KEYWORD="HIST_LAV">
!<Tip> Option whether to generate a histogram of trace LAVs. </Tip>
! Default = YES
! Allowed = YES/NO
! If HIST_LAV = NO then no distribution function (histogram) is generated, if
! HIST_LAV = YES then a histogram is generated for all traces accepted by the
! header flag logic.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module headsum_module
  use pc_module
  !
  use named_constants_module

  !
  use string_module
  !
  use trscan_module
  !
  implicit none
  private
  public :: headsum_create     ! uses the parameter cache.
  public :: headsum_initialize
  public :: headsum_update     ! uses the parameter cache.
  public :: headsum_delete
!<execute_only>
  public :: headsum            ! main execution (trace processing) routine.
  public :: headsum_wrapup
!</execute_only>


  character(len=100), public, save :: HEADSUM_IDENT = &
       '$Id: headsum.f90,v 1.21 2006/06/20 13:11:54 Menger prod sps $'

  !!------------------------- parameter structure --------------------------!!
  !!------------------------- parameter structure --------------------------!!
  !!------------------------- parameter structure --------------------------!!


  type, public :: headsum_struct

    private
    logical                   :: skip_wrapup       ! wrapup flag.
    integer                   :: hdr_flag
    integer                   :: num_headers_to_summarize
    logical                   :: hdr_flag_is_set
    logical                   :: hist_lav
    integer                   :: ntin
    integer                   :: ntfl
    double precision, pointer :: hmin    (:)
    double precision, pointer :: hmax    (:)
    double precision, pointer :: hdsav   (:)
    double precision, pointer :: hmininc (:)
    double precision, pointer :: hmaxinc (:)
    integer,          pointer :: nzro    (:)
    integer,          pointer :: nsav    (:)
    integer                   :: nwih
    integer                   :: lun
    integer                   :: ipn
    integer                   :: ndpt

  end type headsum_struct

  !!-------------------------------- data ----------------------------------!!
  !!-------------------------------- data ----------------------------------!!
  !!-------------------------------- data ----------------------------------!!

  type (headsum_struct), pointer, save :: object      ! needed for traps.
  !
  character (len = *), parameter :: process_name = 'HEADSUM'
  integer,             parameter :: local_when   = 1
  integer,             parameter :: single_trace = 1
  !
  contains

  !!------------------------------- create ---------------------------------!!
  !!------------------------------- create ---------------------------------!!
  !!------------------------------- create ---------------------------------!!

    subroutine headsum_create (obj)
      !
      type(headsum_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify  (obj%hmin)     ! nullify all pointers in headsum_struct
      nullify  (obj%hmax)     ! nullify all pointers in headsum_struct
      nullify  (obj%hdsav)    ! nullify all pointers in headsum_struct
      nullify  (obj%hmininc)  ! nullify all pointers in headsum_struct
      nullify  (obj%hmaxinc)  ! nullify all pointers in headsum_struct
      nullify  (obj%nzro)     ! nullify all pointers in headsum_struct
      nullify  (obj%nsav)     ! nullify all pointers in headsum_struct


      call headsum_initialize (obj)


      return
    end subroutine headsum_create


  !!------------------------------- delete ---------------------------------!!
  !!------------------------------- delete ---------------------------------!!
  !!------------------------------- delete ---------------------------------!!

    subroutine headsum_delete (obj)
      !
      type(headsum_struct),pointer :: obj       ! arguments

!<execute_only>
      call headsum_wrapup (obj)
!</execute_only>

      if (associated(obj%hmin))    deallocate(obj%hmin)
      if (associated(obj%hmax))    deallocate(obj%hmax)
      if (associated(obj%hdsav))   deallocate(obj%hdsav)
      if (associated(obj%hmininc)) deallocate(obj%hmininc)
      if (associated(obj%hmaxinc)) deallocate(obj%hmaxinc)
      if (associated(obj%nzro))    deallocate(obj%nzro)
      if (associated(obj%nsav))    deallocate(obj%nsav)

      deallocate (obj)

      return
    end subroutine headsum_delete


  !!----------------------------- initialize -------------------------------!!
  !!----------------------------- initialize -------------------------------!!
  !!----------------------------- initialize -------------------------------!!

  subroutine headsum_initialize (obj)
    !
    ! - Arguments
    !
    type(headsum_struct),pointer :: obj       ! arguments
    !
    ! - Begin headsum_initialize
    !
    obj%hdr_flag        = 0             ! default hdr_flag to zero
    obj%num_headers_to_summarize = 64
    obj%hdr_flag_is_set = .false.
    obj%hist_lav        = .true.        ! default hist_lav to 'yes'
    obj%ntin            = 0
    obj%ntfl            = 0
    obj%nwih            = HDR_BOTTOM_MUTE
    obj%lun             = 6
    obj%ipn             = 4
    obj%ndpt            = 0
    !
    call headsum_update (obj)
    !
  end subroutine headsum_initialize


  !!-------------------------- start of update -----------------------------!!
  !!-------------------------- start of update -----------------------------!!
  !!-------------------------- start of update -----------------------------!!


  subroutine headsum_update (obj)
    !
    ! - Arguments
    !
    type (headsum_struct), target :: obj                           ! arguments
    !
    ! - Local variables
    !
    integer :: ierr


    real    :: dt
    real    :: tstrt
    !
    ! - Begin headsum_update
    !
    object          => obj         ! needed for traps.
    obj%skip_wrapup = .true.       ! needed for the wrapup routine.


    !!------------------------ read parameters -----------------------------!!
    !!------------------------ read parameters -----------------------------!!
    !!------------------------ read parameters -----------------------------!!

    obj%ipn = pc_get_ipn()
    obj%lun = pc_get_lun()                    ! get logical unit number
    !
    call pc_get_global ('NWIH'  , obj%nwih)   ! if needed.
    call pc_get ('HIST_LAV', obj%hist_lav)
    call pc_get ('HDR_FLAG', obj%hdr_flag, headsum_hdr_flag_trap)
    call pc_get ('num_headers_to_summarize', obj%num_headers_to_summarize)
    !
    ! - Check these values for trscan
    !
    dt    = 0.0
    tstrt = 0.0
    !
    call pc_get_global ('ndpt',  obj%ndpt)  ! number of trace samples.
    call pc_get_global ('dt',    dt)    ! trace sample interval (sec).
    call pc_get_global ('tstrt', tstrt) !time of 1st trace sample(sec).


    !!----------------------- verify parameters ----------------------------!!
    !!----------------------- verify parameters ----------------------------!!
    !!----------------------- verify parameters ----------------------------!!

    if (obj%nwih < HDR_LAV) then
      call pc_error(msg1 = "Global NWIH must be greater than HDR_LAV(",     &
                    var1 = HDR_LAV,                                         &
                    msg2 = "): ",                                           &
                    var2 = obj%nwih)
      obj%nwih = HDR_LAV
    end if
    !
    if (obj%ndpt <= 0) then
      call pc_error(msg1 = "Global NDPT must be greater than zero (0): ",   &
                    var1 = obj%ndpt)
    end if
    !
    if (dt <= 0.0) then
      call pc_error(msg1 = "Global DT must be greater than zero (0.0): ",   &
                    var1 = dt)
    end if
    !
    if (obj%hdr_flag < 0) then
       call pc_error(msg1 = "HDR_FLAG is cannot be less than zero (0): ",   &
                     var1 = obj%hdr_flag)
    else if (obj%hdr_flag > obj%nwih) then
       call pc_error(msg1 = "HDR_FLAG is cannot be greater than NWIH(",     &
                     var1 = obj%nwih,                                       &
                     msg2 = "): ",                                          &
                     var2 = obj%hdr_flag)
    end if
    obj%hdr_flag_is_set = obj%hdr_flag > 0

    if(object%num_headers_to_summarize < 1 ) then
      call pc_warning('NUM_HEADERS_TO_SUMMARIZE must be greater than or equal to one.')
      object%num_headers_to_summarize = 1
    elseif(object%num_headers_to_summarize > object%nwih) then
      call pc_warning('NUM_HEADERS_TO_SUMMARIZE must be less than or equal to NWIH.')
      object%num_headers_to_summarize = object%nwih
    elseif(object%num_headers_to_summarize > 64) then
      call pc_warning('NUM_HEADERS_TO_SUMMARIZE should be less than or equal to 64 for GeoPRO.')
    end if

    !!------------------------ write parameters ----------------------------!!
    !!------------------------ write parameters ----------------------------!!
    !!------------------------ write parameters ----------------------------!!

    call pc_put_global (keyword  = 'NWIH',     scalar = obj%nwih)
    call pc_put        (keyword  = 'HDR_FLAG', scalar = obj%hdr_flag)
    call pc_put ('num_headers_to_summarize', obj%num_headers_to_summarize)
    !
    call pc_put_control ('ntapes',       0)
    call pc_put_control ('need_request', .false.)
    call pc_put_control ('need_label',   .false.)
    call pc_put_control ('twosets',      .false.)
    call pc_put_control ('nscratch',     0)
    call pc_put_control ('nstore',       obj%nwih * 12)
    call pc_put_control ('iftd',         .false.)
    call pc_put_control ('ndisk',        0)
    call pc_put_control ('setup_only',   .false.)
    !
    call pc_put_options_field (keyword  = 'HIST_LAV',            &
                               options  = (/ .true., .false. /),    &
                               noptions = 2)
    !
    call pc_put        (keyword  = 'HIST_LAV', scalar = obj%hist_lav)


    !!--------------------- prepare for execution --------------------------!!
    !!--------------------- prepare for execution --------------------------!!
    !!--------------------- prepare for execution --------------------------!!

    if (associated(obj%hmin))    deallocate(obj%hmin)
    if (associated(obj%hmax))    deallocate(obj%hmax)
    if (associated(obj%hdsav))   deallocate(obj%hdsav)
    if (associated(obj%hmininc)) deallocate(obj%hmininc)
    if (associated(obj%hmaxinc)) deallocate(obj%hmaxinc)
    if (associated(obj%nzro))    deallocate(obj%nzro)
    if (associated(obj%nsav))    deallocate(obj%nsav)

!<execute_only>

    if (pc_do_not_process_traces()) return
    !
    obj%skip_wrapup = .false.
    !
    allocate(obj%hmin (obj%nwih), stat=ierr)
    if (ierr /= 0) then
      call pc_error('Error in allocation of HMIN.')
    else
      obj%hmin = 0.0d0
    end if
    !
    allocate(obj%hmax (obj%nwih), stat=ierr)
    if(ierr /= 0) then
      call pc_error('Error in allocation of HMAX.')
    else
      obj%hmax = 0.0d0
    end if
    !
    allocate(obj%hdsav (obj%nwih), stat=ierr)
    if(ierr /= 0) then
      call pc_error('Error in allocation of HDSAV.')
    else
      obj%hdsav = 0.0d0
    end if
    !
    allocate(obj%hmininc (obj%nwih), stat=ierr)
    if(ierr /= 0) then
      call pc_error('Error in allocation of HMININC.')
    else
      obj%hmininc = 0.0d0
    end if
    !
    allocate(obj%hmaxinc (obj%nwih), stat=ierr)
    if(ierr /= 0) then
      call pc_error('Error in allocation of HMAXINC.')
    else
      obj%hmaxinc = 0.0d0
    end if
    !
    allocate(obj%nzro (obj%nwih), stat=ierr)
    if(ierr /= 0) then
      call pc_error('Error in allocation of NZRO.')
    else
      obj%nzro = 0
    end if
    !
    allocate(obj%nsav (obj%nwih), stat=ierr)
    if(ierr /= 0) then
      call pc_error('Error in allocation of NSAV.')
    else
      obj%nsav = 0
    end if
    !
    call trscan_setup (ipn       = obj%ipn,       &
                       iwhen     = local_when,    &
                       histo_req = obj%hist_lav)

!</execute_only>


    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!

    !
  end subroutine headsum_update

  !!------------------------------- traps ----------------------------------!!
  !!------------------------------- traps ----------------------------------!!
  !!------------------------------- traps ----------------------------------!!

  subroutine headsum_hdr_flag_trap (keyword)
    !
    ! - Arguments
    !
    character(len=*), intent(in) :: keyword
    !
    ! - Begin headsum_hdr_flag_trap
    !
    if(object%hdr_flag < 0 ) then
      call pc_error('HDR_FLAG must be greater than or equal to zero.')
      object%hdr_flag = 0
    elseif(object%hdr_flag > object%nwih) then
      call pc_error('HDR_FLAG must be less than or equal to NWIH.')
      object%hdr_flag = object%nwih
    end if
    !
  end subroutine headsum_hdr_flag_trap


  !!--------------------------- main execution -----------------------------!!
  !!--------------------------- main execution -----------------------------!!
  !!--------------------------- main execution -----------------------------!!

!<execute_only>

  subroutine headsum (obj, ntr, hd, tr)
    !
    ! - Arguments
    !
    type (headsum_struct), intent (inout) :: obj
    integer,               intent (inout) :: ntr
    double precision,      intent (inout) :: hd (:, :)
    real,                  intent (inout) :: tr (:, :)
    !
    ! - Begin headsum
    !
    if (ntr == NEED_TRACES) return
    if (ntr .gt. 0) then
      call headsum_stats (obj, ntr, hd, tr)
    elseif(ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR ) then
      call headsum_report (obj)
      call headsum_wrapup (obj)
    end if
    !
  end subroutine headsum

  !!--------------------------- headsum_stats ------------------------------!!
  !!--------------------------- headsum_stats ------------------------------!!
  !!--------------------------- headsum_stats ------------------------------!!

  subroutine headsum_stats (obj, ntr, hd, tr)
    !
    ! - Arguments
    !
    type (headsum_struct), intent (inout) :: obj
    integer,               intent (inout) :: ntr
    double precision,      intent (inout) :: hd (:, :)
    real,                  intent (inout) :: tr (:, :)
    !
    ! - Local variables
    !
    double precision     :: hdtmp

    integer              :: ih

    integer              :: nn
    !
    ! - Begin headsum_stats
    !
  loop_thru_traces:    &
    do nn = 1,ntr
      !
      obj%ntin = obj%ntin + 1          ! count the input traces
      ! wmm-2009/jan/27 added outer test because intel fortran resolves the
      !                 hd(obj%hdr_flag) before testing if hdr_flag_is_set
      !                 and if hdr_flag==0 this is not valid address.
      if(obj%nwih >= obj%hdr_flag .and. obj%hdr_flag > 0 ) then
        if (obj%hdr_flag_is_set .and. hd (obj%hdr_flag, nn) == 0.0) then
          cycle loop_thru_traces
        end if
      endif
      !
      obj%ntfl = obj%ntfl + 1        ! count number of traces flagged
      !
      if (obj%hist_lav) then
        ! call cps_trscan(4, ntr, hd(:,nn:nn),tr(:,nn:nn))
        call trscan_print (ipn    = obj%ipn,         &
                           n      = single_trace,    &
                           hd     = hd (:, nn:nn),   &
                           tr     = tr (:, nn:nn),   &
                           prname = process_name,    &
                           iwhen  = local_when)
      end if
      !
    loop_thru_hwds:    &
      do ih = 1,obj%num_headers_to_summarize
        !
        if (hd(ih,nn) == 0.0d0) then
          obj%nzro(ih) = obj%nzro(ih) + 1
          cycle loop_thru_hwds
        end if
        !
        ! - Find MAX
        !
        if (obj%hmax(ih) == 0.0d0) then
          obj%hmax(ih) = hd(ih,nn)
        else
          obj%hmax(ih) = max(obj%hmax(ih),hd(ih,nn))
        end if
        !
        ! - Find MIN
        !
        if (obj%hmin(ih) == 0.0d0) then
          obj%hmin(ih) = hd(ih,nn)
        else
          obj%hmin(ih) = min(obj%hmin(ih),hd(ih,nn))
        end if
        !
        if (obj%nsav(ih) > 0) then
          !
          hdtmp = abs (hd (ih, nn) - obj%hdsav (ih))
          !
          if (hdtmp /= 0.0d0) then
            !
            if (obj%hmaxinc(ih) == 0.0d0) then
              obj%hmaxinc(ih) = hdtmp
            else
              obj%hmaxinc(ih) = max(obj%hmaxinc(ih),hdtmp)
            end if
            !
            if (obj%hmininc(ih) == 0.0d0) then
              obj%hmininc(ih) = hdtmp
            else
              obj%hmininc(ih) = min(obj%hmininc(ih),hdtmp)
            end if
            !
          end if
        end if
        !
        obj%hdsav (ih) = hd (ih,nn)
        obj%nsav (ih)  = obj%nsav (ih) + 1
        !
      end do loop_thru_hwds
      !
    end do loop_thru_traces
    !
  end subroutine headsum_stats


  !!--------------------------- headsum_report -----------------------------!!
  !!--------------------------- headsum_report -----------------------------!!
  !!--------------------------- headsum_report -----------------------------!!

  subroutine headsum_report (obj)
    !
    ! - Arguments
    !
    type (headsum_struct), intent (inout) :: obj
    !
    ! - Local variables
    !
    character (len =  8) :: czro
    character (len = 16) :: cmax
    character (len = 16) :: cmaxinc
    character (len = 16) :: cmin
    character (len = 16) :: cmininc
    character (len = 16) :: val

    integer              :: i


    double precision     :: hd (obj%nwih, 1)
    real                 :: tr (obj%ndpt, 1)
    !
    character (len = 16), parameter :: long_field = ' _______________'
    character (len = 16), parameter :: error_field = 'error __________'
    !
    ! - Begin headsum_report
    !
    write(unit=obj%lun,fmt=1001)
  1001 format('1','Header Value Summary')
    write(unit=obj%lun,fmt=*)' '
    write(unit=obj%lun,fmt=*)obj%ntfl,' traces summarized out of ',&
                             obj%ntin,' input.'
    write(unit=obj%lun,fmt=*)' '
    write(unit=obj%lun,fmt=*)' '
    write(unit=obj%lun,fmt=*)'   HD#  Min-Value         Max-Value         &
                               &#-Zeros   Abs Min-Inc       Abs Max-Inc'
    write(unit=obj%lun,fmt=*)'------  ---------         ---------         &
                             &-------   -----------       -----------'
    !
    do i = 1, obj%num_headers_to_summarize
      !
      call string_dd2cc (obj%hmin(i), cmin)
      !
      if (cmin == ' ') then
        if (obj%nzro(i) /= obj%ntfl) then
          cmin = error_field
        else
          cmin = long_field
        end if
      else
        cmin = trim(cmin)//long_field
      end if
      !
      call string_dd2cc (obj%hmax(i),cmax)
      !
      if (cmax == ' ') then
        if (obj%nzro(i) /= obj%ntfl) then
          cmax = error_field
        else
          cmax = long_field
        end if
      else
        cmax = trim(cmax)//long_field
      end if
      !
      call string_ii2cc (obj%nzro(i),czro)
        if (czro == ' ') czro = '********'
      !
      call string_dd2cc (obj%hmininc(i), val)
        cmininc = trim(val)//long_field
      !
      call string_dd2cc (obj%hmaxinc(i), val)
        cmaxinc = trim(val)//long_field
      !
      write(unit=obj%lun,fmt=1002) i, cmin, cmax, czro, cmininc, cmaxinc
    1002 format(2x,i5,2x,a,2x,a,2x,a,2x,a,2x,a)

!!-----------------------------------------------------------------------------
!!                         FOR DEBUGGING
!!-----------------------------------------------------------------------------
!       write(unit=obj%lun,fmt=1003) i, obj%hmin(i), obj%hmax(i), obj%nzro(i),&
!                              & obj%hmininc(i), obj%hmaxinc(i)
!  1003 format(2x,i5,2x,f16.4,2x,f16.4,2x,i8,2x,f16.4,2x,f16.4)
!!-----------------------------------------------------------------------------
!!                         FOR DEBUGGING
!!-----------------------------------------------------------------------------

      !
    end do
    !
    if (obj%hist_lav) then
      !
      hd = 0.0d0
      tr = 0.0
      !
      call trscan_print (ipn    = obj%ipn,            &
                         n      = no_more_traces,    &
                         hd     = hd,                 &
                         tr     = tr,                 &
                         prname = process_name,       &
                         iwhen  = local_when)
      !
    end if
    !
  end subroutine headsum_report

!</execute_only>


  !!-------------------------------- wrapup --------------------------------!!
  !!-------------------------------- wrapup --------------------------------!!
  !!-------------------------------- wrapup --------------------------------!!

!<execute_only>

  subroutine headsum_wrapup (obj)
    !
    ! - Arguments
    !
    type(headsum_struct) :: obj       ! arguments
    !
    ! - Begin headsum_wrapup
    !
    if (obj%skip_wrapup) return
    obj%skip_wrapup = .true.
    !
  end subroutine headsum_wrapup

!</execute_only>


  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!


 end module headsum_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
