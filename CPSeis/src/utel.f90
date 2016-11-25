!<CPS_v1 type="PROCESS"/>
!!------------------------------- utel.f90 ---------------------------------!!
!!------------------------------- utel.f90 ---------------------------------!!
!!------------------------------- utel.f90 ---------------------------------!!

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
!------------------------------------------------------------------------------
!                         C P S   P R O C E S S
!
! Name       : UTEL      (you tell it what to do)
! Category   : miscellaneous
! Written    : 2000-04-11   by: Brad Kruse
! Revised    : 2007-01-30   by: Bill Menger
! Maturity   : beta
! Purpose    : Perform miscellaneous trace-oriented tasks.
! Portability: No known limitations.
! Parallel   : no
!
!------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! UTEL is a general purpose process for performing miscellaneous trace-oriented
! tasks.  Only one choice of the MODE parameter is permitted in each instance
! of the UTEL process.
!
! Options
!
! If MODE = DUPLICATE, then UTEL will make NUM_TR copies of each input trace
! flagged with header word HDR_FLAG and insert them in the trace flow after the
! flagged trace.  Each flagged trace is replaced by (NUM_TR + 1) copies of the
! flagged trace.
!
! If MODE = INSERT_DEAD, then UTEL will insert NUM_TR dead traces after each
! input trace flagged with header word HDR_FLAG.
!
! If MODE = CHANGE_DT, then UTEL will change the global sample rate (thereby
! affecting processes that follow).  This functionality is provided for
! presumably rare cases where unit conversion is required, such as changing
! feet to meters with depth data, or dividing by 1000 to load depth data to
! Landmark.
! In CHANGE_DT mode, no operations are performed on traces, only the sample 
! rate of the subsequent processing stream is modified.
!
! [More options will undoubtedly be added in the future.]
!------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
!
!------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
!
! This process does not alter input traces, but may add duplicate or dead
! traces.  Header words are modified for resequencing, etc. if necessary.
!
!------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
!
! Name       Description                           Action taken
! ----       -----------                           ------------
! NUMTR      Max number of traces per call         no action
! GATHERED   Whether input traces are gathered     no action
! NWIH       Number of words in trace header       Used but not changed
! NDPT       Number of sample values in trace      Used but not changed
!
!------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 1       Sequential Trace Count     Renumbered.
! 3       Current gather             Referenced.
! 4       Current channel            Renumbered.
! 25      Largest absolute value     Reset to 0 when inserting dead traces.
!         HDR_FLAG                   Flagword.
!
! Headers of the inserted duplicated or dead traces are identical to those
! of the flagged trace after which they are inserted, EXCEPT that header words
! 1 and 4 are renumbered, header word 25 is set to 0 for inserted dead traces,
! and the flag in header word HDR_FLAG is cleared from all inserted traces
! (The flag remains set in only those traces which were ORIGINALLY flagged).
!
!------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  8. 2007-01-30  Bill Menger  Modified to pass through unflagged traces also.
!  7. 2007-01-30  Bill Menger  Fixed the flag operation so that utel will only 
!                              operate on flagged traces unless hdr_flag == 0, 
!                              in which case it operates on all traces.
!  6. 2005-01-17  Bill Menger  Modified to operate in setup-only for change_dt
!                              and made process multi-trace capable.
!  5. 2004-01-07  SMCook       Added CHANGE_DT option.
!  4. 2002-04-25  Bob Baumel   Fix front end to enforce single-trace input;
!                              modify screen to display keyword names; clear
!                              flag from the duplicated or inserted dead
!                              traces; several other bug fixes.
!  3. 2001-06-11  Brad Kruse   Change name for wrapup flag to SKIP_WRAPUP for
!                              clarity, and slightly change how it is used.
!  2. 2000-04-11  Brad Kruse   Initial Code.
!  1. 1999-11-18  C I Burch    Original design.
!
!------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
!
!------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST    true     whether this process ever needs to request traces.
! NEED_LABEL      true     whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE         varies    amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     true      if change_dt mode is selected.      
! SETUP_ONLY     false     if other modes are selected.
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
!
! Upon input, NTR must have one of these values:
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces.
!  NTR >= 1              means to process the input traces
!  NTR == anything else  will cause utel to call wrapup and return
!
! Upon output, NTR will have one of these values:
!  NTR == NO_MORE_TRACES    if there are no more traces to output.
!  NTR == NEED_TRACES       if this was passed in. (no action taken here)
!  NTR == ntr_in*(1+num_tr) at least NTR_IN comes out, more if dups requested.
!  NTR == FATAL_ERROR       if this was passed in. (no action taken here but
!                           wrapup called)
!
!------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!------------------------------------------------------------------------------
!                    ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! UTEL uses the flagword logic to take advantage of the several options of
! trace selection in SELECT without duplicating that code.
!
!------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!
!------------------------------------------------------------------------------
!</programming_doc>

!------------------------------------------------------------------------------
!<gui_def>
!<NS UTEL Process/NC=80>
!
!                        yoU TELL it what to do Process
!                   Perform miscellaneous trace-oriented tasks
!
!    ADDED THIS TO THE GUI
!
!
!                   MODE~~~~=`CCCCCCCCCC 
!
!                   HDR_FLAG=`III    (hdr word with flag value(nonzero) in it.)
!
!                   NUM_TR~~=`IIIIII (How many dup traces to add.)
!
!                   NEW_DT~~=`FFFFFF (change sample rate-- no trace action.)
!
!</gui_def>
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 48
! Allowed = 0 - NWIH (number of words in header)
! If HDR_FLAG = 0, then UTEL behaves as if EVERY input trace is flagged; i.e.,
! duplicated or dead traces are inserted after every input trace.
! Otherwise, duplicated or dead traces are inserted after only those traces
! with a flag set (non-zero value) in header word HDR_FLAG.  Other traces are
! passed through.
! Try using SELECT process prior to this one in order to FLAG your traces based
! on some criteria.
!
!</Help>
!<Help KEYWORD="MODE">
!<Tip> Option of task that UTEL is to perform. </Tip>
! Default = DUPLICATE
! Allowed = DUPLICATE, INSERT_DEAD, CHANGE_DT
! If MODE = DUPLICATE, then UTEL will make NUM_TR copies of each input trace
! flagged with header word HDR_FLAG and insert them in the trace flow after the
! flagged trace.  Thus, each flagged trace is replaced by (NUM_TR + 1) copies
! of the flagged trace.
!
! If MODE = INSERT_DEAD, then UTEL will insert NUM_TR dead traces after each
! input trace flagged with header word HDR_FLAG.
!
! If MODE = CHANGE_DT, UTEL will change the global sample rate.
!
!</Help>
!
!<Help KEYWORD="NUM_TR">
!<Tip> Number of Duplicated or Dead traces to insert at a time. </Tip>
! Default = 1
! Allowed = int > 0
! NUM_TR is the number of copies to insert after each flagged trace (MODE =
! DUPLICATE) or the number of dead traces to insert after each flagged
! trace (MODE = INSERT_DEAD).
!</Help>
!
!<Help KEYWORD="NEW_DT">
!<Tip> New global DT (sample rate) (in milliseconds, typically). </Tip>
! Default = .004
! Allowed = real > 0
! NEW_DT is used if and only if MODE = CHANGE_DT.  The global sample rate
! is changed for subsequent processing.
!</Help>
!
!</HelpSection>
!------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module utel_module
  !
  ! - Module references
  !
  use pc_module
  use named_constants_module
  use mem_module
  !
  implicit none
  private
  !
  public :: utel_create
  public :: utel_initialize
  public :: utel_update
  public :: utel_delete

!<execute_only>
  public :: utel            ! main execution (trace processing) routine.
  public :: utel_wrapup
!</execute_only>

  !
  character(len=100),public,save :: UTEL_IDENT = &
'$Id: utel.f90,v 1.8 2007/01/31 13:24:19 Menger beta sps $'


  !!------------------------------- data -----------------------------------!!
  !!------------------------------- data -----------------------------------!!
  !!------------------------------- data -----------------------------------!!

  integer,                    parameter :: mode_len = 11 
                                           ! - Length of an option name
  integer,                    parameter :: mode_noptions = 3
  character (len = mode_len), parameter :: mode_options (mode_noptions)    &
    = (/ 'DUPLICATE  ', &
         'INSERT_DEAD', &
         'CHANGE_DT  ' /)

  !!---------------------- parameter structure -----------------------------!!
  !!---------------------- parameter structure -----------------------------!!
  !!---------------------- parameter structure -----------------------------!!
  type, public :: utel_struct
    !
    private
    logical                    :: skip_wrapup      ! wrapup flag.
    !
    integer                    :: hdr_flag         ! process parameter
    character (len = mode_len) :: mode             ! process parameter
    integer                    :: num_tr           ! process parameter
    !
    integer                    :: nwih             ! global
    integer                    :: ndpt             ! global
    integer                    :: numtr_in         ! incoming numtr
    real                       :: dt_in            ! incoming dt
    !
    integer                    :: seq_hw4          ! dependent variable
    integer                    :: seq_hw1          ! dependent variable
    double precision           :: prev_hw3         ! dependent variable
    real                       :: new_dt           ! dependent variable
    !
  end type utel_struct

  type(utel_struct),pointer,save :: object      ! needed for traps.

  !!---------------------------- interfaces -------------------------------!!
  !!---------------------------- interfaces -------------------------------!!
  !!---------------------------- interfaces -------------------------------!!

contains

  !!----------------------------- create -----------------------------------!!
  !!----------------------------- create -----------------------------------!!
  !!----------------------------- create -----------------------------------!!

  subroutine utel_create (obj)
    type(utel_struct),pointer :: obj       ! arguments
    allocate (obj)
    call utel_initialize (obj)
  end subroutine utel_create


  !!------------------------------- delete ---------------------------------!!
  !!------------------------------- delete ---------------------------------!!
  !!------------------------------- delete ---------------------------------!!

  subroutine utel_delete (obj)
    type (utel_struct), pointer :: obj       ! arguments
!<execute_only>
    call utel_wrapup (obj)
!</execute_only>
    deallocate(obj)
  end subroutine utel_delete


  !!----------------------------- initialize -------------------------------!!
  !!----------------------------- initialize -------------------------------!!
  !!----------------------------- initialize -------------------------------!!

  subroutine utel_initialize (obj)
    type(utel_struct),intent(inout) :: obj       ! arguments
    obj%skip_wrapup = .true.
    obj%hdr_flag    = 48
    obj%mode        = 'DUPLICATE'
    obj%num_tr      = 1
    obj%new_dt      = .004
    !
    call pc_get_global ('NUMTR', obj%numtr_in)    ! number of traces per call.
    call pc_get_global ('NWIH',  obj%nwih)        ! number of header words.
    call pc_get_global ('NDPT',  obj%ndpt)        ! number of trace samples.
    call pc_get_global ('DT',    obj%dt_in)       ! global sample rate on input.
    call utel_update (obj)
    !
  end subroutine utel_initialize


  !!------------------------- start of update ------------------------------!!
  !!------------------------- start of update ------------------------------!!
  !!------------------------- start of update ------------------------------!!

  subroutine utel_update (obj)
    type(utel_struct),intent(inout),target :: obj             ! arguments
    object          => obj      ! needed for traps.
    obj%skip_wrapup = .true.    ! needed for the wrapup routine.

    !!-------------------------- read parameters ---------------------------!!
    !!-------------------------- read parameters ---------------------------!!
    !!-------------------------- read parameters ---------------------------!!
    call pc_get ('HDR_FLAG', obj%hdr_flag)
    call pc_get ('MODE',     obj%mode)
    call pc_get ('NUM_TR',   obj%num_tr)
    call pc_get ('NEW_DT',   obj%new_dt)
    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!

    obj%hdr_flag = min (max(obj%hdr_flag, 0), obj%nwih)

    if      (obj%mode(1:1)=='D' .or. obj%mode(1:1)=='d') then
      obj%mode = 'DUPLICATE'
    else if (obj%mode(1:1)=='I' .or. obj%mode(1:1)=='i') then
      obj%mode = 'INSERT_DEAD'
    else if (obj%mode(1:1)=='C' .or. obj%mode(1:1)=='c') then
      obj%mode = 'CHANGE_DT'
      if(obj%new_dt <= 0 ) then
        call pc_error ('NEW_DT must be positive.')
        return
      end if
    else
      call pc_error ('MODE must be DUPLICATE, INSERT_DEAD, or CHANGE_DT.')
    end if



    !!--------------------- call processes internally ----------------------!!
    !!--------------------- call processes internally ----------------------!!
    !!--------------------- call processes internally ----------------------!!


    !!-------------------------- write parameters --------------------------!!
    !!-------------------------- write parameters --------------------------!!
    !!-------------------------- write parameters --------------------------!!


    call pc_put_options_field ('MODE', mode_options, mode_noptions)

    call pc_put ('HDR_FLAG', obj%hdr_flag)
    call pc_put ('MODE',     obj%mode)
    call pc_put ('NUM_TR',   obj%num_tr)
    call pc_put ('NEW_DT',   obj%new_dt)

    if(obj%mode == 'CHANGE_DT') then
      call pc_put_global ('DT', obj%new_dt)
      !--- reset numtr to value at initialize
      call pc_put_global ('NUMTR', obj%numtr_in) 
      call pc_put_sensitive_field_flag('NEW_DT',   .true.)
      call pc_put_sensitive_field_flag('HDR_FLAG', .false.)
      call pc_put_sensitive_field_flag('NUM_TR',   .false.)
      ! - Set controls
      call pc_put_control ('setup_only', .true.)
    else
      call pc_put_sensitive_field_flag('HDR_FLAG', .true.)
      call pc_put_sensitive_field_flag('NEW_DT',   .false.)
      call pc_put_sensitive_field_flag('NUM_TR',   .true.)
      !--- reset dt to value at initialize
      call pc_put_global ('DT', obj%dt_in)
      obj%num_tr = abs(obj%num_tr)
      if (obj%num_tr <= 0) then
        call pc_error ('NUM_TR must be positive.')
      end if
      call pc_put_global ('NUMTR', obj%numtr_in*(1+obj%num_tr)) 
      ! number of traces per call coming out.
      ! - Set controls
      call pc_put_control ('setup_only', .false.)
    endif

    ! - Set controls
    call pc_put_control ('need_request', .false.)
    call pc_put_control ('need_label',   .false.)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!<execute_only>

    if (pc_do_not_process_traces()) return
    !
    obj%skip_wrapup = .false.
    !
    if (pc_do_not_process_traces()) return   ! in case of allocation errors.
    !
    ! - initialize counters
    !
    obj%seq_hw4   = 0
    obj%seq_hw1   = 0
    obj%prev_hw3  = -huge(0.0)

!</execute_only>


    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!

  end subroutine utel_update


  !!------------------------------- traps ----------------------------------!!
  !!------------------------------- traps ----------------------------------!!
  !!------------------------------- traps ----------------------------------!!


  !!--------------------------- main execution -----------------------------!!
  !!--------------------------- main execution -----------------------------!!
  !!--------------------------- main execution -----------------------------!!

!<execute_only>

  subroutine utel (obj, ntr, hd, tr)
    type (utel_struct), intent (inout) :: obj
    integer,            intent (inout) :: ntr
    double precision,   intent (inout) :: hd(:,:)
    real,               intent (inout) :: tr(:,:)
    !---------------------------------------------
    !--- I am using hdr, tro as temp arrays to hold the traces coming in plus
    !--- any dup traces, etc that the user is asking for.  I then copy the
    !--- temp arrays to hd, tr and reset ntr to the total number coming out.
    !--- The old method only handled one trace at a time, and kept a counter
    !--- so that traces were duplicated one at a time in subsequent calls.  This
    !--- seems simpler and easier to maintain.  I also set the mode to
    !--- "setup only" if the user chooses "CHANGE_DT".  In that case, the job
    !--- builder should not call this routine, but if it does, it will simply
    !--- return with no action.
    integer                            :: i_trace,i_out,n_out,n_dup
    double precision, allocatable      :: hdo(:,:)
    real,             allocatable      :: tro(:,:)

    if(obj%mode == 'CHANGE_DT') then
      ! there is nothing to do.  This is setup only and should not be here.
      ! we have set the "setup only" flag in the update routine.
      return
    endif

    ntrcase: select case (NTR)
      case(NO_MORE_TRACES, FATAL_ERROR) ntrcase !--- error
        call utel_wrapup (obj)
        return
      case(NEED_TRACES) ntrcase !--- go find some traces before us.
        return
      case(1:) ntrcase !--- work on some traces
        n_out = ntr*(1 + obj%num_tr)
        i_out = 0
      case default ntrcase !--- don't know what ntr is!!!
        call utel_wrapup(obj)
        return
    end select ntrcase

    allocate(hdo(size(hd(:,1)),n_out))
    allocate(tro(size(tr(:,1)),n_out))

    loop1: do i_trace = 1, ntr
      !
      ! - Entered from above with a new trace or traces
      !
     
      if (hd(HDR_CURRENT_GROUP,i_trace) /= obj%prev_hw3) then
        obj%prev_hw3 = hd(HDR_CURRENT_GROUP,i_trace)
        obj%seq_hw4  = 0
      end if
      obj%seq_hw1  = obj%seq_hw1 + 1
      obj%seq_hw4  = obj%seq_hw4 + 1
      !
      i_out = i_out + 1
      hdo(:,i_out)                                     = hd(:,i_trace)
      tro(:,i_out)                                     = tr(:,i_trace)
      hdo(HDR_SEQUENCE,i_out)                          = obj%seq_hw1
      hdo(HDR_CURRENT_CHANNEL,i_out)                   = obj%seq_hw4


      if(obj%hdr_flag > 0 ) then
        !-- look for only those traces that have been flagged.  Don't operate on 
        !-- any other traces.
        if(hd(obj%hdr_flag,i_trace) == 0d0) then
          !-- the trace has not been flagged, pass it but don't operate on it.
          cycle
        endif
      endif

      !-- Now we are operating on only flagged traces or on all traces if hdr_flag was
      !-- set to 0.
      innerloop: do n_dup = 1,obj%num_tr
        i_out = i_out + 1
        obj%seq_hw1  = obj%seq_hw1 + 1
        obj%seq_hw4  = obj%seq_hw4 + 1
        hdo(:,i_out)                                   = hd(:,i_trace)
        if (obj%mode == 'DUPLICATE') then
          tro(:,i_out)                                 = tr(:,i_trace)
        else
          tro(:,i_out)                                 = 0.0
          hdo(HDR_LAV,i_out)                           = 0.0d0
        end if
        !-- unflag the traces on output.
        if (obj%hdr_flag > 0)  hdo(obj%hdr_flag,i_out) = 0.0d0
        hdo(HDR_SEQUENCE,i_out)                        = obj%seq_hw1
        hdo(HDR_CURRENT_CHANNEL,i_out)                 = obj%seq_hw4
      end do innerloop



    end do loop1

    !--- Now I am adding in many more traces (potentially) than I started with
    NTR = i_out

    loop2: do i_trace = 1, i_out
      hd(:,i_trace) = hdo(:,i_trace)
      tr(:,i_trace) = tro(:,i_trace)
    end do loop2

    deallocate(hdo)
    deallocate(tro)
    
  end subroutine utel

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

  subroutine utel_wrapup (obj)
    !
    ! - Arguments
    !
    type(utel_struct),intent(inout) :: obj       ! arguments
    !
    ! - Begin
    !
    if (obj%skip_wrapup) return
    !
    obj%skip_wrapup = .true.
    !
  end subroutine utel_wrapup

!</execute_only>

  !!---------------------------- end of module -----------------------------!!
  !!---------------------------- end of module -----------------------------!!
  !!---------------------------- end of module -----------------------------!!


end module utel_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
