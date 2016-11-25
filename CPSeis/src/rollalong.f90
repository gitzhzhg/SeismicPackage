!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ rollalong.f90 -------------------------------!!
!!------------------------------ rollalong.f90 -------------------------------!!
!!------------------------------ rollalong.f90 -------------------------------!!

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
! Name       : ROLLALONG
! Category   : math
! Written    : 2000-09-13   by: Bob Baumel
! Revised    : 2002-06-06   by: Chuck C Burch
! Maturity   : production   2002-06-10
! Purpose    : Manage a running (roll-along) window of traces.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This module manages a roll-along trace window for processes such as MDIP
! which apply some sort of transform in each window and then wish to smoothly
! merge results from different windows by keeping only a subset of traces from
! the middle of each transformed window. The sliding window is specified as
! having width of NUM_WIN traces and is rolled along in increments of NUM_INC
! traces at a time. NUM_INC must be less than NUM_WIN to force some overlap of
! the windows. Parameter HDR_GATH specifies a header word whose transitions
! define gathers (or groups) whose boundaries must not be crossed by the trace
! windows. Users may set HDR_GATH = 0 to specify that the entire dataset be
! considered as a single group.
!
! Actual transformation of the trace windows must be handled by the process
! that calls ROLLALONG. After a window is transformed, ROLLALONG passes a
! subset of the transformed window as output traces. Usually, this subset
! consists of NUM_INC traces from the middle of the window. However, additional
! traces from the beginning of the first window of a group (and from the end of
! the last window of a group) must be output in order to ensure that the
! process calling ROLLALONG ouputs as many traces as were input to it. Also,
! when necessary to avoid a short window at the end of a group, ROLLALONG uses
! an increment smaller than NUM_INC at the end of the group (In this regard,
! the lateral trace windows managed by ROLLALONG are similar to the vertical
! time windows in processes XP and MVXP). When a group's final increment is
! less than NUM_INC, the number of traces output from the middle of that
! group's final window will also be less than NUM_INC.
!
! Processes that call ROLLALONG must require traces to be input singly and will
! output their traces singly. There will be a one-to-one match between input
! and output traces, although at any moment while the job is running, output
! traces will generally lag by some number of traces behind the input. The
! ROLLALONG primitive contains all the logic to maintain this trace flow and
! manage the roll-along windows, leaving your process free to concentrate on
! just transforming the windows.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS
!
! Processes that call ROLLALONG will have the properties:
!    Process is a multiple-trace (loop-splitting) process.
!    This process requires traces to be input one at a time.
!    This process outputs the same traces as it receives (altered).
!    This process outputs one trace at a time.
!
! Subroutine ROLLALONG_STORE includes HD and TR arguments which must match the
! HD and TR arguments in your process's main execution routine. ROLLALONG_STORE
! should be called near the beginning of your main execution routine, just
! after traces have been input.
!
! Subroutine ROLLALONG_STORE includes HD_BUF1 and TR_BUF1 arguments as a buffer
! for collecting the input trace windows. Space for these arrays must be
! provided in your process's data structure.
!
! Subroutine ROLLALONG_SERVE includes HD_BUF2 and TR_BUF2 arguments as a buffer
! where your process must store its transformed windows. Space for these arrays
! must be provided in your process's data structure.
!
! Subroutine ROLLALONG_SERVE includes HD and TR arguments which must match the
! HD and TR arguments in your process's main execution routine. ROLLALONG_SERVE
! should be called near the end of your main execution routine, just before the
! traces are to be output.
!
!-------------------------------------------------------------------------------
!</trace_io_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS
!
! ROLLALONG needs to know the NWIH and NDPT globals, which must therefore be
! passed to it in the ROLLALONG_CREATE call. These globals are used but not
! changed. (THEREFORE, IT IS ASSUMED THAT YOUR PROCESS DOESN'T NEED TO CHANGE
! THESE GLOBALS!)
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS
!
! If HDR_GATH is non-zero, ROLLALONG uses header word HDR_GATH to define groups
! whose boundaries will not be crossed by the trace windows.
!
! ROLLALONG does not alter any header words.
!
! Note: The process that calls ROLLALONG is responsible for setting any header
! words that need to be changed. Generally, you won't have to worry about
! sequence words (because there's a one-to-one match between input and output
! traces), but you'll need to set the LAV header word.
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
!                                          i        i        i
!           call rollalong_create (obj, num_win, num_inc, hdr_gath, &
!                                   i     i
!                                  nwih, ndpt)
!
! type(rollalong_struct),pointer  obj      = pointer to rollalong structure.
! integer                         num_win  = number of traces in window.
! integer                         num_inc  = increment between trace windows.
! integer                         hdr_gath = header word to define gathers.
! integer                         nwih     = the NWIH global.
! integer                         ndpt     = the NDPT global.
!
! Notes:
!    1. The calling process should ensure that NUM_WIN >= 3.
!    2. NUM_INC should be between 1 and NUM_WIN - 2; also NUM_WIN - NUM_INC
!       should be an EVEN number to insure that NUM_INC traces can be found
!       at the middle of each window.
!    3. HDR_GATH may be 0 or may specify a header word number.
!
!
!           call rollalong_delete (obj)
!
! type(rollalong_struct),pointer  obj      = pointer to rollalong structure.
!
!
!                                  b    b   i   i
!           call rollalong_store (obj, ntr, hd, tr, &
!                                    b        b        o
!                                 hd_buf1, tr_buf1, ltransf)
!
! type(rollalong_struct)   obj          = rollalong data structure.
! integer                  ntr          = same as NTR in main execution call.
! double precision         hd(:,:)      = same as HD in main execution call.
! real                     tr(:,:)      = same as TR in main execution call.
! double precision         hd_buf1(:,:) = headers for roll-along windows.
! real                     tr_buf1(:,:) = traces in roll-along windows.
! logical                  ltransf      = tells you whether to do transform.
!
!
!                                  b    b   o   o      b        b
!           call rollalong_serve (obj, ntr, hd, tr, hd_buf2, tr_buf2)
!
! type(rollalong_struct)   obj          = rollalong data structure.
! integer                  ntr          = same as NTR in main execution call.
! double precision         hd(:,:)      = same as HD in main execution call.
! real                     tr(:,:)      = same as TR in main execution call.
! double precision         hd_buf1(:,:) = headers for transformed windows.
! real                     tr_buf1(:,:) = traces in transformed windows.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! SAMPLE CODE
!
! A process that uses ROLLALONG might include the following code:
!
! In your DATA STRUCTURE:
!
!      integer                        :: num_win       ! process parameters.
!      integer                        :: num_inc       ! process parameters.
!      integer                        :: hdr_gath      ! process parameters.
!      integer                        :: nwih, ndpt    ! globals
!      type(rollalong_struct),pointer :: rollalong     ! dependent variables.
!      double precision      ,pointer :: hd_buf1(:,:)  ! dependent variables.
!      real                  ,pointer :: tr_buf1(:,:)  ! dependent variables.
!      double precision      ,pointer :: hd_buf2(:,:)  ! dependent variables.
!      real                  ,pointer :: tr_buf2(:,:)  ! dependent variables.
!
! In your CREATE routine:
!
!      Nullify pointers for obj%rollalong, obj%hd_buf1, obj%tr_buf1,
!                           obj%hd_buf2, and obj%tr_buf2.
!
! In your UPDATE routine:
!
!      call pc_put_control ('NEED_LABEL'  , .true.)
!      call pc_put_control ('NEED_REQUEST', .true.)
!      .....
!      if (pc_do_not_process_traces()) return
!      .....
!      call rollalong_create (obj%rollalong, obj%num_win, obj%num_inc, &
!                             obj%hdr_gath, obj%nwih, obj%ndpt)
!      call mem_alloc (obj%hd_buf1, obj%nwih, obj%num_win)
!      call mem_alloc (obj%tr_buf1, obj%ndpt, obj%num_win)
!      call mem_alloc (obj%hd_buf2, obj%nwih, obj%num_win)
!      call mem_alloc (obj%tr_buf2, obj%ndpt, obj%num_win)
!      .....
!      if (pc_do_not_process_traces()) return
!
! In your MAIN EXECUTION routine:
!
!      subroutine YOURPROCESS (obj, ntr, hd, tr)
!      .....
!      logical         :: ltransf         ! local variables
!      .....
!      call rollalong_store (obj%rollalong, ntr, hd, tr, &
!                            obj%hd_buf1, obj%tr_buf1, ltransf)
!      if (ltransf) then
!        {Perform transform for window of NTR traces,
!        taking input from OBJ%HD_BUF1, OBJ%TR_BUF1 and
!        putting output in OBJ%HD_BUF2, OBJ%TR_BUF2}
!      end if
!      call rollalong_serve (obj%rollalong, ntr, hd, tr, &
!                            obj%hd_buf2, obj%tr_buf2)
!      if (ntr == 1) then
!        call lav_set_hdr (hd(:,1), tr(:,1), obj%ndpt)
!      else if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
!        call YOURPROCESS_wrapup (obj)
!      end if
!      return
!      end subroutine YOURPROCESS
!
! In your WRAPUP routine:
!
!      call mem_free (obj%hd_buf1)
!      call mem_free (obj%tr_buf1)
!      call mem_free (obj%hd_buf2)
!      call mem_free (obj%tr_buf2)
!      if (associated(obj%rollalong)) call rollalong_delete (obj%rollalong)
!
!
! NOTES REGARDING ABOVE CODE
!
!   1. Contents of HD_BUF1, TR_BUF1, HD_BUF2, and TR_BUF2 arrays must persist
!      between calls to your main execution routine; therefore, these arrays
!      must be in your data structure (e.g., they can't be automatic arrays in
!      your main execution routine).
!
!   2. ROLLALONG_CREATE may report an error to the parameter cache. Thus, you
!      should have a second "if (pc_do_not_process_traces()) return" line in
!      your Update routine, after the call to ROLLALONG_CREATE, to insure that
!      the job won't run if ROLLALONG_CREATE reports an error.
!
!   3. If you use ROLLALONG, your process's main execution routine should have
!      only a single set of header/trace arrays. Thus, the TWOSETS control
!      parameter should have its default value of "false" for your process.
!      You do need to set the NEED_LABEL and NEED_REQUEST control parameters
!      to "true".
!
!   4. The HD_BUF1, TR_BUF1 arrays are filled and managed by ROLLALONG_STORE.
!      Do NOT alter the contents of these arrays in any way with your own code.
!      For example, your "if (ltransf)" block in your main execution routine
!      should use HD_BUF1 and TR_BUF1 as input, but be careful not to change
!      their content.
!
!   5. You must set the HD_BUF2, TR_BUF2 arrays when doing the transform in
!      your "if (ltransf)" block. Never alter the HD_BUF2, TR_BUF2 arrays at
!      any OTHER time. ROLLALONG_SERVE will copy traces from the HD_BUF2 and
!      TR_BUF2 buffers into the (HD, TR) output arrays when appropriate.
!
!   6. Do not reset NTR in your code, as the ROLLALONG routines ought to manage
!      that argument correctly. In certain cases, ROLLALONG_STORE will return
!      NTR = FATAL ERROR (e.g., when it receives NTR > 1, which is illegal
!      becauses processes that call ROLLALONG must require single-trace input).
!      If you like, you could test explicitly for NTR = FATAL_ERROR immediately
!      after returning from ROLLALONG_STORE; however, this is unnecessary if
!      you just use the sample code above and don't reset NTR in your own code.
!
!   7. When ROLLALONG_STORE returns LTRANSF=true, it sets NTR to tell you the
!      number of traces to be transformed. Usually, the window size will be
!      NUM_WIN (even at the end of a group). But occasionally, you'll receive
!      a smaller window (e.g., when an entire group defined by jumps in header
!      word HDR_GATH contains fewer than NUM_WIN traces). On those occasions,
!      use only the leftmost NTR traces in the HD_BUF1, TR_BUF1, HD_BUF2, and
!      TR_BUF2 arrays. As in the previous note, do not alter the value of NTR.
!      Your transform must always produce the same number of output traces as
!      received input traces for each window. If you think you received an NTR
!      which is too small to compute a valid transform, you might simply copy
!      the (HD_BUF1, TR_BUF1) input buffer to the (HD_BUF2, TR_BUF2) output
!      buffer, or you might fill the output buffer with dead traces.
!
!
! MEMORY USAGE BY ROLLALONG
!
! If you'd like to report ROLLALONG's memory as part of your own process's
! memory usage, note the following:
!
!   1. When HDR_GATH > 0, the ROLLALONG data structure includes space for
!      storing one header & trace (used for saving the first trace of the next
!      group before completing processing of the current group).
!
!   2. When shifting traces within the HD_BUF1, TR_BUF1 buffer, ROLLALONG may
!      use TEMPORARY memory big enough to hold up to NUM_INC headers & traces.
!      These are automatic arrays, which are quickly allocated & deallocated
!      when needed.
!
! INTERACTION WITH PARAMETER CACHE
!
! ROLLALONG_CREATE calls PC_ERROR if the input NUM_WIN value is less than 3
! or if an error occurs while allocating memory for a saved trace. Your code
! should include a second "if (pc_do_not_process_traces()) return" line after
! calling ROLLALONG_CREATE, to insure that an error in ROLLALONG_CREATE causes
! a job abort. ROLLALONG_CREATE also checks consistency of NUM_INC and NUM_WIN
! parameters, and may decide to use an adjusted NUM_INC if necessary (NUM_INC
! must be between 1 and NUM_WIN - 2, and the difference between NUM_WIN and
! NUM_INC must be even). If an adjustment is necessary, ROLLALONG_CREATE calls
! PC_WARNING to warn that this is being done.
!
! Other routines in the ROLLALONG module do not report errors or warnings to
! the parameter cache, although ROLLALONG_STORE AND ROLLALONG_SERVE may return
! NTR = FATAL_ERROR to indicate an execution-time error.
!
! Although ROLLALONG_CREATE does some checking for the NUM_WIN and NUM_INC
! parameters, this doesn't relieve your process of the need to do your own
! verification of these parameters. Currently, ROLLALONG is written so that
! ROLLALONG_CREATE doesn't get called until back-end setup; however, parameter
! checking really needs to be done earlier in your update routine (to take
! effect during the GUI session). If you perform effective parameter checking
! at that point, ROLLALONG_CREATE will never find any reason to issue its error
! or warning messages.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  4. 2002-06-10  CC Burch     Added ROLLALONG_INIT for use with parallel MDIP
!  3. 2001-01-10  Bob Baumel   Fix error checking in ROLLALONG_CREATE to avoid
!                              case where it might improperly use NUM_INC = 0.
!  2. 2000-09-18  Bob Baumel   Fix missing bracket in XML tag (for DOC program).
!  1. 2000-09-13  Bob Baumel   Initial version.
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
!
!-------------------------------------------------------------------------------
!</programming_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module rollalong_module
      use named_constants_module
      use pc_module
      implicit none

      private
      public :: rollalong_create
      public :: rollalong_delete
      public :: rollalong_init
      public :: rollalong_store
      public :: rollalong_serve

      character(len=100),public,save :: ROLLALONG_IDENT = &
'$Id: rollalong.f90,v 1.4 2002/06/07 18:50:21 CCBurch prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!

      type,public :: rollalong_struct
      private
      integer                   :: num_win            ! window controls
      integer                   :: num_inc            ! window controls
      integer                   :: hdr_gath           ! window controls

      integer                   :: nwih               ! globals
      integer                   :: ndpt               ! globals

      logical                   :: filling, isaved    ! internal params
      logical                   :: endgroup, finished ! internal params
      integer                   :: iwrite, istop      ! internal params
      integer                   :: istart_default     ! internal params
      integer                   :: istop_default      ! internal params
      integer                   :: numinbuf1          ! internal params
      integer                   :: numinbuf2          ! internal params
      double precision          :: group_val          ! internal params
      double precision, pointer :: hdsave (:)         ! internal params
      real            , pointer :: trsave (:)         ! internal params

      end type rollalong_struct

!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      contains

!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!

      subroutine rollalong_create (obj, num_win, num_inc, hdr_gath, &
                                   nwih, ndpt)
      type(rollalong_struct),pointer :: obj          ! arguments
      integer, intent(in)            :: num_win      ! arguments
      integer, intent(in)            :: num_inc      ! arguments
      integer, intent(in)            :: hdr_gath     ! arguments
      integer, intent(in)            :: nwih         ! arguments
      integer, intent(in)            :: ndpt         ! arguments

      integer                        :: ier1, ier2   ! local vars

      allocate (obj)
      nullify (obj%hdsave)
      nullify (obj%trsave)

      if (num_win < 3) then
        call pc_error ('Rollalong_Create found NUM_WIN =', num_win, &
                       'but NUM_WIN must be at least 3.')
        deallocate(obj)
        return
      end if
      obj%num_win  = num_win
      obj%num_inc  = min (max(num_inc,1), num_win-2)
      if (mod(num_win - obj%num_inc, 2) /= 0) then
        if (obj%num_inc == 1) then
          obj%num_inc = 2
        else
          obj%num_inc = obj%num_inc - 1
        end if
      end if
      if (obj%num_inc /= num_inc) then
        call pc_warning ('Rollalong_Create found NUM_INC =', num_inc, &
                         'but will use NUM_INC =', obj%num_inc,       &
                         '. Note: NUM_WIN - NUM_INC must be even.')
      end if
      obj%hdr_gath = hdr_gath
      obj%nwih     = nwih
      obj%ndpt     = ndpt

      call rollalong_init(obj)

      if (hdr_gath > 0) then
        allocate(obj%hdsave(nwih), stat=ier1)
        if (ier1 /= 0) call pc_error  &
               ('Rollalong_Create: Error allocating HDSAVE array.')
        allocate(obj%trsave(ndpt), stat=ier2)
        if (ier2 /= 0) call pc_error  &
               ('Rollalong_Create: Error allocating TRSAVE array.')
        if (ier1/=0 .or. ier2/=0) call rollalong_delete (obj)
      end if

      return
      end subroutine rollalong_create

!!-------------------------------- init -----------------------------------!!
!!-------------------------------- init -----------------------------------!!
!!-------------------------------- init -----------------------------------!!

! --- this routine resets rollalong_struct variables that change  to default

      subroutine rollalong_init(obj)
      type(rollalong_struct),pointer :: obj       ! arguments

      obj%filling      = .true.
      obj%isaved       = .false.
      obj%finished     = .false.
      obj%endgroup     = .false.
      obj%numinbuf1    = 0
      obj%numinbuf2    = 0
      obj%istart_default = (obj%num_win - obj%num_inc)/2 + 1
      obj%istop_default  = obj%istart_default + obj%num_inc - 1
      obj%iwrite = 1
      obj%istop = obj%istop_default
      return
      end subroutine rollalong_init

!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!

      subroutine rollalong_delete (obj)
      type(rollalong_struct),pointer :: obj       ! arguments

      if (associated(obj%hdsave)) deallocate (obj%hdsave)
      if (associated(obj%trsave)) deallocate (obj%trsave)

      deallocate(obj)

      return
      end subroutine rollalong_delete

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

!!--------------------------- rollalong_store -----------------------------!!
!!--------------------------- rollalong_store -----------------------------!!
!!--------------------------- rollalong_store -----------------------------!!

      subroutine rollalong_store (obj, ntr, hd, tr, &
                                  hd_buf1, tr_buf1, ltransf)
      type(rollalong_struct),intent(inout) :: obj              ! arguments
      integer               ,intent(inout) :: ntr              ! arguments
      double precision      ,intent(in)    :: hd(:,:)          ! arguments
      real                  ,intent(in)    :: tr(:,:)          ! arguments
      double precision      ,intent(inout) :: hd_buf1(:,:)     ! arguments
      real                  ,intent(inout) :: tr_buf1(:,:)     ! arguments
      logical               ,intent(out)   :: ltransf          ! arguments

      integer                              :: ishift           ! local

!!----Start subroutine rollalong_store

      ltransf = .false.

      if (obj%finished) then
!!----Nothing to do if FINISHED flag is set.
        ntr = NO_MORE_TRACES
        return
      end if

      if (ntr > 1) then
!!----Processes that call Rollalong must always take single trace input.
        call pc_print ('Rollalong Error: Received', ntr, 'traces in a &
                       &single call. Traces must not be gathered before &
                       &this process.')
        ntr = FATAL_ERROR
        return
      end if

      if (.not. obj%filling) then
!!----If FILLING=false, we must be passing out traces. In this case,
!!----rollalong_store does nothing (i.e., rollalong_serve does the
!!----actual work of passing out traces). But if the job trace flow
!!----is correct, we should always receive NTR = NEED_TRACES when
!!----FILLING=false. Therefore, just check for incorrect NTR values:
        if (ntr == 1) then
          call pc_print ('Rollalong Error: Received a trace when &
                         &expected NEED_TRACES.')
          ntr = FATAL_ERROR
        else if (ntr == NO_MORE_TRACES) then
          call pc_print ('Rollalong Error: Received NO_MORE_TRACES &
                         &when expected NEED_TRACES.')
          ntr = FATAL_ERROR
        end if
        return
      end if

!!----If we get here, FILLING=true, so we're in the process of filling
!!----the HD_BUF1, TR_BUF1 buffers. Either we've been entered from above
!!----with a trace (or NO_MORE_TRACES), or we've been entered from
!!----below with NTR = NEED_TRACES immediately after rollalong_serve has
!!----finished passing out traces and has just switched from FILLING=false
!!----to FILLING=true.

      if (ntr == NEED_TRACES) then
!!----We've been entered from below after rollalong_serve has finished
!!----passing out a batch of traces. If job trace flow is correct, we
!!----should have IWRITE > ISTOP in this case.
        if (obj%iwrite <= obj%istop) then
          call pc_print ('Rollalong Error: Received NEED_TRACES &
                         &when expecting a trace.')
          ntr = FATAL_ERROR
          return
        end if
        if (obj%endgroup) then
!!------We know we've finished processing last window of a group.
          if (obj%isaved) then
!!--------There's a saved trace from next group; move saved trace
!!--------to HD_BUF1, TR_BUF1 buffer and initialize for new group.
            hd_buf1(:obj%nwih, 1) = obj%hdsave
            tr_buf1(:obj%ndpt, 1) = obj%trsave
            obj%isaved = .false.
            obj%endgroup = .false.
            obj%numinbuf1 = 1
            obj%numinbuf2 = 0
            obj%iwrite = 1
            obj%istop = obj%istop_default
          else
!!--------No saved trace; we've finished all processing for the job.
            obj%finished = .true.
            ntr = NO_MORE_TRACES
          end if
        else ! if (.not. obj%endgroup) then
!!------We don't know that this was the last window of a group.
!!------Shift the HD_BUF1, TR_BUF1 buffer leftward by NUM_INC to get
!!------ready for next window but don't re-initialize IWRITE, ISTOP,
!!------NUMINBUF2 because this MAY be the last window of a group,
!!------in which case, we still need to pass out the remaining traces
!!------in the HD_BUF2, TR_BUF2 buffer. The leftward shift is a
!!------CIRCULAR shift so we don't lose any traces.
          ishift = obj%num_inc
          call rollalong_circ_left (obj, hd_buf1, tr_buf1, ishift)
          obj%numinbuf1 = obj%num_win - ishift
        end if
        return
      end if

!!----If we get here, either NTR = 1 or NTR = NO_MORE_TRACES.
!!----Check for end-of-group condition:

      if (ntr == NO_MORE_TRACES) then
        obj%endgroup = .true.
        obj%isaved = .false.
      else if (obj%hdr_gath > 0) then
        if (obj%numinbuf1 == 0) then
!!------Set GROUP_VAL for first trace received in the job.
          obj%group_val = hd(obj%hdr_gath,1)
        else if (hd(obj%hdr_gath,1) /= obj%group_val) then
!!------Found start of next group; save in HDSAVE, TRSAVE.
          obj%endgroup = .true.
          obj%hdsave = hd(:obj%nwih, 1)
          obj%trsave = tr(:obj%ndpt, 1)
          obj%group_val = hd(obj%hdr_gath,1)
          obj%isaved = .true.
        end if
      end if

      if (obj%endgroup) then
!!----We've hit the end of a group (either a transition in header
!!----word HDR_GATH, or end of all the input data.
        if (obj%iwrite > obj%istop) then
!!------We've just finished passing out traces from the middle of
!!------a window. But since we've now hit the end of a group, we
!!------must reset ISTOP to pass out the remaining output traces
!!------from this window.
          obj%istop = obj%numinbuf2
        else ! if (obj%iwrite <= obj%istop) then
!!------We haven't yet filled, transformed and passed out traces from
!!------the current window. We must tell process to do transform and
!!------then pass out all necessary output traces.
          ltransf = .true.
          if (obj%iwrite > 1) then
!!--------There was a previous full window in current group; do circular
!!--------rightward shift in order to transform with a full window.
            ishift = obj%num_win - obj%numinbuf1
            call rollalong_circ_right (obj, hd_buf1, tr_buf1, ishift)
            obj%numinbuf1 = obj%num_win
            obj%iwrite = obj%istart_default + ishift
          end if
          obj%numinbuf2 = obj%numinbuf1
          ntr = obj%numinbuf2
          obj%istop = obj%numinbuf2
          if (obj%istop == 0) then
!!--------This test is true only if NO traces were received in the job.
            ltransf = .false.
            obj%finished = .true.
          end if
        end if
        obj%filling = .false.
        return
      end if

!!----If we reach this point, we have NTR==1, and we haven't hit the end
!!----of a group yet.

      if (obj%iwrite > obj%istop) then
!!----Rollalong_serve has just finished passing out traces from the middle
!!----of a window. We've read in a new trace beyond that window and haven't
!!----yet hit end of group, so initialize IWRITE, ISTOP, NUMINBUF2 for a
!!----new window.
        obj%iwrite = obj%istart_default
        obj%istop  = obj%istop_default
        obj%numinbuf2 = 0
      end if

!!----Add input trace to the HD_BUF1, TR_BUF1 buffer.
      obj%numinbuf1 = obj%numinbuf1 + 1
      hd_buf1(:obj%nwih, obj%numinbuf1) = hd(:obj%nwih, 1)
      tr_buf1(:obj%ndpt, obj%numinbuf1) = tr(:obj%ndpt, 1)
      if (obj%numinbuf1 == obj%num_win) then
!!----We have a full window; tell process to perform transform.
        ltransf = .true.
        obj%filling = .false.
        obj%numinbuf2 = obj%num_win
        ntr = obj%num_win
      end if

      return
      end subroutine rollalong_store

!!--------------------------- rollalong_serve -----------------------------!!
!!--------------------------- rollalong_serve -----------------------------!!
!!--------------------------- rollalong_serve -----------------------------!!

      subroutine rollalong_serve (obj, ntr, hd, tr, hd_buf2, tr_buf2)
      type(rollalong_struct),intent(inout) :: obj              ! arguments
      integer               ,intent(inout) :: ntr              ! arguments
      double precision      ,intent(out)   :: hd(:,:)          ! arguments
      real                  ,intent(out)   :: tr(:,:)          ! arguments
      double precision      ,intent(inout) :: hd_buf2(:,:)     ! arguments
      real                  ,intent(inout) :: tr_buf2(:,:)     ! arguments

      if (ntr == FATAL_ERROR) return

      if (obj%finished) then
        ntr = NO_MORE_TRACES
      else if (obj%filling) then
        ntr = NEED_TRACES
      else
!!----Pass out trace from position IWRITE in HD_BUF2, TR_BUF2 buffer.
        hd(:obj%nwih, 1) = hd_buf2(:obj%nwih, obj%iwrite)
        tr(:obj%ndpt, 1) = tr_buf2(:obj%ndpt, obj%iwrite)
        obj%iwrite = obj%iwrite + 1
        if (obj%iwrite > obj%istop) then
!!------Switch to FILLING mode if passed out all traces up to ISTOP.
          obj%filling = .true.
        end if
        ntr = 1
      end if

      return
      end subroutine rollalong_serve

!!------------------------- rollalong_circ_left ---------------------------!!
!!------------------------- rollalong_circ_left ---------------------------!!
!!------------------------- rollalong_circ_left ---------------------------!!

      subroutine rollalong_circ_left (obj, hd_buf1, tr_buf1, ishift)
!!----Applies left circular shift to HD_BUF1, TR_BUF1 arrays.
      type(rollalong_struct),intent(in)    :: obj              ! arguments
      double precision      ,intent(inout) :: hd_buf1(:,:)     ! arguments
      real                  ,intent(inout) :: tr_buf1(:,:)     ! arguments
      integer               ,intent(in)    :: ishift           ! arguments

      double precision      :: hdtemp (obj%nwih, ishift)       ! automatic
      real                  :: trtemp (obj%ndpt, ishift)       ! automatic

      if (ishift < 1) return

      hdtemp = hd_buf1 (:obj%nwih, :ishift)
      trtemp = tr_buf1 (:obj%ndpt, :ishift)

      hd_buf1 (:obj%nwih, 1:(obj%num_win - ishift) )  &
          = hd_buf1 (:obj%nwih, ishift+1:obj%num_win )
      tr_buf1 (:obj%ndpt, 1:(obj%num_win - ishift) )  &
          = tr_buf1 (:obj%ndpt, ishift+1:obj%num_win )

      hd_buf1 (:obj%nwih, (obj%num_win - ishift + 1):obj%num_win )  &
          = hdtemp
      tr_buf1 (:obj%ndpt, (obj%num_win - ishift + 1):obj%num_win )  &
          = trtemp

      return
      end subroutine rollalong_circ_left

!!------------------------ rollalong_circ_right ---------------------------!!
!!------------------------ rollalong_circ_right ---------------------------!!
!!------------------------ rollalong_circ_right ---------------------------!!

      subroutine rollalong_circ_right (obj, hd_buf1, tr_buf1, ishift)
!!----Applies right circular shift to HD_BUF1, TR_BUF1 arrays.
      type(rollalong_struct),intent(in)    :: obj              ! arguments
      double precision      ,intent(inout) :: hd_buf1(:,:)     ! arguments
      real                  ,intent(inout) :: tr_buf1(:,:)     ! arguments
      integer               ,intent(in)    :: ishift           ! arguments

      double precision      :: hdtemp (obj%nwih, ishift)       ! automatic
      real                  :: trtemp (obj%ndpt, ishift)       ! automatic

      if (ishift < 1) return

      hdtemp  &
         = hd_buf1 (:obj%nwih, (obj%num_win - ishift + 1):obj%num_win )
      trtemp  &
         = tr_buf1 (:obj%ndpt, (obj%num_win - ishift + 1):obj%num_win )

      hd_buf1 (:obj%nwih, ishift+1:obj%num_win )  &
         = hd_buf1 (:obj%nwih, 1:(obj%num_win - ishift) )
      tr_buf1 (:obj%ndpt, ishift+1:obj%num_win )  &
         = tr_buf1 (:obj%ndpt, 1:(obj%num_win - ishift) )

      hd_buf1 (:obj%nwih, :ishift) = hdtemp
      tr_buf1 (:obj%ndpt, :ishift) = trtemp

      return
      end subroutine rollalong_circ_right

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module rollalong_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
