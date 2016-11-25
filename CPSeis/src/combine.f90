!<CPS_v1 type="PROCESS"/>
        
!!------------------------------- combine.f90 --------------------------------!!
!!------------------------------- combine.f90 --------------------------------!!
!!------------------------------- combine.f90 --------------------------------!!

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
! Name       : COMBINE    (Combine trace pairs)
! Category   : miscellaneous
! Written    : 1990-02-13   by: Mike Howard
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Combine trace pairs with a simple mathematical operation.
! Portability: No known limitations.
! Parallel   : Yes
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! 
! Input to COMBINE can be single traces or gathers.  Input traces are grouped 
! internally into sequential pairs.  For each input trace pair (TR1, TR2) a 
! single trace (TR_OUT) is output according to the following formula.
!
!
!                  TR_OUT = (C1 * TR1) + (C2 * TR2) + C3.
!             or   TR_OUT = TR1 * TR2
!
!
! Normally gathers of input traces should be even, otherwise COMBINE will form 
! a pair from the last trace of one gather and the first of the next.
!
!
! Output Header Words
!
! Header word values for TR_OUT are set to the header word values of TR1, 
! except for:
!
!        header word 1        reset
!        header word 2        see chart below
!        header word 3        unchanged
!        header word 4        reset consistent with header word 3
!        header word 5        sum of header word 5 of TR1 and TR2
!        header word 25       recalculated
!        header word 64       see chart below
!        
! Mute header words of TR_OUT are set according to the following rules.
!
!    1.  If TR1 and TR2 are both live, then the head (tail) mute index of 
!        TR_OUT is the maximum (minimum) of the head (tail) mute indices of TR1
!        and TR2.
!
!    2.  If either of TR1 and TR2 is dead, but not both, then the mute
!        header words of TR_OUT are set to the mute headers of the live input 
!        trace.
!
!    3.  If both TR1 and TR2 are dead, then the mute header words of 
!        TR_OUT are set to 1 and NDPT and all samples of TR_OUT are set to C3.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
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
! Input may be single traces or gathers.
!
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process alters input traces.
!
! This process outputs one half as many traces at a time as are input.
!
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
! IPN      process number                        used (must not be changed)
! MAXTR    max number of traces input/output     used but not changed
! NDPT     number of samples in trace            used but not changed
! NWIH     number of words in trace header       used but not changed
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
! Header word values for TR_OUT are set to the header word values of TR1, 
! except for:
!
!        header word 1        reset
!        header word 2        see chart below
!        header word 3        unchanged
!        header word 4        reset consistent with header word 3
!        header word 5        sum of header word 5 of TR1 and TR2
!        header word 25       recalculated
!        header word 64       see chart below
!        
! Mute header words of TR_OUT are set according to the following rules.
!
!    1.  If TR1 and TR2 are both live, then the head (tail) mute index of 
!        TR_OUT is the maximum (minimum) of the head (tail) mute indices of TR1
!        and TR2.
!
!    2.  If either of TR1 and TR2 is dead, but not both, then the mute
!        header words of TR_OUT are set to the mute headers of the live input 
!        trace.
!
!    3.  If both TR1 and TR2 are dead, then the mute header words of 
!        TR_OUT are set to 1 and NDPT and all samples of TR_OUT are set to C3.
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author       Description
!     ----        ------       -----------
!012. 2006-06-20  B. Menger    Removed Unused Variables.
! 11. 2004-01-29  Bill Menger  Fixed the Division method (bug from rev 10)
! 10. 2003-12-10  Bill Menger  Added option to multiply two traces.
!  9. 2001-06-04  Brad Kruse   Change name for wrapup flag to SKIP_WRAPUP for
!                              clarity, and slightly change how it is used.
!  8. 2000-06-16  B. Kruse     Update Maturity from raw to production
!  7. 2000-06-13  B. Kruse     Correct lines > 80 char, and replace tabs.
!                              Review team comments.
!  6. 2000-03-22  B. Kruse     Correct header word 1 & 4 sequencing.
!                              CPS Test report dated 2000-03-17.
!                              Reengineered, replace optional operator with
!                              'Add', remove individual trace constants C2/C4.
!                              Renamed parameters.  Provide single, ungathered
!                              trace handling, with one internal trace buffer.
!                              Add controls for Need_Request and Need_Label.
!  5. 2000-03-06  B. Kruse     Removed extra print statement.
!  4. 2000-02-23  B. Kruse     Add GUI support.  Set LAV, add ComboBox for OPER.
!  3. 1999-12-16  Sharp        Convert to new system
!  2. 1998-11-13  Vunderink    Begin using the f90 compiler.
!  1. 1990-02-13  Howard       Original version.  
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
!
! Control Parm   Value
! -------------  -----
! PARALLEL_SAFE  true 
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
!    NTR == NEED_TRACES    if you need another trace before passing any out. 
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
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
!
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  XML code for the GUI goes in this section. />
!
!<gui_def>
!<NS COMBINE Process/NC=80>
!
!                          COMBINE trace pairs Process
!           Combine trace pairs with a simple mathematical operation.
!
!
!        For each pair of input traces:
!
!  operation=`CC
!
!  for Division, if T2(i) = 0.0 OR C2=0.0 then Output(i) = C3
!  
!
!  Output_Trace = Trc1 x 
!     C1 `FFFFFFFFF (operation)  Trc2 * 
!     C2 `FFFFFFFFF + 
!     C3 `FFFFFFFFF
!
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="operation">
!<Tip> Pick the operation to perform.  Default is to do the Add. </Tip>
! Default = Add
! Allowed = Add (Out = c1*T1 + c2*T2 + c3)
! Allowed = Sub (Out = c1*T1 - c2*T2 + c3)
! Allowed = Mul (Out = c1*T1 x c2*T2 + c3)
! Allowed = Div (Out = c1*T1 / c2*T2 + c3) [if T2(i) = 0.0 then O(i) = c3] )
!</Help>
!<Help KEYWORD="C1">
!<Tip> Constant that multiplies first trace sample values. </Tip>
! Default = 1.0
! Allowed = real
! Constant that multiplies first trace sample values in the following formula.
!
!                 TR_OUT = (C1 * TR1) (op) (C2 * TR2) + C3.
!</Help>
!
!<Help KEYWORD="C2">
!<Tip> Constant that multiplies second trace sample values. </Tip>
! Default = 1.0
! Allowed = real
! Constant that multiplies second trace sample values in the following formula.
!
!                  TR_OUT = (C1 * TR1) (op) (C2 * TR2) + C3.
!</Help>
!
!<Help KEYWORD="C3">
!<Tip> Constant to add to the scaled operation on TR1 and TR2. </Tip>
! Default = 0.0
! Allowed = real
! Constant to add to the scaled trace operation in the following formula.
!    
!                  TR_OUT = (C1 * TR1) (op) (C2 * TR2) + C3.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

module combine_module
  !
  ! - Module references
  !
  use pc_module
  use named_constants_module
  use lav_module, only:  LAV_SET_HDR
  !
  implicit none
  !
  private
  public :: combine_create     ! uses the parameter cache.
  public :: combine_initialize
  public :: combine_update     ! uses the parameter cache.
  public :: combine_delete
!<execute_only>
  public :: combine            ! main execution (trace processing) routine.
  public :: combine_wrapup
!</execute_only>

  character (len = 100), public, save :: combine_IDENT =     &
       '$Id: combine.f90,v 1.12 2006/06/20 13:11:49 Menger prod sps $'

  !!------------------------- parameter structure --------------------------!!
  !!------------------------- parameter structure --------------------------!!
  !!------------------------- parameter structure --------------------------!!

  type,public :: combine_struct              
    !
    private
    double precision          :: hw1_seq
    double precision          :: hw3
    double precision          :: hw4_seq
    integer                   :: ndpt
    integer                   :: nwih
    logical                   :: skip_wrapup       ! wrapup flag.
    real                      :: c1
    real                      :: c2
    real                      :: c3
    logical                   :: buf_flag
    character(len=3)          :: operation
    double precision, pointer :: buf_hd (:)
    real,             pointer :: buf_tr (:)
    !
  end type combine_struct

  integer,parameter           :: num_opt_operation = 4
  character(len=3),parameter  :: opt_operation (num_opt_operation) &
    = (/'Add','Sub','Mul','Div'/)

  !!--------------------------------- data ---------------------------------!!
  !!--------------------------------- data ---------------------------------!!
  !!--------------------------------- data ---------------------------------!!

contains

  !!-------------------------------- create --------------------------------!!
  !!-------------------------------- create --------------------------------!!
  !!-------------------------------- create --------------------------------!!

  subroutine combine_create (obj)
    !
    ! - Arguments
    !
    type (combine_struct), pointer :: obj       ! arguments
    !
    ! - Begin combine_create
    !
        !print*,'Create started'
    nullify(obj)
    allocate (obj)
    nullify (obj%buf_tr)
    nullify (obj%buf_hd)
    !
    call combine_initialize (obj)
    !
        !print*,'Create completed'
  end subroutine combine_create


  !!-------------------------------- delete --------------------------------!!
  !!-------------------------------- delete --------------------------------!!
  !!-------------------------------- delete --------------------------------!!

  subroutine combine_delete (obj)
    !
    ! - Arguments
    !
    type(combine_struct),pointer :: obj       ! arguments
    !
    ! - Begin combine_delete
    !

!<execute_only>
        !print*,'Delete started'

    call combine_wrapup (obj)

!</execute_only>

    if (associated(obj%buf_tr )) deallocate (obj%buf_tr) 
    if (associated(obj%buf_hd )) deallocate (obj%buf_hd) 
    !   
    deallocate(obj)
    !
        !print*,'Delete completed'
  end subroutine combine_delete


  !!--------------------------- initialize ---------------------------------!!
  !!--------------------------- initialize ---------------------------------!!
  !!--------------------------- initialize ---------------------------------!!

  subroutine combine_initialize (obj)
    !
    ! - Arguments
    !
    type(combine_struct),intent(inout) :: obj       ! arguments
    !
    ! - Begin combine_initialize
    !
        !print*,'Initialize started'
    obj%c1       = 1.0
    obj%c2       = 1.0
    obj%c3       = 0.0
    obj%operation='Add'
    obj%ndpt     = 0
    obj%nwih     = HDR_BOTTOM_MUTE
    obj%hw3      = -huge (0.0)
    obj%hw1_seq  = 1.0d0
    obj%hw4_seq  = 1.0d0
    obj%buf_flag = .false.
    !
    call combine_update (obj)
    !
        !print*,'Initialize completed'
  end subroutine combine_initialize


  !!------------------------- start of update ------------------------------!!
  !!------------------------- start of update ------------------------------!!
  !!------------------------- start of update ------------------------------!!

  subroutine combine_update (obj)
    !
    ! - Arguments
    !
    type (combine_struct), intent (inout) :: obj             ! arguments
    !
    ! - Local variables
    !
    integer :: ier2
    !
    ! - Begin combine_update
    !
        !print*,'Update started'
    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


    !!------------------------- read parameters ----------------------------!!
    !!------------------------- read parameters ----------------------------!!
    !!------------------------- read parameters ----------------------------!!

    call pc_get_global ('ndpt',obj%ndpt)  ! number of trace samples.
    call pc_get_global ('nwih',obj%nwih)  ! number of trace samples.
    call pc_get('operation',obj%operation)
    call pc_get('c1',obj%c1)
    call pc_get('c2',obj%c2)
    call pc_get('c3',obj%c3)
    call pc_put_options_field('operation',opt_operation,num_opt_operation)

    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!
        !print*,' Verification started'
    if(obj%ndpt == 0) then
       call pc_warning ("Global value NDPT must be greater than zero; found", &
                        obj%ndpt)
    end if
    !
    if(obj%nwih < HDR_BOTTOM_MUTE) then
       call pc_error ("Global value NWIH must be 64 or greater; found", &
                        obj%nwih)
    end if


    !!--------------------- call processes internally ----------------------!!
    !!--------------------- call processes internally ----------------------!!
    !!--------------------- call processes internally ----------------------!!

    call pc_put_global ('ndpt',obj%ndpt)  ! number of trace samples.
    call pc_put_global ('nwih',obj%nwih)  ! number of trace samples.
    !
    call pc_put_control ('ntapes',       0)             ! default 0 
    call pc_put_control ('need_request', .true.)       ! default false 
    call pc_put_control ('need_label',   .true.)       ! default false 
    call pc_put_control ('twosets',      .false.)       ! default false 
    call pc_put_control ('nscratch',     (obj%ndpt + obj%nwih * 2) * 2) 
    call pc_put_control ('nstore',       obj%ndpt + obj%nwih * 2) 
    call pc_put_control ('iftd',         .false.)       ! default false 
    call pc_put_control ('ndisk',        0)             ! default 0 
    call pc_put_control ('setup_only',   .false.)       ! default .false. 
    call pc_put_control ('parallel_safe' , .true.)
    !
    call pc_put ('operation',obj%operation)
    call pc_put ('C1', obj%c1)
    call pc_put ('C2', obj%c2)
    call pc_put ('C3', obj%c3)


    !!-------------------------- write parameters --------------------------!!
    !!-------------------------- write parameters --------------------------!!
    !!-------------------------- write parameters --------------------------!!
        !print*,' Verification completed'
    !!----------------------- prepare for execution ------------------------!!
    !!----------------------- prepare for execution ------------------------!!
    !!----------------------- prepare for execution ------------------------!!

    if (associated (obj%buf_tr)) deallocate (obj%buf_tr)   ! for all pointers. 
    if (associated (obj%buf_hd)) deallocate (obj%buf_hd)   ! for all pointers. 


!<execute_only>

    if (pc_do_not_process_traces()) return
    !
    obj%skip_wrapup = .false.
    !
    obj%hw3     = -huge (0.0)
    obj%hw1_seq = 1.0d0
    obj%hw4_seq = 1.0d0
    !
    allocate (obj%buf_tr (obj%ndpt), stat=ier2) 
    allocate (obj%buf_hd (obj%nwih), stat=ier2) 
    obj%buf_flag = .false.

!</execute_only>

    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!

    !
        !print*,'Update completed'
  end subroutine combine_update

  !!-------------------------------- traps ---------------------------------!!
  !!-------------------------------- traps ---------------------------------!!
  !!-------------------------------- traps ---------------------------------!!

  !!---------------------------- main execution ----------------------------!!
  !!---------------------------- main execution ----------------------------!!
  !!---------------------------- main execution ----------------------------!!

!<execute_only>

  subroutine combine (obj,ntr,hd,tr)
    !
    ! - Arguments
    !
    type (combine_struct), intent (inout) :: obj                 ! arguments
    integer,               intent (inout) :: ntr                 ! arguments
    double precision,      intent (inout) :: hd (:,:)            ! arguments
    real,                  intent (inout) :: tr (:,:)            ! arguments
    !
    ! - Local variables
    !
    integer :: k, ko, j
    integer :: n1
    integer :: num_pairs


    !
    ! - Begin combine
    ! 
        !print*,'Combine started: ntr=',ntr
    if (ntr < 1) then
      if (ntr == NEED_TRACES) then
        !
        ! - Normal occurence; but need another input before an output trace
        !   is ready
        !
      elseif (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
        call combine_wrapup (obj)
      else
        call pc_error ("COMBINE:  Required input is pairs of traces "    &
                       // "(ntr >= 2).")
        call pc_error ("COMBINE:  Received.  ", ntr, " traces.  Aborting.")
        ntr = FATAL_ERROR
      end if
      return
    end if
    !
    ! - Check for a buffered trace
    !
    n1 = 0
    !
        !print*,' obj%buf_flag = ',obj%buf_flag
    if (obj%buf_flag) then
      !
      n1 = n1 + 1 
      !
        !print*,' 647: combine_tr_pair: n1=',n1
      call combine_tr_pair (tr1    = obj%buf_tr (1:obj%ndpt),    &
                            hd1    = obj%buf_hd (1:obj%nwih),    &
                            tr2    = tr (1:obj%ndpt, 1),         &
                            hd2    = hd (1:obj%nwih, 1),         &
                            tr_out = tr (1:obj%ndpt, n1),        &
                            hd_out = hd (1:obj%nwih, n1),        &
                            obj    = obj)
      !
      obj%buf_flag = .false. 
      !
    end if
    !
    ko = n1
    num_pairs = (ntr - n1) /2
    k = ko + 1
    !
  Loop_Thru_Traces:    &
    DO j = 1, num_pairs 
        !print*,' inside loop: j = ',j
      !
      KO = ko + 1 
      !
      call combine_tr_pair (tr1    = tr (1:obj%ndpt, k),        &
                            hd1    = hd (1:obj%nwih, k),        &
                            tr2    = tr (1:obj%ndpt, k + 1),    &
                            hd2    = hd (1:obj%nwih, k + 1),    &
                            tr_out = tr (1:obj%ndpt, ko),       &
                            hd_out = hd (1:obj%nwih, ko),       &
                            obj    = obj)
      !
      k = k + 2 
      !
    END DO Loop_Thru_Traces
    !
    obj%buf_flag = (num_pairs * 2 + n1) < ntr
    !
    if (obj%buf_flag) then
      !
      obj%buf_tr = tr (1:obj%ndpt, ntr)
      obj%buf_hd = hd (1:obj%nwih, ntr)
      !
    end if
    !
    if (ko == 0) then
      ntr = NEED_TRACES
    else
      ntr = KO 
      !
      call LAV_SET_HDR (TR   = TR,    &
                        HD   = HD,    &
                        NTR  = NTR,   &
                        NDPT = obj%NDPT)
      !
    end if
    !
        !print*,' Return.'
  end subroutine combine


  !!--------------------------- combine_tr_pair ----------------------------!!
  !!--------------------------- combine_tr_pair ----------------------------!!
  !!--------------------------- combine_tr_pair ----------------------------!!

  subroutine combine_tr_pair (tr1, tr2, hd1, hd2, tr_out, hd_out, obj)
    !
    ! - Arguments
    !
    real,                  intent (in)    :: tr1 (:)
    real,                  intent (in)    :: tr2 (:)
    double precision,      intent (in)    :: hd1 (:)
    double precision,      intent (in)    :: hd2 (:)
    real,                  intent (out)   :: tr_out (:)
    double precision,      intent (out)   :: hd_out (:)
    type (combine_struct), intent (inout) :: obj             ! arguments
    !
    ! - Local variables
    !

    integer :: top
    integer :: bottom
    real    :: c
    !
    ! - Begin 
    !

    if    (hd1(HDR_LAV) == 0.0d0 .and. hd2(HDR_LAV) /= 0.0d0 ) then
      hd_out = hd2
      top    = hd2(HDR_TOP_MUTE)
      bottom = hd2(HDR_BOTTOM_MUTE)
    elseif(hd1(HDR_LAV) == 0.0d0 .and. hd2(HDR_LAV) == 0.0d0 ) then
      hd_out=hd1
      top    = 1
      bottom = obj%ndpt
    elseif(hd1(HDR_LAV) /= 0.0d0 .and. hd2(HDR_LAV) == 0.0d0 ) then
      hd_out = hd1
      top    = hd1(HDR_TOP_MUTE)
      bottom = hd1(HDR_BOTTOM_MUTE)
    elseif(hd1(HDR_LAV) /= 0.0d0 .and. hd2(HDR_LAV) /= 0.0d0 ) then
      hd_out = hd1
      top    = max (a1 = hd1 (HDR_TOP_MUTE),       &
                    a2 = hd2 (HDR_TOP_MUTE))
      bottom = min (a1 = hd1 (HDR_BOTTOM_MUTE),    &
                    a2 = hd2 (HDR_BOTTOM_MUTE))
    endif

    select case(obj%operation)
    case('Add')
      if    (hd1(hdr_lav) == 0.0d0 .and. hd2(hdr_lav) == 0.0d0) then
        tr_out(top:bottom) = obj%c3
      elseif(hd1(hdr_lav) == 0.0d0 .and. hd2(hdr_lav) /= 0.0d0) then
        tr_out(top:bottom) = obj%c2*tr2(top:bottom) + obj%c3 
      elseif(hd1(hdr_lav) /= 0.0d0 .and. hd2(hdr_lav) == 0.0d0) then
        tr_out(top:bottom) = obj%c1*tr1(top:bottom) + obj%c3 
      elseif(hd1(hdr_lav) /= 0.0d0 .and. hd2(hdr_lav) /= 0.0d0) then
        tr_out (top:bottom) = obj%c1*tr1(top:bottom) + &
                              obj%c2*tr2(top:bottom) + &
                              obj%c3 
      endif
    case('Mul')
      c = obj%C1*obj%C2
      if(hd1(HDR_LAV) == 0.0d0 .or. hd2(HDR_LAV) == 0.0d0 ) then
        tr_out(top:bottom) = obj%c3
      else
        tr_out (top:bottom) = c*tr1 (top:bottom)*tr2(top:bottom) +obj%c3
      endif
    case('Sub')
      if    (hd1(hdr_lav) == 0.0d0 .and. hd2(hdr_lav) == 0.0d0) then
        tr_out(top:bottom) = obj%c3
      elseif(hd1(hdr_lav) == 0.0d0 .and. hd2(hdr_lav) /= 0.0d0) then
        tr_out(top:bottom) = - obj%c2*tr2(top:bottom) + obj%c3 
      elseif(hd1(hdr_lav) /= 0.0d0 .and. hd2(hdr_lav) == 0.0d0) then
        tr_out(top:bottom) = obj%c1*tr1(top:bottom) + obj%c3 
      elseif(hd1(hdr_lav) /= 0.0d0 .and. hd2(hdr_lav) /= 0.0d0) then
        tr_out (top:bottom) = obj%c1*tr1(top:bottom) - &
                              obj%c2*tr2(top:bottom) + &
                              obj%c3 
      endif
    case('Div')
      if(obj%c2 /= 0.0 ) then 
        c = obj%c1/obj%c2
      else
        c = 0.0
      endif
      where(tr2 == 0.0) tr_out  = obj%c3
      where(tr2 /= 0.0) tr_out  = obj%c3 + c * tr1 / tr2
    case default
    end select
    !
    ! - Mute the resulting trace
    !
    if (top > 1) then
      tr_out (:top-1)  = 0.0
    end if
    !
    tr_out (bottom+1:) = 0.0
    !
    hd_out (HDR_TOP_MUTE) = top
    hd_out (HDR_BOTTOM_MUTE) = bottom


    !
    ! - Update header word sequencing
    !
    if (obj%hw3 == hd_out (HDR_CURRENT_GROUP)) then
      obj%hw4_seq = anint (obj%hw4_seq + 1.0d0)
    else
      obj%hw4_seq = 1.0d0
      obj%hw3 = hd_out (HDR_CURRENT_GROUP)
    end if
    !
    hd_out (HDR_SEQUENCE)         = obj%hw1_seq
    obj%hw1_seq                   = anint (obj%hw1_seq + 1.0d0)
    hd_out (HDR_CURRENT_GROUP)    = obj%hw3
    hd_out (HDR_CURRENT_CHANNEL)  = obj%hw4_seq
    hd_out (HDR_FOLD)             = hd1 (HDR_FOLD) + hd2 (HDR_FOLD)
    !
  end subroutine combine_tr_pair

!</execute_only>

  !!----------------------------- wrapup -----------------------------------!!
  !!----------------------------- wrapup -----------------------------------!!
  !!----------------------------- wrapup -----------------------------------!!

!<execute_only>

  subroutine combine_wrapup (obj)
    !
    ! - Arguments
    !
    type(combine_struct),intent(inout) :: obj       ! arguments
    !
    ! - Begin combine_wrapup
    !
    if (obj%skip_wrapup) return
    obj%skip_wrapup = .true.
    !
  end subroutine combine_wrapup

!</execute_only>

  !!---------------------------- end of module -----------------------------!!
  !!---------------------------- end of module -----------------------------!!
  !!---------------------------- end of module -----------------------------!!

  end module combine_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

