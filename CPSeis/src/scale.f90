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

!<brief_doc>
!-------------------------------------------------------------------------------
!                         C P S   P R O C E S S                  
!
! Name       : SCALE     (SCALE seismic traces)   [includes former SCALEH]    
! Category   : amplitude_mod
! Written    : 1991-04-03   by: Mike Howard
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Operate on seismic trace samples using a general formula.
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
! SCALE requires input to be in single traces.  Each input trace (TR) is 
! operated on by the following formula to form the output trace (TR_OUT).
!
!                 TR_OUT = (C1 * FUNCTION(TR + C2)) + C3.
!
! FUNCTION can take on the following values:
!
!        IDENTITY        (no change),
!        SQRT            (square root),
!        SQUARE          (square),
!        RECIPROCAL      (reciprocal)
!        ABS             (absolute value),
!        EXP             (base e exponential),
!        LOG             (natural log),
!        COS             (cosine of radian argument),
!        TAN             (tangent of radian argument).
! 
! Samples of the input trace in the head mute or tail mute zones are 
! disregarded. 
!
!
! Header Word Values as Formula Parameters
!
! Users may specify the formula parameters directly (by specifying C1, C2 and 
! C3) or by specifying a header word whose value will be used as the formula 
! parameter.  For example, C1 can be replaced by HDR_C1, etc.  However the user 
! must choose whether to use either C1 or HDR_C1, for example, not both.
!
!
! Zero Samples and Dead Traces 
!
! Certain functions are not defined for a zero argument.  In the situation that
! the user chooses such a function and a zero argument is encountered, the 
! value of the function will be set to zero.  
!
!
! Header Words
!
! The mute header words of TR_OUT are the same as those of the input trace.
! Header word 25 is recalculated.  No other header words are changed.
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
! Process is a single-trace process.
!
! This process requires traces to be input as single traces.
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
! This process outputs one trace at a time.
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
! GATHERED whether input is gathered             used but not changed
! MAXTR    max number of traces input/output     used but not changed
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
! Header word values for TR_OUT are the same as TR except for:
!
! 25      LAV                             recalculated
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author       Description
!     ----        ------       -----------
! 10. 2006-06-20  B. Menger    Removed Unused Variables.
!  9. 2001-04-30  Brad Kruse  Change name for wrapup flag to SKIP_WRAPUP for
!                             clarity, and slightly change how it is used.
!  8. 2000-06-23  B. Kruse   Correct for review comments.  Update Maturity from
!                            raw to beta.  Fix bug: wrong values calculated for 
!                            TAN and EXP functions (No change made for EXP).
! 7.  2000-03-23  B. Kruse   Simplified a WHERE expression to satisfy the
!                            Portland Group compiler, when compiling -O2 -vect
! 6.  2000-03-22  B. Kruse   Converted to new system.  Change name to SCALE, 
!                            revised FUNCTION to modify trace samples rather 
!                            than coefficients.  
! 5.  1999-02-15  Goodger    Begin using the fortran90 compiler.          
! 4.  1994-04-06  Goodger    Fix problem scaling dead trace - was raising 
!                            zero to a negative power.
! 3.  1993-10-20  Troutt     Added CONS parameter.
! 2.  1991-07-09  Troutt     Changed process name (and all related names)
!                            from SCALE to SCALEH to avoid conflict with
!                            plot routine in CPSPLOT.
! 1.  1991-04-03  Howard     Original version.
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
!
!<gui_def>
!<NS SCALE Process/NC=80/NR=20>
!
!             SCALE seismic traces Process (includes former SCALEH)
!          Operate on seismic trace samples using a general formula.
!
!     HDR_Flag `III
!
!     Trace Out =  HDR_C1 `III  or C1 `FFFFFFFFFFFF  
!
!                  *  FUNCTION `CCCCCCCCC
!
!                        of (
!
!                              Trace  +  HDR_C2 `III or C2 `FFFFFFFFFFFF 
!
!                            )
!
!                  +  HDR_C3 `III or C3 `FFFFFFFFFFFF
!
!<PARMS or C1[C1]>
!<PARMS or C2[C2]>
!<PARMS or C3[C3]>
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! 
! If HDR_FLAG = 0 then all traces are processed.  Otherwise only traces with 
! a flag set in header word HDR_FLAG are processed. 
!
!</Help>
!
!<Help KEYWORD="C1">
!<Tip> Constant that multiplies the function values. </Tip>
! Default = 1.0
! Allowed = real
! Constant that multiplies the function values in the following formula.
!
!                   TR_OUT = (C1 * FUNCTION(TR + C2)) + C3. 
!
! The C1 parameter is active only if HDR_C1 = 0. 
!</Help>
!
!<Help KEYWORD="HDR_C1">
!<Tip> Header word whose values are used for C1 in the formula. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! Header word whose values are a constant that multiplies the function values 
! in the following formula.
!
!                   TR_OUT = (C1 * FUNCTION(TR + C2)) + C3. 
!
! If HDR_C1 = 0, then the C1 parameter is active.
!</Help>
!
!<Help KEYWORD="FUNCTION">
!<Tip> Function to use in the formula. </Tip>
! Default = IDENTITY
! Allowed = IDENTITY    (no change),
! Allowed = SQRT        (square root),
! Allowed = SQUARE      (square),
! Allowed = RECIPROCAL  (reciprocal)
! Allowed = ABS         (absolute value),
! Allowed = EXP         (base e exponential),
! Allowed = LOG         (natural log),
! Allowed = COS         (cosine of radian argument),
! Allowed = TAN         (tangent of radian argument).
! Function to use in the following formula.
!
!                   TR_OUT = (C1 * FUNCTION(TR + C2)) + C3. 
!</Help>
!
!<Help KEYWORD="C2">
!<Tip> Constant to add to the trace sample values. </Tip>
! Default = 0.0
! Allowed = real
! Constant to add to the trace sample values in the following formula.
!
!                   TR_OUT = (C1 * FUNCTION(TR + C2)) + C3. 
!
! The C2 parameter is active only if HDR_C2 = 0. 
!</Help>
!
!<Help KEYWORD="HDR_C2">
!<Tip> Header word whose values are used for C2 in the formula. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! Header word whose values add to the trace sample values in the following 
! formula.
!
!                   TR_OUT = (C1 * FUNCTION(TR + C2)) + C3. 
!
! If HDR_C2 = 0, then the C2 parameter is active.
!</Help>
!
!<Help KEYWORD="C3">
!<Tip> Constant to add to the scaled function values. </Tip>
! Default = 0.0
! Allowed = real
! Constant to add to the scaled function values in the following formula.
!    
!                   TR_OUT = (C1 * FUNCTION(TR + C2)) + C3. 
!
! The C3 parameter is active only if HDR_C3 = 0. 
!</Help>
!
!<Help KEYWORD="HDR_C3">
!<Tip> Header word whose values are used for C3 in the formula. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! Header word whose values add to the scaled function values in the following 
! formula.
!
!                   TR_OUT = (C1 * FUNCTION(TR + C2)) + C3. 
!
! If HDR_C3 = 0, then the C3 parameter is active.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!! 
!!--------------------------- start of module ------------------------------!! 
!!--------------------------- start of module ------------------------------!! 


module scale_module 
  !
  use pc_module 
  !
  use getlun_module
  !
  use named_constants_module 
  !
  use lav_module, only:  lav_set_hdr
  !
  use string_module, only: string_to_upper
  !
  implicit none 
  private 
  public :: scale_create     ! uses the parameter cache. 
  public :: scale_initialize 
  public :: scale_update     ! uses the parameter cache. 
  public :: scale_delete 
!<execute_only> 
  public :: scale           ! main execution (trace processing) routine. 
  public :: scale_wrapup 
!</execute_only> 
  !
  ! - RCS and object file tag
  !
  character (len = 100), public, save :: SCALE_IDENT = & 
    '$Id: scale.f90,v 1.10 2006/06/20 13:12:05 Menger prod sps $'

  !
  type,public :: scale_struct              
    !
    private 
    !
    logical              :: skip_wrapup       ! wrapup flag. 
    !
    ! - Globals
    !
    integer              :: nwih
    integer              :: ndpt
    !
    ! - Process parameters
    !
    character (len = 10) :: function
    integer              :: hdr_c1
    integer              :: hdr_c2
    integer              :: hdr_c3
    integer              :: hdr_flag
    double precision     :: c1
    double precision     :: c2
    double precision     :: c3
    !
  end type scale_struct 


  !!------------------------------- data -----------------------------------!! 
  !!------------------------------- data -----------------------------------!! 
  !!------------------------------- data -----------------------------------!! 

  double precision, parameter :: two_over_pi = 2.0d0 / pi

  integer, parameter :: FUNCTION_CNT = 9
  character (len = *), parameter, dimension (FUNCTION_CNT) :: FUNCTIONS    &
    = (/ 'IDENTITY  ', 'SQRT      ', 'SQUARE    ', 'RECIPROCAL',     &
         'ABS       ', 'EXP       ', 'LOG       ', 'COS       ',     &
         'TAN       ' /)

contains 


  !!----------------------------- create -----------------------------------!! 
  !!----------------------------- create -----------------------------------!! 
  !!----------------------------- create -----------------------------------!! 

  subroutine scale_create (obj) 
    !
    ! - Arguments
    !
    type(scale_struct),pointer :: obj       ! arguments 
    !
    ! - Begin scale_create
    !
    allocate (obj) 
    !
    call scale_initialize (obj) 
    ! 
  end subroutine scale_create 


  !!------------------------------- delete ---------------------------------!! 
  !!------------------------------- delete ---------------------------------!! 
  !!------------------------------- delete ---------------------------------!! 


  subroutine scale_delete (obj) 
    !
    ! - Arguments
    !
    type(scale_struct),pointer :: obj       ! arguments 
    !
    ! - Begin scale_delete
    !

!<execute_only> 
      call scale_wrapup (obj) 
!</execute_only> 

    deallocate(obj) 
    !
  end subroutine scale_delete 


  !!----------------------------- initialize -------------------------------!! 
  !!----------------------------- initialize -------------------------------!! 
  !!----------------------------- initialize -------------------------------!! 

  subroutine scale_initialize (obj) 
    !
    ! - Arguments
    !
    type(scale_struct),intent(inout) :: obj       ! arguments 
    !
    ! - Begin scale_initialize
    !
    obj%c1         = 1.0d0
    obj%c2         = 0.0d0
    obj%c3         = 0.0d0
    obj%function   = 'IDENTITY'
    obj%hdr_c1     = 0
    obj%hdr_c2     = 0
    obj%hdr_c3     = 0
    obj%hdr_flag   = 0
    obj%skip_wrapup = .true.
    obj%nwih       = 64
    obj%ndpt       = 0
    !
    call scale_update (obj) 
    !
  end subroutine scale_initialize 


!!------------------------- start of update --------------------------------!! 
!!------------------------- start of update --------------------------------!! 
!!------------------------- start of update --------------------------------!! 


  subroutine scale_update (obj) 
    !
    ! - Arguments
    !
    type(scale_struct),intent(inout), target :: obj             ! arguments 
    !
    ! - Local Variables
    !
    integer     :: nscratch,nstore
    !
    ! - Begin
    !
    obj%skip_wrapup = .true.    ! needed for the wrapup routine. 


    !!-------------------------- read parameters ---------------------------!! 
    !!-------------------------- read parameters ---------------------------!! 
    !!-------------------------- read parameters ---------------------------!! 

    call pc_get_global ('ndpt', obj%ndpt)  ! number of trace samples.
    call pc_get_global ('nwih', obj%nwih)  ! number of trace samples.
    ! 
    call pc_get ('C1',       obj%c1) 
    call pc_get ('C2',       obj%c2) 
    call pc_get ('C3',       obj%c3) 
    call pc_get ('HDR_C1',   obj%hdr_c1) 
    call pc_get ('HDR_C2',   obj%hdr_c2) 
    call pc_get ('HDR_C3',   obj%hdr_c3) 
    call pc_get ('HDR_FLAG', obj%hdr_flag) 
    call pc_get ('FUNCTION', obj%function) 
    call string_to_upper (obj%function)


    !!------------------------ verify parameters ---------------------------!!
    !!------------------------ verify parameters ---------------------------!!
    !!------------------------ verify parameters ---------------------------!!

    call scale_check_header ('HDR_C1',   0, obj%nwih, obj%hdr_c1) 
    call scale_check_header ('HDR_C2',   0, obj%nwih, obj%hdr_c2) 
    call scale_check_header ('HDR_C3',   0, obj%nwih, obj%hdr_c3) 
    call scale_check_header ('HDR_FLAG', 0, obj%nwih, obj%hdr_flag) 
    !
    select case (trim (obj%function))
      case ('IDENTITY')
      case ('SQRT')
      case ('SQUARE')
      case ('RECIPROCAL')
      case ('ABS')
      case ('EXP')
      case ('LOG')
      case ('COS')
      case ('TAN')
      case default
        call pc_error ("Scale: Unknown function " // trim (obj%function))
    end select


    !!--------------------- call processes internally ----------------------!! 
    !!--------------------- call processes internally ----------------------!! 
    !!--------------------- call processes internally ----------------------!! 


    !!----------------------- write parameters -----------------------------!! 
    !!----------------------- write parameters -----------------------------!! 
    !!----------------------- write parameters -----------------------------!! 


    call pc_put_global ('ndpt', obj%ndpt)  ! number of trace samples.
    call pc_put_global ('nwih', obj%nwih)  ! number of trace samples.
    ! 
    call pc_put ('C1',       obj%c1) 
    call pc_put ('C2',       obj%c2) 
    call pc_put ('C3',       obj%c3) 
    call pc_put ('HDR_C1',   obj%hdr_c1) 
    call pc_put ('HDR_C2',   obj%hdr_c2) 
    call pc_put ('HDR_C3',   obj%hdr_c3) 
    call pc_put ('HDR_FLAG', obj%hdr_flag) 
    !
    call pc_put_options_field ('FUNCTION', FUNCTIONS, FUNCTION_CNT)
    call pc_put ('FUNCTION', obj%function) 
    !
    call pc_put_sensitive_field_flag ('C1',  obj%hdr_c1 == 0)
    call pc_put_sensitive_field_flag ('C2',  obj%hdr_c2 == 0)
    call pc_put_sensitive_field_flag ('C3',  obj%hdr_c3 == 0)
    !
    call pc_put_control ('ntapes',       0)             ! default 0 
    call pc_put_control ('need_request', .false.)       ! default false 
    call pc_put_control ('need_label',   .false.)       ! default false 
    call pc_put_control ('twosets',      .false.)       ! default false 
    call pc_put_control ('nscratch',     nscratch)      ! default 0 
    call pc_put_control ('nstore',       nstore)        ! default 0 
    call pc_put_control ('iftd',         .false.)       ! default false 
    call pc_put_control ('ndisk',        0)             ! default 0 
    call pc_put_control ('setup_only',   .false.)       ! default .false. 


    !!---------------------- prepare for execution -------------------------!! 
    !!---------------------- prepare for execution -------------------------!! 
    !!---------------------- prepare for execution -------------------------!! 

!<execute_only> 

    if (pc_do_not_process_traces()) return 
    !
    obj%skip_wrapup = .false.

!</execute_only> 


    !!------------------------ finish update -------------------------------!! 
    !!------------------------ finish update -------------------------------!! 
    !!------------------------ finish update -------------------------------!! 

  end subroutine scale_update 


  !!------------------------------- traps ----------------------------------!! 
  !!------------------------------- traps ----------------------------------!! 
  !!------------------------------- traps ----------------------------------!! 


  !!-------------------------- scale_check_header --------------------------!!
  !!-------------------------- scale_check_header --------------------------!!
  !!-------------------------- scale_check_header --------------------------!!

  subroutine scale_check_header (keyword, base, nwih, val)
    !
    ! - Arguments
    !
    character (len = *), intent (in)    :: keyword
    integer,             intent (in)    :: base
    integer,             intent (in)    :: nwih
    integer,             intent (inout) :: val
    !
    ! - Begin 
    !
    if (val < base) then
      !
      call pc_error (msg1 = "Scale -- Header " // trim (keyword) // " is ",   &
                     var1 = val,                                              &
                     msg2 = "but must be greater than or equal to ",          &
                     var2 = base)
      val = base
    else if (val > nwih) then
      !
      call pc_error (msg1 = "Scale -- Header " // trim (keyword) // " is ",   &
                     var1 = val,                                              &
                     msg2 = "but must be less than or equal to ",             &
                     var2 = nwih)
      val = nwih
    end if
    !
  end subroutine scale_check_header


  !!--------------------------- main execution -----------------------------!! 
  !!--------------------------- main execution -----------------------------!! 
  !!--------------------------- main execution -----------------------------!! 


!<execute_only> 

  subroutine scale (obj, ntr, hd, tr) 
    !
    ! - Arguments
    !
    type (scale_struct), intent (inout) :: obj                      ! arguments 
    integer,             intent (inout) :: ntr                      ! arguments 
    double precision,    intent (inout) :: hd (:, :)                ! arguments 
    real,                intent (inout) :: tr (:, :)                ! arguments 
    !
    ! - local variables
    !
    double precision :: tr1 (obj%ndpt) 
    double precision :: trx (obj%ndpt) 
    integer          :: s
    integer          :: t


    !
    ! - Begin scale
    !
    if (NTR == NO_MORE_TRACES) then
      call scale_wrapup (obj = obj)
      return
    else if (NTR == FATAL_ERROR) then
      return
    else if (ntr < 1) then
      call pc_error (msg1 = "Scale: Received unexpected value of ntr, ",    &
                     var1 = ntr)
      ntr = FATAL_ERROR
      return
    end if
    ! 
    ! - Process the traces
    !
  Loop_thru_traces:    &
    do t = 1, ntr
      !
      if (obj%hdr_flag > 0) then
        if (hd (obj%hdr_flag, t) == 0.0d0) cycle Loop_thru_traces
      else if (hd (HDR_LAV, t) <= 0.0d0) then     ! Skip dead traces
        cycle Loop_thru_traces
      end if
      !
      tr1 = dble (tr (1:obj%ndpt, t))
      !
      if (obj%hdr_c2 > 0) then
        tr1 = tr1 + hd (obj%hdr_c2, t)
      else
        tr1 = tr1 + obj%c2
      end if
      !
      !
      select case (obj%function)
        case ('IDENTITY  ')     ! No operation...
        case ('SQRT      ') 
          where (tr1 > 0.0)
            tr1 = sqrt (tr1)
          else where
            tr1 = 0.0d0
          end where
        case ('SQUARE    ') ; tr1 = tr1 ** 2
        case ('RECIPROCAL') ; where (tr1 /= 0.0d0) tr1 = 1.0d0 / tr1
        case ('ABS       ') ; tr1 = abs (tr1)
        case ('EXP       ') ; tr1 = exp (tr1)
        case ('LOG       ')
          where (tr1 > 0.0)
            tr1 = log (tr1)
          else where
            tr1 = 0.0
          end where
        case ('COS       ') ; tr1 = cos (tr1)
        case ('TAN       ')
          trx = mod (abs (tr1 * two_over_pi), 2.0d0)
          where (trx > 1.0d0) trx = 2.0d0 - trx
          do s = 1, obj%ndpt
            if (trx (s) > 0.9999d0) then
              tr1 (s) = 1.0d10
            else if (trx (s) > 1.0d-4) then
              tr1 (s) = tan (tr1 (s))
            else
              tr1 (s) = 0.0d0
            end if
          end do
        case default
      end select

      if (obj%hdr_c1 > 0) then
        tr1 = tr1 * hd (obj%hdr_c1, t)
      else
        tr1 = tr1 * obj%c1
      end if
      !
      if (obj%hdr_c3 > 0) then
        tr1 = tr1 + hd (obj%hdr_c3, t)
      else
        tr1 = tr1 + obj%c3
      end if
      !
      where (tr1 >  1.0d30) tr1 =  1.0d30
      where (tr1 < -1.0d30) tr1 = -1.0d30
      !
      tr (1:obj%ndpt, t) = real (tr1)
      !
    end do Loop_thru_traces
    !
    call lav_set_hdr (tr   = tr,    &
                      hd   = hd,    &
                      ntr  = ntr,   &
                      ndpt = obj%ndpt)
    ! 
  end subroutine scale

!</execute_only> 


  !!------------------------------- wrapup ---------------------------------!! 
  !!------------------------------- wrapup ---------------------------------!! 
  !!------------------------------- wrapup ---------------------------------!! 

!<execute_only> 

  subroutine scale_wrapup (obj) 
    !
    ! - Arguments
    !
    type(scale_struct),intent(inout) :: obj       ! arguments 
    !
    ! - Begin scale_wrapup
    !
    if (obj%skip_wrapup) return 
    obj%skip_wrapup = .true. 
    !
  end subroutine scale_wrapup 

!</execute_only> 

  !!---------------------------- end of module -----------------------------!! 
  !!---------------------------- end of module -----------------------------!! 
  !!---------------------------- end of module -----------------------------!! 


end module scale_module 


!!--------------------------------- end ------------------------------------!! 
!!--------------------------------- end ------------------------------------!! 
!!--------------------------------- end ------------------------------------!! 

