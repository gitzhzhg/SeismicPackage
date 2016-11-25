!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- zoeppritz.f90 ----------------------------!!
!!------------------------------- zoeppritz.f90 ----------------------------!!
!!------------------------------- zoeppritz.f90 ----------------------------!!


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
! Name       : zoeppritz 
! Category   : synthetics
! Written    : 2002-12-16 by: Douglas Hanson
! Revised    : 2007-01-03 by: Douglas Hanson Remove print staement.
! Maturity   : production
! Purpose    : Compute refl. attributes with Zoeppritz eqns or approximations
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! Given velocities and densities at an interface and the incidence angle and the
! nature (p, sv, sh) of a seismic wave, this primitive can compute reflected/
! transmitted amplitudes, energy and phase with full Zoeppritz equations or 
! with approximations
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


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS        
!
! Below, "intent(in)"  means "this parameter is not modified by this primitive"
!        "intent(out)" means "this parameter is overwritten  by this primitive"
!
! real,             intent(in   ) :: inc_ang  ! incident angle in radians
! character(len=*), intent(in   ) :: inc_flag ! type of inc. wave (p, sv, sh)
! character(len=*), intent(in   ) :: att_flag ! type of attribute to compute
! ( a, p, r, i, e, c, s, k - for:  amplitude, phase, real, imag, energy, 
! complex, shuey amplitudes, aki_richards amplitudes ) 
!
! real,             intent(in   ) :: dn1  ! incident  layer density
! real,             intent(in   ) :: vp1  ! incident  layer p velocity 
! real,             intent(in   ) :: vs1  ! incident  layer s velocity
! real,             intent(in   ) :: dn2  ! refracted layer density
! real,             intent(in   ) :: vp2  ! refracted layer p velocity 
! real,             intent(in   ) :: vs2  ! refracted layer s velocity
! real/complex,     intent(  out) :: rp   ! p wave reflection   coef
! real/complex,     intent(  out) :: rs   ! s wave reflection   coef
! real/complex,     intent(  out) :: tp   ! p wave transmission coef
! real/complex,     intent(  out) :: ts   ! s wave transmission coef
! integer,          intent(  out) :: i_err ! Error code. Explanation below:
!
!  0 No error
!  1 Invalid value argument to zoeppritz: inc_ang = +/- pi/2
!  2 Invalid value argument to zoeppritz: inc_flag /= p, sv, sh
!  3 Invalid value argument to zoeppritz: att_flag /= a, p, r, i, e, c, s, k
!  4 Invalid value argument to zoeppritz: dn1 <= 0
!  5 Invalid value argument to zoeppritz: vp1 <= 0
!  6 Invalid value argument to zoeppritz: vs1 <  0
!  7 Invalid value argument to zoeppritz: dn2 <= 0
!  8 Invalid value argument to zoeppritz: vp2 <= 0
!  9 Invalid value argument to zoeppritz: vs2 <  0
! 11 Input matrix to zoeppritz_lin_solve is singular
! 12 Incompatibility between arguments to zoeppritz: inc_flag=sh and vs1=0
! 13 Incompatibility between arguments to zoeppritz: inc_flag=sv and vs1=0
! 14 Incompatibility between arguments to zoeppritz: inc_flag=sv and att_flag=s
! 15 Incompatibility between arguments to zoeppritz: inc_flag=sv and att_flag=k
! 16 Incompatibility between arguments to zoeppritz: inc_flag=sh and att_flag=s
! 17 Incompatibility between arguments to zoeppritz: inc_flag=sh and att_flag=k
! 18 Incompatibility between arguments to zoeppritz: att_flag=c and 
!                                                    rp,rs,tp,ts are real
! 
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!  call zoeppritz ( inc_ang, inc_flag, att_flag, &
!                   dn1, vp1, vs1, dn1, vp2, vs2, rp, rs, tp, ts, i_err)
!
!  where rp, rs, tp and ts are either all real or all complex
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
! => Revision 4 (Vlad, Jul. 2005) changed the behavior of the code with respect
!    to output values for S waves in fluid media. Before, the code would output
!    zoeppritz_nil for S wave reflection/transmission coefficients. This was 
!    not physical - their physical value was actually zero. It was therefore 
!    changed to zero. For zero-amplitude events phase is set to zero. The code 
!    continues to return zoeppritz_nil for Rs and Ts in the Shuey/Aki & Richards
!    case, since these approximations do not cover S waves. 
!
! => For ATT_FLAG='A', the returned "amplitude" is defined as the real part
!    of the coefficient when the actual complex coefficient is real,
!    or the absolute value of the complex coefficient when it has a
!    non-zero imaginary part. (Thus, returned "amplitudes" may exhibit
!    strange, artificial, JUMPS at some angles: from a negative real
!    number to [positive] absolute value of a complex number.)
!
! => For ATT_FLAG='P', returned phase is in DEGREES. Incident angle however is 
!    always in radians.
!
! => When computed coefficients are complex, these are intended to be
!    applied to positive freqencies where Fourier sign convention uses
!    exp(+i*omega*t) in frequency-to-time transform.  Thus, positive
!    values of phase or imaginary part (returned when ATT_FLAG='P','I', or
!    'C') represent phase ADVANCE.
!
! => The Aki & Richards approximation is of the form:
!        Refl Coeff    =    A   +   BS * SIN**2   +   BT * TAN**2
!    where A, BS, BT are coefficients determined from layer parameters,
!    and for greatest accuracy, SIN and TAN should be sine and tangent
!    of the MEAN of incident and refracted angles (but this code simply
!    uses the incident angle). The Aki & Richards approximation
!    depends only on the difference between parameters of the upper and 
!    lower layers being small.
!
! => The Shuey approximation is a special case of the Aki & Richards
!    approximation when angles are small enough that there is essen-
!    tially no difference between sine and tangent [Ioan Vlad's note: Shuey's
!    1985 Geophysics paper stated that his approximation is reasonable under
!    30 degrees].  In that case: Refl Coeff =  A  +   B * SIN**2
!    where  B = BS + BT.  Thus, the Shuey approximation depends on
!    small angles as well as small difference between layer parameters.
!
! => This code was tested: (1) for compatibility with the old code in for all
!    combination of input parameters, (2) same for the condition of the sum of 
!    wave energy being one, (3) for common sense behavior in frequently 
!    encountered cases, (4) for coincidence with Fig. 7 in Geophysics 30, p. 565
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author         Description
!     ----        ------         -----------
!  5  2007-01-03  Douglas Hanson Remove print staement.
!  4  2005-08-16  Ioan Vlad      Rewrote for legibility, robustness
!  3  2004-01-13  Douglas Hanson Put in synthetics
!  2  2003-01-26  Douglas Hanson Use inc_ang rather than inc_sin
!  1  2002-12-16  Douglas Hanson Original F90 port from Baumel's code
!  0  1986        Bob Baumel     Original ZOPR program on Cyber
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
! This code is optimized for legibility and robustness, not speed. In the case 
! when we want the actual Zoeppritz equations, not the Shuey / Aki & Richards 
! approximations, we compute all quantities that can be possibly be computed: 
! amplitude, phase, energy, etc). For true performance, the code should be 
! rewritten to compute only the needed quantity and to use preprocessor 
! directives instead of tests of external parameters inside the program.
!-------------------------------------------------------------------------------
!</programming_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!
module zoeppritz_module
  !
  use named_constants_module, only: radians_per_degree
  use pc_module,              only: pc_info, pc_get_lun
  use string_module,          only: string_upper_compare
  !
  implicit  none
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  interface zoeppritz
     !
     module procedure zoeppritz_r
     module procedure zoeppritz_c
     !
  end interface
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !   
  private
  !
  public :: zoeppritz
  !
  ! This value is returned in an output value if that quantity
  ! cannot be computed / does not make physical sense:
  !
  real, public, parameter :: zoeppritz_nil = 1.e30
  !
  ! rcs identifier string
  !
  character(len=100), public, save :: zoeppritz_ident = &
       '$Id: zoeppritz.f90,v 1.5 2007/01/03 14:01:49 Hanson prod sps $'
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  type :: zoeppr_params_and_constants
     !
     ! Variables of this type should have intent(in) except in init
     !
     logical          :: medium1_is_fluid
     logical          :: medium2_is_fluid
     logical          :: incident_wave_is_P
     logical          :: incident_wave_is_SV
     logical          :: incident_wave_is_SH
     real             :: inc_sin       ! sin(inc_ang)
     real             :: dn1vp1        ! dn1*vp1
     real             :: dn1vs1        ! dn1*vs1
     real             :: dn1vs1sq      ! dn1*vs1*vs1
     real             :: dn2vp2        ! dn2*vp2
     real             :: dn2vs2        ! dn2*vs2
     real             :: dn2vs2sq      ! dn2*vs2*vs2
     !
  end type zoeppr_params_and_constants
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  type :: zoeppritz_internal_output
     ! Output for zoeppritz_compute_all. Does not have anything to do with
     ! the Shuey or Aki & Richards approximations
     !
     ! First letter in each name below shows what the physical quantity is,
     ! Second shows if it is reflected (r) or transmitted (t)
     ! Third shows if it belongs to a P or S wave
     !
     ! Under the critical angle arp/ars/atp/ats return the real 
     ! reflect coeff and after the critical angle, the ABSOLUTE VALUE of the 
     ! complex refl coeff ( wavelet suffers phase rotation )
     !
     real :: arp ! Amplitude of reflected   P wave
     real :: ars ! Amplitude of reflected   S wave
     real :: atp ! Amplitude of transmitted P wave
     real :: ats ! Amplitude of transmitted S wave
     !
     ! Phase rotation is zero under the critical angle
     !
     real :: prp ! Phase rotation of reflected   P wave, in degrees 
     real :: prs ! Phase rotation of reflected   S wave, in degrees 
     real :: ptp ! Phase rotation of transmitted P wave, in degrees 
     real :: pts ! Phase rotation of transmitted S wave, in degrees
     !
     ! Sum of these 4 values should always be 1.
     real :: erp ! Energy of reflected   P wave
     real :: ers ! Energy of reflected   S wave
     real :: etp ! Energy of transmitted P wave
     real :: ets ! Energy of transmitted S wave
     !
     ! For complex reflection coefficients (above critical angle,
     ! wavelet suffers phase rotation: 
     !
     real :: rrp ! Amplitude of reflected   P wave (real part)
     real :: rrs ! Amplitude of reflected   S wave (real part)
     real :: rtp ! Amplitude of transmitted P wave (real part)
     real :: rts ! Amplitude of transmitted S wave (real part)
     !
     real :: irp ! Amplitude of reflected   P wave (imag part)
     real :: irs ! Amplitude of reflected   S wave (imag part)
     real :: itp ! Amplitude of transmitted P wave (imag part)
     real :: its ! Amplitude of transmitted S wave (imag part)
     !
     complex :: crp ! Amplitude of reflected   P wave (complex)
     complex :: crs ! Amplitude of reflected   S wave (complex)
     complex :: ctp ! Amplitude of transmitted P wave (complex)
     complex :: cts ! Amplitude of transmitted S wave (complex)
     !
  end type zoeppritz_internal_output
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
contains
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  subroutine zoeppritz_c ( inc_ang, inc_flag, att_flag, &
       dn1, vp1, vs1, dn2, vp2, vs2, crp, crs, ctp, cts, i_err) 
    !
    real,             intent(in   ) :: inc_ang  ! incident angle in radians
    character(len=*), intent(in   ) :: inc_flag ! type of incident wave 
    !                                             (p, sv, sh)
    character(len=*), intent(in   ) :: att_flag ! type of attribute to compute
    !                                             , should be 'c'
    real,             intent(in   ) :: dn1      ! incident  layer density
    real,             intent(in   ) :: vp1      ! incident  layer p velocity 
    real,             intent(in   ) :: vs1      ! incident  layer s velocity
    real,             intent(in   ) :: dn2      ! refracted layer density
    real,             intent(in   ) :: vp2      ! refracted layer p velocity 
    real,             intent(in   ) :: vs2      ! refracted layer s velocity
    complex,          intent(  out) :: crp      ! p wave reflection   coef
    complex,          intent(  out) :: crs      ! s wave reflection   coef
    complex,          intent(  out) :: ctp      ! p wave transmission coef
    complex,          intent(  out) :: cts      ! s wave transmission coef
    !
    ! Dummy arguments crp, crs, ctp, cts are named differently than 
    ! rp, rs, tp and ts in the sister subroutine because, according to
    ! http://www.cs.rpi.edu/~szymansk/OOF90/bugs.html#10 , arguments that 
    ! distinguish between subroutines in an interface must be different (for 
    ! some compilers) both in type AND in dummy variable name
    !
    integer, intent(out)           :: i_err ! Error code. Explanation below:
    !
    !  0 No error
    !  1 Invalid value argument to zoeppritz: inc_ang = +/- pi/2
    !  2 Invalid value argument to zoeppritz: inc_flag /= p, sv, sh
    !  3 Invalid value argument to zoeppritz: att_flag /= a, p, r, i, e, s, k
    !  4 Invalid value argument to zoeppritz: dn1 <= 0
    !  5 Invalid value argument to zoeppritz: vp1 <= 0
    !  6 Invalid value argument to zoeppritz: vs1 <  0
    !  7 Invalid value argument to zoeppritz: dn2 <= 0
    !  8 Invalid value argument to zoeppritz: vp2 <= 0
    !  9 Invalid value argument to zoeppritz: vs2 <  0
    ! 11 Input matrix to zoeppritz_lin_solve is singular
    ! 12 Incompatibility between arguments to zoeppritz: inc_flag=sh and vs1=0
    ! 13 Incompatibility between arguments to zoeppritz: inc_flag=sv and vs1=0
    ! 14 Incompatibility between arguments to zoeppritz: inc_flag=sv and 
    !                                                    att_flag=s
    ! 15 Incompatibility between arguments to zoeppritz: inc_flag=sv and 
    !                                                    att_flag=k
    ! 16 Incompatibility between arguments to zoeppritz: inc_flag=sh and 
    !                                                    att_flag=s
    ! 17 Incompatibility between arguments to zoeppritz: inc_flag=sh and 
    !                                                    att_flag=k
    !
    ! Functions - LINKER GIVES ERROR MESSAGE IF THEY ARE DECLARED:
    ! type ( zoeppr_params_and_constants ) :: zoeppritz_init
    ! type ( zoeppritz_internal_output )   :: zoeppritz_compute_all
    !
    ! Local variables:
    type ( zoeppr_params_and_constants ) :: c ! internal params and consts
    type ( zoeppritz_internal_output )   :: o ! Since it is a structure, it
    ! will always be followed by the % sign and never mistaken for a zero (0) 
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Start by setting the error flag to "no error"
    i_err = 0
    !
    ! If att_flag is not 'c' when called with complex reflection/transmission
    ! coefficients, compute the complex ones anyway
    !
    if ( .not. string_upper_compare ( att_flag ( 1:1 ), 'c' ) ) &
         call pc_info ( &
         'Warning: in zoeppritz, att_flag should be c when coeffs are complex' )
    !
    ! Computed once, then used to pass values that do not change to procedures: 
    !
    c = zoeppritz_init ( &
    inc_ang, inc_flag, 'c', dn1, vp1, vs1, dn2, vp2, vs2, i_err )
    !
    if ( i_err /= 0 ) return ! fatal error, previous procedure printed message
    !
    ! Compute all that the Zoeppritz equations can give us:
    !
    o = zoeppritz_compute_all ( c, dn1, vp1, vs1, dn2, vp2, vs2, i_err )
    !
    if ( i_err /= 0 ) return ! fatal error, previous procedure printed message
    !
    crp = o%crp
    crs = o%crs
    ctp = o%ctp
    cts = o%cts
    !
    ! return
    !
  end subroutine zoeppritz_c
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  subroutine zoeppritz_r ( inc_ang, inc_flag, att_flag, &
       dn1, vp1, vs1, dn2, vp2, vs2, rp, rs, tp, ts, i_err) 
    !
    real,             intent(in   ) :: inc_ang  ! incident angle in radians
    character(len=*), intent(in   ) :: inc_flag ! type of incident wave 
    !                                             (p, sv, sh)
    character(len=*), intent(in   ) :: att_flag ! type of attribute to compute
    ! ( a,    p,     r,    i,    e,      s,                 k     
    ! ( ampl, phase, real, imag, energy, shuey amplitudes, aki_richards_ampl ) 
    !
    real,             intent(in   ) :: dn1  ! incident  layer density
    real,             intent(in   ) :: vp1  ! incident  layer p velocity 
    real,             intent(in   ) :: vs1  ! incident  layer s velocity
    real,             intent(in   ) :: dn2  ! refracted layer density
    real,             intent(in   ) :: vp2  ! refracted layer p velocity 
    real,             intent(in   ) :: vs2  ! refracted layer s velocity
    real,             intent(  out) :: rp   ! p wave reflection   coef
    real,             intent(  out) :: rs   ! s wave reflection   coef
    real,             intent(  out) :: tp   ! p wave transmission coef
    real,             intent(  out) :: ts   ! s wave transmission coef
    integer , intent(out):: i_err ! Error code. Explanation below:
    !
    !  0 No error
    !  1 Invalid value argument to zoeppritz: inc_ang = +/- pi/2
    !  2 Invalid value argument to zoeppritz: inc_flag /= p, sv, sh
    !  3 Invalid value argument to zoeppritz: att_flag /= a, p, r, i, e, s, k
    !  4 Invalid value argument to zoeppritz: dn1 <= 0
    !  5 Invalid value argument to zoeppritz: vp1 <= 0
    !  6 Invalid value argument to zoeppritz: vs1 <  0
    !  7 Invalid value argument to zoeppritz: dn2 <= 0
    !  8 Invalid value argument to zoeppritz: vp2 <= 0
    !  9 Invalid value argument to zoeppritz: vs2 <  0
    ! 11 Input matrix to zoeppritz_lin_solve is singular
    ! 12 Incompatibility between arguments to zoeppritz: inc_flag=sh and vs1=0
    ! 13 Incompatibility between arguments to zoeppritz: inc_flag=sv and vs1=0
    ! 14 Incompatibility between arguments to zoeppritz: inc_flag=sv and 
    !                                                    att_flag=s
    ! 15 Incompatibility between arguments to zoeppritz: inc_flag=sv and 
    !                                                    att_flag=k
    ! 16 Incompatibility between arguments to zoeppritz: inc_flag=sh and 
    !                                                    att_flag=s
    ! 17 Incompatibility between arguments to zoeppritz: inc_flag=sh and 
    !                                                    att_flag=k
    !
    ! Functions - LINKER GIVES ERROR MESSAGE IF THEY ARE DECLARED:
    ! type ( zoeppr_params_and_constants ) :: zoeppritz_init
    ! type ( zoeppritz_internal_output )   :: zoeppritz_compute_all
    !
    ! Local variables:
    type ( zoeppr_params_and_constants ) :: c ! internal params and consts
    type ( zoeppritz_internal_output )   :: o ! Since it is a structure, it
    ! will always be followed by the % sign and never mistaken for a zero (0) 
    !
    ! Local variables used only for the Shuey / Aki & Richards cases:
    real :: a
    real :: bs
    real :: bt
    real :: inc_sin2
    real :: vsum
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Start by setting the error flag to "no error"
    !
    i_err = 0
    !
    wrong_subroutine: if ( string_upper_compare ( att_flag ( 1:1 ), 'c' )) then 
       !
       i_err = 18
       !
       call pc_info ( &
            'Pass complex refl/transm coeffs to zoeppritz when att_flag=c' )
       !
    end if wrong_subroutine
    !
    ! Computed once, then used to pass values that do not change to procedures: 
    !
    c = zoeppritz_init ( &
         inc_ang, inc_flag, att_flag, dn1, vp1, vs1, dn2, vp2, vs2, i_err )
    !
    if ( i_err /= 0 ) return ! fatal error, previous procedure printed message
    !
    ! The following branch deals with Shuey or Aki & Richards:
    !
    p_wave_refl_approx: & 
    if ( string_upper_compare ( att_flag ( 1:1 ), 's' ) .OR. &
         string_upper_compare ( att_flag ( 1:1 ), 'k' ) ) then 
       !
       vsum = vp1 + vp2
       !
       a = ( c%dn2vp2 - c%dn1vp1 ) / ( c%dn2vp2 + c%dn1vp1 ) 
       !
       bs = 16. * ( c%dn1vs1sq - c%dn2vs2sq ) / ( ( dn1 + dn2 ) * vsum * vsum ) 
       !
       bt = ( vp2 - vp1 ) / vsum 
       !
       inc_sin2 = c%inc_sin * c%inc_sin
       !
       ! If we want the Shuey approximation:
       !
       if ( string_upper_compare ( att_flag ( 1:1 ), 's' ) ) & 
            rp = a + ( bs + bt ) * inc_sin2
       !
       ! If we want the Aki & Richards approximation:
       !
       if ( string_upper_compare ( att_flag ( 1:1 ), 'k' ) ) &
            rp = a + ( bs + bt / ( 1. - inc_sin2 ) ) * inc_sin2
       !
       tp = 1. - rp ! This is really true only at zero incidence
                    ! Should read Shuey / Aki&Richards to find out
                    ! What the transmission coefficient really is
       !
       rs = zoeppritz_nil ! Neither Shuey nor Aki & Richards approx 
       ts = zoeppritz_nil ! deal with S waves
       !
       return ! Done with Shuey/Aki & Richards, no more mention of them!
       !
    end if p_wave_refl_approx
    !
    ! Compute all that the Zoeppritz equations can give us:
    !
    o = zoeppritz_compute_all ( c, dn1, vp1, vs1, dn2, vp2, vs2, i_err )
    !
    if ( i_err /= 0 ) return ! fatal error, previous procedure printed message
    !
    ! Output only what we are interested in:
    !
    if ( string_upper_compare ( att_flag ( 1:1 ), 'a' ) ) then
       !
       ! We want the amplitude 
       !
       rp = o%arp
       rs = o%ars  
       tp = o%atp
       ts = o%ats  
       !
    else if ( string_upper_compare ( att_flag ( 1:1 ), 'p' ) ) then
       !
       ! We want the phase
       !
       rp = o%prp
       rs = o%prs  
       tp = o%ptp
       ts = o%pts  
       !
    else if ( string_upper_compare ( att_flag ( 1:1 ), 'e' ) ) then
       !
       ! We want the energy
       !
       rp = o%erp
       rs = o%ers
       tp = o%etp
       ts = o%ets
       !
    else if ( string_upper_compare ( att_flag ( 1:1 ), 'r' ) ) then  
       !
       ! We want the real part of a complex reflection coefficient
       !
       rp = o%rrp
       rs = o%rrs 
       tp = o%rtp
       ts = o%rts
       !
    else if ( string_upper_compare ( att_flag ( 1:1 ), 'i' ) ) then 
       !
       ! We want the imaginary part of a complex reflection coefficient
       !
       rp = o%irp
       rs = o%irs 
       tp = o%itp
       ts = o%its
       !
    else 
       !
       i_err = 3
       call pc_info ( &
       'Invalid value argument to zoeppritz: att_flag /= a, p, r, i, e, s, k' )
       return
       !
    end if
    !
    ! return
    !
  end subroutine zoeppritz_r
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Procedure to test input arguments and put them into an usable form:
  type ( zoeppr_params_and_constants ) function zoeppritz_init &
       ( inc_ang, inc_flag, att_flag, dn1, vp1, vs1, dn2, vp2, vs2, i_err )
    !
    real,             intent( in    ) :: inc_ang  ! incident angle in radians
    character(len=*), intent( in    ) :: inc_flag ! type of incident wave: 
    !                                    p, sv, sh
    character(len=*), intent( in    ) :: att_flag ! type of attribute to compute
    !    ( a,    p,     r,    i,    e,      c,       s,     k     
    ! for  ampl, phase, real, imag, energy, complex, shuey, aki&richards ) 
    real,             intent( in    ) :: dn1  ! incident  layer density
    real,             intent( in    ) :: vp1  ! incident  layer p velocity 
    real,             intent( in    ) :: vs1  ! incident  layer s velocity
    real,             intent( in    ) :: dn2  ! refracted layer density
    real,             intent( in    ) :: vp2  ! refracted layer p velocity 
    real,             intent( in    ) :: vs2  ! refracted layer s velocity
    integer,          intent( inout ) :: i_err
    integer,          save            :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! Print program version in the report file:
    !
    if ( i_call .eq. 1 ) &
    write(pc_get_lun(), '(  &
    & /, " zoeppritz " ,/ ," REVISION: ", &
    & " 5  2005-09-01  Douglas Hanson Remove print staement. " &
    & )')
    !
    ! Physically meaningful incident angles are between 0 and pi/2 .
    ! Those between -pi/2 and 0 deg give the same results - the
    ! sin function takes care of that. Those between pi/2 and 3pi/2 
    ! are meaningless (need to switch propagation mediums) and only God
    ! and Zoeppritz know what values they produce.
    !
    if ( inc_ang < 0 ) &
         call pc_info ( 'Warning: The argument to zoeppritz inc_ang is < 0' )
    !
    if ( inc_ang > asin (1.) ) &
       call pc_info ( 'Warning: The argument to zoeppritz inc_ang is > pi/2' )
    !
    zoeppritz_init%inc_sin = sin ( inc_ang )
    !
    ! Shuey approximation valid only up to 30 degrees
    if ( string_upper_compare ( att_flag ( 1:1 ), 's' ) .and. &
         zoeppritz_init%inc_sin > 0.5 ) &
         call pc_info ( &
         'Warning: Arg. inc_ang to zoeppritz is > pi/6; shuey approx not valid')
    !
    ! +/- pi/2 are singular values which will result in division by zero
    !
    if ( zoeppritz_init%inc_sin == -1 .OR. zoeppritz_init%inc_sin == 1)  then
       !
       i_err = 1
       call pc_info ( 'Invalid value arg. to zoeppritz: inc_ang = +/- pi/2' )
       return 
       !
    end if
    !
    ! avoiding real-to-real comparisons for fear of rounding error:
    !
    if ( zoeppritz_eq_test ( vs1, 0. )) then
       !
       zoeppritz_init%medium1_is_fluid = .true.
       !
    else
       !
       zoeppritz_init%medium1_is_fluid = .false.
       !
    end if
    !
    if ( zoeppritz_eq_test ( vs2, 0. )) then
       !
       zoeppritz_init%medium2_is_fluid = .true.
       !
    else
       !
       zoeppritz_init%medium2_is_fluid = .false.
       !
    end if
    !
    ! Get rid of case dependency in inc_flag. So that we can use case switches
    ! and we can keep in a single place these ugly-looking tests which may 
    ! change if anything else than string_upper_compare (in (1:1) , 'x') comes
    ! into use. 
    !
    test_inc_flag: if ( string_upper_compare ( inc_flag ( 1:1 ), 'p' ) ) then
       !
       ! Use an easy-to-read logical flag instead in the rest of the code
       !
       zoeppritz_init%incident_wave_is_P  = .true.
       zoeppritz_init%incident_wave_is_SV = .false.
       zoeppritz_init%incident_wave_is_SH = .false.
       !
    else if ( string_upper_compare ( inc_flag ( 1:2 ), 'sv') ) then
       !
       no_sv_waves_in_fluids: if ( zoeppritz_init%medium1_is_fluid ) then
          !
          i_err = 13
          call pc_info ( &
               'Incompatib. between args to zoeppritz: inc_flag=sv and vs1=0' )
          return ! Incident wave can only be P in a fluid medium
          !
       end if no_sv_waves_in_fluids
       !
       shuey_not_for_sv: if ( string_upper_compare ( att_flag ( 1:1 ), 's' ))&
            then
          !
          i_err = 14
          call pc_info ( &
          'Incompatib. between args to zoeppritz: inc_flag=sv and att_flag=s' )
          return ! The Shuey approximation is only for P waves
          !
       end if shuey_not_for_sv
       !
       akir_not_for_sv: if ( string_upper_compare ( att_flag ( 1:1 ), 'k' )) &
            then
          !
          i_err = 15
          call pc_info ( &
          'Incompatib. between args to zoeppritz: inc_flag=sv and att_flag=k' )
          return ! The Aki & Richards approximation is only for P waves
          !
       end if akir_not_for_sv
       !
       ! Use an easy-to-read logical flag instead in the rest of the code
       !
       zoeppritz_init%incident_wave_is_P  = .false.
       zoeppritz_init%incident_wave_is_SV = .true.
       zoeppritz_init%incident_wave_is_SH = .false.
       !
    else if ( string_upper_compare ( inc_flag ( 1:2 ), 'sh') ) then
       !
       no_sh_waves_in_fluids: if ( zoeppritz_init%medium1_is_fluid ) then
          !
          i_err = 12
          call pc_info ( &
               'Incompatib. between args to zoeppritz: inc_flag=sh and vs1=0' )
          return ! Incident wave can only be P in a fluid medium
          !
       end if no_sh_waves_in_fluids
       !
       shuey_not_for_sh: if ( string_upper_compare ( att_flag ( 1:1 ), 's' )) &
            then
          !
          i_err = 16
          call pc_info ( &
          'Incompatib. between args to zoeppritz: inc_flag=sh and att_flag=s' )
          return ! The Shuey approximation is only for P waves
          !
       end if shuey_not_for_sh
       !
       akir_not_for_sh: if ( string_upper_compare ( att_flag ( 1:1 ), 'k' ) ) &
            then
          !
          i_err = 17
          call pc_info ( &
          'Incompatib. between args to zoeppritz: inc_flag=sh and att_flag=k' )
          return ! The Aki & Richards approximation is only for P waves
          !
       end if akir_not_for_sh
       !
       ! Use an easy-to-read logical flag instead in the rest of the code
       !
       zoeppritz_init%incident_wave_is_P  = .false.
       zoeppritz_init%incident_wave_is_SV = .false.
       zoeppritz_init%incident_wave_is_SH = .true.
       !
    else ! if inc_flag is neither p, nor sv, nor sh
       !
       i_err = 2
       call pc_info ( &
            'Invalid value argument to zoeppritz: inc_flag /= p, sv, sh' )
       return 
       !
    end if test_inc_flag
    !
    ! Some more basic testing
    !
    if ( dn1 <= 0. ) then
       !
       i_err = 4
       call pc_info ( 'Invalid value argument to zoeppritz: dn1 <= 0' )
       return 
       !
    end if
    !
    if ( vp1 <= 0. ) then
       !
       i_err = 5
       call pc_info ( 'Invalid value argument to zoeppritz: vp1 <= 0' )
       return 
       !
    end if
    !
    if ( vs1 < 0. ) then
       !
       i_err = 6
       call pc_info ( 'Invalid value argument to zoeppritz: vs1 < 0' )
       return
       !
    end if
    !
    if ( dn2 <= 0. ) then
       !
       i_err = 7
       call pc_info ( 'Invalid value argument to zoeppritz: dn2 <= 0' )
       return
       !
    end if
    !
    if ( vp2 <= 0. ) then
       !
       i_err = 8
       call pc_info ( 'Invalid value argument to zoeppritz: vp2 <= 0' )
       return
       !
    end if
    !
    if ( vs2 < 0. ) then
       !
       i_err = 9
       call pc_info ( 'Invalid value argument to zoeppritz: vs2 < 0' )
       return
       !
    end if
    !
    ! Quantities used often:
    !
    zoeppritz_init%dn1vp1   = dn1 * vp1
    zoeppritz_init%dn1vs1   = dn1 * vs1
    zoeppritz_init%dn1vs1sq = zoeppritz_init%dn1vs1 * vs1
    !
    zoeppritz_init%dn2vp2   = dn2 * vp2
    zoeppritz_init%dn2vs2   = dn2 * vs2
    zoeppritz_init%dn2vs2sq = zoeppritz_init%dn2vs2 * vs2
    !
    ! return
    !
  end function zoeppritz_init
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  function zoeppritz_compute_all ( c, dn1, vp1, vs1, dn2, vp2, vs2, i_err ) &
       result (r)
    !
    type ( zoeppritz_internal_output )               :: r
    type ( zoeppr_params_and_constants ), intent(in) :: c
    real,    intent( in    ) :: dn1      ! incident  layer density
    real,    intent( in    ) :: vp1      ! incident  layer p velocity 
    real,    intent( in    ) :: vs1      ! incident  layer s velocity
    real,    intent( in    ) :: dn2      ! refracted layer density
    real,    intent( in    ) :: vp2      ! refracted layer p velocity 
    real,    intent( in    ) :: vs2      ! refracted layer s velocity
    integer, intent( inout ) :: i_err     ! error flag/number
    !
    ! Functions - LINKER GIVES ERROR MESSAGE IF THEY ARE DECLARED:
    ! complex :: zoeppritz_sqrt_c
    ! complex :: zoeppritz_set_e 
    !
    ! Local variables:
    complex :: m1
    complex :: n1
    complex :: m2
    complex :: n2
    complex :: denom ! denominator of the refl/transm coeff above
    real    :: p     ! ray parameter
    real    :: efact ! energy correction factor
    complex :: e1  ( 4, 4 ) 
    complex :: e2  ( 4, 4 ) 
    integer :: insub ! selects between elements of eqn_sys_matrix used for
    !                  either P or SV incident waves (cases medium 1 not fluid)
    complex, dimension ( :, : ), allocatable :: eqn_sys_matrix 
    !                                           ( input to zoeppr_linear_solve )
    !
    ! Initialize outputs to "undefined":
    !
    r%arp = zoeppritz_nil
    r%ars = zoeppritz_nil
    r%atp = zoeppritz_nil
    r%ats = zoeppritz_nil
    r%prp = zoeppritz_nil
    r%prs = zoeppritz_nil
    r%ptp = zoeppritz_nil
    r%pts = zoeppritz_nil
    r%erp = zoeppritz_nil
    r%ers = zoeppritz_nil
    r%etp = zoeppritz_nil
    r%ets = zoeppritz_nil
    r%rrp = zoeppritz_nil
    r%rrs = zoeppritz_nil
    r%rtp = zoeppritz_nil
    r%rts = zoeppritz_nil
    r%irp = zoeppritz_nil
    r%irs = zoeppritz_nil
    r%itp = zoeppritz_nil
    r%its = zoeppritz_nil
    r%crp = zoeppritz_nil
    r%crs = zoeppritz_nil
    r%ctp = zoeppritz_nil
    r%cts = zoeppritz_nil
    !
    compute_ray_parameter: if ( c%incident_wave_is_P ) then
       !
       p = c%inc_sin / vp1
       !
    else ! if incident wave is SV or SH
       !
       p = c%inc_sin / vs1
       !
    end if compute_ray_parameter
    !
    m1 = zoeppritz_sqrt_c ( 1. - ( vp1*p ) **2 ) 
    n1 = zoeppritz_sqrt_c ( 1. - ( vs1*p ) **2 ) 
    m2 = zoeppritz_sqrt_c ( 1. - ( vp2*p ) **2 ) 
    n2 = zoeppritz_sqrt_c ( 1. - ( vs2*p ) **2 ) 
    !
    fluid_combinations: if ( c%medium1_is_fluid .and. c%medium2_is_fluid ) then
       !
       denom = c%dn2vp2 * m1 + c%dn1vp1 * m2
       !
       denom_test_1 : if ( .not. ( zoeppritz_eq_test ( abs ( denom ), 0. ))) &
            then
          !
          ! set reflection coefficient, then compute the rest:
          !
          r%crp = ( c%dn2vp2 * m1 - c%dn1vp1 * m2 ) / denom
          call zoeppritz_set_attribute &
               ( r%crp, r%rrp, r%irp, r%erp, r%arp, r%prp )
          if ( i_err /= 0 ) return ! fatal error, previous proc. printed message
          !
          ! set transmission coefficient, then compute the rest:
          r%ctp = 2. * c%dn1vp1 * m1 /denom
          call zoeppritz_set_attribute &
               ( r%ctp, r%rtp, r%itp, r%etp, r%atp, r%ptp )
          if ( i_err /= 0 ) return ! fatal error, previous proc. printed message
          !
          ! No S waves in fluids: S wave amplitudes are zero, and phase 
          ! rotation is zero: 
          !
          r%ars = 0; r%ers = 0; r%rrs = 0; r%irs = 0; r%crs = 0; r%prs = 0;
          r%ats = 0; r%ets = 0; r%rts = 0; r%its = 0; r%cts = 0; r%pts = 0;
          !
       end if denom_test_1
       !
       ! Energy correction:
       !
       efact = c%dn1vp1 * real ( m1 ) 
       !
       efact_test: if ( efact > 0 ) then
          !
          r%erp = r%erp * c%dn1vp1 * real (m1) / efact
          r%etp = r%etp * c%dn2vp2 * real (m2) / efact
          !
       else efact_test
          !
          r%erp = zoeppritz_nil ! WHY?? 
          r%etp = zoeppritz_nil ! WHY??
          !
       end if efact_test
       !
       ! END all fluid case
       !
    else if ( c%medium1_is_fluid .and. .not. c%medium2_is_fluid ) then
       !
       e2 = zoeppritz_set_e ( p, dn2, vp2, vs2, c%dn2vs2sq, m2, n2 )
       !
       allocate ( eqn_sys_matrix ( 3, 4 ))
       !
       eqn_sys_matrix ( 1, 1 ) = m1
       eqn_sys_matrix ( 2, 1 ) = c%dn1vp1 
       eqn_sys_matrix ( 3, 1 ) = 0 
       eqn_sys_matrix ( 1, 2 ) = e2 ( 2, 3 ) 
       eqn_sys_matrix ( 2, 2 ) = e2 ( 4, 3 ) 
       eqn_sys_matrix ( 3, 2 ) = e2 ( 3, 3 ) 
       eqn_sys_matrix ( 1, 3 ) = e2 ( 2, 4 ) 
       eqn_sys_matrix ( 2, 3 ) = e2 ( 4, 4 ) 
       eqn_sys_matrix ( 3, 3 ) = e2 ( 3, 4 ) 
       eqn_sys_matrix ( 1, 4 ) = m1
       eqn_sys_matrix ( 2, 4 ) = -c%dn1vp1 
       eqn_sys_matrix ( 3, 4 ) = 0
       !
       call zoeppritz_lin_solve ( eqn_sys_matrix, i_err )
       !
       ! fatal error, previous procedure printed message
       !
       if ( i_err /= 0 ) return 
       !
       r%crp =  eqn_sys_matrix ( 1, 4 )
       !
       call zoeppritz_set_attribute ( r%crp, r%rrp, r%irp, r%erp, r%arp, r%prp )
       !
       ! fatal error, previous procedure printed message
       !
       if ( i_err /= 0 ) return 
       !
       ! when medium 1 is fluid and medium 2 is not, we do not have reflected
       ! S waves (*rs). In the original subroutine they remain as initialized 
       ! (zoeppritz_nil) but I set them to zero because this is what they 
       ! physically are (zoeppritz_nil is 10**30!). Phase rotation is zero.
       !
       r%crs = 0; r%ars = 0; r%ers = 0; r%rrs = 0; r%irs = 0; r%prs = 0;
       !
       r%ctp = eqn_sys_matrix ( 2, 4 )
       !
       call zoeppritz_set_attribute ( r%ctp, r%rtp, r%itp, r%etp, r%atp, r%ptp )
       !
       ! fatal error, previous procedure printed message
       !
       if ( i_err /= 0 ) return 
       !
       r%cts = eqn_sys_matrix ( 3, 4 )
       !
       call zoeppritz_set_attribute ( r%cts, r%rts, r%its, r%ets, r%ats, r%pts )
       !
       ! fatal error, previous procedure printed message
       !
       if ( i_err /= 0 ) return 
       !
       deallocate ( eqn_sys_matrix )
       !
       ! Energy correction:
       !
       efact = c%dn1vp1 * real ( m1 ) 
       !
       efact_test_2: if ( efact > 0 ) then
          !
          r%erp = r%erp * c%dn1vp1 * real (m1) / efact
          r%etp = r%etp * c%dn2vp2 * real (m2) / efact
          r%ets = r%ets * c%dn2vs2 * real (n2) / efact
          !
       else efact_test_2
          !
          r%erp = zoeppritz_nil 
          r%etp = zoeppritz_nil 
          r%ets = zoeppritz_nil
          !
       end if efact_test_2
       !
       ! END medium 1 fluid, medium 2 not fluid case 
       !
    else if ( .not. c%medium1_is_fluid .and. c%medium2_is_fluid ) then
       !
       inc_wave_test_1: if ( c%incident_wave_is_SH ) then
          !
          ! Nothing gets transmitted into fluid when incident wave is SH:
          r%ctp = 0; r%atp = 0; r%etp = 0; r%rtp = 0; r%itp = 0; r%ptp = 0;
          r%cts = 0; r%ats = 0; r%ets = 0; r%rts = 0; r%its = 0; r%pts = 0;
          !
          ! The reflection coefficient is 1 for S waves:
          !
          r%crs = 1.
          call zoeppritz_set_attribute &
               ( r%crs, r%rrs, r%irs, r%ers, r%ars, r%prs )
          if ( i_err /= 0 ) return ! fatal error, previous proc. printed message
          !
          ! No reflected P waves either:
          !
          r%crp = 0; r%arp = 0; r%erp = 0; r%rrp = 0; r%irp = 0; r%prp = 0
          !
          ! Energy correction:
          !
          efact = c%dn1vs1 * real ( n1 )
          !
          efact_test_3: if ( efact > 0 ) then
             !
             r%ers = r%ers * c%dn1vs1 * real (n1) / efact
             !
       else efact_test_3
          !
          r%ers = zoeppritz_nil 
          !
       end if efact_test_3
       !
       else ! incident_wave is P or SV 
          !
          ! Remember, medium1 is NOT fluid, medium 2 IS fluid
          !
          ! The following 2 lines are the only difference between P and SV
          if ( c%incident_wave_is_P  ) insub = 3 
          if ( c%incident_wave_is_SV ) insub = 4 
          !
          e1 = zoeppritz_set_e ( p, dn1, vp1, vs1, c%dn1vs1sq, m1, n1 )
          !
          allocate ( eqn_sys_matrix ( 3, 4 ))
          !
          eqn_sys_matrix ( 1, 1 ) = e1 ( 2, 1 ) 
          eqn_sys_matrix ( 2, 1 ) = e1 ( 4, 1 ) 
          eqn_sys_matrix ( 3, 1 ) = e1 ( 3, 1 ) 
          eqn_sys_matrix ( 1, 2 ) = e1 ( 2, 2 ) 
          eqn_sys_matrix ( 2, 2 ) = e1 ( 4, 2 ) 
          eqn_sys_matrix ( 3, 2 ) = e1 ( 3, 2 ) 
          eqn_sys_matrix ( 1, 3 ) = -m2
          eqn_sys_matrix ( 2, 3 ) = c%dn2vp2 
          eqn_sys_matrix ( 3, 3 ) = 0  
          eqn_sys_matrix ( 1, 4 ) = -e1 ( 2, insub ) 
          eqn_sys_matrix ( 2, 4 ) = -e1 ( 4, insub ) 
          eqn_sys_matrix ( 3, 4 ) = -e1 ( 3, insub ) 
          !
          call zoeppritz_lin_solve ( eqn_sys_matrix, i_err )
          if ( i_err /= 0 ) return ! fatal error, previous proc. printed message
          !
          r%crp =  eqn_sys_matrix ( 1, 4 )
          call zoeppritz_set_attribute &
               ( r%crp, r%rrp, r%irp, r%erp, r%arp, r%prp )
          if ( i_err /= 0 ) return ! fatal error, previous proc. printed message
          !
          r%crs = eqn_sys_matrix ( 2, 4 )
          call zoeppritz_set_attribute &
               ( r%crs, r%rrs, r%irs, r%ers, r%ars, r%prs )
          if ( i_err /= 0 ) return ! fatal error, previous proc. printed message
          !
          r%ctp = eqn_sys_matrix ( 3, 4 )
          call zoeppritz_set_attribute &
               ( r%ctp, r%rtp, r%itp, r%etp, r%atp, r%ptp )
          if ( i_err /= 0 ) return ! fatal error, previous proc. printed message
          !
          deallocate ( eqn_sys_matrix )
          !
          ! No transmitted wave in medium 2 (fluid):
          !
          r%cts = 0; r%ats = 0; r%ets = 0; r%rts = 0; r%its = 0; r%pts = 0
          !
          ! Energy correction:
          !
          P_SV_energy_1: if ( c%incident_wave_is_P ) then
             !
             efact = c%dn1vp1 * real ( m1 ) ! P wave
             !
          else
             !
             efact = c%dn1vs1 * real ( n1 ) ! SV wave 
             ! 
           end if P_SV_energy_1
           !
           efact_test_4: if ( efact > 0 ) then
              !
              r%erp = r%erp * c%dn1vp1 * real (m1) / efact
              r%ers = r%ers * c%dn1vs1 * real (n1) / efact
              r%etp = r%etp * c%dn2vp2 * real (m2) / efact
              !
           else efact_test_4
              !
              r%erp = zoeppritz_nil 
              r%ers = zoeppritz_nil 
              r%etp = zoeppritz_nil 
              !
           end if efact_test_4
           !
        end if inc_wave_test_1 ! END P or SV subcase
        !
        ! END medium 1 not fluid, medium 2 fluid case 
        !
    else if ( .not. c%medium1_is_fluid .and. .not. c%medium2_is_fluid ) then
       !
       inc_wave_test_2: if ( c%incident_wave_is_SH ) then
          !
          ! remember, both mediums solid now
          !
          denom = c%dn1vs1 * n1 + c%dn2vs2 * n2 
          !
          denom_test_2 : if ( .not. ( zoeppritz_eq_test ( abs ( denom ), 0. )))&
               then
             !
             ! LINE BELOW used to be c%dn1vs1 * n1 - c%dn2vs2 * n2 in the 
             ! old code. Changed because the sum of all coefficients was not 1
             ! Unfortunately I do not have handy an independent verification
             ! for sh reflection/transmission coefficients 
             !
             r%crs = ( c%dn2vs2 * n2 - c%dn1vs1 * n1 ) / denom
             !
             call zoeppritz_set_attribute &
             ( r%crs, r%rrs, r%irs, r%ers, r%ars, r%prs )
             !
             if ( i_err /= 0 ) return ! fatal error, previous proc. printed mesg
             !
             r%cts = 2. * c%dn1vs1 * n1 / denom
             !
             call zoeppritz_set_attribute &
                  ( r%cts, r%rts, r%its, r%ets, r%ats, r%pts )
             !
             if ( i_err /= 0 ) return ! fatal error, previous proc. printed mesg
             !
             ! No P waves get reflected or generated by conversion 
             ! when incoming wave is SH:
             !
             r%crp = 0; r%arp = 0; r%erp = 0; r%rrp = 0; r%irp = 0; r%prp=0
             r%ctp = 0; r%atp = 0; r%etp = 0; r%rtp = 0; r%itp = 0; r%ptp=0
             !
             ! Energy correction:
             !
             efact = c%dn1vs1 * real ( n1 )
             !
             efact_test_5: if ( efact > 0 ) then
                !
                r%ers = r%ers * c%dn1vs1 * real (n1) / efact
                r%ets = r%ets * c%dn2vs2 * real (n2) / efact
                !
             else efact_test_5
                !
                r%ers = zoeppritz_nil  
                r%ets = zoeppritz_nil
                !
             end if efact_test_5
             !
          end if denom_test_2  ! End SH case
          !
       else ! incident wave is P or SV. Mediums 1 and 2 solid 
          !
          ! The following 2 lines are the only difference between P and SV
          if ( c%incident_wave_is_P  ) insub = 3 
          if ( c%incident_wave_is_SV ) insub = 4 
          !
          e1 = zoeppritz_set_e ( p, dn1, vp1, vs1, c%dn1vs1sq, m1, n1 )
          e2 = zoeppritz_set_e ( p, dn2, vp2, vs2, c%dn2vs2sq, m2, n2 )
          !
          allocate ( eqn_sys_matrix ( 4, 5 ))
          !
          eqn_sys_matrix ( :, 1 ) =  e1 ( :, 1 ) 
          eqn_sys_matrix ( :, 2 ) =  e1 ( :, 2 ) 
          eqn_sys_matrix ( :, 3 ) = -e2 ( :, 3 ) 
          eqn_sys_matrix ( :, 4 ) = -e2 ( :, 4 ) 
          eqn_sys_matrix ( :, 5 ) = -e1 ( :, insub ) 
          !
          call zoeppritz_lin_solve ( eqn_sys_matrix, i_err )
          if ( i_err /= 0 ) return ! fatal error, previous proc. printed message
          !
          r%crp = eqn_sys_matrix ( 1, 5 )
          call zoeppritz_set_attribute &
               ( r%crp, r%rrp, r%irp, r%erp, r%arp, r%prp )
          if ( i_err /= 0 ) return ! fatal error, previous proc. printed message
          !
          r%crs = eqn_sys_matrix ( 2, 5 )
          call zoeppritz_set_attribute &
               ( r%crs, r%rrs, r%irs, r%ers, r%ars, r%prs )
          if ( i_err /= 0 ) return ! fatal error, previous proc. printed message
          !
          r%ctp = eqn_sys_matrix ( 3, 5 )
          call zoeppritz_set_attribute &
               ( r%ctp, r%rtp, r%itp, r%etp, r%atp, r%ptp )
          if ( i_err /= 0 ) return ! fatal error, previous proc. printed message
          !
          r%cts = eqn_sys_matrix ( 4, 5 )
          call zoeppritz_set_attribute &
               ( r%cts, r%rts, r%its, r%ets, r%ats, r%pts )
          if ( i_err /= 0 ) return ! fatal error, previous proc. printed message
          !
          deallocate ( eqn_sys_matrix )
          !
          ! Energy correction:
          !
          P_SV_energy_2: if ( c%incident_wave_is_P ) then
             !
             efact = c%dn1vp1 * real ( m1 ) ! P wave
             !
          else
             !
             efact = c%dn1vs1 * real ( n1 ) ! SV wave 
             !
          end if P_SV_energy_2
          !
          efact_test_6: if ( efact > 0 ) then
             !
             r%erp = r%erp * c%dn1vp1 * real (m1) / efact
             r%ers = r%ers * c%dn1vs1 * real (n1) / efact
             r%etp = r%etp * c%dn2vp2 * real (m2) / efact
             r%ets = r%ets * c%dn2vs2 * real (n2) / efact
             !
          else efact_test_6
             !
             r%erp = zoeppritz_nil 
             r%ers = zoeppritz_nil 
             r%etp = zoeppritz_nil 
             r%ets = zoeppritz_nil
             !
          end if efact_test_6
          !
       end if inc_wave_test_2 ! END P or SV subcase
       !
       ! END both mediums solid case
       !
    end if fluid_combinations
    !
    ! return
    !
  end function zoeppritz_compute_all
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  subroutine zoeppritz_set_attribute ( cz, rz, iz, ez, az, pz )
    !
    ! Given a reflection or transmission coefficient (cz), 
    ! compute its real (rz) and imaginary (iz) part, energy (ez), 
    ! amplitude of reflection (az) and phase (pz)
    !
    complex, intent ( in    ) :: cz
    real,    intent (   out ) :: rz
    real,    intent (   out ) :: iz
    real,    intent (   out ) :: ez
    real,    intent (   out ) :: az
    real,    intent (   out ) :: pz
    !
    ! Local variable
    real :: acz
    !
    rz = real  ( cz )
    iz = aimag ( cz )
    acz= abs   ( cz )
    ez = acz **2
    !
    set_amplitudes: if ( zoeppritz_eq_test ( iz, 0. )) then
       !
       az = rz
       !
    else ! If refl/transm coeff is complex 
       !
       az = acz  
       !
    end if set_amplitudes
    !
    set_phase: if ( .not. ( zoeppritz_eq_test ( iz, 0. ))) then
       !
       plus_minus_half_pi: if ( zoeppritz_eq_test ( rz, 0. )) then
          !
          if ( iz > 0  ) then
             !
             pz = asin ( 1. ) ! pi/2
             !
          else !  iz < 0 
             !
             pz = asin ( -1. ) ! -pi/2
             !
          end if
          !
       else plus_minus_half_pi ! rz /= 0 and iz /= 0 
          !
          pz = radians_per_degree * atan2 ( iz , rz ) 
          !
       end if plus_minus_half_pi
       !
    else set_phase ! if imag(cz) = 0
       !
       pz = 0 
       !
    end if set_phase
    !
    ! return
    !
  end subroutine zoeppritz_set_attribute
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  function zoeppritz_set_e ( p, dn, vp, vs, dnvssq, m, n) result ( e ) 
    !
    real,    intent( in ) :: p ! ray parameter
    real,    intent( in ) :: dn, vp, vs, dnvssq
    complex, intent( in ) :: m
    complex, intent( in ) :: n
    complex               :: e ( 4 , 4 ) ! output
    !
    complex               :: twom
    complex               :: twon
    real                  :: t
    real                  :: ta
    real                  :: tb
    real                  :: pa
    real                  :: pb
    real                  :: twopmu 
    !
    twopmu = 2. * p * dnvssq
    twom   = twopmu * m
    twon   = twopmu * n
    t      = dn - twopmu * p
    !
    ta = t * vp 
    tb = t * vs 
    pa = p * vp 
    pb = p * vs
    !
    e ( 1, 1 ) = pa 
    e ( 2, 1 ) = -m
    e ( 3, 1 ) = twom
    e ( 4, 1 ) = -ta
    e ( 1, 2 ) = n
    e ( 2, 2 ) = pb 
    e ( 3, 2 ) = tb 
    e ( 4, 2 ) = twon 
    e ( 1, 3 ) = pa 
    e ( 2, 3 ) = m
    e ( 3, 3 ) = -twom
    e ( 4, 3 ) = -ta
    e ( 1, 4 ) = n
    e ( 2, 4 ) = -pb
    e ( 3, 4 ) = -tb
    e ( 4, 4 ) = twon 
    !
    ! return
    !
  end function zoeppritz_set_e
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  subroutine zoeppritz_lin_solve ( a, i_err ) 
    !
    !---------------------------------------------------------------------------
    !   solves set of linear equations  m*x = c, 
    !   where  the input matrix 'a' is   n x ( n+1 ) ,    and initially contains
    !   the coefficient matrix 'm' in its first  n  columns and the constant
    !   vector 'c' in its ( n+1 ) th column. 
    !   after execution,  the first n columns of 'a' are messed up,  and the
    !   ( n+1 ) th column of 'a' contains the solution vector 'x'. 
    !   The integer variable 'i_err' is set to 11 if the matrix 'm' is singular.
    !   I know, it's inelegant, but this whole subroutine should really be 
    !   replaced with a call to some Lapack procedure anyway.
    !---------------------------------------------------------------------------
    !
    complex, intent(inout)    :: a ( :, : )
    integer, intent ( inout ) :: i_err
    !
    ! local variables
    !
    integer                    :: n    ! size of equation system
    integer                    :: imax
    integer                    :: n1
    integer                    :: i1
    integer                    :: i
    integer                    :: j
    integer                    :: ii
    real                       :: amax
    real                       :: tmax
    complex                    :: temp 
    !
    n = size ( a, 1 )
    !
    n1 = n + 1
    !
    do_i1_1 : do i = 1 , n
       !
       i1 = i+1 
       !
       amax = abs ( a ( i, i ) ) 
       !
       imax = i 
       !
       do_ii_1 : do ii = i1 , n 
          !
          tmax = abs ( a ( ii, i ) ) 
          !
          tmax_test : if ( tmax .gt. amax ) then 
             !
             amax = tmax
             !
             imax = ii
             !
          end if tmax_test
          !
       end do do_ii_1
       !
       amax_test : if ( amax  .eq.  0. ) then
          !
          i_err = 11
          call pc_info ( 'Input matrix to zoeppritz_lin_solve is singular' )
          return 
          !
       end if amax_test
       !
       imax_test : if ( imax .ne. i ) then 
          !     
          do_j_1 : do j = i , n1 
             !
             temp = a ( i, j ) 
             !
             a ( i, j ) = a ( imax, j ) 
             !
             a ( imax, j ) = temp 
             !
          end do do_j_1
          !
       end if imax_test
       !
       temp = cmplx ( 1., 0. ) / a ( i, i ) 
       !
       do_j_2 : do j = i1 , n1 
          !
          a ( i, j ) = temp * a ( i, j ) 
          !
       end do do_j_2
       !
       do_ii_2 : do ii = 1 , n
          !
          ii_test : if ( ii  .ne. i ) then
             !
             temp = -a ( ii, i ) 
             !
             do_j_3 : do j = i1 , n1
                !
                a ( ii, j ) = a ( ii, j ) + temp*a ( i, j ) 
                !
             end do do_j_3
             !
          end if ii_test
          !
       end do do_ii_2
       !
    end do do_i1_1
    !
    ! return
    !
  end subroutine zoeppritz_lin_solve
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  complex function zoeppritz_sqrt_c ( x ) 
    !
    ! holdover from the old F77 code. This should be replaced by the overloaded
    ! intrinsic sqrt function, but that one has restrictions on the signs of 
    ! the arguments, which do not result in a decrease in code complexity
    !
    real, intent ( in ) :: x
    !
    if ( x .lt. 0. ) then
       !
       zoeppritz_sqrt_c = cmplx ( 0., -sqrt ( -x ) ) 
       !
    else 
       !
       zoeppritz_sqrt_c = sqrt ( x ) 
       !
    end if
    !
    ! return
    !
  end function zoeppritz_sqrt_c
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  logical function zoeppritz_eq_test ( x, y )
    !
    ! For dealing with rounding error when comparing two real numbers. 
    ! To create new operators, we have to put them in a separate module and 
    ! access them through an interface.
    !
    ! Really all comparisons between floats/complex numbers should be 
    ! performed this way. However, results of numerical computations are
    ! not very likely to be so close to each other. User-input special 
    ! values that are tested for in the program are however very prone
    ! to misrepresentation (i.e. program called with Vs=0, vs gets passed
    ! as 0.9999999, test for vs==0 inside the code fails
    !
    ! a good example/tool is 
    ! http://cimss.ssec.wisc.edu/~paulv/Fortran90/Utility/ ...
    ! (URL continued:) Introduction.html#COMPARE_FLOAT_NUMBERS
    !
    real, intent(in) :: x
    real, intent(in) :: y
    !
    if ( abs ( x - y ) < ( 5 * spacing ( max ( abs(x), abs(y) )))) then
       !
       zoeppritz_eq_test = .true.
       !
    else
       !
       zoeppritz_eq_test = .false.
       !
    end if
    !
    ! return
    !
  end function zoeppritz_eq_test
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module zoeppritz_module

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
