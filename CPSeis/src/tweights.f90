!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- tweights.f90 --------------------------------!!
!!---------------------------- tweights.f90 --------------------------------!!
!!---------------------------- tweights.f90 --------------------------------!!


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
! Name       : TWEIGHTS           (Taper Weights)
! Category   : math
! Written    : 2001-08-17   by: Tom Stoeckley
! Revised    : 2004-06-08   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Calculate a symmetric vector of taper weights.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive calculates a symmetric vector of weights which can be used
! to taper the ends of any other vector.  Examples might be to taper the
! edges of correlation functions, taper the weights of traces which are
! mixed together, or taper the top of a muted trace.  Several types of
! tapers are provided, such as linear and cosine tapers.
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
! To verify the parameters:
!                                                             opt      opt
!                                b                  b          b        b
!   call tweights_verify    (opt_taper,          nweights, npositive, nflat)
!
!
! To calculate the weights:
!                                                             opt      opt
!                                i         o        i          i        i
!   call tweights_calculate (opt_taper, weights, nweights, npositive, nflat)
!
!                                o        i    
!   call tweights_none       (weights, nweights)
!   call tweights_linear     (weights, nweights)
!   call tweights_cosine     (weights, nweights)
!   call tweights_hann       (weights, nweights)
!   call tweights_welch      (weights, nweights)
!
!
! character(len=*)     opt_taper  = taper option.
! real          weights(nweights) = array of taper weights.
! integer               nweights  = total number of weights.
! integer              npositive  = number of center weights greater than zero.
! integer                  nflat  = number of center weights equal to one.
!
! The default value for NPOSITIVE is NWEIGHTS.
! The default value for NFLAT is one.
! 
! Allowed taper options are listed below under EXAMPLES.
! 
!-------------------------------------------------------------------------------
!                           SUBROUTINE DETAILS
!
! TWEIGHTS_VERIFY:
!  (1) makes sure OPT_TAPER is valid.
!  (2) makes sure NWEIGHTS  is >=1 and odd.
!  (3) makes sure NPOSITIVE is >=1 and odd and <=NWEIGHTS.
!  (3) makes sure NFLAT     is >=1 and odd and <=NPOSITIVE.
!  (4) resets any invalid arguments to permitted values.
!
! TWEIGHTS_CALCULATE:
!  (1) calculates a symmetric vector of NWEIGHTS weights.
!  (2) the middle NFLAT weights will have a value of one.
!  (3) the middle NPOSITIVE weights will have positive values.
!  (4) the weights outside of the NPOSITIVE range will have values of zero.
!
! TWEIGHTS_NONE:
! TWEIGHTS_LINEAR:
! TWEIGHTS_COSINE:
! TWEIGHTS_HANN:
! TWEIGHTS_WELCH:
!  (1) calculates a symmetric vector of NWEIGHTS weights.
!  (2) the middle weight will have a value of one.
!  (3) all weights will have positive values.
!
!-------------------------------------------------------------------------------
!                               EXAMPLES
!
! Examples for NWEIGHTS = 9 and NPOSITIVE = 5 and NFLAT = 1:
!
!  OPT_TAPER = NONE:    WEIGHTS =  0  0  1.000  1.000  1  1.000  1.000  0  0
!  OPT_TAPER = LINEAR:  WEIGHTS =  0  0  0.333  0.667  1  0.667  0.333  0  0
!  OPT_TAPER = COSINE:  WEIGHTS =  0  0  0.500  0.866  1  0.866  0.500  0  0
!  OPT_TAPER = HANN:    WEIGHTS =  0  0  0.250  0.750  1  0.750  0.250  0  0
!  OPT_TAPER = WELCH:   WEIGHTS =  0  0  0.556  0.889  1  0.889  0.556  0  0
!
! Examples of the relationship between NWEIGHTS, NPOSITIVE, and NFLAT:
!
!  weight indices:        1    2    3    4    5    6    7    8    9
!  NWEIGHTS  = 9:         x    x    x    x    x    x    x    x    x
!  NPOSITIVE = 7:              x    x    x    x    x    x    x
!  NFLAT     = 3:                        x    x    x
!  weight values:        0.0  TTT  TTT  1.0  1.0  1.0  TTT  TTT  0.0
!
!                          (TTT = tapered values between 0 and 1)
!
!  The number of tapered values (half on each side) is NPOSITIVE - NFLAT.
!  All values will be zero or one (no taper) if NFLAT == NPOSITIVE.
!
!-------------------------------------------------------------------------------
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
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2004-06-08  Stoeckley  Add Hann and Welch filter options; make the
!                             NPOSITIVE argument optional; add the optional
!                             NFLAT argument; add subroutines which are
!                             specific to each filter option..
!  1. 2001-12-11  Stoeckley  Initial version, created from code removed
!                             from SDIP and SDIP3D.
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


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module tweights_module
      use mth_module
      use string_module
      implicit none
      public

      character(len=100),public,save :: TWEIGHTS_IDENT = &
'$Id: tweights.f90,v 1.2 2004/06/08 12:59:36 Stoeckley prod sps $'

      contains


!!------------------------------ tweights verify -----------------------------!!
!!------------------------------ tweights verify -----------------------------!!
!!------------------------------ tweights verify -----------------------------!!


      subroutine tweights_verify (opt_taper, nweights, npositive, nflat)

      character(len=*),intent(inout) :: opt_taper                  ! argument
      integer         ,intent(inout) :: nweights                   ! argument
      integer,optional,intent(inout) :: npositive                  ! argument
      integer,optional,intent(inout) :: nflat                      ! argument
      character(len=1)               :: upper                      ! local
      integer                        :: npositive2,nflat2          ! local

      upper = string_2_upper(opt_taper(1:1))

      select case (upper)
           case ('L')   ; opt_taper = 'LINEAR'
           case ('C')   ; opt_taper = 'COSINE'
           case ('N')   ; opt_taper = 'NONE'
           case ('H')   ; opt_taper = 'HANN'
           case ('W')   ; opt_taper = 'WELCH'
           case default ; opt_taper = 'NONE'
      end select

      npositive2 = nweights
      nflat2     = 1
      if (present(npositive)) npositive2 = npositive
      if (present(nflat    )) nflat2     = nflat

      call mth_constrain_odd (nweights  , 1,      99999)
      call mth_constrain_odd (npositive2, 1,   nweights)
      call mth_constrain_odd (nflat2    , 1, npositive2)

      if (present(npositive)) npositive = npositive2
      if (present(nflat    )) nflat     = nflat2

      end subroutine tweights_verify


!!---------------------------- tweights calculate ---------------------------!!
!!---------------------------- tweights calculate ---------------------------!!
!!---------------------------- tweights calculate ---------------------------!!


      subroutine tweights_calculate &
                        (opt_taper, weights, nweights, npositive, nflat)

      character(len=*),intent(in)  :: opt_taper                  ! argument
      real            ,intent(out) :: weights(:)                 ! argument
      integer         ,intent(in)  :: nweights                   ! argument
      integer,optional,intent(in)  :: npositive                  ! argument
      integer,optional,intent(in)  :: nflat                      ! argument
      integer                      :: istart,nw,iw,iw1,iw2       ! local
      integer                      :: npositive2,nflat2          ! local

      npositive2 = nweights
      nflat2     = 1
      if (present(npositive)) npositive2 = npositive
      if (present(nflat    )) nflat2     = nflat

      weights(1:nweights) = 0.0
      istart = 1 + (nweights - npositive2) / 2
      nw     = 1 + npositive2 - nflat2

      select case (opt_taper)
        case ('LINEAR') ; call tweights_linear   (weights(istart:),nw)
        case ('COSINE') ; call tweights_cosine   (weights(istart:),nw)
        case ('NONE')   ; call tweights_none     (weights(istart:),nw)
        case ('HANN')   ; call tweights_hann     (weights(istart:),nw)
        case ('WELCH')  ; call tweights_welch    (weights(istart:),nw)
        case default    ; call tweights_none     (weights(istart:),nw)
      end select

      if (nflat2 > 1) then
           iw1   = istart + nw/2
           iw2   = istart + nw/2 + nw/2
           do iw = iw2,iw1,-1
                weights(iw + nflat2 - 1) = weights(iw)
           end do
           iw1   = nweights/2 + 1 - nflat2/2
           iw2   = nweights/2 + 1 + nflat2/2
           do iw = iw1,iw2
                weights(iw) = 1.0
           end do
      end if

      end subroutine tweights_calculate


!!---------------------------- tweights linear ---------------------------!!
!!---------------------------- tweights linear ---------------------------!!
!!---------------------------- tweights linear ---------------------------!!


      subroutine tweights_linear (weights,nw)

      real            ,intent(out) :: weights(:)                 ! argument
      integer         ,intent(in)  :: nw                         ! argument
      integer                      :: nhalf,iw                   ! local
      real                         :: factor                     ! local

      nhalf  = nw / 2 + 1
      factor = 1.0 / nhalf
      do iw=1,nw
           weights(iw) = 1.0 - factor * abs(iw - nhalf)
      end do

      end subroutine tweights_linear


!!---------------------------- tweights cosine ---------------------------!!
!!---------------------------- tweights cosine ---------------------------!!
!!---------------------------- tweights cosine ---------------------------!!


      subroutine tweights_cosine (weights,nw)

      real            ,intent(out) :: weights(:)                 ! argument
      integer         ,intent(in)  :: nw                         ! argument
      integer                      :: nhalf,iw                   ! local
      real                         :: factor                     ! local

      nhalf  = nw / 2 + 1
      factor = 0.5 * PI / nhalf
      do iw=1,nw
           weights(iw) = cos(factor * (iw - nhalf))
      end do

      end subroutine tweights_cosine


!!---------------------------- tweights none ---------------------------!!
!!---------------------------- tweights none ---------------------------!!
!!---------------------------- tweights none ---------------------------!!


      subroutine tweights_none (weights,nw)

      real            ,intent(out) :: weights(:)                 ! argument
      integer         ,intent(in)  :: nw                         ! argument

      weights(1:nw) = 1.0

      end subroutine tweights_none


!!---------------------------- tweights hann ---------------------------!!
!!---------------------------- tweights hann ---------------------------!!
!!---------------------------- tweights hann ---------------------------!!


      subroutine tweights_hann (weights,nw)

      real            ,intent(out) :: weights(:)                 ! argument
      integer         ,intent(in)  :: nw                         ! argument
      integer                      :: nhalf,iw                   ! local
      real                         :: factor                     ! local

      nhalf  = nw / 2 + 1
      factor = PI / nhalf
      do iw=1,nw
           weights(iw) = 0.5 * (1.0 + cos(factor * (iw - nhalf)))
      end do

      end subroutine tweights_hann


!!---------------------------- tweights welch ---------------------------!!
!!---------------------------- tweights welch ---------------------------!!
!!---------------------------- tweights welch ---------------------------!!


      subroutine tweights_welch (weights,nw)

      real            ,intent(out) :: weights(:)                 ! argument
      integer         ,intent(in)  :: nw                         ! argument
      integer                      :: nhalf,iw                   ! local
      real                         :: factor,temp                ! local

      nhalf  = nw / 2 + 1
      factor = 1.0 / nhalf
      do iw=1,nw
           temp = factor * (iw - nhalf)
           weights(iw) = 1.0 - temp * temp
      end do

      end subroutine tweights_welch


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module tweights_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

