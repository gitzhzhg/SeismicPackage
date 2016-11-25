!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ stkscale.f90 ------------------------------!!
!!------------------------------ stkscale.f90 ------------------------------!!
!!------------------------------ stkscale.f90 ------------------------------!!

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
! Name       : STKSCALE
! Category   : math
! Written    : 2000-04-24   by: Bob Baumel
! Revised    : 2000-12-11   by: Bob Baumel
! Maturity   : production   2001-02-13
! Purpose    : Stack scaling based on time varying and total fold.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This routine scales stacked traces according to the following 3 parameters:

! FSE: Fold of stack exponential method for scaling stacked traces.
! Allowed = 0.0-1.0
! The Fold of Stack exponential method scales stacked traces by dividing trace
! samples by the total fold raised to the FSE power.
! The FSE method is time-independent.  FSE = 0.0 does no scaling.
!
! MSCL: Mute Scaling method for scaling stacked traces.
! Allowed = 0.0-1.0
! The Mute Scaling method scales stacked traces by multiplying by a complicated
! empirical formula (with no obvious justification) based on maximum fold and
! local time-dependent fold.  Mute scaling is intended to compensate for loss
! of fold due to muting.  MSCL = 0.0 does no scaling.
!
!      Scale(MSCL) = 1.0 + MSCL*(((total fold)/(running fold)) - 1.0)
!
! TVFSE: Time Varying Fold of Stack Exponential method.
! Allowed = 0.0-1.0
! The Time Varying Fold of Stack Exponential method scales stacked traces by
! dividing trace samples by the time varying number of live samples stacked
! together raised to the TVFSE power.  TVFSE = 0.0 does no scaling.  This is an
! industry-standard method.
! If TVFSE > 0.0, then the FSE and MSCL methods are disabled.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS
!
! A single stacked trace is input to STKSCALE in the STACK argument.
! The scaled stacked trace is returned in the same argument.
!
!-------------------------------------------------------------------------------
!</trace_io_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS
!
! A value for NDPT global must be given to STKSCALE as one of its arguments.
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS
!
! A single trace header is supplied to STKSCALE in the HD argument.
! The modified header is returned in the same argument.
! Only the following three header words are modified:
!
!   Word #  Description      Reset to
!   ------  -----------      --------------------------------
!      2    HDR_TOP_MUTE     Index of first positive sample in FOLD array.
!      5    HDR_FOLD         NOMFOLD argument if present; else, largest value
!                            of running fold found in the FOLD array.
!     64    HDR_BOTTOM_MUTE  Index of last positive sample in FOLD array.
!
! Note: Header words 2 and 64 are both set to NDPT if the FOLD array contains
! no positive values.
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
!                            b    b      b          o
!     call  stkscale_check (fse, mscl, tvfse, fse_mscl_sense)
!
!                                                                opt
!                     b     b     i     i     i    i      i       i
!     call  stkscale (hd, stack, fold, ndpt, fse, mscl, tvfse, nomfold)
!
!
! real                fse = FSE coefficient (See GENERAL description above).
! real               mscl = MSCL coefficient (See GENERAL description above).
! real              tvfse = TVFSE coefficient (See GENERAL description above).
! logical  fse_mscl_sense = Whether to set FSE and MSCL sensitive in GUI.
! double precision  hd(:) = Array containing a header for the stacked trace.
! real           stack(:) = Array containing a single stacked trace.
! real            fold(:) = Array containing running fold of stacked trace.
! integer            ndpt = NDPT global for the stacked trace.
! integer, opt    nomfold = Nominal total fold of the stacked trace.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! STKSCALE_CHECK is intended to be called from your update routine, to verify
! parameters in the GUI. This routine projects all three coefficients (FSE,
! MSCL, and TVFSE) into the range 0.0-1.0. Then, if TVFSE > 0.0, it resets FSE
! and MSCL to 0.0 and sets FSE_MSCL_SENSE = .false.; if TVFSE = 0.0, it sets
! FSE_MSCL_SENSE = .true.  Example of use:
!      logical fse_mscl_sense
!      call stkscale_check (obj%fse, obj%mscl, obj%tvfse, fse_mscl_sense)
!      call pc_put_sensitive_field_flag  ('FSE'  , fse_mscl_sense)
!      call pc_put_sensitive_field_flag  ('MSCL' , fse_mscl_sense)
! STKSCALE_CHECK does NOT call the parameter cache.
!
! STKSCALE does the actual stack scaling (one trace at a time). The HD and
! STACK arrays are used for both input and output. You must supply running
! fold in the FOLD array. (Note that FOLD is a real--not integer--array, so
! you may supply fractional fold values, whether or not that is meaningful!)
! STKSCALE assumes that your STACK array is zero at all samples where your FOLD
! array is zero (STKSCALE doesn't check this!). You may optionally specify a
! nominal total fold (e.g., number of live traces that went into the stack) by
! using the NOMFOLD parameter. If you specify NOMFOLD, it will be used as total
! fold for FSE and MSCL scaling and will be set in header word 5. If you omit
! NOMFOLD, the maximum value in your FOLD array will be used for these purposes.
! (It may be reasonable to omit NOMFOLD if, e.g., you are stacking along a
! diagonal swath of traces [as in process AVAST], in which case the number of
! live traces isn't a good measure of total fold.)  STKSCALE assumes that the
! values of FSE, MSCL, and TVFSE have already been checked by STKSCALE_CHECK
! but, even if they haven't, STKSCALE will ignore FSE and MSCL in the event
! that TVFSE > 0.0.  STKSCALE does no scaling at all if your FOLD array contains
! no live samples or if you specify NOMFOLD and set it zero (or negative).
! STKSCALE resets only three words in the HD array (2, 5, and 64, as explained
! above in HEADER WORDS doc). You are still responsible for setting any other
! required header words (sequence words, LAV word, etc.) before traces leave
! your process.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  2. 2001-02-13  Bob Baumel   Doc-only: Fix XML tag to work in DOC program.
!  1. 2000-04-24  Bob Baumel   Initial version.
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

      module stkscale_module
      use named_constants_module
      implicit none

      private
      public :: stkscale_check
      public :: stkscale

      character(len=100),public,save :: STKSCALE_IDENT = &
'$Id: stkscale.f90,v 1.2 2001/02/12 19:56:29 sps prod sps $'

      contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

!!---------------------------- stkscale_check -----------------------------!!
!!---------------------------- stkscale_check -----------------------------!!
!!---------------------------- stkscale_check -----------------------------!!

      subroutine stkscale_check (fse, mscl, tvfse, fse_mscl_sense)
      implicit none
      real   ,intent(inout) :: fse                 ! arguments
      real   ,intent(inout) :: mscl                ! arguments
      real   ,intent(inout) :: tvfse               ! arguments
      logical,intent(out)   :: fse_mscl_sense      ! arguments

      tvfse = min (max(tvfse, 0.0) , 1.0)
      if (tvfse > 0.0) then
        fse  = 0.0
        mscl = 0.0
        fse_mscl_sense = .false.
      else
        fse   = min (max(fse , 0.0) , 1.0)
        mscl  = min (max(mscl, 0.0) , 1.0)
        fse_mscl_sense = .true.
      end if

      return
      end subroutine stkscale_check

!!------------------------------- stkscale --------------------------------!!
!!------------------------------- stkscale --------------------------------!!
!!------------------------------- stkscale --------------------------------!!

      subroutine stkscale (hd, stack, fold, ndpt, fse, mscl, tvfse, nomfold)
      implicit none
      double precision ,intent(inout)        :: hd (:)      ! arguments
      real             ,intent(inout)        :: stack(:)    ! arguments
      real             ,intent(in)           :: fold (:)    ! arguments
      integer          ,intent(in)           :: ndpt        ! arguments
      real             ,intent(in)           :: fse         ! arguments
      real             ,intent(in)           :: mscl        ! arguments
      real             ,intent(in)           :: tvfse       ! arguments
      integer          ,intent(in) ,optional :: nomfold     ! arguments

      integer :: i, ih2, ih64                      ! local
      real    :: maxfold, totalfold, factor        ! local
      real    :: rfold(ndpt)                       ! local

      ih2  = ndpt
      ih64 = ndpt
      maxfold = 0.0
      do i = 1, ndpt
        if (fold(i) > 0.0) then
          if (ih2 == ndpt) ih2 = i
          ih64 = i
          maxfold = max (maxfold, fold(i))
          rfold(i) = 1.0 / fold(i)
        else
          rfold(i) = 1.0
        end if
      end do

      if (present(nomfold)) then
        totalfold = nomfold
      else
        totalfold = maxfold
      end if
      if (totalfold<=0.0 .or. maxfold==0.0) goto 99

      if (tvfse == 1.0) then
        stack(ih2:ih64) = stack(ih2:ih64) * rfold(ih2:ih64)
      else if (tvfse > 0.0) then
        stack(ih2:ih64) = stack(ih2:ih64) * rfold(ih2:ih64)**tvfse
      else
        if (fse == 1.0) then
          factor = 1.0 / totalfold
        else if (fse > 0.0) then
          factor = 1.0 / totalfold**fse
        else
          factor = 1.0
        end if
        if (mscl > 0.0) then
          do i = ih2, ih64
            stack(i) = stack(i) * factor * (1.+ mscl*(totalfold*rfold(i)-1.))
          end do
        else
          stack(ih2:ih64) = stack(ih2:ih64) * factor
        end if
      end if

 99   hd(HDR_TOP_MUTE)    = ih2
      hd(HDR_FOLD)        = totalfold
      hd(HDR_BOTTOM_MUTE) = ih64

      return
      end subroutine stkscale

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module stkscale_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
