!<CPS_v1 type="PRIMITIVE"/>

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
! Name       : DYNSAMP       (DYNAMIC TRACE RESAMPLING)
! Category   : math
! Written    : 2003-10-02   by: Tom Stoeckley
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Dynamically adjust trace timings.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This primitive is used to dynamically resample an array with evenly spaced
! samples.  Such an array is commonly a seismic trace.  This primitive can
! be used for NMO, depth conversion, time-variant statics, and other dynamic
! adjustments on seismic traces.
!
! Two algorithms are available for interpolating between input samples.
! These are 2-point linear interpolation and 4-point cubic interpolation.
!
! The 4-point cubic interpolation algorithm used in this primitive uses
! two points on each side of the specified location to calculate the returned
! value.  Weights are assigned to these four points in such a way as to
! guarantee that the interpolated values and their first derivatives will
! be continuous over the entire length of the arrays.
!
! Points off the ends of the input array are assumed to be zero.
!
! This primitive uses the CUBETERP primitive.
!
! The 4-point cubic interpolation algorithm is used in several primitives.
! See the CUBETERP primitive for details.
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
!                             SUBROUTINES
!
!                               o    i       i  
!         call dynsamp_create (obj, ndpt, terpmode)
!         call dynsamp_delete (obj)
!                               b
!
!                                i   i   i  o    b      b
!         call dynsamp_forward (obj, TA, A, B, mtop, mbottom)
!         call dynsamp_reverse (obj, TB, A, B, mtop, mbottom)
!                                i   i   i  o    b      b
!
!-------------------------------------------------------------------------------
!                        SUBROUTINE ARGUMENTS
!
! type(dynsamp_struct)  obj = pointer to the DYNSAMP object.
! integer              ndpt = length of arrays A, B, TA, and TB.
! integer          terpmode = interpolation option.
! real             TA(ndpt) = array of exact indices on INPUT array which will
!                              move to OUTPUT array.
! real             TB(ndpt) = array of exact indices on OUTPUT array which will
!                              move from INPUT array.
! real              A(ndpt) = input array.
! real              B(ndpt) = output array.
! integer              mtop = top mute index.
! integer           mbottom = bottom mute index.
!
! TERPMODE = DYNSAMP_LINEAR means 2-point linear interpolation.
! TERPMODE = DYNSAMP_CUBIC  means 4-point cubic interpolation.
! TERPMODE = DYNSAMP_FFT2   means FFT densification plus linear interpolation.
! TERPMODE = DYNSAMP_FFT4   means FFT densification plus linear interpolation.
! TERPMODE = DYNSAMP_FFT8   means FFT densification plus linear interpolation.
!
! MTOP and MBOTTOM will be adjusted up or down by the same amount as the
! corresponding array sample moves.
!
! Time reversals are muted.
!
!-------------------------------------------------------------------------------
! Example of the meaning of the TA(NDPT) array:
!
!                      B(indx) = A(TA(indx))
!                      B(12)   = A(TA(12))
!                      B(12)   = A(17.42)
!
! If TA(12) is 17.42, this means that the value of B(12) should come
! from A(17.42).  Since 17.42 is not an integer, array A(NDPT) must be
! interpolated.  Linear interpolation uses A(17) and A(18) to calculate B(12).
! Cubic interpolation uses A(16), A(17), A(18), and A(19) to calculate B(12).
! Values of TA beyond the trace boundaries are OK.
!
!-------------------------------------------------------------------------------
! Example of the meaning of the TB(NDPT) array:
!
!                      B(TB(indx)) = A(indx)
!                      B(TB(12))   = A(12)
!                      B(17.42)    = A(12)
!
! If TB(12) is 17.42, this means that the value of A(12) should be
! moved to B(17.42).  Since 17.42 is not an integer, array A(NDPT) must be
! interpolated to give values which can be moved to integral indices
! of B, such as B(17) and B(18).  Array TB(NDPT) is used to calculate an
! array TA(NDPT) which will do this.
! Values of TB beyond the trace boundaries are OK.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                        COMPARISON WITH DYNCC
!
! DYNSAMP_FORWARD subroutine:
!
!  This subroutine is similar to DYNCC, except that the TB array is
!  eliminated and the TA array has the same length as the trace.
!  [The implied TB array contains indices such that TB(I) = I.]  This
!  subroutine should be faster than DYNCC, and should be more accurate
!  for NMO correction.  This subroutine is appropriate for forward
!  NMO correction, but can be used for any dynamic trace adjustment
!  involving stretching and/or compression.
!
! DYNSAMP_REVERSE subroutine:
!
!  This subroutine is similar to DYNCC, except that the TA array is
!  eliminated and the TB array has the same length as the trace.
!  [The implied TA array contains indices such that TA(I) = I.]  This
!  subroutine should be faster than DYNCC, and should be more accurate
!  for NMO correction.  This subroutine is appropriate for reverse
!  NMO correction, but can be used for any dynamic trace adjustment
!  involving stretching and/or compression.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  7. 2006-06-20  B. Menger    Removed Unused Variables.
!  6. 2005-01-31  Stoeckley    Fix bug whereby the moveout adjustment of the
!                               top mute index was improperly excessive when
!                               a time reversal mute was also applied.
!  5. 2004-05-03  Stoeckley    Remove improper time reversal mute for reverse
!                               sampling.
!  4. 2003-11-03  Stoeckley    Move all muting code (except time reversal
!                               muting) to the MOVEOUT primitive;
!                               remove arguments DOPPLER and TRACEMUTE;
!                               change arguments from optional to required.
!  3. 2003-10-27  Stoeckley    Fix doppler mute problems; add options for
!                               doing FFT resampling.
!  2. 2003-10-16  Stoeckley    Add optional arguments MTOP and MBOTTOM.
!  1. 2003-10-02  Stoeckley    Initial version, made from a subset of the
!                               DYNCC primitive.  See the DYNCC primitive
!                               for previous revision history.
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


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! When the primitive DYNCC was converted to Fortran-90, routine DYNCC_FORWARD
! was modified to make use of some new Fortran-90 constructs involving array
! DO loop notation, array allocation, and WHERE statements.  These changes
! more than doubled the execution time, and therefore were replaced by the
! original code and then moved from DYNCC to this new primitive.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module dynsamp_module
      use densify_module
      use cubeterp_module
      use named_constants_module
      use mth_module
      implicit none
      public
      private :: dynsamp_private_convert

      character(len=100),public :: dynsamp_ident = &
"$Id: dynsamp.f90,v 1.7 2006/06/20 13:11:51 Menger prod sps $"

      type,public :: dynsamp_struct
        private
        integer                      :: ndpt
        integer                      :: terpmode
        integer                      :: fineratio
        integer                      :: nfine
        type(densify_struct),pointer :: densify
      end type dynsamp_struct


      integer,parameter,public :: DYNSAMP_CUBIC  = 1
      integer,parameter,public :: DYNSAMP_LINEAR = 2
      integer,parameter,public :: DYNSAMP_FFT2   = 3
      integer,parameter,public :: DYNSAMP_FFT4   = 4
      integer,parameter,public :: DYNSAMP_FFT8   = 5

      contains


!!------------------------------- create -----------------------------------!!
!!------------------------------- create -----------------------------------!!
!!------------------------------- create -----------------------------------!!


      subroutine dynsamp_create (obj,ndpt,terpmode,error,msg)

      type(dynsamp_struct),pointer     :: obj               ! argument
      integer             ,intent(in)  :: ndpt              ! argument
      integer             ,intent(in)  :: terpmode          ! argument
      logical             ,intent(out) :: error             ! argument
      character(len=*)    ,intent(out) :: msg               ! argument

      error = .false.
      msg   = 'no errors'

      allocate(obj)

      obj%ndpt      = ndpt
      obj%terpmode  = terpmode
      obj%fineratio = 1
      obj%nfine     = ndpt

      nullify (obj%densify)

      !*********************************************
      !**** Prepare to densify the input traces ****
      !*********************************************

      select case (obj%terpmode)
           case (DYNSAMP_FFT2  ) ; obj%fineratio = 2
           case (DYNSAMP_FFT4  ) ; obj%fineratio = 4
           case (DYNSAMP_FFT8  ) ; obj%fineratio = 8
           case (DYNSAMP_CUBIC ) ; obj%fineratio = 1
           case (DYNSAMP_LINEAR) ; obj%fineratio = 1
           case default
                           error = .true.
                           msg = 'illegal value for TERPMODE'
                           return
      end select

      if (obj%fineratio > 1) then
        call densify_create &
                    (obj%densify, ndpt, obj%fineratio, obj%nfine, error, msg)
      end if

      end subroutine dynsamp_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine dynsamp_delete (obj)

      type(dynsamp_struct),pointer :: obj       ! argument

      if (.not.associated(obj)) return

      if (associated(obj%densify)) call densify_delete (obj%densify)

      deallocate(obj)

      end subroutine dynsamp_delete


!!--------------------------- dynsamp reverse -------------------------------!!
!!--------------------------- dynsamp reverse -------------------------------!!
!!--------------------------- dynsamp reverse -------------------------------!!


      subroutine dynsamp_reverse (obj, TB, A, B, mtop, mbottom)

      type(dynsamp_struct),intent(in)    :: obj                 ! argument
      REAL                ,intent(in)    :: TB(:)               ! argument
      REAL                ,intent(in)    :: A(:)                ! argument
      REAL                ,intent(out)   :: B(:)                ! argument
      integer             ,intent(inout) :: mtop,mbottom        ! argument
      REAL                               :: TA(obj%ndpt)        ! local

      call dynsamp_private_convert (obj,TB,TA)
      call dynsamp_forward         (obj,TA,A,B,mtop,mbottom)

      end subroutine dynsamp_reverse


!!--------------------------- private convert -------------------------------!!
!!--------------------------- private convert -------------------------------!!
!!--------------------------- private convert -------------------------------!!


      subroutine dynsamp_private_convert (obj, TB, TA)

      type(dynsamp_struct),intent(in)  :: obj                   ! argument
      REAL                ,INTENT(IN)  :: TB(:)                 ! argument
      REAL                ,INTENT(OUT) :: TA(:)                 ! argument
      INTEGER                          :: istart,jstart,indx,j  ! local

      !**********************************
      !**** Eliminate time reversals ****
      !**********************************

      istart = 1
      jstart = 1
      do indx = obj%ndpt,2,-1
          if (tb(indx) <= tb(indx-1)) then
               istart = int(tb(indx))
               jstart = indx
               exit
          end if
      end do
      call mth_constrain (istart,1,obj%ndpt)

      !******************************
      !**** Calculate TA from TB ****
      !******************************

      if (istart > 1) then
           do indx = 1,istart-1
                ta(indx) = indx - istart  ! to make TA negative but
                                          ! monotonically increasing, so it
                                          ! will not look like time reversals
                                          ! in dynsamp_forward.
           end do
      end if

      J = jstart

      DO indx = istart, obj%ndpt
10      CONTINUE
        IF (TB(J) > indx) THEN
          TA(indx) = 0.0
        ELSE IF (TB(J) == indx) THEN
          TA(indx) = J
        ELSE IF (J == obj%ndpt) THEN
          TA(indx) = obj%ndpt + 1.0
        ELSE IF (TB(J+1) >= indx) THEN
          TA(indx) = J + (indx - TB(J))/(TB(J+1)-TB(J))
        ELSE
          J = J + 1
          GO TO 10
        ENDIF
      END DO

      end subroutine dynsamp_private_convert


!!--------------------------- dynsamp forward -----------------------------!!
!!--------------------------- dynsamp forward -----------------------------!!
!!--------------------------- dynsamp forward -----------------------------!!


      subroutine dynsamp_forward (obj, TA, A, B, mtop, mbottom)

      type(dynsamp_struct),intent(in)    :: obj                 ! argument
      REAL                ,INTENT(IN)    :: TA(:)               ! argument
      REAL                ,INTENT(IN)    :: A(:)                ! argument
      REAL                ,INTENT(OUT)   :: B(:)                ! argument
      integer             ,intent(inout) :: mtop,mbottom        ! argument
      INTEGER                            :: indx      ,istart ! local
      REAL                               :: afine(obj%nfine)    ! local
      REAL                               :: tafine              ! local
      integer                            :: moved,mtopkeep      ! local

      !*******************************************
      !**** Eliminate top mute time reversals ****
      !*******************************************

      istart = 1
      do indx = obj%ndpt,2,-1
           if (ta(indx) <= ta(indx-1)) then
                istart = indx
                exit
           end if
      end do

      mtopkeep = mtop
      call mth_constrain (mtopkeep, 1     , obj%ndpt)
      call mth_constrain (mtop    , istart, obj%ndpt)
      call mth_constrain (mbottom , mtop  , obj%ndpt)

      !****************************************************
      !**** Shift top mute index by the moveout amount ****
      !****************************************************

      moved = istart
      do indx = obj%ndpt,istart,-1
           if (ta(indx) < mtopkeep) then
                moved = indx
                exit
           end if
      end do
      mtop = moved

      !*******************************************************
      !**** Shift bottom mute index by the moveout amount ****
      !*******************************************************

      do indx = obj%ndpt,mtop,-1
           if (ta(indx) < mbottom) then
                moved = indx
                exit
           end if
      end do
      mbottom = moved

      !****************************
      !**** Resample the array ****
      !****************************

      if (istart > 1) b(1:istart-1) = 0.0

      if (obj%fineratio > 1) then
           call densify_forward (obj%densify, a, afine)
           DO indx = istart,obj%ndpt
                tafine = 1.0 + (ta(indx) - 1.0) * obj%fineratio
                b(indx) = cubeterp_linear (afine, obj%nfine, tafine)
           END DO
           return
      end if

      DO indx = istart,obj%ndpt
        if (obj%terpmode == DYNSAMP_LINEAR) then
             b(indx) = cubeterp_linear (a, obj%ndpt, ta(indx))
        else
             b(indx) = cubeterp_cubic  (a, obj%ndpt, ta(indx))
        end if
      END DO

      end subroutine dynsamp_forward


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module dynsamp_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
