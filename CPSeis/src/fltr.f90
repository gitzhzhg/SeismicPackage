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
! Name       : fltr
! Category   : filters
! Written    : 1999-09-03   by: O'Brien    Consolidate filterg and filtrgs
! Revised    : 2000-07-14   by: Baumel
! Maturity   : production   2000-07-25
! Purpose    : computes 1-D time/space domain cross correlation of 2 vectors
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!     fltr_filterg  - truncated cross correlation of 2 vectors.
!     fltr_filtrgs  - cross correlation of 2 vectors with a user defined
!                     shift applied to the filter (first input vector) and
!                     correlation length.
!     fltr_filtgs   - REPLACED by functional equivalent fltr_filtrgs.
!     fltr_hilbert  - fast approximate time-domain Hilbert transform.
!     fltr_envelope - fast approximate time-domain envelope calculation.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
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
!                         i    i   i   i      o
!    call fltr_filterg (filter,m, data,n, correlation)
!
!    integer,  intent(in)    :: m                  ! size of filter
!    integer,  intent(in)    :: n                  ! size of data
!    real,     intent(in)    :: filter(m)          ! input filter
!    real,     intent(in)    :: data(n)            ! input data
!    real,     intent(out)   :: correlation(:)     ! computed correlation
!
!    Note: Number of elements returned in correlation is  abs(n-m) + 1.
!
!
!                         i    i   i   i      b        i     i      i
!    call fltr_filtrgs (filter,m, data,n, correlation, l, iflag, ishift)
!
!  Parameters as for filterg except as noted here.
!    real,     intent(inout) :: correlation(l)     ! computed correlation
!    integer,  intent(in)    :: l                  ! length of correlation
!    integer,  intent(in)    :: iflag              ! summation flag
!                                                   (0 = accumulate results)
!    integer,  intent(in)    :: ishift             ! shift before correlation
!
!
!                        i    i    o
!    call fltr_hilbert (data, n, hilbert)
!
!    integer,  intent(in)    :: n                  ! size of data
!    real,     intent(in)    :: data(n)            ! input data
!    real,     intent(out)   :: hilbert(n)         ! computed hilbert transform
!
!
!                                            opt
!                         i    i     o        i
!    call fltr_envelope (data, n, envelope, isqrt)
!
!    integer,  intent(in)    :: n                  ! size of data
!    real,     intent(in)    :: data(n)            ! input data
!    real,     intent(out)   :: envelope(n)        ! computed envelope
!    logical,  intent(in)    :: isqrt              ! whether to take square
!                                                    root of squared magnitude
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
!  6. 2000-07-25  Baumel        Added routines fltr_hilbert and fltr_envelope;
!                               changed correlation vector in fltr_filterg to
!                               dimension(:) to correctly handle case where
!                               filter() is longer than data(); simplified
!                               summation limit calculation in fltr_filtrgs;
!                               renamed private routine fltr_filtrgs_f77 to
!                               fltr_filtrgs_vector to better express purpose.
!  5. 2000-03-10  O'Brien       Adjusted loop indices in fltr_filtrgs to handle
!                                 case where filter() is longer than data()
!  4. 1999-12-29  O'Brien       Brought xml tags up to date
!                               Add RCS character ID variable
!  3. 1999-09-10  O'Brien       Documentation fixes.
!  2. 1999-09-08  O'Brien       Consolidation of routines
!                               Subroutine naming convention changed
!                                 From: filterg    ==>  To: fltr_filterg
!                                 From: filtrgs    ==>  To: fltr_filtrgs
!                                 From: filtgs     ==>  To: fltr_filtrgs
!  1. 1990-12-07  Baumel        Initial FILTRGS
!
!
!-------------------------------------------------------------------------------
!</history_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!    FLTR_FILTERG:
!       fltr_filterg is an f90 implementation of Cray library routine FILTERG.
!       According to Cray Research Man pages, FILTERG computes the correlation
!       of two vectors as follows:
!
!       Given
!            filter(i)      i = 1,...,m    filter coefficients
!            data  (j)      j = 1,...,n    data
!
!       Compute
!            correlation(k) = sum  ( filter(i) * data(i+k-1) )
!                             i=1,m
!
!            for k = 1, ... ,(n-m+1)
!
!     Note: The cross correlation is shorter than the data vector.
!           The correlation vector is truncated to that portion of the
!             cross correlation where ALL filter points are correlated
!             with data points. (Correlation values where the filter is
!             sliding onto or off of the data not computed.)
!           Implementation protects users from negative values of
!             correlation index k (filter longer than data) by "swapping"
!             filter and data prior to correlation and reversing the
!             index order in the correlation prior to returning.
!
!--------------------------------------------------------------------------
!    FLTR_FILTRGS:
!       fltr_filtrgs allows a user defined shift, effectively adjusting the
!       zero time of the filter.
!
!          ishift < 0 essentially pads data zeros and
!          ishift > 0 truncates data samples at the top of the data
!                     prior to correlation.
!
!       The user is also allowed to accumulate the correlation values by
!       setting iflag == 0. The fltr_filtrgs correlation differs from
!       fltr_filterg in that the filter is allowed to run off the end of the
!       data, computing min(l,n-ishift) correlation values.
!
!       In a formula:
!            correlation(k) = sum  ( filter(i) * data(i+k+ishift-1) )
!                              i
!            where the sum ranges over all values of i such that:
!               1 <=      i       <= m       and
!               1 <= i+k+ishift-1 <= n
!
!       For the common case where data and correlation have equal length
!       and the same starting time, choose
!               ishift = 1 - izero
!       where izero is the index of the filter sample at time zero.
!
!       Remember that fltr_filtrgs does correlation rather than convolution.
!       If you want convolution, you must reverse the order of your filter
!       coefficients before calling fltr_filtrgs.
!
!--------------------------------------------------------------------------
!    FLTR_HILBERT:
!       fltr_hilbert is a fast, approximate Hilbert transform based on code
!       from several processes written by Bill Harlan (including MDIP and
!       ASTK). It is implemented here by convolution with a short time-domain
!       operator (currently, the operator length is hard-wired as 31).
!
!       fltr_hilbert (and matching routine fltr_envelope discussed below) are
!       intended for processes that need only a rather crude estimate of
!       Hilbert transform (or envelope). More accurate Hilbert transforms can
!       be obtained by the CTAN process, which calculates it in the frequency
!       domain. In fact, since the Hilbert transform is just a 90 degree phase
!       advance, this can also be done using any frequency-domain filtering
!       process such as GENFILT. Or it can be done with TSVF, which implements
!       it using time-domain convolution, but with a longer operator than is
!       used by fltr_hilbert.
!
!--------------------------------------------------------------------------
!    FLTR_ENVELOPE:
!       fltr_envelope is a fast, approximate envelope calculation based on
!       code from several processes written by Bill Harlan (including MDIP and
!       ASTK). fltr_envelope calls fltr_hilbert, and therefore makes exactly
!       the same approximations as fltr_hilbert.
!
!       The optional isqrt argument specifies whether to take the square root
!       of the squared magnitude. You must, in fact, specify isqrt=.true. in
!       order to obtain a standard envelope; otherwise, you get the square of
!       the quantity normally called the envelope.
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


      module fltr_module

      use named_constants_module

      implicit none

      private
      public :: fltr_filterg
      public :: fltr_filtrgs
      public :: fltr_hilbert
      public :: fltr_envelope

      character(len=100),public,save :: FLTR_IDENT = &
             '$Id: fltr.f90,v 1.6 2000/07/24 13:49:38 sps prod sps $'

      contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

!**************************************************************************
! subroutine fltr_filterg -- truncated cross correlation of filter and data
!**************************************************************************
      subroutine fltr_filterg (filter,m, data,n, correlation)

      implicit none

      integer,     intent(in)  :: m, n                ! arguments
      real,        intent(in)  :: filter(m), data(n)  ! arguments
      real,        intent(out) :: correlation(:)      ! arguments

      integer                  :: k                   ! local
!-------------------------------------------

      if (m<=n) then
        do k = 1, n-m+1
          correlation(k) = sum( filter(1:m)*data(k:k+m-1) )
        enddo
      else
        do k = 1, m-n+1
          correlation(k) = sum( data(1:n)*filter(k:k+n-1) )
        enddo
        correlation(1:m-n+1) = correlation(m-n+1:1:-1)
      endif

      return
      end subroutine fltr_filterg


!**************************************************************************
! subroutine fltr_filtrgs  --  attempt to use filterg equation style
!         cross correlation with user supplied shift to filter
!         accumulation is allowed
!         cross correlation length not necessarily (n-m+1)
!**************************************************************************
      subroutine fltr_filtrgs(filter,m,data,n,correlation,l,iflag,ishift)

      implicit none

      integer,   intent(in)    :: l,m,n              ! arguments
      integer,   intent(in)    :: iflag,ishift       ! arguments
      real,      intent(in)    :: filter(m), data(n) ! arguments
      real,      intent(inout) :: correlation(l)     ! arguments

      integer                  :: k, k1,k2           ! local
      integer                  :: m1,m2, n1,n2       ! local
!-------------------------------------------
! Handle iflag directive
      if (iflag /= 0) correlation = 0.0

! Set the index limits for the correlation output.
!    k2_max is n-ishift  but k2 can be limitted by argument 'l'
!    k1     is   ( n - ishift )  -  ( n - 1 )      -   ( m - 1 )
!              [ k2_max          -  data_length-1  -   filter_length-1 ]
!
      k1 = max(1,2-m-ishift)
      k2 = min(l,n-ishift)

! Accumulate the correlation
      do k = k1,k2
        ! Indices for filter and data are precomputed due to complexity.
        m1 = max(1,2-k-ishift)   ;   m2 = min(m,1-k+n-ishift)
        n1 = m1+k+ishift-1       ;   n2 = n1+m2-m1
        correlation(k) = correlation(k) + sum( filter(m1:m2) * data(n1:n2) )
      enddo

      return
      end subroutine fltr_filtrgs


!**************************************************************************
! subroutine fltr_filtrgs_vector
!         Vectorized version of fltr_filtrgs. (This may prove to be faster)
!         cross correlation with user supplied shift to filter
!         accumulation is allowed
!         cross correlation length not necessarily (n-m+1)
!**************************************************************************
      subroutine fltr_filtrgs_vector (filter,m, data,n, correlation,l, &
                                      iflag, ishift)
      implicit none

      integer, intent(in) :: l, m, n             ! Arguments
      integer, intent(in) :: iflag, ishift       ! Arguments
      real,    intent(in) :: filter(m)           ! Arguments
      real,    intent(in) :: data(n)             ! Arguments
      real, intent(inout) :: correlation(l)      ! Arguments

      integer             :: i, ii, k            ! local
!-----------------------------------------------

! Handle iflag directive
      if (iflag /= 0) correlation = 0.0

! Accumulate the correlation.
      do i = 1, m
        ii = i + ishift - 1
        do k = max(1,1-ii), min(l,n-ii)
          correlation(k) = correlation(k) + filter(i)*data(k+ii)
        enddo
      enddo

      return
      end subroutine fltr_filtrgs_vector


!**************************************************************************
! subroutine fltr_cross1d
!         Full 1-D cross correlation
!**************************************************************************
      subroutine fltr_cross1d (filter,m,data,n,correlation)

      implicit none

      integer,   intent(in)    :: m,n                ! arguments
      real,      intent(in)    :: filter(m), data(n) ! arguments
      real,      intent(inout) :: correlation(m+n-1) ! arguments

      integer                  :: k                  ! local
!-------------------------------------------

! Calculate the cross correlation
      do k = 1,m+n-1
        correlation(k) = sum( filter(max(1,m-k+1):min(m,m-k+n)) &
                             *data(max(1,k-m+1):min(n,k)) )
      enddo

      return
      end subroutine fltr_cross1d


!**************************************************************************
! subroutine fltr_hilbert
!         Quick time-domain Hilbert transform
!         The operator length (len_oper) is hard-wired as a parameter,
!         currently set to 31, but this can easily be changed.
!         Note: len_oper must be an ODD positive integer.
!**************************************************************************

      subroutine fltr_hilbert (data, n, hilbert)

      implicit none

      integer,   intent(in)    :: n                         ! arguments
      real,      intent(in)    :: data (n)                  ! arguments
      real,      intent(out)   :: hilbert (n)               ! arguments

      integer,   parameter     :: len_oper = 31             ! local
      real,      save          :: oper (len_oper)           ! local
      logical,   save          :: ifirst = .true.           ! local
      integer,   save          :: middle                    ! local
      integer                  :: i                         ! local
      real                     :: factor, taper_fact        ! local

!!----Calculate operator if this is the first time called
      if (ifirst) then
        middle = (1 + len_oper) / 2
        factor = 0.5D0 * PI
        taper_fact = factor / (middle + 0.5)
        oper = 0.0
        do i = 1, middle-1, 2
          oper(middle + i) = cos(taper_fact * i)**2 / (factor * i)
          oper(middle - i) = - oper(middle + i)
        end do
        ifirst = .false.
      end if

!!----Apply operator
      call fltr_filtrgs (oper,len_oper, data,n, hilbert,n, 1, 1-middle)

      return
      end subroutine fltr_hilbert


!**************************************************************************
! subroutine fltr_envelope
!         Quick time-domain envelope calculation.
!         This operates by calling fltr_hilbert.
!         If ISQRT is present and true, you get square root of squared
!           magnitude (standard envelope); otherwise squared magnitude.
!**************************************************************************

      subroutine fltr_envelope (data, n, envelope, isqrt)

      implicit none

      integer,           intent(in)  :: n                     ! arguments
      real,              intent(in)  :: data (n)              ! arguments
      real,              intent(out) :: envelope (n)          ! arguments
      logical, optional, intent(in)  :: isqrt                 ! arguments

      call fltr_hilbert (data, n, envelope)

      envelope = data**2 + envelope**2

      if (present(isqrt)) then
        if (isqrt) envelope = sqrt (envelope)
      end if

      return
      end subroutine fltr_envelope


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module fltr_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
