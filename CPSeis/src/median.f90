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
! Name       : median
! Category   : math
! Written    : 1988-05-12   by: Mike Howard
! Revised    : 2001-05-31   by: Bob Baumel
! Maturity   : production   2001-06-11
! Purpose    : returns the median value in a vector of reals or integers
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!    This routine uses an iterative method to find the median.  Given
!    a guess for the median, it counts the numbers of values bigger and
!    smaller than the guess, and uses this data to update the guess.
!
!    The algorithm USUALLY converges to the median in order LOG N iter-
!    ations, but for "pathologically" distributed data, it COULD take
!    as many as N-1 iterations.
!
!    You may optionally specify a starting guess for the iteration by
!    specifying a 4th argument to the subroutine.  If you call the
!    routine with only 3 arguments, the starting guess will be the
!    average of all values in ARRAY.
!
!    The current version of this routine is based loosely on an earlier
!    version by Mike Howard which was modified from an algorithm in the
!    book "Numerical Recipes" by William H. Press, et al.  The algorithm
!    has now been extensively reworked and no longer includes the book's
!    original iteration formula!  The previous version of this routine
!    did not always return the correct answer!
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
!                                              opt
!                               i    i    o     i
!                call median (array, n, amed, guess)
!
! With real array:
!   real               array(n) =  array from which to extract the median
!   integer            n        =  size of array
!   real               amed     =  median extracted
!   real               guess    =  initial guess at median
!
! With double precision array:
!   double precision   array(n) =  array from which to extract the median
!   integer            n        =  size of array
!   double precision   amed     =  median extracted
!   double precision   guess    =  initial guess at median
!
! With integer array:
!   integer            array(n) =  array from which to extract the median
!   integer            n        =  size of array
!   integer            amed     =  median extracted
!   integer            guess    =  initial guess at median
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!    A good guess will speed convergence. If 'guess' is omitted from the
!    calling arguments, the subroutine uses the mean of input values as
!    an initial guess.
!
!    When 'n' is even, the returned median is the average of the two middle
!    values in the array; in the case of integer data and even 'n' you get
!    the nearest integer ('nint') of the average of the two middle values.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 11. 2001-06-11  Baumel       Doc-only: Match template in brief_doc section.
!                              PRODUCTION.
! 10. 2001-04-26  Baumel       Fixed history_doc XML tag.
!  9. 2000-05-08  Bill Menger  Modified algorithm for real/double only so that
!                              it does not explicitly test for exact equalness
!                              but relies on mth_compare to provide a closeness
!                              test.  Prevents non-convergence on pathological
!                              data not caught with fix in rev#6 And...
!                              Bound number of iterations to be certain of
!                              avoiding endless loop.
!  8. 2000-04-07  Baumel       Fix algorithm for case of integer data (no change
!                              in case of real or double precision data).
!  7. 1999-12-29  O'Brien      Brought xml tags up to date
!                              Added RCS character ID variable
!                              Made generic interface to accept real, integer
!                              and double precision data
!  6. 1999-10-25  Baumel/Day   Fix bug that could prevent convergence.
!  5. 1999-08-06  O'Brien      Full f90 conversion
!  4. 1999-01-11  Goodger      Begin using the fortran90 compiler.
!  3. 1989-06-29  Bob Baumel   New Algorithm.
!  2. 1989-03-22  Harold Ball  Moved from OLD CPS to current system.
!  1. 1988-05-12  Mike Howard  OLD CPS version.
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
!    This routine uses an iterative method to find the median.  Given
!    a guess for the median, it counts the numbers of values bigger and
!    smaller than the guess, and uses this data to update the guess.
!
!    The algorithm USUALLY converges to the median in order LOG N iter-
!    ations, but for "pathologically" distributed data, it COULD take
!    as many as N-1 iterations.
!
!    The current version of this routine is based loosely on an earlier
!    version by Mike Howard which was modified from an algorithm in the
!    book "Numerical Recipes" by William H. Press, et al.  The algorithm
!    has now been extensively reworked and no longer includes the book's
!    original iteration formula!  The previous version of this routine
!    did not always return the correct answer!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!    This module has limits that should eventually be removed. These are:
!
!       1. The current algorithm is well suited to vector processors. Platform
!          dependent algorithms could be added for optimum performance on
!          many architectures.
!
!-------------------------------------------------------------------------------
!</programming_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!

      module median_module
      use mth_module, only: mth_compare
      implicit none

      private
      public :: median

      character(len=100),public,save :: MEDIAN_IDENT = &
'$Id: median.f90,v 1.11 2001/06/07 19:07:06 sps prod sps $'

!--------------------------- interface blocks --------------------------------!
!--------------------------- interface blocks --------------------------------!
!--------------------------- interface blocks --------------------------------!

      interface median
        module procedure median_real
        module procedure median_int
        module procedure median_dble
      end interface

      contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!****************************************************************************
! Median calc for real vector
!****************************************************************************
      subroutine median_real (array,n,amed,guess)

      implicit none

      real,intent(in)          :: array(n)            !arguments.
      integer,intent(in)       :: n                   !arguments.
      real,intent(out)         :: amed                !arguments.
      real,intent(in),optional :: guess               !arguments.

      integer                  :: iter                !local
      integer                  :: n1,n2,j,nle,nle2    !local
      real                     :: aa,a1,a2,x1,x2      !local
!-------------------------------------------------------------------

! If the vector length is 0, then this is a no-op.
      if (n == 0) then
        amed = 0.
        return
      endif

! Find the min and max values in the array
      a1 = minval(array(:))
      a2 = maxval(array(:))

! When a1=a2 the answer is at hand
      if (a1 == a2) then
        amed = a1
        return
      endif

! We need a guess, either through the interface or computed here
      if (present(guess)) then
        aa = guess
      else
        aa = sum(array)/n
      endif

! Initialize endpoint indices
      n1 = 1
      n2 = n

! Loop over points in array to isolate the median
      DO iter = 1, n
        nle = 0
        x1 = a1
        x2 = a2

       !-------------------------------------------------------------------
        do j=1,n
          if (array(j) <= aa) then       ! Each time the guess is updated,
            nle = nle + 1                ! this loop evaluates every sample
            x1 = max(x1,array(J))        ! in the array. If this happens
          else                           ! many times, it might be faster
            x2 = min(x2,array(J))        ! to sort the array and pluck out
          endif                          ! the median
        enddo
       !-------------------------------------------------------------------

        nle2 = 2 * nle
        if (nle2 < n-1) then
          a1 = x2
          n1 = nle + 1
          goto 30
        elseif (nle2 > n+1) then
          a2 = x1
          n2 = nle
          goto 30
        elseif (nle2 == n) then
          amed = (x1+x2) * 0.5
        elseif (nle2 < n) then
          amed = x2
        else
          amed = x1
        endif

! Unless we jumped to label 30, we're done.
        return

  30    if (mth_compare(a1,a2) == 0 ) then
          amed = a1
          return
        endif

! We haven't found it yet so update the guess and cycle again
        aa = a1 + (a2-a1)*real(n+1-2*n1)/real(2*(n2-n1))

! To avoid endless loop in case of numerical precision problem
        if(mth_compare(aa,a2) == 0 ) then
          amed = aa
          return
        endif

      END DO

! We shouldn't get here except in case of severe precision problem
      amed = aa

      return
      end subroutine median_real


!****************************************************************************
! Median calc for double precision vector
!****************************************************************************
      subroutine median_dble (array,n,amed,guess)

      implicit none

      double precision,intent(in)          :: array(n)       !arguments.
      integer,intent(in)                   :: n              !arguments.
      double precision,intent(out)         :: amed           !arguments.
      double precision,intent(in),optional :: guess          !arguments.

      integer                              :: iter                !local
      integer                              :: n1,n2,j,nle,nle2    !local
      double precision                     :: aa,a1,a2,x1,x2      !local
!-------------------------------------------------------------------

! If the vector length is 0, then this is a no-op.
      if (n == 0) then
        amed = 0.
        return
      endif

! Find the min and max values in the array
      a1 = minval(array(:))
      a2 = maxval(array(:))

! When a1=a2 the answer is at hand
      if (a1 == a2) then
        amed = a1
        return
      endif

! We need a guess, either through the interface or computed here
      if (present(guess)) then
        aa = guess
      else
        aa = sum(array)/n
      endif

! Initialize endpoint indices
      n1 = 1
      n2 = n

! Loop over points in array to isolate the median
      DO iter = 1, n
        nle = 0
        x1 = a1
        x2 = a2

       !-------------------------------------------------------------------
        do j=1,n
          if (array(j) <= aa) then       ! Each time the guess is updated,
            nle = nle + 1                ! this loop evaluates every sample
            x1 = max(x1,array(J))        ! in the array. If this happens
          else                           ! many times, it might be faster
            x2 = min(x2,array(J))        ! to sort the array and pluck out
          endif                          ! the median
        enddo
       !-------------------------------------------------------------------

        nle2 = 2 * nle
        if (nle2 < n-1) then
          a1 = x2
          n1 = nle + 1
          goto 30
        elseif (nle2 > n+1) then
          a2 = x1
          n2 = nle
          goto 30
        elseif (nle2 == n) then
          amed = (x1+x2) * 0.5
        elseif (nle2 < n) then
          amed = x2
        else
          amed = x1
        endif

! Unless we jumped to label 30, we're done.
        return

  30    if (mth_compare(a1,a2) == 0 ) then
          amed = a1
          return
        endif

! We haven't found it yet so update the guess and cycle again
        aa = a1 + (a2-a1)*real(n+1-2*n1)/real(2*(n2-n1))

! To avoid endless loop in case of numerical precision problem
        if (mth_compare(aa,a2) == 0 ) then
          amed = aa
          return
        endif

      END DO

! We shouldn't get here except in case of severe precision problem
      amed = aa

      return
      end subroutine median_dble



!****************************************************************************
! Median calc for integer vector
!****************************************************************************
      subroutine median_int (array,n,amed,guess)

      implicit none

      integer,intent(in)          :: array(n)       !arguments.
      integer,intent(in)          :: n              !arguments.
      integer,intent(out)         :: amed           !arguments.
      integer,intent(in),optional :: guess          !arguments.

      integer                     :: iter                !local
      integer                     :: n1,n2,j,nle,nle2    !local
      integer                     :: aa,a1,a2,x1,x2      !local
!-------------------------------------------------------------------

! If the vector length is 0, then this is a no-op.
      if (n == 0) then
        amed = 0.
        return
      endif

! Find the min and max values in the array
      a1 = minval(array(:))
      a2 = maxval(array(:))

! When a1=a2 the answer is at hand
      if (a1 == a2) then
        amed = a1
        return
      endif

! We need a guess, either through the interface or computed here
      if (present(guess)) then
        aa = guess
      else
        aa = nint(real(sum(array))/real(n))
      endif
      if (aa == a2) aa = a2 - 1

! Initialize endpoint indices
      n1 = 1
      n2 = n

! Loop over points in array to isolate the median
      DO iter = 1, n
        nle = 0
        x1 = a1
        x2 = a2

       !-------------------------------------------------------------------
        do j=1,n
          if (array(j) <= aa) then       ! Each time the guess is updated,
            nle = nle + 1                ! this loop evaluates every sample
            x1 = max(x1,array(J))        ! in the array. If this happens
          else                           ! many times, it might be faster
            x2 = min(x2,array(J))        ! to sort the array and pluck out
          endif                          ! the median
        enddo
       !-------------------------------------------------------------------

        nle2 = 2 * nle
        if (nle2 < n-1) then
          a1 = x2
          n1 = nle + 1
          goto 30
        elseif (nle2 > n+1) then
          a2 = x1
          n2 = nle
          goto 30
        elseif (nle2 == n) then
          amed = nint(real(x1+x2)*0.5)
        elseif (nle2 < n) then
          amed = x2
        else
          amed = x1
        endif

! Unless we jumped to label 30, we're done.
        return

  30    if (a1 == a2) then
          amed = a1
          return
        endif

! We haven't found it yet so update the guess and cycle again
        aa = a1 + nint(real((a2-a1)*(n+1-2*n1))/real(2*(n2-n1)))

! Be sure next guess is < a2, to avoid endless loop
        if (aa == a2) aa = a2 - 1

      END DO

! We shouldn't get here unless something is very wrong
      amed = aa

      return
      end subroutine median_int


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module median_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
