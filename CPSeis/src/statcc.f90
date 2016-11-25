
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- statcc.f90 --------------------------------!!
!!------------------------------- statcc.f90 --------------------------------!!
!!------------------------------- statcc.f90 --------------------------------!!

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
! Name       : statcc (Static trace shift by cubic interpolation)
! Category   : math
! Written    : 1988-11-21   by: Stoeckley
! Revised    : 1999-11-17   by: Stoeckley
! Maturity   : production   2000-06-22
! Purpose    : Shift a trace by a specified amount.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!        This routine uses cubic (4-point) interpolation.  The trace amplitude
!        and its derivative are continuous.
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
!                                   i    i   i     o
!                call statcc     (shift, n, a(:), b(:))
!
! real                       shift   =    Static sin ssample point units;
!                                         can be fractional.
! integer                    n       =    number of trace samples to shift.
! real, dimension(:)         a       =    Vector containing trace to shift.
! real, dimension(:)         b       =    Vector containing shifted trace.
!                                         Note:  address 'b' must be different
!                                         from address 'a'.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!    Date        Author       Description
!    ----        ------       -----------
! 5) 1999-11-17  Stoeckley    Made the STATCC subroutine public. Also add
!                              ident string for RCS.
! 4) 1999-10-20  Dorman       Convert to new CPS standards and fortran90;
!                              Replaced most do loops with array expressions.
! 3) 1999-01-11  Goodger      Begin using the fortran90 compiler.
! 2) 1992-11-17  Troutt       Add separate logic for whole sample shift,
!                              and fix edge effects for fractional cases.
! 1) 1988-11-21  Stoeckley    Original version.
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
!        This routine uses cubic (4-point) interpolation.  The trace
!        amplitude and its deare continuous.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!

    module statcc_module
      implicit none

      public

      character(len=100),public,save :: STATCC_IDENT = &
       '$Id: statcc.f90,v 1.5 2000/06/21 17:47:26 sps prod sps $'

      contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

    subroutine statcc (shift,n,a,b)
      implicit none
!----------declarations
      real, dimension(1:n) :: a, b
      real                 :: shift, shft, f, g
      real                 :: f2, g2, denom, wa, wb, wc, wd
      integer              :: n, nn, i, i1, i2, ishft, test
      integer              :: l, la, lb, lc, ld
!----------start
      nn=n
      shft=shift
!----------if shift is zero, copy trace.
      if(shft.eq.0.0) then
           b(1:nn)=a(1:nn)
           return
      end if
!----------if shift is whole number, move data.
      ishft=int(shft)
      test=float(ishft)
      if(test.eq.shft) then
!----------shift is negative.
           if (shft.lt.0.) then
                i1=1
                i2=max(nn+ishft,1)
                b(i2:nn)=0.0
!----------shift is positive.
           else
                i1=min(ishft+1,nn)
                i2=nn
                b(1:i1)=0.0
           end if
           if(abs(ishft).ge.nn) return
           b(i1:i2)=a(i1-ishft:i2-ishft)
           return
      end if
!----------shift has fractional component; interpolate
!----------shift is negative.
      if (shft.lt.0.) then
           l=-shft
           f=-shft-l
           g=1.-f
           la=l-1
!----------shift is positive.
      else
           l=-shft
           g=shft+l
           f=1.-g
           la=l-2
      end if
!----------get weights.
      lb=la+1
      lc=la+2
      ld=la+3
      f2=f**2
      g2=g**2
      denom=f2+g2+1.
      wa=-g2*f/denom
      wb=(g2*f+g2+g)/denom
      wc=(f2*g+f2+f)/denom
      wd=-f2*g/denom
!----------get ready to apply shift.
      i1=max(1,1-la)
      i2=min(nn,nn-ld)
      if (i1.gt.i2) then         !   call clear (b,nn)
           b(1:nn)=0.
           return
      end if
      if (i1.gt.1) then          !   call clear (b(1),i1-1)
           b(1:i1-1)=0.
      end if
      if (i2.lt.n) then          !   call clear (b(i2+1),nn-i2)
           b(i2+1:nn)=0.
      end if
!----------apply shift.
      do i=i1,i2
      b(i)=wa*a(i+la)+wb*a(i+lb)+wc*a(i+lc)+wd*a(i+ld)
      end do

!----------see if we have all the values we can get
      if(shft.lt.0.0) then
           if(i1.gt.1) then
              i1=i1-1
              b(i1)=              wb*a(i1+lb)+wc*a(i1+lc)+wd*a(i1+ld)
           end if
           if(i2.lt.nn-int(shft)) then
              i2=i2+1
              b(i2)=  wa*a(i2+la)+wb*a(i2+lb)+wc*a(i2+lc)
            if(i2.lt.nn-int(shft)) then
               i2=i2+1
               b(i2)= wa*a(i2+la)+wb*a(i2+lb)
             if(i2.lt.nn-int(shft)) then
                i2=i2+1
                b(i2)=wa*a(i2+la)
             end if
            end if
           end if
      end if
      if(shft.gt.0.0) then
           if(i2.lt.nn) then
              i2=i2+1
              b(i2)=  wa*a(i2+la)+wb*a(i2+lb)+wc*a(i2+lc)
           end if
           if(i1.gt.1+int(shft)) then
              i1=i1-1
              b(i1)=              wb*a(i1+lb)+wc*a(i1+lc)+wd*a(i1+ld)
            if(i1.gt.1+int(shft)) then
               i1=i1-1
               b(i1)=                         wc*a(i1+lc)+wd*a(i1+ld)
            end if
           end if
      end if

      return
    end subroutine statcc


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


    end module statcc_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!


