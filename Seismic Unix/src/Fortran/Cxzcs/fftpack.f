**
* Center for Wave Phenomena: 
* $Revision: 1.2 $ $Date: 87/07/28 16:55:19 $
* $Source: /src/general/Fftpack/RCS/fftpack.f,v $
**

***********************************************************************
*                                                                     *
* A self-sorting mixed-radix Fast Fourier Transform package           *
*                                                                     *
*                      written by                                     *
*                                                                     *
*           Brian Sumner, Center For Wave Phenomena                   *
*                                                                     *
*                                                                     *
* Copyright 1987 by Brian Sumner  All rights reserved                 *
*                                                                     *
***********************************************************************

***********************************************************************
*                                                                     *
* FFT - compute Fast Fourier Transforms                               *
*                                                                     *
* Parameters:                                                         *
*    XR(), XI()	 - data to transform                                  *
*    YR(), YI()	 - work area, this algorithm is not "in place"        *
*    N		 - the length of the transform                        *
*    FACS()	 - a list of factors of N, in {2,3,4,5,6}             *
*    NFAC	 - the number of factors of N                         *
*    CC()	 - precomputed table of cosines                       *
*    CS()	 - precomputed table of sines                         *
*                                                                     *
***********************************************************************

      subroutine fft(xr,xi,yr,yi,n,facs,nfac,cc,cs)
      real xr(0:*),xi(0:*),yr(0:*),yi(0:*),cc(0:*),cs(0:*)
      integer n,facs(0:*),nfac

**
* Local variables:
*    LI		 - accumulate product of factors
*    J		 - index variable
*    FAC	 - factor under consideration
*    IZERO, ONE	 - constants
**

      integer izero, one
      parameter (izero = 0, one = 1)
      
      integer li, j, fac

**
*     Code begins.
**
 
      li = one
      j = izero
      
  10  fac = facs(j)
      call fftpas(xr,xi,yr,yi,n,fac,li,cc,cs)
      j = j + one
      if (j .eq. nfac) then
         do 20 j = izero, n - one
            xr(j) = yr(j)
            xi(j) = yi(j)
  20     continue
         return
      endif
      li = li * fac
      fac = facs(j)
      call fftpas(yr,yi,xr,xi,n,fac,li,cc,cs)
      j = j + one
      if (j .eq. nfac) return
      li = li * fac
      go to 10
      
      end

***********************************************************************
*                                                                     *
* FFTPAS - compute one pass of FFT corresponding to factor FAC        *
*                                                                     *
* Parameters:                                                         *
*    ZR(), ZI()	 - real and imaginary parts of data to act on         *
*    XR(), XI()	 - place to put result                                *
*    N		 - the length of the transform                        *
*    FAC	 - the current factor of n under consideration        *
*    LI		 - the product of all factors so far                  *
*    CC()	 - precomputed cosines                                *
*    CS()	 - precomputed sines                                  *
*                                                                     *
***********************************************************************

      subroutine fftpas(zr,zi,xr,xi,n,fac,li,cc,cs)
      real zr(0:*),zi(0:*),xr(0:*),xi(0:*),cc(0:*),cs(0:*)
      integer n,fac,li

**
* Local variables:
*    M		 - gap size in z
*    JUMP	 - spacing in x
*    K		 - gap in omega
*    KK		 - counter for k
*    L		 - loop counter
*    I0, I1, I2, I3, I4, I5
*		- indices in z
*    J0, J1, J2, J3, J4, J5
*		- indices in x
*    Z0R, Z0I .. Z5R, Z5I
*       	 - real and imaginary parts of small z vector to
*		   transform
*    X1R, X1I .. X5R, X5I
*		- real and imaginary parts of transformed vector
*    T1R, T1I .. T11R, T11I
*		- real and imaginary parts of temporaries
*    C1 .. C5	 - hold cosines
*    S1 .. S5	 - hold sines
*    IZERO, ONE, SIN60, HALF, SQ5O4, QRTR, SIN72, SIN36 - constants
*
**

      integer izero, one
      parameter (izero = 0, one = 1)
      
      real sin60, half, sq5o4, qrtr, sin72, sin36
      parameter (sin60 = 0.866025404,
     :           half = 0.5,
     :           sq5o4 = 0.559016994,
     :           qrtr = 0.25,
     :           sin72 = 0.951056516,
     :           sin36 = 0.587785252)

      integer i, j, m, jump, k, l, i0, i1, i2, i3, i4, i5, j0, j1,
     :        j2, j3, j4, j5, kk

      real z0r, z0i, x1r, x1i, x2r, x2i, x3r, x3i, x4r, x4i, x5r,
     :     x5i, t1r, t1i, t2r, t2i, t3r, t3i, t4r, t4i, t5r, t5i,
     :     t6r, t6i, t7r, t7i, t8r, t8i, t9r, t9i, t10r, t10i, t11r,
     :     t11i, c1, c2, c3, c4, c5, s1, s2, s3, s4, s5



**
*     Code begins. first compute base indices in z and x.
**

      i = izero
      j =  izero
      m = n / fac
      jump = (fac - one) * li
      i0 = izero
      j0 = izero
      i1 = m
      j1 = li
      if (fac .eq. 2) go to 10
      i2 = i1 + m
      j2 = j1 + li
      if (fac .eq. 3) go to 50
      i3 = i2 + m
      j3 = j2 + li
      if (fac .eq. 4) go to 90
      i4 = i3 + m
      j4 = j3 + li
      if (fac .eq. 5) go to 130
      i5 = i4 + m
      j5 = j4 + li
      go to 170

**
*     Now we have the code for each factor. Each has the same
*     structure. Loop for k = 0, i.e. all the complex multiplications
*     are by 1. Then loop over all the other k values.
**


**
*     Here is code for fac = 2
**

  10  do 20 l = one, li
	 xr(j1+j) = zr(i0+i)-zr(i1+i)
	 xr(j0+j) = zr(i0+i)+zr(i1+i)
	 xi(j1+j) = zi(i0+i)-zi(i1+i)
	 xi(j0+j) = zi(i0+i)+zi(i1+i)
	 i = i+one
	 j = j+one
  20  continue
      j = j + jump
      do 40 k = li, m-li, li
         s1 = cs(k)
         c1 = cc(k)
         do 30 l = one, li
      	    x1r      = zr(i0+i)-zr(i1+i)
      	    xr(j0+j) = zr(i0+i)+zr(i1+i)
      	    x1i      = zi(i0+i)-zi(i1+i)
      	    xi(j0+j) = zi(i0+i)+zi(i1+i)
	    xr(j1+j) = c1*x1r-s1*x1i
	    xi(j1+j) = s1*x1r+c1*x1i
	    i = i+one
	    j = j+one
  30     continue
         j = j + jump
  40  continue
      return

**
*     Here is code for fac = 3.
**

  50  do 60 l = one, li
         t1r      = zr(i1+i)+zr(i2+i)
         t3r      = sin60*(zr(i1+i)-zr(i2+i))
         t1i      = zi(i1+i)+zi(i2+i)
         t3i      = sin60*(zi(i1+i)-zi(i2+i))
         t2r      = zr(i0+i)-half*t1r
         xr(j0+j) = zr(i0+i)+t1r
         t2i      = zi(i0+i)-half*t1i
         xi(j0+j) = zi(i0+i)+t1i
         xr(j1+j) = t2r-t3i
         xr(j2+j) = t2r+t3i
         xi(j1+j) = t2i+t3r
         xi(j2+j) = t2i-t3r
	 i = i+one
	 j = j+one
  60  continue
      j = j + jump
      do 80 k = li, m-li, li
         s1 = cs(k)
         c1 = cc(k)
         kk = k+k
         s2 = cs(kk)
         c2 = cc(kk)
         do 70 l = one, li
            t1r      = zr(i1+i)+zr(i2+i)
            t3r      = sin60*(zr(i1+i)-zr(i2+i))
            t1i      = zi(i1+i)+zi(i2+i)
            t3i      = sin60*(zi(i1+i)-zi(i2+i))
            t2r      = zr(i0+i)-half*t1r
            xr(j0+j) = zr(i0+i)+t1r
            t2i      = zi(i0+i)-half*t1i
            xi(j0+j) = zi(i0+i)+t1i
            x1r      = t2r-t3i
            x2r      = t2r+t3i
            x1i      = t2i+t3r
            x2i      = t2i-t3r
	    xr(j1+j) = c1*x1r-s1*x1i
	    xi(j1+j) = s1*x1r+c1*x1i
	    xr(j2+j) = c2*x2r-s2*x2i
	    xi(j2+j) = s2*x2r+c2*x2i
	    i = i+one
	    j = j+one
  70     continue
         j = j + jump
  80  continue
      return

**
*     Here is code for fac = 4.
**

  90  do 100 l = one, li
         t1r      = zr(i0+i)+zr(i2+i)
	 t3r      = zr(i0+i)-zr(i2+i)
	 t1i      = zi(i0+i)+zi(i2+i)
	 t3i      = zi(i0+i)-zi(i2+i)
	 t2r      = zr(i1+i)+zr(i3+i)
	 t4r      = zr(i1+i)-zr(i3+i)
	 t2i      = zi(i1+i)+zi(i3+i)
	 t4i      = zi(i1+i)-zi(i3+i)
	 xr(j0+j) = t1r+t2r
	 xr(j2+j) = t1r-t2r
	 xi(j0+j) = t1i+t2i
	 xi(j2+j) = t1i-t2i
	 xr(j1+j) = t3r-t4i
	 xr(j3+j) = t3r+t4i
	 xi(j1+j) = t3i+t4r
	 xi(j3+j) = t3i-t4r
	 i = i+one
	 j = j+one
 100  continue
      j = j + jump
      do 120 k = li, m-li, li
         s1 = cs(k)
         c1 = cc(k)
         kk = k+k
         s2 = cs(kk)
         c2 = cc(kk)
         kk = kk+k
         s3 = cs(kk)
         c3 = cc(kk)
         do 110 l = one, li
            t1r      = zr(i0+i)+zr(i2+i)
   	    t3r      = zr(i0+i)-zr(i2+i)
            t1i      = zi(i0+i)+zi(i2+i)
	    t3i      = zi(i0+i)-zi(i2+i)
	    t2r      = zr(i1+i)+zr(i3+i)
	    t4r      = zr(i1+i)-zr(i3+i)
	    t2i      = zi(i1+i)+zi(i3+i)
	    t4i      = zi(i1+i)-zi(i3+i)
	    xr(j0+j) = t1r+t2r
	    x2r      = t1r-t2r
	    xi(j0+j) = t1i+t2i
	    x2i      = t1i-t2i
	    x1r      = t3r-t4i
	    x3r      = t3r+t4i
	    x1i      = t3i+t4r
	    x3i      = t3i-t4r
            xr(j1+j) = c1*x1r - s1*x1i
            xi(j1+j) = s1*x1r + c1*x1i
            xr(j2+j) = c2*x2r - s2*x2i
            xi(j2+j) = s2*x2r + c2*x2i
            xr(j3+j) = c3*x3r - s3*x3i
            xi(j3+j) = s3*x3r + c3*x3i
	    i = i+one
	    j = j+one
 110     continue
         j = j + jump
 120  continue
      return

**
*     Code for fac = 5.
**

 130  do 140 l = one, li
         t1r      = zr(i1+i)+zr(i4+i)
	 t3r      = zr(i1+i)-zr(i4+i)
	 t1i      = zi(i1+i)+zi(i4+i)
	 t3i      = zi(i1+i)-zi(i4+i)
	 t2r      = zr(i2+i)+zr(i3+i)
	 t4r      = zr(i2+i)-zr(i3+i)
	 t2i      = zi(i2+i)+zi(i3+i)
	 t4i      = zi(i2+i)-zi(i3+i)
	 t5r      = t1r+t2r
	 t6r      = sq5o4*(t1r-t2r)
	 t5i      = t1i+t2i
	 t6i      = sq5o4*(t1i-t2i)
	 z0r      = zr(i0+i)
	 t7r      = z0r-qrtr*t5r
	 xr(j0+j) = z0r+t5r
	 t8r      = t7r+t6r
	 t9r      = t7r-t6r
	 z0i      = zi(i0+i)
	 t7i      = z0i-qrtr*t5i
	 xi(j0+j) = z0i+t5i
	 t8i      = t7i+t6i
	 t9i      = t7i-t6i
	 t10r     = sin72*t3r+sin36*t4r
	 t11r     = sin36*t3r-sin72*t4r
	 t10i     = sin72*t3i+sin36*t4i
	 t11i     = sin36*t3i-sin72*t4i
	 xr(j1+j) = t8r-t10i
	 xr(j4+j) = t8r+t10i
	 xi(j1+j) = t8i+t10r
	 xi(j4+j) = t8i-t10r
	 xr(j2+j) = t9r-t11i
	 xr(j3+j) = t9r+t11i
	 xi(j2+j) = t9i+t11r
	 xi(j3+j) = t9i-t11r
	 i = i+one
	 j = j+one
 140  continue
      j = j + jump
      do 160 k = li, m-li, li
         s1 = cs(k)
         c1 = cc(k)
         kk = k+k
         s2 = cs(kk)
         c2 = cc(kk)
         kk = kk+k
         s3 = cs(kk)
         c3 = cc(kk)
         kk = kk+k
         s4 = cs(kk)
         c4 = cc(kk)
         do 150 l = one, li
            t1r      = zr(i1+i)+zr(i4+i)
	    t3r      = zr(i1+i)-zr(i4+i)
	    t1i      = zi(i1+i)+zi(i4+i)
	    t3i      = zi(i1+i)-zi(i4+i)
	    t2r      = zr(i2+i)+zr(i3+i)
	    t4r      = zr(i2+i)-zr(i3+i)
	    t2i      = zi(i2+i)+zi(i3+i)
	    t4i      = zi(i2+i)-zi(i3+i)
	    t5r      = t1r+t2r
	    t6r      = sq5o4*(t1r-t2r)
	    t5i      = t1i+t2i
	    t6i      = sq5o4*(t1i-t2i)
	    z0r      = zr(i0+i)
	    t7r      = z0r-qrtr*t5r
	    xr(j0+j) = z0r+t5r
	    t8r      = t7r+t6r
	    t9r      = t7r-t6r
	    z0i      = zi(i0+i)
	    t7i      = z0i-qrtr*t5i
	    xi(j0+j) = z0i+t5i
	    t8i      = t7i+t6i
	    t9i      = t7i-t6i
	    t10r     = sin72*t3r+sin36*t4r
	    t11r     = sin36*t3r-sin72*t4r
	    t10i     = sin72*t3i+sin36*t4i
	    t11i     = sin36*t3i-sin72*t4i
	    x1r      = t8r-t10i
	    x4r      = t8r+t10i
	    x1i      = t8i+t10r
	    x4i      = t8i-t10r
	    x2r      = t9r-t11i
	    x3r      = t9r+t11i
	    x2i      = t9i+t11r
	    x3i      = t9i-t11r
            xr(j1+j) = c1*x1r - s1*x1i
            xi(j1+j) = s1*x1r + c1*x1i
            xr(j2+j) = c2*x2r - s2*x2i
            xi(j2+j) = s2*x2r + c2*x2i
            xr(j3+j) = c3*x3r - s3*x3i
            xi(j3+j) = s3*x3r + c3*x3i
            xr(j4+j) = c4*x4r - s4*x4i
            xi(j4+j) = s4*x4r + c4*x4i
	    i = i+one
	    j = j+one
 150     continue
         j = j + jump
 160  continue

      return
         
**
*     Now code for fac = 6.
**

 170  do 180 l = one, li
         t1r      = zr(i2+i)+zr(i4+i)
	 t3r      = sin60*(zr(i2+i)-zr(i4+i))
	 t1i      = zi(i2+i)+zi(i4+i)
	 t3i      = sin60*(zi(i2+i)-zi(i4+i))
	 t2r      = zr(i0+i)-half*t1r
	 t4r      = zr(i0+i)+t1r
	 t2i      = zi(i0+i)-half*t1i
	 t4i      = zi(i0+i)+t1i
	 t5r      = t2r-t3i
	 t6r      = t2r+t3i
	 t5i      = t2i+t3r
	 t6i      = t2i-t3r
	 t1r      = zr(i5+i)+zr(i1+i)
	 t3r      = sin60*(zr(i5+i)-zr(i1+i))
	 t1i      = zi(i5+i)+zi(i1+i)
	 t3i      = sin60*(zi(i5+i)-zi(i1+i))
	 t2r      = zr(i3+i)-half*t1r
	 t7r      = zr(i3+i)+t1r
	 t2i      = zi(i3+i)-half*t1i
	 t7i      = zi(i3+i)+t1i
	 t8r      = t2r-t3i
	 t9r      = t2r+t3i
	 t8i      = t2i+t3r
	 t9i      = t2i-t3r
	 xr(j0+j) = t4r+t7r
	 xr(j3+j) = t4r-t7r
	 xi(j0+j) = t4i+t7i
	 xi(j3+j) = t4i-t7i
	 xr(j4+j) = t5r+t8r
	 xr(j1+j) = t5r-t8r
	 xi(j4+j) = t5i+t8i
	 xi(j1+j) = t5i-t8i
	 xr(j2+j) = t6r+t9r
	 xr(j5+j) = t6r-t9r
	 xi(j2+j) = t6i+t9i
	 xi(j5+j) = t6i-t9i
	 i = i+one
	 j = j+one
 180  continue
      j = j + jump
      do 200 k = li, m-li, li
         s1 = cs(k)
         c1 = cc(k)
         kk = k+k
         s2 = cs(kk)
         c2 = cc(kk)
         kk = kk+k
         s3 = cs(kk)
         c3 = cc(kk)
         kk = kk+k
         s4 = cs(kk)
         c4 = cc(kk)
         kk = kk+k
         s5 = cs(kk)
         c5 = cc(kk)
         do 190 l = one, li
            t1r      = zr(i2+i)+zr(i4+i)
	    t3r      = sin60*(zr(i2+i)-zr(i4+i))
	    t1i      = zi(i2+i)+zi(i4+i)
	    t3i      = sin60*(zi(i2+i)-zi(i4+i))
	    t2r      = zr(i0+i)-half*t1r
	    t4r      = zr(i0+i)+t1r
	    t2i      = zi(i0+i)-half*t1i
	    t4i      = zi(i0+i)+t1i
	    t5r      = t2r-t3i
	    t6r      = t2r+t3i
	    t5i      = t2i+t3r
	    t6i      = t2i-t3r
	    t1r      = zr(i5+i)+zr(i1+i)
	    t3r      = sin60*(zr(i5+i)-zr(i1+i))
	    t1i      = zi(i5+i)+zi(i1+i)
	    t3i      = sin60*(zi(i5+i)-zi(i1+i))
	    t2r      = zr(i3+i)-half*t1r
	    t7r      = zr(i3+i)+t1r
	    t2i      = zi(i3+i)-half*t1i
	    t7i      = zi(i3+i)+t1i
	    t8r      = t2r-t3i
	    t9r      = t2r+t3i
	    t8i      = t2i+t3r
	    t9i      = t2i-t3r
	    xr(j0+j) = t4r+t7r
	    x3r      = t4r-t7r
	    xi(j0+j) = t4i+t7i
	    x3i      = t4i-t7i
	    x4r      = t5r+t8r
	    x1r      = t5r-t8r
	    x4i      = t5i+t8i
	    x1i      = t5i-t8i
	    x2r      = t6r+t9r
	    x5r      = t6r-t9r
	    x2i      = t6i+t9i
	    x5i      = t6i-t9i
            xr(j1+j) = c1*x1r - s1*x1i
            xi(j1+j) = s1*x1r + c1*x1i
            xr(j2+j) = c2*x2r - s2*x2i
            xi(j2+j) = s2*x2r + c2*x2i
            xr(j3+j) = c3*x3r - s3*x3i
            xi(j3+j) = s3*x3r + c3*x3i
            xr(j4+j) = c4*x4r - s4*x4i
            xi(j4+j) = s4*x4r + c4*x4i
            xr(j5+j) = c5*x5r - s5*x5i
            xi(j5+j) = s5*x5r + c5*x5i
	    i = i+one
	    j = j+one
 190     continue
         j = j + jump
 200  continue

      return

      end


************************************************************************
*                                                                      *
* FFTTAB - make fft tables                                             *
*                                                                      *
* Parameters:                                                          *
*    N		 - the length of the transform                         *
*    CC()	 - where to put cosines                                *
*    CS()	 - where to put sines                                  *
*                                                                      *
************************************************************************

      subroutine ffttab(n,cc,cs)
      integer n
      real cc(0:*), cs(0:*)

**
* Local variables:
*    I		 - counter
*    CCOS()	 - holds computed cosine
*    CSIN()	 - holds computed sine
*    DCC	 - holds cosine of delta angle
*    DCS	 - holds sine of delta angle
*    CCTEMP	 - temporary
*    CSTEMP	 - temporary
*    DOMEGA	 - delta angle
*    ZERO, FONE, ONE, IZERO, TWOPI - constants
**

      integer izero, one
      parameter (izero = 0, one = 1)
      
      double precision zero, fone, twopi
      parameter (zero = 0.0D0, fone = 1.0D0, 
     :           twopi = 6.283185307179586476925287D0)

**
* A note. Since we are using a trig identity to compute the cosines and
* sines, we had better use double precision to reduce errors.
**

      double precision ccos,csin,dcc,dcs,domega,cctemp,cstemp

      integer i
**
*     Code begins.
**

      ccos = fone
      csin = zero
      domega = twopi/real(n)
      dcc = dcos(domega)
      dcs = dsin(domega)
      
      do 10 i = izero, n - one
         cc(i) = ccos
         cs(i) = csin
         cctemp = ccos*dcc - csin*dcs
         cstemp = ccos*dcs + csin*dCC
         ccos = cctemp
         csin = cstemp
  10  continue

      return
      end


***********************************************************************
*                                                                     *
* FFTFAC - find least "expensive" number which factors as a           *
*          product of 2, 3, 4, 5, and 6 bigger than a given           *
*          number, and return factors                                 *
*                                                                     *
* Parameters:                                                         *
*    N		 - the number given                                   *
*    NEWN	 - the number produced                                *
*    NFAC	 - the number of factors of newn                      *
*    FAC()	 - holds factors of NEWN, passed to FFT               *
*                                                                     *
***********************************************************************

      subroutine fftfac(n,newn,nfac,fac)

      integer n,newn,nfac,fac(0:*)

**
* Local variables:
*    M		 - number under consideration for NEWN
*    MM		 - for calculations on m
*    ADDS, MULTS - store operation counts
*    FAC2()	 - hold factors temporarily
*    NFAC2	 - temporary nfac
*    MAXM	 -  the biggest m can get
*    COST1, COST2 - cost of fft in terms of floating multiplies
*                   and adds, cost1 contains local minimum
*    NEGONE, IZERO, ONE, TWO, THREE, FOUR, FIVE, SIX - constants
*    BIGINT	 - a big integer, machine dependent
*    LOOKSZ	 - how far ahead we can look
*             ***if you are planning to do large (n>4000) transforms,
*                set looksz to 200 or bigger***
**

      integer negone, izero, one, two, three, four,
     :        five, six, bigint, looksz
      parameter (negone = -1,
     :           izero = 0,
     :           one = 1,
     :           two = 2,
     :           three = 3,
     :           four = 4,
     :           five = 5,
     :           six = 6,
     :           bigint = 2**30,
     :           looksz = 500)

      integer m, mm, maxm, k, i, li, cost1, cost2, adds, mults,
     :        nfac2, fac2(0:20)
     
**
*     Code begins.
**

      cost1 = bigint
      m = n
      maxm = n + looksz

  10  mm = m
      li = one
      adds = izero
      mults = izero
      nfac2 = izero

      k = m / six
  20  if (mod(mm,six) .eq. izero) then
         fac2(nfac2) = six
         nfac2 = nfac2 + one
         adds = adds + k*36 + (k-li)*10
         mults = mults + k*8 + (k-li)*20
         li = li*six
         mm = mm / six
         go to 20
      endif

      k = m / five
  30  if (mod(mm,five) .eq. izero) then
         fac2(nfac2) = five
         nfac2 = nfac2 + one
         adds = adds + k*32 + (k-li)*8
         mults = mults + k*12 + (k-li)*16
         li = li*five
         mm = mm / five
         go to 30
      endif

      k = m / four
  40  if (mod(mm,four) .eq. izero) then
         fac2(nfac2) = four
         nfac2 = nfac2 + one
         adds = adds + k*16 + (k-li)*6
         mults = mults + (k-li)*12
         li = li*four
         mm = mm / four
         go to 40
      endif

      k = m / three
  50  if (mod(mm,three) .eq. izero) then
         fac2(nfac2) = three
         nfac2 = nfac2 + one
         adds = adds + k*12 + (k-li)*4
         mults = mults + k*4 + (k-li)*8
         li = li*three
         mm = mm / three
         go to 50
      endif

      k = m / two
      if (mod(mm,two) .eq. izero) then
         fac2(nfac2) = two
         nfac2 = nfac2 + one
         adds = adds + k*4 + (k-li)*2
         mults = mults + (k-li)*4
         mm = mm / two
      endif

**
*     Check for factorable M.
**
         
      if (mm .ne. one) then
         m = m + one
         go to 10
      endif

**
*     Calculate cost, the three is "machine dependent", use
*     (machine multiply time)/(machine add time) instead.
**

      cost2 = adds + three*mults
      

**
*     If cost lower, store as possibility, and keep looking.
**

      if (cost2 .lt. cost1) then
         cost1 = cost2
         do 60 i = izero, nfac2 - one
            fac(i) = fac2(i)
  60     continue
         newn = m
         nfac = nfac2
      endif

**
*     Ready to stop looking?
**

      if (m .lt. maxm) then
         m = m + one
         go to 10
      endif
**
*     All done.
**

      return
      end

