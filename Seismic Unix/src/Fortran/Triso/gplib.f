* These routines are from the gplib library as revised by K. Araya
* and Sebastien Geoltrain
*
* We went in and quickly trapped a bunch of potentially illegal values,
* however, this stuff should really be re-coded to be passing in
* array lengths instead of hard-wiring work areas, etc.  In the
* pre-release version, a bad value of nsweep in klaud86 could send
* the computations into limbo.  Credit to Darrell Kramer of Mobil on
* this and some related problems.  Jack Cohen, 08/04/89
*
*-------------------------------------------------------------------
* This subroutine performs the one-sided cross correlation of
* two functions by FFT, Bartlet truncation:
*
* Usage:
*     call corr86(x,lx,y,ly,z,lz)
*
*     x          the first array
*     lx         the length of the array x
*     y          the second array to move (*)
*     ly         the length of the array y
*     z          the output array
*     lz         length of the output array z
*
* Note:
*     lx + lz - 1 must be less than or equal to 8192.
*-----------------------------------------------------------------------
*
* Author:
* Source: /src/public/gplib
* Revision: Sebastien Geoltrain Jan.22,1989
*----------------------------------------------------------------------

      subroutine corr86(x,lx,y,ly,z,lz)

      real x(1),y(1),z(1),times
      integer lx,ly,lz,n,i,leng
      complex    w1(8192),w2(8192),w3(8192)

*    	check length parameters

	if(lx .LE. 0 .OR. ly .LE. 0) then
		stop ' corr86: illegal lx or ly'
	endif

*    	computes length of output as closest power of 2, not to 
*	exceed 2**13 (i.e. 8192).

	leng=lx+ly-1

        n=2

        do 10 i=1,13
           if(leng.le.n)then
              leng = n
              go to 1200
           else
              n = n * 2
           endif
10      continue

1200    continue

        if (lx .gt. 8192 .or. ly .gt. 8192 .or. leng .gt. 8192) then
	   stop ' corr86: illegal lx, ly or leng'
	endif

*	clears work arrays

	do 3000 i=1,leng
	   w1(i)=cmplx(0.0,0.0)
	   w2(i)=cmplx(0.0,0.0)
	   w3(i)=cmplx(0.0,0.0)
3000	continue

*    	moves input to work arrays

	do 4000 i=1,lx
	   w1(i)=cmplx(x(i),0.0)
4000	continue

	do 4100 i=1,ly
	   w2(i)=cmplx(y(i),0.0)
4100	continue

*	Fourier transforms both inputs

	call fft85(w1,leng,-1)
	call fft85(w2,leng,-1)

*     	takes complex conjugate of second input

	do 4200 i=1,leng
       	   w2(i)=conjg(w2(i))
4200	continue

*	multiplies first input and conjugate of second input 
*	in frequency domain

	do 4300	i=1,leng
	   w3(i)=w1(i)*w2(i)
4300	continue

*    	performs inverse FFT of resulting product

	call fft85(w3,leng,1)

*    	outputs result as real array of length lz

	times=sqrt(float(leng))

	do 4400	i=1,lz
	   z(i)=times*real(w3(i))
4400	continue

        return
	end
*--------------------------------------------------------------
* This subroutine computes the FFT of a function:
*
* Usage:
*     call fft85(cx,lx,sign)
*
*     cx         the input array (complex)
*                after the calling it is the output.
*     lx         the length of the input array
*     sign       (-1) forward transform
*                (1)  inverse transform
*-----------------------------------------------------------------
*
* Author: J. Claerbout
* Source: /src/public/gplib
* Revision: Sebastien Geoltrain Jan.22,1989
*-----------------------------------------------------------------
      subroutine fft85(cx,lx,sign)
      integer sign,lx,i,j,l,istep,m
      real sc
      complex  cx(lx),carg,cw,ctemp

      j=1
      sc=sqrt(1.e0/lx)

      do 30 i=1,lx

          if(i.gt.j) goto 10

          ctemp=cx(j)*sc
          cx(j)=cx(i)*sc
          cx(i)=ctemp

10        m=lx/2

20        if(j.le.m) goto 30

          j=j-m
          m=m/2
          if(m.ge.1) goto 20

30    j=j+m

      l=1

40    istep=2*l

      do 50 m=1,l

          carg=(0.e0,1.e0)*(3.14159265e0*sign*(m-1))/l
          cw=cexp(carg)

          do 50 i=m,lx,istep

              ctemp=cw*cx(i+l)
              cx(i+l)=cx(i)-ctemp

50    cx(i)=cx(i)+ctemp

      l=istep

      if(l.lt.lx) goto 40

      return
      end
*---------------------------------------------------------------------
* This subroutine generates a two-sided Klauder wavelet
* by a specified frequency array:
*
* Usage:
*     call klaud86(wave,leng,del,nsweep,fl,fh,ialfa)
*
*     wave       array
*     leng       the length of the generated Klauder
*                wavelet (should be an odd number)
*                hardwired at 255 in trisubs.getarray
*     del        sampling rate in seconds
*     nsweep     length of original vibroseis sweep (le. 8192)
*     fl         lower cut frequency
*     fh         higher cut frequency
*     ialfa      taper length (GE.2.and.le.500)
*-----------------------------------------------------------------------
*
* Author:
* Source: /src/public/gplib
* Revision: Sebastien Geoltrain Jan.22,1989
*----------------------------------------------------------------------

      subroutine klaud86(wave,leng,del,nsweep,fl,fh,ialfa)
      real wave(1),sweep(8192),w1(1024),fl,fh,del
      integer i,leng,lengs,lengh,lengh1,nsweep,ialfa,ifirst

*	parameter checks

*	if length is too big or too small, exit

 	if(leng .LE. 2 .OR. leng .GT. 1024) then
 		stop ' klauder86: leng illegal'
 	endif
 
*	if length is even, resets to closest smaller odd number

 	if(mod(leng,2).eq.0) then
 	      leng=leng-1
         endif
 
* 	if sampling rate is negative or null, exit

 	if(del .LE. 0.e0) then
 		stop ' klauder86: del illegal'
 	endif

*	if sweep is too long or too short, exit

	if(nsweep .LE. 0 .OR. nsweep .GT. 8192) then
		stop ' klauder86: nsweep illegal'
	endif

*	if taper is too small or too large, exit

	if(ialfa .LT. 2 .OR. ialfa .GT. 500) then
		stop ' klauder86: ialfa illegal'
	endif 

*	generates vibroseis sweep

	lengs = nsweep

        call vibro86(sweep,lengs,del,fl,fh,ialfa)

*    	crosscorrelates the sweep

	lengh=leng/2+1

	call corr86(sweep,lengs,sweep,lengs,w1,lengh)

	ifirst=2

	call norm85(w1,lengh,1,lengh,ifirst)

*	makes output into a two-sided function

        lengh1=lengh-1

        do 100 i=1,lengh1
              wave(lengh+i)=w1(i+1)
              wave(lengh-i)=w1(i+1)
100     continue

        wave(lengh)=1.e0

	return
	end
*--------------------------------------------------------------------------
* This subroutine normalizes an array by its RMS energy, or the
* first value in the array or by the largest magnitude in the 
* Array:
*
* Usage:
*     call norm85(x,lx,is,ie,in)
*
*     x		the input array
*     lx	the length of array
*     is        the initial point to generate factor
*     ie        the last point to generate factor
*     in        operation flag
*               = 1 normalizes by the RMS energy
*               = 2 normalizes by the first value
*               = 3 normalizes by the largest magnitude
*---------------------------------------------------------------------------
*
* Author:
* Source: /src/public/gplib
* Revision: Sebastien Geoltrain Jan.22,1989
*---------------------------------------------------------------------------

      subroutine norm85(x,lx,is,ie,in)

      integer lx,is,ie,in,i
      real x(lx),b,x1,p

*     test input parameters

      if(lx .LT. 1 .OR. is .GE. ie .OR. is .LE. 0 .OR. ie .GT. lx) then
	  stop ' norm85: illegal parameters'
      endif

      if(in .EQ. 1) then

               p=0.e0

               do 150 i=is,ie
                      p=p+x(i)*x(i)
150            continue

               p=sqrt(p)

               do 160 i=1,lx
                      x(i)=x(i)/p
160            continue

       else if(in .EQ. 2) then

               x1=x(is)

               if(x1 .NE. 0.e0) then

                      do 250 i=1,lx
                             x(i)=x(i)/x1
250                   continue

               endif

        else if(in .EQ. 3) then

               b=0.e0

               do 350 i=is,ie
                      b=amax1(abs(x(i)),b)
350            continue

               do 360 i=1,lx
                      x(i)=x(i)/b
360            continue

	endif

      return
      end
c------------------------------------------------------
	subroutine fasthilbert(p,q,ntrace)
	integer n,ntrace,i2,l1,l2
	real p(ntrace),q(ntrace),h(32),a,b

	call hilbertcoef(h)

	do5 i2=1,ntrace
5	q(i2) = 0.0
	do20 n=1,ntrace
	do30 i2=1,32,2
	l1=n-i2
	l2=n+i2
	if(l1.le.0.or.l1.gt.ntrace) then
	a = 0.0
	else
	a = p(l1)
	endif
	if(l2.le.0.or.l2.gt.ntrace) then
	b = 0.0
	else
	b = p(l2)
	endif
30	q(n) = q(n) + h(i2)*(a-b)
20	continue
	return
	end
c
	subroutine hilbertcoef(h)
	real h(32),opi,pi
	integer i2
	pi = 3.1415927
	opi = 2./pi
	do10 i2 = 1,32,2
10  	h(i2) = -opi/i2*(0.42+0.52*cos(pi*i2/32.)+.08*cos(pi*i2/16.))
	return
	end
c
*-----------------------------------------------------------
* This subroutine generates a vibroseis linear sweep:
*
* Usage:
*     call vibro86(sweep,leng,del,fa,fb,ltaper)
*
*     sweep       the generated sweep output array
*     leng        length of sweep: samples
*     del         sampling rate, in seconds
*     fa          starting cut-off frequency (no restriction)
*                 in hertz
*     fb          ending cut-off frequency (no restriction)
*                 in hertz
*     ltaper      sample length of 2 quadrant cosine taper
*                 (0 = no taper)
*--------------------------------------------------------------
*
* Author:
* Source: /src/public/gplib
* Revision: Sebastien Geoltrain Jan.22,1989
*--------------------------------------------------------------

      subroutine vibro86(sweep,leng,del,fa,fb,ltaper)

      integer i,leng,ltaper,l
      real sweep(leng),fa,fb,t,f,durat,fbad,pi,pi2,weight,del

      pi = 3.1415927e0

      pi2=pi*2.e0

*     tests input

      if(del .LT. 0.e0)then 
         stop ' vibro86: illegal del'
      endif

*     computes sweep

      durat=(leng-1.e0)*del
      fbad=(fb-fa)/(durat*2.e0)

      do 10 i=1,leng
         t = (i-1)*del
         f = fa+fbad*t
         sweep(i) = cos(pi2*f*t)
10    continue

*     applies taper

      do 20 i=1,ltaper-1
         l=leng+1-i
         weight=.5e0-.5e0*cos(pi*(i-1)/(ltaper-1) )
         sweep(i)=sweep(i)*weight
         sweep(l)=sweep(l)*weight
20    continue

      return
      end
