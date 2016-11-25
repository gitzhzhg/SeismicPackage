c      subroutine aqio
C<license>
C-------------------------------------------------------------------------------
C Copyright (c) 2007 ConocoPhillips Company
C
C Permission is hereby granted, free of charge, to any person obtaining a copy
C of this software and associated documentation files (the "Software"), to deal
C in the Software without restriction, including without limitation the rights
C to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
C copies of the Software, and to permit persons to whom the Software is
C furnished to do so, subject to the following conditions:
C
C The above copyright notice and this permission notice shall be included in all
C copies or substantial portions of the Software.
C
C THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
C IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
C FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
C AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
C LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
C OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
C SOFTWARE.
C-------------------------------------------------------------------------------
C</license>
C***************************** COPYRIGHT NOTICE ***********************
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION           *
C*                              OF CONOCO INC.                        *
C*                      PROTECTED BY THE COPYRIGHT LAW                *
C*                          AS AN UNPUBLISHED WORK                    *
C*                                                                    *
C***************************** COPYRIGHT NOTICE ***********************
C      1         2         3         4         5         6         7
C c234567890123456789012345678901234567890123456789012345678901234567890
C\USER DOC
C---------------------------------------------------------------------
C                     CONOCO PROCESSING SYSTEM
C               EXPLORATION RESEARCH & SERVICES DIVISION
C                            CONOCO, INC.
C
C Process name: aqio          aqio emulation
C      Author: Doug Hanson
C     Written: 94/10/14
C Last revised: 94/10/14 Hanson
C
C Purpose:  emulate the cray aqio open, read and write utilitys
C
C---------------------------------------------------------------------
C                         INPUT PARAMETERS
C
C Name  Default   Valid    Description
C----  -------   -----    -----------
C
C---------------------------------------------------------------------
C Primitive processes
C---------------------------------------------------------------------
C                               NOTES
C
C\END DOC
C\PROG DOC
C----------------------------------------------------------------------
C-
C                       REVISION HISTORY
C   Date     Author     Description
C   ----     ------     -----------
C 1. 94/10/14 Hanson    Original version adopted from DMIG.
C----------------------------------------------------------------------
C-
C                         CALLING SEQUENCE
C
C---------------------------------------------------------------------
C                               NOTES
C
C 1.
C---------------------------------------------------------------------
C    LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C                           IN THIS MODULE
C
C aqopen aqread aqwrite aqwait
C 
C***********SUMMARY OF ENTRY NAMES IN THIS MODULE ***********
C
C***********SUMMARY OF COMMON BLOCK NAME IN THIS MODULE ***********
C
C---------------------------------------------------------------------
C          LIST OF ALL EXTERNALS REFERENCED BY THIS MODULE
c  getlun
C
C---------------------------------------------------------------------
C\END DOC
C 
c234567890123456789012345678901234567890123456789012345678901234567890
      subroutine aqopen(iaq,naq,file,istat)
      character file*(*)
      call getlun(iaq,*999)
c      print'('' open iaq='',i5,'' file='',a40)',iaq,file
c      call strin_get_record_length(n_rec)
      n_rec = 1
      open(iaq,file=file,status='unknown'
     1,form='unformatted',access='direct',recl=512*n_rec,err=999)
      istat = 0
      return
  999 continue
      print*,' error in aqopen'
      istat = -1
      return
      end

c234567890123456789012345678901234567890123456789012345678901234567890
      subroutine aqwait(iaq,istat)
      istat = 0
      return
      end

c234567890123456789012345678901234567890123456789012345678901234567890
      subroutine aqwrite(iaq,a,i,n,j1,j2,istat)
      character fileaq*64
      dimension a(1)
      data nerr/0/
c      write(88,*)' aqwrite iaq=',iaq,' i=',i,' n=',n,' irec=',i*n+1
c      print*,' aqwrite iaq=',iaq,' i=',i,' n=',n,' irec=',i*n+1
      do j = 1 , n
        j0 = j
c      write(88,*)' aqwrite rec=',i+j
c      print*,' aqwrite rec=',i+j
        write(iaq,rec=i+j,err=999)(a(k),k=(j-1)*512+1,j*512)
      enddo    ! do j = 1 , n
      istat = 0
      return
  999 continue
      print'('' error in aqwrite i='',i10,'' n='',i10,'' j='',i10)'
     1,i,n,j0
      istat = -1
      nerr = nerr + 1
      if (nerr .ge. 100)stop
      return
      end

c234567890123456789012345678901234567890123456789012345678901234567890
      subroutine aqread(iaq,a,i,n,j1,j2,istat)
      dimension a(1)
c      print*,' aqread iaq=',iaq,' i=',i,' n=',n,' irec=',i+1
      do j = 1 , n
        j0 = j
        read(iaq,rec=i+j,err=999)(a(k),k=(j-1)*512+1,j*512)
      enddo    ! do j = 1 , n
      istat = 0
      return
  999 continue
      print'('' error in aqread i='',i10,'' n='',i10,'' j='',i10)'
     1,i,n,j0
      istat = -1
      return
      end

CXXX TOP OF RCFFT ROUTINES
c234567890123456789012345678901234567890123456789012345678901234567890
      subroutine scfft(i_sign,n_fft
     1,scale,x_inp,x_out,table,work,j_sys)

      init = 0
      call rcfft2(init,i_sign,n_fft,x_inp,work,x_out)
      return
      end

      subroutine csfft(i_sign
     1,n_fft,scale,x_inp,x_out,table,work,j_sys)
      init = 0
      call crfft2(init,i_sign,n_fft,x_inp,work,x_out)
      return
      end

      subroutine ccfft(i_sign,n_fft,x_inp,x_out,work,j_sys)
      call util_copy(n_fft*2,x_inp,x_out)
      call clib_fft(i_sign,n_fft,x_out)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rcfft2(init,idir,n,dr,work,dc)
c  take a real to complex fft - use rtdmcrft for complex to real
c  this is a shell to allow use of the same fft routines on the cray or vax
c  if icray = 1 run on cray if icray= 0 run on vax
c  note flag icray in common block craysys must be set
c
c  scaling is such that data recquires 1. / (2 * n) where n is number
c  of points in real to complex fft 
c
c  on cray work is an array of coeeficients and needs to be initialized 
c  (see rtdmflts for example) of init = 1 needs 3*n + 4 real elements
c  note on cray work must not be changed between calls to rtdmrcft.  
c  note also if forward and invers fft's are different lengths you must
c  use two different work arrays with 2 different initialization calls for.
c
c  on the vax work is a scratch array used to make a complex trace out 
c  of a real trace.  hence work can be changed between calls to rtdmrcft
c
c  init = initialization flag +1,-1 = initialize coefficients 0 = take fft
c  idir exponent for transform +1,-1 are valid - i use +1 for real to complex
c  n = number of points in fft must be power of 2
c  dr = real array in has n real elements
c  work = work array on cray needs 3*n+4 and must not be changed
c                    on vax needs 2*n and is temporary
c  dc = complex data out has n/2+1 complex elements
c
      real dr(1)
      complex work(1),dc(1)
      if (init .ne. 0) return
      do 1 i = 1 , n
        work(i) = cmplx(dr(i),0.)
    1 continue
      call clib_fft(idir,n,work)
      scale = sqrt(float(n))
      do 2 i = 1 , n/2+1
        dc(i) = scale * work(i)
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine crfft2(init,idir,n,dc,work,dr)
c  take a complex to real fft
      real dr(1)
      complex work(1),dc(1)
      if (init .ne. 0) return
      j = n
      do 1 i = 2 , n/2
        work(i) = dc(i)
        work(j) = conjg(dc(i))
        j = j - 1
    1 continue
      work(1) = dc(1)
      work(n/2+1) = dc(n/2+1)
      call clib_fft(idir,n,work)
      scale = sqrt(float(n))
      do 2 i = 1 , n
        dr(i) = scale * real(work(i))
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
        subroutine clib_fft(id,nf,f)
c  f - array to be transformed
c  nf - number of points in trace
c  ln - two power. 
c  id = -1 forward transform , 1 - inverse transform
c  if f is real fill imaginary part with zeros
        complex f(*),c1,ce,tmp
	ln=int(.9999+log(float(nf))/log(2.0))
c	print*,' sub fft nf=',nf,' ln=',ln,' id=',id
	pi=2.0*asin(1.0)
        n=2**ln
	do 100 in = nf + 1 , n
	f(in) = cmplx(0.,0.)
 100	continue
        z1=n
        n2=n/2
        nm1=n-1
        j=1
        do 30 i=1,nm1
        if(i .ge. j)go to 10
        tmp=f(j)
        f(j)=f(i)
        f(i)=tmp
 10     continue
        k=n2
 20     continue
        if(k .ge. j)go to 35
        j=j-k
        k=k/2
        go to 20
 35     continue
        j=j+k
 30     continue
        do 50 l=1,ln
        le=2**l
        le1=le/2
        c1=cmplx(1.0,0.0)
        ce=cmplx(cos(pi/le1),id*sin(pi/le1))
        do 50 j=1,le1
        do 40 i=j,n,le
        ip=i+le1
        tmp=f(ip)*c1
        f(ip)=f(i)-tmp
        f(i)=f(i)+tmp
 40     continue
        c1=c1*ce
 50     continue
        do 60 i=1,n
        if(id .eq. 1)f(i)=f(i)/z1
 60     continue
        return
        end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pack21(n,x_inp,x_out,i)
c  pack x_inp into x_out
      implicit none
      integer  n,i
      real     x_inp(1),x_out(1)

      integer  j

      do j = 1 , n
        x_out(j) = x_inp(j)
      enddo    ! do j = 1 , n

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine expand21(n,x_inp,x_out,i)
c  unpack x_inp into x_out
      implicit none
      integer  n,i
      real     x_inp(1),x_out(1)

      integer  j

      do j = 1 , n
        x_out(j) = x_inp(j)
      enddo    ! do j = 1 , n

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine second(cpu)
c  return current cpu time
      implicit none
      real     cpu

      cpu = 0.

      return
      end

c&&&
c&&&
c  the following are working routinss that could replace cray library routines
      subroutine rcsum(nx,x1,x2)
c  recursive sum
      implicit none
      integer nx
      real     x1(nx),x2(nx)

      integer  ix
      real     x_sum

      if (nx .ge. 1) then

        x_sum = 0.

        do ix = 1 , nx

          x_sum = x_sum + x1(ix)
          x2(ix) = x_sum

        enddo

      endif    ! if (nx .ge. 1) then

      return
      end


c&&&
c  the following are shells for cray library routines
      subroutine mcfft()
      return
      end

      integer function shiftl()
      return
      end

      integer function shiftr()
      return
      end

C     integer function loc()
C     return
C     end

      integer function ishell()
      return
      end

C     real function mpi_wtime()
C     mpi_wtime = 0.
C     return
C     end

C     subroutine mpi_init(mpi_return)
C     return
C     end

C     subroutine mpi_comm_rank(mpi_comm_world,i_pel,mpi_return )
C     return
C     end

C     subroutine mpi_comm_size(mpi_comm_world,n_pel,mpi_return )
C     return
C     end

C     subroutine mpi_barrier(mpi_comm_world,mpi_return)
C     return
C     end

C     subroutine mpi_finalize(mpi_return)
C     return
C     end

      subroutine tsecnd(cpu)
      cpu=0.
      return
      end

      subroutine tremain(cpu)
      cpu=1e6
      return
      end
