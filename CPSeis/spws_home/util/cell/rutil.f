
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
C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_array_size(nx,x0,dx,n,x,i_err)
c  determine the regularity of a set of numbers
      implicit none

      integer  nx,n,i_err
      real     x0,dx,x(n)

      integer  i
      real     x_min,x_max,eps

      call util_min_max(x_min,x_max,n,x)
      nx = 1
      x0 = x_min
      dx = 1.

      if (x_min .eq. x_max) then

        if (n .le. 0) nx = 0

      else    ! if (x_min .eq. x_max) then

        eps = (x_max - x_min) / 10000.
        dx = x_max - x_min

        do i = 2 , n

          if (abs(x(i)-x(i-1)) .gt. eps)
     1dx = min(dx,abs(x(i)-x(i-1)))

 
        enddo    ! do 1 = 1 , n

        nx = nint((x_max - x_min) / dx) + 1

      endif    ! if (x_min .eq. x_max) then

c      print'(/,'' rmod_array_size''
c     1,/,'' n    ='',i10,'' x_min='',G14.7,'' x_max='',G14.7
c     1,/,'' nx   ='',i10,'' x0   ='',G14.7,'' xl   ='',G14.7
c     1,'' dx='',G14.7)'
c     1,n,x_min,x_max,nx,x0,(nx-1)*dx+x0,dx

      if (mod(n,nx) .ne. 0 .or. abs((nx-1)*dx+x0-x_max) .gt. eps) then

        print'(/,'' rmod_array_size''
     1,/,'' n    ='',i10,'' x_min='',G14.7,'' x_max='',G14.7
     1,/,'' nx   ='',i10,'' x0   ='',G14.7,'' xl   ='',G14.7
     1,'' dx='',G14.7)'
     1,n,x_min,x_max,nx,x0,(nx-1)*dx+x0,dx
        print'('' error in rmod_array_size - grid is not regular'')'
        i_err = i_err + 1
      endif    ! if (mod(n,nx) .ne. 0) then

      return
      end

