C      subroutine util
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
C Process name: UTIL   utility routines 
C      Author: Douglas Hanson
C     Written: 96/04/01
C Last revised: 96/04/01 Hanson
C
C Purpose:  Common utility routines
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
C 1. 96/04/01 Hanson    Original version 
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
C***********SUMMARY OF SUBROUTINE NAMES IN THIS MODULE ***********
C
C UTIL                 UTIL_SETR            UTIL_SETR_INC       
C UTIL_COPY            UTIL_INVERT          UTIL_MIN_MAX        
C UTIL_OPEN_FILE       UTIL_CLOSE_FILE      UTIL_GET_FILE_NAME  
C UTIL_ADD_CHARACTER   UTIL_ADD_EXT         UTIL_WORK           
C UTIL_TRANSFORM       UTIL_TRANSFORM_1D    UTIL_TRANSFORM_1      
C UTIL_GET_LUN         UTIL_CAPS            UTIL_DCOD           
C UTIL_HEYU            UTIL_LENL            UTIL_LENP           
C UTIL_LENR            UTIL_decode_value_1     UTIL_decode_value_2    
C UTIL_decode_r        UTIL_CADP            UTIL_CLFT           
C UTIL_ERROR           UTIL_WRITE_CARD      UTIL_ENCODE_CARD    
C UTIL_DELETE_BLANKS  
C
C***********SUMMARY OF ENTRY NAMES IN THIS MODULE ***********
C
C UTIL_ADD_EXTRP      
C UTIL_ADD_EXT_REPLACE .EQ.1)               UTIL_WORS           
C UTIL_WORL            UTIL_WORU            UTIL_WORC           
C UTIL_GET_LUN_S       UTIL_HEYD            UTIL_PLIN           
C UTIL_GLIN            UTIL_PORL            UTIL_GORL           
C UTIL_SET_REWIND      UTIL_SET_NO_REWIND   UTIL_decode_i    
C UTIL_decode_c   UTIL_GET_DEVICE      UTIL_PUT_DEVICE     
C 
C***********SUMMARY OF FUCNTION NAMES IN THIS MODULE ***********
C
C UTIL_INVERT_1        util_r           UTIL_SPAC           
C UTIL_FETCH_I         UTIL_FETCH_R         UTIL_FETCH_D        
C UTIL_FETCH_C         UTIL_RD              UTIL_DR             
C 
C***********SUMMARY OF COMMON BLOCK NAMES IN THIS MODULE ***********
C
C---------------------------------------------------------------------
C          LIST OF ALL EXTERNALS REFERENCED BY THIS MODULE
C
C---------------------------------------------------------------------
C\END DOC
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_setr(n,x,x0)
      implicit  none
      integer  i,n
      real     x(1),x0
      do i = 1 , n
        x(i) = x0
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_setr_inc(i,n,x,x0)
c  set an array to a constant
      implicit  none
      integer  n,i,j
      real     x(i,1),x0
      do j = 1 , n
        x(1,j) = x0
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_seti(n,x,x0)
      implicit  none
      integer  i,n
      integer  x(1),x0
      do i = 1 , n
        x(i) = x0
      enddo
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_seth8(nx,x,x0)
c  set an 8 character variable, x to a constant value x0
      implicit  none

      integer  util_len_r

      integer  nx
      character x(nx)*8
      character x0*(*)

      integer   lx0,ix

      lx0 = min(8,util_len_r(x0))

      do ix = 1 , nx

        x(ix) = x0(1:lx0)

      enddo    ! do ix = 1 , nx

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_seth4(nx,x,x0)
c  set an 4 character variable, x to a constant value x0
      implicit  none

      integer  util_len_r

      integer  nx
      character x(nx)*4
      character x0*(*)

      integer   lx0,ix

      lx0 = min(4,util_len_r(x0))

      do ix = 1 , nx

        x(ix) = x0(1:lx0)

      enddo    ! do ix = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_line(n,x,x0,dx)
      implicit  none
      integer  i,n
      real     x(1),x0,dx
      do i = 1 , n
        x(i) = x0 + (i - 1 ) * dx
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_lini(n,x,x0,dx)
      implicit  none
      integer  i,n
      integer  x(1),x0,dx
      do i = 1 , n
        x(i) = x0 + (i - 1 ) * dx
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_add_vectors(nx,x1,x2,x3)
c  x3 = x1 + x2
      implicit  none

      integer  nx
      real     x1(1),x2(1),x3(1)
      integer  ix

      do ix = 1 , nx

        x3(ix) = x1(ix) + x2(ix)

      enddo    ! do ix = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_multiply_vectors(nx,x1,x2,x3)
c  x3 = x1 * x2
      implicit  none

      integer  nx
      real     x1(1),x2(1),x3(1)
      integer  ix

      do ix = 1 , nx

        x3(ix) = x1(ix) * x2(ix)

      enddo    ! do ix = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_mulitply_complex(nx,x1,x2,x3)
c  multiply complex vectors
      implicit  none

      integer  nx
      complex  x1(nx)
      complex  x2(nx)
      complex  x3(nx)

      integer  ix

      do ix = 1 , nx

        x3(ix) = x1(ix) * x2(ix)

      enddo    ! do ix = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_copy(nx,x1,x2)
c  copy x1 to x2
      implicit  none

      integer  nx
      real     x1(1),x2(1)
      integer  ix

      do ix = 1 , nx

        x2(ix) = x1(ix)

      enddo    ! do ix = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_copy_r(nx,x1,x2)
c  copy x1 to x2 in reverse order
c  this can not be in place
      implicit  none

      integer  nx
      real     x1(1),x2(1)
      integer  ix1,ix2

      ix2 = nx
      do ix1 = 1 , nx

        x2(ix2) = x1(ix1)
        ix2 = ix2 - 1

      enddo    ! do ix1 = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_copi(nx,x1,x2)
c  copy x1 to x2
      implicit  none

      integer  nx
      integer  x1(1),x2(1)
      integer  ix

      do ix = 1 , nx

        x2(ix) = x1(ix)

      enddo    ! do ix = 1 , nx

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_copy_inc(nx,inc1,x1,inc2,x2)
c  copy n values of array x1 to array x2
      implicit  none
      integer  nx

      integer  inc1
      real     x1(1)

      integer  inc2
      real     x2(1)

      integer  ix,ix1,ix2

      ix1 = 1
      ix2 = 1

      do ix = 1 , nx

        x2(ix2) = x1(ix1)
        ix1 = ix1 + inc1
        ix2 = ix2 + inc2

      enddo    ! do ix = 1 , nx

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_copy_inc_c(nx,inc1,x1,inc2,x2)
c  copy n values of array x1 to array x2
      implicit  none
      integer  nx

      integer  inc1
      complex  x1(1)

      integer  inc2
      complex  x2(1)

      integer  ix,ix1,ix2

      ix1 = 1
      ix2 = 1

      do ix = 1 , nx

        x2(ix2) = x1(ix1)
        ix1 = ix1 + inc1
        ix2 = ix2 + inc2

      enddo    ! do ix = 1 , nx

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_copy_inc_2d(nx,ny,nx_dim_1,x1,nx_dim_2,x2)
c  copy n values of array x1 to array x2
      implicit  none
      integer nx,ny,nx_dim_1,nx_dim_2
      real    x1(nx_dim_1,1),x2(nx_dim_2,1)
      integer ix,ix_1,ix_2,iy,iy_1,iy_2

c  if x1 will     over write x2 ...
      if (nx_dim_1 .ge. nx_dim_2) then
        ix_1 = 1
        ix_2 = nx
        iy_1 = 1
        iy_2 = ny
c  if x1 will not over write x2 ...
      else    ! if (nx_dim_1 .ge. nx_dim_2) then
        ix_1 = nx
        ix_2 = 1
        iy_1 = ny
        iy_2 = 1
      endif    ! if (nx_dim_1 .ge. nx_dim_2) then

      do iy = iy_1 , iy_2 , sign(1,iy_2-iy_1)
        do ix = ix_1 , ix_2 , sign(1,ix_2-ix_1)
          x2(ix,iy) = x1(ix,iy)
        enddo    ! do do ix = ix_1 , ix_2 , sign(1,ix_2-ix_1)
      enddo    ! do iy = iy_1 , iy_2 , sign(1,iy_2-iy_1)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_copy_d(n,x1,x2)
      implicit         none
      integer          n
c      double precision x1(1),x2(1)
      real             x1(1),x2(1)
      integer  i

      do i = 1 , n
        x2(i) = x1(i)
      enddo    ! do i = 1 , n

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_copy_abs(nx,x1,x2)
c  copy x1 to x2
      implicit  none

      integer  nx
      real     x1(1),x2(1)
      integer  ix

      do ix = 1 , nx

        x2(ix) = abs(x1(ix))

      enddo    ! do ix = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_power(nx,x,power)
c  raise x to a power
      implicit  none

      integer  nx
      real     x(1),power

      integer  ix

      if (power .eq. 0.) then

        call util_setr(nx,x,1.)

      elseif (power .eq. 1.) then

      elseif (power .eq. -1.) then

        call util_invert(nx,x)

      elseif (power .eq. 2.) then

        do ix = 1 , nx

          x(ix) = x(ix) * x(ix)

        enddo    ! do ix = 1 , nx

      elseif (power .eq. -2.) then

        call util_invert(nx,x)

        do ix = 1 , nx

          x(ix) = x(ix) * x(ix)

        enddo    ! do ix = 1 , nx

      else

        do ix = 1 , nx

          x(ix) = x(ix) ** power

        enddo    ! do ix = 1 , nx

      endif

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_scale(nx,x,scale)
c  scale x by scale
      implicit  none

      integer  nx
      real     x(1),scale
      integer  ix

      do ix = 1 , nx

        x(ix) = scale * x(ix)

      enddo    ! do ix = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_normalize(nx,x,x0)
c  normalize an array to a value
c  x = array to normalize
c  nx = number of elements in array x
c  x0 = value to normalize to
      implicit  none

      integer  nx
      real     x(nx)
      real     x0

      real     x_max

      call util_abs_max(x_max,nx,x)
      if (x_max .ne. 0.0) call util_scale(nx,x,x0/x_max)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_invert(n,x)
      implicit  none
      integer  i,n
      real     x(1)
      do i = 1 , n
        if (x(i) .ne. 0.) x(i) = 1. / x(i)
      enddo
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_imin(imin,imax,n,x)
c  determine min,max locations

      implicit  none

      integer imin,imax,n
      real x(n)
      integer i

      imin = 1
      imax = 1
      do i = 1 , n
        if (abs(x(i)) .lt. abs(x(imin))) imin = i
        if (abs(x(i)) .gt. abs(x(imax))) imax = i
      enddo
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_cave(x_ave,x_max,n,x)
c  determine average, max real amplitude of complex array

      implicit  none

      integer n
      real x_ave,x_max
      complex x(n)
      integer i

      x_ave = 0.
      x_max = 0.
      if (n .le. 0) return
      do i = 1 , n
c        x_ave = x_ave + sqrt(real(x(i))**2+aimag(x(i))**2)
        x_ave = x_ave + abs(real(x(i)))
        x_max = max(x_max,abs(real(x(i))))
      enddo
      x_ave = x_ave / n

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_min_max(x_min,x_max,n,x)
      implicit  none
      integer  i,n
      real     x(1),x_min,x_max
      x_min = 0.
      x_max = 0.
      if (n .eq. 0) return

      x_min = x(1)
      x_max = x(1)
      do i = 1 , n
        x_min = min(x_min,x(i))
        x_max = max(x_max,x(i))
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_min_max_i(x_min,x_max,n,x)
      implicit  none
      integer  i,n
      integer  x(1),x_min,x_max
      x_min = 0
      x_max = 0
      if (n .eq. 0) return

      x_min = x(1)
      x_max = x(1)
      do i = 1 , n
        x_min = min(x_min,x(i))
        x_max = max(x_max,x(i))
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      real function util_amax(n,x)
      implicit  none
      integer  n
      real     x(1)
      real     x_max

      call util_abs_max(x_max,n,x)
      util_amax = x_max

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_pause(title,i_flag)
      implicit  none

      integer  util_r

      character title*(*)
      integer  i_flag
      character  crd01*1

      print'(/,a,/,'' enter carriage return to continue''
     1,/,'' i_flag='',i10)'
     1,title(1:util_r(title)),i_flag
      read(*,'(a)',err=1,end=1) crd01
    1 continue

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_stop(title,i_flag)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer   util_r

      character title*(*)
      integer   i_flag

      write(util_prn(),'(/,a,/,'' stopping i_flag='',i10)')
     1title(1:util_r(title)),i_flag

      if (util_prn() .ne. 6)
     1write(6         ,'(/,a,/,'' stopping i_flag='',i10
     1,'' print unit='',i8,'' error unit='',i8)')
     1title(1:util_r(title)),i_flag,util_prn(),util_err()

      stop
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_pmax(title,lu_out,n,x)
      implicit  none

      integer  util_r

      character title*(*)
      integer  lu_out,n
      real     x(1)
      integer  ix_min,ix_max
      real     x_min
      real     x_max

      call util_min_max_loc(ix_min,ix_max,n,x)
c      call util_min_max(x_min,x_max,n,x)

      write(lu_out,'('' n='',i8
     1,'' min='',i8,1x,g14.6
     1,'' max='',i8,1x,g14.6
     1,1x,a8
     1)')
     1 n
     1,ix_min,x(ix_min)
     1,ix_max,x(ix_max)
     1,title(1:min(8,util_r(title)))
c      write(lu_out,*)
c     1 ' n=',n
c     1,' min=',ix_min,x(ix_min)
c     1,' max=',ix_max,x(ix_max)
c     1,'t=',title(1:min(8,util_r(title)))

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_imax(title,lu_out,n,x)
      implicit  none

      integer  util_r

      character title*(*)
      integer  lu_out,n
      integer  x(1)
      integer  x_min
      integer  x_max

      call util_min_max_i(x_min,x_max,n,x)

      write(lu_out,'(1x,a20,'' n='',i8,'' min='',i16,'' max='',i16
     1)')title(1:min(20,util_r(title))),n,x_min,x_max

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      real function util_amin(n,x)
      implicit  none
      integer  n
      real     x(1)
      real     x_min

      call util_abs_min(x_min,n,x)
      util_amin = x_min

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_abs_max(x_max,n,x)
      implicit  none
      integer  i,n
      real     x(1),x_max
      x_max = 0.
      if (n .eq. 0) return

      x_max = abs(x(1))
      do i = 1 , n
        x_max = max(x_max,abs(x(i)))
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_abs_min(x_min,n,x)
      implicit  none
      integer  i,n
      real     x(1),x_min
      x_min = 0.
      if (n .eq. 0) return

      x_min = abs(x(1))
      do i = 1 , n
        x_min = min(x_min,abs(x(i)))
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_min_max_loc(ix_min,ix_max,n,x)
      implicit  none
      integer  i,n
      real     x(1)
      integer  ix_min,ix_max
      ix_min = 0
      ix_max = 0
      if (n .eq. 0) return

      ix_min = 1
      ix_max = 1
      do i = 1 , n
        if (x(i) .lt. x(ix_min)) ix_min = i
        if (x(i) .gt. x(ix_max)) ix_max = i
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_abs_dif(t_dif,nx,x1,x2)
c  compute the maximum absolute difference between two vectors
      implicit  none
      integer   util_prn,util_err,util_dbg,util_wln,util_test
      real     t_dif
      integer  nx
      real     x1(nx)
      real     x2(nx)

      integer  ix

      t_dif = 0.

      do ix = 1 , nx

c        if (t_dif .lt. abs(x1(ix)-x2(ix))) 
c     1if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' ix='',i8,'' dt='',f12.4,'' dx='',f12.4
c     1,'' x1='',f12.4,'' x2='',f12.4)')
c     1 ix,t_dif,abs(x1(ix)-x2(ix)),x1(ix),x2(ix)

        t_dif = max(t_dif,abs(x1(ix)-x2(ix)))

      enddo    ! do ix = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_isamax(n,x,inc)
c  absolute max index
      implicit  none
      integer  n,inc
      real     x(inc,n)
      integer  i,j

      j = 0

      if (n .gt. 0) then

        j = 1

        do i = 1 , n

          if (abs(x(1,i)) .gt. abs(x(1,j))) j = i

        enddo    ! do i = 1 , n

      endif    ! if (n .gt. 0) then

      util_isamax = j

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_isamin(n,x,inc)
c  absolute min index
      implicit  none
      integer  n,inc
      real     x(inc,n)
      integer  i,j

      j = 0

      if (n .gt. 0) then

        j = 1

        do i = 1 , n

          if (abs(x(1,i)) .lt. abs(x(1,j))) j = i

        enddo    ! do i = 1 , n

      endif    ! if (n .gt. 0) then

      util_isamin = j

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_ismax(n,x,inc)
c  max index
      implicit  none
      integer  n,inc
      real     x(inc,n)
      integer  i,j

      j = 0

      if (n .gt. 0) then

        j = 1

        do i = 1 , n

          if ((x(1,i)) .gt. (x(1,j))) j = i

        enddo    ! do i = 1 , n

      endif    ! if (n .gt. 0) then

      util_ismax = j

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_ismin(n,x,inc)
c  min index
      implicit  none
      integer  n,inc
      real     x(inc,n)
      integer  i,j

      j = 0

      if (n .gt. 0) then

        j = 1

        do i = 1 , n

          if ((x(1,i)) .lt. (x(1,j))) j = i

        enddo    ! do i = 1 , n

      endif    ! if (n .gt. 0) then

      util_ismin = j

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_open_file(i_file,file,status,form,n_rec,i_err)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer   util_r

      integer   i_file
      character file*(*),status*(*),form*(*)
      integer   n_rec,i_err

      i_err = 0

      call util_get_lun(i_file,i_err)
      if (i_err .ne. 0) goto 998

      if (form(1:1) .eq. 'f' .or. form(1:1) .eq. 'F') then

        open(i_file,file=file,status=status,form='formatted'
     1,err=997)

      else

        open(i_file,file=file,status=status,form='unformatted'
     1,access='direct',recl=n_rec,err=996)
c     1,access='direct',recl=n_rec)

      endif

      if (util_dbg() .ge. 0) 
     1write(util_dbg(),'(/,'' util_open_file''
     1,/,'' unit  ='',i8
     1,/,'' file  ='',a
     1,/,'' status='',a
     1,/,'' form  ='',a
     1,/,'' recl  ='',i8
     1)')
     1 i_file
     1,file(1:util_r(file))
     1,status(1:util_r(status))
     1,form(1:util_r(form))
     1,n_rec

      return

  996 continue
      write(util_err()
     1,'(/,'' error in util_open_file unformatted open'')')
      goto 999

  997 continue
      write(util_err()
     1,'(/,'' error in util_open_file formatted open'')')
      goto 999

  998 continue
      write(util_err()
     1,'(/,'' error in util_open_file getting unit number'')')
      goto 999

  999 continue
      write(util_err(),'(/,'' error in util_open_file''
     1,/,'' i_file='',i5,'' file='',a
     1,/,'' status='',a16,'' form='',a16,'' recl='',i5)')
     1 i_file,file,status,form,n_rec
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_close_file(i_file)
      implicit  none
      integer  i_file

      close(i_file)

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_trace_open(i_file,file,status
     1,nx_inp,nh_inp,nt_inp,t0_inp,dt_inp,tr_max,i_err)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer   util_r
      real      util_amax

      integer   i_file
      character file*(*)
      character status*(*)
      integer   nx_inp
      real      t0_inp,dt_inp,tr_max
      integer   i_err

      integer   ix_inp

      integer   nh_inp
      real      hd_inp(1)

      integer   nt_inp
      real      tr_inp(1)

      character hf_sav_0*80
      character df_sav_0*80
      character st_sav_0*8

      integer   m_file
      parameter (m_file=100)
      character hf_sav(m_file)*80
      character df_sav(m_file)*80
      character st_sav(m_file)*8
      integer   nx_sav(m_file)
      integer   nh_sav(m_file)
      integer   nt_sav(m_file)
      real      t0_sav(m_file)
      real      dt_sav(m_file)
      real      tr_sav(m_file)

      data      hf_sav/m_file*'        '/
      data      df_sav/m_file*'        '/
      data      st_sav/m_file*'        '/
      data      nx_sav/m_file* 0 /
      data      nh_sav/m_file* 0 /
      data      nt_sav/m_file* 0 /
      data      t0_sav/m_file* 0./
      data      dt_sav/m_file* 0./
      data      tr_sav/m_file* 0./

      common /util_trace/hf_sav,df_sav,st_sav,nx_sav
     1,nh_sav,nt_sav,t0_sav,dt_sav,tr_sav

      integer   j_sav
      integer   ih_file
      integer   l_rec
      integer   ih_inp,it_inp
      real      hd_tmp,tr_tmp

      i_err = 0

c  apend the header and add suffixes
      hf_sav_0 = file
      call util_add_ext_replace(hf_sav_0,'head')

      df_sav_0 = file
      call util_add_ext_replace(df_sav_0,'data')

      st_sav_0 = status
      call util_caps(st_sav_0,st_sav_0)

c  open the old head file and read the header info
c  get the data file characteristics from this information
c  other wise use the input information
      if (st_sav_0(1:1) .eq. 'O') then

        call util_open_file(ih_file,hf_sav_0,st_sav_0
     1,'formatted',0,i_err)
        if (i_err .ne. 0) goto 1999
        read(ih_file,'(a)')df_sav_0
        read(ih_file,*)nx_inp,nh_inp,nt_inp,t0_inp,dt_inp,tr_max
        close(ih_file)

      endif    ! if (st_sav_0(1:1) .eq. 'O') then

c  open the data file
      l_rec = nh_inp + nt_inp

c  for cray direct acess files use *8 
      if (util_wln() .eq. 8) l_rec = l_rec * 8

c  open the direct access data file
      call util_open_file(i_file,df_sav_0,st_sav_0
     1,'unformatted',l_rec,i_err)
      if (i_file .lt. 0 .or. i_file .gt. m_file
     1 .or. i_err .ne. 0) goto 1999

c  save the header and data file names and the status
      hf_sav(i_file) = hf_sav_0
      df_sav(i_file) = df_sav_0
      st_sav(i_file) = st_sav_0
      nh_sav(i_file) = nh_inp
      nt_sav(i_file) = nt_inp
      t0_sav(i_file) = t0_inp
      dt_sav(i_file) = dt_inp

      if (st_sav_0(1:1) .eq. 'O') then

        nx_sav(i_file) = nx_inp
        tr_sav(i_file) = tr_max

      else    ! if (st_sav_0(1:1) .eq. 'O') then

        nx_sav(i_file) = 0
        tr_sav(i_file) = 0.

      endif    ! if (st_sav_0(1:1) .eq. 'O') then

      if (util_dbg() .ge. 0) 
     1write(util_dbg(),'(/,'' util_trace_open''
     1,/,'' i_file='',i8
     1,/,'' st_sav='',a
     1,/,'' df_sav='',a
     1,/,'' nx_sav='',i8
     1,/,'' nh_sav='',i8
     1,/,'' nt_sav='',i8
     1,/,'' t0_sav='',f10.4
     1,/,'' dt_sav='',f10.4
     1,/,'' tr_sav='',f10.4
     1)')
     1 i_file
     1,st_sav(i_file)(1:util_r(st_sav(i_file)))
     1,df_sav(i_file)(1:util_r(df_sav(i_file)))
     1,nx_sav(i_file)
     1,nh_sav(i_file)
     1,nt_sav(i_file)
     1,t0_sav(i_file)
     1,dt_sav(i_file)
     1,tr_sav(i_file)

      return

 1999 continue
      write(util_err(),'(/,'' error in util_trace_open''
     1,/,'' i_file='',i8,''m_file='',i8)')i_file,m_file
      goto 999

C23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_trace_close(i_file,i_err)

c  close the head file for a new file
      if (st_sav(i_file)(1:1) .eq. 'N') then

c  open new head file and write the header info
        call util_open_file(
     1 ih_file,hf_sav(i_file),st_sav(i_file),'formatted',0,i_err)
        if (i_err .ne. 0) goto 2999
        write(ih_file,'(1x,a)')df_sav(i_file)
        write(ih_file,*)nx_sav(i_file),nh_sav(i_file)
     1,nt_sav(i_file),t0_sav(i_file),dt_sav(i_file),tr_sav(i_file)
        close(ih_file)

      else

      endif

      call util_close_file(i_file)

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'(/,'' util_trace_close''
c     1,/,'' i_file='',i8
c     1,/,'' st_sav='',a
c     1,/,'' df_sav='',a
c     1,/,'' nx_sav='',i8
c     1,/,'' nh_sav='',i8
c     1,/,'' nt_sav='',i8
c     1,/,'' t0_sav='',f10.4
c     1,/,'' dt_sav='',f10.4
c     1,/,'' tr_sav='',f10.4
c     1)')
c     1 i_file
c     1,st_sav(i_file)(1:util_r(st_sav(i_file)))
c     1,df_sav(i_file)(1:util_r(df_sav(i_file)))
c     1,nx_sav(i_file)
c     1,nh_sav(i_file)
c     1,nt_sav(i_file)
c     1,t0_sav(i_file)
c     1,dt_sav(i_file)
c     1,tr_sav(i_file)

      nx_sav(i_file) = 0
      nh_sav(i_file) = 0
      nt_sav(i_file) = 0
      t0_sav(i_file) = 0.
      dt_sav(i_file) = 0.
      tr_sav(i_file) = 0

      return

 2999 continue
      write(util_err(),'(/,'' error in util_trace_close'')')
      goto 999

C23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_trace_write(i_file,ix_inp
     1,nh_inp,hd_inp,nt_inp,tr_inp,i_err)
      i_err = 0

      hd_tmp = 0.
      tr_tmp = 0.

      write(i_file,rec=ix_inp,err=3999)
     1 (hd_inp(ih_inp),ih_inp=       1,min(nh_inp,nh_sav(i_file)))
     1,(hd_tmp        ,ih_inp=nh_inp+1,           nh_sav(i_file) )
     1,(tr_inp(it_inp),it_inp=       1,min(nt_inp,nt_sav(i_file)))
     1,(tr_tmp        ,it_inp=nt_inp+1,           nt_sav(i_file) )

      tr_sav(i_file) = 
     1max(tr_sav(i_file),util_amax(min(nt_inp,nt_sav(i_file)),tr_inp))
      nx_sav(i_file) = max(nx_sav(i_file),ix_inp)

      return

 3999 continue
      write(util_err(),'(/,'' error in util_trace_write''
     1,/,'' ix_inp='',i8)')ix_inp
      goto 999

C23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_trace_read(i_file,ix_inp
     1,nh_inp,hd_inp,nt_inp,tr_inp,i_err)

      i_err = 0

      read(i_file,rec=ix_inp,err=3999)
     1 (hd_inp(ih_inp),ih_inp=       1,min(nh_inp,nh_sav(i_file)))
     1,(hd_tmp        ,ih_inp=nh_inp+1,           nh_sav(i_file) )
     1,(tr_inp(it_inp),it_inp=       1,min(nt_inp,nt_sav(i_file)))
     1,(tr_tmp        ,it_inp=nt_inp+1,           nt_sav(i_file) )

      return

 4999 continue
      write(util_err(),'(/,'' error in util_trace_read''
     1,/,'' ix_inp='',i8)')ix_inp
      goto 999

  999 continue
      write(util_err(),'(/,'' error in util_trace_''
     1,/,'' i_file='',i8
     1,/,'' st_sav='',a
     1,/,'' df_sav='',a
     1,/,'' nx_sav='',i8
     1,/,'' nh_sav='',i8
     1,/,'' nt_sav='',i8
     1,/,'' t0_sav='',f10.4
     1,/,'' dt_sav='',f10.4
     1,/,'' tr_sav='',f10.4
     1)')
     1 i_file
     1,st_sav(i_file)(1:util_r(st_sav(i_file)))
     1,df_sav(i_file)(1:util_r(df_sav(i_file)))
     1,nx_sav(i_file)
     1,nh_sav(i_file)
     1,nt_sav(i_file)
     1,t0_sav(i_file)
     1,dt_sav(i_file)
     1,tr_sav(i_file)
      i_err = -1

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_get_file_name(prompt,file,ext)
      integer   util_prn,util_err,util_dbg,util_wln,util_test
      character *(*) prompt,file,ext
      character crd80*80
    1 continue

      crd80 = ' '
      call util_add_character(prompt,crd80)
      call util_add_character(' -default=',crd80)
      call util_add_character(file,crd80)
      call util_add_character(' ext=',crd80)
      call util_add_character(ext,crd80)
      write(6,'(a)')crd80
      crd80 = ' '
      read (5,'(a)',err=1) crd80
      if (crd80 .ne. ' ') read(crd80,'(a)')file
c      if (file .eq. ' ') goto 1
      if (file .ne. ' '
     1.and. ext(1:4) .ne. 'none' .and. ext(1:4) .ne. 'NONE')
     1call util_add_ext(file,ext)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_add_character(c1,c2)
c  add c1 to end of c2
      integer   util_r
      character c1*(*),c2*(*)
      integer   lc1,lc2,n

      lc1 = util_r(c1)
      lc2 = util_r(c2)
      n = min(lc1,len(c2)-lc2)
      write(c2(lc2+1:lc2+n),'(a)')c1(1:n)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_add_ext(fname,ext)
      character*(*) fname,ext
      ientry = 1
      go to 1
      entry util_add_extrp (fname,ext)
      entry util_add_ext_replace (fname,ext)
      ientry = 2
   1  iclose = index (fname,']')
      if (iclose.eq.0)  iclose = index (fname,'>')
      idot = index (fname(iclose+1:),'.')
      if (idot.gt.0 .and. ientry.eq.1)  return
      isem = index (fname(iclose+1:),';')
      if (isem .gt. 0)  then
        if (idot.eq.0)  idot = isem
        fname = fname(:iclose+idot-1)//'.'//ext//fname(iclose+isem:)
      else
        if (idot.gt.0)  then
          fname = fname(:iclose+idot)//ext
        else
          ilast = index (fname(iclose+1:),' ')
          if (ilast.eq.0)  return
          fname = fname(:iclose+ilast-1)//'.'//ext
        end if
      end if

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_set_range(nx_mig,x0_mig,dx_mig,x1_mig)
c  make sure the image increment is positive and min is less than max
      implicit  none
      integer  nx_mig
      real     x0_mig,dx_mig,x1_mig

      real     x_temp

      x1_mig = (nx_mig - 1) * dx_mig + x0_mig

      if (dx_mig .lt. 0) then

        x_temp = x1_mig
        x1_mig = x0_mig
        x0_mig = x1_mig
        dx_mig = abs(dx_mig)

      endif    ! if (dx_mig .lt. 0) then

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_mem_put(nw_mem,jw_mem,iw_mem,n_work,tw_mem)
c  compute a memory pointer and put it into iw_mem
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer  util_r

      integer  nw_mem,jw_mem,iw_mem(nw_mem)
      integer  n_work
      character tw_mem*(*)

      integer  i_work

      jw_mem  = jw_mem + 1

      if (jw_mem+1 .gt. nw_mem) goto 999

      if (jw_mem .eq. 1) then

        call util_seti(nw_mem,iw_mem,0)
        i_work = 1

      else    ! if (jw_mem .eq. 1) then

        i_work = iw_mem(jw_mem)

      endif    ! if (jw_mem .eq. 1) then

      iw_mem(jw_mem  ) = i_work
      iw_mem(jw_mem+1) = i_work + n_work

      write(util_prn()
     1,'('' util_mem_put jw='',i8,'' iw='',i8,'' nw='',i8,'' tw='',a)')
     1 jw_mem,iw_mem(jw_mem),n_work,tw_mem(1:util_r(tw_mem))

      return

  999 continue
      write(util_err()
     1,'(/,'' error in util_mem_put jw_mem='',i8,'' nw_mem='',i8
     1,'' n_work='',i8)')
     1 jw_mem,nw_mem,n_work

      stop

      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_mem_get(nw_mem,jw_mem,iw_mem,i_work)
c  get a memory pointer from iw_mem
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer  i_work
      integer  nw_mem,jw_mem,iw_mem(nw_mem)

      jw_mem  = jw_mem + 1
      if (jw_mem+1 .gt. nw_mem) goto 999

      i_work = iw_mem(jw_mem)

      return

  999 continue
      write(util_err()
     1,'(/,'' error in util_mem_get jw_mem='',i8,'' nw_mem='',i8
     1,'' i_work='',i8)')
     1 jw_mem,nw_mem,i_work
      stop

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_work(i_work,m_work,j_work,n_work)
c  assign work space within an array
c
c  i_work = current pointer within work array
c  m_work = total memory in work array
c  j_work = pointer for this memory allocation
c  n_work = amount of memory to allocate in this call
c
c  i_work and m_work are initialized by util_wors and modified by
c  other routines, they should never be altered by the user outside 
c  of util_wor...
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer i_work,m_work,j_work,n_work,i_err

      j_work = i_work
      i_work = i_work + n_work

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_wors(i_work,m_work,n_work)
c  initalize the number of word savaliable to n_work 
c  and the pointer to 1
      i_work = 1
      m_work = n_work

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_worl(i_work,m_work,n_work)
c  return the number of words remaining
      n_work = m_work - i_work + 1

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_woru(i_work,m_work,n_work)
c  return the number of word used
      n_work = i_work - 1

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_worc(i_work,m_work,i_err)
c  check if we have used more memory than allowed

      i_err = 0
      if (i_work-1 .gt. m_work) then
        write(util_err()
     1,'(/,'' error in work used='',i8,'' have='',i8)')
     1 i_work-1,m_work
        i_err = -1
      endif    ! if (i_work-1 .gt. m_work) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_transform(nx,x
     1,x_inp_1,x_inp_2,x_out_1,x_out_2)
      implicit  none
      integer  nx
      real     x(1)
      real     x_inp_1,x_inp_2
      real     x_out_1,x_out_2

      integer  i
      real     dxo_dxi

      if (  x_inp_1 .eq. x_inp_2 .or.  x_out_1 .eq. x_out_2
     1.or. (x_inp_1 .eq. x_out_1 .and. x_inp_2 .eq. x_out_2)
     1) return

      dxo_dxi = (x_out_2 - x_out_1) / (x_inp_2 - x_inp_1)

      do i = 1 , nx
        x(i) = (x(i) - x_inp_1) * dxo_dxi + x_out_1
      enddo    ! do i = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_transform_1(x0,x1
     1,x_inp_1,x_inp_2,x_out_1,x_out_2)
c  convert min and max values from input to output units
      implicit  none
      real     x0,x1
      real     x_inp_1,x_inp_2
      real     x_out_1,x_out_2
      real     dx,x_temp

      if (  x_inp_1 .eq. x_inp_2 .or.  x_out_1 .eq. x_out_2
     1.or. (x_inp_1 .eq. x_out_1 .and. x_inp_2 .eq. x_out_2)
     1) return

      dx = x1 - x0

      call util_transform(1,x0
     1,x_inp_1,x_inp_2,x_out_1,x_out_2)

      call util_transform(1,x1
     1,x_inp_1,x_inp_2,x_out_1,x_out_2)

      if (x1 .lt. x0 .and. dx .ge. 0.) then
        x_temp = x1
        x1 = x0
        x0 = x_temp
      endif    ! if (x1 .lt. x0 .and. dx .ge. 0.) then

      return
      end


c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_get_lun(lun,i_err)
      implicit  none
      integer lun,i,i_err
      logical quest
      integer lstart,lstop
      parameter (lstart=20,lstop=99)
      integer last,list(lstop)
      save    last,list
      data    last,list/lstop,lstop*0/

      call getlun(lun,*999)
c      i_err = 0
c      do i=lstart,lstop
c           last=last+1
c           if (last.gt.lstop) last=lstart
c           inquire (last,named=quest,err=999)
c           if (.not.quest) then
c                list(last)=1
c                lun=last
c                return
c           end if
c      end do
      return

999   print *, 'util_get_lun failed'
      i_err = -1
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_get_lun_s
      call getluns
c      do i=lstart,lstop
c           if (list(i).gt.0) then
c                close (i)
c                list(i)=0
c           end if
c      end do
c      last=lstop
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_caps(c_inp,c_out)
c capitalize an ascii string
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      character c_inp*(*),c_out*(*)

      character c_tmp*132
      character small*26,big*26
      integer   nc,ic1,ic2
      data      small/'abcdefghijklmnopqrstuvwxyz'/
      data      big/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/

      c_tmp = c_inp
      c_out = ' '
      c_out = c_tmp
      nc = len(c_out)

      do ic1 = 1 , nc

        do ic2 = 1 , 26

          if (c_out(ic1:ic1) .eq. small(ic2:ic2)) then

            c_out(ic1:ic1) = big(ic2:ic2)
            goto 1

          endif    ! if (c_out(ic1:ic1).eq.small(ic2:ic2)) then

        enddo    ! do ic2 = 1 , 26

    1   continue

      enddo    ! do ic1 = 1 , nc

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_dcod(value, card)
      real value
      character*(*) card
c decode a real number, perhaps in scientific notation.
cndxi dcod( i
      integer n,i,n1,n2
      logical ee
      real r1,r2
      character*20 c20
c shift to left
      call util_clft(card)
c find out if in exponential notation
      n = len(card)
      ee = .false.
      do 100 i=1,n
        ee = (card(i:i).eq.'e'.or.card(i:i).eq.'E')
        if(ee) then
          n1 = i - 1
          n2 = i + 1
          goto 200
        endif
  100 continue
  200 continue
      if(.not.ee) then
c...        no exponent
        c20 = ' '
        c20 = card
        call util_cadp(c20,20)
        read(c20,1401,err=1301) value
      else
c...        have an exponent, read mantissa first
        c20 = ' '
        c20 = card(:n1)
        call util_cadp(c20,20)
        read(c20,1401,err=1301) r1
c...        read exponent second
        c20 = ' '
        c20 = card(n2:)
        call util_cadp(c20,20)
        read(c20,1401,err=1301) r2
        value = r1 * exp(log(10.)*r2)
      endif
      return
 1301 continue
        call util_heyu('dcod: problem decoding card')
        call util_heyu(card)
      return
 1401 format(f20.7)
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_heyu(string)
      integer newdev
      character*(*) string
c write a message to error output; default is unit 6.
cndxa heyu( a
      integer n,i_file
      data i_file/6/
      n = len(string)
      if(n.gt.0) then
        n = max(1,min(80, n ))
        write(i_file,1401) string(:n)
      endif
 1401 format(x,80a)
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_heyd(newdev)
c argument sets heyu() output to another device unit number
      i_file = newdev
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_lenl(n, c)
      character*(*) c
      integer n
c find the length of a string before first blank, from left.
cndxc lenl( c
      integer nc,i
      nc = len(c)
      do 100 i=1,nc
        n = i
        if(c(n:n).eq.' ') goto 200
  100 continue
      n = nc + 1
c break
  200 continue
      n = n - 1
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_lenp(n, c)
      character*(*) c
      integer n
c find the length of a string before first period, from right
cndxc lenp( c
c stop at first bracket ]. if no period stop before first blank.
      integer nc,ncb,i
      nc = len(c)
      ncb = nc + 1
      do 100 i=nc,1,-1
        n = i
        if(c(n:n).eq.' ') ncb = n
        if(c(n:n).eq.']') goto 150
        if(c(n:n).eq.'.') goto 200
  100 continue
  150 continue
      n = ncb
  200 continue
      n = n - 1
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_lenr(n, c,nc)
      character*(*) c
      integer n,nc
c find the lenth of a string before first blank, from right
cndxc lenr( c
      integer i
      do 100 i=nc,1,-1
        n = i
        if(c(n:n).ne.' ') goto 200
  100 continue
      n = 1
  200 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_decode_value_1(value,n_value, card,name)
      character*(*) value,card,name
      integer n_value
c get character value of a variable 'name' from card.  name=value.
cndxc decode_value_1( c
      character ccard*160,cname*80
      integer ic1,ic2,in1,nname,i,jc1,ncard,igap,ibegin
      logical lquote,util_spac
c
      ncard = len(card)

c  modify so # sign comments out following characters dwh 05-03-94
      do ic1 = 1 , ncard
        if (card(ic1:ic1) .eq. '#') then
          ncard = ic1 - 1
          goto 301
        endif    ! if (card(ic1:ic1) .eq. '#') then
      enddo    ! do ic1 = 1 , ncard
  301 continue

c capitalize name and card
      call util_caps(card, ccard)
      call util_caps(name, cname)
      call util_clft(cname)
      n_value = 0
      value = ' '
      call util_lenl(nname, name)
      nname = min(80, nname)
      do 300 ic1=1,ncard
        if(card(ic1:ic1).eq.'=') then
c count igap, the number of intervening blanks
          igap = 0
   50     continue
          ic2 = ic1 - igap - 1
          if(ic2.lt.1) goto 300
          if(ccard(ic2:ic2).eq.' ') then
            igap = igap + 1
            goto 50
          endif
c see if string matches before blanks
          do 100 in1=1,nname
            ic2 = ic1 - nname - 1 + in1 - igap
              if(ic2.lt.1) goto 300
              if(ccard(ic2:ic2).ne.cname(in1:in1)) goto 300
  100     continue
c check to see if cname is not the last part of another name.
          ic2 = ic1 - nname - 1 - igap
          if(ic2.gt.0) then
            if (.not.util_spac(card(ic2:ic2))) goto 300
c            if(      ccard(ic2:ic2).ne.' '.and.
c     &               ccard(ic2:ic2).ne.','.and.
c     &               ccard(ic2:ic2).ne.';'.and.
c     &               ccard(ic2:ic2).ne.'('         ) goto 300
          endif
c have a match ; ignore first blanks after equals sign
          ibegin = ic1 + 1
  150     continue
            if(card(ibegin:ibegin).eq.' ') then
              ibegin = ibegin + 1
              if(ibegin.gt.ncard) goto 300
              goto 150
            endif
c check to see if string is in quotes
          lquote = (card(ibegin:ibegin).eq.'"')
          if(lquote) ibegin = ibegin + 1
c start loop to set output string
          value = ' '
          jc1 = ibegin - 1
          n_value = 1
  200     continue
            ic2 = jc1 + n_value
            if(ic2.le.ncard) then
              if( ((.not.lquote).and.(.not.util_spac(card(ic2:ic2))))
     &      .or. (lquote.and.card(ic2:ic2).ne.'"') ) then
c              if( ((.not.lquote).and.
c     &           card(ic2:ic2).ne.' '.and.
c     &           card(ic2:ic2).ne.')'.and.
c     &           card(ic2:ic2).ne.';'.and.
c     &           card(ic2:ic2).ne.',').or.
c     &           (lquote.and.card(ic2:ic2).ne.'"') ) then
                value(n_value:n_value) = card(ic2:ic2)
                n_value = n_value + 1
                goto 200
              endif
            endif
          n_value = n_value - 1
        endif
  300 continue
cc    write(6,*) 'name value ',cname(:nname),' ',value(:n_value), n_value
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_decode_value_2(value,n_value, name,i_file)
      character*(*) value,name
      integer n_value,i_file
      save mlines,nforl
      data mlines,nforl/200,1/
c get the value of a variable 'name' from device i_file
c  use format   'name=value',  returned value is a string.
c  string is delimited on right by blank, comma, or parenthesis.
c  keep last assignment in file
c first open i_file with
      character card*160, val*80
      integer nname,nval,nlines,jlines,mlines,iforl,jforl,nforl
      integer i_rewind
      data    i_rewind/0/
      if (i_rewind .ne. 1) rewind(i_file)
      n_value = 0
      nlines = 1
      iforl = 0
  100 continue
        read(i_file,1401,end=200,err=1301) card
c      if (util_dbg() .ge. 0) write(util_dbg()
c     1,'('' decode_value_2 card='',a60)')card(1:min(len(card),60))
        call util_decode_value_1(val,nval, card,name)
        if(nval.gt.0) then
          value = val(:nval)
          n_value = nval
          iforl = iforl + 1
          if (iforl .eq. nforl) goto 200
        endif
      nlines = nlines + 1
      if(nlines.lt.mlines) goto 100
  200 continue
        call util_lenl(nname,name)
cc        write(6,*) 'name value ',name(:nname),' ',value(:n_value)
      return
 1301 continue
c      write(6,*) 'error reading device ',i_file
      return
 1401 format(160a)
      entry util_plin(jlines)
c set the number of lines to read in decode_value_2
      mlines = jlines
      return
      entry util_glin(jlines)
c return the number of lines to read in decode_value_2
      jlines = mlines
      return
      entry util_porl(jforl)
c  se the number of occurences to search for
      nforl = jforl
      return
      entry util_gorl(jforl)
c  return the number of occurences to search for
      jforl = nforl
      return

      entry util_set_rewind
      i_rewind = 0
      return

      entry util_set_no_rewind
      i_rewind = 1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_decode_r(rvalue, name)
      integer   util_prn,util_err,util_dbg,util_wln,util_test
      real rvalue
      integer ivalue,nchar,i_filen
      character*(*) name,cvalue
c get real value from file; "name=rvalue" oepn set i_file, close
      integer n_value,i_file,ios,icray,iopen,i
      real r
      character*80 value
      character*20 c20
      data i_file/-1/
        if(i_file.lt.0) return
        call util_decode_value_2(value,n_value, name,i_file)
        if(n_value.ge.1) then
          call util_dcod(rvalue, value(:n_value))
        endif
c      if (util_dbg() .ge. 0) write(util_dbg()
c     1,'('' gvr value='',g16.3,'' name='',a16)')rvalue,name
      return
c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_decode_i(ivalue, name)
c get an integer value from a parameter file
        if(i_file.lt.0) return
        call util_decode_value_2(value,n_value, name,i_file)
        if(n_value.ge.1) then
          call util_dcod(r,value(:n_value))
          ivalue = int(r+0.499)
        endif
c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' gvi ivalue='',i8,'' name='',a16)')
c     1 ivalue,name
      return
c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_decode_c(cvalue,nchar, name)
c get a character string from a parameter file
        if(i_file.lt.0) then
          nchar = 0
          return
        endif
        call util_decode_value_2(value,n_value, name,i_file)
        if(n_value.ge.1) then
          cvalue = ' '
          cvalue = value(:n_value)
          nchar = n_value
        endif
c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' gvc value='',a16,'' name='',a16)')
c     1 cvalue,name
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_get_device(j_file)
c call this to find out what the device (unit) number is open
        j_file = i_file
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_put_device(j_file)
c call this to find out what the device (unit) number is open
        i_file = j_file
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_cadp(c, nc)
      character*(*) c
      integer nc
c add a period to a string, so that resembles floating point number.
cndxc cadp( c
      integer i
      character*80 card
      card = ' '
      card = c(:nc)
      call util_clft(card)
      c(:nc) = card(:nc)
      do 100 i=1,nc
        if(c(i:i).eq.' '.or.c(i:i).eq.'.') goto 200
  100 continue
      nc = nc + 1
      i = nc
  200 continue
      c(i:i) = '.'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_clft(c)
      character*(*) c
c get rid of leading blanks, only one word moved, up to first blank.
cndxc clft( c
      integer i,i1,n,i2,nall,nc,ncrest
      nc = len(c)
      do 100 i=1,nc
        if(c(i:i).ne.' ') goto 200
  100 continue
      return
  200 continue
      if(i.eq.1) return
      ncrest = nc-i+1
      call util_lenr(n, c(i:nc),ncrest)
      nall = i+n-1
      do 300 i1=1,nc
        i2 = i1 + i - 1
        if(i1.le.n) then
          c(i1:i1) = c(i2:i2)
        else
          c(i1:i1) = ' '
        endif
  300 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_error(string,i_err)
      implicit  none
      integer   util_prn,util_err,util_dbg,util_wln,util_test
      character string*(*)
      integer   i_err,lu_err
      data      lu_err/6/
c      if (util_dbg() .ge. 0) write(util_dbg(),'('' erexit'')')
      write(lu_err,'('' error exit'',/,a)')string
      i_err = i_err - 1
c      stop
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_write_card(i_file,nx,x)
c  write nx values of x to file i_file
      implicit  none
      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer   util_r

      integer   i_file
      integer   nx
      real      x(1)

      character crd132*132

      call util_encode_card(nx,x,crd132)
      if (nx .gt. 5) call util_delete_blanks(crd132)
      write(i_file,'(a)')crd132(1:util_r(crd132))

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_encode_card(n,x,card)
c  encode card with n values of x
      character card*(*)
      dimension x(*)
      data l_format/16/
      lencard = len(card)
      card = ' '

c      print*,' n=',n,' lencard=',lencard
      do i = 1 , n

        i1 = (i - 1) * l_format + 2
        i2 = min(lencard,i*l_format)
        j = nint(x(i))

        if (x(i) .eq. float(j)) then

          write(card(i1:),'(i9)')j

        else    ! if (x(i) .eq. float(j)) then

          write(card(i1:),'(g15.6)')x(i)

        endif    ! if (x(i) .eq. float(j)) then

c      if (util_dbg() .ge. 0) write(util_dbg()
c     1,'('' i1='',i3,'' i2='',i3,'' c='',a20)')i1,i2,card(i1:i2)
        if (i2 .ge. lencard) return

      enddo    ! do i = 1 , n

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_delete_blanks(str)
c  remove blanks from a string
      character str*(*)
      nstr = 0
      n = 1
      do 1 i = 2 , len(str)
        if (str(i:i) .eq. ' ' .and. str(n:n) .eq. ' ') goto 1
        n = n + 1
        str(n:n) = str(i:i)
    1 continue
      do 2 i = n+1 , len(str)
        str(i:i) = ' '
    2 continue
      return
      end


c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_interpolate_i_to_r(nx_inp,x_inp,y_inp
     1,nx_out,x0_out,dx_out,y_out)
c  Interpolate from an irregularly sampled function x_inp,y_inp
c  to a regularly sampled function x_out,y_out
c  x_inp can be arbitrarily ordered
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer  nx_inp
      real     x_inp(nx_inp),y_inp(nx_inp)

      integer  nx_out
      real     x0_out,dx_out
      real     y_out(nx_out)

      integer  ix_out
      real     x_out

      integer  ix_inp_1,ix_inp_2
      real     fx_inp_1,fx_inp_2

c  compute the map from input to output
c for each value of x_out find the two values of x_inp that stradlle it
c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' util_interpoalte_i_to_r nx_inp='',i8
c     1,'' nx_out='',i8,'' x0_out='',f10.2,'' dx_out='',f10.4)')
c     1 nx_inp,nx_out,x0_out,dx_out

      do ix_out = 1 , nx_out

        x_out = (ix_out - 1) * dx_out + x0_out

        call util_find_index(x_out,nx_inp,x_inp
     1,ix_inp_1,ix_inp_2,fx_inp_1,fx_inp_2)

        y_out(ix_out) = fx_inp_1 * y_inp(ix_inp_1)
     1                + fx_inp_2 * y_inp(ix_inp_2)

c       if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' ixo='',i5,'' ixi='',i5,1x,i5
c     1,'' xo='',f6.1,'' yo='',f6.1)')
c     1 ix_out,ix_inp_1,ix_inp_2,x_out,y_out(ix_out)

      enddo    ! do ix_out = 1 , nx_out

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_interpolate_i_to_r_2d(
     1 nyi,ixi,nxi,mxi
     1,xi,yi,zi
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,z_hor
     1,yofy
     1,zofx
     1,zofy
     1,z_xy
     1)
c  interpolate from a pointed 2d grid to a uniform 2d grid
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer  nyi
      integer  ixi(nyi),nxi(nyi)
      integer  mxi
      real     xi(mxi)
      real     yi(mxi)
      real     zi(mxi)

      integer  nx_grd
      real     x0_grd,dx_grd

      integer  ny_grd
      real     y0_grd,dy_grd

      real     z_hor(nx_grd,ny_grd)

      real     yofy(nyi)
      real     zofx(nyi)
      real     zofy(ny_grd)
      real     z_xy(nx_grd,nyi)

      integer  iy_hor,ix_1,ix_grd

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'(/,'' util_interpolate_i_to_r_2d''
c     1,'' mxi='',i8)')mxi

c  interpolate to the x grid at each y location
      do iy_hor = 1 , nyi

        ix_1 = ixi(iy_hor) + 1    ! first point in this x line

        yofy(iy_hor) = yi(ix_1)   ! first y value in this x line

        call util_interpolate_i_to_r(nxi(iy_hor),xi(ix_1),zi(ix_1)
     1,nx_grd,x0_grd,dx_grd,z_xy(1,iy_hor))

      enddo    ! do iy_hor = 1 , nyi

c  for each x_grd value interpolate from between ys
      do ix_grd = 1 , nx_grd

c  copy from z_xy to zofx
        call util_copy_inc(nyi,nx_grd,z_xy(ix_grd,1),1,zofx)

c  interpolate from zofx to zofy
        call util_interpolate_i_to_r(nyi,yofy,zofx
     1,ny_grd,y0_grd,dy_grd,zofy)

c  copy from zofy to z_hor
        call util_copy_inc(ny_grd,1,zofy,nx_grd,z_hor(ix_grd,1))

      enddo    ! do ix_grd = 1 , nx_grd

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_find_index(x_out,nx_inp,x_inp
     1,ix_inp_1,ix_inp_2,fx_inp_1,fx_inp_2)
c get nearest index to x_out from x_inp to left and right.
c  if x_inp is outside range of x_out use nearest end member 
c  for both ix_inp_1,ix_inp_2
c  nx_inp = # of elements in x_inp to convider
c  ix_inp_1 = nearest point to left (x_inp<x_out)
c  ix_inp_2 = nearest point to right (x_inp>x_out)
c  fx_inp_1 = weight to left vide
c  fx_inp_2 = weight to right vide
      implicit  none

      real     x_out

      integer  nx_inp
      real     x_inp(nx_inp)

      integer  ix_inp_1,ix_inp_2
      real     fx_inp_1,fx_inp_2

      integer  ix_inp

      real     x_inp_min,x_inp_max,eps,dx

      x_inp_min = x_out
      x_inp_max = x_out

      ix_inp_1 = 0
      ix_inp_2 = 0

      do ix_inp = 1 , nx_inp

        x_inp_min = min(x_inp_min,x_inp(ix_inp))
        x_inp_max = max(x_inp_max,x_inp(ix_inp))

        if (x_inp(ix_inp) .lt. x_out) then

          if (ix_inp_1 .eq. 0) then

            ix_inp_1 = ix_inp

          elseif (x_inp(ix_inp) .gt. x_inp(ix_inp_1)) then

            ix_inp_1 = ix_inp

          endif    ! if (ix_inp_1 .eq. 0) then

        elseif (x_inp(ix_inp) .gt. x_out) then    ! if (x_inp(ix_inp) .le. x_out) then

          if (ix_inp_2 .eq. 0) then

            ix_inp_2 = ix_inp

          elseif (x_inp(ix_inp) .lt. x_inp(ix_inp_2)) then

            ix_inp_2 = ix_inp

          endif    ! if (ix_inp_2 .eq. 0) then

        else    ! if (x_inp(ix_inp) .lt. x_out) then

          ix_inp_1 = ix_inp
          ix_inp_2 = ix_inp

        endif    ! if (x_inp(ix_inp) .lt. x_out) then

      enddo    ! do ix_inp = 1 , nx_inp

c see if output point is off end of input points.
      if (ix_inp_1 .eq. 0) ix_inp_1 = ix_inp_2
      if (ix_inp_2 .eq. 0) ix_inp_2 = ix_inp_1

      if (ix_inp_1 .eq. 0) ix_inp_1 = 1
      if (ix_inp_2 .eq. 0) ix_inp_2 = 1

c now have two points spannng output point
      dx = x_inp(ix_inp_2) - x_inp(ix_inp_1)
      eps = (1.e-8) * abs(x_inp_max-x_inp_min)

      if (dx .gt. eps) then

        fx_inp_2 = (x_out - x_inp(ix_inp_1)) / dx

      else    ! if (dx .gt. eps) then

c...   same point on left and right
        fx_inp_2 = 0.5

      endif

      fx_inp_1 = 1. - fx_inp_2

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_interpolate(
     1 na1,oa1,da1
     1,na2,oa2,da2
     1,na3,oa3,da3
     1,a
     1,nb1,ob1,db1
     1,nb2,ob2,db2
     1,nb3,ob3,db3
     1,b)
c  3D interpolation from a to b
      implicit  none

      integer  na1
      real     oa1,da1

      integer  na2
      real     oa2,da2

      integer  na3
      real     oa3,da3

      real     a(na1,na2,na3)

      integer  nb1
      real     ob1,db1

      integer  nb2
      real     ob2,db2

      integer  nb3
      real     ob3,db3

      real     b(nb1,nb2,nb3)

      integer  ib1,ib2,ib3
      real     b1,b2,b3,a1,a2,a3,da10,da20,da30
      integer  i11,i12,i21,i22,i31,i32
      real     f11,f12,f21,f22,f31,f32

      da10 = da1
      if (da10 .eq. 0.) da10 = 1.
      da20 = da2
      if (da20 .eq. 0.) da20 = 1.
      da30 = da3
      if (da30 .eq. 0.) da30 = 1.

      do ib3 = 1 , nb3

        b3 = (ib3 - 1) * db3 + ob3
        i31 = max(1,min(na3,int((b3-oa3)/da30)+1))
c        i32 = max(1,min(na3,i31+int(sign(1.,da30))))
        i32 = max(1,min(na3,i31+1))
        a3 = (i31 - 1) * da3 + oa3
        f32 = max(0.,min(1.,(b3-a3)/da30))
        f31 = 1. - f32

        do ib2 = 1 , nb2

          b2 = (ib2 - 1) * db2 + ob2
          i21 = max(1,min(na2,int((b2-oa2)/da20)+1))
c          i22 = max(1,min(na2,i21+int(sign(1.,da20))))
          i22 = max(1,min(na2,i21+1))
          a2 = (i21 - 1) * da2 + oa2
          f22 = max(0.,min(1.,(b2-a2)/da20))
          f21 = 1. - f22

          do ib1 = 1 , nb1

            b1 = (ib1 - 1) * db1 + ob1
            i11 = max(1,min(na1,int((b1-oa1)/da10)+1))
c            i12 = max(1,min(na1,i11+int(sign(1.,da10))))
            i12 = max(1,min(na1,i11+1))
            a1 = (i11 - 1) * da1 + oa1
            f12 = max(0.,min(1.,(b1-a1)/da10))
            f11 = 1. - f12

            b(ib1,ib2,ib3) =
     1   f11 * f21 * f31 * a(i11,i21,i31)
     1 + f12 * f21 * f31 * a(i12,i21,i31)
     1 + f11 * f22 * f31 * a(i11,i22,i31)
     1 + f12 * f22 * f31 * a(i12,i22,i31)
     1 + f11 * f21 * f32 * a(i11,i21,i32)
     1 + f12 * f21 * f32 * a(i12,i21,i32)
     1 + f11 * f22 * f32 * a(i11,i22,i32)
     1 + f12 * f22 * f32 * a(i12,i22,i32)
          enddo    ! do ib1 = 1 , nb1
        enddo    ! do ib2 = 1 , nb2
      enddo    ! do ib3 = 1 , nb3

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_interpolate_3d(
     1 na1,oa1,da1
     1,na2,oa2,da2
     1,na3,oa3,da3
     1,a
     1,nb1,ob1,db1
     1,nb2,ob2,db2
     1,nb3,ob3,db3
     1,b)
c  3D interpolation from a to b
      implicit  none

      integer  na1
      real     oa1,da1

      integer  na2
      real     oa2,da2

      integer  na3
      real     oa3,da3

      real     a(na1,na2,na3)

      integer  nb1
      real     ob1,db1

      integer  nb2
      real     ob2,db2

      integer  nb3
      real     ob3,db3

      real     b(nb1,nb2,nb3)

      integer  ib1,ib2,ib3
      real     b1,b2,b3,a1,a2,a3,da10,da20,da30
      integer  i11,i12,i21,i22,i31,i32
      real     f11,f12,f21,f22,f31,f32

      da10 = da1
      if (da10 .eq. 0.) da10 = 1.
      da20 = da2
      if (da20 .eq. 0.) da20 = 1.
      da30 = da3
      if (da30 .eq. 0.) da30 = 1.

      do ib3 = 1 , nb3

        b3 = (ib3 - 1) * db3 + ob3
        i31 = max(1,min(na3,int((b3-oa3)/da30)+1))
c        i32 = max(1,min(na3,i31+int(sign(1.,da30))))
        i32 = max(1,min(na3,i31+1))
        a3 = (i31 - 1) * da3 + oa3
        f32 = max(0.,min(1.,(b3-a3)/da30))
        f31 = 1. - f32

        do ib2 = 1 , nb2

          b2 = (ib2 - 1) * db2 + ob2
          i21 = max(1,min(na2,int((b2-oa2)/da20)+1))
c          i22 = max(1,min(na2,i21+int(sign(1.,da20))))
          i22 = max(1,min(na2,i21+1))
          a2 = (i21 - 1) * da2 + oa2
          f22 = max(0.,min(1.,(b2-a2)/da20))
          f21 = 1. - f22

          do ib1 = 1 , nb1

            b1 = (ib1 - 1) * db1 + ob1
            i11 = max(1,min(na1,int((b1-oa1)/da10)+1))
c            i12 = max(1,min(na1,i11+int(sign(1.,da10))))
            i12 = max(1,min(na1,i11+1))
            a1 = (i11 - 1) * da1 + oa1
            f12 = max(0.,min(1.,(b1-a1)/da10))
            f11 = 1. - f12

            b(ib1,ib2,ib3) =
     1   f11 * f21 * f31 * a(i11,i21,i31)
     1 + f12 * f21 * f31 * a(i12,i21,i31)
     1 + f11 * f22 * f31 * a(i11,i22,i31)
     1 + f12 * f22 * f31 * a(i12,i22,i31)
     1 + f11 * f21 * f32 * a(i11,i21,i32)
     1 + f12 * f21 * f32 * a(i12,i21,i32)
     1 + f11 * f22 * f32 * a(i11,i22,i32)
     1 + f12 * f22 * f32 * a(i12,i22,i32)
          enddo    ! do ib1 = 1 , nb1
        enddo    ! do ib2 = 1 , nb2
      enddo    ! do ib3 = 1 , nb3

      return
      end


c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_interpolate_2d(
     1 na1,oa1,da1
     1,na2,oa2,da2
     1,a
     1,nb1,ob1,db1
     1,nb2,ob2,db2
     1,b)
c  2D interpolation from a to b
      implicit  none

      integer  na1
      real     oa1,da1

      integer  na2
      real     oa2,da2

      real     a(na1,na2)

      integer  nb1
      real     ob1,db1

      integer  nb2
      real     ob2,db2

      real     b(nb1,nb2)

      integer  ib1,ib2
      real     b1,b2,a1,a2,da10,da20
      integer  i11,i12,i21,i22
      real     f11,f12,f21,f22

      da10 = da1
      if (da10 .eq. 0.) da10 = 1.
      da20 = da2
      if (da20 .eq. 0.) da20 = 1.

        do ib2 = 1 , nb2

          b2 = (ib2 - 1) * db2 + ob2
          i21 = max(1,min(na2,int((b2-oa2)/da20)+1))
c          i22 = max(1,min(na2,i21+int(sign(1.,da20))))
          i22 = max(1,min(na2,i21+1))
          a2 = (i21 - 1) * da2 + oa2
          f22 = max(0.,min(1.,(b2-a2)/da20))
          f21 = 1. - f22

          do ib1 = 1 , nb1

            b1 = (ib1 - 1) * db1 + ob1
            i11 = max(1,min(na1,int((b1-oa1)/da10)+1))
c            i12 = max(1,min(na1,i11+int(sign(1.,da10))))
            i12 = max(1,min(na1,i11+1))
            a1 = (i11 - 1) * da1 + oa1
            f12 = max(0.,min(1.,(b1-a1)/da10))
            f11 = 1. - f12

            b(ib1,ib2) =
     1   f11 * f21 * a(i11,i21)
     1 + f12 * f21 * a(i12,i21)
     1 + f11 * f22 * a(i11,i22)
     1 + f12 * f22 * a(i12,i22)

          enddo    ! do ib1 = 1 , nb1

        enddo    ! do ib2 = 1 , nb2

      return
      end


c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_interpolate_1d(
     1 na1,oa1,da1
     1,a
     1,nb1,ob1,db1
     1,b)
c  1D interpolation from a to b
      implicit  none

      integer  na1
      real     oa1,da1

      real     a(na1)

      integer  nb1
      real     ob1,db1

      real     b(nb1)

      integer  ib1
      real     b1,a1,da10
      integer  i11,i12
      real     f11,f12

      da10 = da1
      if (da10 .eq. 0.) da10 = 1.

          do ib1 = 1 , nb1

            b1 = (ib1 - 1) * db1 + ob1

            i11 = max(1,min(na1,int((b1-oa1)/da10)+1))
            i12 = max(1,min(na1,i11+1))

            a1 = (i11 - 1) * da1 + oa1

            f12 = max(0.,min(1.,(b1-a1)/da10))
            f11 = 1. - f12

            b(ib1) =
     1   f11 * a(i11)
     1 + f12 * a(i12)

          enddo    ! do ib1 = 1 , nb1

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_interpolate_1(
     1 na1,ao1,da1
     1,na2,ao2,da2
     1,na3,ao3,da3
     1,a
     1,nb1,bo1,db1
     1,nb2,bo2,db2
     1,nb3,bo3,db3
     1,b
     1,m_work,work
     1,i_err)

      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer na1
      real    ao1,da1

      integer na2
      real    ao2,da2

      integer na3
      real    ao3,da3

      real    a(na1,na2,na3)

      integer nb1
      real    bo1,db1

      integer nb2
      real    bo2,db2

      integer nb3
      real    bo3,db3

      real    b(nb1,nb2,nb3)

      integer m_work
      real    work(m_work)

      integer i_err

      integer i11,i12,i21,i22,i31,i32,j11,j12,j21,j22,j31,j32
      integer i_work_i,i_work_n

c      integer icall
c      data icall/0/
c      icall = icall + 1
c      if (util_dbg() .ge. 0) write(util_dbg()
c     1,'('' util_interpolate_1 icall='',i8,'' m_work='',i8
c     1,/,'' na1='',i8,'' ao1='',f12.2,'' da1='',f12.4
c     1,/,'' na2='',i8,'' ao2='',f12.2,'' da2='',f12.4
c     1,/,'' na3='',i8,'' ao3='',f12.2,'' da3='',f12.4
c     1,/,'' nb1='',i8,'' bo1='',f12.2,'' db1='',f12.4
c     1,/,'' nb2='',i8,'' bo2='',f12.2,'' db2='',f12.4
c     1,/,'' nb3='',i8,'' bo3='',f12.2,'' db3='',f12.4)')
c     1 icall,m_work
c     1,na1,ao1,da1,na2,ao2,da2,na3,ao3,da3
c     1,nb1,bo1,db1,nb2,bo2,db2,nb3,bo3,db3

      i_err = 0
      call util_wors(i_work_i,i_work_n,m_work)
      call util_work(i_work_i,i_work_n,i11,nb1)
      call util_work(i_work_i,i_work_n,i12,nb1)
      call util_work(i_work_i,i_work_n,j11,nb1)
      call util_work(i_work_i,i_work_n,j12,nb1)
      call util_work(i_work_i,i_work_n,i21,nb2)
      call util_work(i_work_i,i_work_n,i22,nb2)
      call util_work(i_work_i,i_work_n,j21,nb2)
      call util_work(i_work_i,i_work_n,j22,nb2)
      call util_work(i_work_i,i_work_n,i31,nb3)
      call util_work(i_work_i,i_work_n,i32,nb3)
      call util_work(i_work_i,i_work_n,j31,nb3)
      call util_work(i_work_i,i_work_n,j32,nb3)
      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 999

      call util_interpolate_2(na1,ao1,da1,na2,ao2,da2,na3,ao3,da3,a
     1,nb1,bo1,db1,nb2,bo2,db2,nb3,bo3,db3,b
     1,work(i11),work(i12),work(j11),work(j12)
     1,work(i21),work(i22),work(j21),work(j22)
     1,work(i31),work(i32),work(j31),work(j32))
      return
  999 continue
      i_err = -1
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_interpolate_2(
     1 na1,ao1,da1
     1,na2,ao2,da2
     1,na3,ao3,da3
     1,a
     1,nb1,bo1,db1
     1,nb2,bo2,db2
     1,nb3,bo3,db3
     1,b
     1,i11,i12,f11,f12
     1,i21,i22,f21,f22
     1,i31,i32,f31,f32)

      implicit  none

      integer na1
      real    ao1,da1

      integer na2
      real    ao2,da2

      integer na3
      real    ao3,da3

      real    a(na1,na2,na3)

      integer nb1
      real    bo1,db1

      integer nb2
      real    bo2,db2

      integer nb3
      real    bo3,db3

      real    b(nb1,nb2,nb3)

      integer i11(nb1),i12(nb1),i21(nb2),i22(nb2),i31(nb3),i32(nb3)
      real    f11(nb1),f12(nb1),f21(nb2),f22(nb2),f31(nb3),f32(nb3)

      integer i1,i2,i3

      call util_interpolate_3(na1,ao1,da1,nb1,bo1,db1,i11,i12,f11,f12)
      call util_interpolate_3(na2,ao2,da2,nb2,bo2,db2,i21,i22,f21,f22)
      call util_interpolate_3(na3,ao3,da3,nb3,bo3,db3,i31,i32,f31,f32)
      do i1 = 1 , nb1
        do i2 = 1 , nb2
          do i3 = 1 , nb3
            b(i1,i2,i3) =
     1   f11(i1) * f21(i2) * f31(i3) * a(i11(i1),i21(i2),i31(i3))
     1 + f12(i1) * f21(i2) * f31(i3) * a(i12(i1),i21(i2),i31(i3))
     1 + f11(i1) * f22(i2) * f31(i3) * a(i11(i1),i22(i2),i31(i3))
     1 + f12(i1) * f22(i2) * f31(i3) * a(i12(i1),i22(i2),i31(i3))
     1 + f11(i1) * f21(i2) * f32(i3) * a(i11(i1),i21(i2),i32(i3))
     1 + f12(i1) * f21(i2) * f32(i3) * a(i12(i1),i21(i2),i32(i3))
     1 + f11(i1) * f22(i2) * f32(i3) * a(i11(i1),i22(i2),i32(i3))
     1 + f12(i1) * f22(i2) * f32(i3) * a(i12(i1),i22(i2),i32(i3))
          enddo
        enddo
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_interpolate_3(na,ao,da,nb,bo,db,i1,i2,f1,f2)

      implicit  none

      integer na,nb
      real    ao,da,bo,db
      integer i1(nb),i2(nb)
      real    f1(nb),f2(nb)

      integer ib
      real    da0,a,b
      da0 = da
      if (da0 .eq. 0.) da0 = 1.
      do  ib = 1 , nb
        b = (ib - 1) * db + bo
        i1(ib) = max(1,min(na,int((b-ao)/da0)+1))
c        i2(ib) = max(1,min(na,i1(ib)+int(sign(1.,da0))))
        i2(ib) = max(1,min(na,i1(ib)+1))
        a = (i1(ib) - 1) * da + ao
        f2(ib) = max(0.,min(1.,(b-a)/da0))
        f1(ib) = 1. - f2(ib)
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_interpolate_3to1(
     1 na1
     1,na2,oa2,da2
     1,na3,oa3,da3
     1,a
     1,b2
     1,b3
     1,b)
c  3D interpolation from a to b
      implicit  none

      integer  na1

      integer  na2
      real     oa2,da2

      integer  na3
      real     oa3,da3

      real     a(na1,na2,na3)

      real     b1

      real     b(na1)

      integer  nb1
      integer  ib1,ib2,ib3
      real     b2,b3,a2,a3,da20,da30

      integer  i21,i22,i31,i32
      real     f21,f22,f31,f32

      real     f21_f31
      real     f22_f31
      real     f21_f32
      real     f22_f32

      nb1 = na1

      da20 = da2
      if (da20 .eq. 0.) da20 = 1.
      da30 = da3
      if (da30 .eq. 0.) da30 = 1.

      i31 = max(1,min(na3,int((b3-oa3)/da30)+1))
      i32 = max(1,min(na3,i31+1))
      a3 = (i31 - 1) * da3 + oa3
      f32 = max(0.,min(1.,(b3-a3)/da30))
      f31 = 1. - f32


      i21 = max(1,min(na2,int((b2-oa2)/da20)+1))
      i22 = max(1,min(na2,i21+1))
      a2 = (i21 - 1) * da2 + oa2
      f22 = max(0.,min(1.,(b2-a2)/da20))
      f21 = 1. - f22

      f21_f31 = f21 * f31
      f22_f31 = f22 * f31
      f21_f32 = f21 * f32
      f22_f32 = f22 * f32

      do ib1 = 1 , nb1

        b(ib1) =
     1   f21_f31 * a(ib1,i21,i31)
     1 + f22_f31 * a(ib1,i22,i31)
     1 + f21_f32 * a(ib1,i21,i32)
     1 + f22_f32 * a(ib1,i22,i32)

      enddo    ! do ib1 = 1 , nb1

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_interpolate_fft(
     1 nx_inp,dx_inp,x_inp
     1,nx_out,dx_out,x_out
     1,m_work,work
     1,i_err)
c  interpolate from nx_inp to nx_out points by fft
c  nx_out must be 2**power * nx_inp
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer  util_pow2

      integer  nx_inp
      real     dx_inp
      real     x_inp(nx_inp)

      integer  nx_out
      real     dx_out
      real     x_out(nx_out)

      integer  m_work
      real     work(m_work)

      integer  i_err

      integer  i_1d_fft
      integer  nf_inp,nf_out,nf_fft,n_work,n_skip
      real     scale_fft
      real     dx_min,dx_max

      i_err = 0

c  get the power of 2 for the input and output vector lengths
      dx_min = min(dx_inp,dx_out)
      dx_max = max(dx_inp,dx_out)
      n_skip = nint(dx_max/dx_min)
      nf_inp = util_pow2(nx_inp)
      nf_out = util_pow2(nx_out)
      nf_fft = max(nf_inp,nf_out)
      n_work = nf_fft * 2

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'(/,'' util_interpolate_fft''
c     1,/,'' nf_inp='',i8,'' nx_inp='',i8,'' dx_inp='',f12.4
c     1,/,'' nf_out='',i8,'' nx_out='',i8,'' dx_out='',f12.4
c     1,/,'' n_skip='',i8,'' nf_fft='',i8
c     1,/,'' m_work='',i8,'' n_work='',i8
c     1)')
c     1 nf_inp,nx_inp,dx_inp
c     1,nf_out,nx_out,dx_out
c     1,n_skip,nf_fft
c     1,m_work,n_work

c  make sure the input and output incrmenets are the right size
      if (abs(n_skip*dx_min-dx_max) .gt. 1.e-3) goto 997

c  if this is decimation do so now
      if (dx_out .ge. dx_inp) then

        call util_copy_inc(nx_out,n_skip,x_inp,1,x_out)

c  use fft to interpolate by factor of 2
      else    ! if (dx_out .ge. dx_inp) then

        if (m_work .lt. n_work) goto 998
 
c  forward fft sign
      i_1d_fft = + 1

c  zero the fft work space
      call util_setr(nf_fft*2,work,0.)

c  copy the real input vector to the complex fft work space
      call util_copy_inc(nx_inp,1,x_inp,2,work)

c  take forward fft nf_inp samples long
      call util_fft(+i_1d_fft,nf_inp,work)

c  take inverse fft nf_out samples long
      call util_fft(-i_1d_fft,nf_out,work)

c  copy the complex output fft work space to the real output vector
      call util_copy_inc(nx_out,2,work,1,x_out)

c  apply fft scale
c      scale_fft = .5 / nf_inp ! 1d fft scale factor
c      call util_scale(nx_out,x_out,scale_fft)

      endif    ! if (dx_out .ge. dx_inp) then

      return

  997 continue
      write(util_err()
     1,'(/,'' error in util_interpolate_fft in vector increments'')')
      goto 999

  998 continue
      write(util_err()
     1,'(/,'' error in util_interpolate_fft in memory allocation'')')
      goto 999

  999 continue
      write(util_err(),'(/,'' error in util_interpolate_fft''
     1,/,'' nf_inp='',i8,'' nx_inp='',i8,'' dx_inp='',f12.4
     1,/,'' nf_out='',i8,'' nx_out='',i8,'' dx_out='',f12.4
     1,/,'' n_skip='',i8,'' nf_fft='',i8
     1,/,'' m_work='',i8,'' n_work='',i8
     1)')
     1 nf_inp,nx_inp,dx_inp
     1,nf_out,nx_out,dx_out
     1,n_skip,nf_fft
     1,m_work,n_work
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_ave_sum(scale,n1,ni,v1,v2)
c  average ni points
      implicit  none
      integer n1,ni,n2,i1,i2,j1,j2,ni2,i1a,i1b
      real    scale,v1(1),v2(1)

      if (ni .eq. 1) then

        call util_copy(n1,v1,v2)

      else

        n2 = (n1 - 1 ) / ni + 1
        ni2 = ni / 2 + 1
        do i2 = 1 , n2
          i1 = (i2 - 1) * ni + 1
          i1a = max(i1-ni2, 1)
          i1b = min(i1+ni2,n1)
          v2(i2) = 0.

          do j1 = i1a , i1b
            v2(i2) = v2(i2) + v1(j1)
          enddo    ! do j1 = i1a , i1b

          v2(i2) = scale * v2(i2) / (i1b-i1a+1)

        enddo    ! do i2 = 1 , n2

      endif


        n2 = (n1 - 1 ) / ni + 1
        ni2 = ni / 2 + 1

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_sum_n(n1,n2,x1,x2)
c  sum x2(i1) = sum i2=1,n2 x1(i1,i2) for i1 = 1,n1  dim x1(n1,1)
      implicit  none
      integer  n1,n2
      real     x1(n1,1),x2(1)
      integer  i1,i2
      call util_setr(n1,x2,0.)
      do i2 = 1 , n2
        do i1 = 1 , n1
          x2(i1) = x2(i1) + x1(i1,i2)
        enddo    ! do i1 = 1 , n1
      enddo    ! do i2 = 1 , n2
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_shift(n,n_shift,x1,x2)
c  shift x by n_shift points
      implicit  none
      integer  n,n_shift,i
      real     x1(1),x2(1)
      if (n_shift .ge. 0) then
        do i = n , 1 , -1
          x2(i+n_shift) = x1(i)
        enddo    ! do i = n , 1 , -1
      else    ! if (n_shift .ge. 0) then
        do i = 1 , n
          x2(i+n_shift) = x1(i)
        enddo    ! do i = 1 , n
      endif    ! if (n_shift .ge. 0) then
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_rep_blanks(c1,c2)
c  replace null characters with blanks
      implicit  none
      character *(*) c1,c2
      integer i,lc
      c2 = c1
      do i = 1 , len(c2)
        if (c2(i:i) .eq. char(0)) c2(i:i) = ' '
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_ncpu(n_cpu)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer  n_cpu,m_cpu
      common/craycpu/m_cpu

      integer  i_call
      data     i_call/0/
      i_call = i_call + 1

c      if (i_call .eq. 1 .and. util_dbg() .ge. 0) write(util_dbg()
c     1,'(/,'' util_ncpu ncpu='',i8)')m_cpu

      n_cpu = max(1,m_cpu)

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_cpu(t_cpu)
c return current cpu time
      implicit  none
      real     t_cpu

      call tsecnd(t_cpu)    ! current cpu time

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_icray(j_cray)
c  return value of cray flag  - flag for cray (1) or vax(0)
      implicit  none
      integer  i_cray,j_cray
      common   /craysys/i_cray    ! flag for cray (1) or vax(0)
      j_cray = max(0,min(1,i_cray))         ! cray flag
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      real function util_bg(nb)
c  convert a number from 512 word blocks to gigabytes 
c   1 word =    8 bytes
c 512 word = 4096 bytes
      implicit  none
      integer  nb
      util_bg = float(nb) * 4096. / 1.e9
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      real function util_gb(nb)
c  convert a number from from gigabytes to 512 word blocks
c   1 word =    8 bytes
c 512 word = 4096 bytes
      implicit  none
      integer  nb
      util_gb = float(nb) * 1.e9 / 4096.
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      function util_invert_1(x)
      implicit  none
      real util_invert_1
      real x
      real eps
      data eps/1.e-10/
      util_invert_1 = 0.
      if (x .ne. 0.) util_invert_1 = 1. / x

c      if (abs(x) .ge. eps) then

c        util_invert_1 = 1. / x

c      else    ! if (abs(x) .ge. eps) then

c        util_invert_1 = 0.

c      endif    ! if (abs(x) .ge. eps) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_len_r(str)
c  find the last non blank character
      character str*(*)
      do j = len(str) , 1 , -1
        util_len_r = j
        if (str(j:j) .ne. ' ') return
      enddo
      util_r = 0
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_r(str)
c  find the last non blank character
      character str*(*)
      do j = len(str) , 1 , -1
        util_r = j
        if (str(j:j) .ne. ' ') return
      enddo
      util_r = 0
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      logical function util_spac(card)
c  is card oone of the delimiters
      character *1 card
      parameter (mc=3)
      character c(mc)*1
      data c/' ',',','('/
      util_spac = .false.
      do 1 i = 1 , mc
        util_spac = util_spac .or. (card .eq. c(i))
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      real function util_rd(x)
      implicit  none
      real     x
      util_rd = x * 90. / asin(1.)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      real function util_dr(x)
      implicit  none
      real     x
      util_dr = x * asin(1.) / 90.
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_fetch_i(name,value)
      implicit  none

      character name*(*)
      integer   value

      integer   x

      integer i_call
      data    i_call/0/
      i_call = i_call + 1

      x = -999

        call util_decode_i(x,name)

      if (x .ne. -999) then
        util_fetch_i = 1
        value = x
      else
        util_fetch_i = 0
      endif

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_fetch_r(name,value)
      implicit  none

      character name*(*)
      real      value

      real      x

      integer i_call
      data    i_call/0/
      i_call = i_call + 1

      x = -999.

        call util_decode_r(x,name)

      if (abs(x+999.) .gt. .01) then
        util_fetch_r = 1
        value = x
      else
        util_fetch_r = 0
      endif

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_fetch_d(name,value)
      implicit  none

      character name*(*)
c      double precision value
      real value

      real      x

      integer i_call
      data    i_call/0/
      i_call = i_call + 1

      x = -999.

        call util_decode_r(x,name)

      if (abs(x+999.) .gt. .01) then
        util_fetch_d = 1
        value = x
      else
        util_fetch_d = 0
      endif

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_fetch_c(name,value)
      implicit  none

      character name*(*)
      character value*(*)

      character x*132
      integer nx

      integer i_call
      data    i_call/0/
      i_call = i_call + 1

      x = '999'

        call util_decode_c(x,nx,name)

      if (x(1:3) .ne. '999') then
        util_fetch_c = 1
        value = x
      else
        util_fetch_c = 0
      endif

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_cros(i_cros
     1,x0,z0
     1,x1_l,z1_l
     1,x1_r,z1_r
     1,x2_l,z2_l
     1,x2_r,z2_r
     1)
c  determine where the line segements defined by x1_l,z1_l to x1_r,z1_r
c  and x2_l,z2_l to x2_r,z2_r  cross and if it is between the endpoints
c  if it is i_cros = 1 if not i_cros = 0
      implicit  none

      real     util_dot

      integer  i_cros
      real     x0,z0
      real     x1_l,z1_l
      real     x1_r,z1_r
      real     x2_l,z2_l
      real     x2_r,z2_r

      real     dx1,dz1
      real     dx2,dz2
      real     denom

      dx1 = x1_r - x1_l
      dz1 = z1_r - z1_l
      dx2 = x2_r - x2_l
      dz2 = z2_r - z2_l
      denom = dz1 * dx2 - dx1 * dz2

      if (denom .eq. 0.) then

        x0 = 0
        z0 = 0
        i_cros = 0

      else    ! if (denom .eq. 0.) then

        x0 = (x1_l*dz1*dx2 - x2_l*dx1*dz2 
     1+ (z2_l-z1_l) * dx1*dx2) / denom
        z0 = (z2_l*dz1*dx2 - z1_l*dx1*dz2 
     1+ (x1_l-x2_l) * dz1*dz2) / denom

c if the dot product of the vectors from the two segments
c to x0, z0 is positive the vectors are in the same direction and
c  the segments do not cross

        if (util_dot(x0,z0,x1_l,z1_l,x0,z0,x1_r,z1_r) .gt. 0.
     1 .or. util_dot(x0,z0,x2_l,z2_l,x0,z0,x2_r,z2_r) .gt. 0.) then

          i_cros = 0

        else    ! if (util_dot(x0,z0,x1_l,z1_l,x0,z0,x1_r,z1_r) .gt. 0.

          i_cros = 1

        endif    ! if (util_dot(x0,z0,x1_l,z1_l,x0,z0,x1_r,z1_r) .gt. 0.

      endif    ! if (denom .eq. 0.) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      real function util_dot(x1_l,z1_l,x1_r,z1_r,x2_l,x2_r,z2_l,z2_r)

      implicit  none

      real    x1_l,z1_l
      real    x1_r,z1_r
      real    x2_l,x2_r
      real    z2_l,z2_r

      util_dot = (x1_r - x1_l) * (x2_r - x2_l) 
     1         + (z1_r - z1_l) * (z2_r - z2_l)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_inquire_exist(file,*)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer   util_len_r

      character file*(*)

      logical   exist

      inquire (file=file,exist=exist)    ! make sure this file exists

      if ( .not. exist) then

        write(6
     1,'('' this file does not exist file:'',a)')
     1 file(1:util_len_r(file))
        return 1

      endif    ! if ( .not. exist) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      logical function util_char_end(card)
c  set logical variable to .true. 
c  if character string starts with E,e,Q,q
      implicit  none

      character card*(*)

      if (card(1:1) .eq. 'E'
     1.or. card(1:1) .eq. 'e'
     1.or. card(1:1) .eq. 'Q'
     1.or. card(1:1) .eq. 'q'
     1) then

        util_char_end = .true.

      else

        util_char_end = .false.

      endif

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_copy_file_name(file1,file2)
      character *(*) file1,file2
      file2 = file1
      i1 = index(file1,']') + 1
      call util_remove_blanks(lfile,file2)
      do 1 i = i1 , len(file2)
        if (file2(i:i) .eq. '.') then
          do 2 j = i , len(file2)
            file2(j:j) = ' '
    2     continue
          goto 3
        endif
    1 continue
    3 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_remove_blanks(nstr,str)
      character str*(*)
      nstr = 0
      do 1 i = 1 , len(str)
        if (str(i:i) .ne. ' ')  then
          nstr = nstr + 1
          str(nstr:nstr) = str(i:i)
        endif
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_line_inc(n,i,x,x0,dx)
c  set an array to a constant plus a linear gradient
      implicit  none
      integer  n,i,j
      real     x(i,1),x0,dx
      do j = 1 , n
        x(1,j) = x0 + (j - 1) * dx
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_copy_n(m,n
     1,x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7,x8,y8,x9,y9,x0,y0)

      implicit  none
      integer  m,n
      real     x1,x2,x3,x4,x5,x6,x7,x8,x9,x0
      real     y1,y2,y3,y4,y5,y6,y7,y8,y9,y0
      if (m .ge. 1 ) call util_copy(n,x1,y1)
      if (m .ge. 2 ) call util_copy(n,x2,y2)
      if (m .ge. 3 ) call util_copy(n,x3,y3)
      if (m .ge. 4 ) call util_copy(n,x4,y4)
      if (m .ge. 5 ) call util_copy(n,x5,y5)
      if (m .ge. 6 ) call util_copy(n,x6,y6)
      if (m .ge. 7 ) call util_copy(n,x7,y7)
      if (m .ge. 8 ) call util_copy(n,x8,y8)
      if (m .ge. 9 ) call util_copy(n,x9,y9)
      if (m .ge. 10) call util_copy(n,x0,y0)

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_copy_pinp(nx_out,ix_inp,x_inp,x_out)
c  copy nx_out values using x_out(ix_out) = x_inp(ix_inp(ix_out)) 
c  ix_out=1,nx_out
      implicit  none
      integer  nx_out
      integer  ix_inp(nx_out)
      real     x_inp(1)
      real     x_out(1)

      integer  ix_out

      do ix_out = 1 , nx_out

        x_out(ix_out) = x_inp(ix_inp(ix_out))

      enddo    ! do ix_out = 1 , nx_out

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_copy_pout(nx_inp,ix_out,x_inp,x_out)
c  copy nx_inp values using x_out(ix_out(ix_inp)) = x_inp(ix_inp)
c  ix_inp=1,nx_inp
      implicit  none
      integer  nx_inp
      integer  ix_out(nx_inp)
      real     x_inp(1)
      real     x_out(1)

      integer  ix_inp

      do ix_inp = 1 , nx_inp

        x_out(ix_out(ix_inp)) = x_inp(ix_inp)

      enddo    ! do ix_inp = 1 , nx_inp

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_invert_inc(i,n,x)
      implicit  none
      integer n,i,j
      real x(i,1)
      do j = 1 , n
        if (x(1,j) .ne. 0.) x(1,j) = 1. / x(1,j)
      enddo    ! do j = 1 , n
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_min_max_inc(x_min,x_max,i,n,x)
c  determine min,max locations

      implicit  none

      integer n,i,j
      real    x_min,x_max,x(i,n)

      if (n .eq. 0) then
        x_min = 0.
        x_max = 0.
      else
        x_min = x(1,1)
        x_max = x(1,1)
        do j = 1 , n
          x_min = min(x_min,x(1,j))
          x_max = max(x_max,x(1,j))
        enddo
      endif

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_abs_max_inc(x_max,i,n,x)
c  determine min,max locations

      implicit  none

      integer n,i,j
      real    x_max,x(i,n)

      if (n .eq. 0) then
        x_max = 0.
      else
        x_max = abs(x(1,1))
        do j = 1 , n
          x_max = max(x_max,abs(x(1,j)))
        enddo
      endif

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_fill_vector(n_fill,n,i,x,x_fill)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer  n_fill,n,i
      real     x(i,n)
      real     x_fill
      integer  i1,i2,i3

      n_fill = 0

c  find the first non flag value
      call util_find_flag(i1,1,n,i,x,x_fill)
      if (i1 .eq. 0) goto 2
      call util_setr_inc(i,i1-1,x(1,   1),x(1,i1))
      n_fill = n_fill + max(0,i1-1)

c  find the last non flag value
      call util_find_flag(i2,n,1,i,x,x_fill)
      call util_setr_inc(i,n-i2,x(1,i2+1),x(1,i2))
      n_fill = n_fill + (n - i2)

      i3 = i2

    1 continue
c  find the next non flagged value
        if (i1 .ge. i3) goto 2

        call util_find_flag(i2,i1+1,n,i,x,x_fill)
        call util_line_inc(i2-i1-1,i,x(1,i1+1)
     1,x(1,i1),(x(1,i2)-x(1,i1))/max(1,i2-i1))
        n_fill = n_fill + max(0,i2-i1-1)
        i1 = i2
        goto 1

    2 continue

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' n='',i10,'' n_fill='',i10)')n,n_fill

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_fill_vector_1(n_fill,n,i,x,x_fill)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer  n_fill,n,i
      real     x(i,n)
      real     x_fill
      integer  i1,i2,i3

      n_fill = 0

c  find the first non flag value
      call util_find_flag(i1,1,n,i,x,x_fill)
      if (i1 .eq. 0) goto 2

c  find the last non flag value
      call util_find_flag(i2,n,1,i,x,x_fill)

      i3 = i2

    1 continue
c  find the next non flagged value
        if (i1 .ge. i3) goto 2

        call util_find_flag(i2,i1+1,n,i,x,x_fill)
        call util_line_inc(i2-i1-1,i,x(1,i1+1)
     1,x(1,i1),(x(1,i2)-x(1,i1))/max(1,i2-i1))
        n_fill = n_fill + max(0,i2-i1-1)
        i1 = i2
        goto 1

    2 continue

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' n='',i10,'' n_fill='',i10)')n,n_fill

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_fill_vector_2(n1,n2,x,x_fill)
      implicit  none
      integer  n1,n2
      real     x(n1,n2),x_fill
      integer  i1,i2
      integer  n_fill

      do i2 = 1 , n2

        call util_fill_vector(n_fill,n1,1,x(1,i2),x_fill)

c        write(82,'('' i2='',i8,'' n_fill='',i8)')i2,n_fill

      enddo    ! do ix_tab = 1 , nx_tab

      do i1 = 1 , n1

        call util_fill_vector(n_fill,n2,n1,x(i1,1),x_fill)

c        write(82,'('' i1='',i8,'' n_fill='',i8)')i1,n_fill

      enddo    ! do i1 = 1 , n1

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_find_flag(i_flag,i1,i2,i,x,x_fill)
      implicit  none
      integer  i_flag,i1,i2,i
      real     x(i,1)
      real     x_fill
      real     eps
      integer  j

      eps = abs(x_fill) * 1e-10
      if (eps .eq. 0.) eps= 1e-10

c  find the first non flag value
      do j = i1 , i2 , isign(1,i2-i1)

        if (abs(x(1,j)-x_fill) .gt. eps) then
          i_flag = j
          goto 1
        endif

      enddo

      i_flag = 0

    1 continue

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_ave_n(n1,n2,x_inp,x_out)
c  compute n2 average vectors
      implicit  none
      integer  n1,n2,i1
      real     x_inp(n1,n2),x_out(n1),x_ave
      do i1 = 1 , n1
        call util_ave(n2,n1,x_inp(i1,1),x_ave)
        x_out(i1) = x_ave
      enddo    ! do i1 = 1 , n1
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_ave(n,i,x,x_ave)
c  comupte the sum of a vector
      implicit  none
      integer  n,i
      real     x(i,1),x_ave,xsum
      call util_sum(n,i,x,xsum)
      if (n .ne. 0) x_ave = xsum / n
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_ave_abs(n,i,x,x_ave)
c  comupte the mean of the absolute values of a vector
      implicit  none
      integer  n,i,J
      real     x(i,1),x_ave,xsum

      xsum = 0.
      do j = 1 , n
        xsum = xsum + abs(x(1,j))
      enddo
      if (n .ne. 0) x_ave = xsum / n

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_sum(n,i,x,xsum)
c  comupte the sum of a vector
      implicit  none
      integer  n,i,j
      real     x(i,1),xsum
      xsum = 0.
      do j = 1 , n
        xsum = xsum + x(1,j)
      enddo
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_swap(nx,x1,x2)
c  swap x1,x2
      implicit  none

      integer  nx
      real     x1(nx),x2(nx)

      integer  ix
      real     x0

      do ix = 1 , nx

        x0 = x2(ix)
        x2(ix) = x1(ix)
        x1(ix) = x0

      enddo    ! do ix = 1 , nx

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_exp(nx,x,x0,dx)
c compute an exponential
      implicit  none
      integer  nx
      real     x(nx)
      real     x0,dx

      integer  ix

      do ix = 1 , nx

        x(ix) = exp((ix-1)*dx+x0)

      enddo    ! do ix = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      real function util_atan2(x,z)
      implicit  none
      real     x,z
      if (x .eq. 0. .and. z. eq. 0.) then
        util_atan2 = 0.
      else
        util_atan2 = atan2(x,z)
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_print_depths(title,lu
     1,nx_tab,x0_tab,dx_tab
     1,ny_tab,y0_tab,dy_tab
     1,z_inp,z_out)

      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer   util_len_r

      character title*(*)

      integer   lu

      integer   nx_tab
      real      x0_tab,dx_tab

      integer   ny_tab
      real      y0_tab,dy_tab

      real      z_inp(nx_tab,ny_tab)
      real      z_out(nx_tab,ny_tab)

      integer   ix_tab
      real      x_tab

      integer   iy_tab
      real      y_tab

      real      z_inp_min,z_inp_max
      real      z_out_min,z_out_max

      integer   ix_inc,iy_inc

      call util_min_max(z_inp_min,z_inp_max,nx_tab*ny_tab,z_inp)
      call util_min_max(z_out_min,z_out_max,nx_tab*ny_tab,z_out)

      write(lu,'(/,''  util_print_depths '',/,a
     1,/,'' nx_tab='',i8,'' x0_tab='',f10.2,'' x1_tab='',f10.2
     1,'' dx_tab='',f10.2
     1,/,'' ny_tab='',i8,'' y0_tab='',f10.2,'' y1_tab='',f10.2
     1,'' dy_tab='',f10.2
     1,/,'' z_inp_min='',f10.2,'' z_inp_max='',f10.2
     1,/,'' z_out_min='',f10.2,'' z_out_max='',f10.2
     1,/,''      x_tab      y_tab      z_inp      z_out'')')
     1title(1:util_len_r(title))
     1,nx_tab,x0_tab,(nx_tab-1)*dx_tab+x0_tab,dx_tab
     1,ny_tab,y0_tab,(ny_tab-1)*dy_tab+y0_tab,dy_tab
     1,z_inp_min,z_inp_max
     1,z_out_min,z_out_max

      ix_inc = 10
      if (ny_tab .eq. 1) ix_inc = 5
      iy_inc = 10
      if (ny_tab .eq. 1) iy_inc = 5

      do ix_tab = 1 , nx_tab , ix_inc

        x_tab = (ix_tab - 1) * dx_tab + x0_tab

        do iy_tab = 1 , ny_tab , iy_inc

          y_tab = (iy_tab - 1) * dy_tab + y0_tab

c      if (util_dbg() .ge. 0) 
      write(util_prn(),'(1x,f10.2,1x,f10.2,1x,f10.2,1x,f10.2)')
     1 x_tab,y_tab,z_inp(ix_tab,iy_tab),z_out(ix_tab,iy_tab)

        enddo    ! do iy_tab = 1 , ny_tab , iy_inc

      enddo    ! do ix_tab = 1 , nx_tab , ix_inc

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_print_depths_1(title,lu
     1,nx_tab,x0_tab,dx_tab
     1,ny_tab,y0_tab,dy_tab
     1,z_inp)

      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer   util_len_r

      character title*(*)

      integer   lu

      integer   nx_tab
      real      x0_tab,dx_tab

      integer   ny_tab
      real      y0_tab,dy_tab

      real      z_inp(nx_tab,ny_tab)

      integer   ix_tab
      real      x_tab

      integer   iy_tab
      real      y_tab

      real      z_inp_min,z_inp_max

      integer   ix_inc,iy_inc

      call util_min_max(z_inp_min,z_inp_max,nx_tab*ny_tab,z_inp)

      write(lu,'(/,''  util_print_depths '',/,a
     1,/,'' nx_tab='',i8,'' x0_tab='',f10.2,'' x1_tab='',f10.2
     1,'' dx_tab='',f10.2
     1,/,'' ny_tab='',i8,'' y0_tab='',f10.2,'' y1_tab='',f10.2
     1,'' dy_tab='',f10.2
     1,/,'' z_inp_min='',f10.2,'' z_inp_max='',f10.2
     1,/,''      x_tab      y_tab      z_inp'')')
     1title(1:util_len_r(title))
     1,nx_tab,x0_tab,(nx_tab-1)*dx_tab+x0_tab,dx_tab
     1,ny_tab,y0_tab,(ny_tab-1)*dy_tab+y0_tab,dy_tab
     1,z_inp_min,z_inp_max

      ix_inc = 10
      if (ny_tab .eq. 1) ix_inc = 5
      iy_inc = 10
      if (ny_tab .eq. 1) iy_inc = 5

      do ix_tab = 1 , nx_tab , ix_inc

        x_tab = (ix_tab - 1) * dx_tab + x0_tab

        do iy_tab = 1 , ny_tab , iy_inc

          y_tab = (iy_tab - 1) * dy_tab + y0_tab

c      if (util_dbg() .ge. 0) 
      write(util_prn(),'(1x,f10.2,1x,f10.2,1x,f10.2)')
     1 x_tab,y_tab,z_inp(ix_tab,iy_tab)

        enddo    ! do iy_tab = 1 , ny_tab , iy_inc

      enddo    ! do ix_tab = 1 , nx_tab , ix_inc

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_compute_map(i_map,f_map,nx_inp,x_inp,nx_out,x_out)
c  Compute a map function from x_inp to x_out
      implicit  none
      integer  nx_out,nx_inp
      integer  i_map(nx_out)
      real     f_map(nx_out)
      real     x_inp(nx_inp)
      real     x_out(nx_out)

      integer  ix_out,ix_inp,jx_inp,kx_inp
      real     dx

c  compute the map from input to output
c for each value of x_out find the two values of x_inp that stradlle it
      jx_inp = 2

      do ix_out = 1 , nx_out

        do ix_inp = jx_inp , nx_inp

          i_map(ix_out) = ix_inp - 1
          if (x_inp(ix_inp) .gt. x_out(ix_out)) goto 1

        enddo    ! do ix_inp = jx_inp , nx_inp

    1   continue

        jx_inp = max(1,i_map(ix_out)-2)

      enddo    ! do ix_out = 2 , nx_out

      do ix_out = 1 , nx_out

        dx = x_inp(i_map(ix_out)+1) - x_inp(i_map(ix_out))
        if (dx .ne. 0.)
     1dx = (x_out(ix_out) - x_inp(i_map(ix_out))) / dx
        f_map(ix_out) = 1. - max(0.,min(1.,dx))

      enddo    ! do ix_out = 1 , nx_out

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_apply_map(nx,i_map,f_map,x1,x2)
c-- map x1 to x2 via the map function
      implicit  none
      integer  nx
      integer  i_map(nx)
      real     f_map(nx)
      real     x1(nx)
      real     x2(nx)

      integer  ix

      do ix = 1 , nx
        x2(ix) = x1(i_map(ix)  ) * (1. - f_map(ix))
     1         + x1(i_map(ix)+1) *       f_map(ix)
      enddo    ! do ix = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_tofz(nz_dat,z0_dat,dz_dat,z_datum,vofz,tofz,work)
c  determine the travel time to each depth point t=0 at z= z_datum
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      real     util_invert_1

      integer  nz_dat
      real     z0_dat,dz_dat,z_datum
      real     vofz(nz_dat),tofz(nz_dat),work(nz_dat)

      integer  iz_dat
      real     v_datum

      integer  iz_m,nz_m
      real     z_m,v_m,t_m

      integer  iz_p,nz_p
      real     z_p,v_p,t_p

c  determine the grid node at which z >= z_datum
      nz_p = 0
      iz_p = int((z_datum-z0_dat)/dz_dat) + 1
      z_p  = (iz_p - 1) * dz_dat + z0_dat
      v_p  = vofz(max(1,min(nz_dat,iz_p)))

c  determine the grid node at which z <  z_datum
      nz_m = 0
      iz_m = iz_p - 1
      z_m  = (iz_m - 1) * dz_dat + z0_dat
      v_m  = vofz(max(1,min(nz_dat,iz_m)))

c  determine the velocity at the datum level
      v_datum = v_m + (z_datum - z_m) * (v_p - v_m) / dz_dat

c  determine the two way travel time at z_m
      t_m = (z_m - z_datum) * (v_m + v_datum)

c  determine the two way travel time at z_p
      t_p = (z_p - z_datum) * (v_p + v_datum)

c      if (util_dbg() .ge. 0) write(util_dbg(),'(/,'' util_tofz''
c     1,/,'' z_datum='',f10.2,'' v_datum='',f10.2
c     1,'' v1='',f10.2,'' vn='',f10.2
c     1,/,'' nz_dat='',i8,'' z0_dat='',f10.2,'' z1_dat='',f10.2
c     1,'' dz_dat='',f10.2
c     1,/,'' iz_m='',i8,'' z_m='',f10.2,'' v_m='',f10.2,'' t_m='',f10.5
c     1,/,'' iz_p='',i8,'' z_p='',f10.2,'' v_p='',f10.2,'' t_p='',f10.5
c     1)')
c     1 z_datum,util_invert_1(v_datum)
c     1,util_invert_1(vofz(1)),util_invert_1(vofz(nz_dat))
c     1,nz_dat,z0_dat,(nz_dat-1)*dz_dat+z0_dat,dz_dat
c     1,iz_m,z_m,util_invert_1(v_m),t_m
c     1,iz_p,z_p,util_invert_1(v_p),t_p

c  fill in points from iz_p to nz_dat
      if (iz_p .le. nz_dat) then

        nz_p       = nz_p + 1
        work(nz_p) = t_p

        do iz_dat = max(iz_p,1)+1 , nz_dat

          nz_p       = nz_p + 1
          work(nz_p) = dz_dat * (vofz(iz_dat) + vofz(iz_dat-1))

        enddo    ! do iz_dat = 1 , nz_dat

c  recursive sum the time increments in work into the summed time in tofz
        call util_rcsum(nz_p,work,tofz(max(iz_p,1)))

      endif    ! if (iz_p .le. nz_dat) then

c  fill in points from 1 to iz_m
      if (iz_m .ge. 1) then

        nz_m       = nz_m + 1
        tofz(nz_m) = t_m

        do iz_dat = min(iz_m,nz_dat)-1, 1 , -1

          nz_m       = nz_m + 1
          tofz(nz_m) = dz_dat * (vofz(iz_dat) + vofz(iz_dat+1))

        enddo    ! do iz_dat = 1 , nz_dat

c  recursive sum the time increments in tofz into the summed time in work
        call util_rcsum(nz_m,tofz,work)
        
c  transfer the summed time from work to tofz in reverse order
        call util_copy_r(nz_m,work,tofz)
      
      endif    ! if (iz_m .ge. 1) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_rcsum(nx,x1,x2)
c  compute recusive sum from x1 to x2
      implicit  none
      integer  nx
      real     x1(nx),x2(nx)

      integer  ix
      real     x_sum
c      x_sum = 0.
c      do ix = 1 , nx
c        x_sum = x_sum + x1(ix)
c        x2(ix) = x_sum
c      enddo    ! do ix = 1 , nx
      call rcsum(nx,x1,x2)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_bicubic_coefficients(
     1 nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,vel,vcof)
c-----------------------------------------------------------------
c     Purpose: Compute Bicubic interpolation coefficients 
c              at nodes in a 2-D gridded model.  The values are input
c              to bcucof.f, described in "Numerical Recipies in FORTRAN,
c              which computes coefficient needed in bicubic interpolatio
c-----------------------------------------------------------------------
c
      implicit  none

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  nz_vel
      real     z0_vel,dz_vel

      real     vel(nz_vel,nx_vel)
      real     vcof(4,4,nz_vel,nx_vel)

      integer  ix_vel,iz_vel
      integer  ix_1,ix_2,ix_3,ix_4
      integer  iz_1,iz_2,iz_3,iz_4
      real     vfun(4),dvdx(4),dvdz(4),dvd2(4)

c  compute the coefficients for each set of four points
      do iz_vel = 1 , max(1,nz_vel-1)

        do ix_vel = 1 , max(1,nx_vel-1)

          ix_1 = ix_vel
          ix_2 = ix_vel + 1
          ix_3 = ix_vel + 1
          ix_4 = ix_vel

          iz_1 = iz_vel
          iz_2 = iz_vel
          iz_3 = iz_vel + 1
          iz_4 = iz_vel + 1

          call util_bicubic_derivitives_1(
     1 iz_1,ix_1
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,vel
     1,vfun(1),dvdz(1),dvdx(1),dvd2(1)
     1)

          call util_bicubic_derivitives_1(
     1 iz_2,ix_2
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,vel
     1,vfun(2),dvdz(2),dvdx(2),dvd2(2)
     1)

          call util_bicubic_derivitives_1(
     1 iz_3,ix_3
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,vel
     1,vfun(3),dvdz(3),dvdx(3),dvd2(3)
     1)

          call util_bicubic_derivitives_1(
     1 iz_4,ix_4
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,vel
     1,vfun(4),dvdz(4),dvdx(4),dvd2(4)
     1)

          call util_bicubic_coefficients_1(
     1 dz_vel,dx_vel
     1,vfun,dvdz,dvdx,dvd2
     1,vcof(1,1,iz_vel,ix_vel)
     1)

        enddo    ! do ix_vel = 1 , max(1,nx_vel-1)

      enddo    ! do iz_vel = 1 , max(1,nz_vel-1)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_bicubic_derivitives_1(
     1 iz_vel,ix_vel
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,vel
     1,vfun,dvdz,dvdx,dvd2
     1)

c  compute the function, its first derivitive in each direction and 
c  the second cross derivitive at node ix_vel,iz_vel
      implicit  none

      integer  ix_vel,iz_vel

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  nz_vel
      real     z0_vel,dz_vel

      real     vel(nz_vel,nx_vel)

      real     vfun,dvdx,dvdz,dvd2

      integer  ix_1,ix_2,ix_3
      integer  iz_1,iz_2,iz_3

      real     v_11,v_12,v_13
      real     v_21,v_22,v_23
      real     v_31,v_32,v_33

      ix_2 = max(1,min(nx_vel,ix_vel))
      ix_1 = max(ix_2-1,1)
      ix_3 = min(ix_2+1,nx_vel)

      iz_2 = max(1,min(nz_vel,iz_vel))
      iz_1 = max(iz_2-1,1)
      iz_3 = min(iz_2+1,nz_vel)

      v_11 = vel(iz_1,ix_1)
      v_21 = vel(iz_2,ix_1)
      v_31 = vel(iz_3,ix_1)

      v_12 = vel(iz_1,ix_2)
      v_22 = vel(iz_2,ix_2)
      v_32 = vel(iz_3,ix_2)

      v_13 = vel(iz_1,ix_3)
      v_23 = vel(iz_2,ix_3)
      v_33 = vel(iz_3,ix_3)

      vfun = v_22
      dvdx = (v_23 - v_21) * .5 / dx_vel
      dvdz = (v_32 - v_12) * .5 / dz_vel
      dvd2 = (v_33 - v_31 - v_13 + v_11) *.25 / (dx_vel*dz_vel)

      if (ix_2 .eq. 1 .or. ix_2 .eq. nx_vel) then

        dvdx = 0.
        dvd2 = 0.

      endif    ! if (ix_2 .eq. 1 .or. ix_2 .eq. nx_vel) then

      if (iz_2 .eq. 1 .or. iz_2 .eq. nz_vel) then

        dvdz = 0.
        dvd2 = 0.

      endif    ! if (iz_2 .eq. 1 .or. iz_2 .eq. nz_vel) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_bicubic_velgrad_n(n_xz,z0,x0,pz,px
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,vcof
     1,vfun,dvdz,dvdx,dvdq
     1,m_work,work
     1,i_err
     1)
c--------------------------------------------------------------
c     Purpose:
c              Find velocity and some of the derivatives from the
c              model.
c--------------------------------------------------------------
c     Input :
c             z0 - z location
c             x0 - x location
c             pz - z ray parameter
c             x0 - x ray parameter
c--------------------------------------------------------------
c     Output :
c             vfun - velocity function
c             dvdx - 1st derivative with respect to global x
c             dvdz - 1st derivative with respect to global z.
c             dvdq - 2nd derivative with respect to ray centred
c                      coordinate.
c--------------------------------------------------------------

      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer  n_xz
      real     x0(n_xz),z0(n_xz),px(n_xz),pz(n_xz)

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  nz_vel
      real     z0_vel,dz_vel

      real     vcof(4,4,nz_vel,nx_vel)

      real     vfun(n_xz),dvdx(n_xz),dvdz(n_xz),dvdq(n_xz)

      integer  m_work
      real     work(m_work)

      integer  i_err

      i_err = 0
      if (m_work .lt. 7 * n_xz) goto 998

      call util_bicubic_velgrad_n0(n_xz,z0,x0,pz,px
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,vcof
     1,vfun,dvdz,dvdx,dvdq
     1,work(1+n_xz*0)
     1,work(1+n_xz*1)
     1,work(1+n_xz*2)
     1,work(1+n_xz*3)
     1,work(1+n_xz*4)
     1,work(1+n_xz*5)
     1,work(1+n_xz*6)
     1)
      return

  998 continue
      write(util_err(),'(/,'' error in util_bicubic_velgrad_n''
     1,/,'' in memory allocation''
     1,/,'' need 7*n_xz='',i8,'' and have='',i8)')
     1 7*n_xz,m_work

      goto 999

  999 continue
      write(util_err(),'(/,'' error in util_bicubic_velgrad_n'')')
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_bicubic_velgrad_n0(n_xz,z0,x0,pz,px
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,vcof
     1,vfun,dvdz,dvdx,dvdq
     1,iz,ix,zf,xf,dvz2,dvx2,dvd2
     1)
c--------------------------------------------------------------
c     Purpose:
c              Find velocity and some of the derivatives from the
c              model.
c--------------------------------------------------------------
c     Input :
c             z0 - z location
c             x0 - x location
c             pz - z ray parameter
c             x0 - x ray parameter
c--------------------------------------------------------------
c     Output :
c             vfun - velocity function
c             dvdx - 1st derivative with respect to global x
c             dvdz - 1st derivative with respect to global z.
c             dvdq - 2nd derivative with respect to ray centred
c                      coordinate.
c--------------------------------------------------------------

      implicit  none

      integer  n_xz
      real     x0(n_xz),z0(n_xz),px(n_xz),pz(n_xz)

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  nz_vel
      real     z0_vel,dz_vel

      real     vcof(4,4,nz_vel,nx_vel)

      real     vfun(n_xz),dvdx(n_xz),dvdz(n_xz),dvdq(n_xz)

      integer  ix(n_xz),iz(n_xz)
      real     xf(n_xz),zf(n_xz)
      real     dvx2(n_xz),dvz2(n_xz),dvd2(n_xz)

      integer  i,i_xz
      real     dxx,dxz,dzz

      integer  m_xz,lu
      data     m_xz/0/

      real     t_cpu_0,t_cpu_1,t_cpu_2
      data     t_cpu_0/0/

      integer  i_call
      data     i_call/0/
      i_call = i_call + 1
      call util_cpu(t_cpu_1)
      m_xz = m_xz + n_xz

      do i_xz = 1 , n_xz

      ix(i_xz) = max(1,min(nx_vel-1,int((x0(i_xz)-x0_vel)/dx_vel)+1))

      iz(i_xz) = max(1,min(nz_vel-1,int((z0(i_xz)-z0_vel)/dz_vel)+1))

      xf(i_xz) = max(0.,min(1.
     1,(x0(i_xz)-(ix(i_xz)-1)*dx_vel-x0_vel)/dx_vel))

      zf(i_xz) = max(0.,min(1.
     1,(z0(i_xz)-(iz(i_xz)-1)*dz_vel-z0_vel)/dz_vel))

      vfun(i_xz) = 0.
      dvdx(i_xz) = 0.
      dvdz(i_xz) = 0.
      dvx2(i_xz) = 0.
      dvz2(i_xz) = 0.
      dvd2(i_xz) = 0.

      enddo    ! do i_xz = 1 , n_xz

      do i = 4 , 1 , -1

      do i_xz = 1 , n_xz

        vfun(i_xz) = xf(i_xz) * vfun(i_xz) 
     1+ ((vcof(i,4,iz(i_xz),ix(i_xz)) *zf(i_xz)
     1+   vcof(i,3,iz(i_xz),ix(i_xz)))*zf(i_xz)
     1+   vcof(i,2,iz(i_xz),ix(i_xz)))*zf(i_xz)
     1+   vcof(i,1,iz(i_xz),ix(i_xz))

        dvdz(i_xz) = xf(i_xz) * dvdz(i_xz) 
     1+(3.*vcof(i,4,iz(i_xz),ix(i_xz)) *zf(i_xz)
     1+ 2.*vcof(i,3,iz(i_xz),ix(i_xz)))*zf(i_xz) 
     1+    vcof(i,2,iz(i_xz),ix(i_xz))

        dvdx(i_xz) = zf(i_xz) * dvdx(i_xz) 
     1+ (3.*vcof(4,i,iz(i_xz),ix(i_xz)) *xf(i_xz)
     1+  2.*vcof(3,i,iz(i_xz),ix(i_xz)))*xf(i_xz) 
     1+     vcof(2,i,iz(i_xz),ix(i_xz))

        dvz2(i_xz) = xf(i_xz) * dvz2(i_xz) 
     1+ 6.*vcof(i,4,iz(i_xz),ix(i_xz))*zf(i_xz) 
     1+ 2.*vcof(i,3,iz(i_xz),ix(i_xz))

        dvx2(i_xz) = zf(i_xz) * dvx2(i_xz) 
     1+ 6.*vcof(4,i,iz(i_xz),ix(i_xz))*xf(i_xz) 
     1+ 2.*vcof(3,i,iz(i_xz),ix(i_xz))

      enddo    ! do i_xz = 1 , n_xz

      enddo    ! do i = 4 , 1 , -1


      do i = 4 , 2 , -1

      do i_xz = 1 , n_xz

        dvd2(i_xz) = xf(i_xz) * dvd2(i_xz)
     1 + real(i-1) * 
     1 (3.*vcof(i,4,iz(i_xz),ix(i_xz)) *zf(i_xz)
     1 +2.*vcof(i,3,iz(i_xz),ix(i_xz)))*zf(i_xz)
     1 +   vcof(i,2,iz(i_xz),ix(i_xz))
      enddo    ! do i_xz = 1 , n_xz

      enddo    ! do i= 4 , 2 , -1

      dxx = dx_vel * dx_vel
      dxz = dx_vel * dz_vel
      dzz = dz_vel * dz_vel

      do i_xz = 1 , n_xz

      dvdx(i_xz) = dvdx(i_xz) / dx_vel
      dvdz(i_xz) = dvdz(i_xz) / dz_vel
      dvx2(i_xz) = dvx2(i_xz) / dxx
      dvz2(i_xz) = dvz2(i_xz) / dzz
      dvd2(i_xz) = dvd2(i_xz) / dxz

      dvdq(i_xz) = vfun(i_xz) * vfun(i_xz) * 
     1 (     pz(i_xz) * pz(i_xz) * dvx2(i_xz) 
     1- 2. * pz(i_xz) * px(i_xz) * dvd2(i_xz) 
     1     + px(i_xz) * px(i_xz) * dvz2(i_xz))

      enddo    ! do i_xz = 1 , n_xz

      call util_cpu(t_cpu_2)
      t_cpu_0 = t_cpu_0 + t_cpu_2 - t_cpu_1

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_bicubic_velgrad_n0_p(lu)

      write(lu,'(/,'' util_bicubic_velgrad_n0_p''
     1,/,'' i_call='',i8,'' m_xz='',i8,'' t_cpu='',f12.4)')
     1i_call,m_xz,t_cpu_0

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_bicubic_velgrad_n0_i
      t_cpu_0 = 0
      m_xz = 0
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_bicubic_velocity_n(n_xz,z0,x0
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,vcof
     1,vfun
     1,m_work,work
     1,i_err
     1)
c--------------------------------------------------------------
c     Purpose:
c              Find velocity only from the model.
c--------------------------------------------------------------
c     Input :
c             z0 - z location
c             x0 - x location
c--------------------------------------------------------------
c     Output :
c             vfun - velocity function
c--------------------------------------------------------------

      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer  n_xz
      real     x0(n_xz),z0(n_xz)

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  nz_vel
      real     z0_vel,dz_vel

      real     vcof(4,4,nz_vel,nx_vel)

      real     vfun(n_xz)

      integer  m_work
      real     work(m_work)

      integer  i_err

      i_err = 0
      if (m_work .lt. 4 * n_xz) goto 998

      call util_bicubic_velocity_n0(n_xz,z0,x0
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,vcof
     1,vfun
     1,work(1+n_xz*0)
     1,work(1+n_xz*1)
     1,work(1+n_xz*2)
     1,work(1+n_xz*3)
     1)
      return

  998 continue
      write(util_err(),'(/,'' error in util_bicubic_velocity_n''
     1,/,'' in memory allocation''
     1,/,'' need 4*n_xz='',i8,'' and have='',i8)')
     1 4*n_xz,m_work

      goto 999

  999 continue
      write(util_err(),'(/,'' error in util_bicubic_velgrad_n'')')
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_bicubic_velocity_n0(n_xz,z0,x0
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,vcof
     1,vfun
     1,iz,ix,zf,xf
     1)
c--------------------------------------------------------------
c     Purpose:
c              Find velocity and some of the derivatives from the
c              model.
c--------------------------------------------------------------
c     Input :
c             z0 - z location
c             x0 - x location
c--------------------------------------------------------------
c     Output :
c             vfun - velocity function
c--------------------------------------------------------------

      implicit  none

      integer  n_xz
      real     x0(n_xz),z0(n_xz)

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  nz_vel
      real     z0_vel,dz_vel

      real     vcof(4,4,nz_vel,nx_vel)

      real     vfun(n_xz)

      integer  ix(n_xz),iz(n_xz)
      real     xf(n_xz),zf(n_xz)

      integer  i,i_xz
      real     dxx,dxz,dzz

      do i_xz = 1 , n_xz

      ix(i_xz) = max(1,min(nx_vel-1,int((x0(i_xz)-x0_vel)/dx_vel)+1))

      iz(i_xz) = max(1,min(nz_vel-1,int((z0(i_xz)-z0_vel)/dz_vel)+1))

      xf(i_xz) = max(0.,min(1.
     1,(x0(i_xz)-(ix(i_xz)-1)*dx_vel-x0_vel)/dx_vel))

      zf(i_xz) = max(0.,min(1.
     1,(z0(i_xz)-(iz(i_xz)-1)*dz_vel-z0_vel)/dz_vel))

      vfun(i_xz) = 0.

      enddo    ! do i_xz = 1 , n_xz

      do i = 4 , 1 , -1

      do i_xz = 1 , n_xz

        vfun(i_xz) = xf(i_xz) * vfun(i_xz) 
     1+ ((vcof(i,4,iz(i_xz),ix(i_xz)) *zf(i_xz)
     1+   vcof(i,3,iz(i_xz),ix(i_xz)))*zf(i_xz)
     1+   vcof(i,2,iz(i_xz),ix(i_xz)))*zf(i_xz)
     1+   vcof(i,1,iz(i_xz),ix(i_xz))

      enddo    ! do i_xz = 1 , n_xz

      enddo    ! do i = 4 , 1 , -1

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_bicubic_velgrad(z0,x0,pz,px
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,vcof
     1,vfun,dvdz,dvdx,dvdq
     1)
c--------------------------------------------------------------
c     Purpose:
c              Find velocity and some of the derivatives from the
c              model.
c--------------------------------------------------------------
c     Input :
c             z0 - z location
c             x0 - x location
c             pz - z ray parameter
c             x0 - x ray parameter
c--------------------------------------------------------------
c     Output :
c             vfun - velocity function
c             dvdx - 1st derivative with respect to global x
c             dvdz - 1st derivative with respect to global z.
c             dvdq - 2nd derivative with respect to ray centred
c                      coordinate.
c--------------------------------------------------------------

      implicit  none

      real     x0,z0,px,pz

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  nz_vel
      real     z0_vel,dz_vel

      real     vcof(4,4,nz_vel,nx_vel)

      real     vfun,dvdx,dvdz,dvdq

      integer  ix,iz
      real     xf,zf

      integer  i
      real     dvx2,dvz2,dvd2

      ix = max(1,min(nx_vel-1,int((x0-x0_vel)/dx_vel)+1))
      iz = max(1,min(nz_vel-1,int((z0-z0_vel)/dz_vel)+1))

      xf = max(0.,min(1.,(x0-(ix-1)*dx_vel-x0_vel)/dx_vel))
      zf = max(0.,min(1.,(z0-(iz-1)*dz_vel-z0_vel)/dz_vel))

      vfun = 0.
      dvdx = 0.
      dvdz = 0.
      dvx2 = 0.
      dvz2 = 0.

      do i = 4 , 1 , -1

        vfun = xf * vfun + ((vcof(i,4,iz,ix)*zf+vcof(i,3,iz,ix))
     1*zf+vcof(i,2,iz,ix))*zf+vcof(i,1,iz,ix)

        dvdz = xf * dvdz + (3.*vcof(i,4,iz,ix)*zf
     1+2.*vcof(i,3,iz,ix))*zf + vcof(i,2,iz,ix)

        dvdx = zf * dvdx + (3.*vcof(4,i,iz,ix)*xf
     1+2.*vcof(3,i,iz,ix))*xf + vcof(2,i,iz,ix)

        dvz2 = xf * dvz2 + 6.*vcof(i,4,iz,ix)*zf + 2.*vcof(i,3,iz,ix)
        dvx2 = zf * dvx2 + 6.*vcof(4,i,iz,ix)*xf + 2.*vcof(3,i,iz,ix)

      enddo    ! do i = 4 , 1 , -1

      dvd2 = 0.

      do i = 4 , 2 , -1

        dvd2 = xf * dvd2
     1 + real(i-1) * (3.*vcof(i,4,iz,ix)*zf
     1               +2.*vcof(i,3,iz,ix))*zf+vcof(i,2,iz,ix)

      enddo    ! do i= 4 , 2 , -1

      dvdx = dvdx / dx_vel
      dvdz = dvdz / dz_vel
      dvx2 = dvx2 / dx_vel / dx_vel
      dvz2 = dvz2 / dz_vel / dz_vel
      dvd2 = dvd2 / dx_vel / dz_vel

      dvdq = vfun * vfun * 
     1 (pz * pz * dvx2 - 2. * pz * px * dvd2 + px * px * dvz2)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_bicubic_coefficients_1(
     1 dz_vel,dx_vel
     1,vfun,dvdz,dvdx,dvd2
     1,vcof
     1)
c--------------------------------------------------------------
c     Purpose:
c              Find bicubic coeffiecients for a single rectangle
c              defined by vfun,dvdx,dvdz,dvd2
c
c--------------------------------------------------------------
c     Input : 
c     the rectangle is defined by the corners 
c     in a counter clockiwise direciton
c     dz_vel = corner separation in direction 1
c     dx_vel = corner separation in direction 2
c     vfun = funcational value at 4 corners     - dimensioned 4
c     dvdz = first derivitive in direction 1    - dimensioned 4
c     dvdx = first derivitive in direction 2    - dimensioned 4
c     dvd2 = cross derivitive in directions 1,2 - dimensioned 4
c-------------------------------------------------------------
c     Output :
c     vcof = bicubic coefficients               - dimensioned 4,4
c-------------------------------------------------------------

      implicit  none
      real     dx_vel,dz_vel
      real     vfun(4),dvdx(4),dvdz(4),dvd2(4)
      real     vcof(4,4)

      integer  i,j,k,l
      real     dxz_vel,xx,cl(16),wt(16,16),x(16)
      save     wt
      data wt/1,0,-3,2,4*0,-3,0,9,-6,2,0,-6,4,8*0,3,0,-9,6,-2,0,6,-4,10*
     *0,9,-6,2*0,-6,4,2*0,3,-2,6*0,-9,6,2*0,6,-4,4*0,1,0,-3,2,-2,0,6,-4,
     *1,0,-3,2,8*0,-1,0,3,-2,1,0,-3,2,10*0,-3,2,2*0,3,-2,6*0,3,-2,2*0,
     *-6,4,2*0,3,-2,0,1,-2,1,5*0,-3,6,-3,0,2,-4,2,9*0,3,-6,3,0,-2,4,-2,
     *10*0,-3,3,2*0,2,-2,2*0,-1,1,6*0,3,-3,2*0,-2,2,5*0,1,-2,1,0,-2,4,
     *-2,0,1,-2,1,9*0,-1,2,-1,0,1,-2,1,10*0,1,-1,2*0,-1,1,6*0,-1,1,2*0,
     *2,-2,2*0,-1,1/

c     compute coefficients c

      dxz_vel = dx_vel * dz_vel

      do i = 1 , 4

        x(i)    = vfun(i)
        x(i+4)  = dvdx(i)   * dx_vel
        x(i+8)  = dvdz(i)   * dz_vel
        x(i+12) = dvd2(i) * dxz_vel

      enddo    ! do i = 1 , 4

      do i = 1 , 16

        xx = 0.

        do k = 1 , 16

          xx = xx + wt(i,k) * x(k)

        enddo    ! do k = 1 , 16

        cl(i) = xx

      enddo    ! do i = 1 , 16

      l = 0

      do i = 1 , 4

        do j = 1 , 4

          l = l + 1
          vcof(i,j) = cl(l)

        enddo    ! do j = 1 , 4

      enddo    ! do i = 1 , 4

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_3d_compute_v_grad(n_xyz
     1,z0,x0,y0
     1,pz,px,py
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vcof
     1,vfun,dvdz,dvdx,dvdy,dvdq11,dvdq12,dvdq22
     1,m_work,work
     1,i_err
     1)
c--------------------------------------------------------------
c     Purpose:
c              Special constant velocity gradient version
c              Find velocity and some of the derivatives from the
c              model.
c--------------------------------------------------------------
c     Input :
c             z0 - z location
c             x0 - x location
c             y0 - y location
c             pz - z ray parameter
c             px - x ray parameter
c             py - y ray parameter
c--------------------------------------------------------------
c     Output :
c             vfun - velocity function
c             dvdz - 1st derivative with respect to global z.
c             dvdx - 1st derivative with respect to global x
c             dvdy - 1st derivative with respect to global y
c             dvdq - 2nd derivative with respect to ray centred
c                      coordinate.
c  right now set v0,xc,yc,zc,vx,vy,vz using the first nodes of vcof
c  v(x,y,z) = v0 + vx*(x-xc) + vy*(y-yc) + vz*(z-zc)
c  dvdx = vx
c  dvdy = vy
c  dvdz = vz
c  dvdq = 0.
c--------------------------------------------------------------
 
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
 
      integer  n_xyz
      real     x0(n_xyz),y0(n_xyz),z0(n_xyz)
      real     px(n_xyz),py(n_xyz),pz(n_xyz)
 
      integer  nx_vel
      real     x0_vel,dx_vel
 
      integer  ny_vel
      real     y0_vel,dy_vel
 
      integer  nz_vel
      real     z0_vel,dz_vel
 
      real     vcof(nz_vel,nx_vel,ny_vel,10)

      real     vfun(n_xyz)
      real     dvdx(n_xyz)
      real     dvdy(n_xyz)
      real     dvdz(n_xyz)
      real     dvdq11(n_xyz)
      real     dvdq12(n_xyz)
      real     dvdq22(n_xyz)

      integer  m_work
      real     work(m_work)
 
      integer  i_err
 
      integer  i_xyz
      real     v0,xc,yc,zc,vx,vy,vz

      integer  ix,iy,iz
      real     xf,yf,zf
      real     cq(3,2)   !   dx_i/dq_j
 
      real     v_xy,v_xy_eps
      data     v_xy_eps/1.e-6/
      real     v_true

      integer  i_call
      data     i_call/0/
      i_call = i_call + 1
 
c  compute v0, dvdx, dvdy, dvdz from velocity
 
      i_err = 0

      do i_xyz = 1 , n_xyz

        ix = max(1,min(nx_vel-1,int((x0(i_xyz)-x0_vel)/dx_vel)+2))

        iy = max(1,min(ny_vel-1,int((y0(i_xyz)-y0_vel)/dy_vel)+2))
 
        iz = max(1,min(nz_vel-1,int((z0(i_xyz)-z0_vel)/dz_vel)+2))
 
        xf = max(0.,min(1.
     1,(x0(i_xyz)-(ix-2)*dx_vel-x0_vel)/dx_vel))

        yf = max(0.,min(1.
     1,(y0(i_xyz)-(iy-2)*dy_vel-y0_vel)/dy_vel))
 
        zf = max(0.,min(1.
     1,(z0(i_xyz)-(iz-2)*dz_vel-z0_vel)/dz_vel))

        vfun(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,1)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,1)) 
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,1) 
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,1)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),1) 
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),1)) 
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),1) 
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),1)))

        v_true = 2000. +1.*z0(i_xyz)
c        if (x0(i_xyz) .eq. 850 .and. y0(i_xyz) .eq. 850 ) then
c        write(31,'(1x,f10.4,1x,f10.4,1x,f10.4,1x,f10.4)')
c     1 z0(i_xyz),vfun(i_xyz),v_true,vfun(i_xyz)-v_true

c    1,vcof(max(1,iz-1),ix,iy,1),vcof(iz,ix,iy,1)
c    1,vcof(iz,max(1,ix-1),iy,1),vcof(max(1,iz-1),max(1,ix-1),iy,1)
c    1,vcof(max(1,iz-1),ix,max(1,iy-1),1),vcof(iz,ix,max(1,iy-1),1)
c    1,vcof(iz,max(1,ix-1),max(1,iy-1),1)
c    1,vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),1)

c      write(31,'(''  '')') 

c      write(29,'(/,''z0='',f8.2,''ix='',i4,''iy'',i4,''iz='',i4)'
c      write(29,'(1x,f8.2,1x,i4,1x,i4,1x,i4)')
c    1 z0(i_xyz),ix,iy,iz

c       endif ! if (x0(i_xyz) .eq. 850 .and. y0(i_xyz) .eq. 850 ) then

 
        dvdx(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,2)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,2))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,2)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,2)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),2)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),2))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),2)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),2)))


        dvdy(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,3)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,3))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,3)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,3)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),3)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),3))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),3)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),3)))


        dvdz(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,4)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,4))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,4)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,4)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),4)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),4))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),4)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),4)))

c        v_xy = 1. / sqrt(px(i_xyz) * px(i_xyz) 
c     1 +py(i_xyz)*py(i_xyz))         ! alpha cann't be 0 or pi 
        v_xy = 1. / max(v_xy_eps,
     1sqrt(px(i_xyz)*px(i_xyz)+py(i_xyz)*py(i_xyz))) ! alpha cann't be 0 or pi 
        
        cq(1,1) = v_xy * py(i_xyz)
        cq(2,1) = -v_xy * px(i_xyz)
        cq(3,1) = 0
        cq(1,2) = vcof(iz,ix,iy,1) *pz(i_xyz) *v_xy *px(i_xyz)
        cq(2,2) = vcof(iz,ix,iy,1) *pz(i_xyz) *v_xy *py(i_xyz)
        cq(3,2) = -vcof(iz,ix,iy,1) / v_xy

        dvdq11(i_xyz) = vcof(iz,ix,iy,8)*cq(1,1)*cq(1,1)
     1+vcof(iz,ix,iy,9)*cq(2,1)*cq(2,1) 
     1+vcof(iz,ix,iy,10)*cq(3,1)*cq(3,1)
     1+ 2 * ( vcof(iz,ix,iy,5)*cq(1,1)*cq(2,1) 
     1+vcof(iz,ix,iy,6)*cq(1,1)*cq(3,1)
     1+vcof(iz,ix,iy,7)*cq(2,1)*cq(3,1)) 

        dvdq12(i_xyz) = vcof(iz,ix,iy,8)*cq(1,1)*cq(1,2)
     1+vcof(iz,ix,iy,9)*cq(2,1)*cq(2,2) 
     1+vcof(iz,ix,iy,10)*cq(3,1)*cq(3,2)     
     1+vcof(iz,ix,iy,5)* (cq(1,2)*cq(2,1) + cq(1,1)*cq(2,2))
     1+vcof(iz,ix,iy,6)* (cq(1,3)*cq(3,2) + cq(1,2)*cq(3,1))
     1+vcof(iz,ix,iy,7)* (cq(2,1)*cq(3,2) + cq(2,2)*cq(3,1))

     
        dvdq22(i_xyz) = vcof(iz,ix,iy,8)*cq(1,2)*cq(1,2)
     1+vcof(iz,ix,iy,9)*cq(2,2)*cq(2,2) 
     1+vcof(iz,ix,iy,10)*cq(3,2)*cq(3,2)     
     1+ 2 * ( vcof(iz,ix,iy,5)*cq(1,2)*cq(2,2) 
     1+vcof(iz,ix,iy,6)*cq(1,2)*cq(3,2)
     1+vcof(iz,ix,iy,7)*cq(2,2)*cq(3,2)) 
   
 
      enddo    ! do i_xyz = 1 , n_xyz
 
      return
 
  999 continue
      write(util_err(),'(/,'' error in util_3d_compute_v_grad'')')
      i_err = -1
      return
 
      end
 

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_3d_compute_v_grad_coef(
     1 nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vel,vcof)
c-----------------------------------------------------------------
c     Purpose: Compute the velocity gradient as a pre-process for 
c              linear interporlation.
c-----------------------------------------------------------------------
c
      implicit  none
   
      integer   util_prn,util_err,util_dbg,util_wln,util_test
      real     util_amax

      integer  nz_vel
      real     z0_vel,dz_vel

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  ny_vel
      real     y0_vel,dy_vel

      real     vel(nz_vel,nx_vel,ny_vel)
      real     vcof(nz_vel,nx_vel,ny_vel,10)

      integer  ix,ix_1,ix_2,ix_3
      real     dx

      integer  iy,iy_1,iy_2,iy_3
      real     dy

      integer  iz,iz_1,iz_2,iz_3
      real     dz

      if (util_dbg() .ge. 0) 
     1write(util_dbg(),'(/,'' util_3d_compute_v_grad_coef''
     1,/,'' computing velocity coefficients''
     1,/,'' nx_vel='',i8,'' x0_vel='',f10.2,'' dx_vel='',f10.2
     1,'' x1_vel='',f10.2
     1,/,'' ny_vel='',i8,'' y0_vel='',f10.2,'' dy_vel='',f10.2
     1,'' y1_vel='',f10.2
     1,/,'' nz_vel='',i8,'' z0_vel='',f10.2,'' dz_vel='',f10.2
     1,'' z1_vel='',f10.2
     1,/,'' v_max='',f10.2
     1)')
     1 nx_vel,x0_vel,dx_vel,(nx_vel-1)*dx_vel+x0_vel
     1,ny_vel,y0_vel,dy_vel,(ny_vel-1)*dy_vel+y0_vel
     1,nz_vel,z0_vel,dz_vel,(nz_vel-1)*dz_vel+z0_vel
     1,util_amax(nx_vel*ny_vel*nz_vel,vel)

      do iz = 1, nz_vel

        do ix = 1, nx_vel

           do iy = 1, ny_vel

             vcof(iz,ix,iy,1) = vel(iz,ix,iy)

c  dv / dx
             ix_1 = max(     1,ix-1)
             ix_2 = min(nx_vel,ix+1)
             dx = max(1,ix_2-ix_1) * dx_vel
             vcof(iz,ix,iy,2) = (vel(iz,ix_2,iy) - vel(iz,ix_1,iy)) / dx

c  dv / dy
             iy_1 = max(     1,iy-1)
             iy_2 = min(ny_vel,iy+1)
             dy = max(1,iy_2-iy_1) * dy_vel
             vcof(iz,ix,iy,3) = (vel(iz,ix,iy_2) - vel(iz,ix,iy_1)) / dy

c  dv / dz
             iz_1 = max(     1,iz-1)
             iz_2 = min(nz_vel,iz+1)
             dz = max(1,iz_2-iz_1) * dz_vel
             vcof(iz,ix,iy,4) = (vel(iz_2,ix,iy) - vel(iz_1,ix,iy)) / dz

c  d2v / dxdy
             vcof(iz,ix,iy,5) = (vel(iz,ix_2,iy_2) + vel(iz,ix_1,iy_1)
     1-vel(iz,ix_2,iy_1) - vel(iz,ix_1,iy_2) ) / dx /dy

c  d2v / dxdz
             vcof(iz,ix,iy,6) = (vel(iz_2,ix_2,iy) + vel(iz_1,ix_1,iy)
     1-vel(iz_1,ix_2,iy) - vel(iz_2,ix_1,iy) ) / dx /dz

c  d2v / dzdy
             vcof(iz,ix,iy,7) = (vel(iz_2,ix,iy_2) + vel(iz_1,ix,iy_1)
     1-vel(iz_2,ix,iy_1) - vel(iz_1,ix,iy_2) ) / dz /dy

c  d2v / dx2
             ix_1 = min(nx_vel-2,  max(     1,ix-1))
             ix_2 = ix_1 + 1
             ix_3 = ix_1 + 2
             vcof(iz,ix,iy,8) = (vel(iz,ix_3,iy) - 2 *vel(iz,ix_2,iy)
     1+ vel(iz,ix_1,iy)) / dx_vel /dx_vel

c  d2v / dy2
             iy_1 = min(ny_vel-2,  max(     1,iy-1))
             iy_2 = iy_1 + 1
             iy_3 = iy_1 + 2
             vcof(iz,ix,iy,9) = (vel(iz,ix,iy_3) - 2 *vel(iz,ix,iy_2)
     1+ vel(iz,ix,iy_1)) / dy_vel /dy_vel

c  d2v / dz2
             iz_1 = min(nz_vel-2,  max(     1,iz-1))
             iz_2 = iz_1 + 1
             iz_3 = iz_1 + 2
             vcof(iz,ix,iy,10) = (vel(iz_3,ix,iy) - 2 *vel(iz_2,ix,iy)
     1+ vel(iz_1,ix,iy)) / dz_vel /dz_vel
 
           enddo ! do iy = 1, ny_vel

        enddo !do ix = 1, nx_vel

      enddo !do iz = 1, nz_vel

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_transform_1d(nx,x0,dx
     1,x_inp_1,x_inp_2,x_out_1,x_out_2)
      implicit  none
      integer  nx
      real     x0,dx
      real     x_inp_1,x_inp_2
      real     x_out_1,x_out_2

      real     x1,x_temp,dx_0

      if (  x_inp_1 .eq. x_inp_2 .or.  x_out_1 .eq. x_out_2
     1.or. (x_inp_1 .eq. x_out_1 .and. x_inp_2 .eq. x_out_2)
     1) return

      dx_0 = dx
      x1 = (nx - 1) * dx + x0

      call util_transform(1,x0
     1,x_inp_1,x_inp_2,x_out_1,x_out_2)

      call util_transform(1,x1
     1,x_inp_1,x_inp_2,x_out_1,x_out_2)

      if (x1 .lt. x0 .and. dx .ge. 0.) then
        x_temp = x1
        x1 = x0
        x0 = x_temp
      endif    ! if (x1 .lt. x0 .and. dx .ge. 0.) then
      dx = (x1 - x0) / max(1,nx-1)
      if (dx .eq. 0. .and. dx_0 .ne. 0.) dx = dx_0

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_transform_2d(
     1 nx_vel,x0_vel,dx_vel
     1,nz_vel,z0_vel,dz_vel
     1,x_inp_1,x_inp_2
     1,x_out_1,x_out_2
     1,z_inp_1,z_inp_2
     1,z_out_1,z_out_2
     1)
      implicit  none

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      real     x_inp_1,x_inp_2
      real     x_out_1,x_out_2

      real     z_inp_1,z_inp_2
      real     z_out_1,z_out_2

c  transform from input to output coordinates
      call util_transform_1d(nx_vel,x0_vel,dx_vel
     1,x_inp_1,x_inp_2,x_out_1,x_out_2)

      call util_transform_1d(nz_vel,z0_vel,dz_vel
     1,z_inp_1,z_inp_2,z_out_1,z_out_2)

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_transform_3d(
     1 nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,x_inp_1,x_inp_2
     1,x_out_1,x_out_2
     1,y_inp_1,y_inp_2
     1,y_out_1,y_out_2
     1,z_inp_1,z_inp_2
     1,z_out_1,z_out_2
     1)
      implicit  none

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      real     x_inp_1,x_inp_2
      real     x_out_1,x_out_2

      real     y_inp_1,y_inp_2
      real     y_out_1,y_out_2

      real     z_inp_1,z_inp_2
      real     z_out_1,z_out_2

c  transform from input to output coordinates
      call util_transform_1d(nx_vel,x0_vel,dx_vel
     1,x_inp_1,x_inp_2,x_out_1,x_out_2)

      call util_transform_1d(ny_vel,y0_vel,dy_vel
     1,y_inp_1,y_inp_2,y_out_1,y_out_2)

      call util_transform_1d(nz_vel,z0_vel,dz_vel
     1,z_inp_1,z_inp_2,z_out_1,z_out_2)

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_read_transform(h_file
     1,x_inp_1,x_inp_2
     1,x_out_1,x_out_2
     1,y_inp_1,y_inp_2
     1,y_out_1,y_out_2
     1,z_inp_1,z_inp_2
     1,z_out_1,z_out_2
     1,i_err)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      real     util_invert_1
      integer  util_len_r
      integer  util_fetch_i
      integer  util_fetch_r
      integer  util_fetch_c

      character h_file*(*)

      real     x_inp_1,x_inp_2
      real     x_out_1,x_out_2

      real     y_inp_1,y_inp_2
      real     y_out_1,y_out_2

      real     z_inp_1,z_inp_2
      real     z_out_1,z_out_2

      real     v_min,v_max

      integer   i_err

      integer   i_inp_file,i_h_file
      character x_coord*16,y_coord*16,z_coord*16

      i_err = 0

c  get the current util decode unit number
      call util_get_device(i_inp_file)

      if (h_file .ne. 'NONE') then

c  open the velocity header file
        call util_open_file(i_h_file,h_file,'old','formatted',0,i_err)
        if (i_err .ne. 0) goto 998

      else    ! if (h_file .ne. 'NONE') then

        i_h_file = i_inp_file

      endif    ! if (h_file .ne. 'NONE') then

c set the logical unit number for read input cards
      call util_put_device(i_h_file)

c  get the transformation information
      if (util_fetch_c('xcoordinate',x_coord) .eq. 0) 
     1 x_coord = 'XBASEMENT'

      if (util_fetch_c('ycoordinate',y_coord) .eq. 0) 
     1 y_coord = 'YBASEMENT'

c23456789012345678901234567890123456789012345678901234567890123456789012
      if (util_fetch_c('zcoordinate',z_coord) .eq. 0) z_coord = 'DEPTH'

      if (x_coord(1:5) .eq. 'XGRID') then

        if (util_fetch_r('XGRID1',x_inp_1) .eq. 0) x_inp_1 = 0.
        if (util_fetch_r('XGRID2',x_inp_2) .eq. 0) x_inp_2 = 1.

      elseif (x_coord(1:11) .eq. 'XANNOTATION') then

        if (util_fetch_r('XANNOTATION1',x_inp_1) .eq. 0) x_inp_1 = 0.
        if (util_fetch_r('XANNOTATION2',x_inp_2) .eq. 0) x_inp_2 = 1.

      else

        if (util_fetch_r('XBASEMENT1',x_inp_1) .eq. 0) x_inp_1 = 0.
        if (util_fetch_r('XBASEMENT2',x_inp_2) .eq. 0) x_inp_2 = 1.

      endif    ! if (x_coord(1:5) .eq. 'XGRID') then

      if (util_fetch_r('XBASEMENT1',x_out_1) .eq. 0) x_out_1 = 0.
      if (util_fetch_r('XBASEMENT2',x_out_2) .eq. 0) x_out_2 = 1.

      if (y_coord(1:5) .eq. 'YGRID') then

        if (util_fetch_r('YGRID1',y_inp_1) .eq. 0) y_inp_1 = 0.
        if (util_fetch_r('YGRID2',y_inp_2) .eq. 0) y_inp_2 = 1.

      elseif (y_coord(1:11) .eq. 'YANNOTATION') then

        if (util_fetch_r('YANNOTATION1',y_inp_1) .eq. 0) y_inp_1 = 0.
        if (util_fetch_r('YANNOTATION2',y_inp_2) .eq. 0) y_inp_2 = 1.

      else

        if (util_fetch_r('YBASEMENT1',y_inp_1) .eq. 0) y_inp_1 = 0.
        if (util_fetch_r('YBASEMENT2',y_inp_2) .eq. 0) y_inp_2 = 1.

      endif    ! if (y_coord(1:5) .eq. 'YGRID') then

      if (util_fetch_r('YBASEMENT1',y_out_1) .eq. 0) y_out_1 = 0.
      if (util_fetch_r('YBASEMENT2',y_out_2) .eq. 0) y_out_2 = 1.


      if (z_coord(1:4) .eq. 'FEET') then

        if (util_fetch_r('FEET1',z_inp_1) .eq. 0) z_inp_1 = 0.
        if (util_fetch_r('FEET1',z_inp_2) .eq. 0) z_inp_2 = 1.

      elseif (z_coord(1:8) .eq. 'KILOFEET') then

        if (util_fetch_r('KILOFEET1',z_inp_1) .eq. 0) z_inp_1 = 0.
        if (util_fetch_r('KILOFEET2',z_inp_2) .eq. 0) z_inp_2 = 1.

      elseif (z_coord(1:5) .eq. 'METER') then

        if (util_fetch_r('METER1',z_inp_1) .eq. 0) z_inp_1 = 0.
        if (util_fetch_r('METER2',z_inp_2) .eq. 0) z_inp_2 = 1.

      elseif (z_coord(1:6) .eq. 'METERS') then

        if (util_fetch_r('METERS1',z_inp_1) .eq. 0) z_inp_1 = 0.
        if (util_fetch_r('METERS2',z_inp_2) .eq. 0) z_inp_2 = 1.

      elseif (z_coord(1:9) .eq. 'KILMETER') then

        if (util_fetch_r('KILOMETER1',z_inp_1) .eq. 0) z_inp_1 = 0.
        if (util_fetch_r('KILOMETER2',z_inp_2) .eq. 0) z_inp_2 = 1.

      elseif (z_coord(1:10) .eq. 'KILOMETERS') then

        if (util_fetch_r('KILOMETERS1',z_inp_1) .eq. 0) z_inp_1 = 0.
        if (util_fetch_r('KILOMETERS2',z_inp_2) .eq. 0) z_inp_2 = 1.

      else

        if (util_fetch_r('DEPTH1',z_inp_1) .eq. 0) z_inp_1 = 0.
        if (util_fetch_r('DEPTH2',z_inp_2) .eq. 0) z_inp_2 = 1.

      endif    ! if (z_coord(1:4) .eq. 'FEET') then

      if (util_fetch_r('DEPTH1',z_out_1) .eq. 0) z_out_1 = 0.
      if (util_fetch_r('DEPTH2',z_out_2) .eq. 0) z_out_2 = 1.

c  close the velocity header file
      call util_close_file(i_h_file)

c  replace the original util decode unit number
      call util_put_device(i_inp_file)

      if (util_dbg() .ge. 0) write(util_dbg()
     1,'(/,''  util_read_transform input parameters''
     1,/,'' h_file ='',a
     1,/,'' x_coord='',a
     1,/,'' y_coord='',a
     1,/,'' z_coord='',a
     1,/,'' x_inp_1='',f10.2,'' x_inp_2='',f10.2
     1,/,'' x_out_1='',f10.2,'' x_out_2='',f10.2
     1,/,'' y_inp_1='',f10.2,'' y_inp_2='',f10.2
     1,/,'' y_out_1='',f10.2,'' y_out_2='',f10.2
     1,/,'' z_inp_1='',f10.2,'' z_inp_2='',f10.2
     1,/,'' z_out_1='',f10.2,'' z_out_2='',f10.2
     1)')
     1 h_file(1:util_len_r(h_file))
     1,x_coord(1:util_len_r(x_coord))
     1,y_coord(1:util_len_r(y_coord))
     1,z_coord(1:util_len_r(z_coord))
     1,x_inp_1,x_inp_2
     1,x_out_1,x_out_2
     1,y_inp_1,y_inp_2
     1,y_out_1,y_out_2
     1,z_inp_1,z_inp_2
     1,z_out_1,z_out_2

      return

  995 continue
      write(util_err(),'(/,'' util_read_transform error ''
     1,'' in reading velocity header file file'',a)')
     1 h_file(1:util_len_r(h_file))
      goto 999

  998 continue
      write(util_err(),'(/,'' util_read_transform error opening ''
     1,'' velocity header file'',a)')
     1 h_file(1:util_len_r(h_file))
      goto 999

  999 continue
      write(util_err()
     1,'(/,'' util_read_transform error file'',a)')h_file
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_read_velocity(h_file
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,m_vel,vel
     1,i_err)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      real     util_invert_1
      integer  util_len_r
      integer  util_fetch_i
      integer  util_fetch_r
      integer  util_fetch_c

      character h_file*(*)
      character d_file*64

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      integer   m_vel
      real      vel(m_vel)

      real     v_min,v_max

      integer   i_err

      integer   i_inp_file,i_h_file
      integer   ix_column,iy_column,iz_column,iv_column 
      data      ix_column,iy_column,iz_column,iv_column /1,2,3,4/

      i_err = 0

c  get the current util decode unit number
      call util_get_device(i_inp_file)

c  open the velocity header file
      call util_open_file(i_h_file,h_file,'old','formatted',0,i_err)
      if (i_err .ne. 0) goto 998

c set the logical unit number for read input cards
      call util_put_device(i_h_file)

c  get the input information
      if (util_fetch_c('file',d_file) .eq. 0) 
     1 call util_error('file must be defined',i_err)

      if (util_fetch_i('nx',nx_vel) .eq. 0)
     1 call util_error('nx must be defined',i_err)
      if (util_fetch_r('xmin',x0_vel) .eq. 0) 
     1 call util_error('xmin must be defined',i_err)
      if (util_fetch_r('xinc',dx_vel) .eq. 0)
     1 call util_error('xinc must be defined',i_err)

      if (util_fetch_i('ny',ny_vel) .eq. 0)
     1 call util_error('ny must be defined',i_err)
      if (util_fetch_r('ymin',y0_vel) .eq. 0) 
     1 call util_error('ymin must be defined',i_err)
      if (util_fetch_r('yinc',dy_vel) .eq. 0)
     1 call util_error('yinc must be defined',i_err)

      if (util_fetch_i('nz',nz_vel) .eq. 0)
     1 call util_error('nz must be defined',i_err)
      if (util_fetch_r('zmin',z0_vel) .eq. 0) 
     1 call util_error('zmin must be defined',i_err)
      if (util_fetch_r('zinc',dz_vel) .eq. 0)
     1 call util_error('zinc must be defined',i_err)

      if (i_err .ne. 0) goto 997

c  close the velocity header file
      call util_close_file(i_h_file)

c  replace the original util decode unit number
      call util_put_device(i_inp_file)

      if (util_dbg() .ge. 0) write(util_dbg()
     1,'(/,''  util_read_velocity input parameters''
     1,/,'' h_file='',a
     1,/,'' d_file='',a
     1,/,'' nx_vel='',i8,'' x0_vel='',f10.2,'' dx_vel='',f10.2
     1,'' x1_vel='',f10.2
     1,/,'' ny_vel='',i8,'' y0_vel='',f10.2,'' dy_vel='',f10.2
     1,'' y1_vel='',f10.2
     1,/,'' nz_vel='',i8,'' z0_vel='',f10.2,'' dz_vel='',f10.2
     1,'' z1_vel='',f10.2
     1)')
     1 h_file(1:util_len_r(h_file))
     1,d_file(1:util_len_r(d_file))
     1,nx_vel,x0_vel,dx_vel,(nx_vel-1)*dx_vel+x0_vel
     1,ny_vel,y0_vel,dy_vel,(ny_vel-1)*dy_vel+y0_vel
     1,nz_vel,z0_vel,dz_vel,(nz_vel-1)*dz_vel+z0_vel

c  make sure we have enough memory for the vleocity model
      if (m_vel .lt. nx_vel*ny_vel*nz_vel) goto 996

c  read the velocity grid
      call util_read_velocity_0(d_file
     1,ix_column,iy_column,iz_column,iv_column 
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1,i_err)
      if (i_err .ne. 0) goto 995

      call util_min_max(v_min,v_max,nx_vel*ny_vel*nz_vel,vel)

      if (util_dbg() .ge. 0) write(util_dbg(),'(
     1   '' v_min='',f10.2,'' v_max='',f10.2
     1,/,'' v1   ='',f10.2,'' vn   ='',f10.2
     1)')
     1 util_invert_1(v_max)
     1,util_invert_1(v_min)
     1,util_invert_1(vel(1))
     1,util_invert_1(vel(nx_vel*ny_vel*nz_vel))

      return

  995 continue
      write(util_err(),'(/,'' util_read_velocity error ''
     1,'' in reading velocity header file file'',a)')
     1 h_file(1:util_len_r(h_file))
      goto 999

  996 continue
      write(util_err(),'(/,'' error in util_read_velocity ''
     1,/,'' need more memory for velocity''
     1,/,'' have='',i8,'' need='',i8
     1,/,'' nx_vel='',i8,'' ny_vel='',i8,'' nz_vel='',i8)')
     1 m_vel,nx_vel*ny_vel*nz_vel,nx_vel,ny_vel,nz_vel
      goto 999

  997 continue
      write(util_err(),'(/,'' util_read_velocity error reading ''
     1,'' velocity data file'',a)')
     1 d_file(1:util_len_r(d_file))
      goto 999

  998 continue
      write(util_err(),'(/,'' util_read_velocity error opening ''
     1,'' velocity header file'',a)')
     1 h_file(1:util_len_r(h_file))
      goto 999

  999 continue
      write(util_err()
     1,'(/,'' util_read_velocity error file'',a)')h_file
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_read_velocity_0(d_file
     1,ix_column,iy_column,iz_column,iv_column 
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1,i_err)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer   util_len_r

      character d_file*(*)
      integer   ix_column,iy_column,iz_column,iv_column 

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      integer   m_vel
      real      vel(nz_vel,nx_vel,ny_vel)

      integer   i_err

      integer   i_file,n_column
      integer   m_xyz,n_xyz,l_xyz
      integer   ix_vel,iy_vel,iz_vel,j
      real      x_vel,y_vel,z_vel,v_vel
      real      v_min,v_max
      integer   m_column
      parameter (m_column=20)
      real      data(m_column)

      i_err = 0
c  open the file
      call util_open_file(i_file,d_file,'old','formatted',0,i_err)
      if (i_err .ne. 0) goto 996

      if (util_dbg() .ge. 0) write(util_dbg()
     1,'(/,'' reading the velocity model'',/,'' file='',a)')
     1 d_file(1:util_len_r(d_file))

      n_column = min(m_column
     1,max(ix_column,iy_column,iz_column,iv_column))
      m_xyz = nx_vel * ny_vel * nz_vel
      n_xyz = 0
      l_xyz = 0
      call util_setr(m_xyz,vel,-999.)

    1 continue
        call util_setr(n_column,data,0.)
        read(i_file,*,err=997,end=2)(data(j),j=1,n_column)
        x_vel = data(ix_column)
        y_vel = data(iy_column)
        z_vel = data(iz_column)
        v_vel = data(iv_column)
        ix_vel = nint((x_vel - x0_vel) / dx_vel) + 1
        iy_vel = nint((y_vel - y0_vel) / dy_vel) + 1
        iz_vel = nint((z_vel - z0_vel) / dz_vel) + 1
        l_xyz  = l_xyz + 1

        if (ix_vel .ge. 1 .and. ix_vel .le. nx_vel
     1.and. iy_vel .ge. 1 .and. iy_vel .le. ny_vel
     1.and. iz_vel .ge. 1 .and. iz_vel .le. nz_vel) then

          if (vel(iz_vel,ix_vel,iy_vel) .le. 0.) then
            n_xyz = n_xyz + 1
            vel(iz_vel,ix_vel,iy_vel) = v_vel
          endif    ! if (vel(iz_vel,ix_vel,iy_vel) .le. 0.) then

        endif    ! if (ix_vel .ge. 1 .and. ix_vel .le. nx_vel

        goto 1

    2 continue

      call util_close_file(i_file)

      call util_min_max(v_min,v_max,m_xyz,vel)

      if (util_dbg() .ge. 0) write(util_dbg()
     1,'(/,'' util_read_velocity velocity values file'',a
     1,/,'' number of points in grid  ='',i10
     1,/,'' number of points read     ='',i10
     1,/,'' number of points filled in='',i10
     1,/,'' v_min='',f10.2,'' v_max='',f10.2)')
     1 d_file(1:util_len_r(d_file)),m_xyz,n_xyz,l_xyz,v_min,v_max

      if (m_xyz .ne. n_xyz .or. m_xyz .ne. l_xyz 
     1.or. v_min .lt. 0. .or. v_max .lt. 0.) goto 998

c  convert from velocity to slowness
      call util_invert(m_xyz,vel)

      return

  996 continue
      write(util_err()
     1,'(/,'' util_read_velocity error opening file'',a)')
     1 d_file(1:util_len_r(d_file))
      goto 999

  997 continue
      write(util_err()
     1,'(/,'' util_read_velocity error reading file'',a)')
     1 d_file(1:util_len_r(d_file))
      goto 999

  998 continue
      write(util_err()
     1,'(/,'' util_read_velocity error in velocity values''
     1,'' file'',a
     1,/,'' m_xyz='',i10,'' n_xyz='',i10,'' l_xyz='',i10
     1,/,'' v_min='',f10.2,'' v_max='',f10.2)')
     1 d_file(1:util_len_r(d_file)),m_xyz,n_xyz,l_xyz,v_min,v_max
      goto 999

  999 continue
      write(util_err(),'(/,'' util_read_velocity error file'',a)')
     1 d_file
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_compute_linear_velocity(
     1 v0,x0,y0,z0,vx,vy,vz
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1)
c  compute a linear velocity model from v0,x0,y0,z0,vx,vy,vz
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      real      util_invert_1

      real      v0,x0,y0,z0,vx,vy,vz

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      real     vel(nz_vel,nx_vel,ny_vel)

      integer   ix_vel,iy_vel,iz_vel
      real      x_vel,y_vel,z_vel
      real      v_min,v_max

      v_min = v0 * .5

      do iy_vel = 1 , ny_vel

        do ix_vel = 1 , nx_vel

          do iz_vel = 1 , nz_vel

            x_vel = (ix_vel - 1) * dx_vel + x0_vel
            y_vel = (iy_vel - 1) * dy_vel + y0_vel
            z_vel = (iz_vel - 1) * dz_vel + z0_vel
            vel(iz_vel,ix_vel,iy_vel) = max(v_min,
     1                                  v0 
     1                                + vx * (x_vel - x0)
     1                                + vy * (y_vel - y0)
     1                                + vz * (z_vel - z0)
     1)

          enddo    ! do iz_vel = 1 , nz_vel

        enddo    ! do ix_vel = 1 , nx_vel

      enddo    ! do iy_vel = 1 , ny_vel

c  convert from velocity to slowness
      call util_invert(nx_vel*ny_vel*nz_vel,vel)

      if (util_dbg() .ge. 0) write(util_dbg()
     1,'(/,''  util_compute_linear_velocity input parameters''
     1,/,'' v0='',f10.2
     1,/,'' x0='',f10.2,'' vx='',f10.4
     1,/,'' y0='',f10.2,'' vy='',f10.4
     1,/,'' z0='',f10.2,'' vz='',f10.4
     1,/,'' nx_vel='',i8,'' x0_vel='',f10.2,'' dx_vel='',f10.2
     1,'' x1_vel='',f10.2
     1,/,'' ny_vel='',i8,'' y0_vel='',f10.2,'' dy_vel='',f10.2
     1,'' y1_vel='',f10.2
     1,/,'' nz_vel='',i8,'' z0_vel='',f10.2,'' dz_vel='',f10.2
     1,'' z1_vel='',f10.2
     1)')
     1 v0
     1,x0,vx
     1,y0,vy
     1,z0,vz
     1,nx_vel,x0_vel,dx_vel,(nx_vel-1)*dx_vel+x0_vel
     1,ny_vel,y0_vel,dy_vel,(ny_vel-1)*dy_vel+y0_vel
     1,nz_vel,z0_vel,dz_vel,(nz_vel-1)*dz_vel+z0_vel

      call util_min_max(v_min,v_max,nx_vel*ny_vel*nz_vel,vel)

      if (util_dbg() .ge. 0) write(util_dbg(),'(
     1   '' v_min='',f10.2,'' v_max='',f10.2
     1,/,'' v1   ='',f10.2,'' vn   ='',f10.2
     1)')
     1 util_invert_1(v_max)
     1,util_invert_1(v_min)
     1,util_invert_1(vel(1,1,1))
     1,util_invert_1(vel(nz_vel,nx_vel,ny_vel))

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_get_i(c_inp,x_inp)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer   util_len_r

      character c_inp*(*)
      integer   x_inp

      integer   m_str
      parameter (m_str=100)
      integer   l_str(m_str)
      integer   x_str(m_str)
      character c_str(m_str)*16
      data      l_str/m_str*0/
      data      x_str/m_str*0/
      data      c_str/m_str*'                '/

      character a_inp*16
      integer   l_inp,i_str

      call util_caps(c_inp,a_inp)
      l_inp = util_len_r(a_inp)

      do i_str = 1 , m_str

        if (l_str(i_str) .gt. 0) then
        if (c_str(i_str)(1:l_str(i_str)) .eq. a_inp(1:l_inp)) then

          x_inp = x_str(i_str)
c      if (util_dbg() .ge. 0) write(util_dbg()
c     1,'('' get_i i_str='',i5,'' c_str='',a16,'' x_inp='',i8)')
c     1 i_str,c_str(i_str),x_inp
          goto 1

        endif    ! if (c_str(i_str)(1:l_str(i_str)) .eq. a_inp(1:l_inp)) then
        endif    ! if (l_str(i_str) .gt. 0) then

      enddo    ! do i_str = 1 , m_str

    1 continue
c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' get_i c_inp='',a16,'' x_inp='',i8)')
c     1 c_inp,x_inp

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_put_i(c_inp,x_inp)

      call util_caps(c_inp,a_inp)
      do i_str = 1 , m_str

        if (l_str(i_str) .eq. 0) then

          c_str(i_str) = a_inp
          x_str(i_str) = x_inp
          l_str(i_str) = util_len_r(c_str(i_str))
c      if (util_dbg() .ge. 0) write(util_dbg()
c     1,'('' put_i i_str='',i5,'' c_str='',a16,'' x_inp='',i8)')
c     1 i_str,c_str(i_str),x_inp
          goto 2

        endif    ! if (l_str(i_str) .eq. 0) then

      enddo    ! do i_str = 1 , m_str

    2 continue
c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' put_i c_inp='',a16,'' x_inp='',i8)')
c     1 c_inp,x_inp

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_clear_i(c_inp)

      call util_caps(c_inp,a_inp)
      l_inp = util_len_r(a_inp)

      do i_str = 1 , m_str

        if (l_str(i_str) .gt. 0) then
        if (c_str(i_str)(1:l_str(i_str)) .eq. a_inp(1:l_inp)) then

          l_str(i_str) = 0
          c_str(i_str) = ' '
          goto 3

        endif    ! if (c_str(i_str)(1:l_str(i_str)) .eq. a_inp(1:l_inp)) then
        endif    ! if (l_str(i_str) .gt. 0) then

      enddo    ! do i_str = 1 , m_str

    3 continue

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_get_r(c_inp,x_inp)
      implicit  none

      integer   util_len_r

      character c_inp*(*)
      real      x_inp

      integer   m_str
      parameter (m_str=100)
      integer   l_str(m_str)
      real      x_str(m_str)
      character c_str(m_str)*16
      character a_inp*16
      data      l_str/m_str*0/
      data      x_str/m_str*0./
      data      c_str/m_str*'                '/

      integer   l_inp,i_str

      call util_caps(c_inp,a_inp)
      l_inp = util_len_r(a_inp)

      do i_str = 1 , m_str

        if (c_str(i_str)(1:l_str(i_str)) .eq. a_inp(1:l_inp)) then

          x_inp = x_str(i_str)
          goto 1

        endif    ! if (c_str(i_str)(1:l_str(i_str)) .eq. a_inp(1:l_inp)) then

      enddo    ! do i_str = 1 , m_str

    1 continue

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_put_r(c_inp,x_inp)

      call util_caps(c_inp,a_inp)

      do i_str = 1 , m_str

        if (l_str(i_str) .eq. 0) then

          c_str(i_str) = a_inp
          x_str(i_str) = x_inp
          l_str(i_str) = util_len_r(c_str(i_str))
          goto 2

        endif    ! if (l_str(i_str) .eq. 0) then

      enddo    ! do i_str = 1 , m_str

    2 continue

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_clear_r(c_inp)

      call util_caps(c_inp,a_inp)
      l_inp = util_len_r(a_inp)

      do i_str = 1 , m_str

        if (c_str(i_str)(1:l_str(i_str)) .eq. a_inp(1:l_inp)) then

          l_str(i_str) = 0
          c_str(i_str) = ' '
          goto 3

        endif    ! if (c_str(i_str)(1:l_str(i_str)) .eq. a_inp(1:l_inp)) then

      enddo    ! do i_str = 1 , m_str

    3 continue

      return

      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_get_c(c_inp,x_inp)
      implicit  none

      integer   util_len_r

      character c_inp*(*)
      character x_inp*(*)

      integer   m_str
      parameter (m_str=100)
      integer   l_str(m_str)
      character x_str(m_str)*16
      character c_str(m_str)*16
      character a_inp*16
      data      l_str/m_str*0/
      data      x_str/m_str*'                '/
      data      c_str/m_str*'                '/

      integer   l_inp,i_str

      call util_caps(c_inp,a_inp)
      l_inp = util_len_r(a_inp)

      do i_str = 1 , m_str

        if (c_str(i_str)(1:l_str(i_str)) .eq. a_inp(1:l_inp)) then

          x_inp = x_str(i_str)
          goto 1

        endif    ! if (c_str(i_str)(1:l_str(i_str)) .eq. a_inp(1:l_inp)) then

      enddo    ! do i_str = 1 , m_str

    1 continue

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_put_c(c_inp,x_inp)

      call util_caps(c_inp,a_inp)

      do i_str = 1 , m_str

        if (l_str(i_str) .eq. 0) then

          c_str(i_str) = a_inp
          x_str(i_str) = x_inp
          l_str(i_str) = util_len_r(c_str(i_str))
          goto 2

        endif    ! if (l_str(i_str) .eq. 0) then

      enddo    ! do i_str = 1 , m_str

    2 continue

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_clear_c(c_inp)

      call util_caps(c_inp,a_inp)
      l_inp = util_len_r(a_inp)

      do i_str = 1 , m_str

        if (c_str(i_str)(1:l_str(i_str)) .eq. a_inp(1:l_inp)) then

          l_str(i_str) = 0
          c_str(i_str) = ' '
          goto 3

        endif    ! if (c_str(i_str)(1:l_str(i_str)) .eq. a_inp(1:l_inp)) then

      enddo    ! do i_str = 1 , m_str

    3 continue

      return

      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_pow2(n)
c  next greater power of 2
      implicit  none
      integer n
      util_pow2 = 2**int(.99999999+log(real(n))/log(2.))
      if (n*2 .le. util_pow2) util_pow2 = util_pow2 / 2
      return
      end


C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_round(n,m)
c  round n up to m vlaues
      implicit  none
      integer n,m
      util_round = m * ( (n + m - 1) / m) 
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_rinw(n,m)
c  number of records length n in m words
      implicit  none
      integer n,m
      util_rinw = (n - 1) / m + 1
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_winw(n)
c round n up to the nearest block of words
      implicit  none
      integer util_rinw,n,nwpb
      data nwpb/512/
      util_winw = util_rinw(n,nwpb) * nwpb
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_winb(n)
c round n up to the nearest block of words
      implicit  none
      integer n,nwpb
      data nwpb/512/
      util_winb = n * nwpb
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_binw(n)
c determine the number of word blocks recquired to hold n words
      implicit  none
      integer util_winw
      integer n,nwpb
      data nwpb/512/
      util_binw = util_winw(n) / nwpb
      if (n .le. 0) util_binw = 0
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_set_global(
     1 nt_inp,t0_inp,dt_inp
     1,nt_out,t0_out,dt_out
     1,scale)
c  set globals from depth values
      implicit  none

      integer nt_inp
      real    t0_inp,dt_inp

      integer nt_out
      real    t0_out,dt_out

      real    scale

      nt_out = nt_inp
      t0_out = t0_inp * scale
      dt_out = dt_inp * scale

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_h_save(i_save,ic1,nh_inp,h_inp,h_save)
c  save a copy of header words
      implicit  none
      integer   util_len_r
      integer   i_save,nh_inp,i,j,lu,ic1,ic2,lt,i1,i2,i3,i4
      integer   nchars,jc1,jc2
      real      h_inp(nh_inp),h_save(nh_inp,12)
      character title*(*)
      character c_date*16,c_time*8
      character crd8*8,crd80*80

c  location 1 is first trace
      i1 = ic1 + 0
      i2 = ic1 + 1
      i3 = ic1 + 2
      i4 = ic1 + 3
      if (i_save .eq. 1) call util_copy(nh_inp,h_inp,h_save(1,i1))   ! first trace
                         call util_copy(nh_inp,h_inp,h_save(1,i2))   ! last  trace
      if (i_save .eq. 1) call util_copy(nh_inp,h_inp,h_save(1,i3))   ! min
      if (i_save .eq. 1) call util_copy(nh_inp,h_inp,h_save(1,i4))   ! max

      do i = 1 , nh_inp
        h_save(i,i3) = min(h_save(i,i3),h_inp(i))
        h_save(i,i4) = max(h_save(i,i4),h_inp(i))
      enddo

      return

      entry util_h_save_p(title,lu,ic1,nh_inp,h_save)

c  print h_save data
c     call get_idate1 (c_date)   ! current date
      c_date(11:)=' '
C     call clock  (c_time)   ! current time
      lt = util_len_r(title)
      if (ic1 .eq. 1) then

        ic2 = 8
        write(lu,'(/,'' input trace header values for '',a
     1,/,'' date='',a8,'' time='',a8
     1,/,'' header first    last      min     max      ''
     1,''first    last     min      max''
     1,/,'' word   input    input    input    input    ''
     1,''saved    saved    saved    saved'')')
     1title(1:lt),c_date,c_time

      else    ! if (ic1 .eq. 1) then

        ic2 = 12
        write(lu,'(/,'' output trace header values for '',a
     1,/,'' date='',a8,'' time='',a8
     1,/,'' header first    last     min      max''
     1,/,'' word   output   output   output   output'')')
     1title(1:lt),c_date,c_time

      endif    ! if (ic1 .eq. 1) then

      do i = 1 , nh_inp

        crd80 = ' '

        do j = ic1 , ic2

          call util_ntoa_r(h_save(i,j),crd8,nchars)
          jc1 = (j - ic1) * 9 + 4
          jc2 = jc1 + 7
          write(crd80(jc1:jc2),'(a8)')crd8

        enddo    ! do j = ic1 , ic2

        write(lu,'(1x,i2,1x,a)')i,crd80

      enddo    ! do i = 1 , nh_inp

      write(lu,'('' '')')

      return

      end
        
c      subroutine util_ntoa_r
C***************************** COPYRIGHT NOTICE ********************************
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
C\USER DOC
C-----------------------------------------------------------------------
C                         CRAY PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C  Process name:  util_ntoa_r
C        Author:  Bob Baumel
C  Last revised:  93/03/30  Dean Peterson
C
C  Purpose:  To produce an alphanumeric field representing a given
C            (real or integer) numeric value.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C                           ----------------
C                   This routine has two entry points;
C          util_ntoa_r for real values, util_ntoa_i for integer values:
C
C                CALL UTIL_NTOA_R ( VALUE,FIELD,NCHARS)
C                CALL UTIL_NTOA_I (IVALUE,FIELD,NCHARS)
C
C          VALUE  is the real value input to entry util_ntoa_r.
C          IVALUE is the integer value input to entry util_ntoa_i.
C          FIELD  is the alphanumeric data returned by the routine.
C          *ERR   is an alternate return in case of error.
C          NCHARS is the number of non-blank characters returned
C                    in FIELD (value returned by the subroutine).
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. This routine can be used on both the Cray and Vax.
C
C 2. The FIELD variable must be of type CHARACTER in the routine that
C    calls util_ntoa_r or util_ntoa_i.
C
C 3. The routine tries to make optimal use of the available field width
C    without displaying more significant figures than are consistent
C    with machine precision.  Some particular features:
C
C    a) The result is always left-justified and filled with blanks on
C       the right.
C
C    b) The routine tries hard to avoid E format (for real values).
C       It resorts to E format only if it cannot fit the value into
C       the available field width (with at least 2 significant digits)
C       WITHOUT using E format.
C                                                                      
C    c) In accord with proper number-writing practice, the routine     
C       does display one zero before the decimal point when the number
C       has absolute value smaller than one.
C
C    d) Trailing zeros (after the decimal point) are NOT displayed.
C
C    e) If a real value evaluates to an integer (to the precision of
C       the display), it is displayed as an integer; i.e. without a
C       decimal point.
C
C    f) If the number is an exact machine zero (real or integer), it
C       is displayed as a single digit '0' at the left-hand edge of
C       the field.
C
C    g) The routine tries to display as many significant figures as
C       possible, within the available field width, but will not
C       exceed a predetermined upper limit hard-wired as a parameter
C       (currently set to 7 in both the Cray and Vax versions).
C
C 4. An error return will occur if the routine cannot fit the value
C    into the available field width with suitable precision (at least
C    two significant figures for reals).  For real values, FIELD should
C    be at least 4 characters long.  You should be able to represent
C    any real value without hitting an error return if FIELD is at
C    least 8 characters in length.
C
C 5. When an error return occurs, the variable FIELD is returned filled
C    with BLANKS.
C-----------------------------------------------------------------------
C No external references called, other than standard fortran functions.
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date     Author     Description
C     ----     ------     -----------
C  3. 93/03/30 Peterson   Faster code due to not having to recalculate 
C                         some of the FORMATs, just index the FORMAT.
C  2. 93/02/01 Troutt     Add check for whole number <100. in util_ntoa_r.
C                         Use util_ntoa_i logic if true.  This allows 1 or
C                         2-digit whole numbers to convert in a 1 or 2-
C                         character field.
C  1. 88/07/05 Baumel     Initial version.
C-----------------------------------------------------------------------
C\END DOC
      subroutine util_ntoa_r (value,field,nchars)
      parameter (nmax=80,nsigmax=7)
      character field*(*),temp*(nmax),fmt*16
      character*8 fmt1(nsigmax),fmt2(nmax),fmt4(nmax)
      character*9 fmt3(nsigmax)
      data fmt1/'(e8.1)  ','(e9.2)  ','(e10.3) ','(e11.4) ','(e12.5) ',
     *          '(e13.6) ','(e14.7) '/
      data fmt2/'(f4.1)  ','(f5.2)  ','(f6.3)  ','(f7.4)  ','(f8.5)  ',
     *          '(f9.6)  ','(f10.7) ','(f11.8) ','(f12.9) ','(f13.10)',
     *          '(f14.11)','(f15.12)','(f16.13)','(f17.14)','(f18.15)',
     *          '(f19.16)','(f20.17)','(f21.18)','(f22.19)','(f23.20)',
     *          '(f24.21)','(f25.22)','(f26.23)','(f27.24)','(f28.25)',
     *          '(f29.26)','(f30.27)','(f31.28)','(f32.29)','(f33.30)',
     *          '(f34.31)','(f35.32)','(f36.33)','(f37.34)','(f38.35)',
     *          '(f39.36)','(f40.37)','(f41.38)','(f42.39)','(f43.40)',
     *          '(f44.41)','(f45.42)','(f46.43)','(f47.44)','(f48.45)',
     *          '(f49.46)','(f50.47)','(f51.48)','(f52.49)','(f53.50)',
     *          '(f54.51)','(f55.52)','(f56.53)','(f57.54)','(f58.55)',
     *          '(f59.56)','(f60.57)','(f61.58)','(f62.59)','(f63.60)',
     *          '(f64.61)','(f65.62)','(f66.63)','(f67.64)','(f68.65)',
     *          '(f69.66)','(f70.67)','(f71.68)','(f72.69)','(f73.70)',
     *          '(f74.71)','(f75.72)','(f76.73)','(f77.74)','(f78.75)',
     *          '(f79.76)','(f80.77)','(f81.78)','(f82.79)','(f83.80)'/
      data fmt3/'(1pe8.1) ','(1pe9.2) ','(1pe10.3)','(1pe11.4)',
     *          '(1pe12.5)','(1pe13.6)','(1pe14.7)'/
      data fmt4/'(i1)    ','(i2)    ','(i3)    ','(i4)    ','(i5)    ',
     *          '(i6)    ','(i7)    ','(i8)    ','(i9)    ','(i10)   ',
     *          '(i11)   ','(i12)   ','(i13)   ','(i14)   ','(i15)   ',
     *          '(i16)   ','(i17)   ','(i18)   ','(i19)   ','(i20)   ',
     *          '(i21)   ','(i22)   ','(i23)   ','(i24)   ','(i25)   ',
     *          '(i26)   ','(i27)   ','(i28)   ','(i29)   ','(i30)   ',
     *          '(i31)   ','(i32)   ','(i33)   ','(i34)   ','(i35)   ',
     *          '(i36)   ','(i37)   ','(i38)   ','(i39)   ','(i40)   ',
     *          '(i41)   ','(i42)   ','(i43)   ','(i44)   ','(i45)   ',
     *          '(i46)   ','(i47)   ','(i48)   ','(i49)   ','(i50)   ',
     *          '(i51)   ','(i52)   ','(i53)   ','(i54)   ','(i55)   ',
     *          '(i56)   ','(i57)   ','(i58)   ','(i59)   ','(i60)   ',
     *          '(i61)   ','(i62)   ','(i63)   ','(i64)   ','(i65)   ',
     *          '(i66)   ','(i67)   ','(i68)   ','(i69)   ','(i70)   ',
     *          '(i71)   ','(i72)   ','(i73)   ','(i74)   ','(i75)   ',
     *          '(i76)   ','(i77)   ','(i78)   ','(i79)   ','(i80)   '/
      logical point
c
      if(abs(value).lt.100.) then
        intvalue=nint(value) !uSE ALF2NUMI FOR SMALL WHOLE NUMBERS.
        if(float(intvalue).eq.value) goto 222 
      end if
c
ccc   if (value.eq.0.)  then
ccc     nchars = 1
ccc     field = '0'
ccc     return
ccc   end if
      lfield = len(field)
      if (value.lt.0.)  then
        field(1:1) = '-'
        nsign = 1
        tval = -value
      else
        nsign = 0
        tval = value
      end if
      if (tval.gt.1.e25 .or. tval.lt.1.e-25)  go to 999
      ipass = 1
  10  nsig = min(lfield-nsign-1,nsigmax)
      if (nsig.lt.2)  go to 999
ccc   write (fmt,100) nsig+7,nsig
c100  format ( '(e' , i2 , '.' , i2 , ')' )
ccc   write (temp,fmt,err=999) tval
      write (temp,fmt1(nsig),err=999) tval
      read (temp(nsig+5:),'(i3)',err=999) nexp
      if (nexp.le.0)  then                        ! vERY sMALL nUMBER
        ndec = min(lfield-nsign-2,nsigmax-nexp)
        if (ndec+nexp.lt.2)  go to 70
        if (ndec+3.gt.nmax)  go to 70
ccc     write (fmt,200) ndec+3,ndec
c200    format ( '(f' , i2 , '.' , i2 , ')' )
ccc     write (temp,fmt,err=999) tval
        write (temp,fmt2(ndec),err=999) tval
        do 20 i=1,ndec+2
  20    field(nsign+i:nsign+i) = temp(1+i:1+i)
        if (field(nsign+1:nsign+1).eq.' ') field(nsign+1:nsign+1) = '0'
        nchars = nsign+2+ndec
        point = .true.
      else if (nexp.gt.nsig)  then                ! vERY lARGE nUMBER
        if (nsign+nexp.gt.lfield+1)  go to 70
        if (nsig.lt.nsigmax .and. ipass.eq.1)  then
          nsig = nsig + 1
ccc       write (fmt,100) nsig+7,nsig
ccc       write (temp,fmt,err=999) tval
          write (temp,fmt1(nsig),err=999) tval
          read (temp(nsig+5:),'(i3)',err=999) nexp
          if (nexp.lt.nsig)  then
            ipass = 2
            go to 10
          end if
        end if
        if (nsign+nexp.gt.lfield)  go to 70
        do 30 i=1,nsig
  30    field(nsign+i:nsign+i) = temp(3+i:3+i)
        do 40 i=nsig+1,nexp
  40    field(nsign+i:nsign+i) = '0'
        nchars = nsign+nexp
        point = .false.
      else                                     ! mODERATE sIZE nUMBER
        do 50 i=1,nexp
  50    field(nsign+i:nsign+i) = temp(3+i:3+i)
        field(nsign+nexp+1:nsign+nexp+1) = '.'
        do 60 i=nexp+1,nsig
  60    field(nsign+1+i:nsign+1+i) = temp(3+i:3+i)
        nchars = nsign+1+nsig
        point = .true.
      end if
      go to 90
c
c  Come here if need E format
c
  70  ndec = min(lfield-nsign-6,nsigmax-1)
      if (ndec.lt.1)  go to 999
ccc   write (fmt,300) ndec+7,ndec
c300  format ( '(1pe' , i2 , '.' , i2 , ')' )
ccc   write (temp,fmt,err=999) tval
      write (temp,fmt3(ndec),err=999) tval
      do 80 i=1,ndec+6
  80  field(nsign+i:nsign+i) = temp(1+i:1+i)
      nchars = nsign+ndec+6
      point = .false.
c
c  Fill tail end of field with blanks
c                                                 
  90  do 110 i=nchars+1,lfield
 110  field(i:i) = ' '
      if (.not.point)  return
c
c  Remove trailing zeros if appropriate
c
      do 120 i=nchars,nsign+2,-1
        if (field(i:i).eq.'0')  then
          field(i:i) = ' '
        else
          if(field(i:i).eq.'.')  then
            field(i:i) = ' '
            nchars = i - 1
          else
            nchars = i
          end if
          return
        end if
 120  continue
      nchars = i
      return
c***********************************************************************
      entry util_ntoa_i (ivalue,field,nchars)
      intvalue=ivalue
  222 if (intvalue.eq.0)  then
        nchars = 1
        field = '0'
        return
      end if
      lfield = len(field)
      if (lfield.gt.nmax)  go to 999
ccc   write (fmt,400) lfield
c400  format ( '(i' , i2 , ')' )
ccc   write (temp,fmt,err=999) intvalue
      write (temp,fmt4(lfield),err=999) intvalue
      if (temp(1:1).eq.'*')  go to 999
      nchars = 0
      do 130 i=1,lfield
        if (temp(i:i).ne.' ')  then
          nchars = nchars+1
          field(nchars:nchars) = temp(i:i)
        end if
 130  continue
      do 140 i=nchars+1,lfield
 140  field(i:i) = ' '
      return
c
c  Error return
c
 999  nchars = 0
      field = ' '
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_xy_taper(ix,iy,nx,ny,nx_taper,ny_taper,n_tr,tr)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer  ix,iy,nx,ny,nx_taper,ny_taper,n_tr
      real     tr(n_tr)
      real     x_scale,y_scale
      integer  jx,jy,i_call
      data     i_call/0/
      i_call = i_call + 1

      jx = ix

      if (ix .gt. nx/2) jx = nx - ix + 1

      if (nx .gt. nx_taper .and. jx .le. nx_taper) then

        x_scale = float(jx) / float(max(1,nx_taper))

      else    ! if (nx .gt. nx_taper .and. jx .le. nx_taper) then

        x_scale = 1.

      endif    ! if (nx .gt. nx_taper .and. jx .le. nx_taper) then

      jy = iy

      if (iy .gt. ny/2) jy = ny - iy + 1

      if (ny .gt. ny_taper .and. jy .le. ny_taper) then

        y_scale = float(jy) / float(max(1,ny_taper))

      else    ! if (ny .gt. ny_taper .and. jy .le. ny_taper) then

        y_scale = 1.

      endif    ! if (ny .gt. ny_taper .and. jy .le. ny_taper) then

      call util_scale(n_tr,tr,x_scale*y_scale)

      if (i_call .eq. 1 .and. util_dbg() .ge. 0) write(util_dbg()
     1,'('' util_xy_taper nx_taper='',i8,'' ny_taper='',i8
     1,'' nx='',i8,'' ny='',i8)')
     1 nx_taper,ny_taper,nx,ny

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_c_phase(mig_dir,n_phase,c_phase,d_phase)
c  compute a table of complex phases
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer   n_phase
      real      d_phase
      complex   c_phase(1)
      character mig_dir*(*)

      integer   i_phase,s_phase
      real      phase

      if (n_phase .le. 0) return

      if (mig_dir(1:7) .eq. 'MIGRATE') then

        s_phase = + 1

      elseif (mig_dir(1:5) .eq. 'MODEL') then    ! if (mig_dir(1:7) .eq. 

        s_phase = - 1

      else    ! if (mig_dir(1:7) .eq. 'MIGRATE') then

        write(util_err()
     1,'(/,'' error in util_c_phase parameter MIG_DIR='',a8
     1,'' This must be MIGRATE or MODEL'')')mig_dir
        stop

      endif    ! if (mig_dir(1:7) .eq. 'MIGRATE') then

      d_phase = 4. * asin(1.) / n_phase
      do i_phase = 1 , n_phase

        phase = float(s_phase) * (i_phase - 1) * d_phase
        c_phase(i_phase) = cmplx(cos(phase),sin(phase))

      enddo    ! do i_phase = 1 , n_phase

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_filter(
     1 f_min,f_max,f_tap_1,f_tap_2
     1,nt_inp,t0_inp,dt_inp
     1,tr_inp
     1,m_work,work
     1,i_err
     1)
c  filter a time trace
c  f_min,f_max,f_tap_1,f_tap_2 are in hertz
c  t0_inp,dt_inp are in seconds
c  m_work = 2*nf_fft where nf_fft is the next power of 2>nt_inp

      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer   util_pow2
      real      util_rd
      real      util_amax


      integer  nt_inp
      real     t0_inp,dt_inp

      real     f_min,f_max,f_tap_1,f_tap_2

      real     tr_inp(nt_inp)

      integer  m_work
      complex  work(m_work/2)

      integer  i_err

      integer  nf_fft,if_fft,i
      real     f_nyquist,df_fft,pi,f,f_amp
      real     fmin1,fmin2,df1
      real     fmax1,fmax2,df2
      real     tr_inp_max,tr_out_max

      i_err = 0

      nf_fft = util_pow2(nt_inp)

      if (m_work .lt. nf_fft*2) goto 999

      tr_inp_max = util_amax(nt_inp,tr_inp)
c  initialize work space to zero
      call util_setr(nf_fft*2,work,0.)

c  copy input trace to comple work array
      call util_copy_inc(nt_inp,1,tr_inp,2,work)

c  take real to complex fft
      call util_fft(-1,nf_fft,work)

c  apply filter
      fmin1     = f_min - f_tap_1 / 2.
      fmin2     = f_min + f_tap_1 / 2.

      fmax1     = f_max - f_tap_2 / 2.
      fmax2     = f_max + f_tap_2 / 2.

      df1       = fmin2 - fmin1
      df2       = fmax2 - fmax1

      f_nyquist = 1. / (2. * dt_inp)           ! in hertz
      df_fft    = f_nyquist / (nf_fft - 1)     ! in hertz
      f         = -df_fft
      pi        = 2. * asin(1.)

      do if_fft = 1 , nf_fft

        f = f + df_fft

        if (fmin1 .lt. f .and. f .lt. fmin2) then

          f_amp = (1. + cos(pi * (fmin2 - f) / df1)) / 2.

        elseif (fmin2 .le. f .and. f .le. fmax1) then

          f_amp = 1.

        elseif (fmax1 .lt. f .and. f .le. fmax2) then

          f_amp = (1. + cos(pi * (f - fmax1) / df2)) / 2.

        else

          f_amp = 0.

        endif

        work(if_fft) = work(if_fft) * f_amp

      enddo    ! do if_fft = 1 , nf_fft

c  take complex to real fft
      call util_fft(+1,nf_fft,work)

c  copy real part of work back to tr_inp
      call util_copy_inc(nt_inp,2,work,1,tr_inp)

      tr_out_max = util_amax(nt_inp,tr_inp)

c  note cray scaling is by 1 . / n where n is forward fft
      write(util_prn(),'(/,'' util_filter''
     1,/,'' time filter characteristics''
     1,/,'' nf_fft='',i8
     1,'' nt_inp='',i8,'' t0_inp='',f10.2,'' dt_inp='',f10.4
     1,/,'' f_min  ='',f10.2,'' f_max  ='',f10.2
     1,/,'' f_tap_1='',f10.2,'' f_tap_2='',f10.2
     1,/,'' tr_inp_max='',g16.9
     1,/,'' tr_out_max='',g16.9
     1)')
     1 nf_fft,nt_inp,t0_inp,dt_inp
     1,f_min,f_max,f_tap_1,f_tap_2
     1,tr_inp_max,tr_out_max

      return

  998 continue
      write(util_err(),'(/,'' error in util_filter''
     1,/,'' need more work space for fft''
     1,/,'' nt_inp='',i8,'' nf_fft='',i8
     1,/,'' need nf_fft*2='',i8,'' have='',i8
     1)')
     1 nt_inp,nf_fft,nf_fft*2,m_work
      goto 999

  999 continue
      write(util_err(),'(/,'' error in util_filter'')')
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
        subroutine util_fft(id,nf,f)
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

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_1d_fft_rc(init,i_1d_fft
     1,nt_inp,t0_inp,dt_inp,tr_inp
     1,nt_fft,w_fft,r_fft,c_fft
     1,n_work,work)
c  take 1d real to complex of input trace
      implicit   none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer   util_len_r

      integer   init,i_1d_fft

      integer   nt_inp
      real      t0_inp,dt_inp
      real      tr_inp(nt_inp)

      integer   nt_fft
      real      w_fft(3*nt_fft+4)
      real      r_fft(nt_fft+2)
      complex   c_fft(nt_fft/2+1)
      integer   n_work
      real      work(n_work)

      real      scale_fft

      integer   icall
      data      icall/0/

      icall = icall + 1
c      if (icall .eq. 1 .or. init .eq. 1)
c     1if (util_dbg() .ge. 0) write(util_dbg()
c     1,'(/,'' util_1d_fft_rc icall='',i8,'' init='',i8
c     1,'' i_1d_fft='',i3,'' nt_fft='',i8
c     1,/,'' nt_inp='',i8,'' t0_inp='',f10.2,'' dt_inp='',f10.4)')
c     1 icall,init,i_1d_fft,nt_fft,nt_inp,t0_inp,dt_inp

      scale_fft = .5 / nt_fft                  ! 1d fft scale factor

c  real to complex fft

c  replace rcfft2 and crfft by scfft and csfft respectively
c  initialize tables - w_fft
c      if (init .eq. 1)
c     1call rcfft2(1,+i_1d_fft,nt_fft,r_fft,w_fft,c_fft)
      if (init .eq. 1)
     1call util_scfft(0,nt_fft,r_fft,c_fft,w_fft,work,0)

c  copy input trace into r_fft
      call util_copy(nt_inp,tr_inp,r_fft)
      call util_setr(nt_fft+2-nt_inp,r_fft(nt_inp+1),0.)

c  real to complex fft r_fft to c_fft
c      call rcfft2(0,+i_1d_fft,nt_fft,r_fft,w_fft,c_fft)
      call util_scfft(+i_1d_fft,nt_fft,r_fft,c_fft,w_fft,work,0)

c  scale c_fft
c      call util_scale(nt_fft+2,c_fft,scale_fft)

c  apply a phase shift to account for the non zero time origin
      call util_phase_shift(+i_1d_fft,nt_fft/2+1
     1,nt_fft,t0_inp,dt_inp,c_fft)

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_1d_fft_cr(init,i_1d_fft
     1,nt_inp,t0_inp,dt_inp,tr_inp
     1,nt_fft,w_fft,r_fft,c_fft
     1,n_work,work)
c  take 1d complex to real of input trace
      implicit   none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer   util_len_r

      integer   init,i_1d_fft

      integer   nt_inp
      real      t0_inp,dt_inp
      real      tr_inp(nt_inp)

      integer   nt_fft
      real      w_fft(1)
      real      r_fft(1)
      complex   c_fft(nt_fft/2+1)
      integer   n_work
      real      work(n_work)

      real      scale_fft

      integer   icall
      data      icall/0/

      icall = icall + 1
c      if (icall .eq. 1 .or. init .eq. 1)
c     1if (util_dbg() .ge. 0) write(util_dbg()
c     1,'(/,'' util_1d_fft_cr icall='',i8,'' init='',i8
c     1,'' i_1d_fft='',i3,'' nt_fft='',i8
c     1,/,'' nt_inp='',i8,'' t0_inp='',f10.2,'' dt_inp='',f10.4)')
c     1 icall,init,i_1d_fft,nt_fft,nt_inp,t0_inp,dt_inp

      scale_fft = .5 / nt_fft                  ! 1d fft scale factor

c  complex to real fft

c  replace rcfft2 and crfft by scfft and csfft respectively
c  initialize tables - w_fft
c      if (init .eq. 1)
c     1call crfft2(1,-i_1d_fft,nt_fft,r_fft,w_fft,r_fft)
        if (init .eq. 1)
     1call util_csfft(0,nt_fft,r_fft,c_fft,w_fft,work,0)

c  apply a phase shift to account for the non zero time origin
      call util_phase_shift(-i_1d_fft,nt_fft/2+1
     1,nt_fft,t0_inp,dt_inp,c_fft)

c  scale c_fft
c      call util_scale(nt_fft+2,c_fft,scale_fft)

c  complex to real fft c_fft to r_fft
c      call crfft2(0,-i_1d_fft,nt_fft,c_fft,w_fft,r_fft)
      call util_scfft(-i_1d_fft,nt_fft,r_fft,c_fft,w_fft,work,0)

c  copy from r_fft into tr_inp
      call util_copy(nt_inp,r_fft,tr_inp)

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_phase_shift(i_1d_fft,nw_fft
     1,nt_fft,t0_inp,dt_inp,c_fft)
c  apply phase shift of t0_inp to nw_inp frequencies starting at w=0
      implicit  none

      integer  i_1d_fft

      integer  nw_fft
      integer  nt_fft
      real     t0_inp,dt_inp

      complex  c_fft(1)

      integer  iw_fft
      real     dw_fft
      complex  c_exp

c  apply a phase shift to account for the nonzero origin
      dw_fft = 1. / ((nt_fft - 1) * dt_inp)    ! fft frequency increment
      c_exp = cmplx(0.,i_1d_fft*t0_inp)

      do iw_fft = 1 , nw_fft

        c_fft(iw_fft) = c_fft(iw_fft) * cexp(c_exp * (iw_fft-1)*dw_fft)

      enddo    ! do iw_fft = 1 , nw_fft

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_1d_fft(input,init,i_1d_fft
     1,nt_inp,it_fft,nt_fft,w_fft,r_fft,c_fft,tr_inp,n_work,work)
c  take 1d real to complex or complex to real fft of input trace
      implicit   none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer   util_isamax
      integer   util_len_r

      character input*(*)
      integer   init,i_1d_fft
      integer   nt_inp,it_fft,nt_fft

      real      w_fft(1)      ! PVP 100 + 4*n MPP 2*n - table
      real      r_fft(nt_fft)
      complex   c_fft(nt_fft/2+1)

      real      tr_inp(nt_inp)

      integer   n_work        ! PVP 4 + 4*n MPP 2*n
      real      work(n_work)

      real      scale_fft

      integer   icall
      data      icall/0/
      icall = icall + 1

c     table     Cray PVP systems:  Real array of dimension (100 + 4*n).        |
c               (input or output)                                              |
c               Cray MPP systems:  Real array of dimension (2*n).  (input or   |
c               output)                                                        |
c               Table of factors and trigonometric functions.
c
c               If isign = 0, the table array is initialized to contain
c               trigonometric tables needed to compute an FFT of size n.  If
c               isign = +1 or -1, the values in table are assumed to be
c               initialized already by a prior call with isign = 0.
c
c     work      Cray PVP systems:  Real array of dimension (4 + 4*n).          |
c               (scratch output)                                               |
c               Cray MPP systems:  Real array of dimension (2*n).
c               Work array used for intermediate calculations.
c               Its address space must be different from that of the input
c               and output arrays.

c      if (icall .eq. 1 .or. init .eq. 1) then
c        li = util_len_r(input)
c        if (util_dbg() .ge. 0) write(util_dbg()
c     1,'(/,'' util_1d_fft icall='',i8,'' init='',i8
c     1,'' i_1d_fft='',i3,'' input='',a
c     1,/,'' it_fft='',i8,'' nt_inp='',i8,'' nt_fft='',i8)'
c     1,icall,init,i_1d_fft,input(1:li),it_fft,nt_inp,nt_fft
c      endif

      scale_fft = .5 / nt_fft    ! 1d fft scale factor

c  replace rcfft2 and crfft by scfft and csfft respectively
c SCFFT, CSFFT - Computes a real-to-complex or complex-to-real FFT
c call util_scfft (isign, n, x, y, table, work, isys)
c call util_csfft (isign, n, x, y, table, work, isys)

      if (input(1:4) .eq. 'REAL') then
c  real to complex fft

c       if (init .eq. 1)
c    1call rcfft2(1,+i_1d_fft,nt_fft,r_fft,w_fft,c_fft)
        if (init .eq. 1)
     1call util_scfft(0,nt_fft,r_fft,c_fft,w_fft,work,0)
        call util_setr(nt_fft+2,r_fft,0.)
        call util_copy(nt_inp,tr_inp,r_fft(it_fft))
c       call rcfft2(0,+i_1d_fft,nt_fft,r_fft,w_fft,c_fft)
c       call util_scale(nt_fft+2,c_fft,scale_fft)
        call util_scfft(+i_1d_fft,nt_fft,r_fft,c_fft,w_fft,work,0)

      elseif (input(1:7) .eq. 'COMPLEX') then    ! if (input(1:4) .eq. 'REAL'
c  complex to real fft

c       if (init .eq. 1)
c    1call crfft2(1,-i_1d_fft,nt_fft,r_fft,w_fft,r_fft)
        if (init .eq. 1)
     1call util_csfft(0,nt_fft,r_fft,c_fft,w_fft,work,0)
c       call util_scale(nt_fft+2,c_fft,scale_fft)
c       call crfft2(0,-i_1d_fft,nt_fft,c_fft,w_fft,r_fft)
        call util_scfft(-i_1d_fft,nt_fft,r_fft,c_fft,w_fft,work,0)
        call util_copy(nt_inp,r_fft(it_fft),tr_inp)

      else    ! if (input(1:4) .eq. 'REAL') then

        write(util_err()
     1,'(/,'' error in util_1d_fft parameter INPUT must be''
     1,'' REAL or COMPLEX INPUT='',a)')
     1input(1:util_len_r(input))
        stop

      endif    ! if (input(1:4) .eq. 'REAL') then

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_mcfft(i_2d_fft,mt,nt,nx,data
     1,n_table,table,n_work,work)
c  take inverse complex to complex fft in the x direction
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer  i_2d_fft,nx,nt,mt,n_table,n_work
      complex  data(mt/2,nx)
      real     table(n_table),work(n_work)

      real     scale
      integer  icall,i,loc
      data     icall/0/

      icall = icall + 1

      scale  = 1. / sqrt(float(nx)) ! scale value for 1D fft
c     n_table = 100 + 2n             ! length of tables
c     n_work  = 4*n*min(m,16*cpus)   ! length of work
      if (mod(mt,2) .ne. 0 .or. mt .lt. nt) then
        write(util_err()
     1,'(/,'' error in util_mcfft mt='',i8,'' nt='',i8,'' nx='',i8
     1,'' n_table='',i8,'' n_work='',i8)')
     1 mt,nt,nx,n_table,n_work
        stop
      endif    ! if (mod(mt,2) .ne. 0 .or. mt .lt. nt) then

c      if (util_dbg() .ge. 0) write(util_dbg()
c     1,'(/,'' util_mcfft transforming a section over space''
c     1,/,'' i_2d_fft='',i2,'' nx='',i6,'' nt='',i6,'' mt='',i6
c     1,'' n_table='',i8,'' n_work='',i8)')
c     1 i_2d_fft,nx,nt,mt,n_table,n_work

c  compute tables
      call mcfft(       0,nx,nt,scale,data,mt/2,1,data,mt/2,1
     1,table,n_table,work,n_work)

c  compute ffts
      call mcfft(i_2d_fft,nx,nt,scale,data,mt/2,1,data,mt/2,1
     1,table,n_table,work,n_work)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_scfft(i_sign,n_fft,x_inp,x_out,table,work,i_sys)
c     SCFFT, CSFFT - Computes a real-to-complex or complex-to-real Fast
c     Fourier Transform (FFT)
c
cSYNOPSIS
c     CALL SCFFT (isign, n, scale, x, y, table, work, isys)
c
c     CALL CSFFT (isign, n, scale, x, y, table, work, isys)
c
cIMPLEMENTATION
c     All Cray Research systems
c
c     On Cray MPP systems, this subroutine executes on a single processor
c     and uses only private data.
c
cDESCRIPTION
c     SCFFT computes the FFT of the real array X, and it stores the results
c     in the complex array Y.  CSFFT computes the corresponding inverse
c     complex-to-real transform.
c
c     It is customary in FFT applications to use zero-based subscripts; the
c     formulas are simpler that way.  First the function of SCFFT is
c     described.  Suppose that the arrays are dimensioned as follows:
c
c          REAL    X(0:n-1)
c          COMPLEX Y(0:n/2)
c
c     Then the output array is the FFT of the input array, using the
c     following formula for the FFT:
c
c                         n-1
c          Y(k) = scale * Sum [ X(j)*w**(isign*j*k) ]    for k = 0, ..., n/2
c                         j=0
c
c          where:
c          w = exp(2*pi*i/n),
c          i = + sqrt(-1),
c          pi = 3.14159...,
c          isign = +1 or -1.
c      Different authors use different conventions for which of the
c     transforms, isign = +1 or isign = -1, is the forward or inverse
c     transform, and what the scale factor should be in either case.  You
c     can make these routines compute any of the various possible
c     definitions, however, by choosing the appropriate values for isign and
c     scale.
c
c     The relevant fact from FFT theory is this:  If you call SCFFT with any
c     particular values of isign and scale, the mathematical inverse
c     function is computed by calling CSFFT with -isign and 1/(n*scale).  In
c     particular, if you use
c
c          isign = +1 and scale = 1.0
c
c     in SCFFT for the forward FFT, you can compute the inverse FFT by using
c     CSFFT with the following:
c
c          isign = -1 and scale = 1.0/n
c
c     This routine has the following arguments:
c
c     isign     Integer.  (input)
c               Specifies whether to initialize the table array or to do the
c               forward or inverse Fourier transform, as follows:
c
c               If isign = 0, the routine initializes the table array and
c               returns.  In this case, the only arguments used or checked
c               are isign, n, and table.  If isign = +1 or -1, the value of
c               isign is the sign of the exponent used in the FFT formula.
c
c     n         Integer.  (input)
c               Size of transform.
c               If n <= 2, SCFFT returns without calculating the transform.
c
c     scale     Real.  (input)
c               Scale factor.
c               Each element of the output array is multiplied by scale
c               after taking the Fourier transform, as defined in the
c               preceding formula.
c
c     x         SCFFT:  Real array of dimension (0:n-1).  (input)
c               CSFFT:  Complex array of dimension (0:n/2).  (input)
c               Input array of values to be transformed.
c
c     y         SCFFT:  Complex array of dimension (0:n/2).  (output)
c               CSFFT:  Real array of dimension (0:n-1).  (output)
c               Output array of transformed values.
c               The output array, Y, is the FFT of the the input array, X,
c               computed according to the preceding formula.  The output
c               array may be equivalenced to the input array in the calling
c               program.  Be careful when dimensioning the arrays, in this
c               case, to allow for the fact that the complex array contains
c               two (real) words more than the real array.
c
c     table     Cray PVP systems:  Real array of dimension (100 + 4*n).
c               (input or output)
c               Cray MPP systems:  Real array of dimension (2*n).  (input or
c               output)
c               Table of factors and trigonometric functions.
c
c               If isign = 0, the table array is initialized to contain
c               trigonometric tables needed to compute an FFT of size n.  If
c               isign = +1 or -1, the values in table are assumed to be
c               initialized already by a prior call with isign = 0.
c
c     work      Cray PVP systems:  Real array of dimension (4 + 4*n).
c               (scratch output)
c               Cray MPP systems:  Real array of dimension (2*n).
c               Work array used for intermediate calculations.
c               Its address space must be different from that of the input
c               and output arrays.
c
c     isys      Integer array of dimension (0:isys(0)).  (input and output)
c               That is, the first element of the array specifies how many
c               more elements are in the array.
c               You may use isys to specify certain processor-specific
c               parameters or options.
c
c               If isys(0) = 0, the default values of such parameters are
c               used.  In this case, you can specify the argument value as
c               the scalar integer constant 0.  If isys(0) > 0, isys(0)
c               gives the upper bound of the isys array; that is, if il =
c               isys(0), user-specified parameters are expected in isys(1)
c               through isys(il).
c
      implicit  none
      integer i_sign,n_fft,i_sys
      real    x_inp(n_fft)
      complex x_out(n_fft/2+1)
      real    table(1)
      real    work(1)

      real    scale
      integer j_sys

      j_sys = 0
      scale = 1. / n_fft

      call scfft(i_sign,n_fft,scale,x_inp,x_out,table,work,j_sys)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_csfft(i_sign,n_fft,x_inp,x_out,table,work,i_sys)
c  take complex to real fft using cray routine
      implicit  none
      integer i_sign,n_fft,i_sys
      complex x_inp(n_fft/2+1)
      real    x_out(n_fft)
      real    table(1)
      real    work(1)

      real    scale
      integer j_sys

      j_sys = 0
      scale = 1. 

      call csfft(i_sign,n_fft,scale,x_inp,x_out,table,work,j_sys)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_ccfft(i_sign,n_fft,x_inp,x_out,table,work,i_sys)
c  take complex to complex fft using cray routine
cNAME
c     CCFFT - Applies a multitasked complex-to-complex Fast Fourier
c     Transform (FFT)
c
cSYNOPSIS
c     CALL CCFFT (isign, n, scale, x, y, table, work, isys)
c
cIMPLEMENTATION
c     All Cray Research systems.
c
c     On Cray MPP systems, this subroutine executes on a single processor
c     and uses private data only.
c
cDESCRIPTION
c     CCFFT computes the Fast Fourier Transform (FFT) of the complex vector
c     x, and it stores the result in vector y.
c
c     In FFT applications, it is customary to use zero-based subscripts; the
c     formulas are simpler that way.  Suppose that the arrays are
c     dimensioned as follows:
c
c          COMPLEX X(0:N-1), Y(0:N-1)
c
c     The output array is the FFT of the input array, using the following
c     formula for the FFT:
c
c                         n-1
c          Y(k) = scale * Sum [ X(j)*w**(isign*j*k) ]    for k = 0, ..., n-1
c                         j=0
c
c          where:
c          w = exp(2*pi*i/n),
c          i = + sqrt(-1),
c          pi = 3.14159...,
c          isign = +1 or -1
c     Different authors use different conventions for which of the
c     transforms, isign = +1 or isign = -1, is the forward or inverse
c     transform, and what the scale factor should be in either case.  You
c     can make this routine compute any of the various possible definitions,
c     however, by choosing the appropriate values for isign and scale.
c
c     The relevant fact from FFT theory is this:  If you take the FFT with
c     any particular values of isign and scale, the mathematical inverse
c     function is computed by taking the FFT with -isign and 1/(n*scale).
c     In particular, if you use the following for the forward FFT:
c
c          isign = +1 and scale = 1.0
c
c     you can compute the inverse FFT by using the following:
c
c          isign = -1 and scale = 1.0/n
c
c     The output array may be the same as the input array, provided that n
c     has at least 2 factors.
c
c     On Cray MPP systems only:  if the length of the FFT (i.e. n) is not
c     factorizable into powers of 2, 3 and 5 (that is, when a fast
c     mixed-radix algorithm cannot be used) the user may specify that a fast
c     chirp-z transform-based algorithm be used instead of a slow O(n^2)
c     algorithm.  The isys variable allows the user to exercise this option.
c     Setting the value of isys to 0 uses the slow algorithm while setting
c     it to 1 flags the use of the fast algorithm.  Depending on the value
c     of isys specified, the size of the table vector and workspace vector
c     vary.
c
c     This routine has the following arguments:
c
c     isign   Integer.  (input)
c
c             Specifies whether to initialize the table array or to do the
c             forward or inverse Fourier transform, as follows:
c
c             If isign = 0, the routine initializes the table array and
c             returns.  In this case, the only arguments used or checked are
c             isign, n, and table.
c
c             If isign = +1 or -1, the value of isign is the sign of the
c             exponent used in the FFT formula.
c
c     n       Integer.  (input)
c
c             Size of the transform (the number of values in the input
c             array).  n >= 2.
c
c     scale   Real.  (input)
c
c             Scale factor.  Each element of the output array is multiplied
c             by scale after taking the Fourier transform, as defined by the
c             previous formula.
c
c     x       Complex array of dimension (0:n-1).   (input)
c
c             Input array of values to be transformed.
c
c     y       Complex array of dimension (0:n-1).  (output)
c
c             Output array of transformed values.  The output array may be
c             the same as the input array, in which case, the transform is
c             done in place; that is, the input array is overwritten with
c             the transformed values.
c
c     table   Cray PVP systems:  Real array of dimension 100 + 8*n. (input
c             or output)
c             Cray MPP systems:  Real array of dimension 2*n when isys = 0
c             and real array of dimension 12*n when isys = 1.
c
c             Table of factors and trigonometric functions.
c
c             If isign = 0, the routine initializes table (table is output
c             only).
c
c             If isign = +1 or -1, the values in table are assumed to be
c             initialized already by a prior call with isign = 0 (table is
c             input only).
c
c     work    Cray PVP systems:  Real array of dimension 8*n.   (workspace)
c             Cray MPP systems:  Real array of dimension 4*n when isys = 0
c             and real array of dimension 8*n when isys = 1 (workspace).
c
c             Work array.  This is a scratch array used for intermediate
c             calculations.  Its address space must be different address
c             space from that of the input and output arrays.
c
c     isys    Integer. (Input)
c
c             On Cray MPP systems:  If n is prime or not factorizable into
c             powers of 2, 3 and 5 then intializing isys = 1 results in the
c             use of a fast chirp-z based FFT algorithm.  When n is
c             factorizable into powers of 2, 3 and 5, then setting isys = 0
c             results in the fastest algorithm being used.  Setting isys =
c             1, in the latter case results in a slower algorithm.
c
c             On Cray PVP systems:  the value of isys must be set to 0.
      implicit  none
      integer i_sign,n_fft,i_sys
      complex x_inp(n_fft)
      complex x_out(n_fft)
      real    table(1)
      real    work(1)

      real    scale
      integer j_sys

      j_sys = 0
      scale = 1. 
      if (i_sign .gt. 0) scale = 1. / n_fft

      call ccfft(i_sign,n_fft,scale,x_inp,x_out,table,work,j_sys)

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_pack_complex_to_real(nt_out,tr_inp,tr_out)
c  pack the tr_inp into the real components of tr_out

      implicit  none

      integer  nt_out
      real     tr_out(nt_out)
      complex  tr_inp(nt_out)

      integer  it_out

      do it_out = 1 , nt_out

          tr_out(it_out) = real(tr_inp(it_out))

      enddo    ! do it_out = 1 , nt_out

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_pack_real_to_complex(nt_out,tr_inp,tr_out)
c  unpack the real components of tr_inp into tr_out
      implicit  none

      integer  nt_out
      real     tr_inp(nt_out)
      complex  tr_out(nt_out)

      integer  it_out

      do it_out = 1 , nt_out

          tr_out(it_out) = cmplx(tr_inp(it_out),0.)

      enddo    ! do it_out = 1 , nt_out

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_apply_mute(mh_inp,hd_inp,nt_inp,tr_inp)
c  reapply mutes
      implicit  none

      integer  mh_inp
      real     hd_inp(mh_inp)

      integer  nt_inp
      real     tr_inp(nt_inp)

      integer   it_mute,nt_mute

c  reapply top mute
      it_mute = 1
      nt_mute = max(0,min(nt_inp,nint(hd_inp( 2))-1))
      call util_setr(nt_mute,tr_inp(it_mute),0.)

c  reapply bottom mute
      it_mute = max(1,min(nt_inp,nint(hd_inp(64))+1))
      nt_mute = max(0,min(nt_inp,nt_inp-it_mute+1))
      call util_setr(nt_mute,tr_inp(it_mute),0.)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_set_mute(m_hd,hd,n_tr,tr)
c  set the top and bottom mute indexes to the first nonzero value location
      implicit  none
      integer   m_hd,n_tr
      real      hd(m_hd),tr(n_tr)

      integer   i_tr

c  set mute
      do i_tr = 1 , n_tr

        if (tr(i_tr) .ne. 0.) then

          hd(2) = i_tr
          goto 1

        endif    ! if (tr(i_tr) .ne. 0.) then

      enddo    ! do i_tr = 1 , n_tr

    1 continue

c  set tail mute
      do i_tr = n_tr , 1 , -1

        if (tr(i_tr) .ne. 0.) then

          hd(64) = i_tr
          goto 2

        endif    ! if (tr(i_tr) .ne. 0.) then

      enddo    ! do i_tr = n_tr , 1 , -1

    2 continue

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_tpow(alpha,beta,t1,t2,nt,t0,dt,x)
c  apply tpow type scaling
c  x out = x inp * t **alpha * exp(t *beta) t1<=t<=t2
c  x out = x inp * t1**alpha * exp(t1*beta) t1>=t
c  x out = x inp * t2**alpha * exp(t2*beta)     t>=t2
      implicit  none

      real     alpha,beta
      real     t1,t2
      integer  nt
      real     t0,dt
      real     x(nt)

      integer  it
      integer  it1,it2
      real     t,t_scale

      it1 = max(nint((t1-t0)/dt+1), 1)
      it2 = min(nint((t2-t0)/dt+1),nt)

      if (alpha .ne. 0.) then

        do it = it1 , it2

          t = (it - 1) * dt + t0
          t_scale = t ** alpha * exp(beta*t)
          x(it) = x(it) * t_scale

        enddo    ! do it = it1 , it2

        it = it1
        t = (it - 1) * dt + t0
        t_scale = t ** alpha * exp(beta*t)
        call util_scale(it1-1 ,x(    1),t_scale)

        it = it2
        t = (it - 1) * dt + t0
        t_scale = t ** alpha * exp(beta*t)
        call util_scale(nt-it2,x(it2+1),t_scale)

      else    ! if (alpha .ne. 0.) then

        do it = it1 , it2

          t = (it - 1) * dt + t0
          t_scale = exp(beta*t)
          x(it) = x(it) * t_scale

        enddo    ! do it = it1 , it2

        it = it1
        t = (it - 1) * dt + t0
        t_scale = exp(beta*t)
        call util_scale(it1-1 ,x(    1),t_scale)

        it = it2
        t = (it - 1) * dt + t0
        t_scale = exp(beta*t)
        call util_scale(nt-it2,x(it2+1),t_scale)

      endif    ! if (alpha .ne. 0.) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_compute_gaussian(title
     1,i_gauss,nt_gauss,dt_gauss,gauss)
c  compute a gaussian wavelet or its derivitieves
c  title = character string for printing
c  i_gauss  = type of wavelet flage 
c             0,1,2 for gaussian or its first two derivitives.
c  nt_gauss = number of elements in wavelet 
c  dt_gauss = time increment - used only for print purposes
c  gauss    = wavelet array
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      character title*(*)

      integer  i_gauss
      real     dt_gauss

      integer  nt_gauss
      real     gauss(nt_gauss)

      integer  it_gauss
      real     t,tmax,t0,t1,y0,y1,y2
      real     t_gauss

      tmax = (nt_gauss - 1) * dt_gauss
      t0 = tmax / 2.0
      t1 = tmax / 6.
      y0 = -1.0 / t1 ** 2
      y1 = 2.0 * y0

      do it_gauss = 1 , nt_gauss

        t = (it_gauss - 1) * dt_gauss
        t_gauss = t - t0

c  gaussian
        if (i_gauss .eq. 0) then

          y2 = 1

c  first derivitive
        elseif (i_gauss .eq. 1) then    ! if (i_gauss .eq. 0) then

          y2 = - y1 * t_gauss

c  second derivitive
        else

          y2 = - y1 * (1.0 + y1 * t_gauss * t_gauss)

        endif    ! if (i_gauss .eq. 0) then

        gauss(it_gauss) = y2 * exp (y0 * t_gauss ** 2)

      enddo    ! do it_gauss = 1 , nt_gauss

c  normalize the wavelet
      call util_normalize(nt_gauss,gauss,1.)

      call util_print_gaussian(title,util_prn()
     1,i_gauss,nt_gauss,dt_gauss,gauss)

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_print_gaussian(title,lu_out
     1,i_gauss,nt_gauss,dt_gauss,gauss)

c  print a gaussian wavelet or its derivitives
c  i_gauss = type of wavelet flage 
c             0,1,2 for gaussian or its first two derivitives.
c  nt_gauss = number of elements in wavelet 
c  dt_gauss = time increment
c  gauss    = wavelet array
      implicit  none

      integer  util_len_r

      character title*(*)
      integer    lu_out

      integer  i_gauss
      integer  nt_gauss
      real     dt_gauss
      real     gauss(nt_gauss)

      integer  it_gauss

      if (lu_out .lt. 0 .or. lu_out .gt. 99) return

      write(lu_out,'(/,'' util_print_gaussian''
     1,/,a
     1,/,'' i_gauss='',i8,'' nt_gauss='',i8,'' dt_gauss='',f12.4
     1)')
     1 title(1:util_len_r(title))
     1,i_gauss,nt_gauss,dt_gauss
      write(lu_out,'(1x,f12.4,1x,f12.4,1x,i8,1x,i8)')
     1((it_gauss-1)*dt_gauss,gauss(it_gauss),1,it_gauss
     1,it_gauss=1,nt_gauss)

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_linear_taper(ax_taper_1,ax_taper_2
     1,nx_inp,x_inp)
c  apply a linear taper that varies 
c  from ax_taper_1 at ix=1 to ax_taper_2 at ix=nx_inp
c  ax_taper_1 = amplitude at start of taper, x_inp(1     )=ax_taper_1
c  ax_taper_2 = amplitude at end   of taper, x_inp(nx_inp)=ax_taper_2
c  nx_inp     = number of elements in array x_inp
c  x_inp      = taper coefficients
      implicit  none

      real     ax_taper_1,ax_taper_2
      integer  nx_inp
      real     x_inp(nx_inp)

      integer  ix_inp
      real     x_taper,da_dx

      da_dx = (ax_taper_2 - ax_taper_1) / max(1,nx_inp-1)

c  linear taper
      do ix_inp = 1 , nx_inp

        x_taper = ax_taper_1 + (ix_inp - 1) * da_dx
        x_inp(ix_inp) = x_inp(ix_inp) * x_taper

      enddo    ! do ix_inp = 1 , nx_inp

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_taper_vector(i_taper,a_taper,nx_inp,x_inp)
c  apply a taper that varies 
c  from ax_taper_1 at ix=1 to ax_taper_2 at ix=nx_inp
c  i_taper = type of taper 1=liner, 2=exponential, 3=gaussian, 4=cosine
c  a_taper = taper value
c  nx_inp  = number of elements in array x_inp
c  x_inp   = taper coefficients
      implicit  none

      integer  i_taper
      real     a_taper
      integer  nx_inp
      real     x_inp(nx_inp)

      integer  ix_inp
      real     b_taper
      real     x_taper,da_dx,alpha

      b_taper = max(0.,min(1.,a_taper))

c  linear taper
      if (i_taper .eq. 1) then

        alpha = (1.0 - b_taper) / max(1,nx_inp-1)

        do ix_inp = 1 , nx_inp

          x_taper = max(0.,min(1.,b_taper+(ix_inp-1)*alpha))
          x_inp(ix_inp) = x_inp(ix_inp) * x_taper

        enddo    ! do ix_inp = 1 , nx_inp

c  exponential taper
      elseif (i_taper .eq. 2) then    ! if (i_taper .eq. 1) then

c exponential decay factor
        b_taper = max(1.e-3,b_taper)
        alpha = -log(b_taper) / max(1,nx_inp-1)

        do ix_inp = 1 , nx_inp-1

          x_taper = max(0.,min(1.,exp((-nx_inp+ix_inp)*alpha)))
          x_inp(ix_inp) = x_inp(ix_inp) * x_taper

        enddo    ! do ix_inp = 1 , nx_inp-1

c  gaussian taper
      elseif (i_taper .eq. 3) then    ! if (i_taper .eq. 1) then

c exponential decay factor
        b_taper = max(1.e-3,b_taper)
        alpha = -log(b_taper) / max(1,nx_inp-1)**2

        do ix_inp = 1 , nx_inp-1

          x_taper = max(1.,min(0.,exp(-(-nx_inp+ix_inp)**2*alpha)))
          x_inp(ix_inp) = x_inp(ix_inp) * x_taper

        enddo    ! do ix_inp = 1 , nx_inp-1

c  cosine taper
      elseif (i_taper .eq. 4) then    ! if (i_taper .eq. 1) then

        alpha = acos(b_taper) / max(1,nx_inp-1)

        do ix_inp = 1 , nx_inp-1

          x_taper = max(0.,min(1.,cos((nx_inp-ix_inp)*alpha)))
          x_inp(ix_inp) = x_inp(ix_inp) * x_taper

        enddo    ! do ix_inp = 1 , nx_inp-1

      endif    ! if (i_taper .eq. 1) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_compute_taper(i_taper,a_taper,nx_taper,x_taper)
c  compute an exponential taper that varies 
c  from a_taper at ix=1 to 1. at ix=nx_taper
c  i_taper = type of taper 1=liner, 2=exponential, 3=gaussian, 4=cosine
c  nx_taper   = number of taper coefficients >= 1
c  x_taper    = taper coefficients
      implicit  none

      integer  i_taper
      real     a_taper
      integer  nx_taper
      real     x_taper(nx_taper)

      call util_setr(nx_taper,x_taper,1.)

      call util_taper_vector(i_taper,a_taper,nx_taper,x_taper)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_print_taper(title,lu_out
     1,i_taper,a_taper,nx_taper,x_taper)
c  print a taper
      implicit  none

      integer  util_len_r

      character title*(*)
      integer   lu_out

      integer  i_taper
      real     a_taper
      integer  nx_taper
      real     x_taper(nx_taper)

      integer  ix_taper

      if (lu_out .lt. 0 .or. lu_out .gt. 99) return

      write(lu_out,'(/,'' util_print_taper''
     1,/,a
     1,/,'' i_taper='',i8,'' a_taper='',f10.4,'' nx_taper='',i8
     1,/,''    ix       x_taper''
     1)')
     1title(1:util_len_r(title))
     1,i_taper,a_taper,nx_taper
      write(lu_out,'(1x,i8,1x,f10.6)')
     1(ix_taper,x_taper(ix_taper),ix_taper=1,nx_taper)

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_taper_2d(ix,iy,nx,ny,nx_taper,ny_taper,nt,tr)
c  scale an input trace - tr of length nt
c at location ix,iy within a nx,ny space 
c  using taper widths of nx_taper, ny_taper
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer  ix,iy,nx,ny,nx_taper,ny_taper,nt
      real     tr(nt)
      real     x_scale,y_scale
      integer  jx,jy,icall
      data     icall/0/
      icall = icall + 1

      jx = ix
      if (ix .gt. nx/2) jx = nx - ix + 1
      if (nx .gt. nx_taper .and. jx .le. nx_taper) then
        x_scale = float(jx) / float(max(1,nx_taper))
      else
        x_scale = 1.
      endif

      jy = iy
      if (iy .gt. ny/2) jy = ny - iy + 1
      if (ny .gt. ny_taper .and. jy .le. ny_taper) then
        y_scale = float(jy) / float(max(1,ny_taper))
      else
        y_scale = 1.
      endif

      call util_scale(nt,tr,x_scale*y_scale)

c      if (icall .eq. 1) 
c     1if (util_dbg() .ge. 0) write(util_dbg()
c     1,'('' util_taper_2d nx_taper='',i8,'' ny_taper='',i8
c     1,'' nx='',i8,'' ny='',i8)')
c     1 nx_taper,ny_taper,nx,ny

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_apply_taper_3d(
     1 i_edge_x1,i_edge_x2
     1,i_edge_y1,i_edge_y2
     1,i_edge_z1,i_edge_z2
     1,nx_mig,nx_taper,x_taper
     1,ny_mig,ny_taper,y_taper
     1,nz_mig,nz_taper,z_taper
     1,data_0
     1)
c  taper the edges of a 3D cube
c  if i_edge = 0 the edge is NOT tapered
c  note x_taper(nx_taper) = 1. so we do not include that term
      implicit  none

      integer  i_edge_x1,i_edge_x2
      integer  i_edge_y1,i_edge_y2
      integer  i_edge_z1,i_edge_z2

      integer  nx_mig,nx_taper
      real     x_taper(nx_taper)

      integer  ny_mig,ny_taper
      real     y_taper(ny_taper)

      integer  nz_mig,nz_taper
      real     z_taper(nz_taper)

      real     data_0(nz_mig,nx_mig,ny_mig)

      integer  n_dim_1,n_dim_2

c  apply taper in the x direction
      n_dim_1 = nz_mig
      n_dim_2 = ny_mig

      call util_apply_taper_3d_1(
     1 i_edge_x1,i_edge_x2
     1,nx_mig,nx_taper,x_taper
     1,n_dim_1,n_dim_2
     1,data_0
     1)

c  apply taper in the y direction
      n_dim_1 = nz_mig * nx_mig
      n_dim_2 = 1

      call util_apply_taper_3d_1(
     1 i_edge_y1,i_edge_y2
     1,ny_mig,ny_taper,y_taper
     1,n_dim_1,n_dim_2
     1,data_0
     1)

c  apply taper in the z direction
      n_dim_1 = 1
      n_dim_2 = nx_mig * ny_mig

      call util_apply_taper_3d_1(
     1 i_edge_z1,i_edge_z2
     1,nz_mig,nz_taper,z_taper
     1,n_dim_1,n_dim_2
     1,data_0
     1)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_apply_taper_3d_1(
     1 i_edge_x1,i_edge_x2
     1,nx_mig,nx_taper,x_taper
     1,nz_mig,ny_mig
     1,data_0
     1)
c  taper the edges of a 2D slice from a 3D cube
c  if i_edge = 0 the edge is NOT tapered
c  note x_taper(nx_taper) = 1. so we do not include that term
      implicit  none

      integer  i_edge_x1,i_edge_x2

      integer  nx_mig,nx_taper
      real     x_taper(nx_taper)
      integer  ny_mig,nz_mig

      real     data_0(nz_mig,nx_mig,ny_mig)

      integer  ix_taper,lx_taper
      integer  ix_mig_1,ix_mig_2
      integer  iy_mig,iz_mig

      lx_taper = min(nx_mig/2,nx_taper)
      if (lx_taper .le. 1) return

c  we have separate loops for when both edges are tapered or only one
c  we also if test the loop lengths so we put the longest on the inside
c  note for the special case of 2D no tapering is done in the y direction 
c  because ny_mig=1

c  taper both edges
      if (i_edge_x1 .ne. 0 .and. i_edge_x2 .ne. 0) then

c  loop over distance from edge - taper coefficients
        do ix_taper = 1 , lx_taper-1

          ix_mig_1 = ix_taper
          ix_mig_2 = nx_mig - ix_taper + 1

c  nz_mig is the longer loop
          if (nz_mig .ge. ny_mig) then

            do iy_mig = 1 , ny_mig

              do iz_mig = 1 , nz_mig

                data_0(iz_mig,ix_mig_1,iy_mig) = 
     1          data_0(iz_mig,ix_mig_1,iy_mig) * x_taper(ix_taper)

                data_0(iz_mig,ix_mig_2,iy_mig) = 
     1          data_0(iz_mig,ix_mig_2,iy_mig) * x_taper(ix_taper)

              enddo    ! do iz_mig = 1 , nz_mig
  
            enddo    ! do iy_mig = 1 , ny_mig

c  ny_mig is the longer loop
          else    ! if (nz_mig .ge. ny_mig) then

            do iz_mig = 1 , nz_mig

              do iy_mig = 1 , ny_mig

                data_0(iz_mig,ix_mig_1,iy_mig) = 
     1          data_0(iz_mig,ix_mig_1,iy_mig) * x_taper(ix_taper)

                data_0(iz_mig,ix_mig_2,iy_mig) = 
     1          data_0(iz_mig,ix_mig_2,iy_mig) * x_taper(ix_taper)

              enddo    ! do iy_mig = 1 , ny_mig

            enddo    ! do iz_mig = 1 , nz_mig

          endif    ! if (nz_mig .ge. ny_mig) then

        enddo    ! do ix_taper = 1 , lx_taper-1

c  taper either edge 1 or edge 2 only
      elseif (i_edge_x1 .ne. 0 .or. i_edge_x2 .ne. 0) then    ! if (i_edge_x1 .ne. 0 .and. 

c  loop over distance from edge - taper coefficients
        do ix_taper = 1 , lx_taper-1

c  taper edge 1 only
          if (i_edge_x1 .ne. 0) then

            ix_mig_1 = ix_taper

c  taper edge 2 only
          elseif (i_edge_x2 .ne. 0) then    ! if (i_edge_x1 .ne. 0) then

            ix_mig_1 = nx_mig - ix_taper + 1

          endif    ! if (i_edge_x1 .ne. 0) then

c  nz_mig is the longer loop
          if (nz_mig .ge. ny_mig) then

            do iy_mig = 1 , ny_mig

              do iz_mig = 1 , nz_mig

                data_0(iz_mig,ix_mig_1,iy_mig) = 
     1          data_0(iz_mig,ix_mig_1,iy_mig) * x_taper(ix_taper)

              enddo    ! do iz_mig = 1 , nz_mig
  
            enddo    ! do iy_mig = 1 , ny_mig

c  ny_mig is the longer loop
          else    ! if (nz_mig .ge. ny_mig) then

            do iz_mig = 1 , nz_mig

              do iy_mig = 1 , ny_mig

                data_0(iz_mig,ix_mig_1,iy_mig) = 
     1          data_0(iz_mig,ix_mig_1,iy_mig) * x_taper(ix_taper)

              enddo    ! do iy_mig = 1 , ny_mig

            enddo    ! do iz_mig = 1 , nz_mig

          endif    ! if (nz_mig .ge. ny_mig) then

        enddo    ! do ix_taper = 1 , lx_taper-1

      endif    ! ! if (i_edge_x1 .ne. 0 .and. i_edge_x2 .ne. 0) then

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_compute_operator_n(title
     1,no_op
     1,nx_op,x_op,ax_op,dx_mig
     1,m_work,work
     1,i_err
     1)
c  compute the optimal finite difference oeprator in the x directions
c  title  = character string printed with information
c  no_op  = Order of derivative to compute operator for. (2=acoustic wave eq.)
c  nx_op  = operator half length, the total operator length is 2 * nx_op - 1
c  x_op   = derived operator
c  ax_op  = Fraction of Nyquist for accuracy of operator.
c  dx_mig = spatial increment.  The operator is scaled by 1./dx_mig**2
c  m_work = length of work array >= (nx_op+no_op)*(nx_op+no_op+1) +2*nx_op-1
c  work   = work array
c  i_err  = error flag 0=o.k. -1 = error condition

      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      character title*(*)

      integer  no_op

      real     ax_op
      integer  nx_op
      real     x_op(nx_op,nx_op),dx_mig

      integer  m_work
      real     work(m_work)

      integer  i_err

      integer  ix_op,jx_op

      i_err = 0

      call util_setr(nx_op**2,x_op,0.)

c  nx_op=1 is the special condition of a 2D migration 
c  and we want the central y term to be 0
      do ix_op = 1 , nx_op

        jx_op = nx_op - ix_op + 1
        call util_compute_operator('NONE'
     1,no_op
     1,jx_op,x_op(1,ix_op),ax_op,dx_mig
     1,m_work,work
     1,i_err
     1)
        if (i_err .ne. 0) goto 998

      enddo    ! do ix_op = 1 , nx_op

      call util_print_operator_n(title,6
     1,no_op
     1,nx_op,x_op,ax_op,dx_mig
     1)

      return

  998 continue
      write(util_err(),'(/,'' error in util_compute_operator_n''
     1,/,'' need more work space for computing operator in FINDIFOP''
     1,/,'' have='',i8,'' and need='',i8
     1,/,''= (LENGTH+ORDER)*(LENGTH+ORDER+1)  elements.''
     1)')
     1 m_work,(no_op+nx_op)*(no_op+nx_op+1)
      goto 999

  999 continue
      write(util_err()
     1,'(/,'' error in util_compute_operator_n''
     1,/,'' no_op='',i8
     1,/,'' nx_op='',i8,'' ax_op='',f10.4,'' dx_mig='',f10.4
     1)')
     1 no_op
     1,nx_op,ax_op,dx_mig
      i_err = -1

      return

      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_print_operator_n(title,lu_out
     1,no_op
     1,nx_op,x_op,ax_op,dx_mig
     1)
c  print the optimal finite difference oeprator in the x direction
c  the total operator length is 2 * nx_op - 1
      implicit  none

      integer  util_len_r

      character title*(*)
      integer   lu_out

      integer  no_op

      real     ax_op
      integer  nx_op
      real     x_op(nx_op,nx_op),dx_mig

      integer  ix_op,jx_op

      if (lu_out .lt. 0 .or. lu_out .gt. 99
     1.or. title(1:4) .eq. 'none' .or. title(1:4) .eq. 'NONE') return

      write(lu_out,'(/,'' util_print_operator_n''
     1,/,a
     1,/,'' no_op='',i8
     1,/,'' nx_op='',i8,'' ax_op='',f10.4,'' dx_mig='',f10.4
     1,/,''        j       i   operator         x_op''
     1)')
     1title(1:util_len_r(title))
     1,no_op
     1,nx_op,ax_op,dx_mig

      do ix_op = 1 , nx_op

        write(lu_out,'(1x,i8,1x,i8,1x,g16.9,1x,g16.9)')
     1(ix_op,jx_op,x_op(jx_op,ix_op)*dx_mig**2,x_op(jx_op,ix_op)
     1,jx_op=1,nx_op-ix_op+1)

      enddo    ! do jx_op = 1 , nx_op

      return

      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_compute_operator(title
     1,no_op
     1,nx_op,x_op,ax_op,dx_mig
     1,m_work,work
     1,i_err
     1)
c  compute the optimal finite difference oeprator in the x directions
c  title  = character string printed with information
c  no_op  = Order of derivative to compute operator for. (2=acoustic wave eq.)
c  nx_op  = operator half length, the total operator length is 2 * nx_op - 1
c  x_op   = derived operator
c  ax_op  = Fraction of Nyquist for accuracy of operator.
c  dx_mig = spatial increment.  The operator is scaled by 1./dx_mig**2
c  m_work = length of work array >= (nx_op+no_op)*(nx_op+no_op+1) +2*nx_op-1
c  work   = work array
c  i_err  = error flag 0=o.k. -1 = error condition

      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      character title*(*)

      integer  no_op

      real     ax_op
      integer  nx_op
      real     x_op(nx_op),dx_mig

      integer  m_work
      real     work(m_work)

      integer  i_err

      integer  mx_op
      integer  i_work_1,i_work_2
      integer  i

      i_err = 0

c  nx_op=1 is the special conditiyon of a 2D migration 
c  and we want the central y term to be 09
      if (nx_op .le. 1) then

        x_op(1) = 0.

      else    ! if (nx_op .eq. 1) then

        mx_op = 2 * nx_op - 1

c  make sure the work aray is long enough
        if (m_work .lt. (no_op+mx_op)*(no_op+mx_op+1) + mx_op) 
     1goto 998

        i_work_1 = 1
        i_work_2 = mx_op + 1

C--Primitive FINDIFOP computes the F.D. operator
C--Scale 2nd derivative operator to the grid size

c  initialize the spatial derivitive operator coefficients to zero
        call util_setr(mx_op,work(i_work_1),0.)

c  compute the operator coefficients
c       call findifop(no_op,mx_op,ax_op
c    1,work(i_work_2),work(i_work_1),*999)

c  copy the work operator coefficients to the x operator coefficients
c  because the operator is symetric we keep only the center to the end
        call util_copy(nx_op,work(i_work_1+nx_op-1),x_op)

c  scale the spatial operator coefficients by the spatial increments squared
        call util_scale(nx_op,x_op,1./dx_mig**2)

      endif    ! if (nx_op .eq. 1) then

      call util_print_operator(title,6
     1,no_op
     1,nx_op,x_op,ax_op,dx_mig
     1)

      return

  998 continue
      write(util_err(),'(/,'' error in util_compute_operator''
     1,/,'' need more work space for computing operator in FINDIFOP''
     1,/,'' have='',i8,'' and need='',i8
     1,/,''= (LENGTH+ORDER)*(LENGTH+ORDER+1)  elements.''
     1)')
     1 m_work,(no_op+nx_op)*(no_op+nx_op+1)
      goto 999

  999 continue
      write(util_err()
     1,'(/,'' error in util_compute_operator''
     1,/,'' no_op='',i8
     1,/,'' nx_op='',i8,'' ax_op='',f10.4,'' dx_mig='',f10.4
     1)')
     1 no_op
     1,nx_op,ax_op,dx_mig
      i_err = -1

      return

      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_print_operator(title,lu_out
     1,no_op
     1,nx_op,x_op,ax_op,dx_mig
     1)
c  print the optimal finite difference oeprator in the x direction
c  the total operator length is 2 * nx_op - 1
      implicit  none

      integer  util_len_r

      character title*(*)
      integer   lu_out

      integer  no_op

      real     ax_op
      integer  nx_op
      real     x_op(nx_op),dx_mig

      integer  ix_op

      if (lu_out .lt. 0 .or. lu_out .gt. 99
     1.or. title(1:4) .eq. 'none' .or. title(1:4) .eq. 'NONE') return

      write(lu_out,'(/,'' util_print_operator''
     1,/,a
     1,/,'' no_op='',i8
     1,/,'' nx_op='',i8,'' ax_op='',f10.4,'' dx_mig='',f10.4
     1,/,''        i   operator         x_op''
     1)')
     1title(1:util_len_r(title))
     1,no_op
     1,nx_op,ax_op,dx_mig

      write(lu_out,'(1x,i8,1x,g16.9,1x,g16.9)')
     1(ix_op,x_op(ix_op)*dx_mig**2,x_op(ix_op),ix_op=1,nx_op)

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_ftoi(nx,x0,dx,ny,y)
c  convert the floating point array y to the integer array y
      implicit  none

      integer  ny
      real     y(ny)

      integer  nx
      real     x0,dx

      integer  ix,iy

      do iy = 1 , ny

        ix = max(1,min(nx,nint((y(iy)-x0)/dx)+1))
        call util_copi(1,ix,y(iy))

      enddo    ! do iy = 1 , ny

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_itof(nx,x0,dx,ny,y)
c  convert the integer y to the floating point array array y
      implicit  none

      integer  ny
      real     y(ny)

      integer  nx
      real     x0,dx

      integer  ix,iy

      do iy = 1 , ny

        call util_copi(1,y(iy),ix)
        y(iy) = (ix - 1) * dx + x0

      enddo    ! do iy = 1 , ny

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_flip_vector(nx,x)
c  flip the order of a vector
      implicit  none
      integer nx
      real    x(nx)

      integer ix,jx
      real    x0

      jx = nx

      do ix = 1 , nx/2

        x0    = x(ix)
        x(ix) = x(jx)
        x(jx) = x0
        jx    = jx - 1

      enddo    ! do ix = 1 , nx/2

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_inside_polygon(x0,y0,nx_poly,x_poly,y_poly,inside)
c  determine if a point x0,y0 is inside a closed polygon x,y
c  inside inside = 1  outside inside = 0
      implicit  none

      real     x0,y0

      integer  nx_poly
      real     x_poly(nx_poly),y_poly(nx_poly)

      integer  inside 

      integer  n_above,n_below,i_ab
      integer  ix_poly,jx_poly
      real     y1

      inside = 0
      n_above = 0
      n_below = 0

      do ix_poly = 1 , nx_poly-1

        if (x0 .eq. x_poly(ix_poly) 
     1.and. y0 .eq. y_poly(ix_poly)) then

          inside = 1
          goto 1

        endif    ! if (x0 .eq. x_poly(ix_poly) 

        if (x0 .ge. min(x_poly(ix_poly),x_poly(ix_poly+1))
     1.and. x0 .le. max(x_poly(ix_poly),x_poly(ix_poly+1))) then

          if (x_poly(ix_poly) .eq. x_poly(ix_poly+1)) then

            if (y0 .lt. min(y_poly(ix_poly),y_poly(ix_poly+1))) then

              n_above = n_above + 2

            elseif (y0 .gt. max(y_poly(ix_poly),y_poly(ix_poly+1))) then

              n_below = n_below + 2

            elseif (y0 .ge. min(y_poly(ix_poly),y_poly(ix_poly+1))
     1        .and. y0 .le. max(y_poly(ix_poly),y_poly(ix_poly+1))) then

              inside = 1
              goto 1

            endif

          else    ! if (x_poly(ix_poly) .eq. x_poly(ix_poly+1)) then

            y1 = y_poly(ix_poly)+(x0-x_poly(ix_poly))
     1*(y_poly(ix_poly+1)-y_poly(ix_poly))
     1/(x_poly(ix_poly+1)-x_poly(ix_poly))

            jx_poly = mod(ix_poly+1,nx_poly-1) + 1
            i_ab = 1

            if (x0 .eq. x_poly(ix_poly+1) 
     1.and. x_poly(ix_poly+1) .gt. min(x_poly(ix_poly),x_poly(jx_poly)) 
     1.and. x_poly(ix_poly+1) .lt. max(x_poly(ix_poly),x_poly(jx_poly))) 
     1i_ab = 0

            if (y1 .eq. y0) then

              inside = 1
              goto 1

            elseif (y1 .lt. y0) then

              n_above = n_above + i_ab

            else

              n_below = n_below + i_ab

            endif    ! if (y1 .eq. y0) then

          endif    ! if (x_poly(ix_poly) .eq. x_poly(ix_poly+1)) then

        endif    ! if (x0 .ge. min(x_poly(ix_poly),x_poly(ix_poly+1))

      enddo ! do ix_poly = 1 , nx_poly-1

      if (mod(n_above,2) .eq. 1) inside = 1

    1 continue

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_create_unique_file_name(fn_default,fn_data)
c  create a unique file name, fn_data 
c  incorporateing the logical unit number, lu_data
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer   util_r

      character fn_default*(*)
      character fn_data*(*)
      integer   lu_data
      integer   ln_data

      lu_data = 1

      if (fn_default(1:4) .eq. 'NONE'
     1.or. fn_default(1:4) .eq. 'none') then
        fn_data = 'util.data'
      else
        fn_data = fn_default
      endif

      ln_data = util_r(fn_data) + 1

      if (lu_data .lt. 0) then

      elseif (lu_data .lt. 10) then

        write(fn_data(ln_data:ln_data),'(i1)')lu_data

      elseif (lu_data .lt. 100) then

        write(fn_data(ln_data:ln_data+1),'(i2)')lu_data

      endif

      if (util_dbg() .ge. 0) 
     1write(util_dbg(),'(/,'' util_create_unique_file_name''
     1,/,'' lu_data='',i8
     1,/,'' fn_data='',a)')
     1 lu_data,fn_data(1:util_r(fn_data))

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_open_disk_file(lu_data,fn_default,status
     1,nx_mig,ny_fft,m_head,head,m_data,data,i_err)
c  open and initialize a direct access disk file for the fft data

      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer   util_r

      integer   lu_data
      character fn_default*(*)
      character status*(*)

      integer  nx_mig,ny_fft

      integer  m_head
      real     head(m_head)

      integer  m_data
      real     data(m_data)

      integer  i_err

      character fn_data*80
      character form*10

      integer  n_recl
      integer  i_rec
      i_err = 0

      if (util_wln() .eq. 8) then

        n_recl = (m_head + m_data) * 8

      else    ! if (util_wln() .eq. 8) then

         n_recl = (m_head + m_data)

      endif    ! if (util_wln() .eq. 8) then

      lu_data = 0

      if (util_dbg() .ge. 0)
     1write(util_dbg(),'(/,'' util_open_disk_file''
     1,/,'' fn_default='',a
     1,/,'' status    ='',a
     1,/,'' lu_data   ='',i8,'' n_recl    ='',i8
     1,/,'' m_head    ='',i8,'' m_data    ='',i8
     1,/,'' nx_mig    ='',i8,'' ny_fft    ='',i8
     1)')
     1 fn_default(1:util_r(fn_default))
     1,status(1:util_r(status))
     1,lu_data,n_recl
     1,m_head,m_data,nx_mig,ny_fft

c  create a unique file name
      call util_create_unique_file_name(fn_default,fn_data)

      if (util_dbg() .ge. 0) 
     1write(util_dbg(),'(/,'' lu_data='',i8,'' fn_data='',a)')
     1 lu_data,fn_data(1:util_r(fn_data))

c  open the disk file with the default status
      form = 'unformatted'
      call util_open_file(lu_data,fn_data,status,form,n_recl,i_err)
      if (i_err .ne. 0) goto 997

c  zero the disk file
      call util_zero_disk_file(lu_data
     1,nx_mig,ny_fft,m_head,head,m_data,data,i_err)
      if (i_err .ne. 0) goto 996

      if (util_dbg() .ge. 0)
     1write(util_dbg(),'(/,'' util_open_disk_file''
     1,/,'' fn_default='',a
     1,/,'' status    ='',a
     1,/,'' lu_data   ='',i8,'' n_recl    ='',i8
     1,/,'' m_head    ='',i8,'' m_data    ='',i8
     1,/,'' nx_mig    ='',i8,'' ny_fft    ='',i8
     1)')
     1 fn_default(1:util_r(fn_default))
     1,status(1:util_r(status))
     1,lu_data,n_recl
     1,m_head,m_data,nx_mig,ny_fft

      return

  996 continue
      write(util_err(),'(/,'' error in util_open_disk_file''
     1,/,'' during util_zero_disk_file'')')
      goto 999

  997 continue
      write(util_err(),'(/,'' error in util_open_disk_file''
     1,/,'' during open'')')
      goto 999

  998 continue
      write(util_err(),'(/,'' error in util_open_disk_file''
     1,/,'' during getlun'')')
      goto 999

  999 continue
      write(util_err(),'(/,'' error in util_open_disk_file''
     1,/,'' fn_default='',a
     1,/,'' status    ='',a
     1,/,'' lu_data   ='',i8,'' n_recl    ='',i8
     1,/,'' m_head    ='',i8,'' m_data    ='',i8
     1,/,'' nx_mig    ='',i8,'' ny_fft    ='',i8
     1)')
     1 fn_default(1:util_r(fn_default))
     1,status(1:util_r(status))
     1,lu_data,n_recl
     1,m_head,m_data,nx_mig,ny_fft
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_close_disk_file(lu_data)
c  close a direct access disk file

      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer  lu_data

      if (util_dbg() .ge. 0) 
     1write(util_dbg(),'(/,'' util_close_disk_file''
     1,/,'' lu_data='',i8
     1)')
     1 lu_data

c  close the disk file
      close(lu_data,status='delete')

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_zero_disk_file(lu_data
     1,nx_mig,ny_fft,m_head,head,m_data,data,i_err)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer  lu_data

      integer  nx_mig,ny_fft

      integer  m_head
      real     head(m_head)

      integer  m_data
      real     data(m_data)

      integer  i_err

      integer  i_rec

      i_err = 0

      if (util_dbg() .ge. 0) 
     1write(util_dbg(),'(/,'' util_zero_disk_file''
     1,/,'' lu_data='',i8
     1,/,'' m_head='',i8,'' m_data='',i8
     1,/,'' nx_mig='',i8,'' ny_fft='',i8
     1)')
     1 lu_data,m_head,m_data,nx_mig,ny_fft

c  initialize the file to zero
      call util_setr(m_head,head,0.)
      call util_setr(m_data,data,0.)

      do i_rec = 1 , nx_mig * ny_fft

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' util_zero i_rec='',i8)')i_rec
        call util_write_data(lu_data,1,i_rec,1
     1,m_head,m_head,head,m_data,m_data,data,i_err)
        if (i_err .ne. 0) goto 999

      enddo    ! do i_rec = 1 , nx_mig * ny_fft

      return

  999 continue
      write(util_err(),'(/,'' error in util_zero_disk_file''
     1,/,'' during write'')')
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_read_data(lu_data,n_inp,i_rec_0,i_rec_skip
     1,m_head,n_head,head,m_data,n_data,data,i_err)

      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      real    util_amax

      integer lu_data

      integer n_inp
      integer i_rec_0,i_rec_skip

      integer m_head,n_head
      real    head(m_head,1)

      integer m_data,n_data
      real    data(m_data,1)

      integer i_err

      integer i_rec,i_inp,i_head,i_data

      i_err = 0

      do i_inp = 1 , n_inp

        i_rec = (i_inp - 1) * i_rec_skip + i_rec_0

        read(lu_data,rec=i_rec,err=999)
     1 (head(i_head,i_inp),i_head=1,n_head)
     1,(data(i_data,i_inp),i_data=1,n_data)
        if (i_err .ne. 0) goto 999

c      if (util_dbg() .ge. 0) write(util_dbg()
c     1,'('' util_read   n='',i8,'' i_rec='',i8,'' i_inp='',i8
c     1,'' a='',g16.9)')
c     1 n_data,i_rec,i_inp,util_amax(n_data,data(1,i_inp))

      enddo    ! do i_inp = 1 , n_inp

      return

  999 continue
      write(util_err()
     1,'(/,'' error in util_read_data''
     1,/,'' lu_data='',i8
     1,'' m_head='',i8,'' n_head='',i8
     1,'' m_data='',i8,'' n_data='',i8)')
     1 lu_data,m_head,n_head,m_data,n_data
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_write_data(lu_data
     1,n_out,i_rec_0,i_rec_skip
     1,m_head,n_head,head
     1,m_data,n_data,data
     1,i_err)
c  write out n_out records starting at i_rec_0 with a do skip of i_rec_skip
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      real    util_amax

      integer lu_data

      integer n_out
      integer i_rec_0,i_rec_skip

      integer m_head,n_head
      real    head(m_head,1)

      integer m_data,n_data
      real    data(m_data,1)

      integer i_err

      integer i_rec,i_out,i_head,i_data

      i_err = 0

      do i_out = 1 , n_out

        i_rec = (i_out - 1) * i_rec_skip + i_rec_0

c      if (util_dbg() .ge. 0) write(util_dbg()
c     1,'('' util_write l='',i2,'' h='',i6,'' n='',i6
c     1,'' r='',i6,'' o='',i6,'' a='',g12.5)')
c     1 lu_data,n_head,n_data,i_rec,i_out
c     1,util_amax(n_data,data(1,i_out))

        write(lu_data,rec=i_rec,err=999)
     1 (head(i_head,i_out),i_head=1,n_head)
     1,(data(i_data,i_out),i_data=1,n_data)
        if (i_err .ne. 0) goto 999

      enddo    ! do i_out = 1 , n_out

      return

  999 continue
      write(util_err()
     1,'(/,'' error in util_write_data''
     1,/,'' lu_data='',i8
     1,'' m_head='',i8,'' n_head='',i8
     1,'' m_data='',i8,'' n_data='',i8)')
     1 lu_data,m_head,n_head,m_data,n_data
      i_err = -1
      return

      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_trace_location(n_dim
     1,ix_inp,x_inp,x0_mig,dx_mig
     1,iy_inp,y_inp,y0_mig,dy_mig
     1)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer  n_dim

      integer  ix_inp
      real     x_inp,x0_mig,dx_mig

      integer  iy_inp
      real     y_inp,y0_mig,dy_mig

c  get trace location
      ix_inp = nint((x_inp-x0_mig)/dx_mig) + 1
      iy_inp = nint((y_inp-y0_mig)/dy_mig) + 1
      if (n_dim .eq. 2) iy_inp = 1

c      if (util_dbg() .ge. 0) write(util_dbg()
c     1,'('' util_trace_location n_dim='',i2,'' ix='',i8,'' nx='',i8
c     1,'' x='',f10.2,'' x0='',f10.2,'' dx='',f10.4)')
c     1 n_dim,ix,nx,x,x0,dx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_poly_cross(x0,n,x,z,nc,zc,work)
c  find the crossing points in the closed polygon x,z at x0
c  work requires 2 * n points
      implicit  none

      real     x0

      integer  n
      real     x(1),z(1)

      integer  nc
      real     zc(1)

      real     work(1)

      integer  i,im,ip
      real     dx1,dx2

      nc = 0

      do i = 2 , n

        im = i - 1
        ip = mod(i-1,n-1) + 2
        dx1 = x(i) - x(im)
        dx2 = x(ip) - x(i)

c  if x0 falls between 2 x points
c  or it falls on this point and the next point count this as a crossing

        if ((x0 .eq. x(i) .and. dx1*dx2 .ge. 0. 
     1.and. abs(dx1)+abs(dx2) .ne. 0.)
     1.or. (x0.gt.min(x(i),x(ip)) .and. x0.lt.max(x(i),x(ip)))) then

          nc = nc + 1

          if (x(i)  .eq. x(ip)) then

            zc(nc) = z(i)

          else    ! if (x(i)  .eq. x(ip)) then

            zc(nc) = z(i) + (x0 - x(i)) 
     1* (z(ip) - z(i)) / (x(ip) - x(i))

          endif    ! if (x(i)  .eq. x(ip)) then

        endif    ! if ((x0 .eq. x(i) .and. dx1*dx2 .ge. 0. 

      enddo    ! do i = 2 , n

      call util_sort(nc,zc,work)
      call util_ord(nc,zc,work(1),work(nc+1))

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_ord(n,x,i,y)
c  order a set of points according to an index i
      implicit  none

      integer  n
      integer  i(1)
      real     x(1),y(1)

      integer  j

      do j = 1 , n

        y(j) = x(j)

      enddo    ! do j = 1 , n

      do j = 1 , n

        x(j) = y(i(j))

      enddo    ! do j = 1 , n

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_sort(n,x,i)
c  determine the index i of x in icreasing order
      implicit  none
      integer  n
      real    x(1)
      integer i(1)

      integer j,l,i0,i1,i2
      real    x0

      do j  =  1 , n

        i(j) = j

      enddo    ! do j  =  1 , n

      if (n .le. 1) return

      l = n / 2 + 1
      i1 = n

    2 continue

        if (l .gt. 1) then

          l = l - 1
          i2 = i(l)
          x0 = x(i2)

        else    ! if (l .gt. 1) then

          i2 = i(i1)
          x0 = x(i2)
          i(i1) = i(1)
          i1 = i1 - 1

          if (i1 .eq. 1) then

            i(1) = i2
            return

          endif    ! if (i1 .eq. 1) then

        endif    ! if (l .gt. 1) then

        i0 = l
        j = l + l

    3   continue

        if (j .le. i1) then

          if (j .lt. i1) then

            if (x(i(j)) .lt. x(i(j+1))) j = j + 1

          endif    ! if (j .lt. i1) then

          if (x0 .lt. x(i(j))) then

            i(i0) = i(j)
            i0 = j
            j = j + j

          else    ! if (x0 .lt. x(i(j))) then

            j = i1 + 1

          endif    ! if (x0 .lt. x(i(j))) then

        goto 3

        endif    ! if (j .le. i1) then

        i(i0) = i2

      goto 2

      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_ctoi(i_inp,i_out)
c  convert an input character to an integer form
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer  i_inp,i_out
      i_out = i_inp
c      if (util_dbg() .ge. 0) write(util_dbg()
c     1,'(/,'' util_ctoi i_inp='',a8,'' i_out='',a8)')i_inp,i_out
      return
      end

c&&&
C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_scale_fos(
     1 jt_fos,at_fos
     1,nt_fos,tr_fos
     1,nt_mig,tr_mig
     1)
c  scale trace by fold of stack correction

      implicit  none

      integer  jt_fos
      real     at_fos

      integer  nt_fos
      real     tr_fos(1)

      integer  nt_mig
      real     tr_mig(nt_mig)

      integer  it_mig,it_fos
      integer  it_fos_1,it_fos_2
      real     ft_fos_1,ft_fos_2
      real     t0_fos,dt_fos

      real     tr_scale
      real     eps_fos

      eps_fos = 1.e-6

      if (nt_fos .eq. 0) then
c  do nothing for nt_fos = 0

      elseif (nt_fos .eq. 1) then

        if (tr_fos(1) .gt. 0.) 
     1call util_scale(nt_mig,tr_mig,1./tr_fos(1)**at_fos)

      elseif (nt_fos .gt. 1) then    ! if (nt_fos .eq. 0) then

        t0_fos = 1. + eps_fos
        dt_fos = 1. / jt_fos

        do it_mig = 1 , nt_mig
          
          it_fos_1 = min(nt_fos,max(1,int(t0_fos)))
          it_fos_2 = min(nt_fos,it_fos_1+1)
          ft_fos_2 = max(0.,min(1.,t0_fos-it_fos_1))
          ft_fos_1 = 1. - ft_fos_2
          t0_fos   = t0_fos + dt_fos
          tr_scale = ft_fos_1 * tr_fos(it_fos_1)
     1             + ft_fos_2 * tr_fos(it_fos_2)
          if (tr_scale .gt. 0.) tr_scale = (1. / tr_scale) ** at_fos
          tr_mig(it_mig) = tr_scale * tr_mig(it_mig)
          
        enddo    ! do it_mig = 1 , nt_mig

      endif    ! if (nt_fos .eq. 0) then

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_scale_median(nx_inp,x_inp)
c  scale trace by fold of stack correction

      implicit  none

      integer  nx_inp
      real     x_inp(nx_inp)

      real     x_med,x_ave

c  scale by the median value in the trace
      call util_ave_abs(nx_inp,1,x_inp,x_ave)
      call util_median(x_inp,nx_inp,x_med,x_ave)
      if (x_med .ne. 0.) call util_scale(nx_inp,x_inp,1./x_med)

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_scale_tpow(tpow,nt,t0,dt,tr)
c  scale by 1 / t**tpow
      implicit  none
      integer   nt
      real      t0,dt
      real      tpow,tr(nt)

      integer   it
      real      t
      real      t_eps

      t_eps = .01 * dt

      if (tpow .ne. 0.) then

        do it = 1 , nt

          t = max(t_eps,abs((it-1)*dt+t0))
          tr(it) = tr(it) * t ** tpow

        enddo    ! do it = 1 , nt

      endif

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_scale_f(f_mig,f_scale,f_lim)
c  determine the amplitude scaling factor for this frequency

      implicit  none
      real     f_mig,f_scale,f_lim(4)

      f_scale = 1.

      if (f_mig .lt. f_lim(1) .or. f_mig .gt. f_lim(4)) then

        f_scale = 0.

      elseif (f_mig .ge. f_lim(2) .and. f_mig .le. f_lim(3)) then

        f_scale = 1.

      elseif (f_mig .ge. f_lim(1) .and. f_mig .le. f_lim(2)
     1  .and. f_lim(1) .ne. f_lim(2)) then

        f_scale = (f_mig - f_lim(1)) / (f_lim(2) - f_lim(1))

      elseif (f_mig .ge. f_lim(3) .and. f_mig .le. f_lim(4)
     1  .and. f_lim(3) .ne. f_lim(4)) then

        f_scale = (f_mig - f_lim(3)) / (f_lim(4) - f_lim(3))

      endif

      f_scale = max(0.,min(1.,f_scale))

      return

      end

C23456789012345678901234567890123456789012345678901234567890123456789012
c       subroutine util_median
C***************************** COPYRIGHT NOTICE ********************************
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C\USER DOC
C-----------------------------------------------------------------------
C                        CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                               CONOCO INC.
C
C                        C P S   P R I M I T I V E
C
CPrimitive name:  MEDIAN
C        Author:  Bob Baumel  (earlier version by Mike Howard)
C  Last revised:  89/06/29
C
C  Purpose:  Find the median value of a list of unsorted values.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C       CALL MEDIAN (ARRAY,N,AMED [,GUESS] )
C
C Name   Type*   Valid     Description      *Type: I=IN, O=OUT, B=BOTH
C ----   ----    -----     -----------
C ARRAY    I   real-array  Array of values for which median is desired.
C N        I     int>=0    Number of values in ARRAY.
C AMED     O      real     Returned median value.
C GUESS    I      real     OPTIONAL initial guess for the median.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. This routine uses an iterative method to find the median.  Given
C    a guess for the median, it counts the numbers of values bigger and
C    smaller than the guess, and uses this data to update the guess.
C
C 2. The algorithm USUALLY converges to the median in order LOG N iter-
C    ations, but for "pathologically" distributed data, it COULD take
C    as many as N-1 iterations.
C
C 3. You may optionally specify a starting guess for the iteration by
C    specifying a 4th argument to the subroutine.  If you call the
C    routine with only 3 arguments, the starting guess will be the
C    average of the biggest and smallest values in ARRAY.
C
C 4. The current version of this routine is based loosely on an earlier
C    version by Mike Howard which was modified from an algorithm in the
C    book "Numerical Recipes" by William H. Press, et al.  The algorithm
C    has now been extensively reworked and no longer includes the book's
C    original iteration formula!  The previous version of this routine
C    did not always return the correct answer!
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C 89/06/29  Bob Baumel   New Algorithm.
C 89/03/22  Harold Ball  Moved from OLD CPS to current system.
C 88/05/12  Mike Howard  OLD CPS version.
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES IN THIS MODULE
C     Subroutine:  MEDIAN
C-----------------------------------------------------------------------
C                  EXTERNALS REFERENCED BY THIS MODULE
C  UTIL_ISMIN   UTIL_ISMAX   NUMARG
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C  STORAGE       - 0
C  HEAP(dynamic) - 0
C-----------------------------------------------------------------------
C\END DOC
       subroutine util_median(array,n,amed,guess)
       real array(n)
       if (n.eq.0) then
         amed = 0.
         return
       endif
       i1 = util_ismin(n,array,1)
       a1 = array(i1)
       i2 = util_ismax(n,array,1)
       a2 = array(i2)
       if (a1.eq.a2) then
         amed = a1
         return
       endif
       if (numarg().eq.4) then
         aa = guess
       else
         aa = (a1+a2) * 0.5
       endif
       n1 = 1
       n2 = n
  10   continue
       nle = 0
       x1 = a1
       x2 = a2
       do 20 j=1,n
         if (array(j).le.aa) then
           nle = nle + 1
           x1 = max(x1,array(j))
         else
           x2 = min(x2,array(j))
         endif
  20   continue
       nle2 = 2 * nle
       if (nle2.lt.n-1) then
         a1 = x2
         n1 = nle + 1
         goto 30
       elseif (nle2.gt.n+1) then
         a2 = x1
         n2 = nle
         goto 30
       elseif (nle2.eq.n) then
         amed = (x1+x2) * 0.5
       elseif (nle2.lt.n) then
         amed = x2
       else
         amed = x1
       endif
       return
  30   if (a1.eq.a2) then
         amed = a1
         return
       endif
       aa = a1 + (a2-a1)*real(n+1-2*n1)/real(2*(n2-n1))
       goto 10
       end

C23456789012345678901234567890123456789012345678901234567890123456789012
c       subroutine util_median_abs
C***************************** COPYRIGHT NOTICE ********************************
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C\USER DOC
C-----------------------------------------------------------------------
C                        CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                               CONOCO INC.
C
C                        C P S   P R I M I T I V E
C
CPrimitive name:  MEDIAN
C        Author:  Bob Baumel  (earlier version by Mike Howard)
C  Last revised:  89/06/29
C
C  Purpose:  Find the median value of a list of unsorted values.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C       CALL MEDIAN (ARRAY,N,AMED [,GUESS] )
C
C Name   Type*   Valid     Description      *Type: I=IN, O=OUT, B=BOTH
C ----   ----    -----     -----------
C ARRAY    I   real-array  Array of values for which median is desired.
C N        I     int>=0    Number of values in ARRAY.
C AMED     O      real     Returned median value.
C GUESS    I      real     OPTIONAL initial guess for the median.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. This routine uses an iterative method to find the median.  Given
C    a guess for the median, it counts the numbers of values bigger and
C    smaller than the guess, and uses this data to update the guess.
C
C 2. The algorithm USUALLY converges to the median in order LOG N iter-
C    ations, but for "pathologically" distributed data, it COULD take
C    as many as N-1 iterations.
C
C 3. You may optionally specify a starting guess for the iteration by
C    specifying a 4th argument to the subroutine.  If you call the
C    routine with only 3 arguments, the starting guess will be the
C    average of the biggest and smallest values in ARRAY.
C
C 4. The current version of this routine is based loosely on an earlier
C    version by Mike Howard which was modified from an algorithm in the
C    book "Numerical Recipes" by William H. Press, et al.  The algorithm
C    has now been extensively reworked and no longer includes the book's
C    original iteration formula!  The previous version of this routine
C    did not always return the correct answer!
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C 89/06/29  Bob Baumel   New Algorithm.
C 89/03/22  Harold Ball  Moved from OLD CPS to current system.
C 88/05/12  Mike Howard  OLD CPS version.
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES IN THIS MODULE
C     Subroutine:  UTIL_MEDIAN_ABS
C-----------------------------------------------------------------------
C                  EXTERNALS REFERENCED BY THIS MODULE
C  UTIL_ISAMIN  UTIL_ISAMAX  NUMARG
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C  STORAGE       - 0
C  HEAP(dynamic) - 0
C-----------------------------------------------------------------------
C\END DOC
       subroutine util_median_abs(array,n,amed,guess)
       real array(n)
       if (n.eq.0) then
         amed = 0.
         return
       endif
       i1 = util_isamin(n,array,1)
       a1 = abs(array(i1))
       i2 = util_isamax(n,array,1)
       a2 = abs(array(i2))
       if (a1.eq.a2) then
         amed = a1
         return
       endif
       if (numarg().eq.4) then
         aa = abs(guess)
       else
         aa = (a1+a2) * 0.5
       endif
       n1 = 1
       n2 = n
  10   continue
       nle = 0
       x1 = a1
       x2 = a2
       do 20 j=1,n
         if (abs(array(j)).le.aa) then
           nle = nle + 1
           x1 = max(x1,abs(array(j)))
         else
           x2 = min(x2,abs(array(j)))
         endif
  20   continue
       nle2 = 2 * nle
       if (nle2.lt.n-1) then
         a1 = x2
         n1 = nle + 1
         goto 30
       elseif (nle2.gt.n+1) then
         a2 = x1
         n2 = nle
         goto 30
       elseif (nle2.eq.n) then
         amed = (x1+x2) * 0.5
       elseif (nle2.lt.n) then
         amed = x2
       else
         amed = x1
       endif
       return
  30   if (a1.eq.a2) then
         amed = a1
         return
       endif
       aa = a1 + (a2-a1)*real(n+1-2*n1)/real(2*(n2-n1))
       goto 10
       end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_cps_info(vel_file
     1,n_vel,ix_vel,iy_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,m_work,work,i_err)
c  read the number and range of velocity functions

      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer   util_len_r

c  file names
      character vel_file*(*)

c  velocity x,y location header word
      integer   ix_vel,iy_vel

c  number of velocity / velping functions
      integer   n_vel

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   m_work
      real      work(m_work)

      integer   i_err

      character vel_name*8
      integer   l_vel,lx_vel,ly_vel,ix,iy,it,iv,iw,il,i,nv

      i_err = 0

cc    call getvsi(vel_file,l_vel,n_vel,lx_vel,ly_vel,*999)
c     call cvfopn_get_hdr(ix_vel,iy_vel)

      write(util_prn(),'(/,'' util velocity file='',a
     1,/,'' l_vel='',i5,'' n_vel='',i5
     1,'' ix_vel='',i2,'' iy_vel='',i2
     1,'' lx_vel='',i2,'' ly_vel='',i2)')
     1 vel_file,l_vel,n_vel,ix_vel,iy_vel,lx_vel,ly_vel

      ix = 1
      iy = ix + n_vel
      it = iy + n_vel
      iv = it + 100
      iw = iv + 100
      il = iw + 100

c  make sure there is enough work space
      if (m_work .lt. il-ix) then

        write(util_err()
     1,'(/,'' error in util_cps_info need more memory''
     1,/,'' n_vel='',i10,'' have='',i10,'' and need='',i10)')
     1 n_vel,m_work,il-ix
        goto 999

      endif    ! if (m_work .lt. il-ix) then

      do i = 1 , n_vel

c       call getv(vel_name,work(ix+i-1),work(iy+i-1),nv
c    1,work(it),work(iv),*999,work(iw))

      enddo    ! i = 1 , n_vel

c  determine the x dimension of the util velocity grid
      call util_array_size(nx_vel,x0_vel,dx_vel
     1,n_vel,work(ix),i_err)

c  determine the y dimension of the util velocity grid
      call util_array_size(ny_vel,y0_vel,dy_vel
     1,n_vel,work(iy),i_err)

      if (util_dbg() .ge. 0) write(util_dbg()
     1,'(/,'' util velocity grid characteristics''
     1,/,'' n_vel='',i10
     1,/,'' nx   ='',i10,'' x_min='',G14.7,'' x_max='',G14.7
     1,'' dx='',G14.7
     1,/,'' ny   ='',i10,'' y_min='',G14.7,'' y_max='',G14.7
     1,'' dy='',g14.7)')
     1 n_vel
     1,nx_vel,x0_vel,(nx_vel-1)*dx_vel+x0_vel,dx_vel
     1,ny_vel,y0_vel,(ny_vel-1)*dy_vel+y0_vel,dy_vel

c  make sure the util velocity grid is rectangular
      if (i_err .ne. 0 .or. nx_vel * ny_vel .ne. n_vel) then
        write(util_err(),'(/,'' error in util_cps_info''
     1,/,'' the util velocity grid should be regular''
     1,/,'' have n_vel='',i10,'' and should be='',i10
     1,/,''     i             x                y'')')
     1 n_vel,nx_vel*ny_vel
        write(util_err(),'(1x,i10,1x,G14.7)')
     1 (i+1,work(ix+i),work(iy+i),i=0,n_vel-1)
        goto 999
      endif    ! if (nx_vel * ny_vel .ne. n_vel) then

      return

  999 continue
      write(util_err(),'(/,'' error in util_cps_info'')')
      i_err = -1
      return

      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_array_size(nx,x0,dx,n,x,i_err)
c  determine the regularity of a set of numbers
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

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

      if (util_dbg() .ge. 0) write(util_dbg(),'(/,'' util_array_size''
     1,/,'' n    ='',i10,'' x_min='',G14.7,'' x_max='',G14.7
     1,/,'' nx   ='',i10,'' x0   ='',G14.7,'' xl   ='',G14.7
     1,'' dx='',G14.7)')
     1 n,x_min,x_max,nx,x0,(nx-1)*dx+x0,dx

      if (mod(n,nx) .ne. 0 .or. abs((nx-1)*dx+x0-x_max) .gt. eps) then
        write(util_err(),'(/,'' error in util_array_size''
     1,'' - grid is not regular'')')
        i_err = i_err - 1
      endif    ! if (mod(n,nx) .ne. 0) then

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      real function util_dk(nx,dx)
c  compute dk from nx,dx
      implicit  none
      integer  nx
      real     dx
      real     dk

      if (nx .gt. 1) then
        dk = 4. * asin(1.) / ((nx - 1) * dx)  ! x wavenumber spacing
      else    ! if (nx .gt. 1) then
        dk = 1.                               ! x wavenumber spacing
      endif    ! if (nx .gt. 1) then
      util_dk = dk
      return
      end


C23456789012345678901234567890123456789012345678901234567890123456789012
      real function util_kx(ix,nx,dx)
c  compute kx from ix,nx,dx
      implicit  none
      integer  ix,nx
      real     dx
      real     kx,dk

      if (nx .gt. 1) then
        dk = 4. * asin(1.) / ((nx - 1) * dx)  ! x wavenumber spacing
      else    ! if (nx .gt. 1) then
        dk = 1.                               ! x wavenumber spacing
      endif    ! if (nx .gt. 1) then

      if (ix .le. nx/2+1) then
        kx = (ix - 1) * dk
      else    ! if (ix .le. nx/2+1) then
        kx = (nx + 1 - ix) * dk
      endif    ! if (ix .le. nx/2+1) then

      util_kx = kx

      return
      end


C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_real_to_complex(n1,n2,xr,xc,work)
c  copy a real array to a complex array - may be inplace
      implicit  none
      integer   n1,n2
      real      xr(n1,n2)
      complex   xc(n1,n2)
      real      work(n1)
      integer   i1,i2

c  set real component of xc to xr
      do i2 = n2 , 1 , -1
      
        do i1 = 1 , n1
          work(i1) = xr(i1,i2)
        enddo    ! do i1 = 1 , n1

        do i1 = 1 , n1
          xc(i1,i2) = cmplx(work(i1),0.)
        enddo    ! do i1 = 1 , n1

      enddo    ! do i2 = n2 , 1 , -1

      return

C23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_complex_to_real(n1,n2,xr,xc,work)
c  copy a complex array to a real array - may be inplace

c  set xr to the real component of xc
      do i2 = 1 , n2
      
        do i1 = 1 , n1
          work(i1) = real(xc(i1,i2))
        enddo    ! do i1 = 1 , n1

        do i1 = 1 , n1
          xr(i1,i2) = work(i1)
        enddo    ! do i1 = 1 , n1

      enddo    ! do i2 = 1 , n2

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_compute_dk(ix_fft,nx_fft,dx_fft,dkx,kx)
c  compute wavenumber value from its index and the spatial characteristics
      implicit  none
      real     util_dkx

      integer  ix_fft,nx_fft
      real     dx_fft,dkx,kx

      dkx = util_dkx(nx_fft,dx_fft)

      if (ix_fft .le. nx_fft/2+1) then

        kx = (ix_fft - 1) * dkx

      else    ! if (ix_fft .le. nx_fft/2+1) then

        kx = -(nx_fft - ix_fft + 1) * dkx

      endif    ! if (ix_fft .le. nx_fft/2+1) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      real function util_dkx(nx_fft,dx_fft)
c  compute wavenumber spacing from the spatial characteristics
      implicit  none

      integer  nx_fft
      real     dx_fft

      if (nx_fft .gt. 1) then

        util_dkx = 4. * asin(1.) / ((nx_fft - 1) * dx_fft)! x wavenumber spacing

      else    ! if (nx_fft .gt. 1) then

        util_dkx = 1.                                     ! x wavenumber spacing

      endif    ! if (nx_fft .gt. 1) then

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_sum_trace(weight
     1,nt_mig,tr_map
     1,it_mig_1,it_mig_2,tr_mig
     1,it_fos_1,it_fos_2,tr_fos
     1)
c  map the trace from input to output
      implicit  none

      real     weight

      integer  it_mig_1,it_mig_2
      integer  nt_mig

      real     tr_map(nt_mig)
      real     tr_mig(nt_mig)

      integer  it_fos_1,it_fos_2
      real     tr_fos(1)

      integer  it_mig
      integer  it_fos

c  trace summation
      do it_mig = it_mig_1 , it_mig_2

        tr_mig(it_mig) = tr_mig(it_mig) + weight * tr_map(it_mig)

      end do    ! do it_mig = it_mig_1 , it_mig_2

c  fold of stack correction
      do it_fos = it_fos_1 , it_fos_2

        tr_fos(it_fos) = tr_fos(it_fos) + weight

      end do    ! do it_mig = it_mig_1 , it_mig_2

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_get_xy_trace(lu_buf
     1,ix_mem,ix_mig,nx_mem,nx_mig
     1,iy_mem,iy_mig,ny_mem,ny_mig
     1,i_xy_mem
     1,mh_buf,h_buf
     1,mt_buf,t_buf
     1,i_err)
c  make sure this trace is in memory
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      real      util_amax

      integer lu_buf

      integer ix_mem,ix_mig,nx_mem,nx_mig
      integer iy_mem,iy_mig,ny_mem,ny_mig

      integer i_xy_mem(nx_mem,ny_mem)

      integer mh_buf
      real    h_buf(mh_buf,nx_mem,ny_mem)

      integer mt_buf
      real    t_buf(mt_buf,nx_mem,ny_mem)

      integer i_err

      integer j_xy_mig

      i_err = 0

      ix_mem = mod(ix_mig-1,nx_mem) + 1
      iy_mem = mod(iy_mig-1,ny_mem) + 1
c      iy_mem = (iy_mig-1) / ny_mem  + 1

      j_xy_mig = (iy_mig - 1) * nx_mig + ix_mig

c      if (j_xy_inp .eq. j_xy_mig) then

c  make sure the input trace is in memory
        if (i_xy_mem(ix_mem,iy_mem) .ne. 0
     1.and. i_xy_mem(ix_mem,iy_mem) .ne. j_xy_mig) then

c  first write out the trace already in memory out to disk
c      if (util_dbg() .ge. 0) write(util_dbg()
c     1,'('' util_get_xy_trace write ix='',i5,'' iy='',i5
c     1,'' i_xy='',i5,'' h1='',f6.0,'' a='',g16.9)')
c     1 ix_mem,iy_mem,i_xy_mem(ix_mem,iy_mem),h_buf(1,ix_mem,iy_mem)
c     1,util_amax(mt_buf,t_buf(1,ix_mem,iy_mem))

        call util_write_data(lu_buf
     1,1,i_xy_mem(ix_mem,iy_mem),1
     1,mh_buf,mh_buf,h_buf(1,ix_mem,iy_mem)
     1,mt_buf,mt_buf,t_buf(1,ix_mem,iy_mem)
     1,i_err)
        if (i_err .ne. 0) goto 998

c  second read the required trace from disk into memory

        call util_read_data(lu_buf
     1,1,i_xy_mem(ix_mem,iy_mem),1
     1,mh_buf,mh_buf,h_buf(1,ix_mem,iy_mem)
     1,mt_buf,mt_buf,t_buf(1,ix_mem,iy_mem)
     1,i_err)
        if (i_err .ne. 0) goto 997

c      if (util_dbg() .ge. 0) write(util_dbg()
c     1,'('' util_get_xy_trace read ix='',i5,'' iy='',i5
c     1,'' i_xy='',i5,'' h1='',f6.0,'' a='',g16.9)')
c     1 ix_mem,iy_mem,i_xy_mem(ix_mem,iy_mem),h_buf(1,ix_mem,iy_mem)
c     1,util_amax(mt_buf,t_buf(1,ix_mem,iy_mem))

      endif    ! if (i_xy_mem(ix_mem,iy_mem) .ne. 0

c  add the input trace into the memory location

      i_xy_mem(ix_mem,iy_mem) = j_xy_mig

      return

  997 continue
      write(util_err()
     1,'(/,'' error in util_get_xy_trace during read'')')
      goto 999

  998 continue
      write(util_err()
     1,'(/,'' error in util_get_xy_trace during write'')')
      goto 999

  999 continue
      write(util_err(),'(/,'' error in util_get_xy_trace''
     1,/,'' lu_buf  ='',i8
     1,/,'' j_xy_mig='',i8,'' i_xy_mem='',i8
     1,/,'' ix_mig  ='',i8,'' iy_mig  ='',i8
     1,/,'' ix_mem  ='',i8,'' iy_mem  ='',i8
     1)')
     1 lu_buf
     1,j_xy_mig
     1,i_xy_mem(max(1,min(nx_mem,ix_mem))
     1         ,max(1,min(ny_mem,iy_mem)))
     1,ix_mig,iy_mig
     1,ix_mem,iy_mem

      i_err = -1
      return

      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_scale_trace(
     1 tr_med,tr_med_2,tr_ave
     1,jt_fos,at_fos
     1,nt_fos,tr_fos
     1,nt_mig,tr_mig
     1,n_work,work
     1)
c  scale trace by fold of stack correction

      implicit  none

      real     tr_med,tr_med_2,tr_ave

      integer  jt_fos
      real     at_fos

      integer  nt_fos
      real     tr_fos(1)

      integer  nt_mig
      real     tr_mig(nt_mig)

      integer  n_work
      real     work(n_work)

      integer  it_mig,it_fos
      integer  it_fos_1,it_fos_2
      real     ft_fos_1,ft_fos_2
      real     t0_fos,dt_fos
      real     tr_scale
      real     eps_fos

      eps_fos = 1.e-6

      if (nt_fos .eq. 1) then

        if (tr_fos(1) .ne. 0.) 
     1call util_scale(nt_mig,tr_mig,1./tr_fos(1)**at_fos)

      elseif (nt_fos .gt. 1) then    ! if (nt_fos .eq. 1) then

        t0_fos = 1. + eps_fos
        dt_fos = 1. / jt_fos

        do it_mig = 1 , nt_mig
          
          it_fos_1 = min(nt_fos,max(1,int(t0_fos)))
          it_fos_2 = min(nt_fos,it_fos_1+1)
          ft_fos_2 = max(0.,min(1.,t0_fos-it_fos_1))
          ft_fos_1 = 1. - ft_fos_2
          t0_fos   = t0_fos + dt_fos
          tr_scale = ft_fos_1 * tr_fos(it_fos_1)
     1             + ft_fos_2 * tr_fos(it_fos_2)
          if (tr_scale .gt. 0.) tr_scale = (1. / tr_scale) ** at_fos
          tr_mig(it_mig) = tr_scale * tr_mig(it_mig)
          
        enddo    ! do it_mig = 1 , nt_mig

      endif    ! if (nt_fos .eq. 1) then

c  scale by the median value in the trace
      
      if (nt_fos .gt. 1. and. n_work .ge. nt_mig) then

        call util_copy_abs(nt_mig,tr_mig,work)
        call util_ave_abs(nt_mig,1,work,tr_ave)
        call util_median(work,nt_mig,tr_med,tr_ave)
        if (tr_med .ne. 0.) call util_scale(nt_mig,tr_mig,1./tr_med)
        call util_copy_abs(nt_mig,tr_mig,work)
        call util_median(work,nt_mig,tr_med_2,1.)

      endif ! if (nt_fos .gt. 1. and. n_work .ge. nt_mig) then

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_compute_fos(
     1 nt_inp,t0_inp,dt_inp
     1,nt_fos,t0_fos,dt_fos,jt_fos)

      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      integer  nt_inp
      real     t0_inp,dt_inp

      integer  nt_fos
      real     t0_fos,dt_fos
      integer  jt_fos

      real     t1_fos

      nt_fos = max(1,min(nt_inp,nt_fos))
      t0_fos = t0_inp
      t1_fos = (nt_inp - 1) * dt_inp + t0_fos
      dt_fos = dt_inp
      jt_fos = nt_inp - 1

      if (nt_fos .gt. 1) then

        jt_fos = (nt_inp - 1) / max(1,nt_fos-1)
    1 continue
        dt_fos = dt_inp * jt_fos
        t1_fos = max(0,nt_fos-1) * dt_fos + t0_fos
        if (t1_fos .le. (nt_inp-1)*dt_inp+t0_inp) then
          jt_fos = jt_fos + 1
          goto 1
        endif

c        if (t1_fos .le. (nt_inp-1)*dt_inp+t0_inp) then

c          if (util_dbg() .ge. 0) write(util_dbg(),'(
c     1 /,'' nt_inp='',i8,'' t0_inp='',f10.4,'' dt_inp='',f10.4
c     1,'' t1_inp='',f10.4
c     1 /,'' nt_fos='',i8,'' t0_fos='',f10.4,'' dt_fos='',f10.4
c     1,'' t1_fos='',f10.4
c     1,/,'' jt_fos='',i8
c     1)')
c     1 nt_inp,t0_inp,dt_inp,(nt_inp-1)*dt_inp+t0_inp
c     1,nt_fos,t0_fos,dt_fos,(nt_fos-1)*dt_fos+t0_fos
c     1,jt_fos
c
c        endif    ! if (t1_fos .le. (nt_inp-1)*dt_inp+t0_inp) then

      endif    ! if (nt_fos .le. 1) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_anti_alias_filter_compute(
     1 nt_fft_f,nt_fft_i
     1,dt_fft
     1,f_amp_1,f_amp_2,f_pow,f_phase
     1,fft_f
     1,fft_i
     1,filter
     1,n_work,work
     1)
c  compute the filter
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test
      real     util_rd
      real     util_amax

      integer  nt_fft_f,nt_fft_i
      real     dt_fft
      real     f1_flt,f2_flt
      real     t1_flt,t2_flt
      real     f_amp_1,f_amp_2,f_pow,f_phase
      real     fft_f(3*nt_fft_f+4)
      real     fft_i(3*nt_fft_i+4)
      complex  filter(nt_fft_f/2+1)
      integer  n_work
      real     work(n_work)

      integer  nf_fft_f,nf_fft_i,if_fft
      real     f_nyquist,df_fft,df_amp,f_fft,f_amp,pi
      real     a1,a2,da
      real     b1,b2,db

      complex  c_fact

      nf_fft_f = nt_fft_f / 2 + 1
      nf_fft_i = nt_fft_i / 2 + 1
      f_nyquist = 1. / (2. * dt_fft)
      df_fft = f_nyquist / (nf_fft_i - 1)
      df_amp = (f_amp_2 - f_amp_1) / f_nyquist
      f1_flt = 0.
      f2_flt = f_nyquist
      t1_flt = 0.
      t2_flt = 0.

c      write(util_prn(),'(/,'' util_anti_alias_filter_compute''
c     1,/,'' nt_fft_f='',i8,'' nt_fft_i='',i8,'' dt_fft='',f10.4
c     1,/,'' nf_fft_f='',i6,'' nf_fft_i='',i6
c     1,/,'' f_nyquist='',f10.2,'' df_fft='',f10.2
c     1,/,'' f1_flt='',f10.2,'' f2_flt='',f10.2
c     1,/,'' t1_flt='',f10.2,'' t2_flt='',f10.2
c     1,/,'' f_amp_1='',f10.2,'' f_amp_2='',f10.2
c     1,'' f_pow='',f10.2,'' phase='',f10.2
c     1)')
c     1 nt_fft_f,nt_fft_i,dt_fft
c     1,nf_fft_f,nf_fft_i
c     1,f_nyquist,df_fft
c     1,f1_flt,f2_flt
c     1,t1_flt,t2_flt
c     1,f_amp_1,f_amp_2,f_pow,util_rd(f_phase)

c  initlaize fft tables
      call util_scfft(0,nt_fft_f,work,work,fft_f,work,0)
      call util_csfft(0,nt_fft_i,work,work,fft_i,work,0)

c  c_fact gives a phase shift 
c  the 1. / 2 is for linear frequency scaling
      c_fact = cmplx(cos(f_phase),sin(f_phase)) / 2.

      a1 = f1_flt - t1_flt / 2.
      a2 = f1_flt + t1_flt / 2.

      b1 = f2_flt - t2_flt / 2.
      b2 = f2_flt + t2_flt / 2.
      da  = a2 - a1
      if (da .eq. 0.) da = 1.
      db = b2 - b1
      if (db .eq. 0.) db = 1.
      pi = asin(1.) * 2

c  include a linear frequency scale
      do if_fft = 1 , nf_fft_f

        f_fft = (if_fft - 1) * df_fft

        if (f_fft .gt. a1 .and. f_fft .lt. a2) then

          f_amp = (1. + cos(pi * (a2 - f_fft) / da)) / 2.

        elseif (f_fft .ge. a2 .and. f_fft .le. b1) then

          f_amp = 1.

        elseif (f_fft .gt. b1 .and. f_fft .le. b2) then

          f_amp = (1. + cos(pi * (f_fft - b1) / db)) / 2.

        else

          f_amp = 0.

        endif

        filter(if_fft) = f_amp * c_fact * (f_amp_1 + f_fft * df_amp)

      enddo    ! do if_fft = 1 , nf_fft_f
c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' a filter='',g16.9)')
c     1,util_amax(nf_fft_f*2,filter)

      if (f_pow .ne. 0.) then

        do if_fft = 1 , nf_fft_f

          f_fft = (if_fft - 1) * df_fft
          filter(if_fft) = filter(if_fft) * sqrt(f_fft**f_pow)

        enddo    ! do if_fft = 1 , nf_fft_f

      endif    ! if (f_pow .ne. 0.) then

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' b filter='',g16.9)')
c     1 util_amax(nf_fft_f*2,filter)
      return

      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_anti_alias_filter_apply(
     1 nt_inp,t0_inp,dt_inp
     1,n_alias,l_alias,f_alias
     1,nt_fft_f,fft_f
     1,nt_fft_i,fft_i
     1,filter
     1,mh_inp,hd_inp
     1,mt_inp,tr_inp
     1,mt_fft,tr_fft
     1,n_work,work)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer  isamax
      real     util_amax

      integer   nt_inp
      real      t0_inp,dt_inp

      integer   n_alias,l_alias
      real      f_alias

      integer    nt_fft_f
      real       fft_f(3*nt_fft_f+4)

      integer    nt_fft_i
      real       fft_i(3*nt_fft_i+4)

      integer    mh_inp
      real       hd_inp(mh_inp)

      integer    mt_inp
      real       tr_inp(mt_inp)

      integer    mt_fft
      real       tr_fft(mt_fft,n_alias)

      integer    n_work
      real       work(n_work)

      complex    filter(nt_fft_f)

      integer    i_1d_fft
      integer    nf_fft_f,nf_fft_i
      integer    if_fft
      integer    it_inp,lt_inp
      integer    i_zero,n_zero
      integer    n_taper
      integer    it_mute,nt_mute
      integer    ib_mute,nb_mute
      real       t_mute,b_mute
      integer    i_alias
      real       tr_inp_max,tr_out_max

      i_1d_fft = +1

      nf_fft_f  = nt_fft_f / 2 + 1
      nf_fft_i  = nt_fft_i / 2 + 1

      it_inp = max(1,nint(t0_inp/dt_inp)+1)
      lt_inp = min(nt_fft_f,nt_inp) - it_inp + 1

      t_mute = hd_inp(2)
      it_mute = 1
      nt_mute = max(0,min(nt_fft_i
     1,(nint(t_mute)+it_inp-1)*(nt_fft_i/nt_fft_f)))

      b_mute = hd_inp(64)
      if (b_mute .le. 0.) b_mute = nt_inp
      ib_mute = max(1,min(nt_fft_i
     1,(nint(b_mute)+it_inp-1)*(nt_fft_i/nt_fft_f)))
      nb_mute = nt_fft_i - ib_mute + 1

      if (n_work .lt. nt_fft_f+2+2*n_alias*l_alias) goto 999

c  get max input amplitude
      tr_inp_max = util_amax(nt_inp,tr_inp)

c  initialize tr_fft to zero
      call util_setr(nt_fft_f+2,tr_fft,0.)

c  copy tr_inp to tr_fft
      call util_copy(lt_inp,tr_inp,tr_fft(it_inp,1))

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'(/,'' util_anti_alias_filter_apply''
c     1,/,'' mt_inp='',i6,'' nt_inp='',i6,'' it_inp='',i6,'' lt_inp='',i6
c     1,/,'' mt_fft='',i6,'' n_alias='',i6,'' l_alias='',i6
c     1,/,'' t_mute='',f10.6,'' it_mute='',i8,'' nt_mute='',i8
c     1,/,'' b_mute='',f10.6,'' ib_mute='',i8,'' nb_mute='',i8
c     1,/,'' nt_fft_f='',i6,'' nt_fft_i='',i6
c     1,/,'' nf_fft_f='',i6,'' nf_fft_i='',i6
c     1,/,'' tr_inp=''g16.9
c     1,/,'' work  =''g16.9
c     1)')
c     1 mt_inp,nt_inp,it_inp,lt_inp
c     1,mt_fft,n_alias,l_alias
c     1,t_mute,it_mute,nt_mute
c     1,b_mute,ib_mute,nb_mute
c     1,nt_fft_f,nt_fft_i
c     1,nf_fft_f,nf_fft_i
c     1,util_amax(nt_inp,tr_inp)
c     1,util_amax(nt_fft_f+2,tr_fft)

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' bef sc tr_fft='',g16.9)')
c     1 util_amax(nt_fft_f,tr_fft)
c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' bef    filter='',g16.9)')
c     1 util_amax(nf_fft_f*2,filter)

c  take real to complex fft - nt_fft_f long
      call util_scfft(+i_1d_fft,nt_fft_f,tr_fft,tr_fft,fft_f,work,0)

c  apply the filter
      call util_mulitply_complex(nf_fft_f,tr_fft,filter,tr_fft)
c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' c filter='',g16.9)')
c     1 util_amax(nf_fft_f*2,filter)

c  set the higher frequencies to zero
      n_zero = (nf_fft_i - nf_fft_f) * 2
      i_zero = 2 * nf_fft_f + 1
      call util_setr(n_zero,tr_fft(i_zero,1),0.)
 
c  take complex to real fft - nt_fft_i long
      call util_csfft(-i_1d_fft,nt_fft_i,tr_fft,tr_fft,fft_i,work,0)

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' aft cr tr_fft='',g16.9)')
c     1 util_amax(nt_fft_i,tr_fft)

c  reapply the top and bottom mutes
      call util_setr(nt_mute,tr_fft(it_mute,1),0.)
      call util_setr(nb_mute,tr_fft(ib_mute,1),0.)

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' aft mt tr_fft='',g16.9)')
c     1 util_amax(nt_fft_i,tr_fft)

c  compute Claerbout type anti_aliased traces for n_alias different values
c  of aliasing time shifts
c  use a linear taper of length n_taper at the start and end of each trace
c      do i_alias = 2 , n_alias
c        call util_copy(nt_fft_i,tr_fft,tr_fft(1,i_alias))
c      enddo    ! do i_alias = 2 , n_alias
      n_taper = min(21,nt_fft_i)
      call util_triangle_anti_alias(n_taper,n_alias,l_alias
     1,mt_fft,nt_fft_i,tr_fft,n_work,work)
c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' end anti tr_fft='',g16.9)')
c     1 util_amax(mt_fft*n_alias,tr_fft)

c  get max input amplitude
      tr_out_max = util_amax(nt_fft_i,tr_fft)

c  scale so the first output trace has the same max amplitude as the input trace
      if (tr_out_max .ne. 0.) 
     1call util_scale(mt_fft*n_alias,tr_fft,tr_inp_max/tr_out_max)

      return

  999 continue
      write(util_err()
     1,'(/,'' error in util_anti_alias_filter_apply''
     1,/,'' work space s too small''
     1,/,'' have n_work              ='',i8
     1,/,'' need nt_fft_f+2+2*n_alias*l_alias='',i8
     1,/,'' mt_inp='',i6,'' nt_inp='',i6
     1,/,'' mt_fft='',i6,'' n_alias='',i6,'' l_alias='',i6
     1,/,'' nt_fft_f='',i6,'' nt_fft_i='',i6
     1,/,'' nf_fft_f='',i6,'' nf_fft_i='',i6
     1,/,'' tr_inp=''g16.9
     1)')
     1 n_work,nt_fft_f+2+2*n_alias*l_alias
     1,mt_inp,nt_inp,mt_fft,n_alias,l_alias
     1,nt_fft_f,nt_fft_i
     1,nf_fft_f,nf_fft_i
     1,util_amax(nt_inp,tr_inp)
      stop

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_triangle_anti_alias(n_taper,n_alias,l_alias
     1,mt_inp,nt_inp,tr_inp,m_work,work)
c  apply Claerbout type triangle integral antialiasing
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      real     util_amax

      integer  n_taper
      integer  n_alias,l_alias
      integer  mt_inp,nt_inp
      real     tr_inp(mt_inp,n_alias)

      integer  m_work
      real     work(m_work)

      integer  l_taper
      integer  i_taper_1,i_taper_2
      real     a_taper_1,a_taper_2

      integer  i_alias,j_alias,it_alias,it_inp
      real     f_alias

c      if (util_dbg() .ge. 0) 
c      write(util_dbg(),'(/,'' util_triangle_anti_alias''
c     1,/,'' n_alias='',i8,'' l_alias='',i8,'' n_taper='',i8
c     1,/,'' mt_inp='',i8,'' nt_inp='',i8,'' m_work='',i8
c     1)')
c     1 n_alias,l_alias,n_taper
c     1,mt_inp,nt_inp,m_work

c      if (util_dbg() .ge. 0) 
c      write(util_dbg(),'('' input  tr=''g16.9)')
c     1 util_amax(nt_inp,tr_inp)

c  make sure there is enough work space
      if (m_work .lt. nt_inp+2*n_alias*l_alias) goto 998

c  initialize the work space to zero
      call util_setr(n_alias*l_alias,work(                       1),0.)
      call util_setr(n_alias*l_alias,work(nt_inp+n_alias*l_alias+1),0.)

c  copy the input trace into the work trace
      call util_copy(nt_inp,tr_inp,work(n_alias*l_alias+1))

c      write(84,'(1x,g16.9,1x,f10.4,1x,i8)')
c     1 (tr_inp(it_inp,1),(it_inp-1)*.001,0,it_inp=1,nt_inp)

c  double integration of trace - use tr_inp as a work array
      call util_double_integration(nt_inp+2*n_alias*l_alias,work,tr_inp)

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' work   tr=''g16.9)')
c     1 util_amax(nt_inp*2*n_alias,work)

c  initialze tr_inp to zero
      call util_setr(mt_inp*n_alias,tr_inp,0.)

c  compute anti_aliased trace for for n_alias different values
c  of aliasing time shifts

      l_taper = min(nt_inp,n_taper)
      i_taper_1 = 1
      i_taper_2 = nt_inp - l_taper + 1
      a_taper_1 = 0.
      a_taper_2 = 1.

CFPP$ CNCALL R
      do i_alias = 1 , n_alias

c  scale by the area in the summation
        j_alias = (i_alias - 1) * l_alias + 1
        f_alias = 1. / j_alias ** 2

        do it_inp = 1 , nt_inp

          it_alias = it_inp + n_alias*l_alias

c  Claerbout triangular anti_aliasing
          tr_inp(it_inp,i_alias) = f_alias
     1* (2.*work(it_alias        )
     1     -work(it_alias-j_alias)
     1     -work(it_alias+j_alias))

        enddo    ! do it_inp = 1 , nt_inp
c      write(84,'(1x,g16.9,1x,f10.4,1x,i8)')
c     1 (tr_inp(it_inp,i_alias),(it_inp-1)*.001,i_alias,it_inp=1,nt_inp)

c  taper trace ends,
c  Note we set the last 2 values to 0.  When this trace is migrated
c  any depth point which needs a time past the end of the trace
c  will be given one of the last 2 samples.  We set 2 samples to 0
c  because the total travel time is the sum of the source and the
c  reciever travel times.  Round off may increment the total by 1.

c  apply tapers to the begging and end of the de-aliased trace
        call util_linear_taper(a_taper_1,a_taper_2
     1,l_taper,tr_inp(i_taper_1,i_alias))

        call util_linear_taper(a_taper_2,a_taper_1
     1,l_taper,tr_inp(i_taper_2,i_alias))

        tr_inp(nt_inp-1,i_alias) = 0.
        tr_inp(nt_inp-0,i_alias) = 0.

c        if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' alias ia='',i8,'' tr_inp=''g16.9)')
c     1 i_alias,util_amax(nt_inp,tr_inp(1,i_alias))

c      write(6,'('' alias ia='',i8,'' tr_inp=''g16.9)')
c     1 i_alias,util_amax(nt_inp,tr_inp(1,i_alias))

      enddo    ! do i_alias = 1 , n_alias

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' output  tr=''g16.9)')
c     1 util_amax(mt_inp*n_alias,tr_inp)

      return

  998 continue
      write(util_err()
     1,'(/,'' error in util_triangle_anti_alias in work size'')')
      goto 999

  999 continue
      write(util_err(),'(/,'' error in util_triangle_anti_alias''
     1,/,'' n_alias='',i8,'' l_alias='',i8,'' n_taper='',i8
     1,/,'' mt_inp='',i8,'' nt_inp='',i8,'' m_work='',i8
     1,/,'' tr_inp=''g16.9
     1)')
     1 n_alias,l_alias,n_taper
     1,mt_inp,nt_inp,m_work
     1,util_amax(nt_inp,tr_inp)
      stop

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_double_integration(n,x1,x2)
c  double integration , causal, acausal
c  used for the antialias fitler
c  note x1,x2 may not be inplace 
c  x1 is both input and output 
c  x2 is a work array
      implicit  none

      integer  n,i
      real     x1(1),x2(1)
      real     x_sum

      integer  j

      call rcsum(n,x1,x2)

      j = n

      do i = 1 , n

        x1(j) = x2(i)
        j = j - 1

      enddo    ! do i = 1 , n

      call rcsum(n,x1,x2)

      j = n

      do i = 1 , n

        x1(j) = x2(i)
        j = j - 1

      enddo    ! do i = 1 , n

c  standard fortran replacement for above code
c      x_sum = 0.
c      do i = 1 , n
c        x_sum = x_sum + x1(i)
c        x2(i) = x_sum
c      enddo    ! do i = 1 , n
c
c      x_sum = 0.
c      do i = n , 1 , - 1
c        x_sum = x_sum + x2(i)
c        x1(i) = x_sum
c      enddo    ! do i = n , 1 , - 1

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_read_save(lu_out
     1,n_save,n_skip
     1,mh_inp,mt_dsk,hd_inp
     1,mt_inp,nt_inp,tr_inp
     1,i_err
     1)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer   lu_out
      integer   n_save
      integer   n_skip

      integer   mh_inp,mt_dsk
      real      hd_inp(mh_inp,1)

      integer   mt_inp,nt_inp
      real      tr_inp(mt_inp,1)

      integer   i_err

      integer   i_save,i_dsk

      i_err = 0

      do i_save = 1 , n_save

        i_dsk = n_skip + i_save

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg()
c     1,'('' read extra j_save='',i8,'' i_xy='',i8
c     1,'' a='',g16.9)')j_save,i_dsk,util_amax(nt_inp,tr_inp)

        call util_read_data(lu_out
     1,1,i_dsk,1
     1,mh_inp,mh_inp,hd_inp(1,i_save)
     1,mt_dsk,nt_inp,tr_inp(1,i_save)
     1,i_err)
        if (i_err .ne. 0) goto 999

      enddo    ! do j_save = 1 , n_save

      return

  999 continue
      write(util_err(),'(/,'' error in util_read_save'')')
      i_err = -1
      return

      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_write_save(lu_out
     1,n_save,n_skip
     1,mh_inp,mt_dsk,hd_inp
     1,mt_inp,nt_inp,tr_inp
     1,i_err
     1)
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer   lu_out
      integer   n_save
      integer   n_skip

      integer   mh_inp,mt_dsk
      real      hd_inp(mh_inp,1)

      integer   mt_inp,nt_inp
      real      tr_inp(mt_inp,1)

      integer   i_err

      integer   i_save,i_dsk


      i_err = 0

      do i_save = 1 , n_save

        i_dsk = n_skip + i_save

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' write extra j_save='',i8,'' i_xy='',i8
c     1,'' a='',g16.9)')j_save,i_dsk,util_amax(nt_inp,tr_inp)

        call util_write_data(lu_out
     1,1,i_dsk,1
     1,mh_inp,mh_inp,hd_inp(1,i_save)
     1,mt_dsk,nt_inp,tr_inp(1,i_save)
     1,i_err)
        if (i_err .ne. 0) goto 999

      enddo    ! do j_save = 1 , l_trin

      return

  999 continue
      write(util_err(),'(/,'' error in util_write_save'')')
      i_err = -1
      return

      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_write_mem(lu_out
     1,n_xy_mem,i_xy_mem
     1,mh_inp,hd_inp
     1,mt_inp,tr_inp
     1,i_err)
c  writ the current contents of memory to disk
      implicit  none

      integer   util_prn,util_err,util_dbg,util_wln,util_test

      integer  lu_out

      integer  n_xy_mem
      integer  i_xy_mem(n_xy_mem)

      integer  mh_inp
      real     hd_inp(mh_inp,n_xy_mem)

      integer  mt_inp
      real     tr_inp(mt_inp,n_xy_mem)

      integer  i_err

      integer  j_xy_mem

      i_err = 0

      do j_xy_mem = 1 , n_xy_mem

c      if (util_dbg() .ge. 0) 
c     1write(util_dbg(),'('' flush j_xy='',i5
c     1,'' i_xy='',i5,'' h1='',f6.0,'' a='',g16.9)')
c     1 j_xy_mem,i_xy_mem(j_xy_mem),hd_inp(1,j_xy_mem)
c     1,util_amax(mt_inp,tr_inp(1,j_xy_mem))

          if (i_xy_mem(j_xy_mem) .gt. 0) then

            call util_write_data(lu_out
     1,1,i_xy_mem(j_xy_mem),1
     1,mh_inp,mh_inp,hd_inp(1,j_xy_mem)
     1,mt_inp,mt_inp,tr_inp(1,j_xy_mem)
     1,i_err)
            if (i_err .ne. 0) goto 999

          endif    ! if (i_xy_mem(j_xy_mem) .gt. 0) then

      enddo    ! do j_xy_mem = 1 , n_xy_mem

      return

  999 continue
      write(util_err(),'(/,'' error in util_write_mem'')')
      i_err = -1
      return

      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_prn()
      implicit  none
      integer   lu_prn

      call util_prn_get(lu_prn)
      util_prn = lu_prn

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_prn_get(lu_prn)
      implicit  none
      integer   lu_prn
      integer   lu_prn_save
      data      lu_prn_save/6/
      save      lu_prn_save

      lu_prn = lu_prn_save

      return

C23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_prn_put(lu_prn)

      if (lu_prn .ne. lu_prn_save)
     1print'(/,'' util_prn_put changing print flag''
     1,'' from='',i8,'' to='',i8)',lu_prn_save,lu_prn

      lu_prn_save = lu_prn


      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_err()
      implicit  none
      integer   lu_err

      call util_err_get(lu_err)
      util_err = lu_err

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_err_get(lu_err)
      implicit  none
      integer   lu_err
      integer   lu_err_save
      data      lu_err_save/6/
      save      lu_err_save

      lu_err = lu_err_save

      return

C23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_err_put(lu_err)

      if (lu_err .ne. lu_err_save)
     1print'(/,'' util_err_put changing error flag''
     1,'' from='',i8,'' to='',i8)',lu_err_save,lu_err

      lu_err_save = lu_err

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_dbg()
      implicit  none
      integer   lu_dbg

      call util_dbg_get(lu_dbg)
      util_dbg = lu_dbg

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_dbg_get(lu_dbg)
      implicit  none
      integer   lu_dbg
      integer   lu_dbg_save
      data      lu_dbg_save/-1/
      save      lu_dbg_save

      lu_dbg = lu_dbg_save

      return

C23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_dbg_put(lu_dbg)

      if (lu_dbg .ne. lu_dbg_save)
     1print'(/,'' util_dbg_put changing debug flag''
     1,'' from='',i8,'' to='',i8)',lu_dbg_save,lu_dbg

      lu_dbg_save = lu_dbg

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_wln()
      implicit  none
      integer   sizeof_real

      util_wln = sizeof_real()

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_test()
      implicit  none
      integer   lu_test

      call util_test_get(lu_test)
      util_test = lu_test

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_test_get(test)
      implicit  none
      integer   test
      integer   test_save
      data      test_save/0/
      save      test_save

      test = test_save

      return

C23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_test_put(test)

      if (test .ne. test_save)
     1print'(/,'' util_test_put changing test flag''
     1,'' from='',i8,'' to='',i8)',test_save,test

      test_save = test

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_dcode()
      implicit  none
      integer   lu_dcode

      call util_dcode_get(lu_dcode)
      util_dcode = lu_dcode

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_dcode_get(dcode)
      implicit  none
      integer   dcode
      integer   dcode_save
      data      dcode_save/0/
      save      dcode_save

      dcode = dcode_save

      return

C23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_dcode_put(dcode)

      if (dcode .ne. dcode_save)
     1print'(/,'' util_dcode_put changing dcode flag''
     1,'' from='',i8,'' to='',i8)',dcode_save,dcode

      dcode_save = dcode

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_dcode_set
      implicit  none
      integer   util_prn,util_err,util_dbg,util_wln,util_test,util_dcode
      integer   lu_dcode,ip_dcode

      if (util_test() .eq. 1) then

        call util_dcode_get(lu_dcode)
        ip_dcode = 1
        call dcodebw(util_wln(),.false.)
        call dcodes(lu_dcode,ip_dcode)  !Setting unit number and all printing.

      endif    ! if (util_test() .eq. 1) then
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_t3e()
      implicit  none
      integer   lu_t3e

      call util_t3e_get(lu_t3e)
      util_t3e = lu_t3e

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_t3e_get(t3e)
      implicit  none
      integer   t3e
      integer   t3e_save
      data      t3e_save/0/
      save      t3e_save

      t3e = t3e_save

      return

C23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_t3e_put(t3e)

      if (t3e .ne. t3e_save)
     1print'(/,'' util_t3e_put changing t3e flag''
     1,'' from='',i8,'' to='',i8)',t3e_save,t3e

      t3e_save = t3e

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_cset_2(a0,a1,a2)
      implicit  none
      integer   util_r
      character a0*(*)
      character a1*(*)
      character a2*(*)
      character b0*80
      character b1*80
      character b2*80
      call util_caps(a0,b0)
      call util_caps(a1,b1)
      call util_caps(a2,b2)
      if (b0(1:1) .eq. b1(1:1)) then
        a0 = b1
      else    ! if (b0(1:1) .eq. b1(1:1)) then
        a0 = b2
      endif    ! if (b0(1:1) .eq. b1(1:1)) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_prn_i(lu,cx,x)
      implicit  none

      integer   util_r

      integer   lu
      character cx*(*)
      integer   x

      write(lu,'(1x,a10,'' = '',i16)')
     1 cx(1:min(10,util_r(cx))),x

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_prn_r(lu,cx,x)
      implicit  none

      integer   util_r

      integer   lu
      real      x
      character cx*(*)

      write(lu,'(1x,a10,'' = '',g16.9)')
     1 cx(1:min(10,util_r(cx))),x

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_prn_c(lu,cx,x)
      implicit  none

      integer   util_r

      integer   lu
      character cx*(*)
      character  x*(*)
      integer   lx

      lx = util_r(x)
      write(lu,'(1x,'' l='',i4,'' '',a40,'' = '',a)')
     1 lx,cx(1:min(40,util_r(cx))),x(1:lx)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_prn_1(lu,cx,nx,x1)
      implicit  none

      integer   util_r

      integer   lu
      character cx*(*)
      integer   nx
      real      x1(1)
      integer   ix

      write(lu,'(/,1x,a10,'' = '',i10)')
     1 cx(1:min(10,util_r(cx))),nx

      do ix = 1 , nx
        write(lu,'(1x,i10,1x,g16.9)')ix,x1(ix)
      enddo    ! do ix = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_prn_2(lu,cx,nx,x1,x2)
      implicit  none

      integer   util_r

      integer   lu
      character cx*(*)
      integer   nx
      real      x1(1)
      real      x2(1)
      integer   ix

      write(lu,'(/,1x,a10,'' = '',i10)')
     1 cx(1:min(10,util_r(cx))),nx

      do ix = 1 , nx
        write(lu,'(1x,i10,1x,g16.9,1x,g16.9)')
     1ix,x1(ix),x2(ix)
      enddo    ! do ix = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_prn_3(lu,cx,nx,x1,x2,x3)
      implicit  none

      integer   util_r

      integer   lu
      character cx*(*)
      integer   nx
      real      x1(1)
      real      x2(1)
      real      x3(1)
      integer   ix

      write(lu,'(/,1x,a10,'' = '',i10)')
     1 cx(1:min(10,util_r(cx))),nx

      do ix = 1 , nx
        write(lu,'(1x,i10,1x,g16.9,1x,g16.9,1x,g16.9)')
     1ix,x1(ix),x2(ix),x3(ix)
      enddo    ! do ix = 1 , nx

      return
      end

