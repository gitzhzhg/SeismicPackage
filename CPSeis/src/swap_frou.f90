!<CPS_v1 type="AUXILIARY_FILE"/>
! SEE ALSO: swap.c swap.h
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
!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY 
!     Date        Author       Description
!     ----        ------       -----------
!  2. 1999-10-21  Bill Menger  Added 2 argument calling capability for vectors,
!                              added "unk" call, for calling with one type but
!                              being able to explicitly declare a size of 
!                              another type.
!  1. 1999-09-30  Bill Menger  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>

module swap_module

  private

  public :: swap_bytes, swap_bytes_unk, swap_endian

  interface swap_bytes_unk

    subroutine swap_unk_2i(tn,isize)
      integer(kind=2),intent(inout)     :: tn
      integer        ,intent(in)        :: isize
    end subroutine swap_unk_2i

    subroutine swap_unk_4i(tn,isize)
      integer(kind=4),intent(inout)     :: tn
      integer        ,intent(in)        :: isize
    end subroutine swap_unk_4i

    subroutine swap_unk_4f(tn,isize)
      real           ,intent(inout)     :: tn
      integer        ,intent(in)        :: isize
    end subroutine swap_unk_4f

    subroutine swap_unk_8f(tn,isize)
      double precision,intent(inout)     :: tn
      integer        ,intent(in)         :: isize
    end subroutine swap_unk_8f

    subroutine swap_unk_cvec_2i(tn,isize,lgth)
      integer(kind=2),intent(inout)     :: tn
      integer        ,intent(in)        :: isize,lgth
    end subroutine swap_unk_cvec_2i

    subroutine swap_unk_cvec_4i(tn,isize,lgth)
      integer(kind=4),intent(inout)     :: tn
      integer        ,intent(in)        :: isize,lgth
    end subroutine swap_unk_cvec_4i

    subroutine swap_unk_cvec_4f(tn,isize,lgth)
      real           ,intent(inout)     :: tn
      integer        ,intent(in)        :: isize,lgth
    end subroutine swap_unk_cvec_4f

    subroutine swap_unk_cvec_8f(tn,isize,lgth)
      double precision,intent(inout)     :: tn
      integer        ,intent(in)         :: isize,lgth
    end subroutine swap_unk_cvec_8f

    module procedure swap_unk_2i_vec
    module procedure swap_unk_4i_vec
    module procedure swap_unk_4f_vec
    module procedure swap_unk_8f_vec

  end interface

  interface swap_bytes


    subroutine swap_short_2(tni2)
      integer(kind=2),intent(inout)     :: tni2
    end subroutine swap_short_2

    subroutine swap_short_2_cvec(tni2,lgth)
      integer(kind=2),intent(inout)     :: tni2
      integer        ,intent(in   )     :: lgth
    end subroutine swap_short_2_cvec

    !subroutine swap_u_short_2(unsigned short *tni2)
    !end subroutine swap_u_short_2

    subroutine swap_int_4(tni4)
      integer(kind=4),intent(inout)     :: tni4
    end subroutine swap_int_4

    subroutine swap_int_4_cvec(tni4,lgth)
      integer(kind=4),intent(inout)     :: tni4
      integer        ,intent(in   )     :: lgth
    end subroutine swap_int_4_cvec

    !subroutine swap_u_int_4(unsigned int *tni4)
    !end subroutine swap_u_int_4

    !subroutine swap_long_4(long *tni4)
    !end subroutine swap_long_4

    !subroutine swap_u_long_4(unsigned long *tni4)
    !end subroutine swap_u_long_4

    subroutine swap_float_4(tnf4)
      real(kind=4),intent(inout)        :: tnf4
    end subroutine swap_float_4

    subroutine swap_float_4_cvec(tnf4,lgth)
      real(kind=4),intent(inout)        :: tnf4
      integer        ,intent(in   )     :: lgth
    end subroutine swap_float_4_cvec

    subroutine swap_double_8(tndd8)
      real(kind=8), intent(inout)       :: tndd8
    end subroutine swap_double_8

    subroutine swap_double_8_cvec(tndd8,lgth)
      real(kind=8), intent(inout)       :: tndd8
      integer        ,intent(in   )     :: lgth
    end subroutine swap_double_8_cvec

    module procedure swap_short_2_vec
    module procedure swap_int_4_vec
!    module procedure swap_longlong_vec
    module procedure swap_float_4_vec
    module procedure swap_double_8_vec

  end interface

  interface 
    integer function swap_endian()
    end function swap_endian
  end interface

  contains

    subroutine swap_unk_2i_vec(x,isize)
      integer(kind=2),intent(inout) , dimension (:) :: x
      integer, intent(in)                   :: isize
      integer                               :: temp
      temp = size(x)
      call swap_unk_cvec_2i(x(1),isize,temp)
    end subroutine swap_unk_2i_vec

    subroutine swap_unk_4i_vec(x,isize)
      integer,intent(inout) , dimension (:) :: x
      integer, intent(in)                   :: isize
      integer                               :: temp
      temp = size(x)
      call swap_unk_cvec_4i(x(1),isize,temp)
    end subroutine swap_unk_4i_vec

    subroutine swap_unk_4f_vec(x,isize)
      real   ,intent(inout) , dimension (:) :: x
      integer, intent(in)                   :: isize
      integer                               :: temp
      temp = size(x)
      call swap_unk_cvec_4f(x(1),isize,temp)
    end subroutine swap_unk_4f_vec

    subroutine swap_unk_8f_vec(x,isize)
      double precision ,intent(inout) , dimension (:) :: x
      integer, intent(in)                   :: isize
      integer                               :: temp
      temp = size(x)
      call swap_unk_cvec_8f(x(1),isize,temp)
    end subroutine swap_unk_8f_vec

    subroutine swap_int_4_vec(x)
      integer,intent(inout) , dimension (:) :: x
      integer                               :: temp
      temp = size(x)
      call swap_bytes(x(1),temp)
    end subroutine swap_int_4_vec

    subroutine swap_short_2_vec(x)
      integer(kind=2),intent(inout) , dimension (:) :: x
      integer                               :: temp
      temp = size(x)
      call swap_bytes(x(1),temp)
    end subroutine swap_short_2_vec

    subroutine swap_float_4_vec(x)
      real , intent(inout) , dimension (:) :: x
      integer                               :: temp
      temp = size(x)
      call swap_bytes(x(1),temp)
    end subroutine swap_float_4_vec

    subroutine swap_double_8_vec(x)
      double precision,intent(inout) , dimension (:) :: x
      integer                               :: temp
      temp = size(x)
      call swap_bytes(x(1),temp)
    end subroutine swap_double_8_vec

end module swap_module
