module test_module
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

	implicit none

	public

    interface b
      subroutine b(x,n)
        real,intent(inout),dimension(:) :: x
        integer, intent(in)             :: n
      end subroutine b
    end interface

	contains

	subroutine square (x,n)

	integer,intent(in)              :: n
	real,intent(inout),dimension(:) :: x(n)

	integer                         :: i

	do i = 1, n

		x(i) = x(i)*x(i)

	end do

	end subroutine square

end module test_module

subroutine square_root (x,n)
	integer,intent(in)              :: n
	real,intent(inout),dimension(:) :: x(n)
	integer                         :: i 

	do i = 1, n
		x(i) = x(i)/i
	end do

end subroutine square_root

program test_program

use test_module
implicit none

real :: x(10)

integer :: n=10
integer :: i

do i = 1, n
	x(i) = i
end do

call square(x,n)

write(6,*) x

call square_root(x,n) 

write(6,*) x

call b(x,n)

write(6,*) x

end program test_program


