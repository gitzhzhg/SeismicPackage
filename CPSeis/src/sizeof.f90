!<CPS_v1 type="PRIMITIVE"/>
! other files are:  sizeof_crou.c
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
! Name       : sizeof 
! Category   : miscellaneous
! Written    : 1999-10-19   by: Bill Menger
! Revised    : 2004-05-03   by: Bill Menger
! Maturity   : production
! Purpose    : Provide size of various types of variables for the machine.
! Portability: 
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
! This function provides native word sizes for various types of variables.
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
!                             i
! number_of_bytes  = sizeof(variable)
! integer  :: number_of_bytes
! integer | real | complex | double precision | logical :: variable
! AND
! integer(kind=2) , integer(kind=1)                     :: variable
! AND
! character(len=nnn)                                    :: variable
!
! The number of bytes returned is the number of bytes for the variable type
! that is input.
! For character(len=nnn) the returned number is the number of bytes required
! for a character string of length "nnn".  Typically, this is "nnn" bytes,
! but if a machine required 2 bytes per character, you would see 2xnnn.
!
! THIS IS NOT DESIGNED TO REPLACE THE "SIZE" FUNCTION OF F90.  It will NOT
! accept "arrays", but can handle character strings.  (e.g. it cannot handle
! character arrays.)
! Example:
!   character(len=5) :: a
!   print*, "size of a = ",sizeof(a) ! should return "5"
!
!   BUT
!
!   character(len=1) :: a(5)
!   print*, "size of a = ",sizeof(a) ! Should return compiler error
!   print*, "size of a(1) = ",sizeof(a(1)) ! should return "1"
!
!</calling_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  3. 2004-05-03  Bill Menger  Added "string" removed "char" (since it is a 
!                              subset of "string".
!  2. 2000-08-21  Bill Menger  Added short, byte, char.
!  1. 1999-10-19  Bill Menger  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
! No known limitations.
!</portability_doc>
!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
!-------------------------------------------------------------------------------
!</compile_doc>
!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!-------------------------------------------------------------------------------
!</algorithm_doc>
!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!-------------------------------------------------------------------------------
!</programming_doc>
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
module sizeof_module
  implicit none
  private
  public :: sizeof

  character(len=100),save,public :: sizeof_ident = &
  "$Id: sizeof.f90,v 1.3 2004/05/03 11:29:37 Menger prod sps $"

  interface sizeof
    module procedure sizeof_double
    module procedure sizeof_real
    module procedure sizeof_logical
    module procedure sizeof_integer
    module procedure sizeof_short
    module procedure sizeof_byte
    module procedure sizeof_complex
    module procedure sizeof_string
  end interface

  interface
    integer function sizeof_c_short(x,y) result(i)
      integer(kind=2),intent(in) :: x,y
    end function sizeof_c_short
  end interface

  interface
    integer function sizeof_c_byte(x,y) result(i)
      integer(kind=1),intent(in) :: x,y
    end function sizeof_c_byte
  end interface

  interface
    integer function sizeof_c_string(x,y) result(i)
      character(len=*),intent(in) :: x,y
    end function sizeof_c_string
  end interface

  interface
    integer function sizeof_c_real(x,y) result(i)
      real,intent(in) :: x,y
    end function sizeof_c_real
  end interface

  interface
    integer function sizeof_c_complex(x,y) result(i)
      complex,intent(in) :: x,y
    end function sizeof_c_complex
  end interface
  
  interface
    integer function sizeof_c_double(x,y) result(i)
      double precision ,intent(in) :: x,y
    end function sizeof_c_double
  end interface
  
  interface
    integer function sizeof_c_integer(x,y) result(i)
      integer,intent(in) :: x,y
    end function sizeof_c_integer
  end interface
  
  interface
    integer function sizeof_c_logical(x,y) result(i)
      logical,intent(in) :: x,y
    end function sizeof_c_logical
  end interface

  contains

  integer function sizeof_double (d) result (sizeof)
    double precision, intent(in) :: d
    double precision             :: x(2)
    sizeof = sizeof_c_double(x(1),x(2))
  end function sizeof_double

  integer function sizeof_complex (d) result (sizeof)
    complex, intent(in)          :: d
    complex                      :: x(2)
    sizeof = sizeof_c_complex(x(1),x(2))
  end function sizeof_complex

  integer function sizeof_real (d) result (sizeof)
    real, intent(in)             :: d
    real                         :: x(2)
    sizeof = sizeof_c_real(x(1),x(2))
  end function sizeof_real

  integer function sizeof_integer (d) result (sizeof)
    integer, intent(in)          :: d
    integer                      :: x(2)
    sizeof = sizeof_c_integer(x(1),x(2))
  end function sizeof_integer

  integer function sizeof_logical (d) result (sizeof)
    logical, intent(in)          :: d
    logical                      :: x(2)
    sizeof = sizeof_c_logical(x(1),x(2))
  end function sizeof_logical

  integer function sizeof_short (d) result (sizeof)
    integer(kind=2),intent(in)   :: d
    integer(kind=2)              :: x(2)
    sizeof = sizeof_c_short  (x(1),x(2))
  end function sizeof_short  

  integer function sizeof_byte  (d) result (sizeof)
    integer(kind=1),intent(in)   :: d
    integer(kind=1)              :: x(2)
    sizeof = sizeof_c_byte   (x(1),x(2))
  end function sizeof_byte

  integer function sizeof_string (d) result (sizeof)
    character(len=*),intent(in)   :: d
    character(len=len(d))         :: x(2)
    sizeof = sizeof_c_string   (x(1),x(2))
  end function sizeof_string

end module sizeof_module
