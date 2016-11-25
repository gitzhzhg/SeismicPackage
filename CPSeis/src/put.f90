!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- put.f90 --------------------------------!!
!!------------------------------- put.f90 --------------------------------!!
!!------------------------------- put.f90 --------------------------------!!
!
! other files are:  put_crou.c
!
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
!
!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : put 
! Category   : io
! Written    : 2001-09-06   by: Bill Menger
! Revised    : 2001-09-06   by: Bill Menger
! Maturity   : beta
! Purpose    : Allow interface to "c" stdout, stdin, stderr i/o from F90
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>
!
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
! The put module contains i/o functions that supplement what can be done
! with fortran print and write functions.  Some implementations of f90 have
! been found deficient when performing i/o with the "advance=no" feature 
! followed by a read statement (for example).  This module allows one to use
! "c" functions to interact with the user instead of or in conjunction with
! existing fortran i/o read/write statements.
!-------------------------------------------------------------------------------
!</descript_doc>
!
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
!   o             i       i  
! status = put(        variable)
! status = put(format, variable)
!
! integer                     :: status ! 0 = ok, else bad.
! character(len=*),intent(in) :: format ! C format statement within which to
!                                       ! format your "variable"
! character(len=*),intent(in) :: variable
! integer,intent(in)          :: variable
! real   ,intent(in)          :: variable
! double precision,intent(in) :: variable
!
!
!-------------------------------------------------------------------------------
!</calling_doc>
!
!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2001-09-06  Bill Menger       Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


module put_module
  implicit none
  private

  public:: put
  character(len=100),public,save :: put_IDENT = &
  '$Id: put.f90,v 1.11 2001/09/06 19:47:54 mengewm Exp $'

  character,parameter                  :: NULL=char(0)

  interface
    function put_str_c(format, str) result (status)
      character(len=*),intent(in) :: format
      character(len=*),intent(in) :: str
      integer                     :: status
    end function put_str_c
  end interface
  
  interface
    function put_int_c(format, int) result (status)
      character(len=*),intent(in) :: format
      integer  ,intent(in)        :: int
      integer                     :: status
    end function put_int_c
  end interface
  
  interface
    function put_flt_c(format, flt) result (status)
      character(len=*),intent(in) :: format
      real     ,intent(in)        :: flt
      integer                     :: status
    end function put_flt_c
  end interface
  
  interface
    function put_dbl_c(format, dbl) result (status)
      character(len=*),intent(in) :: format
      double precision,intent(in) :: dbl
      integer                     :: status
    end function put_dbl_c
  end interface

  interface put
    module procedure put_int
    module procedure put_int_only
    module procedure put_flt
    module procedure put_flt_only
    module procedure put_dbl
    module procedure put_dbl_only
    module procedure put_str
    module procedure put_str_only
  end interface
  
  contains
  !------------------------------ integer ---------------------- 
  function put_int(format,int) result (status)
      character(len=*),intent(in) :: format
      integer  ,intent(in)        :: int
      integer                     :: status
      status = put_int_c(format//NULL,int)
  end function put_int

  function put_int_only(int) result (status)
      integer  ,intent(in)        :: int
      integer                     :: status
      status = put_int_c("%d"//NULL,int)
  end function put_int_only
  
  !------------------------------ string  ---------------------- 
  function put_str(format,str) result (status)
      character(len=*),intent(in) :: format
      character(len=*),intent(in) :: str
      integer                     :: status
      status = put_str_c(format//NULL,str//NULL)
  end function put_str
  
  function put_str_only(str) result (status)
      character(len=*),intent(in) :: str
      integer                     :: status
      status = put_str_c("%s"//NULL,str//NULL)
  end function put_str_only

  !------------------------------ real ---------------------- 
  function put_flt(format,flt) result (status)
      character(len=*),intent(in) :: format
      real     ,intent(in)        :: flt
      integer                     :: status
      status = put_flt_c(format//NULL,flt)
  end function put_flt

  function put_flt_only(flt) result (status)
      real     ,intent(in)        :: flt
      integer                     :: status
      status = put_flt_c("%f"//NULL,flt)
  end function put_flt_only
  
  !------------------------------ double precision ---------------------- 
  function put_dbl(format,dbl) result (status)
      character(len=*),intent(in)          :: format
      double precision  ,intent(in)        :: dbl
      integer                     :: status
      status = put_dbl_c(format//NULL,dbl)
  end function put_dbl

  function put_dbl_only(dbl) result (status)
      double precision  ,intent(in)        :: dbl
      integer                     :: status
      status = put_dbl_c("%g"//NULL,dbl)
  end function put_dbl_only
  
end module put_module
