!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- ll.f90 --------------------------------!!
!!------------------------------- ll.f90 --------------------------------!!
!!------------------------------- ll.f90 --------------------------------!!
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
! Name       : ll (Long Long)
! Category   : math
! Written    : 2001-01-02   by: Bill Menger
! Revised    : 2002-07-29   by: Ed Schmauch
! Maturity   : production   2002-08-12
! Purpose    : Performs long-long (8-byte) integer operations.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! To perform mathematical file offset calculations on files with variable sized 
! extents ranging up to 2**31-1 bytes, a method is needed that is general and
! allows simple operations such as +, -, =, add, and subtract to calculate file
! offsets when the values are greater than what can be held within a 4-byte 
! integer.  This module defines a new datatype:
!     
!     type(ll)                     :: my_ll_variable
! 
! Type (ll) consists of 2 integers of kind=4.  The equivalent type would be:
!
!     integer(kind=4),dimension(2) :: my_ll_equivalent
!
! The ll type has 3 operators defined for it, and the ll module also includes
! 2 functions for operations on integer,dimension(2) variables.  The module
! allows access into the radix used for rolling over values into the higher
! 4 bytes.  A user may set the radix(called maxintval) from 2 to 2**31-1.
!
! The  default maxintval == 2**31-1 (2147483647)
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
! Purpose:  These two routines get/set the maximum value allowed within a 4 byte
!           integer for your particular long-long application.
!
!           integer :: status
!           integer :: input_maxintval
!           integer :: maxintval
!   o                                  i
! status    = ll_set_maxintval(input_maxintval)
! maxintval = ll_get_maxintval()
!
! Purpose:  The following two routines perform long-long addition/subtraction
!           without using the ll type.  They always use the integer(kind=4)
!           type with dimension (2).
!           
!           integer(kind=4) :: addend1,addend2,subend1,subend2,result
!
!   o                   i       i
! result    = ll_add(addend1,addend2)
! result    = ll_sub(subend1,subend2) (result=subend1 - subend2)
!
! Purpose:  The following operators are defined to change type to/from ll and
!           integer(kind=4),dimension(2), to add, subtract, negate, or equival-
!           ence the ll data type
!
!           type(ll),integer, or integer(2) :: val1, val2, result
!           integer(kind=4),dimension(2)  :: foreign
!           integer                       :: fourbyte
!
!           (These work between all of the above types ...)
!           result = val1 + val2
!           result = val1 - val2
!           result = - val1
!           val1   = val2
!
!           foreign = val1
!           val1    = foreign
!           fourbyte = val1     !(could overflow fourbyte if val1.hi > 0 !!!)
!           val1     = fourbyte !(val1's high word is set to 0)
!
! Purpose:  The following logical operators are defined between ll variables.
!          == /= < <= >= > .eq. .ne. .lt. .le. .ge. .gt.
!           
! Purpose: The following functions allow one access into the ll data structure.
!          one may set or get the hi or lo part of the long long data.
!
!          integer :: hi, lo
!          type(ll):: ll_data
!          
!    o                    i
!   hi     = ll_get_hi(ll_data)
!   lo     = ll_get_lo(ll_data)
!                 i     o
! call ll_set_hi(hi, ll_data)
! call ll_set_lo(lo, ll_data)

! Purpose:  The following two routines allow one to pass ll or integer(kind=4),
!           dimension(2) arguments to subroutines needing the particular type
!           of data that is converted to.
!
!           type(ll) :: ll_arg
!           integer(kind=4),dimension(2) :: ii_arg
!
!     o                 i
!  ll_arg = ll_ii2ll(ii_arg)
!  ii_arg = ll_ll2ii(ll_arg)
!
!-------------------------------------------------------------------------------
!</calling_doc>
!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!If you are using ll for file offsets, be sure to get the extent size of the
!file from cio and set the ll_maxintval to this number before performing any
!operations.
!-------------------------------------------------------------------------------
!</advice_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author      Description
!     ----        ------      -----------
!  4. 2002-08-12  Ed Schmauch Fixed bug in ll_add_private.
!  3. 2001-10-16  Bill Menger Modified routine names (internal).
!  2. 2001-01-09  Bill Menger Added logical operators, data access routines.
!  1. 2001-01-02  Bill Menger Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!-------------------------------------------------------------------------------
!</portability_doc>
!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
!none
!-------------------------------------------------------------------------------
!</compile_doc>
!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!
module ll_module
!
  implicit none
!
  private
!
  character(len=100),save,public :: ll_ident = &
  '$Id: ll.f90,v 1.4 2002/08/07 22:54:27 Schmauch prod sps $'

  type :: ll
    private
    sequence
    integer(kind=4),dimension(2) :: i 
  end type ll
!
  integer(kind=4),save :: maxintval = 2147483647 !! 2**31 - 1
  integer(kind=4),save :: ZERO      = 0
!
  !--- types -----------
!
  public :: ll
!
  !--- Functions -------
!
  public :: ll_add          ,ll_sub
  public :: ll_set_maxintval,ll_get_maxintval
  public :: ll_get_hi       ,ll_get_lo
  public :: ll_set_hi       ,ll_set_lo
  public :: ll_ii2ll        ,ll_ll2ii
!
  !--- Operators and Assignment functions ----------
!
  public :: operator(+), operator(-), assignment(=)
  public :: operator(==)! operator(.eq.)
  public :: operator(/=)! operator(.ne.)
  public :: operator(<) ! operator(.lt.)
  public :: operator(<=)! operator(.le.)
  public :: operator(>=)! operator(.ge.)
  public :: operator(>) ! operator(.gt.)
!
  !----------------------------------------------------------
!
  interface operator (+)
    module procedure ll_plus
    module procedure ll_plus1
    module procedure ll_plus2
    module procedure ll_plus3
    module procedure ll_plus4
  end interface
!
  interface operator (-)
    module procedure ll_minus
    module procedure ll_minus1
    module procedure ll_minus2
    module procedure ll_minus3
    module procedure ll_minus4
    module procedure ll_negation
  end interface
!
  interface assignment(=)
    module procedure ll_eqs_ll2i
    module procedure ll_eqs_i2ll
    module procedure ll_eqs_ll1i
    module procedure ll_eqs_i1ll
  end interface

  interface operator(==)
    module procedure ll_eq_ll
  end interface

  interface operator(/=)
    module procedure ll_ne_ll
  end interface

  interface operator(<)
    module procedure ll_lt_ll
  end interface

  interface operator(<=)
    module procedure ll_le_ll
  end interface

  interface operator(>)
    module procedure ll_gt_ll
  end interface

  interface operator(>=)
    module procedure ll_ge_ll
  end interface
 
  interface ll_add
    module procedure ll_plus
    module procedure ll_plus1
    module procedure ll_plus2
    module procedure ll_plus3
    module procedure ll_plus4
    module procedure ll_add_private
  end interface
!
  interface ll_sub
    module procedure ll_minus
    module procedure ll_minus1
    module procedure ll_minus2
    module procedure ll_minus3
    module procedure ll_minus4
    module procedure ll_sub_private
  end interface
!
  contains
!
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-- Get the high part of an ll (01/02/2001 Bill Menger) ------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  function ll_get_hi(ll_data) result (hi)
    type(ll), intent(in)       :: ll_data
    integer(kind=4)                    :: hi
    hi = ll_data%i(1)
  end function ll_get_hi


!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-- Get the low part of an ll (01/02/2001 Bill Menger) ------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  function ll_get_lo(ll_data) result (lo)
    type(ll), intent(in)       :: ll_data
    integer(kind=4)                    :: lo
    lo = ll_data%i(2)
  end function ll_get_lo


!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-- Set the high part of an ll (01/02/2001 Bill Menger) ------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  subroutine ll_set_hi(hi,ll_data)
    integer(kind=4),intent(in)         :: hi
    type(ll), intent(out)      :: ll_data
    ll_data%i(1) = hi
  end subroutine ll_set_hi

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-- Set the low part of an ll (01/02/2001 Bill Menger) -------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  subroutine ll_set_lo(lo,ll_data)
    integer(kind=4),intent(in)         :: lo
    type(ll), intent(out)      :: ll_data
    ll_data%i(2) = lo
  end subroutine ll_set_lo

!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-- Set Maximum Integer Value (12/29/2000 Bill Menger) -------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  function ll_set_maxintval(input_maxintval) result (status)
    integer(kind=4),intent(in)     :: input_maxintval
    integer(kind=4)                :: status
    if(input_maxintval > 1 .and. input_maxintval <= 2147483647) then
      maxintval = input_maxintval
      status = 0
    else
      status = -1
    endif
  end function ll_set_maxintval
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-- Get Maximum Integer Value (12/29/2000 Bill Menger)--------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  function ll_get_maxintval() result (output_maxintval)
    integer(kind=4)                :: output_maxintval
    output_maxintval = maxintval
  end function ll_get_maxintval
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-- Add Long Long (09/06/2000 Bill Menger) -------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  function ll_add_private(addend1,addend2) result(ll_add_result)
    integer(kind=4),intent(in),dimension(2) :: addend1,addend2
    integer(kind=4),dimension(2)            :: ll_add_result
    integer(kind=4)                                 :: i,j
!
    ll_add_result = 0
    !--- start with lowest order ...
    ll_add_result(2) = addend1(2) + addend2(2)
    if(ll_add_result(2) >= maxintval ) then
      !--- we need the carry "bit" 
      i = ll_add_result(2)/maxintval
      j = modulo(ll_add_result(2),maxintval)
      ll_add_result(1) = i 
      ll_add_result(2) = j
    endif
!
    if(btest(ll_add_result(2),31) .and. &
      (addend1(2) >= 0 .and. addend2(2) >= 0) ) then
      !--- we rolled over
      ll_add_result(2) = ll_add_result(2) + maxintval + 1
      ll_add_result(1) = ll_add_result(1) + 1
    endif
!
    !--- we are negative
    do while(btest(ll_add_result(2),31))
      ll_add_result(2) = ll_add_result(2) + maxintval + 1
      ll_add_result(1) = ll_add_result(1) - 1
    end do
!
    !--- next higher order...
    ll_add_result(1) = ll_add_result(1) + addend1(1) + addend2(1)
        
!    write(6,'(3(B32.32,1x))')addend1(2),addend2(2),ll_add_result(2)
!    write(6,'(3(32L1,1x))')(btest(addend1(2),i),i=31,0,-1),&
!                           (btest(addend2(2),i),i=31,0,-1),&
!                           (btest(ll_add_result(2),i),i=31,0,-1)
  end function ll_add_private
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-- Subtract Long Long (09/06/2000 Bill Menger) --------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  function ll_sub_private(subend1,subend2) result(ll_sub_result)
    integer(kind=4),intent(in),dimension(2) :: subend1,subend2
    integer(kind=4),dimension(2)            :: ll_sub_result
    ll_sub_result = ll_add(subend1,-subend2)
  end function ll_sub_private
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-- Addition operator (09/06/2000 Bill Menger) ---------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  function ll_plus(addend1,addend2) result(ll_plus_result)
    type(ll) , intent(in)             :: addend1,addend2
    type(ll)                          :: ll_plus_result
    ll_plus_result%i = ll_add(addend1%i,addend2%i)
  end function ll_plus

  function ll_plus1(addend1,addend2) result(ll_plus_result)
    type(ll) , intent(in)             :: addend1
    integer(kind=4),intent(in)                :: addend2
    type(ll)                          :: ll_plus_result
    ll_plus_result%i = ll_add(addend1%i,(/ZERO,addend2/))
  end function ll_plus1

  function ll_plus2(addend1,addend2) result(ll_plus_result)
    integer(kind=4),intent(in)                :: addend1
    type(ll) , intent(in)             :: addend2
    type(ll)                          :: ll_plus_result
    ll_plus_result%i = ll_add(addend2%i,(/ZERO,addend1/))
  end function ll_plus2

  function ll_plus3(addend1,addend2) result(ll_plus_result)
    type(ll) , intent(in)             :: addend1
    integer(kind=4),intent(in),dimension(2)   :: addend2
    type(ll)                          :: ll_plus_result
    ll_plus_result%i = ll_add(addend1%i,addend2)
  end function ll_plus3

  function ll_plus4(addend1,addend2) result(ll_plus_result)
    integer(kind=4),intent(in),dimension(2)   :: addend1
    type(ll) , intent(in)             :: addend2
    type(ll)                          :: ll_plus_result
    ll_plus_result%i = ll_add(addend1,addend2%i)
  end function ll_plus4
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-- Subtraction operator (09/06/2000 Bill Menger) ------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  function ll_minus(subend1,subend2) result(ll_minus_result)
    type(ll) , intent(in)                   :: subend1,subend2
    type(ll)                                :: ll_minus_result
    ll_minus_result%i = ll_sub(subend1%i,subend2%i)
  end function ll_minus

  function ll_minus1(subend1,subend2) result (ll_minus_result)
    type(ll) , intent(in)                   :: subend1
    integer(kind=4),intent(in)              :: subend2
    type(ll)                                :: ll_minus_result
    ll_minus_result%i = ll_sub(subend1%i,(/ZERO,subend2/))
  end function ll_minus1

  function ll_minus2(subend1,subend2) result (ll_minus_result)
    type(ll) , intent(in)                   :: subend1
    integer(kind=4),intent(in),dimension(2) :: subend2
    type(ll)                                :: ll_minus_result
    ll_minus_result%i = ll_sub(subend1%i,subend2)
  end function ll_minus2

  function ll_minus3(subend1,subend2) result (ll_minus_result)
    integer(kind=4),intent(in)              :: subend1
    type(ll) , intent(in)                   :: subend2
    type(ll)                                :: ll_minus_result
    ll_minus_result%i = ll_sub((/ZERO,subend1/),subend2%i)
  end function ll_minus3

  function ll_minus4(subend1,subend2) result (ll_minus_result)
    integer(kind=4),intent(in),dimension(2) :: subend1
    type(ll) , intent(in)                   :: subend2
    type(ll)                                :: ll_minus_result
    ll_minus_result%i = ll_sub(subend1,subend2%i)
  end function ll_minus4
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!--- Negation operator ll (09/06/2000 Bill Menger) -----------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  function ll_negation(arg1) result(neg_arg1)
    type(ll) , intent(in) :: arg1
    type(ll)              :: neg_arg1
    neg_arg1%i = - arg1%i
  end function ll_negation
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-- Transfer Integer(2) to LL type   (09/06/2000 Bill Menger) ------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  subroutine ll_eqs_i2ll(ll_arg,int1_arg)
    type(ll),intent(out)                       :: ll_arg
    integer(kind=4),dimension(2),intent(in)    :: int1_arg
    ll_arg%i = int1_arg
  end subroutine ll_eqs_i2ll
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-- Transfer LL-type to integer(2) type (09/06/2000 Bill Menger) ---------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  subroutine ll_eqs_ll2i(int1_arg,ll_arg)
    integer(kind=4),intent(out),dimension(2)    :: int1_arg
    type(ll),intent(in)                         :: ll_arg
    int1_arg = ll_arg%i
  end subroutine ll_eqs_ll2i

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-- Transfer Integer to LL type   (01/02/2001 Bill Menger) ------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  subroutine ll_eqs_i1ll(ll_arg,int1_arg)
    type(ll),intent(out)                       :: ll_arg
    integer(kind=4),             intent(in)    :: int1_arg
    ll_arg%i(1) = 0
    ll_arg%i(2) = int1_arg
  end subroutine ll_eqs_i1ll
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-- Transfer LL-type to integer type (01/02/2001 Bill Menger) ---------------
!-- *** WARNING *** COULD CAUSE INTEGER OVERFLOW ***
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  subroutine ll_eqs_ll1i(int1_arg,ll_arg)
    integer(kind=4),intent(out)                 :: int1_arg
    type(ll),intent(in)                         :: ll_arg
    if(ll_arg%i(1) == 0 ) then
      int1_arg = ll_arg%i(2)
    else
      int1_arg = ll_arg%i(1) * maxintval + ll_arg%i(2)
    endif
  end subroutine ll_eqs_ll1i

!
  function ll_ll2ii(ll_arg) result (ii_result)
    type(ll),intent(in) :: ll_arg
    integer(kind=4),dimension(2) :: ii_result
    ii_result = ll_arg
  end function ll_ll2ii

  function ll_ii2ll(ii_arg) result (ll_result)
    integer(kind=4),dimension(2),intent(in) :: ii_arg
    type(ll)           :: ll_result
    ll_result = ii_arg
  end function ll_ii2ll


!------------ LOGICAL OPERATORS -------------------
  function ll_eq_ll(op1,op2) result (logic)
    type(ll),intent(in) :: op1,op2
    logical             :: logic
    if(op1%i(1) == op2%i(1) .and. &
       op1%i(2) == op2%i(2) ) then
      logic = .true.
    else
      logic = .false.
    endif
  end function ll_eq_ll
  
  function ll_ne_ll(op1,op2) result (logic)
    type(ll),intent(in) :: op1,op2
    logical             :: logic
    if(op1%i(1) /= op2%i(1) .or. &
       op1%i(2) /= op2%i(2) ) then
      logic = .true.
    else
      logic = .false.
    endif
  end function ll_ne_ll
  
  function ll_lt_ll(op1,op2) result (logic)
    type(ll),intent(in) :: op1,op2
    logical             :: logic
    type(ll)            :: tmp

    tmp = op1 - op2

    if(tmp%i(1) < 0 .or. &
       (tmp%i(1) == 0 .and. tmp%i(2) < 0 ) ) then
      logic = .true.
    else
      logic = .false.
    endif
  end function ll_lt_ll
  
  function ll_le_ll(op1,op2) result (logic)
    type(ll),intent(in) :: op1,op2
    logical             :: logic
    type(ll)            :: tmp

    tmp = op1 - op2

    if(tmp%i(1) < 0 .or. &
       (tmp%i(1) == 0 .and. tmp%i(2) <= 0 ) ) then
      logic = .true.
    else
      logic = .false.
    endif
  end function ll_le_ll
  
  function ll_ge_ll(op1,op2) result (logic)
    type(ll),intent(in) :: op1,op2
    logical             :: logic
    type(ll)            :: tmp

    tmp = op2 - op1

    if(tmp%i(1) < 0 .or. &
       (tmp%i(1) == 0 .and. tmp%i(2) <= 0 ) ) then
      logic = .true.
    else
      logic = .false.
    endif
  end function ll_ge_ll
  
  function ll_gt_ll(op1,op2) result (logic)
    type(ll),intent(in) :: op1,op2
    logical             :: logic
    type(ll)            :: tmp

    tmp = op2 - op1

    if(tmp%i(1) < 0 .or. &
       (tmp%i(1) == 0 .and. tmp%i(2) < 0 ) ) then
      logic = .true.
    else
      logic = .false.
    endif
  end function ll_gt_ll

end module ll_module
