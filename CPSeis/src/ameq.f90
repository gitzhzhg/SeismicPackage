!<CPS_v1 type="PRIMITIVE"/>
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
! Name       : ameq 
! Category   : math
! Written    : 1999-07-16   by: Randy L. Selzler
! Revised    : 2001-10-01   by: Chuck C Burch
! Maturity   : production   2001-10-18
! Purpose    : Return true if AlMost EQual, comparison of 2 floats
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION                            
!
!<pm>
! Compare float value1 and value2.
! Return TRUE if equal within +/- epsilon, otherwise return FALSE.
!
! An integer can also be specified for epsilon in which case the integer
!  specifies the number of consectutive numbers in the floating point 
!  representation just less than or just greater than the floating point
!  representation of of value 1 that value 2 can be and be considered
!  almost equal to value1
!
! If epsilon is not specified, then value2 is considered almost equal to value1
!   if value 2 is value1 or the nearest numbers with the floating point 
!   representation just less or just greater than value 1.
!
!</pm>
!
!------------------------------------------------------------------------------
!
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE                               
!
!                  o              i        i       i
!                ALMOST = ameq (value1, value2, epsilon)
!
! logical  ameq     =       function return value
! real or double     value1   =       1st operand for comparison
! real or double     value2   =       2nd operand for comparison
! real or double     epsilon  =       tolerance for equality test
!
! epsilon can be missing in which case value1 is considered almost equal to
!  value2 if value2 is value1 or the nearest floatimg point numbers just less 
!  than or just greater than value1
!
! epsilon can also be an integer which indicates the number of values  
!   that values2 can be in the floating point representation just less or 
!   greater than value1 to return true.
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                              
!
!     Date        Author                 Description
!     ----        ------                 -----------
!  3. 2001-10-18  Chuck C. Burch         Added support for double prec. epsilon.
!                                        Made internal routines private.
!                                        Provide for integer epsilon which
!                                         specifies the number of numbers that
!                                         values2 can be in the floatimg point
!                                         representation that are just  less or 
!                                         greater than value1 to return true.
!                                        Provide for missing epsilon where
!                                         value1 and the floating point numbers
!                                         just less than or just greater than
!                                         value1 are considered almost equal to
!                                         value2.
!  2. 2000-07-25  Karen Goodger          Allow double precision arguments.
!  1. 1999-07-16  Randy L. Selzler       Initial f90 version.
!  1. 1994-04-05  Karen Goodger          Initial C version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!------------------------------------------------------------------------------
!</portability_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


  module ameq_module

    implicit none
    private

    public :: ameq

    interface ameq
      module procedure ameq_both_real
      module procedure ameq_both_real_eps_double
      module procedure ameq_both_real_num_mode
      module procedure ameq_both_real_no_eps
      
      module procedure ameq_first_double
      module procedure ameq_first_double_eps_double
      module procedure ameq_first_double_num_mode
      module procedure ameq_first_double_no_eps
      
      module procedure ameq_second_double
      module procedure ameq_second_double_eps_double
      module procedure ameq_second_double_num_mode
      module procedure ameq_second_double_no_eps
      
      module procedure ameq_both_double
      module procedure ameq_both_double_eps_double
      module procedure ameq_both_double_num_mode
      module procedure ameq_both_double_no_eps
    end interface


    character(len=100),public,save :: AMEQ_IDENT = &
'$Id: ameq.f90,v 1.3 2001/10/17 13:59:10 CCBurch prod sps $'

    contains

! ----- support routines for value1 and value2 being both real -------

    function ameq_both_real (value1, value2, epsilon) result(results)
      real, intent(in) :: value1, value2
      real, intent(in) :: epsilon
      logical          :: results

      if((value1 > value2 + epsilon) .OR. &
         (value1 < value2 - epsilon)) then
        results = .FALSE.
      else
        results = .TRUE.
      endif

      return
    end function ameq_both_real
 
    function ameq_both_real_eps_double (value1, value2, epsilon) result(results)
      real,             intent(in) :: value1, value2
      double precision, intent(in) :: epsilon
      logical                      :: results

      results=ameq_both_double_eps_double(dble(value1), dble(value2), epsilon)
      return
    end function ameq_both_real_eps_double
 
    function ameq_both_real_num_mode (value1, value2, n_vals) result(results)
      real,             intent(in) :: value1, value2
      integer,          intent(in) :: n_vals
      logical                      :: results

      integer                      :: i
      real                         :: r, dir

      results=.true.
      if(value1.eq.value2) return

      if(value2.gt.value1) then
        dir=1.
      else
        dir=-1.
      endif

      r=value1
      do i=1, n_vals
        r=nearest(r,dir)
        if(value2.eq.r) return
      enddo

      results=.false.
      return
    end function ameq_both_real_num_mode
 
    function ameq_both_real_no_eps (value1, value2) result(results)
      real,   intent(in) :: value1, value2
      logical            :: results

      results=ameq_both_real_num_mode(value1, value2, 1)
      return
    end function ameq_both_real_no_eps

! -------- support routines for value1 double and value2 real ----------

    function ameq_first_double (value1, value2, epsilon) result(results)
      double precision, intent(in) :: value1
      real,             intent(in) :: value2
      real,             intent(in) :: epsilon
      logical          :: results

      results=ameq_both_double_eps_double(value1, dble(value2), dble(epsilon))
      return
    end function ameq_first_double

    function ameq_first_double_eps_double (value1, value2, epsilon)  & 
     result(results)
      double precision,  intent(in) :: value1
      real,              intent(in) :: value2
      double precision,  intent(in) :: epsilon
      logical                       :: results

      results=ameq_both_double_eps_double(value1, dble(value2), epsilon)
      return
    end function ameq_first_double_eps_double

    function ameq_first_double_num_mode (value1, value2, n_vals) result(results)
      double precision,  intent(in) :: value1
      real,              intent(in) :: value2
      integer,           intent(in) :: n_vals
      logical                       :: results

      results=ameq_both_double_num_mode(dble(value2), value1, n_vals)
      return
    end function ameq_first_double_num_mode

    function ameq_first_double_no_eps (value1, value2) result(results)
      double precision, intent(in) :: value1
      real,             intent(in) :: value2
      logical                      :: results

      results= ameq_both_double_num_mode(dble(value2), value1, 1)
      return
    end function ameq_first_double_no_eps

! ---------- support routines for value1 real and value2 double ----------

    function ameq_second_double (value1, value2, epsilon) result(results)
      real,             intent(in) :: value1
      double precision, intent(in) :: value2
      real,             intent(in) :: epsilon
      logical                      :: results

      results=ameq_both_double_eps_double(dble(value1), value2, dble(epsilon))
      return
    end function ameq_second_double

    function ameq_second_double_eps_double (value1, value2, epsilon) &
     result(results)
      real,             intent(in) :: value1
      double precision, intent(in) :: value2
      double precision, intent(in) :: epsilon
      logical                      :: results

      results=ameq_both_double_eps_double(dble(value1), value2, epsilon)
      return
    end function ameq_second_double_eps_double

    function ameq_second_double_num_mode (value1, value2, n_vals) &
     result(results)
      real,             intent(in) :: value1
      double precision, intent(in) :: value2
      integer,          intent(in) :: n_vals
      logical                      :: results

      results=ameq_both_double_num_mode(dble(value1), value2, n_vals)
      return
    end function ameq_second_double_num_mode

    function ameq_second_double_no_eps (value1, value2) result(results)
      real,             intent(in) :: value1
      double precision, intent(in) :: value2
      logical                      :: results

      results=ameq_both_double_num_mode(dble(value1), value2, 1)
      return
    end function ameq_second_double_no_eps
    
! --------------- support routines for value1 and value2 double -------

    function ameq_both_double (value1, value2, epsilon) result(results)
      double precision, intent(in) :: value1,value2
      real,             intent(in) :: epsilon
      logical                      :: results

      results=ameq_both_double_eps_double(value1, value2, dble(epsilon))
      return
    end function ameq_both_double

    function ameq_both_double_eps_double (value1, value2, epsilon) & 
     result(results)
      double precision, intent(in) :: value1, value2, epsilon
      logical                      :: results

      if((value1 > value2 + epsilon) .OR. &
         (value1 < value2 - epsilon)) then
        results = .FALSE.
      else
        results = .TRUE.
      endif

      return
    end function ameq_both_double_eps_double

    function ameq_both_double_num_mode (value1, value2, n_vals)  &
     result(results)
      double precision, intent(in) :: value1, value2
      integer,          intent(in) :: n_vals
      logical                      :: results

      integer                      :: i
      double precision             :: r, dir

      results=.true.
      if(value1.eq.value2) return

      if(value2.gt.value1) then
        dir=1.
      else
        dir=-1.
      endif

      r=value1
      do i=1, n_vals
        r=nearest(r,dir)
        if(value2.eq.r) return
      enddo

      results=.false.
      return
    end function ameq_both_double_num_mode

    function ameq_both_double_no_eps (value1, value2) result(results)
      double precision, intent(in) :: value1,value2
      logical          :: results

      results=ameq_both_double_num_mode (value1, value2, 1) 
      return
    end function ameq_both_double_no_eps

  end module ameq_module

!!-------------------- end module, start test program ---------------------!!
!!-------------------- end module, start test program ---------------------!!
!!-------------------- end module, start test program ---------------------!!

!program test
! use ameq_module
! implicit none

! real             :: r1, r2, r3, r4, rn1, rn2, rn3, rn4, re
! double precision :: d1, d2, d3, d4, dn1, dn2, dn3, dn4, de

! r1=1.
! rn1=nearest(r1, 1.)    !number just after r1 as floating point
! rn2=nearest(rn1,1.)    !2nd number just after r1 as floating point 
! re=r1/1024.
! r2=r1+re
! d1=r1
! dn1=nearest(d1, 1.d0)  !sane as rn1 but doule precision domain
! dn2=nearest(dn1,1.d0)  !same as rn2 but double precision domain
! de=d1/1024.
! d2=d1+de
! 
! r3=1.
! rn3=nearest(r3,-1.)    !number just before r3 as floating point
! rn4=nearest(rn3,-1.)   !2nd number just before r3 as floating point
! r4=r1-re
! d3=r3
! dn3=nearest(d3, -1.d0) !same as rn3 but double precision domain
! dn4=nearest(dn3,-1.d0) !same as rn4 but double precision domain
! d4=d3-de
! 
! print *,"testing r,r,r"  
! print *,"ameq(",r1,",",dble(r2),",",re,")=",ameq(r1,r2,re)
! print *,"ameq(",r1,",",dble(nearest(r2,1.)), ",",re,")=",   &
!                                             ameq(r1,nearest(r2,1.),re)

! print *,"ameq(",r3,",",dble(r4),",",re,")=",ameq(r3,r4,re)
! print *,"ameq(",r3,",",dble(nearest(r4,-1.)), ",",re,")=",   &
!                                             ameq(r3,nearest(r4,-1.),re)

! print *,""
! print *,"testing r,r,d (also tests d,d,d)"
! print *,"ameq(",r1,",",dble(r2),",",de,")=",ameq(r1,r2,de)
! print *,"ameq(",r1,",",dble(nearest(r2,1.)), ",",de,")=",   &
!                                             ameq(r1,nearest(r2,1.),de)
! print *,"ameq(",r3,",",dble(r4),",",de,")=",ameq(r3,r4,de)
! print *,"ameq(",r3,",",dble(nearest(r4,-1.)), ",",de,")=",   &
!                                             ameq(r3,nearest(r4,-1.),de)

! print *,"testing r,d,r"  
! print *,"ameq(",r1,",",dble(r2),",",re,")=",ameq(r1,dble(d2),re)
! print *,"ameq(",r1,",",dble(nearest(r2,1.)), ",",re,")=",   &
!                                             ameq(r1,dble(nearest(r2,1.)),re)

! print *,"ameq(",r3,",",dble(r4),",",re,")=",ameq(r3,dble(r4),re)
! print *,"ameq(",r3,",",dble(nearest(r4,-1.)), ",",re,")=",   &
!                                             ameq(r3,dble(nearest(r4,-1.)),re)

! print *,""
! print *,"testing r,d,d (also tests d,d,d)"
! print *,"ameq(",r1,",",dble(r2),",",de,")=",ameq(r1,dble(r2),de)
! print *,"ameq(",r1,",",dble(nearest(r2,1.)), ",",de,")=",   &
!                                             ameq(r1,dble(nearest(r2,1.)),de)
! print *,"ameq(",r3,",",dble(r4),",",de,")=",ameq(r3,dble(r4),de)
! print *,"ameq(",r3,",",dble(nearest(r4,-1.)), ",",de,")=",   &
!                                             ameq(r3,dble(nearest(r4,-1.)),de)

! print *,"testing d,r,r"  
! print *,"ameq(",r1,",",dble(r2),",",re,")=",ameq(dble(r1),r2,re)
! print *,"ameq(",r1,",",dble(nearest(r2,1.)), ",",re,")=",   &
!                                             ameq(dble(r1),nearest(r2,1.),re)

! print *,"ameq(",r3,",",dble(r4),",",re,")=",ameq(dble(r3),r4,re)
! print *,"ameq(",r3,",",dble(nearest(r4,-1.)), ",",re,")=",   &
!                                             ameq(dble(r3),nearest(r4,-1.),re)

! print *,""
! print *,"testing d,r,d (also tests d,d,d)"
! print *,"ameq(",r1,",",dble(r2),",",de,")=",ameq(dble(r1),r2,de)
! print *,"ameq(",r1,",",dble(nearest(r2,1.)), ",",de,")=",   &
!                                             ameq(dble(r1),nearest(r2,1.),de)
! print *,"ameq(",r3,",",dble(r4),",",de,")=",ameq(dble(r3),r4,de)
! print *,"ameq(",r3,",",dble(nearest(r4,-1.)), ",",de,")=",   &
!                                             ameq(dble(r3),nearest(r4,-1.),de)
! print *,""
! print *,"testing d,d,r"  
! print *,"ameq(",d1,",",d2,  ",",re,")=", ameq(d1,d2, re)
! print *,"ameq(",d1,",",nearest(d2,1.d0), ",",re,")=", &
!                                          ameq(d1,nearest(d2,1.d0),re)

! print *,"ameq(",d3,",",d4,  ",",re,")=", ameq(d3,d4, re)
! print *,"ameq(",d3,",",nearest(d4,-1.d0), ",",re,")=", &
!                                          ameq(d3,nearest(d4,-1.d0),re)

! print *,""
! print *,"testing r,r,i"
! print *,"ameq(",r1,",",dble(rn1),",1)=",    ameq(r1,rn1,1)
! print *,"ameq(",r1,",",dble(rn2),",1)=",    ameq(r1,rn2,1)
! print *,"ameq(",r1,",",dble(rn2),",2)=",    ameq(r1,rn2,2)
! print *,"ameq(",r3,",",dble(rn3),",1)=",    ameq(r3,rn3,1)
! print *,"ameq(",r3,",",dble(rn4),",1)=",    ameq(r3,rn4,1)
! print *,"ameq(",r3,",",dble(rn4),",2)=",    ameq(r3,rn4,2)

! print *,""
! print *,"testing d,d,i"
! print *,"ameq(",d1,",",dn1,",1)=",    ameq(d1,dn1,1)
! print *,"ameq(",d1,",",dn2,",1)=",    ameq(d1,dn2,1)
! print *,"ameq(",d1,",",dn2,",2)=",    ameq(d1,dn2,2)
! print *,"ameq(",d3,",",dn3,",1)=",    ameq(d3,dn3,1)
! print *,"ameq(",d3,",",dn4,",1)=",    ameq(d3,dn4,1)
! print *,"ameq(",d3,",",dn4,",2)=",    ameq(d3,dn4,2)

! print *,""
! print *,"testing r,d,i"
! print *,"ameq(",r1,",",dn1,",1)=",    ameq(r1,dn1,1)
! print *,"ameq(",r1,",",dn2,",1)=",    ameq(r1,dn2,1)
! print *,"ameq(",r1,",",dn2,",2)=",    ameq(r1,dn2,2)
! print *,"ameq(",r3,",",dn3,",1)=",    ameq(r3,dn3,1)
! print *,"ameq(",r3,",",dn4,",1)=",    ameq(r3,dn4,1)
! print *,"ameq(",r3,",",dn4,",2)=",    ameq(r3,dn4,2)

! print *,""
! print *,"testing d,r,i"
! print *,"ameq(",dn1,",",dble(r1),",1)=",    ameq(dn1,r1,1)
! print *,"ameq(",dn2,",",dble(r1),",1)=",    ameq(dn2,r1,1)
! print *,"ameq(",dn2,",",dble(r1),",2)=",    ameq(dn2,r1,2)
! print *,"ameq(",dn3,",",dble(r3),",1)=",    ameq(dn3,r3,1)
! print *,"ameq(",dn4,",",dble(r3),",1)=",    ameq(dn4,r3,1)
! print *,"ameq(",dn4,",",dble(r3),",2)=",    ameq(dn4,r3,2)

! print *,""
! print *,"testing r,r"
! print *,"ameq(",r1,",",dble(rn1),")=",      ameq(r1,rn1)
! print *,"ameq(",r1,",",dble(rn2),")=",      ameq(r1,rn2)
! print *,"ameq(",r3,",",dble(rn3),")=",      ameq(r1,rn3)
! print *,"ameq(",r3,",",dble(rn4),")=",      ameq(r1,rn4)

! print *,""
! print *,"testing d,d"
! print *,"ameq(",d1,",",dn1,")=",      ameq(d1,dn1)
! print *,"ameq(",d1,",",dn2,")=",      ameq(d1,dn2)
! print *,"ameq(",d3,",",dn3,")=",      ameq(d3,dn3)
! print *,"ameq(",d3,",",dn4,")=",      ameq(d3,dn4)

! print *,""
! print *,"testing r,d"
! print *,"ameq(",r1,",",dn1,")=",      ameq(r1,dn1)
! print *,"ameq(",r1,",",dn2,")=",      ameq(r1,dn2)
! print *,"ameq(",r3,",",dn3,")=",      ameq(r1,dn3)
! print *,"ameq(",r3,",",dn4,")=",      ameq(r1,dn4)

! print *,""
! print *,"testing d,r"
! print *,"ameq(",dn1,",",dble(r1),")=",      ameq(dn1,r1)
! print *,"ameq(",dn2,",",dble(r1),")=",      ameq(dn2,r1)
! print *,"ameq(",dn3,",",dble(r3),")=",      ameq(dn3,r3)
! print *,"ameq(",dn4,",",dble(r3),")=",      ameq(dn4,r3)

!end program test 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
