!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- lnklst.f90 --------------------------------!!
!!------------------------------- lnklst.f90 --------------------------------!!
!!------------------------------- lnklst.f90 --------------------------------!!

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
! Name       : lnklst
! Category   : miscellaneous
! Written    : 2002-09-23   by: Charles C Burch
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Repository for miscellaneous link list subprograms
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!</calling_doc>
                                                                          

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!002. 2006-06-20  B. Menger   Removed Unused Variables.
!  1. 2003-02-27  C C Burch  Initial version
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
                                                                             
  module lnklst_module                                                         
    implicit none                                                          
    private                                                             
 
    character(len=100),public,save :: LNKLST_IDENT = &
      '$Id: lnklst.f90,v 1.2 2006/06/20 13:11:58 Menger prod sps $'

  interface lnklst_get_var
    module procedure lnklst_get_var_a
    module procedure lnklst_get_var_i
    module procedure lnklst_get_var_r
    module procedure lnklst_get_var_d
    module procedure lnklst_get_var_z
  end interface

  interface lnklst_put_var
    module procedure lnklst_put_var_a
    module procedure lnklst_put_var_i
    module procedure lnklst_put_var_r
    module procedure lnklst_put_var_d
    module procedure lnklst_put_var_z
  end interface

  public :: lnklst_get_var
  public :: lnklst_put_var

  contains                                                         

!---------------------- GET/PUT_VAR -----------------------
! get/put_var components:part of first-in first-out queue
! 
! Written April 2001 by Charles C Burch
!---------------------------------------------------------
  subroutine lnklst_get_var_a(var, val, nval)     !get character
    character (len=*), intent(in)  :: var
    character (len=*), intent(out) :: val
    integer, optional, intent(out) :: nval

    integer                        :: n, i
    character                      :: buff(len(val)+1)

    call lnklst_get_var_list(trim(var)//char(0), buff)

    n=len(val)
    do i=1,n
      if(buff(i).eq.char(0)) then
        val(i:n)=""
        if(present(nval)) nval=i-1
        return
      endif
      val(i:i)=buff(i)
    enddo

    if(present(nval)) nval=n
    return
  end subroutine lnklst_get_var_a

  subroutine lnklst_put_var_a(var, val, nval)        !put character
    character (len=*), intent(in)  :: var
    character (len=*), intent(in ) :: val
    integer, optional, intent(in ) :: nval

    integer      :: n

    if(present(nval)) then
      n=nval
    else
      n=-1
    endif
    if(n.lt.0) n=len_trim(val)

    if(n.gt.0) then
      call lnklst_put_var_list(trim(var)//char(0),val(1:n)//char(0))
    else
      call lnklst_put_var_list(trim(var)//char(0),char(0))
    endif
    return
  end subroutine lnklst_put_var_a 

  subroutine lnklst_get_var_i(var, val)              !get integer
    character (len=*), intent(in)  :: var
    integer          , intent(out) :: val

    character (len=20) :: buff
    integer            :: n

    call lnklst_get_var_a(var,buff, n)
    if(n.gt.0) then
      read(buff(1:n),*) val
    else
      val=0
    endif

    return
  end subroutine lnklst_get_var_i

  subroutine lnklst_put_var_i(var, val)            !put integer
    character (len=*), intent(in)  :: var
    integer          , intent(in ) :: val

    character (len=20) :: buff
    integer            :: temp

    write(buff,*) val
    buff=adjustl(buff)
    temp = len_trim(buff)
    call lnklst_put_var_a(var,buff,temp)
    return
  end subroutine lnklst_put_var_i

  subroutine lnklst_get_var_r(var, val)             !get real
    character (len=*), intent(in)  :: var
    real             , intent(out) :: val

    character (len=20) :: buff
    integer            :: n

    call lnklst_get_var_a(var,buff, n)
    if(n.gt.0) then
      read(buff(1:n),*) val
    else
      val=0
    endif

    return
  end subroutine lnklst_get_var_r

  subroutine lnklst_put_var_r(var, val)              !put real
    character (len=*), intent(in)  :: var
    real             , intent(in ) :: val

    character (len=20) :: buff
    integer            :: temp


    write(buff,*) val
    buff=adjustl(buff)

    temp = len_trim(buff)
    call lnklst_put_var_a(var,buff, temp)
    return
  end subroutine lnklst_put_var_r

  subroutine lnklst_get_var_d(var, val)             !get double
    character (len=*), intent(in)  :: var
    double precision , intent(out) :: val

    character (len=30) :: buff
    integer            :: n

    call lnklst_get_var_a(var,buff, n)
    if(n.gt.0) then
      read(buff(1:n),*) val
    else
      val=0
    endif

    return
  end subroutine lnklst_get_var_d

  subroutine lnklst_put_var_d(var, val)              !put double
    character (len=*), intent(in)  :: var
    double precision , intent(in ) :: val

    character (len=30) :: buff
    integer            :: temp


    write(buff,*) val
    buff=adjustl(buff)
    temp = len_trim(buff)
    call lnklst_put_var_a(var,buff, temp)
    return
  end subroutine lnklst_put_var_d

  subroutine lnklst_get_var_z(var, val)                !get complex
    character (len=*), intent(in)  :: var
    complex          , intent(out) :: val

    character (len=40) :: buff
    integer            :: n

    call lnklst_get_var_a(var,buff, n)
    if(n.gt.0) then
      read(buff(1:n),*) val
    else
      val=0
    endif
    return
  end subroutine lnklst_get_var_z

  subroutine lnklst_put_var_z(var, val)                !put complex
    character (len=*), intent(in)  :: var
    complex          , intent(in ) :: val

    character (len=40) :: buff
    integer            :: temp


    write(buff,*) val
    buff=adjustl(buff)
    temp      = len_trim(buff)
    call lnklst_put_var_a(var,buff,temp)
    return
  end subroutine lnklst_put_var_z
                                                                    

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

  end module lnklst_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
