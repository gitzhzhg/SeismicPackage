!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ timeglob.f90 ------------------------------!!
!!------------------------------ timeglob.f90 ------------------------------!!
!!------------------------------ timeglob.f90 ------------------------------!!

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
! Name       : TIMEGLOB 
! Category   : miscellaneous
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2003-05-16   by: Douglas Hanson  add functions
! Maturity   : production   2003-06-05
! Purpose    : get and put time globals ndpt, tstr, dt
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!  Get and put time grid globals
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
!
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
!
!-------------------------------------------------------------------------------
!</global_doc>

 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS        
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in   ) = value required upon INPUT.
!   o = intent(  out) = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!
!                  o                i     b      o
!                hello = TIMEGLOB     (aaaa, cvar , msg)
!
!                                   i     b      o
!                call    TIMEGLOB_YYY (bbbb, cvar , msg)
!
!                                        opt    opt
!                                   i     i      o
!                call    TIMEGLOB_ZZZ (bbbb, indx, value)
!
! character(len=*)           aaaa(*) =    --> description 
! character(len=8),pointer   bbbb(:) =    --> description 
! double precision           cvar    =    --> description
! character(len=*)           msg     =    --> description 
! integer                    hello   =    --> description
! integer         ,optional  indx    =    --> description
! double precision,optional  value   =    --> description
!
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author         Description
!     ----        ------         -----------
!  4. 2003-06-05  Douglas Hanson add functions
!  3. 2001-01-10  Douglas Hanson cpsfcr
!  2. 2000-05-19  Brad Kruse     Converted from old system.
!  1. 2000-05-01  Douglas Hanson Initial version.
!
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


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!
!
!-------------------------------------------------------------------------------
!</programming_doc>
!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
module timeglob_module
  !
  ! - Module references
  !
  use pc_module
  !
  implicit none
  !
  private
  !
  public :: timeglob_put ! put time grid globals
  public :: timeglob_get ! get time grid globals
  !
  interface timeglob_get
    !
    module procedure timeglob_get_3
    module procedure timeglob_get_4
    !
  end interface 
  !
  character(len=100),public,save :: TIMEGLOB_IDENT = &
    '$Id: timeglob.f90,v 1.4 2003/06/04 15:20:53 Hanson prod sps $'
  !
  contains
  !
  subroutine timeglob_get_3 ( nt_inp, t0_inp, dt_inp )
    !
    ! - get the three time grid globals nt_inp, t0_inp, dt_inp
    !
    !
    ! - Arguments
    !
    integer, intent(  out) :: nt_inp
    real,    intent(  out) :: t0_inp
    real,    intent(  out) :: dt_inp
    !
    ! - Begin timeglob_get
    !
    call pc_get_global ( 'NDPT' , nt_inp )
    !
    call pc_get_global ( 'TSTRT', t0_inp )
    !
    call pc_get_global ( 'DT'   , dt_inp )
    !
  end subroutine timeglob_get_3
  !
  subroutine timeglob_get_4 ( nt_inp, t0_inp, t1_inp, dt_inp )
    !
    ! - get the four time grid globals nt_inp, t0_inp, t1_inp, dt_inp
    !
    !
    ! - Arguments
    !
    integer, intent(  out) :: nt_inp
    real,    intent(  out) :: t0_inp
    real,    intent(  out) :: t1_inp
    real,    intent(  out) :: dt_inp
    !
    ! - Begin timeglob_get
    !
    call timeglob_get_3 ( nt_inp, t0_inp, dt_inp )
    !
    t1_inp = (nt_inp - 1 ) * dt_inp + t0_inp
    !
  end subroutine timeglob_get_4
  !
  subroutine timeglob_put ( nt_inp, t0_inp, dt_inp )
    !
    ! - put the three time grid globals nt_inp, t0_inp, dt_inp
    !
    !
    ! - Arguments
    !
    integer, intent(in   ) :: nt_inp
    real,    intent(in   ) :: t0_inp
    real,    intent(in   ) :: dt_inp
    !
    ! - Begin timeglob_put
    !
    call pc_put_global ( 'NDPT' , nt_inp )
    !
    call pc_put_global ( 'TSTRT', t0_inp )
    !
    call pc_put_global ( 'DT'   , dt_inp )
    !
  end subroutine timeglob_put
  !
end module timeglob_module
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

