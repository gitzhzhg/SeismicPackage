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
! Name       : pclun 
! Category   : cfe
! Written    : 2000-08-15   by: Donna K. Vunderink
! Revised    : 2003-08-12   by: Tom Stoeckley
! Maturity   : production   2003-09-15
! Purpose    : Parameter Cache LUN Module.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This primitive is used by cfe.
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
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  2. 2003-09-15  Stoeckley    Changed type to primitive; changed category to
!                               cfe.
!  1. 2000-08-15  Vunderink    Initial version.
!
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module pclun_module

      implicit none

      private
      public :: pclun_set
      public :: pclun_get

      character(len=100),public,save :: pclun_ident = &
       '$Id: pclun.f90,v 1.2 2003/09/12 19:48:03 Stoeckley prod sps $'

      integer,save,private        :: pc_lun = 6


      contains


!!---------------------------- pclun_set -----------------------------------!!
!!---------------------------- pclun_set -----------------------------------!!
!!---------------------------- pclun_set -----------------------------------!!


      subroutine pclun_set (lun)
      implicit none
      integer,intent(in) :: lun                                ! argument

      pc_lun = lun

      return
      end subroutine pclun_set


!!---------------------------- pclun_get -----------------------------------!!
!!---------------------------- pclun_get -----------------------------------!!
!!---------------------------- pclun_get -----------------------------------!!


      function pclun_get () result (lun)
      implicit none
      integer :: lun                                           ! result

      lun = pc_lun

      return
      end function pclun_get

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module pclun_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

