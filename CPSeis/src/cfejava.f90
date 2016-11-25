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
! Name       : cfejava 
! Category   : miscellaneous
! Written    : 2002-07-24   by: Donna K. Vunderink
! Revised    : 2002-07-24   by: Donna K. Vunderink
! Maturity   : production   2002-08-12
! Purpose    : Get and set CFE Java application execution mode
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This module is to be used by an application written to using the CFE Java
! toolkit.  It provides a means to inform the C/Fortran code if the applciation
! is executing in single direct or client/server mode.
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
!                                               i
!                call cfejava_set_execution_mode (mode)
!
!                 o
!                mode = cfejava_get_execution_mode()
!
!
! integer mode = execution mode of Java application
!                    CFEJAVA_UNKNOWN    = Unknown (state if set not called)
!                    CFEJAVA_DIRECTAPP  = Single direct application
!                    CFEJAVA_CSAPP      = Client/Server application
!
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2002-08-12  Vunderink  Initial version.
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


      module cfejava_module
      implicit none
      private


      character(len=100),public,save :: cfejava_ident = &
'$Id: cfejava.f90,v 1.1 2002/08/07 15:56:12 Vunderink prod sps $'


      public :: cfejava_set_execution_mode
      public :: cfejava_get_execution_mode

      integer,parameter,public    :: CFEJAVA_UNKOWN     = 0
      integer,parameter,public    :: CFEJAVA_DIRECTAPP  = 1
      integer,parameter,public    :: CFEJAVA_CSAPP      = 2

      integer,save,private        :: exemode            = 0

      contains

!!-------------------- cfejava_set_execution_mode -------------------------!!
!!-------------------- cfejava_set_execution_mode -------------------------!!
!!-------------------- cfejava_set_execution_mode -------------------------!!


      subroutine cfejava_set_execution_mode (mode)
      implicit none
      integer,intent(in) :: mode                                ! argument

      exemode = mode

      return
      end subroutine cfejava_set_execution_mode


!!-------------------- cfejava_get_execution_mode -------------------------!!
!!-------------------- cfejava_get_execution_mode -------------------------!!
!!-------------------- cfejava_get_execution_mode -------------------------!!


      function cfejava_get_execution_mode () result (mode)
      implicit none
      integer :: mode                                           ! result

      mode = exemode

      return
      end function cfejava_get_execution_mode


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module cfejava_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

