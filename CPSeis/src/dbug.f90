!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- dbug.f90 --------------------------------!!
!!------------------------------- dbug.f90 --------------------------------!!
!!------------------------------- dbug.f90 --------------------------------!!


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
! Name       : dbug
! Category   : main
! Written    : 2002-07-18   by: C C Burch  
! Revised    : 2002-07-25   by: C C Burch
! Maturity   : production   2002-08-12
! Purpose    : Interface to debug routines
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! These routines provide a means to set a debug message which can later be
! retrieved by a signal handler when an error occurs
!
! Also included is a means to install a default handler which will then
! print out the last stored debug message when an  error occurs 
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!   subroutine dbug_get_message(message)
!     character(len=*), intent(in) :: message
!   Purpose: to get the last stored debug message
!            normally called by signal handler
!
!   subroutine dbug_set_message(message)
!     character(len=*), intent(in) :: message
!   Purpose: to set a debug message
!
!   subroutine dbug_get_system_message(message)
!     character(len=*), intent(in) :: message
!   Purpose: to get the last stored system debug message
!            normally called by signal handler
!
!   subroutine dbug_set_system_message(message)
!     character(len=*), intent(in) :: message
!   Purpose: to set a system debug message
!   Note:    This should only be called by system routines and not applications
!
!   subroutine dbug_install_signal_handler()
!   Purpose: to install a default signal handler to print our last debug message
!            in a non-CPS stand-alone program.
!   Note: this routine should not be called by programs used with CPS as CPS has
!         its own signal handler.
!
!-------------------------------------------------------------------------------
!</calling_doc>
                                                                          

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2002-08-12  C C Burch  Add System debug messages
!  1. 2002-07-18  C C Burch  Initial version
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
                                                                             
  module dbug_module                                                         
    implicit none                                                          
    private                                                             
 
    character(len=100),public,save :: DBUG_IDENT = &
'$Id: dbug.f90,v 1.2 2002/08/07 22:49:42 CCBurch prod sps $'

!********************** interface to c routines **************
    interface 
      subroutine dbug_get_message_f(message, n)
        character, intent(out) :: message
        integer, intent(in)    :: n
      end subroutine dbug_get_message_f

      subroutine dbug_install_c()
      end subroutine dbug_install_c

      subroutine dbug_set_message_f(message, n)
        character, intent(in) :: message
        integer, intent(in)   :: n
      end subroutine dbug_set_message_f

    end interface

!******************** public routines *************************
    public :: dbug_get_message
    public :: dbug_get_system_message
    public :: dbug_set_message
    public :: dbug_set_system_message
    public :: dbug_install_signal_handler

    contains                                                         
    
!***************************************************************
! Retrieve last stored debug message
!
! Written July 2002 by Charles C Burch
!***************************************************************
    subroutine dbug_get_message(message)
      character(len=*), intent(out) :: message
      integer                       :: n

      character                     :: dbug_message(256)

      n=size(dbug_message)
      call dbug_get_message_f(dbug_message(1),n)

      if(n.gt.len(message)) n=len(message)
      write(message,'(256a1)') dbug_message(1:n)
      return
    end subroutine dbug_get_message
    
!***************************************************************
! Set debug message
!
! Written July 2002 by Charles C Burch
!***************************************************************
    subroutine dbug_set_message(message)
      character(len=*), intent(in) :: message
      integer                      :: i, n

      character                    :: dbug_message(256)

      n=len(message)
      if(n.gt.size(dbug_message)) n=size(dbug_message)

      do i=1, n
        dbug_message(i)=message(i:i)
      enddo

      call dbug_set_message_f(dbug_message(1),n)
      return
    end subroutine dbug_set_message

!***************************************************************
! Retrieve last stored system debug message
!
! Written July 2002 by Charles C Burch
!***************************************************************
    subroutine dbug_get_system_message(message)
      character(len=*), intent(out) :: message
      integer                       :: n

      character                     :: dbug_message(256)

      n=size(dbug_message)
      call dbug_get_system_message_f(dbug_message(1),n)

      if(n.gt.len(message)) n=len(message)
      write(message,'(256a1)') dbug_message(1:n)
      return
    end subroutine dbug_get_system_message
    
!***************************************************************
! Set system debug message
!
! Written July 2002 by Charles C Burch
!***************************************************************
    subroutine dbug_set_system_message(message)
      character(len=*), intent(in) :: message
      integer                      :: i, n

      character                    :: dbug_message(256)

      n=len(message)
      if(n.gt.size(dbug_message)) n=size(dbug_message)

      do i=1, n
        dbug_message(i)=message(i:i)
      enddo

      call dbug_set_system_message_f(dbug_message(1),n)
      return
    end subroutine dbug_set_system_message

!***************************************************************
! Install default dbug signal handler
!
! Written July 2002 by Charles C Burch
!***************************************************************
    subroutine dbug_install_signal_handler()
      call dbug_install_signal_handler_c()
      return
    end subroutine dbug_install_signal_handler
    
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

  end module dbug_module


! -----------------basic test driver------------

!program main
!  use dbug_module
!  implicit none
!
!  character(len=80) :: msg
!
!  call dbug_get_message(msg)
!  print *,"msg=",msg
!
!  call dbug_set_message("Message1");
!  call dbug_get_message(msg)
!  print *,"msg=",msg
!
!  call dbug_get_system_message(msg)
!  print *,"system msg=",msg
!
!  call dbug_set_system_message("System Message1");
!  call dbug_get_system_message(msg)
!  print *,"system_msg=",msg
!
!  call dbug_install_signal_handler()
!  call dbug_raise_signal(8)
!
!end program main

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
