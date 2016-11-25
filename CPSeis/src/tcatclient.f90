!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- tcatclient.f90 -----------------------------!!
!!---------------------------- tcatclient.f90 -----------------------------!!
!!---------------------------- tcatclient.f90 -----------------------------!!

! other files are:  tcclient.c

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
! Name       : tcatclient 
! Category   : io
! Written    : 2001-10-01   by: Ed Schmauch
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Provide client interface to tapecat server.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! Send a single request to the tapecat server and returns the response and
! error status.
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
!                  o                        i    o
!                status = tcatclient_query(req, resp)
!
! integer                    status    = Return status; 1 = ok, -1 == failed.
! character(len=*)           req       = Request to tapecat server.
! character(len=*)           resp      = Response from tapecat server.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author        Description
!     ----        ------        -----------
!004. 2006-10-16  D. Glover     Added NULLIFY statements for Intel compiler.
!  3. 2003-01-23  Ed Schmauch   Remove server and port from argument list.
!  2. 2002-08-19  Ed Schmauch   Change cps_print to pcps_print.
!  1. 2001-10-18  Ed Schmauch   Initial version.
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

      module tcatclient_module
      use string_module
      use pcps_module

      implicit none

      private
      public :: tcatclient_query

      character(len=100),public,save :: TCAT_CLIENT_IDENT = &
'$Id: tcatclient.f90,v 1.4 2006/10/17 13:45:48 Glover prod sps $'

!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

      interface
        integer function tcclient_start()
        end function tcclient_start
      end interface

      interface
        subroutine tcclient_send(socket, message)
            integer                       ,intent(in)    :: socket
            integer                       ,intent(in)    :: message
        end subroutine tcclient_send
      end interface

      interface
        integer function tcclient_recv(socket, message, max_chars)
            integer                       ,intent(in)    :: socket
            integer                       ,intent(out)   :: message
            integer                       ,intent(in)    :: max_chars
        end function tcclient_recv
      end interface

      interface
        subroutine tcclient_halt(socket)
            integer                       ,intent(in)    :: socket
        end subroutine tcclient_halt
      end interface

      contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

      integer function tcatclient_query(req, resp)

      implicit none
      character(len=*)    :: req
      character(len=*)    :: resp

      integer, pointer     :: ireq   (:)
      integer, allocatable :: iresp  (:)
      integer              :: socket
      integer              :: ierr,temp

      nullify (ireq) ! jpa
      allocate(iresp(1:string_num_integers(resp)), STAT=ierr)
      if (ierr /= 0) then
         call pcps_print('Out of memory in tcatclient_query')
         tcatclient_query = -1
         return
      endif

      call string_cc2hh_alloc(req   , ireq   )

      socket = tcclient_start()
      if (socket > 0) then
         call tcclient_send(socket,ireq(1))
         temp = len(resp)
         ierr=tcclient_recv(socket, iresp(1), temp)
         if (ierr >= 0) then
            call string_hh2cc(iresp, resp)
            tcatclient_query =  1
         else
            call pcps_print('tcatclient_query: tcclient_recv error')
            tcatclient_query = -1
         endif
         call tcclient_halt(socket)
      else
         call pcps_print('tcatclient_query: tcclient_start failed')
         tcatclient_query = -1
      endif

      deallocate(iresp  , STAT=ierr)
      if (ierr /= 0) then
         call pcps_print('Dellocate error in tcatclient_query')
      endif

      deallocate(ireq   , STAT=ierr)
      if (ierr /= 0) then
         call pcps_print('Dellocate error in tcatclient_query')
      endif

      return
      end function tcatclient_query

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module tcatclient_module

!!------------------------------ test code --------------------------------!!
!!------------------------------ test code --------------------------------!!
!!------------------------------ test code --------------------------------!!

!<auto_test>
!
!     program auto_test
!
!     use tcatclient_module
!
!     implicit none
!     integer            :: status
!     character(len=240) :: resp
!
!     status = tcatclient_query('ISCAT=xyzzyx', resp)
!
!     if (status /= 1) then
!        write (6, *) 'tcatclient auto_test failed:  bad status code'
!     else if (resp(1:37) /= 'TAPECAT: ISCAT=N volser=xyzzyx stat=0') then
!        write (6, *) 'tcatclient auto_test failed:  bad response'
!     else
!        write (6, *) 'tcatclient auto_test passed'
!     endif
!
!     end program auto_test
!
!</auto_test>

!!--------------------------- end of test code ----------------------------!!
!!--------------------------- end of test code ----------------------------!!
!!--------------------------- end of test code ----------------------------!!

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
