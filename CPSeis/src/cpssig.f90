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
! Name       : cpssig 
! Category   : main_prog
! Written    : 2002-01-02  by: Donna K. Vunderink
! Revised    : 2002-06-19  by: Donna K. Vunderink
! Maturity   : production  2002-07-01
! Purpose    : Dummy cps signal handler for linking old jobs.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This is a dummy version of a subroutine called by the signal handler in the
! cps backbone.  It is used when linking older jobs that do not have this
! subroutine built into them.  Since this subroutine is called by a C language
! function, it must be a F77 primitive.
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
!                                        i
!                call cpssig_shutdown (signal)
!
! integer  signal = the signal caught by the signal handler 
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  4. 2002-07-01  Vunderink  Added call to cps_write_accounting_signal
!  3. 2002-06-06  Vunderink  Modified to use cps_print_current_status
!  2. 2002-04-22  Vunderink  Improved signal handling
!  1. 2002-01-02  Vunderink  Initial version.
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


!!--------------------------- start of code -------------------------------!!
!!--------------------------- start of code -------------------------------!!
!!--------------------------- start of code -------------------------------!!


      subroutine cpssig_shutdown (signal)

      use cps_module
      use pcps_module
      use cio_module

      implicit none

      character(len=100),save :: CPSSIG_IDENT = &
       '$Id: cpssig.f90,v 1.4 2002/06/19 17:30:57 Vunderink beta sps $'

      integer,intent(in) :: signal                              ! argument

      logical,save       :: flag0 = .true.                      ! local
      integer,parameter  :: pcps_kill_sig=12                    ! local SIGUSR2
      integer            :: lun                                 ! local

      if (flag0) then
        flag0 = .false.

        call cps_write_accounting_signal (signal)
        call cps_print_current_status()

        if (pcps_current_worker_num.eq.0) &
         call cps_finish_processing (signal=signal)  !boss execs only

        if (pcps_num_procs.gt.1) then
          call cio_finalize()
          call pfio_exit()

          if(signal.ne.pcps_kill_sig) then
            call pcps_kill_parallel_cpus (pcps_kill_sig) 
          endif

          lun = cps_get_lun()
          close(unit=lun,status="keep")

        endif

      endif

      return
      end


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

