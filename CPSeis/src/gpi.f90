!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- gpi.f90 --------------------------------!!
!!------------------------------- gpi.f90 --------------------------------!!
!!------------------------------- gpi.f90 --------------------------------!!

        ! other files are:  gpi_crou.c
 


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
! Name       : GPI 
! Category   : miscellaneous
! Written    : 2002-08-09   by: Ed Schmauch
! Revised    : 2002-08-09   by: Ed Schmauch
! Maturity   : production   2002-09-11
! Purpose    : Provide CFE with interface to gnuplot.
! Portability: Requires unix, gnuplot, and posix threads.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! GPI provides CFE with an interface to gnuplot.
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
!                  o
!                 lun   = gpi_open()
!                     Open a gnuplot display.  Returns logical unit number >=0
!                     which is used to refer to gnuplot in gpi_close and
!                     gpi_command functions.  Returns -1 if fails.
!
!                  o                 i
!                status = gpi_close(lun)
!                     Close a gnuplot display.  Returns 0 if ok, -1 if fails.
!
!                  o
!                status = gpi_close_all()
!                     Close all gnuplot displays opened by current execution.
!                     Returns 0 if ok, -1 * number of failed closes if fails.
!                     Invoking gpi_close_all with no open gnuplot displays
!                     is valid and will not cause any error.
!
!                  o                   i    i
!                status = gpi_command(lun, cmd)
!                     Send a command to a gnuplot display.  Returns 0 if ok,
!                     -1 if fails.  0 does not mean that gnuplot successfully
!                     executed the command, it simply means that the command
!                     was successfully sent.  To indicate an error in executing
!                     the command, gnuplot error messages are output to
!                     standard error.
!
! integer lun          = Logical unit number for a specific gnuplot.
! integer status       = 0 if ok, <0 if failure.
! character(len=*) cmd = Command for gnuplot.
!
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR DEVELOPERS
!
!  Consult the gnuplot manual at:
!     http://www.fnal.gov/docs/products/gnuplot/manual
!
!  Use gnuplot standalone to determine the commmands you need before
!     implementing the commands in your CPS code.
!
!  To view gnuplot error messages from cfe use:
!     cfecustom -o > /dev/null
!  Sending standard out to /dev/null will deobfuscate the gnuplot errors
!  which go to standard error.
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2002-09-11  Ed Schmauch Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! Requires unix, gnuplot, and posix threads.
!
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!
! gnuplot is started with the popen call and written to with the FILE*
! returned from the popen.  A fifo is created and gnuplot stdout and stderr
! are sent to the fifo.  A pthread is used to read from the fifo and print
! any gnuplot errors.
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


module gpi_module

   use pc_module
   use string_module
   use cfejava_module
   implicit none
   private
   public :: gpi_open
   public :: gpi_close
   public :: gpi_close_all
   public :: gpi_command


      character(len=100),public,save :: GPI_IDENT = &
'$Id: gpi.f90,v 1.1 2002/09/10 17:44:08 Schmauch prod sps $'


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


   !
   ! Interfaces to gpi_crou.c
   !

   interface
      function gpi_open_c() result(lun)
         integer :: lun
      end function gpi_open_c
   end interface

   interface
      function gpi_close_c(lun) result(status)
         integer, intent(in) :: lun
         integer             :: status
      end function gpi_close_c
   end interface

   interface
      function gpi_close_all_c() result(status)
         integer :: status
      end function gpi_close_all_c
   end interface

   interface
      function gpi_command_c(lun, cmd) result(status)
         integer, intent(in)               :: lun
         integer, intent(in), dimension(*) :: cmd
         integer                           :: status
      end function gpi_command_c
   end interface


!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!


   contains


   function gpi_can_do() result (status)
      logical :: status

      if (.not. pc_do_not_process_traces()) then
         call pc_error('gpi function called in backend')
         status = .false.
      elseif (cfejava_get_execution_mode() /= CFEJAVA_DIRECTAPP) then
         call pc_error('gpi function called from client server application')
         status = .false.
      else
         status = .true.
      endif

   end function gpi_can_do


   function gpi_open() result (lun)
      integer :: lun

      if (gpi_can_do()) then
         lun = gpi_open_c()
      else
         lun = -1
      endif

   end function gpi_open


   function gpi_close(lun) result (status)
      integer, intent(in) :: lun
      integer             :: status

      if (gpi_can_do()) then
         status = gpi_close_c(lun)
      else
         status = -1
      endif

   end function gpi_close


   function gpi_close_all() result (status)
      integer :: status

      if (gpi_can_do()) then
         status = gpi_close_all_c()
      else
         status = -1
      endif

   end function gpi_close_all


   function gpi_command(lun, cmd) result (status)
      integer         , intent(in)   :: lun
      character(len=*), intent(in)   :: cmd
      integer                        :: status

      integer                        :: dstat

      integer, pointer, dimension(:) :: icmd

      if (gpi_can_do()) then

         nullify(icmd)
         call string_cc2hh_alloc(cmd, icmd)

         status = gpi_command_c(lun, icmd)

         deallocate(icmd, stat=dstat)
         if (dstat /= 0) then
            call pc_error("gpi_command deallocate error")
            status = -1
         endif

      else

         status = -1

      endif

   end function gpi_command


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


end module gpi_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

