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
! Name       : putsys 
! Category   : miscellaneous
! Written    : 1999-07-01   by: Donna K. Vunderink
! Revised    : 2003-04-21   by: SMCook
! Maturity   : production   2003-06-05
! Purpose    : Execute a UNIX command from FORTRAN 90.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! 
! This module is a FORTRAN wrapper for C routines to execute a UNIX command. 
!
! Function                       Description
! --------                       -----------
! putsys_cmd                     Execute a command using the C function system
! putsys_env                     Set a value for an environment variable
! putsys_texec                   Execute a command using an exec to tcsh
! 
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!
! To execute UNIX command using the C function system 
!                                          opt
!                                     i     o
!                call    putsys_cmd (cmd, istat)
!
! character(len=*)  cmd   =   UNIX command to be executed
! integer,optional  istat =   return status
!                             0 = no error occurred
!                            -1 = an error occurred
!
!
!
! To set an environment variable
!                                     i    i     o
!                call    putsys_env (var, val, istat)
!
! character(len=*)  var   =   name of variable to be set
! character(len=*)  val   =   value of variable
! integer           istat =   return status
!                             0 = no error occurred
!                            -1 = an error occurred
!
!
! To execute a UNIX command using an exec to the tcsh shell
!
!                                            opt
!                                       i     o
!                call    putsys_texec (cmd, istat)
!
! character(len=*)  cmd   =   UNIX command to be executed
! integer,optional  istat =   return status
!                             0 = no error occurred
!                            -1 = an error occurred
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! 
! 
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  4. 2003-06-05  SMCook       Added putsys_env. 
!  3. 2001-04-30  Vunderink    Updated documentation tags.
!  2. 2000-03-15  Vunderink    Added routine putsys_texec.
!  1. 1999-07-01  Vunderink    Initial version.
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
! 
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module putsys_module
      use string_module
      implicit none

      private

      public :: putsys_cmd
      public :: putsys_env
      public :: putsys_texec


      contains


!!----------------------------- putsys_cmd --------------------------------!!
!!----------------------------- putsys_cmd --------------------------------!!
!!----------------------------- putsys_cmd --------------------------------!!


      subroutine putsys_cmd (cmd, cmd_stat)

      implicit none
      character(len=*),intent(in)  :: cmd                       ! argument
      integer,optional,intent(out) :: cmd_stat                  ! argument

      integer,allocatable          :: htmp(:)                   ! local
      integer                      :: i                         ! local
      integer                      :: istat                     ! local
      integer                      :: cputsys_cmd               ! local

      if (present(cmd_stat)) cmd_stat = 0
      i = len(cmd)
      allocate(htmp(i+1),stat=istat)
      if (istat .eq. 0) then
         call string_cc2hh (cmd,htmp)
         istat = cputsys_cmd (htmp)
         if (present(cmd_stat)) cmd_stat = istat
         deallocate(htmp,stat=istat)
      else
        if (present(cmd_stat)) cmd_stat = -1
      endif
 
      return
      end subroutine putsys_cmd


!!----------------------------- putsys_env --------------------------------!!
!!----------------------------- putsys_env --------------------------------!!
!!----------------------------- putsys_env --------------------------------!!


      subroutine putsys_env (var, val, istat)

      implicit none
      character(len=*), intent(in)  :: var                      ! argument
      character(len=*), intent(in)  :: val                      ! argument
      integer,optional,intent(out)  :: istat                    ! argument
      integer                       :: cputsys_env              ! local

      integer     :: h1(512)
      integer     :: h2(512)

      h1 = 0
      h2 = 0

      call string_cc2hh(trim(var), h1)
      call string_cc2hh(trim(val), h2)

      istat = cputsys_env(h1,h2)

      return
      end subroutine putsys_env


!!---------------------------- putsys_texec -------------------------------!!
!!---------------------------- putsys_texec -------------------------------!!
!!---------------------------- putsys_texec -------------------------------!!


      subroutine putsys_texec (cmd, cmd_stat)

      implicit none
      character(len=*),intent(in)  :: cmd                       ! argument
      integer,optional,intent(out) :: cmd_stat                  ! argument

      integer,allocatable          :: htmp(:)                   ! local
      integer                      :: i                         ! local
      integer                      :: istat                     ! local
      integer                      :: cputsys_texec             ! local

      if (present(cmd_stat)) cmd_stat = 0
      i = len(cmd)
      allocate(htmp(i+1),stat=istat)
      if (istat .eq. 0) then
         call string_cc2hh (cmd,htmp)
         istat = cputsys_texec (htmp)
         if (present(cmd_stat)) cmd_stat = istat
         deallocate(htmp,stat=istat)
      else
        if (present(cmd_stat)) cmd_stat = -1
      endif
 
      return
      end subroutine putsys_texec


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module putsys_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!


