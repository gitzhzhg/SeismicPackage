!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- unix.f90 --------------------------------!!
!!------------------------------- unix.f90 --------------------------------!!
!!------------------------------- unix.f90 --------------------------------!!
! other files are:  unix_crou.c

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
! Name       : unix 
! Category   : miscellaneous
! Written    : 2000-04-04   by: Bill Menger
! Revised    : 2005-01-31   by: Bill Menger
! Maturity   : production
! Purpose    : Provide access to unix system features
! Portability: No known limitations except perhaps cray's O/S.
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!  Seeing the need for access to a sleep function, a fork function, and
!  general access to the operating system, these interfaces were created.
!   ----------------------------------------------------------------------------
!   unix_abort            issue a sig_abrt with printed message
!   unix_fork             fork the process return the pid to parent
!   unix_get_cmdline      return #args and passed args to f90 (like argc, argv)
!   unix_sleep            sleep for n seconds
!   unix_sleep_random     randomly sleep (0-10 seconds by default)
!   unix_set_sleep_max    set max seconds for sleep_random (default=10)
!   unix_system           send a shell a command (optional shell argument)
!   unix_system_timeout   send a shell a command with timeout (opt. sh. arg.)
!   unix_umask            set the umask
!   unix_utime            return user+system time for process
!   unix_wtick            return the clock tick in seconds
!   unix_wtime            return elapsed time for process
!   unix_waitpid          wait for child to return
!   unix_get_host_name      return host name without ".domain.network"
!   unix_get_user_name      return current user's name
!   unix_get_user_dir       return current user's home directory
!   unix_is_process_active  return 1 if process is active, 0 if not.
!   ----------------------------------------------------------------------------

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
!  io = intent(inout) = value BOTH required upon input and changed upon output.
! opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!                  opt
! call  unix_abort(message)
!       Purpose to issue a SIG_ABRT which aborts the program
!       if message is provided, it is printed out before the abort
!
!-------------------------------------------------------------------------------
! pid = unix_fork()
!       Purpose:  To provide FORTRAN the ability to fork a process.
!       Return:  pid=0 (I am the child) pid>0 (pid of child, I am parent)
!                pid < 0  (error condition)
!-------------------------------------------------------------------------------
!                         o    o
! call  unix_get_cmdline(argc,argv)
!       Purpose: Get the command line of the running process
! integer,intent(out)              argc = number of arguments to running process
! character(len=*),dimension(:),intent(out) :: argv(0:) list of arguments,
!                                  argv(0) = name of process
!                                  argv(1) = first argument,...
!                                  ...
!                                  argv(argc) = last argument
!-------------------------------------------------------------------------------
! Purpose of the "get" commands, is to return system/user information
!                           O        I
! call  unix_get_host_name(name, lengthofname)
! call  unix_get_user_name(name, lengthofname)
! call  unix_get_user_dir (name, lengthofname)
!-------------------------------------------------------------------------------
! Purpose is to discover if a process is running on a remote machine.
!                                  I    I
! status = unix_is_process_active(host,pid)
! Integer Status == 1 for process "pid" running on host "host"
!                   0 if process does not exist.
!-------------------------------------------------------------------------------
!                         i
! call  unix_sleep(seconds_to_sleep)
!       Purpose: Cause process to sleep for a number of seconds.
!-------------------------------------------------------------------------------
! call  unix_sleep_random()
!       Purpose: Cause process to sleep for random number of seconds, from
!                1 to max_seconds_for_sleep_random.  max is defaulted to 10,
!                but can be set differently below.
!-------------------------------------------------------------------------------
!                                       i
! call  unix_set_sleep_max(max_seconds_for_sleep_random)
!       Purpose: Used with unix_sleep_random to set maximum sleep time.
!
!-------------------------------------------------------------------------------
!                        i            opt(i) 
! status = unix_system(command_string,shell)
!       Purpose: To provide access to unix commands.  You provide a command
!                and an optional shell to run the command. (default shell is
!                "/bin/sh" but can be a program like /bin/awk if you wish.)
!                Return code = 0 for success. otherwise, error or as defined
!                by the shell.
!
!
! character(len=*)           command_string = unix command to perform
! character(len=*),optional           shell = optional shell to run
!
!-------------------------------------------------------------------------------
!      NOT    YET   IMPLEMENTED FULLY --- TIMEOUT IGNORED --- 
!                                i              i      opt(i) 
! status = unix_system_timeout(command_string,timeout, shell)
!       Purpose: To provide access to unix commands.  You provide a command
!                and an optional shell to run the command. (default shell is
!                "/bin/sh" but can be a program like /bin/awk if you wish.)
!                Return code = 0 for success. otherwise, error or as defined
!                by the shell.
!
!
! character(len=*)           command_string = unix command to perform
! integer                           timeout = finish within this time(sec)
!                                             [OR RETURN WITH FAILURE CODE]
! character(len=*),optional           shell = optional shell to run
!
!-------------------------------------------------------------------------------
!                    i
! call  unix_umask(imask)
!       Purpose: Change system umask (typical value for imask = 022)
!-------------------------------------------------------------------------------
!                    opt(i)
! call  unix_utime(start_time)
!       Purpose: Return most accurate system+user time spent within process.
!                (works just like unix_wtime below)
!-------------------------------------------------------------------------------
! wtick = unix_wtick()
!       Purpose: Provide an approximate system "tick" time in seconds.
!
!-------------------------------------------------------------------------------
!                      opt(i)
! wtime = unix_wtime(start_time)
!       Purpose: Provide the most accurate system wall clock time available.
!                This provides time in double precision, and appears
!                to use the system "tick" to make a very accurate timer.
!       Usage:   Initial call, use it to set the time as follows:
!                start_time = unix_wtime() , then after events to be timed
!                are completed, use
!                elapsed_time = unix_wtime(start_time).
!
!-------------------------------------------------------------------------------
!                        i
! status = unix_waitpid(pid)
!        Purpose: To wait for a given fork pid to finish and return status
!        status=-1 means error occurred
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  7. 2005-01-31  Bill Menger  Modified get host, dir, username calls
!                              (Mike Ried -- added unix_errmsg)
!  6. 2004-08-12  Bill Menger  Added system_timeout function, repaired the
!                              call to passwd struct, added streamlined code
!                              for passwd and system information, and added
!                              fortran interface for "is process active".
!  5. 2004-01-21  Bill Menger  Added unix_utime (returns user+system time for
!                              your process) and added 
!                              unix_get_cmdline to get the command line args.
!  4. 2003-09-04  SMCook       Added unix_umask
!  3. 2003-06-25  C C Burch    Added unix_get_pid, unix_get_user_name, and
!                              unix_get_user_dir
!  2. 2002-05-15  C C Burch    Added unix_abort and alphabetized lists
!                              Included simple test driver
!  1. 2000-04-04  Bill Menger  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
! No known problems
!-------------------------------------------------------------------------------
!</portability_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
module unix_module

  implicit none
  private

  public :: unix_abort
  public :: unix_errmsg
  public :: unix_fork
  public :: unix_get_cmdline
  public :: unix_get_host_name
  public :: unix_get_pid
  public :: unix_get_user_name
  public :: unix_get_user_dir
  public :: unix_mem_usage
  public :: unix_set_sleep_max
  public :: unix_sleep
  public :: unix_sleep_random
  public :: unix_system
  public :: unix_system_timeout
  public :: unix_umask
  public :: unix_utime
  public :: unix_waitpid
  public :: unix_wtick
  public :: unix_wtime
  public :: unix_is_process_active

  character(len=100),parameter,public :: unix_ident = &
  '$Id: unix.f90,v 1.7 2005/01/31 14:10:49 Menger prod sps $'

  integer,save         :: unix_isleep_max=10

! --- Interface to unix_crou.c routines

  interface unix_is_process_active
    integer function unix_is_process_active_f(host,pid)
      character(len=*),intent(in)   :: host
      integer,intent(in)           :: pid
    end function unix_is_process_active_f
  end interface

  interface
    subroutine unix_get_cmdline_c(argc, argv,lenargv)
      integer,intent(out)          :: argc
      character(len=*),intent(out) :: argv
      integer,intent(in)           :: lenargv
    end subroutine unix_get_cmdline_c
  end interface

  interface
    subroutine unix_abort_c(message)
      character(len=*),intent(in) :: message
    end subroutine unix_abort_c
  end interface

  interface 
    subroutine unix_get_host_name_f(host, n)
      character(len=*), intent(out) :: host
      integer, intent(in)    :: n
    end subroutine unix_get_host_name_f
  end interface

  interface 
    subroutine unix_get_pid_f(pid)
      integer, intent(out) :: pid
    end subroutine unix_get_pid_f
  end interface

  interface 
    subroutine unix_get_user_dir_f(dir, n)
      character(len=*),intent(out)  :: dir
      integer, intent(in)    :: n
    end subroutine unix_get_user_dir_f
  end interface

  interface 
    subroutine unix_get_user_name_f(name, n)
      character(len=*), intent(out) :: name
      integer, intent(in)    :: n
    end subroutine unix_get_user_name_f
  end interface

  interface
    subroutine unix_errmsg_c(message)
      character(len=*),intent(in) :: message
    end subroutine unix_errmsg_c
  end interface

  interface
    function unix_system_c(command,shell) result (status)
      character(len=*),intent(in) :: command
      character(len=*),intent(in) :: shell
      integer                     :: status
    end function unix_system_c
  end interface

  interface
    function unix_system_timeout_c(command,timeout,shell) result (status)
      character(len=*),intent(in) :: command
      integer         ,intent(in) :: timeout
      character(len=*),intent(in) :: shell
      integer                     :: status
    end function unix_system_timeout_c
  end interface

  interface
    function unix_wtime_c(start_time) result (elapsed_time)
      double precision, intent(in) :: start_time
      double precision             :: elapsed_time
    end function unix_wtime_c
  end interface

  interface
    function unix_utime_c(start_time) result (elapsed_time)
      double precision, intent(in) :: start_time
      double precision             :: elapsed_time
    end function unix_utime_c
  end interface

  interface unix_mem_usage
    subroutine unix_mem_usage_c(line,i)
      character(len=*) :: line
      integer          :: i
    end subroutine unix_mem_usage_c
  end interface


! --- interface to unix.f90 routines

  interface unix_abort
    module procedure unix_abort_message
    module procedure unix_abort_no_message
  end interface
  
  interface unix_fork
    function unix_fork_c() result(pid)
      integer     :: pid
    end function unix_fork_c
  end interface

  interface unix_errmsg
    module procedure unix_errmsg_message
  end interface

  interface unix_sleep
    subroutine unix_sleep_c(seconds)
      integer,intent(in)          :: seconds
    end subroutine unix_sleep_c
  end interface

  interface unix_umask
    subroutine unix_umask_c(imask)
      integer,intent(in)          :: imask
    end subroutine unix_umask_c
  end interface

  interface unix_waitpid
    function unix_waitpid_c(pid) result(status)
      integer, intent(in) :: pid
      integer             :: status
    end function unix_waitpid_c
  end interface

  interface unix_wtime
    module procedure unix_wtime_fd
  end interface

  interface unix_utime
    module procedure unix_utime_fd
  end interface

  contains

! --- abort routines

  subroutine unix_abort_message(message)
    character(len=*), intent(in) :: message

    call unix_abort_c(trim(message)//char(0))
  end subroutine unix_abort_message
  
  subroutine unix_abort_no_message()

    call unix_abort_c(char(0))
  end subroutine unix_abort_no_message

! --- errmsg routine

  subroutine unix_errmsg_message(message)
    character(len=*), intent(in) :: message

    call unix_errmsg_c(trim(message)//char(0))
  end subroutine unix_errmsg_message

! --- sleep routines

  subroutine unix_sleep_random()
    integer :: isleep
    real    :: x
    call random_number(x)
    isleep = nint(unix_isleep_max*x)
    if(isleep == 0 ) isleep = unix_isleep_max
    call unix_sleep(isleep)
  end subroutine unix_sleep_random  

  subroutine unix_set_sleep_max(isleep_max)
    integer,intent(in)           :: isleep_max

    unix_isleep_max   = isleep_max
  end subroutine unix_set_sleep_max
  
! --- system routines

  function unix_system(command,shell) result (status)
    character(len=*),intent(in)          :: command
    character(len=*),intent(in),optional :: shell
    integer                              :: status
    character(len=64)                    :: defaultshell

    if(present(shell) ) then
      defaultshell = trim(shell)
    else
      defaultshell = '/bin/sh'
    endif 

    status = unix_system_c(trim(command)//char(0),&
                           trim(defaultshell)//char(0))
  end function unix_system


  function unix_system_timeout(command,timeout,shell) result (status)
    character(len=*),intent(in)          :: command
    integer,intent(in)                   :: timeout
    character(len=*),intent(in),optional :: shell
    integer                              :: status
    character(len=64)                    :: defaultshell

    if(present(shell) ) then
      defaultshell = trim(shell)
    else
      defaultshell = '/bin/sh'
    endif 

    if(timeout > 0 ) then

    status = unix_system_timeout_c(trim(command)//char(0),&
             timeout, &
             trim(defaultshell)//char(0))
    else
      status = unix_system_c(trim(command)//char(0),&
                           trim(defaultshell)//char(0))
    endif

  end function unix_system_timeout


! --- wtime routines

  double precision function unix_wtime_fd(start_time) result (elapsed_time)
    double precision, intent(in),optional :: start_time

    if(present(start_time)) then
      elapsed_time = unix_wtime_c(start_time)
    else
      elapsed_time = unix_wtime_c(0d0)
    endif
  end function unix_wtime_fd

  double precision function unix_utime_fd(start_time) result (elapsed_time)
    double precision, intent(in),optional :: start_time

    if(present(start_time)) then
      elapsed_time = unix_utime_c(start_time)
    else
      elapsed_time = unix_utime_c(0d0)
    endif
  end function unix_utime_fd

  double precision function unix_wtick() result (tick_length)
    double precision :: start_time

    tick_length = 0d0
    start_time  = unix_wtime_c(0d0)
    do while (tick_length <= 0d0)
      tick_length = unix_wtime_c(start_time)
    end do
  end function unix_wtick
 
! --- get routines

  subroutine unix_get_pid(pid)
    integer, intent(out) :: pid

    call unix_get_pid_f(pid)
    return
  end subroutine unix_get_pid
  
  subroutine unix_get_user_dir(dir)
    character(len=*), intent(out) :: dir
    integer                       :: temp
    temp = len(dir)-1
    call unix_get_user_dir_f(dir,temp)
    call unix_strip_past_null(dir)
  end subroutine unix_get_user_dir
 
  subroutine unix_get_host_name(name)
    character(len=*), intent(out) :: name
    integer                       :: temp
    temp = len(name)-1
    call unix_get_host_name_f(name,temp)
    call unix_strip_past_null(name)
  end subroutine unix_get_host_name
 
  subroutine unix_get_user_name(name)
    character(len=*), intent(out) :: name
    integer                       :: temp
    temp = len(name)-1
    call unix_get_user_name_f(name,temp)
    call unix_strip_past_null(name)
  end subroutine unix_get_user_name
    
  subroutine unix_get_cmdline(argc, argv)
      integer,intent(out)                       :: argc
      character(len=*),intent(out),dimension(:) :: argv(0:)
      character(len=4096)                       :: cargv
      integer                                   :: i, istrt,iend
      character(len=1),parameter                :: VTAB=achar(11)
      integer                                   :: temp

      argc=0
      !----- argc will == number of arguments, 3 = 3 passed args, with one
      !----- extra arg being the program name (arg#0).
      !----- So, if you get argc=3, you have 4 strings passed out:
      !----- argv(0) = program_name
      !----- argv(1) = first argument
      !----- argv(2) = second argument
      !----- argv(3) = third argument.
      !----- The function returning cargv (below) embeds ver. tab characters to
      !----- separate args, so an argument CANNOT have an embedded vertic.tab.
      !----- The command line can't be > 4096 characters long.
      temp = len(cargv)
      call unix_get_cmdline_c(argc,cargv,temp)
      !---print*,'argc=',argc
      !---print*,'character string=[',cargv,']'
      istrt=1
      do i = 0, argc
        iend=istrt+index(cargv(istrt:),VTAB)-2
        !---print*,'cargv[',istrt,':',iend,']=',cargv(istrt:iend)
        if(iend < istrt) iend = 4096
        !---print*,'cargv[',istrt,':',iend,']=',cargv(istrt:iend)

        argv(i) = trim(cargv(istrt:iend))

        !---print*,'argv(',i,')=',argv(i)
        !---print*,'argv(',i,')=',argv(i)
        istrt=iend+2
        !---print*,'next characters=cargv(',istrt,':)=',cargv(istrt:)
      end do
      return
  end subroutine unix_get_cmdline

  subroutine unix_strip_past_null(string)
    character(len=*), intent(inout) :: string
    integer                         :: nullpos
    if(len(string) < 1) return
      nullpos = scan(string,char(0))
      if(nullpos > 0 ) string(nullpos:) = ' '
  end subroutine unix_strip_past_null

 
end module unix_module


!!--------------------Basic test driver ----------------------
!program test
!  use unix_module
!  implicit none
!
!  double precision :: t_start
!  integer          :: pid
!  character(len=40):: name
!
!  call unix_get_pid(pid)
!  print *,"pid=",pid

!  call unix_get_user_name(name)
!  print *, "User name=",trim(name)

!  call unix_get_host_name(name)
!  print *,"host name=",trim(name)

!  call unix_get_user_dir(name)
!  print *,"user dir=",trim(name)

!  print *,"system status=",unix_system("hostname")
!
!  t_start=unix_wtime()
!  call unix_sleep(2)
!  print *,"wtime after sleep(2)=",unix_wtime(t_start)
!
!  pid=unix_fork()
!  if(pid==0) then
!    print *,"hello from the fork child"
!    call unix_abort()
!  else
!    print *,"hello from the parent"
!    print *,"waitpid status(6 is SIGABRT)=",unix_waitpid(pid)
!  endif
!
!  call unix_sleep(2)
!  print *,"wtime after fork and sleep(1)=",unix_wtime(t_start)

!  call unix_abort("job being aborted")
!  print *,"This message should not happen"
!
!  call unix_errmsg("an error has occurred")
!end program test

!!------------------------------- end --------------------------------!!
!!------------------------------- end --------------------------------!!
!!------------------------------- end --------------------------------!!

