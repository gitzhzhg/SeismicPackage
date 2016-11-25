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
! Name       : GETSYS 
! Category   : miscellaneous
! Written    : 1999-07-01   by: Donna K. Vunderink
! Revised    : 2007-12-20   by: Goodger             
! Maturity   : beta                      
! Purpose    : A collection of routines for getting system information.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION                   
!
! This module is a collection of routines for getting information from system.
!
! Function/Subroutine             Description
! -------------------             -----------
!   getsys_cpu_speed              Get speed of cpu
!   getsys_current_dir            Get current working directory
!   getsys_env                    Get value for an evironment variable
!   getsys_hostname               Get current host name
!   getsys_library                Get CPS library used for link
!   getsys_machine                Get operating system type of a given node
!   getsys_memory                 Get number of cpus and physical memory 
!   getsys_memory_per_cpu         Get the total and available memory divided
!                                   by the number of cpus
!   getsys_my_pid_memory          Get memory usage of the current process 
!   getsys_netinfo                Get items from _netinfo card
!   getsys_os_version             Get OS release
!   getsys_ostype                 Get current operating system type
!   getsys_pid                    Get process ID
!   getsys_seconds                Get processor time used by the current process
!                                 This is a reduced precision value of user time
!   getsys_stime                  Get elapsed system time for the current
!                                   process
!   getsys_usage                  Get information about resource utilization
!   getsys_username               Get current user name
!   getsys_utime                  Get elapsed user time for the current process
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
! To get current host name
!
!                                           o
!                call getsys_hostname    (host)
!
! character(len=*) host  = current host name
!
!-------------------------------------------------------------------------------
! To get current user name
!
!                                          o
!                call getsys_username    (user)
!
! character(len=*) user  = current user name
!
!-------------------------------------------------------------------------------
! To get current working directory
!
!                                           o
!                call getsys_current_dir (cdir)
!
! character(len=*) cdir  = current directory name
!
!-------------------------------------------------------------------------------
! To get value of an environment variable
!
!                                  i      o
!                call getsys_env (name, value)
!
! character(len=*) name   = environment variable name
! character(len=*) value  = environment variable vlaue
!
!-------------------------------------------------------------------------------
! To get current operating system type
!
!                  o
!                ostyp = getsys_ostype ()
!
! integer ostyp = type of operating system (one of the named constants listed 
!                 here)
!                         GETSYS_UNKNOWN   =  Unknown
!                         GETSYS_CRAYPVP   =  Cray PVP
!                         GETSYS_VMS       =  DEC VMS
!                         GETSYS_ULTRIX    =  DEC Ultrix
!                         GETSYS_IBM       =  IBM
!                         GETSYS_SOLARIS   =  Solaris
!                         GETSYS_HP        =  HP
!                         GETSYS_SGI       =  SGI
!                         GETSYS_CRAYMPP   =  Cray MPP
!                         GETSYS_INTELSOL  =  Intel Solaris
!                         GETSYS_LINUX     =  Linux
!
!-------------------------------------------------------------------------------
! To get operating system type of a given node
!
!                  o                         i
!                ostyp = getsys_machine (machine)
!
! character(len=*) machine  = machine node name
! integer          ostyp    = type of operating system (one of the named
!                             constants listed here)
!
!                                  GETSYS_UNKNOWN   =  Unknown
!                                  GETSYS_CRAYPVP   =  Cray PVP
!                                  GETSYS_VMS       =  DEC VMS
!                                  GETSYS_ULTRIX    =  DEC Ultrix
!                                  GETSYS_IBM       =  IBM
!                                  GETSYS_SOLARIS   =  Solaris
!                                  GETSYS_HP        =  HP
!                                  GETSYS_SGI       =  SGI
!                                  GETSYS_CRAYMPP   =  Cray MPP
!                                  GETSYS_INTELSOL  =  Intel Solaris
!                                  GETSYS_LINUX     =  Linux
!
!-------------------------------------------------------------------------------
! To get items from _netinfo card
!
!                                            o         o         o
!                call getsys_netinfo     (net_node, net_user, net_path)
!
! character(len=*) net_node = node name from _netinfo card
! character(len=*) net_user = user name from _netinfo card
! character(len=*) net_path = path name from _netinfo card
!
!-------------------------------------------------------------------------------
! To get processor time used by the current process
!
!                  o
!                ptime = getsys_seconds()
!
! real ptime  = processor time in seconds
!
!                  o
!                utime = getsys_utime
! double precision utime = user time in seconds
!
!                  o
!                stime = getsys_stime
! double precision stime = system time in seconds
!
!-------------------------------------------------------------------------------
! To get information about process resource utilization
!
!                                       o     o     o      o       o
!                call cgetsys_usage (maxrss,nswap,minflt,majflt,inblock,  &
!                                          o       o     o
!                                       outblock,utime,stime)
!
! integer maxrss   = the maximum resident set size
! integer nswap    = the number of swaps
! integer minflt   = the number of page faults not requiring physical I/O
! integer majflt   = the number of page faults requiring physical I/O
! integer inblock  = the number of block input operations
! integer outblock = the number of block output operations
! real    utime    = the user time used
! real    stime    = the system time used
!
!-------------------------------------------------------------------------------
! To get process ID of the current process
!
!                 o
!                pid = cgetsys_pid()
!
! integer pid  = processor ID
!
!-------------------------------------------------------------------------------
! To get CPS library used for link
!
!                  o
!                lnklib = getsys_library ()
!
! integer lnklib = library (one of the named constants listed here)
!                         GETSYS_UNKNOWN   =  Unknown
!                         GETSYS_PRODLIB   =  CPS production library
!                         GETSYS_BETALIB   =  CPS beta library
!
!-------------------------------------------------------------------------------
! To get the CPU speed
!
!                                         o
!                call getsys_cpu_speed (speed)
!
! integer speed  =  CPU speed in megahertz.
!                   returns 0 if CPU speed not available.
!
!-------------------------------------------------------------------------------
! To get the operating system release
!
!                                           o
!                call getsys_os_version (version)
!
! chararacter(len=*) version  = Operating System release
!
!-------------------------------------------------------------------------------
!To get the number of cpus and total memory
!
!                                      o       o       o          o
!                call getsys_memory (ncpus, pagesz, physpgs, avphyspgs)
!
! integer ncpus     = number of cpus
! integer pagesz    = memory page size
! integer physpgs   = total number of pages of physical memory
! integer avphyspgs = number of currently available pages of physical memory
!
!-------------------------------------------------------------------------------
!To get the maximium memory per cpu.
!
!                                                o           o
!                call getsys_memory_per_cpu (total_mem, available_mem)
!
! real total_mem     = total memory divided by the number of cpus
! real available_mem = available memory divided by the number of cpus
!
!-------------------------------------------------------------------------------
! To get the memory usage of the current process (LINUX only)
!
!                                             o     o
!                call getsys_my_pid_memory (vsize, rss)
!
! integer vsize = virtual memory size
! integer rss   = resident set size
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                    
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                     
!     Date        Author       Description
!     ----        ------       -----------
! 13. 2007-12-20  Goodger      Insure that array to hold os_version message is
!                              at least 8 words.
! 12. 2002-06-24  Vunderink    Added getsys_my_pid_memory
! 11. 2002-02-27  Vunderink    Added ALPHALIB to libraries
! 10. 2002-02-13  Vunderink    Added getsys_memory and getsys_memory_per_cpu
!  9. 2001-09-10  Vunderink    Changes required to rename TESTLIB to BETALIB
!  8. 2001-04-05  Vunderink    Added getsys_cpu_speed and getsys_os_version
!  7. 2001-04-03  Vunderink    Updated documentation tags
!  6. 2000-02-01  Vunderink    Added getsys_library
!  5. 2000-01-07  Brad Kruse   Corrected cgetsys interface in getsys_stime 
!                              and in getsys_utime
!  4. 1999-12-30  Brad Kruse   Added cgetsys_time, cgetsys_utime, cgetsys_stime 
!  3. 1999-11-30  Vunderink    Added getsys_pid
!  2. 1999-09-28  Vunderink    Added getsys_env getsys_usage, and
!                              getsys_seconds.  Modified getsys_machine to call
!                              cgetsys_machine.  Changed named constants to
!                              conform to standard.
!  1. 1999-07-01  Vunderink    Initial version.
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


      module getsys_module

      use string_module

      implicit none

!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      character(len=100),public,save :: getsys_ident = &
       '$Id: getsys.f90,v 1.13 2007/12/20 15:08:58 Goodger beta sps $'

      integer, parameter         :: GETSYS_UNKNOWN    =  0
      integer, parameter         :: GETSYS_CRAYPVP    =  1
      integer, parameter         :: GETSYS_VMS        =  2
      integer, parameter         :: GETSYS_ULTRIX     =  3
      integer, parameter         :: GETSYS_IBM        =  4
      integer, parameter         :: GETSYS_SOLARIS    =  5
      integer, parameter         :: GETSYS_HP         =  6
      integer, parameter         :: GETSYS_SGI        =  7
      integer, parameter         :: GETSYS_CRAYMPP    =  8
      integer, parameter         :: GETSYS_INTELSOL   =  9
      integer, parameter         :: GETSYS_LINUX      = 10

      integer, parameter         :: GETSYS_PRODLIB    =  1
      integer, parameter         :: GETSYS_BETALIB    =  2
      integer, parameter         :: GETSYS_TESTLIB    =  2
      integer, parameter         :: GETSYS_ALPHALIB   =  3

      contains


!!--------------------------- getsys_hostname -----------------------------!!
!!--------------------------- getsys_hostname -----------------------------!!
!!--------------------------- getsys_hostname -----------------------------!!


      subroutine getsys_hostname (host)
      implicit none
      character(len=*),intent(out)  :: host                          ! argument

      integer                       :: istat                         ! local
      integer,allocatable           :: htmp(:)                       ! local
      integer                       :: i                             ! local
      integer,external              :: cgetsys_hostname              ! local


      host = 'NONE'
      i = len(host)
      allocate(htmp(string_num_integers(host)),stat=istat)
      if (istat .eq. 0) then
         istat = cgetsys_hostname(htmp,i)
         if (istat .eq. 0) then
            call string_hh2cc(htmp,host)
         endif
      endif
      deallocate(htmp,stat=istat)

      return
      end subroutine getsys_hostname


!!--------------------------- getsys_username -----------------------------!!
!!--------------------------- getsys_username -----------------------------!!
!!--------------------------- getsys_username -----------------------------!!


      subroutine getsys_username (user)
      implicit none
      character(len=*),intent(out)  :: user                          ! argument
 
      integer                       :: istat                         ! local
      integer,allocatable           :: htmp(:)                       ! local
      integer                       :: i                             ! local
      integer,external              :: cgetsys_username              ! local
 
 
      user = 'NONE'
      i = len(user)
      allocate(htmp(string_num_integers(user)),stat=istat)
      if (istat .eq. 0) then
         istat = cgetsys_username(htmp,i)
         if (istat .eq. 0) then
            call string_hh2cc(htmp,user)
         endif
      endif
      deallocate(htmp,stat=istat)
 
      return
      end subroutine getsys_username


!!------------------------- getsys_current_dir ----------------------------!!
!!------------------------- getsys_current_dir ----------------------------!!
!!------------------------- getsys_current_dir ----------------------------!!


      subroutine getsys_current_dir (cdir)
      implicit none
      character(len=*),intent(out)  :: cdir                          ! argument
 
      integer                       :: istat                         ! local
      integer,allocatable           :: htmp(:)                       ! local
      integer                       :: i                             ! local
      integer,external              :: cgetsys_current_dir           ! local
 
 
      cdir = 'NONE'
      i = len(cdir)
      allocate(htmp(string_num_integers(cdir)),stat=istat)
      if (istat .eq. 0) then
         istat = cgetsys_current_dir(htmp,i)
         if (istat .eq. 0) then
            call string_hh2cc(htmp,cdir)
         endif
      endif
      deallocate(htmp,stat=istat)
      call string_strip_blanks(cdir,i)
      cdir  = cdir(1:i) // '/'
 
      return
      end subroutine getsys_current_dir


!!----------------------------- getsys_env --------------------------------!!
!!----------------------------- getsys_env --------------------------------!!
!!----------------------------- getsys_env --------------------------------!!


      subroutine getsys_env (name, value)
      implicit none
      character(len=*),intent(in)   :: name                          ! argument
      character(len=*),intent(out)  :: value                         ! argument
 
      integer                       :: istat                         ! local
      integer,allocatable           :: hname(:)                      ! local
      integer,allocatable           :: hvalue(:)                     ! local
      integer                       :: iname                         ! local
      integer                       :: ivalue                        ! local
      integer,external              :: cgetsys_env                   ! local
 
 
      value  = 'NONE'
      iname  = len(name)
      ivalue = len(value)
      allocate(hname(string_num_integers(name)),stat=istat)
      if (istat .ne. 0) return
      allocate(hvalue(string_num_integers(value)),stat=istat)
      if (istat .ne. 0) return
      call string_cc2hh(name,hname)
      istat = cgetsys_env(hname,hvalue,ivalue)
      if (istat .eq. 0) then
        call string_hh2cc(hvalue,value)
        deallocate(hname,stat=istat)
        deallocate(hvalue,stat=istat)
      endif
 
      return
      end subroutine getsys_env


!!---------------------------- getsys_ostype ------------------------------!!
!!---------------------------- getsys_ostype ------------------------------!!
!!---------------------------- getsys_ostype ------------------------------!!


      function getsys_ostype () result (ostype)
      implicit none
      integer                  :: ostype                        ! result

      integer,external         :: cgetsys_ostype                ! local
 
      ostype = cgetsys_ostype()
 
      return
      end function getsys_ostype


!!--------------------------- getsys_machine ------------------------------!!
!!--------------------------- getsys_machine ------------------------------!!
!!--------------------------- getsys_machine ------------------------------!!


      function getsys_machine (machine) result (ostype)
      implicit none
      character(len=*),intent(in)   :: machine                       ! argument
      integer                       :: ostype                        ! result

      integer                       :: istat                         ! local
      integer,allocatable           :: htmp(:)                       ! local
      integer                       :: i                             ! local
      integer,external              :: cgetsys_machine               ! local
 
 
      ostype = GETSYS_UNKNOWN
      i = len(machine)
      allocate(htmp(string_num_integers(machine)),stat=istat)
      call string_cc2hh (machine,htmp)
      if (istat .eq. 0) then
         ostype = cgetsys_machine(htmp)
      endif
      deallocate(htmp,stat=istat)

      return
      end function getsys_machine


!!--------------------------- getsys_netinfo ------------------------------!!
!!--------------------------- getsys_netinfo ------------------------------!!
!!--------------------------- getsys_netinfo ------------------------------!!


      subroutine getsys_netinfo (node, user, path)
      implicit none
      character(len=*),intent(out)  :: node                          ! argument
      character(len=*),intent(out)  :: user                          ! argument
      character(len=*),intent(out)  :: path                          ! argument
 
      integer                       :: istat                         ! local
      integer,allocatable           :: hnode(:)                      ! local
      integer,allocatable           :: huser(:)                      ! local
      integer,allocatable           :: hpath(:)                      ! local
      integer                       :: inode                         ! local
      integer                       :: iuser                         ! local
      integer                       :: ipath                         ! local
      integer,external              :: cgetsys_netinfo               ! local
 
 
      node  = 'NONE'
      user  = 'NONE'
      path  = 'NONE'
      inode = len(node)
      iuser = len(user)
      ipath = len(path)
      allocate(hnode(string_num_integers(node)),stat=istat)
      if (istat .ne. 0) return
      allocate(huser(string_num_integers(user)),stat=istat)
      if (istat .ne. 0) return
      allocate(hpath(string_num_integers(path)),stat=istat)
      if (istat .ne. 0) return
      istat = cgetsys_netinfo(hnode,huser,hpath)
      if (istat .ne. 3) return
      call string_hh2cc(hnode,node)
      call string_hh2cc(huser,user)
      call string_hh2cc(hpath,path)
      deallocate(hnode,stat=istat)
      deallocate(huser,stat=istat)
      deallocate(hpath,stat=istat)
 
      return
      end subroutine getsys_netinfo


!!---------------------------- getsys_seconds -----------------------------!!
!!---------------------------- getsys_seconds -----------------------------!!
!!---------------------------- getsys_seconds -----------------------------!!


      function getsys_seconds () result (t)
      implicit none
      real                          :: t                             ! result

      real,external                 :: cgetsys_seconds               ! local
 
      t = 0.0
      t = cgetsys_seconds()

      return
      end function getsys_seconds


!!---------------------------- getsys_usage -------------------------------!!
!!---------------------------- getsys_usage -------------------------------!!
!!---------------------------- getsys_usage -------------------------------!!


      subroutine getsys_usage (maxrss,nswap,minflt,majflt,inblock,outblock,  &
                               utime, stime)
      implicit none
      integer,intent(out)           :: maxrss                        ! argument
      integer,intent(out)           :: nswap                         ! argument
      integer,intent(out)           :: minflt                        ! argument
      integer,intent(out)           :: majflt                        ! argument
      integer,intent(out)           :: inblock                       ! argument
      integer,intent(out)           :: outblock                      ! argument
      real   ,intent(out)           :: utime                         ! argument
      real   ,intent(out)           :: stime                         ! argument
 
      integer                       :: istat                         ! local
      integer,external              :: cgetsys_usage                 ! local

      istat = cgetsys_usage(maxrss,nswap,minflt,majflt,inblock,outblock,  &
                            utime, stime)

      return
      end subroutine getsys_usage


!!------------------------------ getsys_pid -------------------------------!!
!!------------------------------ getsys_pid -------------------------------!!
!!------------------------------ getsys_pid -------------------------------!!


      function getsys_pid () result (pid)
      implicit none
      integer                          :: pid                        ! result

      integer,external                 :: cgetsys_pid                ! local
 
      pid = 0
      pid = cgetsys_pid()

      return
      end function getsys_pid


!!---------------------------- getsys_time -------------------------------!!
!!---------------------------- getsys_time -------------------------------!!
!!---------------------------- getsys_time -------------------------------!!


      subroutine getsys_time (utime, stime)

      double precision, intent(out) :: utime                         ! argument
      double precision, intent(out) :: stime                         ! argument

      integer                       :: istat                         ! local
      integer,external              :: cgetsys_time                  ! local

      istat = cgetsys_time(utime, stime)

      end subroutine getsys_time


!!---------------------------- getsys_utime ------------------------------!!
!!---------------------------- getsys_utime ------------------------------!!
!!---------------------------- getsys_utime ------------------------------!!


      function getsys_utime () result (utime)

      double precision           :: utime                            ! argument

      double precision, external :: cgetsys_utime                    ! local

      utime = cgetsys_utime()

      end function getsys_utime


!!---------------------------- getsys_stime ------------------------------!!
!!---------------------------- getsys_stime ------------------------------!!
!!---------------------------- getsys_stime ------------------------------!!


      function getsys_stime () result (stime)

      double precision           :: stime                            ! argument

      double precision, external :: cgetsys_stime                    ! local

      stime = cgetsys_stime()

      end function getsys_stime


!!--------------------------- getsys_library ------------------------------!!
!!--------------------------- getsys_library ------------------------------!!
!!--------------------------- getsys_library ------------------------------!!


      function getsys_library () result (lnklib)
      implicit none
      integer                  :: lnklib                        ! result

      integer,external         :: cgetsys_library               ! local
 
      lnklib = cgetsys_library()
 
      return
      end function getsys_library


!!-------------------------- getsys_cpu_speed -----------------------------!!
!!-------------------------- getsys_cpu_speed -----------------------------!!
!!-------------------------- getsys_cpu_speed -----------------------------!!


      subroutine getsys_cpu_speed (speed)
      implicit none
      integer,intent(out)      :: speed                         ! argument

      integer,external         :: cgetsys_cpu_speed             ! local
 
      speed = cgetsys_cpu_speed()
 
      return
      end subroutine getsys_cpu_speed


!!------------------------- getsys_os_version -----------------------------!!
!!------------------------- getsys_os_version -----------------------------!!
!!------------------------- getsys_os_version -----------------------------!!


      subroutine getsys_os_version (os_version)
      implicit none
      character(len=*),intent(out)  :: os_version               ! argument

      integer                       :: istat                    ! local
      integer,allocatable           :: htmp(:)                  ! local
      integer                       :: i,nwrds
      integer,external              :: cgetsys_os_version       ! local
 
      os_version = 'NONE'
      i = len(os_version)
      nwrds=string_num_integers(os_version)
      if(nwrds.lt.8)nwrds=8
      allocate(htmp(nwrds),stat=istat)
      if (istat .eq. 0) then
         istat = cgetsys_os_version(htmp)
         call string_hh2cc(htmp,os_version)
      endif
      deallocate(htmp,stat=istat)

      return
      end subroutine getsys_os_version


!!--------------------------- getsys_memory -------------------------------!!
!!--------------------------- getsys_memory -------------------------------!!
!!--------------------------- getsys_memory -------------------------------!!


      subroutine getsys_memory (ncpu, pagesz, physpgs, avphyspgs)
      implicit none
      integer,intent(out)           :: ncpu                     ! argument
      integer,intent(out)           :: pagesz                   ! argument
      integer,intent(out)           :: physpgs                  ! argument
      integer,intent(out)           :: avphyspgs                ! argument

      call cgetsys_memory(ncpu, pagesz, physpgs, avphyspgs)

      return
      end subroutine getsys_memory


!!----------------------- getsys_memory_per_cpu ---------------------------!!
!!----------------------- getsys_memory_per_cpu ---------------------------!!
!!----------------------- getsys_memory_per_cpu ---------------------------!!


      subroutine getsys_memory_per_cpu (total_mem, available_mem)
      implicit none
      real,intent(out)              :: total_mem                ! argument
      real,intent(out)              :: available_mem            ! argument

      integer                       :: ncpu                     ! local
      integer                       :: pagesz                   ! local
      integer                       :: physpgs                  ! local
      integer                       :: avphyspgs                ! local

      call cgetsys_memory(ncpu, pagesz, physpgs, avphyspgs)

      if (ncpu .gt. 0) then
        total_mem     = float(pagesz) * float(physpgs)   / float(ncpu)
        available_mem = float(pagesz) * float(avphyspgs) / float(ncpu)
      else
        total_mem     = 0.0
        available_mem = 0.0
      endif

      return
      end subroutine getsys_memory_per_cpu


!!------------------------ getsys_my_pid_memory ---------------------------!!
!!------------------------ getsys_my_pid_memory ---------------------------!!
!!------------------------ getsys_my_pid_memory ---------------------------!!


      subroutine getsys_my_pid_memory (vsize, rss)
      implicit none
      integer,intent(out)           :: vsize                    ! argument
      integer,intent(out)           :: rss                      ! argument

      call cgetsys_my_pid_memory (vsize, rss)

      return
      end subroutine getsys_my_pid_memory


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module getsys_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

