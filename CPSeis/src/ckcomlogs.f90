!<CPS_v1 type="PROGRAM"/>
!!------------------------------ ckcomlogs.f90 --------------------------------!!
!!------------------------------ ckcomlogs.f90 --------------------------------!!
!!------------------------------ ckcomlogs.f90 --------------------------------!!


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
!                       C P S   P R O G R A M 
!
! Name       : CKCOMLOGS
! Category   : stand-alone
! Written    : 2006-10-13   by: Karen Goodger
! Revised    : 2007-06-05   by: Karen Goodger
! Maturity   : production
! Purpose    : Reads the x.log files from compile output and lists problems.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!  ckcomlogs reads file xlogFiles which is the result of a find of all the   
!  x.log files.  It then reads the individual x.log file for each platform
!  into memory.  If keywords warning, ansi, or error are found, it backs
!  up in the array to find the compile card.  If the path shows the routine
!  to be in spsmodules, the information is printed, otherwise, the information
!  is ignored.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
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
!  5. 2007-06-05  Goodger    Ignore assembler warning messages on
!                            linuxab90_debug platform.
!  4. 2007-05-30  Goodger    touch a 'platform'FAILED file on platforms
!                            that are missing the making.so card.
!  3. 2007-05-09  Goodger    Draw a box of @ signs around the 'making so'
!                            library problem so the message will be more
!                            visible.
!  2. 2006-10-17  Goodger    Output a file with the name of the offending
!                            source code.  The file will be read by the
!                            cycFromReady script.
!  1. 2006-10-13  Goodger    Initial version.
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
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
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



!!--------------------------- private module -------------------------------!!
!!--------------------------- private module -------------------------------!!
!!--------------------------- private module -------------------------------!!




!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!


      module ckcomlogs_module

      use putsys_module
      use string_module

      implicit none

       integer            :: istat,lun_log=1,lun_badfile=3,nlines,ndxso,ndxmaking,ndo
       integer, parameter :: maxlines=50000
       integer, parameter :: maxchar=1000
       character(len=maxchar) :: xlog(maxlines)
       character(len=32) :: platform,fc,cc
       logical :: debug=.false.,passed=.true.

      contains

       subroutine ckcomlogs_main


       integer :: i,i1,i2,k
       integer :: lun_loglist=2

       character(len=80) :: card,ctmp


       open(lun_loglist,file='logList',status='old',iostat=istat)
       if(istat.ne.0)then
         print*,' Error opening logList = ',istat
         stop
       endif

       open(lun_badfile,file='badFile',status='replace')

       DO
         read(lun_loglist,9001,iostat=istat)card
         if(istat.lt.0)exit
         i1=index(card,'/') +1
         i2=index(card,'/',.true.) -1
         i=i1+i2
         if(i.eq.0)then
           print*,' Error problem with platform card'
           print*,card
           stop
         endif
         platform=card(i1:i2)
         print*,'****************************************'
         print*,' platform = ',platform
         print*,'***************************************'
         select case (platform)
         case('linuxab90','linuxab90_debug')
            fc='ab90_f90'
            cc='gcc'
            call ckcomlogs_generic
            call ckcomlogs_sharedlib
         case ('linuxab80','linuxab80_debug','linuxab80_xeon','linuxab80_prof')
            fc='ab80_f90'
            cc='gcc'
            call ckcomlogs_generic
            call ckcomlogs_sharedlib
         case ('linuxab75','linuxab75_debug')
            fc='ab75_f90'
            cc='gcc'
            call ckcomlogs_generic
            call ckcomlogs_sharedlib
         case ('linuxp','linuxp_debug')
            fc='pgf90'
            cc='gcc3'
            call ckcomlogs_generic
         case ('64linuxp60','64linuxp60_debug')
            fc='pgf60_f90'
            cc='gcc'
            call ckcomlogs_generic
         case ('64linuxp61','64linuxp61_debug')
            fc='pgf61_f90'
            cc='gcc'
            call ckcomlogs_generic
         case ('32elinuxp50')
            fc='pgf3250_f90'
            cc='gcc'
            call ckcomlogs_generic
         case ('linuxi81','linuxi81_debug','linuxi90','linuxi90_debug')
            fc='ifort'
            cc='gcc3'
            call ckcomlogs_generic
         case ('64sol62','64sol62_debug','sol62')
            fc='sol62_f90'
            cc='sol62_cc'
            call ckcomlogs_generic
         case ('sol62_debug')
            fc='sol62_f90'
            cc='sol62_cc'
            call ckcomlogs_generic
            call ckcomlogs_sharedlib
         case ('sol70','sol70_debug')
            fc='sol70_f90'
            cc='sol70_cc'
            call ckcomlogs_generic
         case ('64sgi73','64sgi73_debug')
            fc='f90'
            cc='cc  -c'
            call ckcomlogs_generic
         case default
            print*,' Need a routine for platform ',platform
         end select
       ENDDO
       print*,' '
       if(passed)then
        print*,' ckeckit passed'
        call putsys_cmd('touch CHECKIT_PASSED')
       else
        print*,' checkcomlogs FAILED'
       endif

 9001 format(A)

       end subroutine ckcomlogs_main
       subroutine ckcomlogs_examineError(ndx,flagthis)

         integer :: flagthis,k,ndx

         flagthis=0
         k=index(xlog(ndx),'f90fe: error')
         if(k.ne.0)flagthis=1

       end subroutine ckcomlogs_examineError

       subroutine ckcomlogs_examineWarning(ndx,flagthis)

         integer :: flagthis,k,k1,k2,l1,l2,l3,ndx

!           Ignore this warning
         flagthis=0
         k1=index(xlog(ndx),'warning: -xvector option has no effect')
         l1=index(xlog(ndx),'.s')
         l2=index(xlog(ndx),'warning: value')
         l3=index(xlog(ndx),' truncated to ')
         if(l1.ne.0.and.l2.ne.0.and.l3.ne.0)k2=1
         k=k1+k2
         if(k.ne.0)then
           flagthis=1
           return
         endif

       end subroutine ckcomlogs_examineWarning

       subroutine ckcomlogs_generic

         integer :: i,i1,j,k,k1,k2,k3,k4,k5,l,l1,l2,l3,ignore,nc,ndxdot,ndxprnt,prob
         character(len=4) :: ext
         character(len=32) :: badsource
         character(len=80) :: cmd
         logical :: foundbadsource=.false.
         call ckcomlogs_readintomem
      
         ndxprnt=0
         DO i=1,ndo
           ignore=0
           prob=0
           if(i.lt.ndxprnt)cycle
           k1=index(xlog(i),'warning')
           if(k1.ne.0)call ckcomlogs_examineWarning(i,ignore)
           k2=index(xlog(i),'error')
           if(k2.ne.0)call ckcomlogs_examineError(i,prob)
           k3=index(xlog(i),'ansi')
           if(k3.ne.0)then  !  ignore if its the compile card 
             k4=index(xlog(i),'-ansi')
             if(k4.ne.0)k3=0
           endif
           k=k1+k2+k3
           if(k.eq.0)cycle
           if(ignore.eq.1)cycle

!              back up to find compile card and print everything until next compile card or EOF
           j=i-1
           do
             l1=index(xlog(j),trim(fc))
             l2=index(xlog(j),trim(cc))
             l=l1+l2
             if(l.ne.0)exit
               j=j-1
               if(j.lt.0)then
                 print*,' Yikes backed up to beginning of x.log - something wrong'
                 stop
             endif
           enddo
           ndxprnt=j
!             Is spsmodules or updates part of the path to the source code
           nc=len_trim(xlog(ndxprnt))
           if(nc.ge.maxchar)then
             print*,' The following card has exceeded ',maxchar,' characters'
             print*,xlog(ndxprnt)
             print*,' Increase limits in program ckcomlogs'
             stop
           endif
           i1=index(xlog(ndxprnt),'/home/',.true.)
           l1=index(xlog(ndxprnt)(i1:),'spsmodules')  
           l2=index(xlog(ndxprnt)(i1:),'updates')
           l=l1+l2+prob
           if(l.eq.0)cycle
!            This file is one being checked in or the make failed  - print the information
!diag
! print*,' Diag A - l = ',l,' prob = ',prob
           passed=.false.
!            Output a file with the offending source name
           ndxdot=0
           k1=index(xlog(ndxprnt),'.c')
           k2=index(xlog(ndxprnt),'.f90')
           if(k1.ne.0)then
             ext='.c'
             ndxdot=k1
           else if(k2.ne.0)then
             ext='.f90'
             ndxdot=k2
           endif
           if(ndxdot.ne.0)then
             k=index(xlog(ndxprnt),'/',.true.)
             badsource=xlog(ndxprnt)(k+1:ndxdot-1) // trim(ext)
           else
             badsource='FAILED2PARSE'
           endif
!                  Just need one badsource file
           if(.not.foundbadsource)write(lun_badfile,9001)trim(badsource)
           foundbadsource=.true.
           do 
             print*,trim(xlog(ndxprnt))
             ndxprnt=ndxprnt+1
             if(ndxprnt.gt.ndo)return
             if(ndxprnt.gt.maxlines)return
             l1=index(xlog(ndxprnt),trim(fc))
             l2=index(xlog(ndxprnt),trim(cc))
             l=l1+l2
             if(l.ne.0)exit
           enddo
         ENDDO

 9001  format(A)

       end subroutine ckcomlogs_generic
       subroutine ckcomlogs_readintomem

         character(len=80) :: cmd,thispath
         integer :: i,k
         thispath=trim(platform) // '/x.log'
         open(lun_log,file=trim(thispath),status='old',iostat=istat)
         if(istat.ne.0)then
           print*,' Unable to open ',trim(thispath)
           return     
         endif
         nlines=0
         ndxso=0
         ndxmaking=0
         do i=1,maxlines
           read(lun_log,9001,iostat=istat)xlog(i)
           if(istat.lt.0)exit
           call string_to_lower(xlog(i))
           k=index(xlog(i),'making junklib.so')
           if(k.ne.0)then
             ndxmaking=i
           endif
           k=index(xlog(i),'ldd -r junklib.so')
           if(k.ne.0)then
            ndxso=i
           endif
           if(i.eq.maxlines)then
             print*,' The x.log file for platform ',trim(platform),' has reached ',maxlines,' lines'
           endif
         enddo
         nlines=i-1
         ndo=ndxmaking
         if(ndxmaking.eq.0)then
           passed=.false.
           print*,' '
           print*,'@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'
           print*,'@    Did not find making junklib.so   @'
           print*,'@       Platform = ',platform             
           print*,'@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'
           print*,' '
           cmd='touch ' // trim(platform) // 'FAILED'
           call putsys_cmd(trim(cmd))
           ndo=nlines
         endif
         close(lun_log,status='keep')

 9001  format(A)
         
       end subroutine ckcomlogs_readintomem
       subroutine ckcomlogs_sharedlib
     
        integer :: i,k
        i=ndxso+1
        if(ndxso.eq.0)i=ndxmaking+1
        if(ndxmaking.eq.0)return
        do 
          k=index(xlog(i),'finished')
          if(k.ne.0)exit
          k=index(xlog(i),'undefined')
          if(k.ne.0)then
            print*,trim(xlog(i))
            passed=.false.
!diag
! print*,' diag B'
          endif
          i=i+1
          if(i.gt.nlines)exit
        enddo

       end subroutine ckcomlogs_sharedlib

      end module ckcomlogs_module

      program ckcomlogs
      use ckcomlogs_module

      character(len=100),save :: CKCOMLOGS_IDENT = &
'$Id: ckcomlogs.f90,v 1.5 2007/06/05 19:55:30 Goodger prod sps $'

      call ckcomlogs_main
      end program ckcomlogs


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!




