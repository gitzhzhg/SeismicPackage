!<CPS_v1 type="AUXILIARY_FILE"/>
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
!                        C P S  P R O G R A M  F I L E
!
! Name       : cfeseries_frou
! Category   : stand-alone
! Written    : 2000-01-24   by: Donna K. Vunderink
! Revised    : 2004-04-21   by: Goodger
! Maturity   : beta
! Purpose    : Submits jobs based on a job series file
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 15. 2004-04-21  Goodger      Get full path for programs from config file.
! 14. 2004-02-06  SMCook       Rearranged "if" logic again, this time to prevent
!                                "next series" jobs from starting too soon.
! 13. 2004-02-04  SMCook       Commented out jobname "if" statement that was
!                                preventing "next series" jobs from starting.
! 12. 2003-12-23  Goodger      Changed name from series_sub to cfeseries_frou to
!                              conform with standards.  Determine if alpha,beta,
!                              or production and use the appropriate version of
!                              cfesub.  Create alpha and beta version of 
!                              cfeseries.
! 11. 2001-04-11  Vunderink    Added independent series.
! 10. 2001-03-15  Vunderink    Modified to sleep 60 seconds between submits.
!  9. 2001-01-23  Vunderink    Added support of the PBS queuing system
!  8. 2000-05-25  Vunderink    Compare jobnames not jobs when looking to start
!                                another series.
!  7. 2000-04-17  Vunderink    Check for file extention on jobs.
!                              Also, increased character variable lengths.
!  6. 2000-03-27  Vunderink    Uncommented lockfile logic, changed to use
!                                putsys_texec, added some error checking, and
!                                made some cosmetic changes.
!  5. 2000-02-06  Vunderink    Commented out lockfile logic for the time being
!                                and added support for next series.
!  4. 2000-01-24  Vunderink    Initial version. Written using CPS primitive
!                                series.f90 as model/skeleton.
!
! Revision history for series.f90:
!
!  3. 1999-06-30  Goodger      Create a file called series.lock to allow
!                                only one job to access the series file at a
!                                time.
!  2. 1999-06-28  Goodger      Changed return status to zero if unable to
!                                open series file as the file could have
!                                legitimately been deleted.  Print a message
!                                only.
!                                Check for CHARACTER*8 CENDSCR,CENDSTO rather
!                                than just character.  There will be several
!                                character statements if custom code is
!                                included.
!  1. 1999-06-22  Goodger      Original Version
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


!!--------------------------- start of code -------------------------------!!
!!--------------------------- start of code -------------------------------!!
!!--------------------------- start of code -------------------------------!!


      subroutine cfeseries_f (h_jobname,h_seriesfile,h_batchsystem,rtnstat)

      use cio_module
      use cardset_module
      use cnfg_module
      use finquire_module
      use getlun_module
      use getsys_module
      use path_module
      use putsys_module
      use string_module

      implicit none

      character(len=100),save :: cfeseries_frou_ident = &
       '$Id: cfeseries_frou.f90,v 1.15 2004/04/21 15:23:13 Goodger beta sps $'

      integer,intent(in)                    :: h_jobname(*)     ! arguments
      integer,intent(in)                    :: h_seriesfile(*)  ! arguments
      integer,intent(in)                    :: h_batchsystem(*) ! arguments
      integer,intent(out)                   :: rtnstat          ! arguments

      character(len=1024)                   :: cmd              ! local
      character(len=256)                    :: working_dir      ! local
      character(len=256)                    :: lockfile         ! local
      character(len=80)                     :: jobname          ! local
      character(len=256)                    :: seriesfile       ! local
      character(len=80)                     :: batchsystem      ! local
      character(len=80)                     :: independent      ! local
      character(len=256)                    :: nextseriesfile   ! local
      character(len=80)                     :: errmsg           ! local
      character(len=80)                     :: userid           ! local
      character(len=80)                     :: node             ! local
      character(len=256)                    :: path             ! local
      character(len=256)                    :: file             ! local
      character(len=256),pointer            :: jobs(:)          ! local
      character(len=256),pointer            :: jobnames(:)      ! local
      character(len=256),pointer            :: next(:)          ! local
      character(len=256)                    :: tempname         ! local
      character(len=256)                    :: file_path        ! local
      character(len=256)                    :: seriesname       ! local
      character(len=80)                     :: card             ! local
      character(len=80),pointer             :: cards(:)         ! local
      character(len=4)                      :: ctemp            ! local
      character(len=2)                      :: mode             ! local
      character(len=40)                     :: date_time        ! local
      character(len=80)                     :: subprog,serprog

      logical                               :: allneg           !local
      logical                               :: alreadyopen      !local

      integer                               :: stdout = 6       !local
      integer                               :: k                !local
      integer                               :: i                !local
      integer                               :: j                !local
      integer                               :: l                !local
      integer                               :: m                !local
      integer                               :: n                !local
      integer                               :: istat            !local
      integer                               :: total            !local
      integer                               :: submit_total     !local
      integer,pointer                       :: count(:)         !local
      integer                               :: njobs            !local
      integer                               :: ncount           !local
      integer                               :: nnext            !local
      integer                               :: ncard            !local
      integer                               :: ncards           !local
      integer                               :: ntempname        !local
      integer                               :: lun_series       !local
      integer                               :: lun_lock         !local
      integer                               :: knt              !local
      integer                               :: temp

      type(cardset_struct),pointer          :: cardset          !local

      call string_hh2cc (h_jobname    ,jobname)
      call string_hh2cc (h_seriesfile ,seriesfile)
      call string_hh2cc (h_batchsystem,batchsystem)

      call string_to_upper (batchsystem)
      if (batchsystem(1:1) .eq. 'P') then
        batchsystem = 'PBS'
      else
        batchsystem = 'NQS'
      endif

      independent = 'NO'
      rtnstat = 0

      nullify(cardset)
      nullify(jobnames)
      nullify(jobs)
      nullify(count)
      nullify(next)

      

      k=getsys_ostype()
      select case(k)
        case (GETSYS_LINUX)
          call cnfg_get_value('bin_path_linux1',subprog)
        case (GETSYS_SOLARIS)
          call cnfg_get_value('bin_path_solaris1',subprog)
      end select
      serprog=subprog

      k=getsys_library()
      select case(k)
        case(GETSYS_BETALIB)
          subprog=trim(subprog) // '/cfesubbeta'
          serprog=trim(serprog) // '/cfeseriesbeta'
        case(GETSYS_ALPHALIB)
          subprog=trim(subprog) // '/cfesubalpha'
          serprog=trim(serprog) // '/cfeseriesalpha'
        case default
          subprog=trim(subprog) // '/cfesub'
          serprog=trim(serprog) // '/cfeseries'
      end select

!------------------------------------------------------------------------------
!          Get the series name
!------------------------------------------------------------------------------
      call path_parse (seriesfile,userid,node,path,file)
      k = index(file,".")
      seriesname = ' '
      seriesname = file(1:k-1)
      write(stdout,*) '+------------------------------------------------------+'
      write(stdout,*) '|                  Program cfeseries                   |'
      write(stdout,*) '+------------------------------------------------------+'
      write(stdout,*) ' '
      write(stdout,*) '   This job is part of job series ',trim(seriesname)
      write(stdout,*) ' '

!------------------------------------------------------------------------------
!         File may be accessed by another job.
!------------------------------------------------------------------------------
      if (trim(batchsystem) .eq. 'NQS') then

        call getlun (lun_lock)
        if (lun_lock .eq. -1) then
          write(stdout,*) '   GETLUN error'
          rtnstat = -1
          goto 9999
        endif
        lockfile = trim(path) // trim(seriesname) // '.lock'
        open (lun_lock,file=trim(lockfile),status='NEW',iostat=istat)
        if (istat .ne. 0) alreadyopen = .true.
        knt = 0
        do
          if (alreadyopen) then
            knt = knt + 1
            if (knt .gt. 20) then
              write(stdout,*) '   Series file still open after 10 minutes'
              write(stdout,*) '   Something wrong'
              rtnstat = 1
              goto 9999
            endif
            write(stdout,*) '   Series file was already open'
            write(stdout,*) '   Will try again in 30 seconds'
            call cfeseries_sleep(30)
            alreadyopen = .false.
            open (lun_lock,file=trim(lockfile),status='NEW',iostat=istat)
            if (istat .ne. 0) alreadyopen = .true.
          else
            exit
          endif
        enddo

      else                       !PBS

        lockfile = trim(path) // trim(seriesname) // '.lock'
        istat = cio_lock_file(trim(lockfile),1800)

      endif

!------------------------------------------------------------------------------
!           Open and read series file
!------------------------------------------------------------------------------
      if (trim(batchsystem) .eq. 'NQS') then

        call getlun (lun_series)
        if (lun_series .eq. -1) then
          write(stdout,*) '  GETLUN error'
          rtnstat = -1
          goto 9999
        endif
        open (lun_series,file=trim(seriesfile),iostat=istat,status='OLD')
        if (istat .ne. 0) then
          write(stdout,*) '   Unable to open series file' // trim(seriesfile)
          rtnstat = 0
          goto 9999
        endif
        call cardset_create (cardset)
        allneg = .true.
        do
          read(lun_series,'(A80)',end=8000) card
          call cardset_add_card (cardset,card)
        enddo

      else                       !PBS

        mode = "r+"
        lun_series = cio_fopen(trim(seriesfile),mode)
        if (lun_series .lt. 100) then
          write(stdout,*) '   Unable to open series file '//trim(seriesfile)
          rtnstat = 0
          goto 9999
        endif
        call cardset_create (cardset)
        allneg = .true.
        do
          ncard = cio_fgetline(card,CARDSET_DATACARD_LENGTH,lun_series)
          if (ncard .lt. 0) exit
          call cardset_add_card (cardset,card)
        enddo

      endif
 8000 continue

      call cardset_get_scalar  (cardset,'WORKING_DIR',working_dir,errmsg)
      call cardset_get_scalar  (cardset,'INDEPENDENT',independent,errmsg)
      call cardset_alloc_array (cardset,'JOBS',jobs,njobs,errmsg)
      call cardset_alloc_array (cardset,'COUNT',count,ncount,errmsg)
      call cardset_alloc_array (cardset,'NEXT',next,nnext,errmsg)
      total = min(njobs,ncount,nnext)

      if (total .gt. 0) allocate(jobnames(total))
      do i=1,total
        tempname = path_get_file(jobs(i))
        j = index(tempname,'.wrk',.true.)
        if (j .eq. 0) j = index(tempname,'.job',.true.)
        if (j .eq. 0) j = len_trim(tempname) + 1
        jobnames(i) = tempname(1:j-1)
      enddo

      submit_total = 0
      do i=1,total
        count(i) = count(i) - 1
        if (count(i) .ge. 0) allneg = .false.
        if (count(i) .eq. 0) then
          file_path = path_get_dir(jobs(i))
          if (file_path .eq. PATH_EMPTY) then
            tempname = trim(working_dir) // trim(jobs(i))
          else
            tempname = trim(jobs(i))
          endif
          ntempname = len_trim(tempname)
          if (tempname(ntempname-3:ntempname) .eq. '.wrk') then
            tempname(ntempname-3:ntempname) = '.job'
          else if (tempname(ntempname-3:ntempname) .ne. '.job') then
            tempname(ntempname+1:) = '.job'
          endif
          write(stdout,*) ' '
          write(stdout,*) '   Job ',trim(tempname),' is ready for submission'
          write(stdout,*) ' '
          istat = finquire_input (trim(tempname))
          if (istat .eq. FINQUIRE_ERROR) then
            write(stdout,*) '   Unable to open job file '//trim(tempname)
            rtnstat=1
            goto 9999
          endif

!------------------------------------------------------------------------------
!           Submit the temporary job
!------------------------------------------------------------------------------
          if (trim(batchsystem) .eq. 'NQS') then
            if (trim(independent) .eq. 'YES') then
              cmd = trim(subprog) //'  -n -i -s '//trim(seriesfile)//' '//&
                    trim(tempname)
            else
              cmd = trim(subprog) //' -n -s '//trim(seriesfile)//' '//&
                    trim(tempname)
            endif
          else                       !PBS
            if (trim(independent) .eq. 'YES') then
              cmd = trim(subprog) // ' -p -i -s '//trim(seriesfile)//' '//&
                    trim(tempname)
            else
              cmd = trim(subprog) // ' -p -s '//trim(seriesfile)//' '//&
                    trim(tempname)
            endif
          endif
          if (submit_total .gt. 0) call cfeseries_sleep(60)
          print *,trim(cmd)
          call putsys_texec(trim(cmd),istat)
          if(istat.ne.0)then
            write(stdout,*) '   Status error when executing ',trim(subprog)
            rtnstat=1
            goto 9999
          endif
          submit_total = submit_total + 1

          if (len_trim(next(i)) .gt. 0) then   ! note that count(i) is zero
            write(stdout,*) ' '
            write(stdout,*) '   Series ',trim(next(i)),' is ready to start'
            write(stdout,*) ' '
            j = index(seriesfile,'/',.true.)
            if (j .gt. 0) then
              nextseriesfile = seriesfile(1:j) // trim(next(i)) // '.series'
              if (trim(batchsystem) .eq. 'NQS') then
                cmd = trim(serprog) // ' -n -s ' // trim(nextseriesfile)
              else                       !PBS
                cmd = trim(serprog) // ' -p -s ' // trim(nextseriesfile)
              endif
              print *,trim(cmd)
              call putsys_texec(trim(cmd),istat)
              if(istat.ne.0)then
                write(stdout,*) '   Status error when executing cfeseries'
                rtnstat=1
                goto 9999
              endif
            endif
          endif

        endif
      enddo

!------------------------------------------------------------------------------
!         Rewrite the job series file
!------------------------------------------------------------------------------
      if (trim(batchsystem) .eq. 'NQS') then

        rewind lun_series
        if(.not.allneg)then
          call cardset_put_array   (cardset,'JOBS' ,jobs ,total)
          call cardset_put_array   (cardset,'COUNT',count,total)
          call cardset_alloc_cards (cardset,cards,ncards)
          do i=1,ncards
            write(lun_series,'(A80)') cards(i)
          enddo
          endfile lun_series
          close(unit=lun_series)
        else
          close(unit=lun_series,status='DELETE')
        endif

      else                       !PBS

        call cio_frewind (lun_series)
        if(.not.allneg)then
          call cardset_put_array   (cardset,'JOBS' ,jobs ,total)
          call cardset_put_array   (cardset,'COUNT',count,total)
          call cardset_alloc_cards (cardset,cards,ncards)
          do i=1,ncards
            temp  = len_trim(cards(i))
            ncard = cio_fputline(cards(i),temp,lun_series)
          enddo
          istat = cio_fclose(lun_series)
        else
          istat = cio_fclose(lun_series)
          istat = cio_remove(trim(seriesfile))
        endif

      endif

 9999 continue
      write(stdout,*) '+------------------------------------------------------+'
      write(stdout,*) '|                    End cfeseries                     |'
      write(stdout,*) '+------------------------------------------------------+'

      if (trim(batchsystem) .eq. 'NQS') then

        inquire (unit=lun_lock,opened=alreadyopen)
        if (alreadyopen) close (unit=lun_lock,status='DELETE')

      else                       !PBS

        istat = cio_unlock_file(trim(lockfile))

      endif

      return

      end subroutine cfeseries_f
