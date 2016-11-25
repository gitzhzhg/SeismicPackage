!<CPS_v1 type="PROGRAM"/>
!!------------------------------ checkc.f90 --------------------------------!!
!!------------------------------ checkc.f90 --------------------------------!!
!!------------------------------ checkc.f90 --------------------------------!!


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
! Name       : CHECKC
! Category   : stand-alone
! Written    : 2001-03-07   by: Karen Goodger
! Revised    : 2006-12-20   by: Karen Goodger
! Maturity   : production
! Purpose    : Checks code for standards.
! Portability: linux.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!  This program is executed by the script checkc.  It uses files prepared
!  by the script called checkc_routine and checkc_names, which are deleted
!  after checkc completes.  If it is C source code, it is first compiled, so 
!  that the nm command can be used to create the checkc_names file.  The 
!  checkc_names file is used to test function names for the proper naming
!  conventions.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!  Type in checkc followed by the the source code name.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author         Description
!     ----        ------         -----------
! 52. 2006-12-20  Goodger        Touch the CHECKC_FAILED file for all user
!                                names.  The script can look for this file
!                                and return an error code if it exists.
! 51. 2006-12-15  Goodger        Allow only beta checkins.  We are no longer
!                                maintaining alphalib.
! 50. 2006-11-28  Goodger        Change the checkin deadline time from 3pm 
!                                to 9pm.
! 49. 2006-10-11  Goodger        Make int_api.f90 an exception to the 
!                                underscore rule.
! 48. 2006-09-15  Goodger        Comment out passed=.false. flag on condition 
!                                where RCS files not found.  This was set only
!                                for sps and usually only happens on a first
!                                checkin, which should pass.  Needed for 
!                                automated checkins.
! 47. 2006-09-11  Goodger        Touch a file called CHECHC_FAILED if checkc
!                                fails from account sps.  This will be used
!                                by the automation procedure.
! 46. 2006-07-13  Goodger        Remove 80 character line restriction.
! 45. 2004-08-31  Goodger        Insure checkin day has a leading zero when 1
!                                digit.
! 44. 2004-08-19  Goodger        Use the checkin date (Tue or Thur) rather
!                                than the current date, to determine date
!                                validity.
! 43. 2004-07-21  Goodger        Indicate error if maturity is not the same
!                                as the spsmodules directory, for account sps.
! 42. 2004-03-15  Goodger        Use the filetype from xlrev when determining
!                                valid categories.
! 41. 2004-02-19  Goodger        Check the Ident flag.
!                                Get maturity from xlrev_maturity file.
! 40. 2004-01-19  Goodger        Check for xlrev_abort file and fail if it
!                                exists.
! 39. 2004-01-06  Goodger        Add check for valid categories. Add pass/fail
!                                flag.
! 38. 2003-08-26  Karen Goodger  Allow the word entry in print statements.
! 37. 2003-07-28  Karen Goodger  Use source code for revision checking rather
!                                than .history files.
! 36. 2003-07-18  Karen Goodger  Fix bug with c files.  It was not allowing
!                                the name without the underscore.
! 35. 2002-10-25  Karen Goodger  Remove print to standand out restriction.
!                                Remove pc_get_lun requirement.
! 34. 2002-10-23  Karen Goodger  Allow an underscore in the file name if 
!                                _crou or _frou.
! 33. 2002-10-22  Karen Goodger  Check file name for an underscore.
! 32. 2002-08-20  Karen Goodger  Change wording of error message to indicate
!                                parallel rather than portability.
! 31. 2002-05-16  Karen Godoger  Make the brief doc section required for 
!                                header and aux files.
! 30. 2002-04-30  Karen Goodger  Use alpha logs for alpha and beta logs for 
!                                beta.  Give warning if programs do not have
!                                production as maturity.
! 29. 2002-03-13  Karen Goodger  Use alpha directory for history logs.
! 28. 2002-03-06  Karen Goodger  Allow alpha maturity.
! 27. 2002-02-08  Karen Goodger  Add lmrkout as valid input process.
! 26. 2002-02-06  Karen Goodger  Add trinsort as valid input process.
! 25. 2002-01-08  Karen Goodger  Allow C syntax on comment card.
! 24. 2002-01-03  Karen Goodger  Fix bug in revision history number check.
!                                Zero was not being recognized as a number.
! 23. 2001-12-28  Karen Goodger  Compare revision history number with last
!                                number in its history log file.
! 22. 2001-12-27  Karen Goodger  Reads file checkc_cr created by the script.
!                                It contains lines with ^M characters. 
! 21. 2001-12-26  Karen Goodger  Print manhist message for users goodgkp, 
!                                vundedk, or sps only.
! 20. 2001-12-20  Karen Goodger  Add header files.
! 19. 2001-11-14  Karen Goodger  Add check for Parallel line in brief doc. 
! 18. 2001-11-05  Karen Goodger  Add check for entry statements. 
! 17. 2001-10-04  Karen Goodger  Allow job_data and project_data to be 
!                                exceptions to the underscore rule.
!                                Fix problem with _frou file not picking up 
!                                auxiliary tag correctly. Allow _wrapper
!                                to be an exception to the underscore rule and
!                                to indicate a wrapper routine.
! 16. 2001-08-27  Karen Goodger  Allow fortran auxiliary files.
!                                Add check for reserved names.
!                                Insure contains card has nothing else on it.
!                                Do not print pc_get_lun message if there are
!                                 no prints or writes in the file.
!                                Print message if a new io process.
! 15. 2001-06-12  Karen Goodger  Use non-advancing io to test for lines greater
!                                than 80 characters.
! 14. 2001-06-11  Karen Goodger  Print implicit none message only for fortran.
! 13. 2001-05-24  Karen Goodger  Allow print* if a program.  Check for 
!                                implicit none.
! 12. 2001-05-23  Karen Goodger  Do not allow extraneous information on 
!                                maturity card.
! 11. 2001-05-14  Karen Goodger  Allow a maturity of beta only.
! 10. 2001-05-07  Karen Goodger  Ignore write 6 and print* if comments.
!  9. 2001-05-01  Karen Goodger  Look for print * and writes to unit 6 only if
!                                fortran code. Add more checks to determine if
!                                truly a print or write statement.
!  8. 2001-04-27  Karen Goodger  Look for print *.
!  7. 2001-04-11  Karen Goodger  Check for existance of RCS ident string.
!  6. 2001-03-15  Karen Goodger  Check for pc_get_lun only if a process.
!  5. 2001-03-14  Karen Goodger  Allow for primitive gui section.
!                                Check that pc_get_lun is being used.
!  4. 2001-03-13  Karen Goodger  Allow gui section to be longer than 80 char.
!  3. 2001-03-08  Karen Goodger  Skip tag checking if unable to determine type.
!  2. 2001-03-07  Karen Goodger  Check fortran code as well as c code.
!  1. 2001-02-27  Karen Goodger  Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! Depends on the linux format of the nm command
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

      program checkc
      use getsys_module
      use putsys_module
      use string_module  
      implicit none

      character(len=100),save :: XXXX_IDENT = &
'$Id: checkc.f90,v 1.52 2006/12/20 16:57:41 Goodger prod sps $'

      integer, parameter :: ncats=18,ntags=17

      integer :: lunroutine=1,lunnames=2,lunc=3,luncr=4,lunhist=7,luntype=8
      integer :: lunmaturity=9
      integer :: histnum,istat,i,i1,i2,j
      integer :: k,k1,k1a,k1b,k1c,k1d,k2,k2a,k2b,k2c,k3,k4,k4a,k5,k6,k6a,k6b,knt=0
      integer :: kntprint=0,kntwrite=0
      integer :: nc,nccor,ncread,thisnum
      integer :: starttags(ntags),endtags(ntags),required(ntags)
      integer :: auxrequired(ntags),procrequired(ntags),progrequired(ntags)
      integer :: month,cday,year,nmonth,nday,nyear,hour

      character(len=3)  :: dow,ndow
      character(len=8)  :: ctemp,cthisnum,uname
      character(len=12) :: filetype,maturity,xlrevtype
      character(len=16) :: cdate='    -  -'
      character(len=20) :: correct,correctlc,startnames(ntags)
      character(len=20) :: endnames(ntags),pstartnames(ntags),pendnames(ntags)
      character(len=80) :: card,card1,category,ctmp80,bdate,hdate
      character(len=80) :: validprimcats(ncats),validcats(ncats)
      character(len=160):: lcard,croutine,routine,routinelc
      character(len=160):: lcard_lower,lcard_compressed,thisdir

      logical :: briefdate,histdate,foundnum,stillnum,check_naming,check_tags
      logical :: startgui,endgui,pc_get_lun,rcsident,fortran
      logical :: startbrief,endbrief,startmodule,startcontains,foundimplicit
      logical :: foundparallel,prevhist=.true.,printsok=.false.
      logical :: ok=.true.,pass=.true.,validcat=.false.,there=.false.


      call getsys_username(uname)
      call getsys_current_dir(thisdir)

      briefdate=.false.
      histdate=.false.
      check_naming=.true.
      check_tags=.true.
      startgui=.false.
      endgui=.false.
      startbrief=.false.
      endbrief=.false.
      startmodule=.false.
      startcontains=.false.
      pc_get_lun=.false.
!         quick fix to remove pc_get_lun requirement
      pc_get_lun=.true.
      rcsident=.false.
      foundimplicit=.false.
      foundparallel=.false.

      hdate=' '
      bdate=' '
      startnames=' '
      endnames=' '

      filetype='UNKNOWN'

      validcats(1)='amplitude_mod'
      validcats(2)='cfe'
      validcats(3)='diagnostics'
      validcats(4)='filters'
      validcats(5)='headers'
      validcats(6)='inversion'
      validcats(7)='io'
      validcats(8)='migrations'
      validcats(9)='miscellaneous'
      validcats(10)='multi_component'
      validcats(11)='plot'
      validcats(12)='sorts'
      validcats(13)='stacks'
      validcats(14)='statics'
      validcats(15)='synthetics'
      validcats(16)='transforms'
      validcats(17)='velocity_analysis'
      validcats(18)='main_prog'

      validprimcats(1)='amplitude_mod'
      validprimcats(2)='cfe'
      validprimcats(3)='character'
      validprimcats(4)='filters'
      validprimcats(5)='headers'
      validprimcats(6)='io'
      validprimcats(7)='main_prog'
      validprimcats(8)='math'
      validprimcats(9)='memory'
      validprimcats(10)='migrations'
      validprimcats(11)='miscellaneous'
      validprimcats(12)='moves'
      validprimcats(13)='packs'
      validprimcats(14)='plot'
      validprimcats(15)='sorts'
      validprimcats(16)='synthetics'
      validprimcats(17)='velocity'
      validprimcats(18)='velocity'

      starttags=0
      endtags=0
      required=0
      auxrequired=0
      procrequired=0
      progrequired=0

      startnames(1)='!<copyright'
      startnames(2)='!<brief_doc>'
      startnames(3)='!<descript_doc>'
      startnames(4)='!<trace_io_doc>'
      startnames(5)='!<global_doc>'
      startnames(6)='!<header_word_doc>'
      startnames(7)='!<calling_doc>'
      startnames(8)='!<advice_doc>'
      startnames(9)='!<history_doc>'
      startnames(10)='!<portability_doc>'
      startnames(11)='!<compile_doc>'
      startnames(12)='!<algorithm_doc>'
      startnames(13)='!<programming_doc>'
      startnames(14)='!<gui_def>'

      endnames(1)='!</copyright'
      endnames(2)='!</brief_doc>'
      endnames(3)='!</descript_doc>'
      endnames(4)='!</trace_io_doc>'
      endnames(5)='!</global_doc>'
      endnames(6)='!</header_word_doc>'
      endnames(7)='!</calling_doc>'
      endnames(8)='!</advice_doc>'
      endnames(9)='!</history_doc>'
      endnames(10)='!</portability_doc>'
      endnames(11)='!</compile_doc>'
      endnames(12)='!</algorithm_doc>'
      endnames(13)='!</programming_doc>'
      endnames(14)='!</gui_def>'

      pstartnames(1)='!<copyright'
      pstartnames(2)='!<brief_doc>'
      pstartnames(3)='!<descript_doc>'
      pstartnames(4)='!<advice_doc>'
      pstartnames(5)='!<trace_in_doc>'
      pstartnames(6)='!<trace_out_doc>'
      pstartnames(7)='!<global_doc>'
      pstartnames(8)='!<header_word_doc>'
      pstartnames(9)='!<history_doc>'
      pstartnames(10)='!<portability_doc>'
      pstartnames(11)='!<compile_doc>'
      pstartnames(12)='!<calling_doc>'
      pstartnames(13)='!<int_calling_doc>'
      pstartnames(14)='!<algorithm_doc>'
      pstartnames(15)='!<programming_doc>'
      pstartnames(16)='!<gui_def>'
      pstartnames(17)='!<HelpSection>'


      pendnames(1)='!</copyright'
      pendnames(2)='!</brief_doc>'
      pendnames(3)='!</descript_doc>'
      pendnames(4)='!</advice_doc>'
      pendnames(5)='!</trace_in_doc>'
      pendnames(6)='!</trace_out_doc>'
      pendnames(7)='!</global_doc>'
      pendnames(8)='!</header_word_doc>'
      pendnames(9)='!</history_doc>'
      pendnames(10)='!</portability_doc>'
      pendnames(11)='!</compile_doc>'
      pendnames(12)='!</calling_doc>'
      pendnames(13)='!</int_calling_doc>'
      pendnames(14)='!</algorithm_doc>'
      pendnames(15)='!</programming_doc>'
      pendnames(16)='!</gui_def>'
      pendnames(17)='!</HelpSection>'


      required(1)=1
      required(2)=1
      required(3)=1
      required(7)=1
      required(9)=1
      required(10)=1

      auxrequired(1)=1
      auxrequired(2)=1
      auxrequired(9)=1

      progrequired(1)=1
      progrequired(2)=1
      progrequired(3)=1
      progrequired(9)=1      

      procrequired(1)=1
      procrequired(2)=1
      procrequired(3)=1
      procrequired(4)=1
      procrequired(5)=1
      procrequired(6)=1
      procrequired(7)=1
      procrequired(8)=1
      procrequired(9)=1
      procrequired(10)=1
      procrequired(11)=1
      procrequired(12)=1
      procrequired(13)=1
      procrequired(14)=1
      procrequired(15)=1
      procrequired(16)=1
      procrequired(17)=1


      call date_and_time(ctemp)
      cdate(1:4)=ctemp(1:4)
      cdate(6:7)=ctemp(5:6)
      cdate(9:10)=ctemp(7:8)

      call string_time_date(ctmp80)
      call string_to_lower(ctmp80)
      dow=ctmp80(1:3)
      call string_cc2ii(cdate(1:4),year)
      call string_cc2ii(cdate(6:7),month)
      call string_cc2ii(cdate(9:10),cday)
      call string_cc2ii(ctmp80(12:13),hour)
      if(hour.ge.21.and.uname(1:3).ne.'sps')then
        call checkc_tomorrow(dow,month,cday,year,nmonth,nday,nyear,ndow)
        dow=ndow
        month=nmonth
        cday=nday
        year=nyear
      endif

      call checkc_tuthur(dow,month,cday,year,nmonth,nday,nyear)


      call string_ii2cc(nyear,cdate(1:4))
      if(nmonth.ge.10)then
         call string_ii2cc(nmonth,cdate(6:7))
      else
         call string_ii2cc(nmonth,cdate(7:7))
      endif
      if(nday.ge.10)then
        call string_ii2cc(nday,cdate(9:10))
      else
        cdate(9:9)='0'
        call string_ii2cc(nday,cdate(10:10))
      endif

      open(lunmaturity,file='./xlrev_maturity',status='old',iostat=istat)
      if(istat.ne.0)then
        print*,' Unable to open file xlrev_maturity'
        stop
      endif
      read(lunmaturity,9001,iostat=istat)maturity
      call string_to_lower(maturity)

      open(lunroutine,file='./checkc_routine',status='old',iostat=istat)
      if(istat.ne.0)then
        print*,' Unable to open file checkc_routine'
        stop
      endif
      read(lunroutine,9001,iostat=istat)routine
      k=index(routine,'_')
      k1=index(routine,'job_data')
      k1a=index(routine,'project_data')
      k1b=index(routine,'_crou')
      k1c=index(routine,'_frou')
      k1d=index(routine,'int_api')
      if(k.ne.0.and.(k1+k1a+k1b+k1c+k1d).eq.0)then
        print*,'The file name must not contain an underscore'
        print*,' '
        pass=.false.
      endif
      open(lunnames,file='./checkc_names',status='old',iostat=istat)
      if(istat.ne.0)then
        check_naming=.false.
        fortran=.true.
      else
        fortran=.false.
      endif
      open(luncr,file='./checkc_cr',status='old',iostat=istat)
      if(istat.ne.0)then
        print*,' Unable to open file checkc_cr'
        stop
      endif


      read(luncr,9001,iostat=istat)card
      if(istat.eq.0)then
        print*,' '
        print*,' Your file contains ^M characters'
        print*,' You can strip them out with the following command'
        print*,' col -bx <input>output'
        print*,' '
        pass=.false.
      endif

      open(luntype,file='./xlrev_filetype',iostat=istat)
      if(istat.ne.0)then
        print*,' Unable to open file xlrev_filetype'
        stop
      endif
      read(luntype,9001,iostat=istat)xlrevtype


      k1=index(routine,'.f90')
      k2=index(routine,'.h')
      if((k1+k2).ne.0)then
        if(k2.ne.0)fortran=.false.
        croutine=routine
        go to 50
      endif
      k=index(routine,'.c')
      if(k.eq.0)then
        croutine=trim(routine) // '.c'
        fortran=.false.
      endif
 50   continue
      open(lunc,file=croutine,status='old',iostat=istat)
      if(istat.ne.0)then
        print*,'Unable to open file ',croutine
        stop
      endif

!          Check naming conventions
      k=index(routine,'_crou')
      if(k.ne.0)then
        routine(k:k+4)=' '
      endif
      k=index(routine,'_frou')
      if(k.ne.0)then
        routine(k:k+4)=' '
      endif
      k=index(routine,'_wrapper')
      if(k.ne.0)then
        routine(k:k+7)=' '
      endif
      k=index(routine,'.f90')
      if(k.ne.0)then
        routine(k:k+3)=' '
      endif
      routinelc=routine
      call string_to_lower(routinelc)
!          If there is a full path name - parce out the routine name
      k=index(routine,'/',.true.)
      if(k.ne.0)then
        routine(1:k)=' '
        routine=adjustl(routine)
      endif

      correct=trim(routine) // '_'
      call string_to_upper(correct)
      correctlc=correct
      call string_to_lower(correctlc)
      nccor=len_trim(correctlc) 
      call checkc_reserved(correctlc(1:nccor-1))
      if(check_naming)then
        DO
          read(lunnames,9001,iostat=istat)card
          if(istat.lt.0)exit
          call string_to_upper(card)
          k=index(card,trim(correct))
          k2=index(card,'MAIN')
          if(k.eq.0)then
            k3=len_trim(card)
            k4=len_trim(correct)
            k5=index(card,correct(1:k4-1))
            print*,card(k5:k3),correct(1:k4-1)
            if(card(k5:k3).eq.correct(1:k4-1))cycle
            if(k2.ne.0)cycle
            print*,'correct = ',correct
            print*,'Naming convention problem...'
            print*,card
            pass=.false.
          endif
        ENDDO
      endif

!          Check source code for standards
      foundnum=.false.
      DO
        read(lunc,9001,iostat=istat,advance='NO',size=ncread,eor=75)lcard
        if(istat.lt.0)exit
 75     continue
        lcard_lower=lcard
        call string_to_lower(lcard_lower)
        call string_compress_blanks(lcard_lower,lcard_compressed)
        knt=knt+1
!          check for start of module and contains line
        if(fortran.and.trim(xlrevtype).eq.'programs'.and..not.foundimplicit)then
!             Need to imporove this check on programs
!             implicit none should be in each subroutine
          k=index(lcard_compressed,'implicit none')
          if(k.ne.0)foundimplicit=.true.
        endif
        if(fortran.and..not.startmodule.and.filetype.ne.'PROGRAM')then  
          k=index(lcard_lower,'module')
!             is this the start of the module?
            if(k.ne.0)then
              j=index(lcard_lower,trim(routine))
              if(j.ne.0.and.j.gt.k)then
                startmodule=.true.
              endif
            endif
        endif
        if(fortran.and..not.startcontains.and.filetype.ne.'PROGRAM')then  
          k=index(lcard_lower,'contains')
!             is this the contains line of a module?
            if(k.ne.0)then
              nc=len_trim(lcard_lower)
              if(nc.eq.(k+7).and.lcard_lower(1:k-1).eq.' ')then
                startcontains=.true.
              endif
            endif
        endif
!                 Look for implicit none
        if(fortran.and.startmodule.and..not.startcontains)then
          k=index(lcard_compressed,'implicit none')
          if(k.ne.0)foundimplicit=.true.
        endif
!                 Allow for beta maturity, in brief section
!                 If a program should be production maturity
        if(startbrief.and..not.endbrief)then
          k=index(lcard,'Parallel')
          k1=index(lcard_lower,'revised')
          if(k.ne.0.and.k1.eq.0)then
             foundparallel=.true.
!                 Parallel must be yes or no
               k1=index(lcard_lower,'yes')
               k2=index(lcard_lower,'no')
               if((k1+k2).eq.0)then
                 print*,'Parallel MUST be Yes or No.'
                 print*,trim(lcard_lower)
                 print*,' '
                 pass=.false.
               endif
          endif
          k=index(lcard_lower,'maturity')
          if(k.ne.0)then
!    maturity now set earlier in program from xlrev_maturity card
            k1=index(lcard_lower,'beta')
            k3=index(lcard_lower,'alpha')
            if(filetype.eq.'PROGRAM'.and.maturity.ne.'production')then
              print*,' '
              print*,' It is unusual for there to be two versions of a program'
              print*,' Generally, a programs maturity will be production.'
              print*,' Please change the maturity if there is only one version &
                       &of this program'
              print*,' '
            endif
!   if sps account compare the maturity to the spsmodules directory we are in
            if(trim(uname).eq.'sps')then
              if(trim(maturity).eq.'beta')then
                k=index(thisdir,'beta')
                if(k.eq.0)then
                  print*,' '
                  print*,' The maturity is beta, but you are not in the beta &
                          &directory'
                  print*,' '
                  pass=.false.
                endif
              endif
              if(trim(maturity).eq.'alpha')then  
                k=index(thisdir,'alpha')
                if(k.eq.0)then
                  print*,' '
                  print*,' The maturity is alpha, but you are not in the alpha &
                          &directory'
                  print*,' '
                  pass=.false.
                endif
              endif
            endif
            if(maturity.ne.'beta')then
              if(filetype.ne.'PROGRAM')then
                print*,' '
                print*,' Maturity MUST be beta'
                print*,lcard(1:80)
                pass=.false.
              endif
              if(maturity.eq.'production'.and.filetype.eq.'PROGRAM')then
                print*,' Maturity is production'
              endif
            else
              print*,'maturity = ',trim(maturity)
!                Insure nothing else on the Maturity card
              k5=k1+4
              if(k3.ne.0)k5=k3+5
              do i=k5,80
                if(lcard(i:i).eq.' ')cycle
                print*,' '
                print*,' Extraneous information found on Maturity card'
                print*,lcard(1:80)
                pass=.false.
                exit
              enddo
            endif
          endif
        endif
        if(.not.pc_get_lun)then
          k=index(lcard,'pc_get_lun')
          if(k.ne.0)then
!                Make sure it is not a comment
            j=index(lcard,'!')
            if(k.lt.j.or.j.eq.0)pc_get_lun=.true.     
          endif
        endif
        if(.not.rcsident)then
          k=index(lcard,'$Id')
          if(k.ne.0)then
            rcsident=.true.
            if(uname.eq.'sps')then
              if(maturity.eq.'beta')then
                j=index(lcard,'beta')
                if(j.eq.0)then
                  print*,' '
                  print*,' Need to fix rcsident card beta'
                  print*,' '
                  pass=.false.
                endif
              else if(maturity.eq.'alpha')then
                j=index(lcard,'alpha')
                if(j.eq.0)then
                  print*,' Need to fix rcsident card '
                  pass=.false.
                endif
              endif
            endif
          endif
        endif
!                 Look for print * and writes to unit 6 if fortran
!                 Quick fix to allow print* and writes
        printsok=.true.
        if(fortran.and.filetype.ne.'PROGRAM'.and..not.printsok)then
          k1=index(lcard_compressed,'print')
          if(k1.gt.0)then
!              Insure this is a print statement
!                Ignore if a comment
            k=index(lcard_compressed,'!')
            if(k.gt.0.and.k.lt.k1)go to 150
!                There should be a space in front unless it starts in col 1
            if(k1.gt.1.and.lcard_compressed(k1-1:k1-1).ne.' ')go to 150
            k2=index(lcard_compressed,'*')
            if(k2.eq.0)then
!                Is it a print statement label, (print 9001)
              if(lcard_compressed(k1+5:k1+5).ne.' ')go to 150
              j=ichar(lcard_compressed(k1+6:k1+6))
              if(j.ge.49.and.j.le.57)then
                 print*,' print statements are not allowed...'
                 print*,lcard_compressed(1:80)
              endif
            endif
            if(k2.gt.k1)then
              if((k2-k1).le.10)then
!               All characters between the print and the * should be spaces
                do i=k1+5,k2-1
                  if(lcard_compressed(i:i).ne.' ')go to 150
                enddo
                print*,' print* is not allowed'
                print*,lcard_compressed(1:80)
              endif
            endif
            kntprint=kntprint+1
          endif
 150      continue
          k1=index(lcard,'write')
          if(k1.eq.0)go to 160
          k=index(lcard_compressed,'call pc_')
          if(k.ne.0)go to 160
!                Ignore if a comment
            k=index(lcard,'!')
            if(k.gt.0.and.k.lt.k1)go to 160
!            There should be a space in front of write unless its col 1
          if(k1.gt.1.and.lcard(k1-1:k1-1).ne.' ')go to 160
          if(k1.gt.0)then
            k2=index(lcard,'6')
            if(k2.gt.k1)then
              if((k2-k1).le.10)then
!                    The information between write and 6 can be a left paren
!                      space or "unit="
                do i=k1+5,k2-1
                  k=index(lcard(i:i),'unit=')
                  if(lcard(i:i).ne.' '.and.lcard(i:i).ne.'('.and.k.eq.0)&
                     go to 160
                  if(k.ne.0)cycle
                enddo
                print*,' writes to unit 6 are not allowed'
                print*,lcard(1:80)
              endif
            endif
            kntwrite=kntwrite+1
          endif
        endif
 160    continue
!                 Do not allow entry statements
        k=index(lcard_lower,' entry ')
        if(k.ne.0)then
          k1=index(lcard_compressed,'!')
          if(k1.ne.0)then
            if(k1.lt.k)go to 162  !  comment card, stop checking
          endif

!                Word entry found and it is not a comment
!                If it is a print statement, then its ok
          ok=.false.
          k1=index(lcard_compressed,'call pc_')
          if(.not.fortran)ok=.true.
          if(k1.ne.0)ok=.true.
          k1=index(lcard_compressed,'print')
          if(k1.ne.0.and.k1.lt.k)ok=.true.
          if(.not.ok)then
            print*,' entry statements are not allowed'
            print*,trim(lcard_compressed)
            print*,' '
            pass=.false.
          endif
        endif
162     continue

!                 Check naming conventions if fortran
        if(fortran)then
           if(lcard_compressed(1:1).eq.'!')go to 163
           k=index(lcard_compressed,'end program')
           if(k.ne.0)then
             i1=k+12
             i2=index(lcard_compressed(i1:),' ')
             if(i2.eq.0)then
               i2=ncread
             else
               i2=i2+i1
             endif
             if(lcard_compressed(i1:i2).ne.trim(routinelc))then
               print*,'Naming convention problem...'
               print*,lcard_compressed
               pass=.false.
             endif
           endif
           k=index(lcard_compressed,'end module')
           k1=index(lcard_compressed,'end subroutine')
           k2=index(lcard_compressed,'end function')
           if((k+k1+k2).ne.0)then
             if(k.gt.0)i1=k+11
             if(k1.gt.0)i1=k1+15
             if(k2.gt.0)i1=k2+13
             i2=index(lcard_compressed,'_')
             if(i2.eq.0)then
               nc=len_trim(lcard_compressed)
               if(lcard_compressed(i1:nc).eq.correctlc(1:nccor-1))go to 163
             endif
             if(lcard_compressed(i1:i1+3).eq.'job_')then
               i2=i2+5    ! Exception for job_data
             else if(lcard_compressed(i1:i1+7).eq.'project_')then
               i2=i2+5    ! Exception for project_data
             endif
             if(lcard_compressed(i1:i2).ne.correctlc(1:nccor).and.&
                startcontains)then
               print*,'Naming convention problem...'
               print*,lcard_compressed(1:60)
               pass=.false.
             endif
           endif
        endif
 163    continue
!                 Check for lines over 80 unless in gui section
!!      if(startgui.and..not.endgui)go to 165
!!      k=index(lcard,'$Id')
!!      if(k.ne.0)go to 165
!!      if(ncread.gt.80)then
!!        print*,' card ',knt,' is greater than 80 characters'
!!        print*,' character passed col 80 are...'
!!        print*,lcard(81:ncread)
!!        pass=.false.
!!      endif
!165    continue
        stillnum=.false.
        k=1
        cthisnum=' '
        DO i=1,ncread
          j=ichar(lcard(i:i))
!                foundnum flag used in checking revision history date
          if(starttags(9).eq.1.and.endtags(9).eq.0)then
            if(j.ge.48.and.j.le.57.and.i.lt.40.and..not.foundnum)then
              foundnum=.true.
              stillnum=.true.
            endif
            if(stillnum)then
              if(j.ge.48.and.j.le.57)then
                cthisnum(k:k)=lcard(i:i)
                k=k+1
              else
                stillnum=.false.
              endif
            endif
          endif
          if(j.lt.0.or.j.gt.127)then
            print*,'card ',knt,' contains non-ascii characters'
            pass=.false.
          endif
        ENDDO 
        if(knt.eq.1)then
          k1=0
          k1a=0
          k1b=0
          k2=0
          k2a=0
          k2b=0
          k2c=0
          k3=0
          k4=0
          k4a=0
          k5=0
          k6=0
          k6a=0
          k1a=index(lcard,'/*<CPS_v1 type="PRIMITIVE"')
          k1b=index(lcard,'!<CPS_v1 type="PRIMITIVE"')
          k2=index(lcard,'!<CPS_v1 type="PROGRAM"/>')
          k2b=index(lcard,'/*<CPS_v1 type="PROGRAM"')
          k4=index(lcard,'/*<CPS_v1 type="AUXILIARY_FILE"')  
          k4a=index(lcard,'!<CPS_v1 type="AUXILIARY_FILE"')  
          k5=index(lcard,'!<CPS_v1 type="PROCESS"')
          k6=index(lcard,'!<CPS_v1 type="HEADER_FILE"/>')
          k6b=index(lcard,'/*<CPS_v1 type="HEADER_FILE"')         
          card1=lcard(1:80)
          cycle
        endif
        if(knt.eq.2)then
          k1=index(lcard,'!<CPS_v1 type="PRIMITIVE"/>')
          k2a=index(lcard,'!<CPS_v1 type="PROGRAM"/>')
          k2c=index(lcard,'/*<CPS_v1 type="PROGRAM"')
          k3=index(lcard,'!<CPS_v1 type="AUXILIARY_FILE"/>')
          k6a=index(lcard,'!<CPS_v1 type="HEADER_FILE"')
          k=k1+k2+k2a+k2b+k2c+k3+k4+k4a+k1a+k1b+k5+k6+k6a+k6b
          if(k.eq.0)then
            print*,' CPS_v1 type card not found'
            print*,' card1 = ',card1
            print*,' card2 = ',lcard(1:80)
            print*,' Cannot properly check tags'
            print*,' Tag check will be skipped'
            check_tags=.false.
            pass=.false.
          else
            if((k1+k1a+k1b).ne.0)then
              print*,' type is primitive'
              filetype='PRIMITIVE'
            endif
            if((k2+k2a+k2b+k2c).ne.0)then
               filetype='PROGRAM'
               print*,' type is program'
               required=progrequired
            endif
            if((k3+k4+k4a).ne.0)then
              j=index(croutine,'.h')
              if(j.ne.0)then
                print*,' Type should be HEADER_FILE for the .h extension'
                filetype='HEADER'
                required=auxrequired
                pass=.false.
              else
                filetype='AUXILIARY'
                print*,' type is auxiliary'
                required=auxrequired
              endif
            endif
            if(k5.ne.0)then
              filetype='PROCESS'           
              print*,' type is process'
              print*,' '
              required=procrequired
              startnames=pstartnames
              endnames=pendnames
            endif
            if((k6+k6a+k6b).ne.0)then
              filetype='HEADER'
              print*, 'type is HEADER_FILE'
              required=auxrequired
            endif
            if(filetype.ne.'PROCESS')pc_get_lun=.true.
            if(filetype.eq.'AUXILIARY'.or.filetype.eq.'HEADER')rcsident=.true.
          endif
        endif
!               If in brief_doc or history_doc sections - check the date
!                 and get category
        if(.not.briefdate.and.starttags(2).eq.1.and.endtags(2).eq.0)then
          k=index(lcard_compressed,'category')
          if(k.ne.0.and.filetype.ne.'PROGRAM')then
            k1=index(lcard_compressed,':') +2
            k2=len_trim(lcard_compressed)
            category=lcard_compressed(k1:k2)
            if(filetype.eq.'PRIMITIVE'.or.trim(xlrevtype).eq.'primitives')&
               validcats=validprimcats
            validcat=.false.
            do i=1,ncats
              if(trim(validcats(i)).eq.category)validcat=.true.
            enddo
            if(.not.validcat.and.filetype.ne.'HEADER'.and.trim(xlrevtype).ne.&
                'programs')then
              print*,' '
              print*,trim(category),' is not a valid category for filetype ',&
                     trim(xlrevtype)
              print*,' '
              pass=.false.
            endif
          endif
          k=index(lcard,trim(cdate))
          if(k.ne.0)briefdate=.true.
          call string_to_lower(lcard)
          k=index(lcard,'revised')
          if(k.ne.0)then
            bdate=lcard(1:80)
          endif
        endif
        if(.not.histdate.and.starttags(9).eq.1.and.endtags(9).eq.0)then
          k=index(lcard,trim(cdate))
          if(k.ne.0)histdate=.true.
!             Assume latest revision history entry is first card with a number
          if(foundnum.and.hdate.eq.' ')then
            hdate=lcard(1:80)
!              Compare revision history number with log file
            k1=index(croutine,'.')
            k2=index(croutine,'/',.true.)
            if(k2.eq.0)then
              k2=1
            else
              k2=k2+1
            endif
            if(filetype.eq.'HEADER')then
              ctmp80='checkc_rcs'
            else
              ctmp80='checkc_rcs'
            endif
            open(lunhist,file=ctmp80,status='old',iostat=istat)
            if(istat.ne.0)then
              if(uname.eq.'sps')then
                print*,' '
                print*,'Unable to open RCS file ',trim(ctmp80)
                print*,'Unable to check revision history'
                print*,'OK if this is the first version'
                print*,' '
!!!!                pass=.false.
              endif
              prevhist=.false.
            endif
            histnum=-2
            if(prevhist)then
              do 
                read(lunhist,9001,iostat=istat)card
                k2=index(card,'head:')
                if(k2.eq.0)cycle
                k3=index(card,'.',.true.)
                nc=len_trim(card)
                ctemp=card(k3+1:nc)
                call string_cc2ii(ctemp,histnum)
                exit          
              enddo
              if(histnum.eq.-2)then
                print*,'Unable to determine history number from .history file'
                pass=.false.
              else
                call string_cc2ii(cthisnum,thisnum)
                if((thisnum-histnum).ne.1)then
                  print*,' '
                  print*,' Revision history numbering problem'
                  print*,' Your number = ',thisnum
                  print*,' Last number = ',histnum
                  print*,' The next revision MUST be ',histnum+1
                  print*,' '
                  pass=.false.
                endif
              endif
            endif
          endif
        endif
        IF(check_tags)then
!               Flag tags
          do i=1,ntags
            if(startnames(i).eq.' ')exit
            k=index(lcard,trim(startnames(i)))
            if(k.ne.0)then
              starttags(i)=1
!                  Check order
              do j=1,i
                if(starttags(j).eq.0.and.required(j).eq.1)then
                  print*,' Tag ',trim(startnames(i)),' is out of order'
                  pass=.false.
                endif
              enddo
            endif
            k=index(lcard,trim(endnames(i)))
            if(k.ne.0)endtags(i)=1
          enddo
!                    Set gui flags
          if(endgui)cycle
          select case (filetype)
            case('PROCESS')
              if(starttags(16).eq.1)startgui=.true.
              if(endtags(16).eq.1)endgui=.true.
            case('PRIMITIVE')
              if(starttags(14).eq.1)startgui=.true.
              if(endtags(14).eq.1)endgui=.true.
          end select
!
!                   Set brief flags
          if(endbrief)cycle
          if(starttags(2).eq.1)startbrief=.true.
          if(endtags(2).eq.1)endbrief=.true.
        ENDIF
      ENDDO


!                    Check tags
!
      if(check_tags)then
        do i=1,ntags
          if(starttags(i).eq.1.and.endtags(i).eq.0)then
             print*,' Tag ',trim(startnames(i)),' has no ending tag'
             pass=.false.
          endif
          if(endtags(i).eq.1.and.starttags(i).eq.0)then
            print*,' Tag ',trim(endnames(i)),' has no starting tag'
            pass=.false.
          endif
          if(required(i).eq.1.and.starttags(i).eq.0)then
            print*,' Required tag ',trim(startnames(i)),' is missing'
            pass=.false.
          endif
        enddo
      endif

      if(.not.pc_get_lun.and.(kntprint.gt.0.or.kntwrite.gt.0))then
 
        print*,' pc_get_lun not found in file'
        print*,' This function should be used to get the unit number for &
                &standard out'
        print*,' if you are using write or print statements'
        print*,' '
      endif

      if(.not.foundimplicit.and.fortran)then
        print*,' implicit none not found in file'
        print*,' startmodule = ',startmodule
        print*,' startcontains = ',startcontains
        print*,' '
        pass=.false.
      endif

      if(filetype.eq.'PROCESS'.and..not.foundparallel)then
        print*,'Parallel card not found in brief doc section'
        print*,' '
        pass=.false.
      endif

      if(.not.rcsident)then
        print*,' RCS ident string not found in file'
        print*,' '
        pass=.false.
      endif

      if(category.eq.'io'.and.filetype.eq.'PROCESS'.and.(uname.eq.'goodgkp'&
         .or.uname.eq.'sps'))then
        if(correctlc.ne.'ttrin_'.and.correctlc.ne.'ttrot_'.and.&
           correctlc.ne.'trin_'.and.correctlc.ne.'trot_'.and.correctlc&
           .ne.'lmrkin_'.and.correctlc.ne.'trinsort_'.and.correctlc.ne.&
            'lmrkout_')then
          print*,' New io process - add to manhist'
          print*,' '
        endif
      endif

!                   Check dates
      
      if(.not.briefdate.and.required(2).eq.1)then
        print*,' The checkin date was not found in the brief_doc section'
        print*,trim(bdate)
        print*,' The checkin date is ',cdate
        pass=.false.
      endif
      if(.not.histdate)then
        print*,' The checkin date was not found in the history_doc section'
        print*,trim(hdate)
        print*,'The checkin date is ',cdate
        pass=.false.
      endif

      there=.false.
      inquire(file='xlrev_abort',exist=there)
      if(there)pass=.false.

      if(pass)then
!!!!        if(uname.eq.'sps')call putsys_cmd('touch CHECKC_PASSED')
        print*,' checkc complete'
      else
        call putsys_cmd('touch CHECKC_FAILED')
        print*,'             ***                   **                 *  '
        print*,'             *   *            *      *                 * ' 
        print*,'             *                       *                 * ' 
        print*,'             *       ****    **      *     ****    *** * ' 
        print*,'            ****         *    *      *    *    *  *   ** ' 
        print*,'             *       *****    *      *    ******  *    * ' 
        print*,'             *      *    *    *      *    *       *    * ' 
        print*,'             *      *   **    *      *    *    *  *   ** ' 
        print*,'             *       *** *  *****  *****   ****    *** * ' 
        
      endif

 9001 format(A)
      end program checkc
      subroutine checkc_reserved(name)
 
      character(len=*),intent(in) :: name

      integer,parameter :: nreserved=22

      integer :: i

      character(len=32) :: reserved_names(nreserved)

!          Names 1 - 22 are in lmrk_stubs.c
      data reserved_names(1)/'cs2ano'/
      data reserved_names(2)/'cs2int'/
      data reserved_names(3)/'dm3dcc'/
      data reserved_names(4)/'dm3dcl'/
      data reserved_names(5)/'dm3dco'/
      data reserved_names(6)/'dm3dft'/
      data reserved_names(7)/'dm3dmx'/
      data reserved_names(8)/'dm3dnx'/
      data reserved_names(9)/'dm3dop'/
      data reserved_names(10)/'dm3dou'/
      data reserved_names(11)/'dm3dpf'/
      data reserved_names(12)/'dm3dtr'/
      data reserved_names(13)/'hrzcls'/
      data reserved_names(14)/'hrzfnd'/
      data reserved_names(15)/'hrzhdr'/
      data reserved_names(16)/'hrzopn'/
      data reserved_names(17)/'hrzrw'/
      data reserved_names(18)/'hzdcls'/
      data reserved_names(19)/'prjend'/
      data reserved_names(20)/'prjin2'/
      data reserved_names(21)/'pumknm'/
      data reserved_names(22)/'srvbnd'/

     DO i=1,nreserved
       if(name.eq.reserved_names(i))then
         print*,' Name ',trim(reserved_names(i)),' is reserved'
         print*,' You must choose another name'
       endif
     ENDDO

      end subroutine checkc_reserved

      SUBROUTINE checkc_tuthur (dow,month, cday, year, nmonth, nday, nyear)
!
!
!            inputs: dow,month,cday,year -- today
!           outputs: nmonth,nday,nyear---The checkin date

      implicit none



      character(len=3) :: cdays(7),dow
      integer          :: cday,days2target,i,k,month
      integer :: nday,nmonth,nyear,target,thismonth,year

      cdays(1)='sun'
      cdays(2)='mon'
      cdays(3)='tue'
      cdays(4)='wed'
      cdays(5)='thu'
      cdays(6)='fri'
      cdays(7)='sat'

      if(dow.eq.'wed'.or.dow.eq.'thu')then
        target=5
      else
        target=3
      endif

!        Determine the number of days to the target day
      do i=1,7
        if(dow.eq.cdays(i))exit
      enddo
      if(i.gt.target)target=target+7
      days2target=target-i



!          Add days2target to today to get checkin date
 
      nyear=year
      nmonth=month
      nday=cday+days2target

      select case(month)

        case(1,3,5,7,8,10)
          
          if(nday.gt.31)then
            nmonth=nmonth+1
            nday=nday-31
          endif

        case(2)

          thismonth=28
          k=mod(year,4)
          if(k.eq.0)thismonth=29
          if(nday.gt.thismonth)then
            nday=nday-thismonth
            nmonth=3
          endif

        case(4,6,9,11)
          
           if(nday.gt.30)then
             nday=nday-30
             nmonth=nmonth+1
           endif

        case(12)

           if(nday.gt.31)then
             nday=nday-31
             nmonth=1
             nyear=nyear+1
           endif

      end select


      end subroutine checkc_tuthur
      SUBROUTINE checkc_tomorrow (dow,month,cday,year,nmonth,nday,nyear,ndow)

      implicit none

!          Given todays date, return tomorrow's date


      character(len=3) :: cdays(7),dow,ndow
      integer          :: cday,days2target,i,k,month
      integer :: nday,nmonth,nyear,target,thismonth,year

 

      cdays(1)='sun'
      cdays(2)='mon'
      cdays(3)='tue'
      cdays(4)='wed'
      cdays(5)='thu'
      cdays(6)='fri'
      cdays(7)='sat'


!!        Determine tomorrows - day of week
      do i=1,7
        if(dow.eq.cdays(i))exit
      enddo
      k=i+1
      if(k.eq.8)k=1
      ndow=cdays(k)



!          Add 1 to today 
 
      nyear=year
      nmonth=month
      nday=cday+1

      select case(month)

        case(1,3,5,7,8,10)
          
          if(nday.gt.31)then
            nmonth=nmonth+1
            nday=1
          endif

        case(2)

          thismonth=28
          k=mod(year,4)
          if(k.eq.0)thismonth=29
          if(nday.gt.thismonth)then
            nday=1
            nmonth=3
          endif

        case(4,6,9,11)
          
           if(nday.gt.30)then
             nday=1
             nmonth=nmonth+1
           endif

        case(12)

           if(nday.gt.31)then
             nday=1
             nmonth=1
             nyear=nyear+1
           endif

      end select


      end subroutine checkc_tomorrow



!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!




