!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- manhist.f90 --------------------------------!!
!!------------------------------- manhist.f90 --------------------------------!!
!!------------------------------- manhist.f90 --------------------------------!!
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
! Name       : manhist
! Category   : main_prog
! Written    : 2001-08-03   by: Karen Goodger
! Revised    : 2007-02-15   by: Bill Menger
! Maturity   : beta
! Purpose    : Manage history within a job stream
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!  
!  Manage histories which were read in by TRIN or TTRIN and prepare histories
!  for output by TROT or TTROT.
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
!
!
!    call manhist_check_name(name,status)
!
!                  Check the name of an output to see if it already exists in
!                  the history.  Job should be aborted if the name already
!                  exists.
!
! character(len=*)   name     = Name of the output
! integer            status   = status return
!                               0 = OK
!                              -2 = Name already exists.  Calling program
!                                   should abort job.
!
!
!
!                              opt
!                     i    b   i 
!    call manhist_cio(unit,opt,process_number_arg)
!
!                 Output history to a cio file
!
! integer            unit               = cio unit number 
! character(len=*)   opt                = History option, MODEL,ALL,NONE,CURRENT
! integer            process_number_arg = process number.  If > 0, writes
!                                         history cards through this process
!                                         number.  Otherwise, writes all history
!                                         cards.
!
!
!
!
!
!                          opt     opt      opt
!                          o       o        i
!    call manhist_get_info(knthist,maxcards,opt)
!
! integer          knthist    = The total number of history records in the
!                               history file.  If opt=MODEL, the number of
!                               history records in the model is returned.
! integer          maxcards   = The number of cards in the largest history.
! character(len=8) opt        = The history file option,MODEL,ALL,NONE,CURRENT
!                               If not present, ALL is used.
!
!
!
!
!
!                                           opt     opt      opt
!                                i    o     o       o        o  
!    call manhist_get_table_info(irec,istat,version,numcards,seconds)
!
!                 Get information about history irec
!
! integer          irec       = The history record number
! integer          istat      = Status return - zero = OK.
! character(len=4) version    = The history version number
!                               0.0 = cray
!                               1.0 = history module
!                               2.0 = manhist module
! integer          numcards   = The number of cards in history irec
! integer          seconds    = The date expressed as the number of seconds 
!                               since 1970, for history irec.
!
!
!
!
!                            b
!    call manhist_initialize(istat)
!
!                 Initialize history.  This routine is NOT needed if putcard
!                 or putrecord is called before trace processing.  Otherwise,
!                 it should be called before trace processing begins.
!
! integer             istat   = status return, zero = OK
!
!
!
!
!
!
!    call manhist_finished_reading
!
!                 Called only by tape, so manhist will know when all histories
!                 have been read.  Manhist can determine this from tags if 
!                 histories are from a trcio file.
!
!
!
!
!                            i   o   o      o       i
!    call manhist_gethistory(rec,buf,ncards,nheader,opt)
!
!                 Gets history number rec
!
! integer            rec     = The history record number
! character(len=80)  buf(:)  = The array to hold the history record
! integer            ncards  = The number of history cards in history rec
! integer            nheader = The number of header cards in history rec
!                              There will be nheader header cards before the
!                              history cards
! character(len=*)   opt     = The history option, MODEL,ALL,NONE,CURRENT
!
!
!
!                               i   o
!    call manhist_gethistoryrec(rec,buf)
!
!                 Gets unmodeled history number rec
!
! integer            rec     = The history record number
! character(len=80)  buf(:)  = The array to hold the history record
!
!
!                           o
!    call manhist_histsused(buf)
!
!                 Returns the histories to be used in array buf
!
! integer             buf     = Array containing history numbers to be used.
!
!
!
!                       b
!    call manhist_phist(opt)
! 
!                 Print history to the report file.
!
! character(len=*)    opt     = The history file option, MODEL,ALL,NONE,CURRENT
!
!
!
!
!
!                         i    o
!    call manhist_putcard(card,istat)
!
!                 Puts one card to the history.  This routine buffers up
!                 cards into one history record based on the AS_HIST flag
!                 as card 1.
!
! character(len=80)   card     =  One history card
! integer             istat    =  Status return, zero = OK.
!
!
!
!
!
!                           i     i      o     i    o        o
!    call manhist_putrecord(bufin,lcards,istat,cray,nwihcray,ndptcray)
!
!                Puts a history record from tape to the history file
!
! real      bufin    = buffer containing the history.
! integer   lcards   = number of cards in the largest history
! integer   istat    = return status - 0 = OK
! logical   cray     = cray flag, true or false
! integer   nwihcray = number of cray header words
! integer   ndptcray = number of cray sample points
!
!
!    call manhist_track
!
!                 Track through histories by inputs and outputs and generate
!                 a table of histories to keep.
!
!
!
!
!
!  An input program needs to read the history and output it to the history
!  file.  If the input reads a trcio file, it should call the routine trcio_
!  read_history_cards before trace processing begins.  See trcio for specific
!  documentation.  TRIN is an example of a process using this routine.
!
!  An output program needs to write the history to appropriate media.  If
!  writing to a trcio file, the routine trcio_write_history cards should be
!  called prior to writing any data.  See trcio for specific documentation.
!  An output program is responsible to insuring that an output file name does
!  not already exist in this history.  If there are duplicate names, the MODEL
!  option will not work.  Call manhist_check_name to see if the output names
!  exist in the history.  If it does, abort the job and print a message that
!  the user must choose another name.
!
!  Tape histories are not tied to trcio, and read and write with their own
!  routines.  TTRIN and TTROT are examples of tape io.  Histories are written
!  to tape as ascii information.  One job is equivalent to one history record.
!  The last history and the data are separated by an EOF mark.
!
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS   
!
!
!    History options exist in job_data, trin, ttrin, trot, and ttrot.
!    The job data options refer to print out of the history only.
!    The trot, ttrot options refer to what history will get written to 
!    to the specified media.  
!    The options are MODEL,ALL,NONE, and CURRENT. The MODEL option will
!    keep a set of histories that models processing.
!    The trin, ttrin options are either yes or no, to initially keep or get
!    rid of the history.  It should be set to yes unless there are some
!    history problems.          
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author         Description
!     ----        ------         -----------
! 19. 2007-02-15  Bill Menger    Modified the message "JOB ABORT" from putcard
!                                and putrecord to say "JOB WARNING" so that in
!                                SeisSpace it won't say "abort" when it doesn't.
!                                (Modified trcio to not abort on this kind of 
!                                error also.)
! 18. 2006-01-03  Goodger        Abort job in routine manhist_putcard if the
!                                knthist variable is out of range.  This
!                                can happen if a history record is bad, and
!                                the header record is not readable.
! 17. 2005-06-22  Stoeckley      Initialize CURRENTJOB and USERNAME in case
!                                they are not in the parameter cache; add
!                                the routine MANHIST_INITIALIZE_FROM_C to
!                                facilitate this initialization from C.  These
!                                changes are needed for use from SeisSpace
!                                and potentially other places outside of CPS.
! 16. 2004-12-15  Goodger        Change in a print statement to make the
!                                the Absoft 9.0 compiler happy.
! 15. 2004-03-18  Goodger        Fix copy overrun message which had an
!                                erroneous calculation on the number of bytes
!                                in the array holding the history record.
! 14. 2004-01-08  R. Selzler     Replaced intrinsic transfer function with cmem.
! 13. 2003-11-20  Karen Goodger  Use swap_module rather than declare routine as
!                                external.
! 12. 2002-10-07  Karen Goodger  Cleanup variables declared but never used.
!                                Nullify pointers.  Initialize nhcards to 
!                                zero in routine putcard.
! 11. 2002-09-20  Karen Goodger  Insure only portion of history up to a trot
!                                file is written to that file.  The entire
!                                job history was getting written to the first
!                                trot.
! 10. 2002-08-26  Karen Goodger  Parse new trin and trot formats.
!  9. 2002-02-04  Karen Goodger  Add trinsort as a valid input process.
!  8. 2001-12-10  Karen Goodger  Set model to false if looping problem in job-
!                                step table.
!                                Fixed problem with output table.  It was
!                                indexing into the sort table incorrectly.
!  7. 2001-11-01  Karen Goodger  Fix problem with two loop job when trot file
!                                created and read back in, in same job.
!  6. 2001-10-18  Karen Goodger  Do not call modeling routine when there are no
!                                histories.
!  5. 2001-08-31  Karen Goodger  In routine manhist_initialize set status to
!                                zero.
!  4. 2001-08-29  Karen Goodger  In routine get_info, insure that the modeling
!                                has been done if requested.  This fixes a
!                                bug when the model option is seleced in TTROT.
!  3. 2001-08-27  Karen Goodger  Change linux to Linux to satisfy prodlib 
!                                history routine.
!  2. 2001-08-07  Karen Goodger  Temporarily make 4 header cards so files can
!                                be read by prodlib history module.
!                                Delete history records file before open to aid
!                                interactive developers.
!                                Insure history file is initialized only once.
!  1. 2001-08-03  Karen Goodger  Initial version.
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



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module manhist_module
      use array_module
      use cmem_module
      use cio_module
      use getlun_module
      use getsys_module
      use hist_module
      use histprim_module
      use pc_module
      use string_module
      use swap_module
      use wrdc_module

      implicit none


      private
      public :: manhist_putrecord,manhist_putcard,manhist_phist,manhist_cio
      public :: manhist_get_info,manhist_get_table_info,manhist_initialize
      public :: manhist_finished_reading,manhist_gethistory,manhist_check_name
      public :: manhist_track,manhist_histsused,manhist_gethistoryrec

      character(len=100),public,save :: MANHIST_IDENT = &
'$Id: manhist.f90,v 1.19 2007/02/16 14:00:19 Menger beta sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!

      type sortinfo
        integer :: recnum,ncards,nseconds,nheadercards
        character(len=24) :: jobname
        character(len=4)  :: version
      end type sortinfo



!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      type(sortinfo),pointer :: sorttable(:)

      character(len=80),pointer :: record(:)
      character(len=80),pointer :: output_names(:)

      character(len=80) :: beginning(3)

      character(len=24) :: currentjob

      integer,save :: lun,knthist=0,maxcards,maxinput=100,maxhist
      integer,save :: nused,oldmaxcards,oldmaxhist,stdo,currentseconds

      real,save :: tstrt,dt
      integer,save :: ndpt,nwih


      integer,pointer :: histused(:),jobstep_table(:,:),njst(:)
      integer,pointer :: noutputs(:),outputs(:,:)

      logical,save :: bigendian
      logical,save :: idiag=.true.,ignore=.false.,init=.false.
      logical,save :: model=.true.,newinput=.false.,tracked=.false.



      contains



!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!  manhist_check_name  !!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_check_name(name,status)

      character(len=*),intent(in) :: name

      integer,intent(out) :: status

      integer :: i

      status=0
      do i=1,knthist
        if(adjustl(output_names(i)).eq.name)then
          status=-2
          exit
        endif
      enddo

      end subroutine manhist_check_name

      subroutine manhist_cio(unit,opt,process_number_arg)

!        Output history to a cio file

      integer,intent(in) :: unit
      integer,optional   :: process_number_arg
      
      character(len=*) :: opt

      integer :: i,ipn,istat,i2,j,k,krec,nc,nh,nhist,num,numcurrent
      integer :: process_number

      character(len=80) :: card

      if(.not.init)then
        call manhist_initialize(istat)
        if(istat.ne.0)then
          write(stdo,*)'MANHIST_cio: Unable to initialize history'
        endif
      endif  
      if(present(process_number_arg))then
        process_number=process_number_arg
      else
        process_number=0
      endif
      if(opt.eq.'MODEL')call manhist_track
      nhist=knthist
      if(.not.model.and.opt.eq.'MODEL')then
        opt='ALL'
      endif
      if(opt.eq.'MODEL')nhist=nused
      if(opt.eq.'CURRENT')go to 100
      DO i=1,nhist
        if(opt.eq.'MODEL')then
          krec=histused(i)
          do i2=1,knthist
            if(krec.eq.sorttable(i2)%recnum)then
              nc=sorttable(i2)%ncards
              nh=sorttable(i2)%nheadercards
              exit
            endif
          enddo
        else
          krec=sorttable(i)%recnum
          nc=sorttable(i)%ncards
          nh=sorttable(i)%nheadercards
        endif
        call manhist_gethistoryrec(krec,record)
        do j=1,nc+nh
          k=cio_fputline(record(j),80,unit)
        enddo
      ENDDO
 100  continue

!         header card
      numcurrent=hist_numcards()
      card=' '
!           C        1         2         3         4         5         6   
!           C2345678901234567890123456789012345678901234567890123456789012345
      card='AS_HIST  NCARDS=     NWIH=   TAPE=--NO-VOL  NDPT=       TINC= '
      call string_ii2cc(numcurrent,card(17:20))
      call string_ii2cc(nwih,card(27:28))
      call string_ii2cc(ndpt,card(50:55))
      call string_ff2cc(dt,card(62:66))
      k=cio_fputline(card,80,unit)
!         Add 3 blank cards so old history can read this file
      card=' '
      do i=1,3
        k=cio_fputline(card,80,unit)
      enddo

!        beginning cards next
      do i=1,3
        k=cio_fputline(beginning(i),80,unit)
      enddo

!        current history cardset
      ipn=hist_maxipn()
      if(process_number.gt.0)ipn=process_number
      do i=1,ipn
        k=hist_read(i,num,record)
          do j=1,num
            k=cio_fputline(record(j),80,unit)
          enddo
      enddo


      end subroutine manhist_cio

!!!!!!!!!!!!!!!!!!!!!!!  manhist_finished_reading  !!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_finished_reading

!        This routine called by tape read so manhist will know when to sort

      call manhist_sort(knthist)

      end subroutine manhist_finished_reading

!!!!!!!!!!!!!!!!!!!!!!!!!!!! manhist_init !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_init(istat,tstrt_opt,dt_opt,ndpt_opt)


      integer,intent(out) :: istat
      real,optional :: tstrt_opt,dt_opt
      integer,optional :: ndpt_opt
      integer :: i
      integer(kind=8) :: length8
    

      real :: time,tstrt

      character(len=8) :: ctemp,username

      nullify(sorttable)
      nullify(record)
      nullify(output_names)
      nullify(histused)
      nullify(jobstep_table)
      nullify(njst)
      nullify(noutputs)
      nullify(outputs)

      currentjob = 'unspecified'
      call getsys_username (username)

!          start with a maximum history size and number of histories
!            will reallocate if larger
        if(init)return
        maxhist=2000
        maxcards=2000
        oldmaxcards=maxcards
        oldmaxhist=maxhist

      i=swap_endian()
      if(i.eq.1)then
        bigendian=.true.
      else
        bigendian=.false.
      endif

      stdo=pc_get_lun()
      call pc_get_jdata('JOBNAME',currentjob)
      call pc_get_pdata('USER_NAME',username)
      allocate(sorttable(maxhist),stat=istat)
      if(istat.ne.0)then
        write(stdo,*)'MANHIST: Unable to allocate memory for history record'
        return
      endif

      call array_alloc(record,maxcards)
      record=' '

!          Open history direct access file
      call getlun(lun)
      inquire(iolength=length8)record
      open(lun,access='direct',recl=length8,status='replace',iostat=istat,&
           file='%HISTORY_RECORDS')
      if(istat.ne.0)then
        write(stdo,*)'MANHIST: Unable to open file %HISTORY_RECORDS'
        return
      endif
      
!          Create the 1st 3 print cards of the current history
      beginning(1)=' '
      beginning(2)=' '
      beginning(3)=' '
      call string_date(beginning(1)(9:))
      beginning(1)(13:13)='/'
      beginning(1)(16:16)='/'
      call string_time(beginning(1)(27:))
      currentseconds=manhist_seconds(beginning(1))
      i=getsys_ostype()
      select case(i)
        case(GETSYS_LINUX)
          ctemp='Linux'
        case(GETSYS_SOLARIS)
          ctemp='solaris'
        case(GETSYS_HP)
          ctemp='hp'
        case(GETSYS_SGI)
          ctemp='sgi'
        case default
          ctemp='unknown'
      end select
      beginning(1)(50:)=ctemp
      call getsys_hostname(beginning(1)(63:))
      beginning(1)(45:49)='MACH='
      beginning(1)(58:62)='HOST='

      beginning(2)='JOB=' // trim(currentjob) 
      beginning(2)(21:)='USER=' // trim(username)
      beginning(2)(34:)='     VERSION=2.0'

      if(present(tstrt_opt))then
        tstrt=tstrt_opt
        dt=dt_opt
        ndpt=ndpt_opt
      else
        call pc_get_global('TSTRT',tstrt)
        call pc_get_global('DT',dt)
        call pc_get_global('NDPT',ndpt)
        call pc_get_global('NWIH',nwih)
      endif
      time=real(ndpt)*dt-dt
      write(beginning(3),9001)time,dt,tstrt





      init=.true.

 9001 format(' TIME=',f4.1,' DT = ',f10.5,' TSTRT=',f4.1)

      end subroutine manhist_init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!! manhist_initialize !!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_initialize(istat)

!           This routine will need to be called from calling routine before 
!            trace processing, if putcard or putrecord is not called until
!            after trace processing.

      integer, intent(inout) :: istat

      istat=0
      if(init)return
      call pc_get_global('TSTRT',tstrt)
      call pc_get_global('DT',dt)
      call pc_get_global('NDPT',ndpt)

      call manhist_init(istat,tstrt,dt,ndpt)
      init=.true.

      end subroutine manhist_initialize

!!!!!!!!!!!!!!!!!!!!!!!!!!!! manhist_getanswer !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_getanswer(card,parm,ans)

!        Looks for the name parm in card and returns the information 
!         following parm between the equal sign and 1st blank following
!         non-blanks

!        card = history card
!        parm = parameter to look for on card
!        ans  = answer returned

      character(len=*),intent(in)  :: card,parm
      character(len=*),intent(out) :: ans

      integer :: i,i1,i2,icol,j,k,lenans,n

      ans=' '
      lenans=len(ans)
      n=len_trim(parm)
      k=index(card,parm(1:n))
      if(k.eq.0)return
!
      icol = k 
      icol = index(card(icol+1:),'=') + icol 
      i1 = icol + 1 
      j=0
      
!         find 1st nonblank
      do i=i1,len(card)
        if(card(i:i).ne.' ')then
          i1=i
          j=i+1
          exit
        endif
      enddo
      if(j.eq.0)return
!        find terminating blank
      i2=0
      do i=j,len(card)
        if(card(i:i).eq.' ')then
          i2=i-1
          exit    
        endif
      enddo
      if(i2.eq.0)i2=len(card)

      if((i2-i1+1).gt.lenans)i2=i1+lenans-1
      ans=card(i1:i2)

      end subroutine manhist_getanswer

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! manhist_gethistory !!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_gethistory(rec,buf,ncards,nheader,opt)

      integer, intent(in) :: rec

      character(len=*) ,intent(in)    :: opt
      character(len=80),intent(inout) :: buf(maxcards)

      integer,intent(out) :: ncards,nheader

      integer :: histnum,i

      if(opt.eq.'MODEL')then
        histnum=histused(rec)
        do i=1,knthist
          if(sorttable(i)%recnum.eq.histnum)then
             ncards=sorttable(i)%ncards
             nheader=sorttable(i)%nheadercards
          endif
        enddo
      else
        histnum=sorttable(rec)%recnum
        ncards=sorttable(rec)%ncards
        nheader=sorttable(rec)%nheadercards
      endif
      call manhist_gethistoryrec(histnum,buf)

      end subroutine manhist_gethistory

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! manhist_gethistoryrec !!!!!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_gethistoryrec(rec,buf)

      integer,intent(in) :: rec

      character(len=80),intent(inout) :: buf(maxcards)

      read(lun,rec=rec)buf

      end subroutine manhist_gethistoryrec

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! manhist_get_info !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_get_info(knthist_arg,maxcards_arg,opt_arg)

      integer,intent(out),optional :: knthist_arg,maxcards_arg

      character(len=*),intent(in) ,optional :: opt_arg

      character(len=8) :: opt

   
      if(present(opt_arg))then
         opt=opt_arg
      else
         opt='ALL'
      endif
      if(opt.eq.'MODEL'.and.nused.eq.0.and.model)then
        call manhist_track
      endif
      if(present(knthist_arg))then
        if(opt.eq.'MODEL')then
          knthist_arg=nused
        else
          knthist_arg=knthist
        endif
      endif
      if(present(maxcards_arg))maxcards_arg=maxcards

      end subroutine manhist_get_info

!!!!!!!!!!!!!!!!!!!!!!! manhist_get_table_info !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_get_table_info(irec,istat,version,numbercards,seconds)

      integer,intent(in)  :: irec
      integer,intent(out) :: istat
      integer,optional,intent(out) :: numbercards,seconds
      character(len=4),optional,intent(out) :: version

      integer :: ndx

      istat=0
      if(irec.le.0.or.irec.gt.knthist)then
        istat=-1
        return
      endif
!          Find the index of irec in the table
      do ndx=1,knthist
        if(sorttable(ndx)%recnum.eq.irec)exit
      enddo
      if(present(version))version=sorttable(ndx)%version
      if(present(numbercards))numbercards=sorttable(ndx)%ncards
      if(present(seconds))seconds=sorttable(ndx)%nseconds

      end subroutine manhist_get_table_info

!!!!!!!!!!!!!!!!!!!!!!!!!!! manhist_histused !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_histsused(buf)

!           Returns the histories to be used in array buf

      integer,intent(inout) :: buf(*)

      integer :: i

      if(.not.tracked)call manhist_track
      if(model)then
        do i=1,nused
          buf(i)=histused(i)
        enddo
      else
        do i=1,knthist
          buf(i)=i
        enddo
      endif
      end subroutine manhist_histsused


!!!!!!!!!!!!!!!!!!!!!!!!!!!! manhist_io !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_io(status)

!          Get the inputs for each history

      integer,intent(out) :: status
      
      integer,pointer :: ninputs(:),inputs(:,:)

      character(len=4)  :: version
      character(len=32) :: process_name
      character(len=80) :: ioname
      integer         :: histnum,i,j,k,l,l2,ncards,nc,ndx
      logical,save ::first=.true.

      if(first)then
        nullify(ninputs)
        nullify(inputs)
        first=.false.
      endif
      status=0
      call array_free(ninputs)
      call array_free(inputs)

      call array_alloc(ninputs,knthist)
      call array_alloc(inputs,knthist,maxinput)

      ninputs=0
      inputs=0

!              Find the inputs based on output name table
      DO i=1,knthist
        histnum=sorttable(i)%recnum
        call manhist_gethistoryrec(histnum,record)
        call manhist_get_table_info(histnum,status,numbercards=ncards,&
                                    version=version)
        DO j=1,ncards
          if(version.ne.'0.0')then
            k=index(record(j),'*HISTORY IPN:')
            if(k.eq.0)cycle
            call histprim_process_name(record(j),process_name)
            select case(process_name)
              case('TRIN')
                call histprim_gans('TRIN_INPUT_FILES',ioname,record,j+1,ncards)
                k=index(ioname,'(')
                if(k.ne.0)ioname(k:k)=' '
                k=index(ioname,')')
                if(k.ne.0)ioname(k:k)=' '
                nc=len_trim(ioname)
                if(ioname(nc:nc).eq.',')ioname(nc:nc)=' '
                ioname=adjustl(ioname)
                if(ioname.eq.' ')then
                  call histprim_gans('PATHNAMES',ioname,record,j+1,ncards)
                  k=index(ioname,'(')
                  if(k.ne.0)ioname(k:k)=' '
                  k=index(ioname,')')
                  if(k.ne.0)ioname(k:k)=' '
                  ioname=adjustl(ioname)
                endif
                if(ioname.eq.' ')then
                  call histprim_gans('PATHNAMES',ioname,record,j+1,ncards)
                endif
                if(ioname.eq.' ')then
                  call histprim_gans('PATHNAME',ioname,record,j+1,ncards)
                endif
              case('TRINSORT')
                call histprim_gans('PATHNAME',ioname,record,j+1,ncards)
              case('TTRIN')
                call histprim_gans('VOLUME',ioname,record,j+1,ncards)
                k=index(ioname,'(')
                if(k.ne.0)ioname(k:k)=' '
                k=index(ioname,')')
                if(k.ne.0)ioname(k:k)=' '
                ioname=adjustl(ioname)
              case default
                cycle
            end select
          else  ! cray
            if(record(j)(1:1).eq.' ')cycle
            process_name=record(j)(1:4)
            select case(process_name)
              case('STRI')
                call histprim_gans('FNAME',ioname,record,j+1,ncards)
              case('TTRI')
                l2=0
                do l=j+1,ncards
                  k=index(record(l),'PDN')
                  if(k.ne.0)then
                    l2=l+1
                    exit
                  endif
                enddo
                if(l2.eq.0)cycle
                ioname=record(l2)(2:7)           
              case('PAST')
                l2=0
                do l=J+2,ncards
                  k=index(record(l),'OPTION') ! Can stop when hit option card
                  if(k.ne.0)exit
                  call histprim_gsprm(7,record(l),ioname,nc)
                  ndx=manhist_update_name(ioname,.false.)
                  ninputs(histnum)=ninputs(histnum)+1
                  inputs(histnum,ninputs(histnum))=ndx
                  ioname=' '
                enddo
                exit
              case default
                cycle
            end select
          endif
          ndx=manhist_update_name(ioname,.false.)
          ninputs(histnum)=ninputs(histnum)+1
          inputs(histnum,ninputs(histnum))=ndx
          exit
        ENDDO
      ENDDO

      if(idiag)then
        write(stdo,*)' '
        write(stdo,*)' INPUTS...'
        do i=1,knthist
          write(stdo,*)i,ninputs(i),(inputs(i,j),j=1,20)
        enddo
        write(stdo,*)' '
        write(stdo,*)' OUTPUTS...'
        do i=1,knthist
          write(stdo,*)i,noutputs(i),(outputs(i,j),j=1,20)
        enddo
      endif

      if(model)call manhist_jst(ninputs,inputs)

      end subroutine manhist_io

!!!!!!!!!!!!!!!!!!!!!!!!!!!! manhist_jst  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine manhist_jst(ninputs,inputs)
!
!    routine to set up a job step table of pointers linking histories

      integer, intent(in) :: ninputs(*),inputs(:,:)

      integer :: i,ii,j,k,l,m
      integer :: irow,isav,istart,nxt

      integer,pointer :: keys(:)
      logical,save    :: first=.true.
!
!
!
!    generate job step table - determine linkage between histories
!
      irow=1
!
!
!          for each output reel, looks at all the input reels in each
!            history to find links.
!
!          nhist  =  total number of history files
!          noutputs =  array containing number of output reels per history
!          ninputs =  array containing number of input reels per history
!          reel   =  array containing input reels
!          outputs  =  array containing output reels
!          njst   =  array containing number of entries in each row of
!                     the job-step table
!          j      =  counter for number output reels per history
!          l      =  counter for number of input reels per history
!          k      =  history number
!          i      =  counter for number of histories
!
!          in the job step table--array jobstep_table
!
!             row  =  history file inputs from a certain output
!                      (up to 100)
!
      if(first)then
        nullify(keys)
        first=.false.
      endif
      call array_free(jobstep_table)
      call array_free(keys)
      call array_free(njst)
      call array_free(histused)
      call array_alloc(jobstep_table,maxinput,knthist)
      call array_alloc(keys,knthist)
      call array_alloc(njst,knthist)
      call array_alloc(histused,knthist)

!           Keys not necessarily sequential in old atb as some histories
!            were discarded
      do i=1,knthist
        keys(i)=i
      enddo
      njst=0

      do 5001 i=1,(knthist-1)
        irow=1
        do 5002 j=1,noutputs(i)
          do 5003 k=1,knthist
            do l=1,ninputs(k)
!
              if(outputs(i,j).eq.inputs(k,l))then
!
                do m=1,(irow-1)
                  if(jobstep_table(m,i).eq.keys(k))go to 5002
                enddo
!
                jobstep_table(irow,i)=keys(k)
                njst(i)=irow
                irow=irow+1
                if(irow.gt.maxinput)go to 5001
              endif
!
            enddo
5003      continue
5002    continue
5001  continue
!
      jobstep_table(1,knthist)=-9999
      njst(knthist)=1
      if(idiag)then
        write(stdo,*)' JOB-STEP TABLE'
        do i=1,knthist
          write(stdo,9001)njst(i),keys(i),(jobstep_table(j,i),j=1,njst(i))
        enddo
      endif

!          From the jobstep_table - determine a path to the current history
      istart=0
      do i=1,knthist
        if(njst(i).ne.0)then
          istart=i
          exit
        endif
      enddo
      if(istart.eq.0)then
        write(stdo,*)'MANHIST_JST: Unable to determine path to current history'
        model=.false.
        return
      endif
      j=1
      isav=0
      histused(j)=keys(istart)
 70   i=istart
  75  if(njst(i).eq.0)then
        if(isav.gt.0)then
          i=isav
          nxt=jobstep_table(njst(i),i)
          do l=1,knthist
            if(histused(l).eq.nxt)then
              j=l-1
            endif
          enddo
          isav=0
          njst(i)=njst(i)-1
          go to 79
        endif
        istart=istart+1
        j=1
        histused(1)=keys(istart)
        if(istart.eq.knthist)go to 80
        go to 70
      endif
 79   nxt=jobstep_table(njst(i),i)
!        If nxt has already been used there is a problem
      do ii=1,j
        if(histused(ii).eq.nxt)then
          write(stdo,*)'MANHIST_jst - Problem with job-step table'
          write(stdo,*)'              Unable to model'
          model=.false.
          return
        endif
      enddo
      if(njst(i).gt.1)isav=i
      if(nxt.eq.-9999)go to 80
!          find index of nxt history
      do 77 l=1,knthist
        if(keys(l).eq.nxt)then
          i=l
          go to 78
        endif
 77   continue
      write(stdo,*)' MANHIST_JST STATEMENT 77 - PROBLEM FINDING HISTORY TO&
                   & START'
      model=.false.
      return 
 78   j=j+1
      if(j.gt.knthist)go to 80
      histused(j)=keys(i)
      if(i.ge.knthist)go to 80
      go to 75
!
 80   continue
      nused=j
!! 82   continue

      write(stdo,*)'HISTORIES USED ARE...'
      do i=1,nused
        write(stdo,*)histused(i)
      enddo

      return
 9001 format(12i5)
      end subroutine manhist_jst


!!!!!!!!!!!!!!!!!!!!!!!!!!!! manhist_nametable !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_nametable(status)

!          Get the outputs for each history and builds an output name table

      
      integer,intent(out) :: status

      character(len=4)  :: version
      character(len=32) :: process_name
      character(len=80) :: ioname
      integer         :: histnum,i,j,k,ncards,nc,ndx
      logical         :: validout

      call array_free(noutputs)
      call array_free(outputs)
      call array_free(output_names)

      call array_alloc(noutputs,knthist)
      call array_alloc(outputs,knthist,maxinput)
      call array_alloc(output_names,knthist)


      noutputs=0
      outputs=0
      output_names=' '

      DO i=1,knthist
        histnum=sorttable(i)%recnum
        call manhist_gethistoryrec(histnum,record)
        call manhist_get_table_info(histnum,status,numbercards=ncards,&
                                    version=version)
        if(version.eq.'0.0')ncards=ncards+4
!             Find the outputs and build a name table
        DO j=ncards,1,-1
          if(version.ne.'0.0')then
            k=index(record(j),'*HISTORY IPN:')
            if(k.eq.0)cycle
            call histprim_process_name(record(j),process_name)
            validout=.false.
            select case(process_name)
              case('TROT')
                call histprim_gans('TROT_OUTPUT_FILES',ioname,record,j+1,ncards)
                k=index(ioname,'(')
                if(k.ne.0)ioname(k:k)=' '
                k=index(ioname,')')
                if(k.ne.0)ioname(k:k)=' '
                ioname=adjustl(ioname)
                nc=len_trim(ioname)
                if(ioname(nc:nc).eq.',')ioname(nc:nc)=' '
                if(ioname.eq.' ')then
                  call histprim_gans('PATHNAMES',ioname,record,j+1,ncards)
                  k=index(ioname,'(')
                  if(k.ne.0)ioname(k:k)=' '
                  k=index(ioname,')')
                  if(k.ne.0)ioname(k:k)=' '
                  ioname=adjustl(ioname)
                endif
                if(ioname.eq.' ')then
                  call histprim_gans('PATHNAME',ioname,record,j+1,ncards)
                endif
                validout=.true.
              case('TTROT')
                call histprim_gans('VOLUME',ioname,record,j+1,ncards)
                validout=.true.
            end select
          else !  cray format
            if(record(j)(1:1).eq.' ')cycle
            process_name=record(j)(1:4)
            validout=.false.
            select case(process_name)
              case('STRO')
                call histprim_gans('FNAME',ioname,record,j+1,ncards)
                validout=.true.     
              case('TTRO')           
                call histprim_gans(' REEL',ioname,record,j+1,ncards)
                validout=.true.     
            end select
          endif 
          if(validout)then
            ndx=manhist_update_name(ioname,.true.)
            noutputs(histnum)=noutputs(histnum)+1
            outputs(histnum,noutputs(histnum))=ndx
            exit
          endif
        ENDDO
        if(.not.validout)then
          write(stdo,*)'MANHIST_nametable - Valid output not found in history '&
                        ,histnum
          model=.false.
        endif
      ENDDO

      write(stdo,*)' Outputs are ...'
      do i=1,knthist
        write(stdo,*)i,output_names(i)
      enddo

      end subroutine manhist_nametable
!!!!!!!!!!!!!!!!!!!!!!!!!!!! manhist_phist !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_phist(opt)

!         print the histories to the report file

      character(len=*),intent(inout) :: opt

      character(len=80) :: asterisks

      data asterisks/'****************************************&
                     &****************************************'/

      integer :: i,i2,istat,j,krec,nc,nh,nhist

      character(len=24) :: jname


      if(opt.eq.'MODEL')call manhist_track
      if(.not.model.and.opt.eq.'MODEL')opt='ALL'
      if(opt.eq.'CURRENT')go to 100

      write(stdo,*)asterisks
      write(stdo,*)'Previous history summary'
      nhist=knthist
      if(opt.eq.'MODEL')nhist=nused
      DO i=1,nhist
        if(opt.eq.'MODEL')then
          krec=histused(i)
          do i2=1,knthist
            if(krec.eq.sorttable(i2)%recnum)then
              nc=sorttable(i2)%ncards
              nh=sorttable(i2)%nheadercards
              jname=sorttable(i2)%jobname
              exit
            endif
          enddo
        else
          krec=sorttable(i)%recnum
          nc=sorttable(i)%ncards
          nh=sorttable(i)%nheadercards
          jname=sorttable(i)%jobname
        endif

        write(stdo,*)'History no. ',krec,', name=',jname
        call manhist_gethistoryrec(krec,record)
        do j=nh+1,nc + nh
          write(stdo,9001)record(j)
        enddo
        write(stdo,*)' '
        write(stdo,*)' '
      ENDDO
       
 100  continue

!           Print the current history
      write(stdo,*)'Current history record'
      do i=1,3
        write(stdo,9001)beginning(i)
      enddo
      istat=hist_print(jobname=currentjob)
    
 

 9001 format('  ****    ',A,'  ****')    

      end subroutine manhist_phist


      subroutine manhist_putcard(card,istat)

      character(len=80) :: card
      character(len=24) :: cans
      character(len=4)  :: version
      integer :: i,istat,k
      integer :: recreplace=0,recreplaceNDX=0
      integer :: totseconds

      integer,save :: kntcards,ndx,nhcards=0,prehcards

      if(.not.init)then
        call manhist_init(istat)
        if(istat.ne.0)return
      endif

!              Buffer up cards into history records, make tables, and write
!               to file
      if(card(1:11).eq.'#</history>')then
        if(ignore)then
          ignore=.false.
          go to 200
        endif
!
!         write out last history record and sort
        k=knthist
        ndx=k
        if(recreplace.gt.0)then
          k=recreplace
          ndx=recreplaceNDX
          recreplace=0
        endif
        if(knthist.gt.maxhist)then
          oldmaxhist=maxhist
          maxhist=maxhist*2
          write(stdo,*)'MANHIST: Maximum histories reset to ',maxhist
          call manhist_realloc(istat)
          if(istat.ne.0)return
        endif

!          Abort job if this job name has already been used
        do i=1,knthist
          if(currentjob.eq.sorttable(i)%jobname)then
!             If time and date are the same - its another loop in the same job
            if(sorttable(i)%nseconds.eq.currentseconds)then
               knthist=knthist-1
               go to 220
            endif
            write(stdo,*)' '
            write(stdo,*)'+++++++++++ job name problem ++++++++++++++'
            write(stdo,*)' Job name ',trim(currentjob),' already exists in &
                         &the history'
            write(stdo,*)' You should choose another job name'
            write(stdo,*)' MANHIST_PUTCARD : JOB WARNING'
            istat=-2
          endif
        enddo
        write(lun,rec=k)record
        record=' '
        sorttable(ndx)%ncards=kntcards-nhcards
 200    continue
        call manhist_sort(knthist)

 220    continue
!         Set flag for next trin/ttrin
        newinput=.true.
        return
      endif
      if(ignore)return
      if(card(1:7).eq.'AS_HIST')then
        ignore=.false.
        prehcards=nhcards
        nhcards=4
        if(card(8:8).eq.'2')nhcards=1
!         write out previous history record   
        if(knthist.gt.0.and..not.newinput)then
          k=knthist
          ndx=k
          if(recreplace.gt.0)then
            k=recreplace
            ndx=recreplaceNDX
            recreplace=0
          endif          
          sorttable(ndx)%ncards=kntcards-prehcards
          write(lun,rec=k)record
          record=' '
        endif  !  knthist.gt.0
        kntcards=0
        newinput=.false.

!           Begin new history record
        call manhist_getanswer(card,'NCARDS',cans)
        knthist=knthist+1
        if(knthist.gt.maxhist)then
          oldmaxhist=maxhist
          maxhist=maxhist*2
          write(stdo,*)'MANHIST: Maximum histories reset to ',maxhist
          call manhist_realloc(istat)
          if(istat.ne.0)return
        endif
        call string_cc2ii(cans,sorttable(knthist)%ncards)
        sorttable(knthist)%recnum=knthist
        sorttable(knthist)%nheadercards=nhcards
      endif  ! card(1:7).eq.'AS_HIST'
      kntcards=kntcards+1
      if(kntcards.gt.maxcards)then
        oldmaxcards=maxcards
        maxcards=maxcards*2
        write(stdo,*)' MANHIST_PUTCARD: Maxcards reset to ',maxcards
        call manhist_realloc(istat)
        if(istat.ne.0)return
      endif
      record(kntcards)=card
      if(kntcards.eq.(nhcards+1))then
!          Get the date and time and convert to seconds
        totseconds=manhist_seconds(card)
        if(knthist.le.0.or.knthist.gt.maxhist)then
          print*,'manhist_putcard ABORT-A'
          print*,' knthist is out of range'
          print*,' knthist = ',knthist,' maxhist = ',maxhist
          istat=-2
          return
        endif
        sorttable(knthist)%nseconds=totseconds
      endif
      if(kntcards.eq.(nhcards+2))then
!          Get the job name
        call manhist_getanswer(card,'JOB',cans)
        call manhist_getanswer(card,'VERSION',version)
        if(version.eq.' ')version='0.0'
!          Determine if job name already exists
        recreplace=0
        do i=1,knthist-1
          if(sorttable(i)%jobname.eq.cans)then
!              Keep the most recent
            if(totseconds.le.sorttable(i)%nseconds)then
              ignore=.true.
              knthist=knthist-1
              go to 500
            endif
            recreplace=sorttable(i)%recnum
            recreplaceNDX=i
            knthist=knthist-1
            go to 500
          endif
        enddo
        sorttable(knthist)%jobname=cans
        sorttable(knthist)%version=version
      endif

 500  continue

      end subroutine manhist_putcard

!!!!!!!!!!!!!!!!!!!!!!!!!!!!! manhist_putrecord !!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_putrecord(bufin,lcards,istat,cray,nwihcray,ndptcray)

!          puts a history record from tape to the history file


      real,intent(in)     :: bufin(:)    ! buffer containing history
      integer,intent(in)  :: lcards      ! num cards in largest history
      integer,intent(out) :: istat       ! status to return to calling routine
      logical,intent(in)  :: cray        ! cray flag, true or false
      integer,intent(out) :: nwihcray    ! number of header words 
      integer,intent(out) :: ndptcray    ! number of sample points
      integer             :: temp

      integer :: i,k,ncards,ndx,nhcards,totseconds
      real    :: oldcrayheader(71)

      character(len=4)  :: version
      character(len=24) :: cans
      character(len=80) :: card

      istat=0

      if(.not.init)then
        call manhist_init(istat)
        if(istat.ne.0)return
      endif

      record=' '
      !before 8 Dec 2003 (R. Selzler)
      !record=transfer(bufin,(/'                                        &
      !                        &                                        '/))
      !after 8 Dec 2003
      if(80 * size(record) < sizeof(bufin(1)) * size(bufin)) then
          print *, "DEBUG: manhist_putrecord: copy overrun",  &
               size(record), sizeof(bufin(1)), size(bufin)
      endif
      temp = sizeof(bufin(1))*size(bufin)
      call cmem_cpy(record, bufin, temp)
      if(record(1)(1:3).eq.'PR=')return
      if(lcards.gt.maxcards)then
        maxcards=lcards
        call manhist_realloc(istat)
        if(istat.ne.0)return
      endif


      if(cray)then
!            The first 4 cards are ibm words
!            word5  = number of sample points in data
!            word6  = dt
!            word7  = tstrt
!            word54 = number of cards in the history
!            word71 = num words in header
        call wrdc_ibm_to_float(bufin,oldcrayheader,71,bigendian)
        nwihcray=nint(oldcrayheader(71))
        ndptcray=nint(oldcrayheader(5))
        ncards=nint(oldcrayheader(54))
        call wrdc_ebc_asc(record)
        write(record(1),'(A,I3,A,I2,A,I5)')'AS_HIST NCARDS=',ncards,' NWIH=',&
                                            nwihcray,' NDPT=',ndptcray
        record(2)=' '
        record(3)=' '
        record(4)=' '
                                   
      else
        call manhist_getanswer(record(1),'NCARDS',cans)
        call string_cc2ii(cans,ncards)
      endif

      nhcards=4
      if(record(1)(1:8).eq.'AS_HIST2')then
        nhcards=1
      else
        nhcards=4
      endif

      if(ncards.gt.maxcards)then
        maxcards=ncards
        call manhist_realloc(istat)
        if(istat.ne.0)return
      endif

      knthist=knthist+1
      if(knthist.gt.maxhist)then
        oldmaxhist=maxhist
        maxhist=maxhist*2
        write(stdo,*)'MANHIST: Maximum histories reset to ',maxhist
        call manhist_realloc(istat)
        if(istat.ne.0)return
      endif

!         Get the date and time and convert to seconds
!         It is on the same card as MACH
      do i=1,maxcards
        k=index(record(i),'MACH')
        if(k.eq.0)cycle
        card=record(i)
        exit
      enddo
      totseconds=manhist_seconds(card)


!          Get the job name
      do i=1,maxcards
        k=index(record(i),'JOB')
        if(k.eq.0)cycle
        card=record(i)
        call manhist_getanswer(card,'JOB',cans)
        call manhist_getanswer(card,'VERSION',version)
        if(version.eq.' ')version='0.0'
        exit
      enddo


!          Determine if the job name already exists
      ndx=knthist
      do i=1,knthist-1
        if(sorttable(i)%jobname.eq.cans)then
!            Keep the most recent
          if(totseconds.gt.sorttable(i)%nseconds)then
            ndx=i
            knthist=knthist-1
          endif
          exit
        endif
      enddo

!          Abort job is this job name already exists in history
      if(currentjob.eq.cans)then
         write(stdo,*)' '
         write(stdo,*)'+++++++++++ job name problem ++++++++++++++'
         write(stdo,*)' Job name ',trim(currentjob),' already exists in &
                      &the history'
         write(stdo,*)' You should choose another job name'
         write(stdo,*)' MANHIST_PUTCARD : JOB WARNING'
         istat=-2
      endif

!          Update tables
      sorttable(ndx)%recnum=ndx
      sorttable(ndx)%ncards=ncards
      sorttable(ndx)%nseconds=totseconds
      sorttable(ndx)%nheadercards=nhcards
      sorttable(ndx)%jobname=cans
      sorttable(ndx)%version=version


      write(lun,rec=ndx)record
      
        


      end subroutine manhist_putrecord

!!!!!!!!!!!!!!!!!!!!!!!!!!! manhist_realloc !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_realloc(istat)


      integer,intent(out) :: istat

      type(sortinfo),pointer :: temptable(:)
      
      character(len=80) :: temprecord(maxcards),tempbuf(oldmaxcards)

      integer :: i,templun
      integer(kind=8) :: length
      logical,save :: first=.true.

      if(first)then
        nullify(temptable)
        first=.false.
      endif

      istat=0
      if(maxhist.gt.oldmaxhist)then
        allocate(temptable(maxhist),stat=istat)

        if(istat.ne.0)then
          write(stdo,*)'MANHIST_REALLOC: Unable to allocate memory for history &
                       &record'
          return
        endif
        do i=1,oldmaxhist
          temptable(i)=sorttable(i)
        enddo

        deallocate(sorttable)
        allocate(sorttable(maxhist),stat=istat)

        if(istat.ne.0)then
          write(stdo,*)'MANHIST_REALLOC: Unable to allocate memory for history &
                       &record'
          return
        endif

        sorttable=temptable
        deallocate(temptable)
      
      endif  ! maxhist.gt.oldmaxhist

      if(maxcards.gt.oldmaxcards)then
        temprecord=' '
        do i=1,oldmaxcards
          temprecord(i)=record(i)
        enddo
        call array_free(record)
        call array_alloc(record,maxcards)
        record=temprecord
      endif

!          Open history direct access file
      call getlun(templun)
      inquire(iolength=length)record
      open(templun,access='direct',recl=length,status='new',iostat=istat,&
           file='temp%HISTORY_RECORDS')
      if(istat.ne.0)then
        write(stdo,*)'MANHIST: Unable to open file temp%HISTORY_RECORDS'
        return
      endif

      do i=1,knthist-1
        read(lun,rec=i)tempbuf
        write(templun,rec=i)tempbuf
      enddo

      close(lun,status='delete')
      inquire(iolength=length)record
      open(lun,access='direct',recl=length,status='new',iostat=istat,&
           file='%HISTORY_RECORDS')
      if(istat.ne.0)then
        write(stdo,*)'MANHIST_realloc: Unable to open file %HISTORY_RECORDS'
        return
      endif

      do i=1,knthist-1
        read(templun,rec=i)temprecord
        write(lun,rec=i)temprecord
      enddo

      close(templun,status='delete')



      end subroutine manhist_realloc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  manhist_seconds  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      integer function manhist_seconds(card)
!!!      integer function manhist_seconds(jyr,jmo,jda,jho,jmi,jse)

!    Get date and time from card and compute number of seconds since 1980

      character(len=*),intent(in) :: card
      integer :: jyr,jmo,jda,jho,jmi,jse
      integer :: nlday,jday,i
      integer :: i1,i2,icol,k

        k=index(card,'/')
        i1=k-4
        i2=k-1
        call string_cc2ii(card(i1:i2),jyr)
        i1=k+1
        i2=k+2
        call string_cc2ii(card(i1:i2),jmo)
        i1=k+4
        i2=k+5
        call string_cc2ii(card(i1:i2),jda)
        icol=k+7
        k=index(card(icol:),'.') + icol-1
        i1=k-2
        i2=k-1
        call string_cc2ii(card(i1:i2),jho)
        i1=k+1
        i2=k+2
        call string_cc2ii(card(i1:i2),jmi)
        i1=k+4
        i2=k+5
        call string_cc2ii(card(i1:i2),jse)


        if (jyr.gt.1979) jyr = jyr-1980
        nlday = 1 + jyr/4
        jday = jda
        do i=1,jmo-1
          if      (i.eq.2) then
            jday = 28+jday       !month=Febuary.
          else if (i.eq.4.or.i.eq.6.or.i.eq.9.or.i.eq.11) then
            jday = 30+jday       !month=Sept., April, June, or Novem.
          else
            jday = 31+jday
          endif
        enddo
        manhist_seconds = nlday*86400 +     &   !# of seconds in leap day.
                  jyr  *31536000 +          &   !# of seconds in year.
                  jday *86400 +             &   !# of seconds in a day.
                  jho  *3600 +              &   !# of seconds in an hour.
                  jmi  *60   +              &   !# of seconds in a minute.
                  jse                           !# of seconds.

      end function manhist_seconds

      subroutine manhist_sort(total)

!        sorts the sort table based on time

      integer,intent(in) :: total

      integer :: key,i,j,status
      integer,pointer :: temp(:)
      type(sortinfo) :: keyndx
      logical,save :: first=.true.

      if(first)then
        nullify(temp)
        first=.false.
      endif
      

      call array_alloc(temp,total)
      do i=1,total
        temp(i)=sorttable(i)%nseconds
      enddo

      do j=2,total
        key=temp(j)
        keyndx=sorttable(j)
        i=j-1
        do
          if(i.gt.0 .and. temp(i) .gt. key)then
            temp(i+1)=temp(i)
            sorttable(i+1)=sorttable(i)
            i=i-1
          else
            exit
          endif
        enddo
        temp(i+1)=key
        sorttable(i+1)=keyndx
      enddo
      call array_free(temp)

      call manhist_nametable(status)
      tracked=.false.

      end subroutine manhist_sort

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! manhist_track !!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine manhist_track

!           Track through histories by inputs and outputs and generate
!          a table of histories to keep

      integer :: status
   
      if(knthist.eq.0)return
      if(.not.tracked)call manhist_io(status)
      tracked=.true.

      end subroutine manhist_track


!!!!!!!!!!!!!!!!!!  manhist_update_name  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      integer function manhist_update_name(name,add_to_table)

      character(len=*),intent(in) :: name
      logical,intent(in) :: add_to_table

      integer :: i

      manhist_update_name=0
      do i=1,knthist
        if(name.eq.output_names(i))then
           manhist_update_name=i
           return
        endif
        if(output_names(i).eq.' '.and.add_to_table)then
          output_names(i)=name
          manhist_update_name=i
          return
        endif
      enddo

      end function manhist_update_name




!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module manhist_module


!!------------------------ outside of module ------------------------------!!
!!------------------------ outside of module ------------------------------!!
!!------------------------ outside of module ------------------------------!!


      subroutine manhist_initialize_from_c
      use manhist_module
      implicit none
      integer :: istat
      logical,save :: STARTING = .true.

      if (STARTING) then
          call manhist_initialize (istat)
          STARTING = .false.
      endif

      end subroutine manhist_initialize_from_c


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

