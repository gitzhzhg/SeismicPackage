!<CPS_v1 type="PROGRAM"/>
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
!!------------------------------ permsave.f90 --------------------------------!!
!!------------------------------ permsave.f90 --------------------------------!!
!!------------------------------ permsave.f90 --------------------------------!!




!<brief_doc>
!-------------------------------------------------------------------------------
!                       C P S   P R O G R A M 
!
! Name       : PERMSAVE
! Category   : stand-alone
! Written    : 2001-02-21   by: Karen Goodger
! Revised    : 2001-02-21   by: Karen Goodger
! Maturity   : beta
! Purpose    : Save files names to be written to a permanent save tape.
! Portability: Unix operating system.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!  This program reads the output of an ls -R command and writes the path
!  names to a file called permsave.list.  This file will be read by the
!  ttrot process which will fetch the files and write them to tape.  The
!  program uses temporary files of permsave.tmp1, and permsave.home which
!  it deletes.
!  Permsave determines if a file is a text file by reading it and checking 
!  that all characters fall within the ascii character set.  
!  All other files are skipped.  The file permsave.skip lists files
!  which were rejected because they contain non-ascii characters.
!  Permsave also skips the following extensions...
!  job,trc,byt,sgy
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!  The user should cd to the directory where they wish to begin permanent
!  saving. The names of files in that directory as well as its subdirectories
!  will get written to permsave.list.  
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
!  1. 2001-02-21  Karen Goodger Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! This program is dependent on the unix operating system command ls.
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




!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!


      program permsave
      use getlun_module
      use putsys_module
      implicit none

      integer :: homelun,istat,i1,i2,j,k,knt,k1,k2,lsout,nc,ncworkdir
      integer :: plist,skplun,txtlun
      logical :: there1,there2,there3,there4

      character(len=120) :: card,cmd,outcard,fname,field1,tmpcard
      character(len=80)  :: workdir
      character(len=1)   :: ians

!  Check to see if permsave files already exist
      inquire(file='./permsave.list',exist=there1)
      inquire(file='./permsave.tmp1',exist=there2)
      inquire(file='./permsave.home',exist=there3)
      inquire(file='./permsave.skip',exist=there4)
      if(there1)then
        print*,' File permsave.list already exists in your account.'
      endif
      if(there2)then
        print*,' File permsave.tmp1 already exists in your account.'
      endif
      if(there3)then
        print*,' File permsave.home already exists in your account.'
      endif
      if(there4)then
        print*,' File permsave.skip already exists in your account.'
      endif
      if(there1.or.there2.or.there3.or.there4)then
 50     continue
        print*,' File names permsave.list, permsave.tmp1, permsave.home  '
        print*,' and permsave.skip are used by permsave.' 
        print*,' Enter Y to continue and permsave will rewrite these files'
        print*,' Enter N to stop permsave so you can rename these files'
        print*,' '
        read*,ians
        if(ians.eq.'N'.or.ians.eq.'n')stop
        if(ians.ne.'Y'.and.ians.ne.'y')go to 50
      endif
      knt=0
!          Get the root directory
      workdir=' '
      cmd='pwd > ./permsave.home'
      call putsys_cmd(cmd)
      call getlun(homelun,istat)
      if(istat.ne.0)then
        print*,' Unable to get unit number for permsave.home'
        stop
      endif
      open(homelun,file='./permsave.home',status='old',iostat=istat)
      if(istat.ne.0)then
        print*,' Error opening permsave.home'
        stop
      endif
      read(homelun,'(A)')workdir
      print*,' Working directory is ',trim(workdir)
      ncworkdir=len_trim(workdir)
      call putsys_cmd('ls -R -l > ./permsave.tmp1',istat)
      if(istat.ne.0)then
        print*,' ls -R failed'
        stop
      endif
      call getlun(skplun,istat)
      if(istat.ne.0)then
        print*,' Unalbe to get unit number for skip file'
        stop
      endif
      open(skplun,file='./permsave.skip',status='replace',iostat=istat)
      if(istat.ne.0)then
        print*,' Unable to open permsave.skip'
        stop
      endif
      call getlun(txtlun,istat)
      if(istat.ne.0)then
         print*,' Unable to get unit number for temporary file'
         stop
      endif
      call getlun(lsout,istat)
      if(istat.ne.0)then
        print*,' Unable to get a unit number for ls output'
        stop
      endif
      open(lsout,file='./permsave.tmp1',iostat=istat,status='old')
      if(istat.ne.0)then
        print*,' Open of permsave.tmp1 file failed'
        stop
      endif
      call getlun(plist,istat)
      if(istat.ne.0)then
        print*,'Unable to get unit number for permsave.list'
        stop
      endif
      open(plist,file='./permsave.list',iostat=istat,status='replace')
      if(istat.ne.0)then
        print*,' Open of permsave.list failed'
        stop
      endif

      print*,' '
      print*,' Permsave is determining whether files are binary.'
      print*,' This may take some time.'
      print*,' Please stand by...'
      print*,' '
      outcard=' '
      DO
        read(lsout,9001,iostat=istat)card
        if(istat.lt.0)exit
        if(card(1:6).eq.'total ')cycle
        if(card.eq.' ')then
          outcard=' '
          cycle
        endif
        call permsave_parse(card,1,field1,istat)
        if(istat.ne.0)then
          print*,'Unable to parse field 1'
        endif
        k=index(field1,':')
        IF(k.ne.0)then
!           This is a directory name
          outcard=' '
          tmpcard=' '
          tmpcard(1:k-1)=card(1:k-1)
          tmpcard(k:k)='/'
!               Relace the dot with the working directory
          outcard=trim(workdir) // trim(tmpcard(2:))
          i1=len_trim(outcard) + 1
        ELSE
!              Ignore if a link or directory
          if(card(1:1).eq.'d'.or.card(1:1).eq.'l')then
            cycle
          endif
!           Get the file name
          call permsave_parse(card,9,fname,istat)
          if(istat.ne.0)then
            print*,' Unable to parse card ',trim(card)
          endif
!             Eliminate certain extensions
          if(index(fname,'.trc').ne.0)cycle
          if(index(fname,'.tfile').ne.0)cycle
          if(index(fname,'.job').ne.0)cycle
          if(index(fname,'.byt').ne.0)cycle
          if(index(fname,'.glbl').ne.0)cycle
          if(index(fname,'.sgy').ne.0)cycle
          outcard(i1:)=' '
          nc=len_trim(fname)
          outcard(i1:i1+nc-1)=fname(1:nc)
          knt=knt+1
          k=mod(knt,100)
          if(k.eq.0)print*,' Examining file ',knt
!             Determine if this is a text file
          call permsave_testtext(txtlun,outcard,istat)
          if(istat.eq.1)then
            write(skplun,9001)outcard
            cycle
          endif
          write(plist,9001)outcard
        ENDIF
      ENDDO
      cmd='rm -f permsave.tmp1 permsave.home'
      call putsys_cmd(cmd)
      print*,' '
      print*,' Permsave is finished'

 9001 format(A)

      end program permsave
      subroutine permsave_testtext(lun,filename,status)

!         Read a file as if it is character data
!         If there are any characters out of the range of the ascii character
!           set - reject it as character data

      implicit none
      integer, intent(in) :: lun
      character(len=*),intent(in) :: filename
      integer, intent(out) :: status   ! 1=NOT text file, 0 = text file

!             local variables
      integer :: istat,i,j
      character(len=120) :: record

      status=0
      open(lun,file=filename,status='old',iostat=istat)
      if(istat.ne.0)then
        status=1
        go to 500
      endif

      DO
        read(lun,'(A)',iostat=istat)record
        if(istat.lt.0)exit
        do i=1,120
          j=ichar(record(i:i))
          if(j.lt.0.or.j.gt.127)then
            status=1
            go to 500
          endif
        enddo
      ENDDO
 500  continue
      close(lun)
      return

      end subroutine permsave_testtext
      subroutine permsave_parse(card,col,field,istat)

!         permsave_parse returns the colth field in card
!         columns are separated by spaces   
      implicit none  

      character(len=*),intent(in)  :: card
      character(len=*),intent(out) :: field
 
      integer,intent(in)  :: col
      integer,intent(out) :: istat

!        Local variables
      integer :: i,i1,i2,j,knt,nc

      nc=len(card)
      field=' '
      istat=0
      i1=0
      if(col.eq.1)i1=1
      i2=nc
      knt=1
      i=0
      DO 
        i=i+1
        if(i.gt.nc)exit
        if(card(i:i).eq.' ')then
           knt=knt+1
           do j=i,nc
             if(card(j:j).eq.' ')cycle
             i=j
             exit
           enddo
        endif
        if(knt.eq.col)then
          i1=i
          do j=i,nc
            if(card(j:j).eq.' ')then
              i2=j-1
               go to 500
             endif
          enddo
        endif
      ENDDO
 500  continue
      if(i1.eq.0)then
        istat=1
        return
      endif
      field=card(i1:i2)

      end subroutine permsave_parse


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!




