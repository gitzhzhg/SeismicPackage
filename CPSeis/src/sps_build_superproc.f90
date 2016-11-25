!<CPS_v1 type="PROGRAM"/>
!!----------------------- sps_build_superproc.f90 ---------------------------!!
!!----------------------- sps_build_superproc.f90 ---------------------------!!
!!----------------------- sps_build_superproc.f90 ---------------------------!!


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
! Name       : SPS_BUILD_SUPERPROC
! Category   : stand-alone
! Written    : 2003-11-03   by: Tom Stoeckley
! Revised    : 2005-04-28   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Build superproc primitive and process wrappers from templates.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This program builds the following files from templates:
!
!             superproc_frou.f90
!             superproc_crou.c
!             superwrap.c         (C wrappers for all CPS processes)
!             xxxx_superwrap.f90  (a Fortran wrapper file for each CPS process)
!
! Standard-in contains the list of potential CPS processes.
!
! Standard-in contains the maturity by itself on the first line, plus the
! following line for each CPS process:
!
!      process_name   maturity   full_path_name   mode(C or S)
!
! Standard-in contains the CPS processes in the following order:
!
!      custom processes     (mode C)
!      alpha processes      (mode S)  (only if maturity is alpha)
!      beta processes       (mode S)  (only if maturity is alpha or beta)
!      production processes (mode S)
!
! The following templates must exist:
!
!             $CFECUSTOMDIR/superproc_frou_template
!             $CFECUSTOMDIR/superproc_crou_template
!             $CFECUSTOMDIR/superwrap_cproc1_template
!             $CFECUSTOMDIR/superwrap_cproc2_template
!             $CFECUSTOMDIR/fproc_superwrap_template
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
!  4. 2005-04-28  Stoeckley  Change this to THIS so superproc_crou.c can be
!                             compiled with C++.
!  3. 2003-11-12  Stoeckley  Fix bug when searching for oneset and twosets;
!                             did not work correctly when the main routine
!                             has hd2 and tr2 optional.
!  2. 2003-11-04  Stoeckley  Add code to set the maturity correctly.
!  1. 2003-11-03  Stoeckley  Initial version.
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



!!--------------------------- start of module -------------------------------!!
!!--------------------------- start of module -------------------------------!!
!!--------------------------- start of module -------------------------------!!


      module sps_build_superproc_module
      use string_module
      use getsys_module
      use putsys_module
      use alphasort_module
      implicit none
      private
      public :: workhorse

      character(len=100),save :: SPS_BUILD_SUPERPROC_IDENT = &
'$Id: sps_build_superproc.f90,v 1.3 2008/02/14 19:52:23 mengewm Exp $'

      integer,parameter :: LUNI       = 5     ! standard in.
      integer,parameter :: LUNO       = 11    ! new output file.
      integer,parameter :: LUNS       = 22    ! CPS process source file.
      integer,parameter :: LUNT       = 33    ! template file.
      integer,parameter :: NPROC_SIZE = 666   ! allowed number of processes.
      integer,parameter :: CHAR_SIZE  = 444   ! length of character variables.

      character(len=CHAR_SIZE)  :: desired_maturity
      character(len=CHAR_SIZE)  :: lines    (NPROC_SIZE)
      character(len=CHAR_SIZE)  :: names    (NPROC_SIZE)
      character(len=CHAR_SIZE)  :: category (NPROC_SIZE)
      character(len=CHAR_SIZE)  :: maturity (NPROC_SIZE)
      logical                   :: oneset   (NPROC_SIZE)
      logical                   :: twosets  (NPROC_SIZE)
      logical                   :: custom   (NPROC_SIZE)
      integer                   :: nproc
      character(len=CHAR_SIZE)  :: syswrap  (NPROC_SIZE)
      integer                   :: nsyswrap

      contains


!!-------------------------- workhorse -------------------------------------!!
!!-------------------------- workhorse -------------------------------------!!
!!-------------------------- workhorse -------------------------------------!!


      subroutine workhorse

      call read_standard_in
      call read_superproc_frou_template
      call read_superproc_crou_template
      call read_superwrap_cproc_templates
      call read_fproc_superwrap_template

      end subroutine workhorse


!!------------------------ read standard in --------------------------------!!
!!------------------------ read standard in --------------------------------!!
!!------------------------ read standard in --------------------------------!!


      subroutine read_standard_in
      character(len=CHAR_SIZE) :: name,mat,path,cat,line,cust         ! local
      integer                  :: istat,iline,ntokens,nlines,itoken   ! local
      integer                  :: length                              ! local
      logical                  :: duplicate,one,two,whoops            ! local
      character(len=CHAR_SIZE) :: tokens(10)                          ! local

      print '("--> reading list of CPS process source files...")'

!!!!!!!!!!!!!!! read the desired maturity:

      read (LUNI,*,iostat=istat) desired_maturity
      if (istat /= 0) then
           print *, '!!!!! error trying to read the desired maturity'
           stop
      end if

!!!!!!!!!!!!!!! read the lines and eliminate duplicates:

      nsyswrap = 0
      nlines = 0
      do

           read (LUNI,200,iostat=istat) line
200        format (a)
           if (istat /= 0) then
                if (nlines > 0) exit               ! EOF encountered.
                print *, '!!!!! standard-in is empty'
                stop
           end if
           if (line == ' ') cycle

           call string_get_tokens (line,tokens,ntokens,' ')
           if (ntokens /= 4) then
                print *, '!!!!! bad line "'//trim(line)//'"'
                print *, '!!!!! ntokens =',ntokens
                do itoken = 1,ntokens
                     print *, 'token ',itoken,': "'//trim(tokens(itoken))//'"'
                end do
                stop
           end if

           name = tokens(1)
           if (name(1:9) == 'superproc') cycle

           cust = tokens(4)
           length = len_trim(name)
           if (name(length-9:length) == '_superwrap') then
                if (cust == 'S') then
                     nsyswrap = nsyswrap + 1
                     if (nsyswrap > NPROC_SIZE) then
                          print *, '!!!!! NSYSWRAP ',nsyswrap,' too large'
                          stop
                     end if
                     syswrap(nsyswrap) = name(1:length-10)
                end if
                cycle
           end if

           duplicate = .false.
           do iline = 1,nlines
                if (name == names(iline)) then
                     duplicate = .true.
                     exit
                end if
           end do
           if (duplicate) cycle

           nlines = nlines + 1
           if (nlines > NPROC_SIZE) then
                print *, '!!!!! NLINES ',nlines,' too large'
                stop
           end if
           lines(nlines) = line
           names(nlines) = name

      end do

!!!!!!!!!!!!!!! sort the lines:

      call alphasort_sort (lines,nlines)

!!!!!!!!!!!!!!! scan the source files:

      nproc = 0
      do iline = 1,nlines

           call string_get_tokens (lines(iline),tokens,ntokens,' ')
           if (ntokens /= 4) then
                print *, '!!!!! bad string "'//trim(line)//'"'
                print *, '!!!!! ntokens =',ntokens
                do itoken = 1,ntokens
                     print *, 'token ',itoken,': "'//trim(tokens(itoken))//'"'
                end do
                stop
           end if

           name = tokens(1)
           mat  = tokens(2)
           path = tokens(3)
           cust = tokens(4)

           call scan_file (name,path,cat,one,two,whoops)
           if (whoops) cycle

           call string_to_upper (name)
           call string_to_upper (cat)
           call string_to_upper (mat)
           call string_to_upper (cust)

           nproc = nproc + 1
           names    (nproc) = name
           category (nproc) = cat
           maturity (nproc) = mat
           oneset   (nproc) = one
           twosets  (nproc) = two
           custom   (nproc) = (cust == 'C')

      end do

      end subroutine read_standard_in


!!----------------------- read superproc_frou template ----------------------!!
!!----------------------- read superproc_frou template ----------------------!!
!!----------------------- read superproc_frou template ----------------------!!


      subroutine read_superproc_frou_template
      character(len=CHAR_SIZE) :: directory,template,newfile          ! local
      character(len=CHAR_SIZE) :: line,date,choice,routine            ! local
      integer                  :: istat,iproc,kount                   ! local

      print '("--> building superproc_frou.f90...")'

!!!!!!!!!!!!!!! open template file:

      call getsys_env ('CFECUSTOMDIR', directory)
      template = trim(directory)//'/superproc_frou_template'
      newfile  = 'superproc_frou.f90'

      open (LUNT,file=template,status="old",iostat=istat)
      if (istat /= 0) then
           print *, '!!!!! error opening '//trim(template)
           stop
      end if

!!!!!!!!!!!!!!! open new output file:

      open (LUNO,file=newfile,status="replace",iostat=istat)
      if (istat /= 0) then
           print *, '!!!!! error opening '//trim(newfile)
           stop
      end if

!!!!!!!!!!!!!!! read template file:

      kount = 0
      do

           read (LUNT,1000,iostat=istat) line
1000       format (a)
           if (istat /= 0) then
                if (kount > 0) exit               ! EOF encountered.
                print *, '!!!!! error reading '//trim(template)
                exit
           end if
           kount = kount + 1

           if (line == '!!!!MATURITY') then

                write (LUNO,1500,iostat=istat) trim(desired_maturity)
1500            format ('! Maturity   : ',a)
                if (istat /= 0) then
                     exit
                end if

           else if (line == '!!!!LIST') then

                write (LUNO,2000,iostat=istat) nproc
2000            format ('!PROCESS_COUNT=',i3)
                if (istat /= 0) then
                     exit
                end if
                do iproc = 1,nproc
                     write (LUNO,3000,iostat=istat) &
                             names(iproc),category(iproc),maturity(iproc)
3000                 format ('!',a25,1x,a25,1x,a25)
                     if (istat /= 0) then
                          exit
                     end if
                end do

           else if (line == '!!!!IDENT') then

                do iproc = 1,nproc
                     choice  = '"'//trim(names(iproc))//'"'
                     routine =      trim(names(iproc))//'_superwrap_rcs'
                     write (LUNO,11000,iostat=istat) choice,routine
11000                format ('      case (',a15,') ; call ',a35,' (ident)')
                     if (istat /= 0) then
                          exit
                     end if
                end do

           else if (line == '!!!!LEN_ALL') then

                write (LUNO,15000,iostat=istat) nproc
15000           format ('      integer,parameter,public  &
                                & :: LEN_ALL              = ',i3)
                if (istat /= 0) then
                     exit
                end if

           else if (line == '!!!!LOADLIST') then

                do iproc = 1,nproc
                  write (LUNO,15001,iostat=istat) iproc,trim(names(iproc))
                  if (istat /= 0) then
                       exit
                  end if
                  write (LUNO,15002,iostat=istat) iproc,trim(category (iproc))
                  if (istat /= 0) then
                       exit
                  end if
                  write (LUNO,15003,iostat=istat) iproc,trim(maturity (iproc))
                  if (istat /= 0) then
                       exit
                  end if
                  write (LUNO,15004,iostat=istat)
                  if (istat /= 0) then
                       exit
                  end if
15001             format ('      all(',i3,')%process  = "',a,'"')
15002             format ('      all(',i3,')%category = "',a,'"')
15003             format ('      all(',i3,')%maturity = SUPERPROC_',a)
15004             format (' ')
                end do

           else

                write (LUNO,17000,iostat=istat) trim(line)
17000           format (a)
                if (istat /= 0) then
                     exit
                end if

           end if

      end do

!!!!!!!!!!!!!!! close files:

      close (LUNO,iostat=istat)
      if (istat /= 0) then
           print *, '!!!!! error closing '//trim(newfile)
           stop
      end if

      close (LUNT,iostat=istat)
      if (istat /= 0) then
           print *, '!!!!! error closing '//trim(template)
           stop
      end if

      end subroutine read_superproc_frou_template


!!-------------------- read superproc_crou template -------------------------!!
!!-------------------- read superproc_crou template -------------------------!!
!!-------------------- read superproc_crou template -------------------------!!


      subroutine read_superproc_crou_template
      character(len=CHAR_SIZE) :: directory,template,newfile,choice   ! local
      character(len=CHAR_SIZE) :: line,date,upper,lower,routine       ! local
      integer                  :: istat,iproc,kount                   ! local

      print '("--> building superproc_crou.c...")'

!!!!!!!!!!!!!!! open template file:

      call getsys_env ('CFECUSTOMDIR', directory)
      template = trim(directory)//'/superproc_crou_template'
      newfile  = 'superproc_crou.c'

      open (LUNT,file=template,status="old",iostat=istat)
      if (istat /= 0) then
           print *, '!!!!! error opening '//trim(template)
           stop
      end if

!!!!!!!!!!!!!!! open new output file:

      open (LUNO,file=newfile,status="replace",iostat=istat)
      if (istat /= 0) then
           print *, '!!!!! error opening '//trim(newfile)
           stop
      end if

!!!!!!!!!!!!!!! read template file:

      kount = 0
      do

           read (LUNT,1000,iostat=istat) line
1000       format (a)
           if (istat /= 0) then
                if (kount > 0) exit               ! EOF encountered.
                print *, '!!!!! error reading '//trim(template)
                exit
           end if
           kount = kount + 1

           if (line == '!!!!MATURITY') then

                write (LUNO,1500,iostat=istat) trim(desired_maturity)
1500            format ('! Maturity   : ',a)
                if (istat /= 0) then
                     exit
                end if

           else if (line == '!!!!PROTO') then

                do iproc = 1,nproc
                     call string_to_lower (names(iproc),lower)
                     routine = 'superwrap_'//trim(lower)//'_startup'
                     write (LUNO,3000,iostat=istat) trim(routine)
3000                 format ('ProcessStartup ',a,';')
                     if (istat /= 0) then
                          exit
                     end if
                end do

           else if (line == '!!!!STARTUP') then

                do iproc = 1,nproc
                     call string_to_upper (names(iproc),upper)
                     call string_to_lower (names(iproc),lower)
                     choice  = '"'//trim(upper)//'"'
                     routine = 'superwrap_'//trim(lower)//'_startup'
                     write (LUNO,4000,iostat=istat) choice,routine
4000                 format ('  if(strcmp(name, ',a15,')==0) ', &
                             a30,'(THIS);')
                     if (istat /= 0) then
                          exit
                     end if
                end do

           else

                write (LUNO,17000,iostat=istat) trim(line)
17000           format (a)
                if (istat /= 0) then
                     exit
                end if

           end if

      end do

!!!!!!!!!!!!!!! close files:

      close (LUNO,iostat=istat)
      if (istat /= 0) then
           print *, '!!!!! error closing '//trim(newfile)
           stop
      end if

      close (LUNT,iostat=istat)
      if (istat /= 0) then
           print *, '!!!!! error closing '//trim(template)
           stop
      end if

      end subroutine read_superproc_crou_template


!!-------------------- read superwrap_cproc templates ----------------------!!
!!-------------------- read superwrap_cproc templates ----------------------!!
!!-------------------- read superwrap_cproc templates ----------------------!!


      subroutine read_superwrap_cproc_templates
      character(len=CHAR_SIZE) :: directory,newfile                    ! local
      character(len=CHAR_SIZE) :: template1,template2                  ! local
      character(len=CHAR_SIZE) :: upper,command,edit1,edit2,lower      ! local
      integer                  :: iproc                                ! local

      print '("--> building superwrap.c...")'

      call getsys_env ('CFECUSTOMDIR', directory)
      template1 = trim(directory)//'/superwrap_cproc1_template'
      template2 = trim(directory)//'/superwrap_cproc2_template'
      newfile   = 'superwrap.c'

      edit1   = '-e s+MMMM+'//trim(desired_maturity)//'+g'
      command = 'sed '//trim(edit1)//  &
                      ' '//trim(template1)//' > '//trim(newfile)

 !!!  command = 'cat '//trim(template1)//' > '//trim(newfile)

      call putsys_cmd (command)

      do iproc = 1,nproc
           call string_to_upper (names(iproc),upper)
           call string_to_lower (names(iproc),lower)

           edit1   = '-e s+XXXX+'//trim(lower)//'+g'
           edit2   = '-e s+UUUU+'//trim(upper)//'+g'
           command = 'sed '//trim(edit1)//' '//trim(edit2)//  &
                           ' '//trim(template2)//' >> '//trim(newfile)

           call putsys_cmd (command)

      end do

      end subroutine read_superwrap_cproc_templates


!!-------------------- read fproc_superwrap template ----------------------!!
!!-------------------- read fproc_superwrap template ----------------------!!
!!-------------------- read fproc_superwrap template ----------------------!!


      subroutine read_fproc_superwrap_template
      character(len=CHAR_SIZE) :: directory,template                   ! local
      character(len=CHAR_SIZE) :: upper,command                        ! local
      character(len=CHAR_SIZE) :: edit1,edit2,edit3,edit4              ! local
      character(len=CHAR_SIZE) :: oneone,twotwo,lower,name1,name2      ! local
      integer                  :: iproc,isyswrap,kount1,kount2         ! local
      logical                  :: exists                               ! local

!!!!!!!!!!!!!!! initialization:

      print '("--> building xxxx_superwrap.f90 wrappers...")'

      call getsys_env ('CFECUSTOMDIR', directory)
      template = trim(directory)//'/fproc_superwrap_template'

!!!!!!!!!!!!!!! start looping through processes:

      kount1 = 0
      kount2 = 0
      do iproc = 1,nproc

           call string_to_upper (names(iproc),upper)
           call string_to_lower (names(iproc),lower)

           name1 = trim(lower)//'_superwrap.f90'
           name2 = trim(lower)//'_superwrap.f90_previous'

!!!!!!!!!!!!!!! find out if system wrapper exists if not custom process:

           if (.not.custom(iproc)) then
                exists = .false.
                do isyswrap = 1,nsyswrap
                     if (syswrap(isyswrap) == lower) then
                          exists = .true.
                          exit
                     end if
                end do

!!!!!!!!!!!!!!! remove previous and current wrapper and cycle if not
!!!!!!!!!!!!!!! custom process and if system wrapper exists:

                if (exists) then
                     command = 'if [ -f '//trim(name1)//' ] ; then &
                               &"rm" '//trim(name1)//' ; fi'
                     call putsys_cmd (command)
                     command = 'if [ -f '//trim(name2)//' ] ; then &
                               &"rm" '//trim(name2)//' ; fi'
                     call putsys_cmd (command)
                     cycle
                end if
           end if

!!!!!!!!!!!!!!! save previous version of wrapper:

   !!!! C shell:
   !!!!    command = 'if(-f '//trim(name1)//') then ; &
   !!!!              &"mv" '//trim(name1)//' '//trim(name2)//' ; endif'
   !!!! end C shell.

   !!!! bourne shell:
           command = 'if [ -f '//trim(name1)//' ] ; then &
                     &"mv" '//trim(name1)//' '//trim(name2)//' ; fi'
   !!!! end bourne shell.

           call putsys_cmd (command)

!!!!!!!!!!!!!!! build new version of wrapper:

           oneone = '!!!'
           twotwo = '!!!'
           if (oneset (iproc)) oneone = ' '
           if (twosets(iproc)) twotwo = ' '

           edit1   = '-e s+XXXX+'//trim(lower)//'+g'
           edit2   = '-e s+ONE+'//trim(oneone)//'+g'
           edit3   = '-e s+TWO+'//trim(twotwo)//'+g'
           edit4   = '-e s+MMMM+'//trim(desired_maturity)//'+g'

           command = 'sed '//trim(edit1)//' '//trim(edit2)//  &
                  ' '//trim(edit3)//' '//trim(edit4)//  &
                  ' '//trim(template)//' > '//trim(name1)

           call putsys_cmd (command)

!!!!!!!!!!!!!!! replace with previous version if unchanged:

   !!!! C shell:
   !!!!    command = 'if(-f '//trim(name2)//') then ; &
   !!!!               &set changed = false ; &
   !!!!   &diff '//trim(name1)//' '//trim(name2)//' >& /dev/null || &
   !!!!                     &set changed = true ; &
   !!!!               &if($changed == false) then ; &
   !!!!        &"mv" '//trim(name2)//' '//trim(name1)//  &
   !!!!          ' > /dev/null ; else ; "rm" '//trim(name2)//' ; endif ; endif'
   !!!! end C shell.

   !!!! bourne shell:
           command = 'if [ -f '//trim(name2)//' ] ; then &
                      &changed=false ; &
          &diff '//trim(name1)//' '//trim(name2)//' >& /dev/null || &
                            &changed=true ; &
                      &if [ $changed = false ] ; then &
               &"mv" '//trim(name2)//' '//trim(name1)//  &
                 ' > /dev/null ; else "rm" '//trim(name2)//' ; fi ; fi'
   !!!! end bourne shell.

           call putsys_cmd (command)

!!!!!!!!!!!!!!! finish looping through processes:

           if (custom(iproc)) then
                kount1 = kount1 + 1
           else
                kount2 = kount2 + 1
           end if

      end do

      print 1000, kount1,"custom"
      print 1000, kount2,"system"
1000  format &
        ("--> ",i5," xxxx_superwrap.f90 wrappers built for ",a," processes.")

      end subroutine read_fproc_superwrap_template


!!-------------------------- scan file -----------------------------------!!
!!-------------------------- scan file -----------------------------------!!
!!-------------------------- scan file -----------------------------------!!


      subroutine scan_file (name,path,cat,one,two,whoops)
      character(len=*),intent(in)  :: name,path                ! arguments
      character(len=*),intent(out) :: cat                      ! arguments
      logical         ,intent(out) :: one,two,whoops           ! arguments
      character(len=CHAR_SIZE)     :: line                     ! local
      character(len=CHAR_SIZE)     :: process_tag              ! local
      character(len=CHAR_SIZE)     :: category_prompt          ! local
      character(len=CHAR_SIZE)     :: interface_name           ! local
      character(len=CHAR_SIZE)     :: subroutine_name          ! local
      character(len=CHAR_SIZE)     :: end_subroutine_name      ! local
      integer                      :: len_category_prompt      ! local
      integer                      :: len_subroutine_name      ! local
      integer                      :: len_end_subroutine_name  ! local
      integer                      :: istat,kount              ! local
      integer                      :: indx,ncommas,indx2       ! local
      integer                      :: searching_for_optional   ! local

!!!!!!!!!!!!!!! initialize output arguments:

      cat    = ' '
      one    = .false.
      two    = .false.
      whoops = .true.

!!!!!!!!!!!!!!! open CPS process source file:

      open (LUNS,file=path,status="old",iostat=istat)
      if (istat /= 0) then
           print *, '!!!!! error opening ',trim(path)
           whoops = .true.
           return
      end if

!!!!!!!!!!!!!!! scan CPS process source file:

      process_tag             = '!<cps_v1type="process"/>'
      category_prompt         = '!category:'
      interface_name          = 'interface'//name
      subroutine_name         = 'subroutine'//trim(name)//'('
      end_subroutine_name     = 'endsubroutine'//name
      len_category_prompt     = len_trim(category_prompt)
      len_subroutine_name     = len_trim(subroutine_name)
      len_end_subroutine_name = len_trim(end_subroutine_name)
      searching_for_optional  = 0

      kount = 0
      do

           read (LUNS,1000,iostat=istat) line
1000       format (a)
           if (istat /= 0) then
                if (kount > 0) exit               ! EOF encountered.
                print *, '!!!!! error reading ',trim(path)
                whoops = .true.
                exit
           end if
           if (line == ' ') cycle

           call string_squeeze_blanks (line)
           call string_to_lower       (line)

           kount = kount + 1
           if (kount == 1) then
                if (line == process_tag) then
                     whoops = .false.
                     cycle
                else
                     whoops = .true.
                     exit
                end if
           end if

           if (cat == ' ') then
                if (line(1:len_category_prompt) == category_prompt) then
                     cat = line(len_category_prompt+1:)
                     cycle
                end if
           end if

          if (line == interface_name) then
               one = .true.
               two = .true.
               exit
          end if

          if (line(1:len_subroutine_name) == subroutine_name) then
               ncommas = 0
               do indx = 1,len(line)
                    if (line(indx:indx) == ',') ncommas = ncommas + 1
               end do
               if (ncommas == 3) then
                    one = .true.
                    exit
               else if (ncommas == 5) then
                    two = .true.
                    searching_for_optional = 1
               end if
          end if

!!!!!!!!!!!!! one might be true even if two is true
!!!!!!!!!!!!! if the last two arguments are optional:

          if (searching_for_optional > 0) then
               searching_for_optional = searching_for_optional + 1
               if (searching_for_optional > 50) then
                    searching_for_optional = 0
                    exit
               else if (line(1:len_end_subroutine_name) &
                                == end_subroutine_name) then
                    searching_for_optional = 0
                    exit
               else if (line(1:5) == 'real,') then
                    indx = index(line,',optional')
                    if (indx > 0) then
                         indx2 = index(line,'!')
                         if (indx2 == 0 .or. indx2 > indx) then
                              one = .true.
                              searching_for_optional = 0
                              exit
                         end if
                    end if
               end if
          end if

      end do

!!!!!!!!!!!!!!! close CPS process source file:

      if (cat == ' ') whoops = .true.

      close (LUNS,iostat=istat)
      if (istat /= 0) then
           print *, '!!!!! error closing '//trim(path)
           stop
      end if

      end subroutine scan_file


!!----------------------------- end of module -------------------------------!!
!!----------------------------- end of module -------------------------------!!
!!----------------------------- end of module -------------------------------!!


      end module sps_build_superproc_module


!!---------------------------- main program -------------------------------!!
!!---------------------------- main program -------------------------------!!
!!---------------------------- main program -------------------------------!!


      program sps_build_superproc
      use sps_build_superproc_module
      implicit none

      call workhorse
      end program sps_build_superproc


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!

