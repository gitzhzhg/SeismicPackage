!<CPS_v1 type="PROGRAM"/>
!!------------------------------ pickmerge.f90 -------------------------------!!
!!------------------------------ pickmerge.f90 -------------------------------!!
!!------------------------------ pickmerge.f90 -------------------------------!!


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
! Name       : PICKMERGE
! Category   : stand-alone
! Written    : 1990-02-02   by: Greg Lazear
! Revised    : 2001-12-20   by: Karen Goodger
! Maturity   : beta
! Purpose    : Create 3rd pick file using picks from one and geometry from one.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!           Reads two refraction static pick files and creates    
!           a third by using the geometry data from the first     
!           file and the pick times from the second file. this    
!           allows the geometry (fgd) to be modified while keeping 
!           picks from manual or interactive editing.             
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS

!  1. In order to correct the geometry (fgd) on a data set and still     
!     retain the picks made interactively, perform the following        
!     sequence:                                                         
!               - correct the fgd and run scrs batch picking to create   
!                 a new pickfile with the correct geometry.             
!               - run this program pickmerge and give it three file     
!                 names.                                                
!                      first is the new pickfile with good geometry     
!                      second is the old pickfile which has the picks   
!                        to be used                                     
!                      third is the name of the new pickfile which has  
!                        the new geometry and correct picks             
!               - use the output pickfile in future scrs solutions or   
!                 editing.                                              
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author          Description
!     ----        ------          -----------
!
!  2. 2001-12-20  Karen Goodger   Converted from old system.
!  1. 1990-02-02  Greg Lazear     Initial version.
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


!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!


      program pickmerge
!!!   use abcdef_module   ! for each module to be used.
      implicit none

      character(len=100),save :: PICKMERGE_IDENT = &
'$Id: pickmerge.f90,v 1.1 2008/02/15 19:13:03 mengewm Exp $'

!     
      integer :: inch, lfn1, lfn2, lfn3 
      parameter(inch = 1000,lfn1 = 7,lfn2 = 8,lfn3 = 9) 
      character(len=80) :: vaxfile1,vaxfile2,vaxfile3,temp

      real :: shot(10),xpw(100),ypw(100),pkwn(1800)
      integer :: iarriv(inch),ixgp(inch),iygp(inch),    &
                 ioff(inch),ielev(inch),ibad(inch)

      integer :: i,ic,id1,id3,id4,id5,id6,id7,id8,incgp
      integer :: ipkwhx,ipkwhy,istat,itmax,izc
      integer :: jd1,jd2,jd3,jd4,jd5,jd6,jgrp,jgrp2
      integer :: mnxgp,mnygp,mxo,mxxgp,mxygp
      integer :: nch,nch2,npwoff,nxpw,nypw
      
      real :: d2,d7,rinc,trsh
                                                             
!                                                                       
   10 print * ,'ENTER THE NAME OF THE PICKFILE WITH CORRECT GEOMETRY ' 
      vaxfile1 = ' ' 
      read(5,'(A80)',end = 15) vaxfile1 
      if(vaxfile1.ne.' ') goto 17 
   15 stop 
   17 call pickmerge_openfile(lfn1,vaxfile1,'READ',istat) 
!                                                                       
      print * ,'ENTER THE NAME OF THE PICKFILE WITH CORRECT PICKS ' 
      read(5,'(A80)') vaxfile2 
      call pickmerge_openfile(lfn2,vaxfile2,'READ',istat)
      if(istat.ne.0)go to 777 
!                                                                       
      print * ,'ENTER NAME FOR THE OUTPUT FILE(DEFAULT = FIRST NAME)' 
      temp = ' ' 
      vaxfile3 = vaxfile1 
      read(5,'(A80)',end = 20) temp 
      if(temp.ne.' ') vaxfile3 = temp 
   20 continue 
      call pickmerge_openfile(lfn3,vaxfile3,'WRITE',istat) 
      if(istat.ne.0)go to 777
!                                                                       
!  read parameters from the two files to create the output parameter set
!                                                                       
      rewind lfn1 
      rewind lfn2 
      rewind lfn3 
      read(lfn1,5001) jgrp,id1,mxxgp,mxygp,mnxgp,mnygp,mxo,    &
                      incgp,rinc,nch,d2,id3,id4,id5,id6,id7,id8                
      read(lfn2,5001) jgrp2,itmax,jd1,jd2,jd3,jd4,jd5,jd6,d7,&
                      nch2,trsh,izc,nxpw,nypw,npwoff,ipkwhx,ipkwhy  
!                                                                       
!   number of channels and shot groups must agree for the two files     
!                                                                       
      if(jgrp.ne.jgrp2) then 
        print * ,'THE TWO FILES DO NOT HAVE THE SAME NUMBER OF SHOTS' 
        stop 
      endif 
      print * ,'NUMBER OF SHOT FILES = ',jgrp 
      if(nch.ne.nch2) then 
      print * ,'THE TWO FILES DO NOT HAVE THE SAME NUMBER OF CHANNELS',&
     & ' PER SHOT'                                                      
        stop 
      endif 
      print * ,'NUMBER OF CHANNELS PER SHOT FILE = ',nch 
!                                                                       
!   keep the pickwindow parameters from the file with good picks        
!                                                                       
      read(lfn1,5005)(xpw(i),i = 1,id4) 
      read(lfn1,5005)(ypw(i),i = 1,id5) 
      read(lfn1,5006)(pkwn(i),i = 1,id6 * 3 * id4 * id5) 
!                                                                       
      read(lfn2,5005)(xpw(i),i = 1,nxpw) 
      read(lfn2,5005)(ypw(i),i = 1,nypw) 
      read(lfn2,5006)(pkwn(i),i = 1,npwoff * 3 * nxpw * nypw) 
!                                                                       
!   output parameters to the new pickfile                               
!                                                                       
      write(lfn3,5001) jgrp,itmax,mxxgp,mxygp,mnxgp,mnygp,mxo, &
      incgp,rinc,nch,trsh,izc,nxpw,nypw,npwoff,ipkwhx,ipkwhy   
      write(lfn3,5005)(xpw(i),i = 1,nxpw) 
      write(lfn3,5005)(ypw(i),i = 1,nypw) 
      write(lfn3,5006)(pkwn(i),i = 1,npwoff * 3 * nxpw * nypw) 
      print * ,'FILE PARAMETERS HAVE BEEN WRITTEN TO NEW FILE' 
!                                                                       
!  read shot records from each file and output the merged data to new fi
!                                                                       
      print * ,'MERGING PICKS AND GEOMETRY DATA' 
      do i = 1,jgrp 
        read(lfn2,5002)(shot(ic),ic = 1,10),(iarriv(ic),       &
                        ixgp(ic),iygp(ic),ioff(ic),ielev(ic),ic = 1,nch)       
        read(lfn1,5002)(shot(ic),ic = 1,10),(ibad(ic),ixgp(ic)&
                        ,iygp(ic),ioff(ic),ielev(ic),ic = 1,nch)                
        write(lfn3,5002)(shot(ic),ic = 1,10),(iarriv(ic),      &
                         ixgp(ic),iygp(ic),ioff(ic),ielev(ic),ic = 1,nch)       
        if(mod(i,100) .eq.0) print* ,i,' SHOT RECORDS MERGED' 
      enddo 
      print * ,'MERGING OF FILES COMPLETE' 
!                                                                       
  777 close(lfn1) 
      close(lfn2) 
      close(lfn3) 
!                                                                       
 5001 format(8i6,/,f6.1,i6,f6.3,3x,a3,5i4) 
 5002 format(5f10.3,/,5f10.3,/,(5i6)) 
 5005 format(5(1x,f9.0)) 
 5006 format(3(1x,f9.3)) 
!                                                                       
      stop 
      end program pickmerge                         
!                                                                       
      subroutine pickmerge_openfile(lfn,vaxfile,rw,status) 
!     open a file for read or write.                                
!     lfn = local file name(unit number).                              
!     vaxfile = file name.                                          
!     rw = 'READ' to open an existing vax file for read.                
!     rw = 'WRITE'(or anything else) to open a new vax file for write. 
!     file name will be expanded to full description.               
!     prints message and uses alternate return upon error.    

      integer,intent(in)    :: lfn
      integer,intent(inout) :: status          
!----------dimension statements.                                        
      character(len=*),intent(inout) :: vaxfile
      character(len=*),intent(in)    :: rw 
      character(len=80) :: fullname 
      logical :: quest 

      status=0
!----------open for write.                                              
      if(rw.ne.'READ') open(unit = lfn,err = 999,file = vaxfile,   &
                            status = 'NEW')          
!----------expand name and check on existance.                          
      inquire(file = vaxfile,name = fullname,err = 999,exist = quest)
      vaxfile = fullname 
      if(.not.quest) goto 999 
!----------open for read.                                               
      if(rw.eq.'READ') open(unit = lfn,err = 999,file = vaxfile,   &
                            status = 'OLD')
!----------we are successful.                                           
      return 
!----------we are not successful.                                       
  999 print *,vaxfile 
      print * ,'ERROR ON ATTEMPT TO OPEN ABOVE FILE FOR ',rw 
      status=-2
      end subroutine pickmerge_openfile                       



!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!




