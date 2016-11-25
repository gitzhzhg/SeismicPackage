!<CPS_v1 type="PROGRAM"/>
!!------------------------------ sd2fg.f90 --------------------------------!!
!!------------------------------ sd2fg.f90 --------------------------------!!
!!------------------------------ sd2fg.f90 --------------------------------!!


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
! Name       : SD2FG
! Category   : stand-alone
! Written    : 2002-07-10   by: Karen Goodger
! Revised    : 2004-06-29   by: Karen Goodger
! Maturity   : production
! Purpose    : Read SEGD field header and output pp and rp cards for FGD.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!  SD2FG gets header information from the SEGD file header for the I/O SYSTEM
!  TWO field system.
!
!  A file containing a hex dump of the input is created when the SEGD field
!  tapes are read in with Oil Data software.  This dump is read into a 
!  character variable.  Values are extracted by internal reads using Z-format
!  for binary values and I-format for binary-coded-decimal values (BCD).  Input
!  data are organized in 32-byte blocks.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!  File input: Input to SD2FG are the header files from the SEGD tapes which
!  are saved to disk by the Oil Data software when the tapes are read.  These
!  files should be copied to the directory where SD2FG will be run.  One file is
!  created for each tape.  Naming conventions for these files is 
!  JOBNAME_reelno.HDR.  You may read all files in at once using an * for a wild
!  card, e.g. JOBNAME*.  SD2FG will read in all the JOBNAME files in reelno
!  order.  One can also have a file containing a list of the header files in 
!  the order to be read.  Enter FILE=filename in that case.  The 'FILE=' is a
!  keyword indicating that filename is a file containing a list of names.
!
!  File output: SD2FG will prompt for an output file name.  Give it a name 
!  without an extension.  SD2FG will output two files.  One will be your RPPP
!  cards and will have the extension .RPPP.  The other will be a .CPR file and
!  will contain information about what profiles were located on what header
!  files, where aux channels are, and any problems encountered.
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date         Author     Description
!     ----         ------     -----------
!3.   2004-06-29   Goodger    Increase nline array from 99 to 300.  Skip
!                             files which do not have the extended binary
!                             header.
!2.   2004-03-25   Goodger    Change syntax on unix sort.  Previous syntax is
!                             no longer valid.
!1.   2002-07-10   Goodger    Converted from Vax 
!
!                OLD REVISION HISTORY
! 00/05/18   K. Goodger    Increase #channel sets from 40 to 99.        
! 00/05/17   K. Goodger    Increase nline array from 16 to 99.          
! 00/05/15   K. Goodger    Added extra printout to cpr file.            
! 00/05/09   K. Goodger    Increase #channel sets from 20 to 40.        
! 00/05/08   K. Goodger    Change format on seis type read from I1 to Z1
! 00/03/01   K. Goodger    More corrupt line numbers.  Able to use      
!                          total from channel set rather than line, but 
!                          not much else help.                          
! 00/02/18   K. Goodger    Had some files with a corrupt line number.   
!                          I put in a zero for line number on this data 
!                          and the user will have to hand edit.         
! 00/02/18   K. Goodger    Added test for the HEADER card after the     
!                          read of shotpoint.                           
! 00/02/17   K. Goodger    Add message about vision format.  Remove     
!                          byte count message.  Use index function to   
!                          check for header card.                       
! 00/02/09   K. Goodger    Fix bug in compusies which was using the     
!                          first receiver line on each card.            
! 00/02/08   K. Goodger    Fix bugs in compuseis.                       
! 00/02/05   K. Goodger    Add routine compuseis for manuf code 33.     
! 98/08/14   K. Goodger    The format of the HEADER BYTE COUNT is moved 
!                          over 1 space on the new equipment.  Fix to   
!                          read either format.                          
! 98/07/28   K. Goodger    Search for the underscore in the filename    
!                          backwards rather than forwards.  Encountered 
!                          a directory name with an underscore.         
! 98/04/04   K. Goodger    Fix bug reading the skips information out of 
!                          order.  Change some abort code to warnings.  
! 97/08/06   K. Goodger    Changed more spacing.                        
! 97/07/24   K. Goodger    Changed more spacing.                        
! 97/06/06   K. Goodger    Change spacing on JD and LD header cards to  
!                          be compatible with CFG.                      
! 96/09/04   K. Goodger    Increase format on #rp cards from i4 to i5.  
! 96/08/21   K. Goodger    Add JD header cards which count the LD,RP,   
!                          and PP cards.  Add dummy LD card.            
! 96/08/20   K. Goodger    Get ancillary data from data sheets if needed
!                          Handle a header file with zero records at the
!                          end.                                         
! 95/08/24   K. Goodger    First version converted from Bill Troutt's   
!                          TTRIN_FORDG.CFT on the Cray.                 
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

      module sd2fg_units
      integer :: irppp,ipp,irp,icpr,ifnames,inp
      end module sd2fg_units
      module sd2fg_ancillary
      integer :: ifile(1000),shot1(1000),shot2(1000) 
      end module sd2fg_ancillary
      module sd2fg_fg3
        integer :: itf,ifh,iff,jfile,jset,isets(2,300),nline,lines(9,300),&
                 jbad,nskip,iskips(3,50)        
      real :: shot(2)                      
      end module sd2fg_fg3


!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!

      module sd2fg_module
      use putsys_module
      use sd2fg_ancillary
      use sd2fg_fg3
      use sd2fg_units
      use string_module
      implicit none

      logical,save :: loboadj=.false.

      contains

      subroutine sd2fg_main

      call sd2fg_filein
      call sd2fg_fileout
      call sd2fg_datasheets
      call sd2fg_end
      print*,' finished'
      write(icpr,9001)' finished' 
      endfile icpr 
 9001 format(A)       

      end subroutine sd2fg_main
       SUBROUTINE SD2FG_END 
!                                                                       
      character cfile*64,errtyp*32 
      character(len=82) :: cards(30),card 
      character(len=80) :: irphead,ipphead,ijdhead,dumld 
      character(len=90) :: ildhead 
      character(len=3) :: mth
      character(len=8) :: ktime,ctemp 
      character(len=9) :: kdate 
      character(len=10) :: ctemp10
      character(len=24) :: cardfile
                                                                        
      integer :: iblk,iblok,ier,ilist(2,50),iiss,iset,istat,j
      integer :: k,kchan,kchtot,kk,kline,kset,ksline,kslineo,kssta,ksstao,ktyp
      integer :: len,lfile,manuf
      integer :: nancbl,naux,naux1,naux2,nbad,nblk,nbytes,nchan,nchans,&
                 nchk,ncrd,ncsets,&
                 ncset1
      integer :: ngapd,ngap,ngap1,ngap2,nleft
      integer :: nl,nldone,nlinbl,ns,nseg,nskpbl,nsta1,nsta2,nsta12
      integer :: numprof,numrp,numskp,numwrd,trtot 
                                                                        
      character *60 FILEIN 
      save cardfile 
      logical :: dbug,hex,ifirst,data_sheets,long_bytes 
      data irphead/' PAT#   FLAG     sp#   LINE#    #X  XINC      #Y   Y&
     &INC     XsD     YsD    ELsD'/                                     
      data ipphead/' Sp#    Line#  sP#    line#  PAT# Xsd  Ysd  HOLD  El&
     &ev  Hd  Tuh  SRC  REC  GRP#'/                                     
      data ijdhead/'  DATE       TIME      CHAIN     #LD    #RP    #PP  &
     &  #ZT1   #ZT2   #ZT3   #ZT4'/                                     
      data ildhead/' SP#     DIST     XLOC    YLOC     ELEV   HD  TUH   &
     &  TR   TS  XSD   YSD    ELSD'/                                    
      data dumld/'   1    220'/ 
      data numprof/0/,trtot/0/,numrp/0/ 
      data data_sheets/.FALSE./,long_bytes/.TRUE./ 
                         !turn off print of values                      
      data dbug/.FALSE./ 
                       !size of common SD2FG3 in words (begin w/ jfile) 
!-----info saved from SEGD file header (except for 1st 3 items)         
!-----  itf=trace counter in "file", ifh=flag for 1st file              
!-----  iff=file counter                                                
!     jfile = field file#                                               
!     jset  = #channel sets                                             
!     isets = channel set info:                                         
!            isets(1,*)=1, for seis --- else aux.                       
!            isets(2,*)=#tr in this set                                 
!     nline = #lines (# sets info in lines)                             
!     lines = line info:                                                
!             lines(1,*)=line#                                          
!             lines(2,*)=#ch sets this line                             
!             lines(3,*)=1st ch set# this line                          
!             lines(4,*)=1st station # this line                        
!             lines(5,*)=last station # this line                       
!             lines(6,*)=# stations for this line                       
!             lines(7,*)=# stations in gap                              
!             lines(8,*)=1st station in gap                             
!             lines(9,*)=# of skipped stations this line                
!     shot  = the source location:                                      
!             shot(1) = source line #                                   
!             shot(2) = source sp #                                     
!     jbad  = a flag for a profile with "bad" headers                   
!           = 0, okay                                                   
!           = 1, bad                                                    
!     nskip = number of triplets in iskips                              
!     iskips= skip info as read in (may contain irrelevant skips)       
!             Triplet values: (1,*)=line#, (2,*)=1st stn, (3,*)=number  
!                                                                       
      loboadj=.FALSE. 
      lfile=-99999 
!                                                                       
!-get/check record size parameters                                      
   50 continue 
!        GET THE FILE NAME                                              
      read(ifnames,9001,END=8000,iostat=istat)FILEIN 
      write(icpr,9001)' Reading file ',filein 
      print*,' Reading file ',filein 
      if(istat.ne.0)then 
        print*,' Abort'
        print*,' Status reading header file name = ',istat 
        stop 
      endif 
!       open for read                                                   
      OPEN (UNIT=inp,iostat=istat,file=FILEIN,status='old')
      if(istat.ne.0)go to 777
       if(dbug) print *,'SD2FG-SEGD-dbug  decoded header follows' 
!          read the byte count card                                     
  100 continue 
      read(inp,9001,end=8001)cfile 
      k=index(cfile,'HEADER') 
      if(k.eq.0)then 
        print*,' First card is not the byte count card' 
        stop 
      endif 
!          Find the length on the header card                           
      k=index(cfile,':') 
      if(k.eq.0)then 
        print*,' Colon missing from byte count card' 
        stop 
      endif 
      ctemp=' '
      j=1 
      k=k+1 
      do while(cfile(k:k).eq.' ') 
        k=k+1 
      enddo 
      do while(cfile(k:k).ne.' ') 
        ctemp(j:j)=cfile(k:k) 
        k=k+1 
        j=j+1 
      enddo 
      call string_cc2ii(ctemp,len)
                                                                        
!          INCREMENT NUMBER PROFILES                                    
!------read  1st block (32-byte General Header)                         
!-clear all stored values in /SD2FG3/ beginning with jfile   
      jfile=0
      jset=0
      isets=0
      nline=0
      lines=0
      jbad=0
      nskip=0
      iskips=0
      shot=0.0           
      read(inp,9001,end=8001)cfile 
!                                                                       
!          CHECK THE MANUFACTURER'S CODE  byte 17                       
      read(cfile(33:34),9003)manuf 
      if(manuf.eq.18.or.manuf.eq.33)then 
!        Good format                                                    
      else if(manuf.eq.7)then 
        print*,' This is vision format - manufacture code 7' 
        print*,' This format does not contain enough information' 
        print*,'  to build rp and pp cards' 
        print*,' Sorry!' 
        stop 
      else 
        Print*,' ABORT - NOT IO Systems data' 
        Print*,' ABORT - NOT Compuseis data' 
        Print*,' Manufacturer''s code = ',manuf 
        stop 
      endif 
!                  COMPUSEIS DATA                                       
      if(manuf.eq.33)then 
        call sd2fg_compuseis(cfile,0) 
  125   read(inp,9001,end=130)cfile(1:7) 
        IF(cfile(2:7).eq.'HEADER')THEN 
          backspace inp 
          GO TO 100 
        ELSE 
          go to 125 
        ENDIF 
      endif 
      go to 140 
  130 continue 
      call sd2fg_compuseis(cfile,1) 
      go to 8001 
  140 continue 
!                                                                       
!------file# BCD bytes 1-2 of Gen. Head                                 
      read (cfile(1:4) ,'(i4)' ) jfile 
      IF(jfile.le.lfile)THEN 
        Print*,'WARNING---file numbers are not incrementing' 
        Write(icpr,9001)'WARNING---file numbers are not incrementing' 
      ENDIF 
      lfile=jfile 
!------#channel sets (32-bytes each) BCD byte 29 (hex 57,58)            
      read (cfile(57:58) ,'(i2)' ) jset 
                          !arbitrary limit                              
      if(jset.gt.300) then 
       errtyp='too many channel sets (gt.300)' 
                !fatal                                                  
       goto 999 
      end if 
!------#extn. hdr. blocks (32-bytes each) BCD byte 31                   
      read (cfile(61:62) ,'(i2)' ) nblk 
                                                                        
      if(dbug) write(icpr,*)'file#=',jfile,' #channel sets=',jset,      &
     &  ' #extnd. hdr. blks.=',nblk                                     
                                                                        
                                       !1st 32 is Gen. Header           
      nbytes = 32 + jset*32 + nblk*32 
      if(len .lt. nbytes) then 
        write(icpr,9001)'SD2FGDG Error on SEGD profile header byte count&
     &.'                                                                
        write(icpr,*)'   Record=',len,'  headers say ',nbytes 
      end if 
!-channel set descriptors BCD                                           
      nchans=0 
                     !look at each channel set descriptor (32-bytes)    
      DO ISET=1,JSET 
        read(inp,9001,end=8001)cfile 
!       ignore byte1 - will always be 01                                
                                        !set# BCD byte 2                
        read (cfile(3:4) ,'(i2)' ) kset 
                              !something wrong                          
        if(kset.ne.iset) then 
          errtyp='kset .ne. iset' 
          goto 998 
        end if 
                                           !#ch BCD bytes 9,10          
        read (cfile(17:20) ,'(i4)' ) kchan 
                           !total                                       
                                          ! seis=1, else aux.           
        read (cfile(21:21) ,'(Z1)' ) ktyp 
        isets(1,iset)=ktyp 
        isets(2,iset)=kchan 
      END DO 
!-extended general header BINARY                                        
                          !gen.hdr. + chn.set.desc. + extn.gen.hed      
      iblk = 1 + jset + 1 
      read(inp,9001,end=8001)cfile 
      k=index(cfile,'HEADER')
      if(k.ne.0)then
        print*,' Extended general header missing for file ',jfile
        write(icpr,*)' Extended general header missing for file ',jfile
        write(icpr,*)' File ',jfile,' skipped'
        backspace inp
        go to 100
      endif
                                                                        
                                        !# lines BINARY byte 3          
      read (cfile(5:6) ,'(z2)' ) nline 
                           !exceeds documented limit                    
      if(nline.gt.300) then 
       errtyp='more than 300 lines' 
       print*,' nline = ',nline 
                !fatal                                                  
       goto 999 
       end if 
                                        !#blks for line descr.          
      read (cfile(7:8) ,'(z2)' ,iostat=istat) nlinbl 
      if(istat.ne.0)then
        print*,' Read error abort - file ',jfile
        print*,' Status B = ',istat,' cfile = ',cfile(7:8)
        stop
      endif
                                        !#blks for ancillary info.      
      read (cfile(9:10),'(z2)' ) nancbl 
                                        !#blks for skip info.           
      read (cfile(11:12),'(z2)' ) nskpbl 
                                                                        
      if(dbug) then 
       write(icpr,*)'  from Extn.Gen.Hdr. #lines=',nline,' #lin.des.blk=&
     &',                                                                &
     &        nlinbl,' #ancl.des.blk=',nancbl,' #skip.des.blk=',nskpbl  
      end if 
                                                                        
                                         !# aux. channels               
      read (cfile(21:22) ,'(z2)' ) naux 
                                         !1st aux.                      
      read (cfile(23:24) ,'(z2)' ) naux1 
                                         !last aux.                     
      read (cfile(25:26) ,'(z2)' ) naux2 
                                                                        
      if(dbug) write(icpr,*)'    #aux=',naux,' 1st=',naux1,             &
     &        ' last=',naux2                                            
                                                                        
!-line descriptor headers BINARY (2 sets per 32-byte block)             
!     (info per rec. line: ch-sets, sta#s, gap channels,                
!      skip ch, & bad traces)                                           
!                                                                       
               !line counter                                            
      nldone=0 
                        !max is 16 lines                                
      DO IBLOK=1,NLINBL 
        iblk=iblk+1 
        read(inp,9001,end=8001)cfile 
        nldone=nldone+1 
                                           !line#                       
        read (cfile(5:8) ,'(z4)' ,iostat=istat) kline 
        if(istat.ne.0)then
          print*,' Status A = ',istat,' reading ',cfile(5:8)
          exit
        endif
                                           !#ch set this line           
        read (cfile(9:10) ,'(z2)' ) ncsets 
                                                                        
        if(ncsets.gt.0) then 
!         if(ncsets.gt.1) then                                          
!          write(icpr,9001)'**** %%WARNING%% #ch sets for line .GT. 1 **
!     X* A '                                                            
!                                                                       
!          print *,'**** %%WARNING%% #ch sets for line .GT. 1 ****'     
!          write(icpr,9001)'**** %%WARNING%% #ch sets for line .GT. 1 **
!     X*'                                                               
!off      print *,'SD2FG-SEGD-dbug  header follows'                     
!off      print 1001,((i-1)*16+1,jbuf(i),jbuf(i+1),i=1,len,2)           
!         end if                                                        
                                             !1st ch set                
         read (cfile(11:12) ,'(z2)' ) ncset1 
                                            !1st stn# this line         
         read (cfile(13:16) ,'(z4)' ) nsta1 
                                            !last stn#                  
         read (cfile(17:20) ,'(z4)' ) nsta2 
         if(dbug) print *,                                              &
     &   '  Unaltered receiver line=',kline,' 1st/last=',nsta1,nsta2    
         if(loboadj)then 
           nsta1=nsta1-kline+1 
           nsta2=nsta2-kline+1 
         endif 
                                 !# staions (may include gaps?)         
         nsta12 = nsta2-nsta1+1 
! **    gap info                                                        
                                            !1st stn# in gap            
         read (cfile(21:24) ,'(z4)' ) ngap1 
                                            !#  stns in gap             
         read (cfile(25:26) ,'(z2)' ) ngap 
                                                                        
                            !there is supposed to be gap                
         if(ngap.gt.0) then 
           if(loboadj)ngap1=ngap1-kline+1 
                                 !last stn # in gap                     
           ngap2 = ngap1+ngap-1 
!    make sure gap is fully contained within live spread                
                                                       !gap is off end  
           if(ngap1.le.nsta1 .or. ngap2.ge.nsta2) then 
             print *,'**** %%WARNING%% gap in line ',kline,' ignored' 
             print *,'                 gap from ',ngap1,' to ',ngap2 
             print *,'**** %%WARNING%% gap in line ',kline,' ignored' 
             print *,'                 gap from ',ngap1,' to ',ngap2 
             write(icpr,9001)'**** %%WARNING%% gap in line ',kline,     &
     &' ignored'                                                        
             write(icpr,9001)'                 gap from ',ngap1,' to ', &
     &ngap2                                                             
             write(icpr,9001)'**** %%WARNING%% gap in line ',kline,     &
     &' ignored'                                                        
             write(icpr,9001)'                 gap from ',ngap1,' to ', &
     &ngap2                                                             
             ngap=0 
             ngap1=0 
           else 
                                  !# stations (gaps not included)       
             nsta12= nsta12-ngap 
           end if 
         end if 
! **    get total from ch set header                                    
         nchk=isets(2,ncset1) 
         if(ncsets.gt.1) then 
          do iiss=2,ncsets 
           nchk=nchk+isets(2,ncset1+iiss-1) 
          end do 
         end if 
! **     check for skips                                                
!        (nskip says if info available, ngapd says we need some)        
                                            !#  gap descr. ???????      
         read (cfile(27:28) ,'(z2)' ) ngapd 
         if(ngapd.gt.0 .AND. nskip.gt.0) then 
          call SD2FG_d1                                                  &
     &         (kline/100,nsta1,nsta2,nchk,ngap,ngap1,numskp,ier)       
          if(ier.gt.0) then 
           goto 998 
          else 
                                !exclude any skips from stn count       
           nsta12=nsta12-numskp 
          end if 
         else 
          numskp=0 
         end if 
! **    check total here against value from ch set header               
                                 !accounting problem                    
         if(nchk.ne.nsta12) then 
          write(icpr,*)' A here' 
          write(icpr,*)'**** %%WARNING%% mismatch #st line vs. ch set' 
          write(icpr,*)'                 line=',nsta12,' ch set=',nchk 
          print*,'**** %%WARNING%% mismatch #st line vs. ch set' 
          print*,'                 line=',nsta12,' ch set=',nchk 
          if(nsta12.lt.0)then 
            write(icpr,*)' ch set used' 
            nsta12=nchk 
          endif 
          errtyp='#stations from line .ne. ch set' 
!cc       goto 998 !skip                                                
         end if 
         if(loboadj)kline=kline/100 
         lines(1,nldone)=kline 
         lines(2,nldone)=ncsets 
         lines(3,nldone)=ncset1 
         lines(4,nldone)=nsta1 
         lines(5,nldone)=nsta2 
         lines(6,nldone)=nsta12 
         lines(7,nldone)=ngap 
         lines(8,nldone)=ngap1 
         lines(9,nldone)=numskp 
             !ncsets<1 no traces this line!!!                           
        else 
         if(loboadj)kline=kline/100 
         lines(1,nldone)=kline 
         write(icpr,*)'**** %%WARNING%% ZERO traces for line=',kline 
        end if 
                                                                        
        if(dbug) then 
         write(icpr,*)'   line info: line=',kline,' #csets=',ncsets,    &
     &          ' 1st set=',ncset1,' 1st stn=',nsta1,' last=',nsta2,    &
     &          ' total=',nsta12                                        
         if(ngap.gt.0)                                                  &
     &   write(icpr,*)'        #stn in gap=',ngap,' 1st=',ngap1         
        end if 
                                                                        
! **    bad info -- not sure how to use these values                    
                                           !#  bad tr. descr. ???????   
        read (cfile(29:30) ,'(z2)' ) nbad 
        if(nbad.gt.0) then 
         print *,'**** %%WARNING%% bad trace info present ****' 
         print *,'SD2FG-SEGD-dbug  header follows' 
!fix     print 1001,((i-1)*16+1,jbuf(i),jbuf(i+1),i=1,len,2)            
        end if 
                                 !2nd line in block if not finished     
        if(nldone.lt.nline) then 
          nldone=nldone+1 
          read (cfile(33:36) ,'(z4)' ) kline 
          read (cfile(37:38) ,'(z2)' ) ncsets 
                                                                        
          if(ncsets.gt.0) then 
           if(ncsets.gt.1) then 
            print *,'**** %%WARNING%% #ch sets for line .GT. 1 ****' 
!off        print *,'SD2FG-SEGD-dbug  header follows'                   
!off        print 1001,((i-1)*16+1,jbuf(i),jbuf(i+1),i=1,len,2)         
           end if 
           read (cfile(39:40) ,'(z2)' ) ncset1 
           read (cfile(41:44) ,'(z4)' ) nsta1 
           read (cfile(45:48) ,'(z4)' ) nsta2 
           if(dbug) print *,                                            &
     &   '  Unaltered receiver line=',kline,' 1st/last=',nsta1,nsta2    
           if(loboadj)then 
             nsta1=nsta1-kline+1 
             nsta2=nsta2-kline+1 
           endif 
                                  !#stns (may include gap?)             
           nsta12 = nsta2-nsta1+1 
! **      gap info                                                      
                                              !1st stn# in gap          
           read (cfile(49:52) ,'(z4)' ) ngap1 
                                              !#  stns in gap           
           read (cfile(53:54) ,'(z2)' ) ngap 
                                                                        
                            !there is supposed to be gap                
         if(ngap.gt.0) then 
           if(loboadj)ngap1=ngap1-kline+1 
                                 !last stn # in gap                     
           ngap2 = ngap1+ngap-1 
!    make sure gap is fully contained within live spread                
                                                       !gap is off end  
           if(ngap1.le.nsta1 .or. ngap2.ge.nsta2) then 
             write(icpr,*)'**** %%WARNING%% gap in line ',kline,' ignore&
     &d'                                                                
             write(icpr,*)'                 gap from ',ngap1,' to ',ngap&
     &2                                                                 
             ngap=0 
             ngap1=0 
           else 
                                  !# stations (gaps not included)       
             nsta12= nsta12-ngap 
           end if 
         end if 
! **      get total from ch set header                                  
           nchk=isets(2,ncset1) 
           if(ncsets.gt.1) then 
            do iiss=2,ncsets 
             nchk=nchk+isets(2,ncset1+iiss-1) 
            end do 
           end if 
! **     check for skips                                                
!        (nskip says if info available, ngapd says we need some)        
                                            !#  gap descr. ???????      
         read (cfile(55:56) ,'(z2)' ) ngapd 
         if(ngapd.gt.0 .AND. nskip.gt.0) then 
          call SD2FG_d1                                                  &
     &         (kline/100,nsta1,nsta2,nchk,ngap,ngap1,numskp,ier)       
          if(ier.gt.0) then 
           goto 998 
          else 
                                !exclude any skips from stn count       
           nsta12=nsta12-numskp 
          end if 
         else 
          numskp=0 
         end if 
                                                                        
! **      check total here against value from ch set header             
                                   !accounting problem                  
           if(nchk.ne.nsta12) then 
            write(icpr,*)' B here' 
            write(icpr,*)'**** %%WARNING%% mismatch #st line vs. ch set' 
            write(icpr,*)'                 line=',nsta12,' ch set=',nchk 
            write(icpr,*)'                 ch set used' 
!---  Don Roy's lobo data has negative nsta12 - use nchk                
            if(nsta12.lt.0)nsta12=nchk 
            errtyp='#stations from line .ne. ch set' 
!cc         goto 998 !skip                                              
           end if 
           if(loboadj)kline=kline/100 
           lines(1,nldone)=kline 
           lines(2,nldone)=ncsets 
           lines(3,nldone)=ncset1 
           lines(4,nldone)=nsta1 
           lines(5,nldone)=nsta2 
           lines(6,nldone)=nsta12 
           lines(7,nldone)=ngap 
           lines(8,nldone)=ngap1 
           lines(9,nldone)=numskp 
               !ncsets<1 no traces this line!!!                         
          else 
           if(loboadj)kline=kline/100 
           lines(1,nldone)=kline 
           write(icpr,*)'**** %%WARNING%% ZERO traces for line=',kline 
          end if 
                                                                        
          if(dbug) then 
           write(icpr,*)'   line info: line=',kline,' #csets=',ncsets,  &
     &          ' 1st set=',ncset1,' 1st stn=',nsta1,' last=',nsta2,    &
     &          ' total=',nsta12                                        
           if(ngap.gt.0)                                                &
     &     write(icpr,*)'              #stn in gap=',ngap,' 1st=',ngap1 
          end if 
                                                                        
! **      bad info -- not sure how to use these values                  
                                             !#  bad tr. descr. ??????? 
          read (cfile(57:58) ,'(z2)' ) nbad 
          if(nbad.gt.0) then 
           print *,'**** %%WARNING%% bad trace info present ****' 
           print *,'SD2FG-SEGD-dbug  header follows' 
!fix       print 1001,((i-1)*16+1,jbuf(i),jbuf(i+1),i=1,len,2)          
          end if 
        end if 
      END DO 
!-ancillary data headers BINARY                                         
      if(data_sheets)then 
        shot(1)=shot1(jfile) 
        shot(2)=shot2(jfile) 
!          position file to next byte count card                        
  150   read(inp,9001,end=200)cfile(1:6) 
        if(cfile(1:6).eq.'HEADER')THEN 
          backspace inp 
          go to 200 
        else 
          go to 150 
        endif 
      endif 
! ** we got # of blocks in gen.extn.hdr, but appears to alway be 2!!!!! 
                  !skip header block 1                                  
      iblk=iblk+1 
      read(inp,9001,end=8001)cfile 
                  !header block 2 has shot info                         
      iblk=iblk+1 
      read(inp,9001,end=8001)cfile 
      kk=index(cfile,'HEADER') 
      if(kk.ne.0)then 
        backspace inp 
        go to 100 
      endif 
                                          !source line#                 
      read (cfile(9:12) ,'(z4)' ) ksline 
                                          !source sp#                   
      read (cfile(13:16) ,'(z4)' ) kssta 
      if(dbug) write(icpr,*)                                            &
     &' Unaltered shot info: line=',ksline,' stn=',kssta                
      if(loboadj)then 
        kssta=kssta-ksline+1 
        ksline=ksline/10 
      endif 
                                           !line offset                 
      read (cfile(17:18) ,'(z2)' ) kslineo 
                                           !sp offset                   
      read (cfile(19:20) ,'(z2)' ) ksstao 
                                          !#channels (incl. aux.)       
      read (cfile(21:24) ,'(z4)' ) kchtot 
      shot(1) = float(ksline)+float(kslineo)/100. 
      shot(2) = float(kssta)+float(ksstao)/100. 
  175 continue 
      if(long_bytes)then 
!          position file to next byte count card                        
        read(inp,9001,end=200)cfile 
        kk=index(cfile,'HEADER') 
        IF(kk.ne.0)then 
          backspace inp 
        else 
          go to 175 
        endif 
      endif 
  200 continue 
      if(dbug) then 
       write(icpr,*)'   shot info: line/offset=',ksline,'/',kslineo,    &
     &        ' stn/offset=',kssta,'/',ksstao,' #ch=',kchtot            
                                                                        
       write(icpr,*)'SD2FG-SEGD-dbug-end' 
      end if 
!-skips headers not being processed for now (they come after ancillary) 
!     THERE IS A WARNING IF THEY ARE PRESENT                            
!--We will read skip info and store it whether or not it gets used      
!     - comment out skip stuff                                          
!cc   if(nskpbl.gt.0) then !store skip info for later                   
!cc    call SD2FGds(nlinbl,nancbl,nskpbl)                               
!cc   end if                                                            
!                                                                       
!-----Report summary of header info here (regardless of dbug value)     
!c    write(icpr,*)'&&This will be final summary for production runs&&' 
      write(icpr,*)'&&' 
      write(icpr,*)'&&  SEGD input file header' 
      write(icpr,9005)jfile,shot(1),shot(2),kchtot 
      do nl=1,nline 
       write(icpr,9006)lines(1,nl),lines(4,nl),lines(5,nl),lines(6,nl) 
       trtot=trtot+lines(6,nl) 
                                 !there is a gap                        
       if(lines(7,nl).gt.0) then 
       write(icpr,*)'&&&                       Gap of ',lines(7,nl),    &
     & ' beginning at stn# ',lines(8,nl)                                
       end if 
                                 !there are skips                       
       if(lines(9,nl).gt.0) then 
       write(icpr,*)'&&&                       Also ',lines(9,nl),      &
     & ' skipped stns. '                                                
       end if 
      end do 
!c    write(icpr,*)'&&'                                                 
!c    write(icpr,*)'&&      End of final summary for production runs&&' 
!---put FG cards in online unless this is test shot                     
!                                                                       
                                                  !write cards          
      if(shot(1).ne.0.0 .or. shot(2).ne.0.0) then 
      if(lines(4,1).gt.9999.or.lines(4,1).lt.0)then 
        print*,' line number is corrupt = ',lines(4,1) 
        print*,' line number reset to zero' 
        lines(4,1)=0 
      endif 
       write(ipp,1004)     shot(2) , int(shot(1)), lines(4,1),          &
     &              lines(1,1), jfile, jfile                            
      numprof=numprof+1 
!                                                                       
!                                                                       
!1002 format  (' <',f6.2,1x,1x,i4,2x,i4,3x,1x,i4,2x,i4,41x,2x,i4,1x,    
! 1002 format  (' <',f7.2,   1x,i4,2x,i4,3x,1x,i4,2x,i4,41x,2x,i4,1x,    &
!     &' &&FG_PP')                                                       
!                 !  ------- ----- -------- ----- ----- --- --------     
!                   SP2     LINE2    SP3   LINE3 IPAT2        IG        
!                   a7      a5       a9    a5    a6           a7        
!                   *7      *4       *7    *4    *3           *5        
!                                            need *4                    
!                                                                       
       do nl=1,nline 
        if(lines(6,nl).gt.0) then 
                          !if lines are not in channel set order abort  
         if(nl.gt.1) then 
           if(lines(6,nl-1).gt.0) then 
             if(lines(3,nl).le.lines(3,nl-1)) then 
               print*,'SD2FG: lines not in channel set order' 
               write(icpr,9001)'SD2FG: lines not in channel set order' 
               go to 8001 
             end if 
           end if 
         end if 
                                                          !no gap,skips 
         if(lines(7,nl).eq.0 .AND. lines(9,nl).eq.0) then 
           write(irp,1005) jfile, float(lines(4,nl)), lines(1,nl),      &
     &              lines(6,nl),1,1,1,0.,0.,0.                          
                                        !gap only (no skip)             
         else if(lines(9,nl).eq.0) then 
                                  !1st stn this card                    
           nsta1  = lines(4,nl) 
                                  !last stn this card                   
           nsta2  = lines(8,nl)-1 
           nsta12 = nsta2-nsta1+1 
           write(irp,1005) jfile, float(nsta1), lines(1,nl),            &
     &              nsta12,1,1,1,0.,0.,0.                               
           nsta1  = lines(8,nl)+lines(7,nl) 
           nsta2  = lines(5,nl) 
           nsta12 = nsta2-nsta1+1 
           write(irp,1005) jfile, float(nsta1), lines(1,nl),            &
     &              nsta12,1,1,1,0.,0.,0.                               
              !skips (with or w/o gaps)                                 
         else 
           call SD2FG_d2(nl,ilist,nseg) 
           do ns=1,nseg 
             nsta1  = ilist(1,ns) 
             nsta2  = ilist(2,ns) 
             nsta12 = nsta2-nsta1+1 
             write(irp,1005) jfile, float(nsta1), lines(1,nl),          &
     &              nsta12,1,1,1,0.,0.,0.                               
           end do 
         end if 
         numrp=numrp+1 
             !missing traces for this line                              
        else 
         write(irp,'(a)') 
        end if 
       end do 
!                                                                       
 1003  format(' <',1x,i4,3x,'X   ',f9.2,i6,i7,i6,i8,i7,f9.0,f8.0,f8.0,  &
     &' &&FG_RP')                                                       
!---put FG cards in file                                                
       write(cards(1),1004,iostat=istat)shot(2),int(shot(1)),lines(4,1),&
     &              lines(1,1), jfile, jfile                            
       if(istat.ne.0)then 
         if(istat.eq.63)print*,' Output conversion error' 
         print*,' Error status = ',istat 
         go to 8500 
       endif 
       ncrd=1 
                                                                        
!1004  format  (f6.2,1x,1x,i4,2x,i4,3x,1x,i4,2x,i4,41x,2x,i4,1x,'PP')   
 1004  format  (f7.2,   1x,i4,2x,i4,3x,1x,i4,2x,i4,41x,2x,i4) 
!               ------- ----- -------- ----- ----- --- --------         
!               SP2     LINE2    SP3   LINE3 IPAT2        IG            
!               a7      a5       a9    a5    a6           a7            
!               *7      *4       *7    *4    *3           *5            
!                                          need *4                      
!                                                                       
!                                                                       
 1005  format(1x,i4,3x,'X   ',f9.2,i6,i7,i6,i8,i7,f9.0,f8.0,f8.0) 
!                                                                       
           !shot location zeros, treat as aux.                          
      else 
       print *,'**** %%WARNING%% all traces flagged aux. ****' 
       print *,'**** because source location zero,zero   ****' 
       print *,'**** NO cards written for FG             ****' 
       write(icpr,9001)'**** %%WARNING%% all traces flagged aux. ****' 
       write(icpr,9001)'**** because source location zero,zero   ****' 
       write(icpr,9001)'**** NO cards written for FG             ****' 
       do iset=1,jset 
                        !flag all traces as aux for deletion            
        isets(1,iset)=0 
       end do 
      end if 
!          SAVE SOME INFO TO BE PUT IN TRACE HEADER                     
      CALL SD2FG_AUX 
!                                                                       
      go to 100 
 8001 continue 
      close(unit=inp) 
      go to 50 
 8000 endfile irp 
      endfile ipp 
      rewind irp 
      rewind ipp 
      if(manuf.eq.18)then 
        write(icpr,9007)numprof 
        print*,' number profiles = ',numprof 
        print*,' headers for ',trtot,' traces' 
        write(icpr,*)' headers for ',trtot,' traces' 
      endif 
      close(unit=ifnames,status='delete') 
!          WRITE JD HEADER CARDS                                        
      write(irppp,9001)ijdhead 
      call string_date(ctemp10) 
      kdate=' '
      kdate(1:2)=ctemp10(9:10)
      ctemp=' '
      ctemp(1:2)=ctemp10(6:7)
      select case (ctemp)
        case ('01')
          mth='JAN'
        case ('02')
          mth='FEB'
        case ('03')
          mth='MAR'
        case ('04')
          mth='APR'
        case ('05')
          mth='MAY'
        case ('06')
          mth='JUN'
        case ('07')
          mth='JUL'
        case ('08')
          mth='AUG'
        case ('09')
          mth='SEP'
        case ('10')
          mth='OCT'
        case ('11')
          mth='NOV'
        case ('12')
          mth='DEC'
      end select
      kdate(4:6)=mth
      kdate(8:9)=ctemp10(3:4)
      kdate(3:3)='-'
      kdate(7:7)='-'
      
      call string_time(ktime) 
      write(irppp,9009)kdate,ktime,numrp,numprof 
      write(irppp,9001)ildhead 
      write(irppp,9001)dumld 
      write(irppp,9001)irphead 
 8005 read(irp,9001,end=8010)card 
      write(irppp,9001)card 
      go to 8005 
 8010 continue 
!cc   close(unit=irp,status='delete')                                   
      write(irppp,9001)ipphead 
 8015 read(ipp,9001,end=8020)card 
      write(irppp,9001)card 
      go to 8015 
 8020 CONTINUE 
!cc   close(unit=ipp,status='delete')                                   
      endfile irppp 
      return 
 8500 print*,' error writing rp cards--abort' 
      stop 
 9001 format(A) 
! 9002 format(1x,A6,13X,I6) 
 9003 format(I2) 
! 9004 format(A7,A5,A9,A5,A6,41X,A7) 
                                                                        
 9005 format('&&& Field file#=',I6,' source (line,sp) = ',F6.0,          &
     &' , ',F7.0,' #channels = ',I6)                                    
 9006 format('&&&   Receiver line = ',I6,' 1st = ',I6,                  &
     &' last = ',I6,' (',I6,')')                                        
 9007 format(' Number profiles = ',I10) 
 9008 format('** NO CARDS WRITTEN FOR FILE # ',I7,' **',/) 
 9009 format(2x,A9,1X,A8,3X,'SLOP',8X,'1',2X,I5,3X,I4,4(6X,'0')) 
!                                                                       
  777 CONTINUE 
      PRINT*,' ERROR OPENING FILE' 
      STOP 
!-come here to ignore this profile header                               
!                                                                       
  998 CONTINUE 
      print *,'****SD2FGDG '//errtyp//' -- not supported ****' 
      print *,'** NO CARDS WRITTEN FOR FILE #',jfile,' **' 
      write(icpr,9001)'****SD2FGDG '//errtyp//' -- not supported ****' 
      write(icpr,9008)jfile 
      jbad=1 
      numprof=numprof-1 
!        find the next header card                                      
 8600 CONTINUE 
      read(inp,9001,end=8001)cfile 
      kk=index(cfile,'HEADER') 
      IF(kk.ne.0)THEN 
        backspace inp 
        GO TO 100 
      ELSE 
        go to 8600 
      ENDIF 
!                                                                       
!-stop here if find significant info. not currently handled             
!                                                                       
  999 print *,'**** '//errtyp//' -- not supported ****' 
      write(icpr,*)'**** '//errtyp//' -- not supported ****' 
      print*,' file number = ',jfile 
      write(icpr,*)' file number = ',jfile 
      print *,'SD2FG-SEGD-dbug  header follows' 
!fix  print 1001,((i-1)*16+1,jbuf(i),jbuf(i+1),i=1,len,2)               
      print*,'SD2FG-SEGD-file_header input failed' 
      write(icpr,9001)'SD2FG-SEGD-zfile_header input failed' 
      END subroutine SD2FG_END                                          
      SUBROUTINE SD2FG_ds(nlinbl,nancbl,nskpbl) 
!                                                                       
!     routine to get skip info. pertaining to this profile.             
!                                                                       
!     nlinbl = # 32-byte blocks for line info (for indexing)            
!     nancbl = # 32-byte blocks for ancillary info (for indexing)       
!     nskpbl = # 32-byte blocks for skip info (max 10 blks = 50 items)  
!                                                                       
!-----info saved from SEGD file header (except for 1st 3 items)         
!-----  itf=trace counter in "file", ifh=flag for 1st file              
!-----  iff=file counter                                                
!     jfile = field file#                                               
!     jset  = #channel sets                                             
!     isets = channel set info:                                         
!            isets(1,*)=1, for seis --- else aux.                       
!            isets(2,*)=#tr in this set                                 
!     nline = #lines (# sets info in lines)                             
!     lines = line info:                                                
!             lines(1,*)=line#                                          
!             lines(2,*)=#ch sets this line                             
!             lines(3,*)=1st ch set# this line                          
!             lines(4,*)=1st station # this line                        
!             lines(5,*)=last station # this line                       
!             lines(6,*)=# stations for this line                       
!             lines(7,*)=# stations in gap                              
!             lines(8,*)=1st station in gap                             
!             lines(9,*)=# of skipped stations this line                
!     shot  = the source location:                                      
!             shot(1) = source line #                                   
!             shot(2) = source sp #                                     
!     jbad  = a flag for a profile with "bad" headers                   
!           = 0, okay                                                   
!           = 1, bad                                                    
!     nskip = number of triplets in iskips                              
!     iskips= skip info as read in (may contain irrelevant skips)       
!             Triplet values: (1,*)=line#, (2,*)=1st stn, (3,*)=number  
!                                                                       
      integer :: i1,i2,iblk,iskbl,iskip
      integer :: j1,j2,k1,k2,nleft,NANCBL,nlinbl,NSKPBL 
      character(len=64) :: cfile
      character(len=32) :: errtyp                                        
                                                                        
                                                                        
!-compute index to skip decriptors                                      
                          !gen.hdr. + chn.set.desc. + extn.gen.hed      
      iblk = 1 + jset + 1 
                                     ! + line.info + ancillary          
      iblk = iblk + nlinbl + nancbl 
      nskip=0 
                        !loop over skip blocks                          
      DO ISKBL=1,nskpbl 
                       !next skip block                                 
       iblk = iblk + 1 
       read(inp,9001,end=8000)cfile 
!cc    write(cfile(1:64),'(4z16)') (ibuf(i,iblk),i=1,4)                 
                                         !#skips remaining              
       read (cfile(1:2) ,'(z2)' ) nleft 
       i1=5 
       i2=i1+3 
       j1=i2+1 
       j2=j1+3 
       k1=j2+1 
       k2=k1+3 
                               !max 5 skips per card                    
       do iskip=1,min(5,nleft) 
         nskip=nskip+1 
         if(nskip.gt.50)then 
           print*,'SD2FG_DS - too many skips' 
           write(icpr,9001)'SD2FG_DS ABORT- too many skips' 
           stop 
         endif 
                                                      !line# for skip   
         read (cfile(i1:i2) ,'(z4)' ) iskips(1,nskip) 
                                                      !1st stn# for skip
         read (cfile(j1:j2) ,'(z4)' ) iskips(2,nskip) 
                                                      !# stns. in skip  
         read (cfile(k1:k2) ,'(z4)' ) iskips(3,nskip) 
         if(loboadj)then 
           iskips(2,nskip) = iskips(2,nskip)-iskips(1,nskip)+1 
           iskips(1,nskip) = iskips(1,nskip)/100 
         endif 
         i1=i1+12 
         i2=i2+12 
         j1=j1+12 
         j2=j2+12 
         k1=k1+12 
         k2=k2+12 
       end do 
             !loop over skip blocks                                     
      END DO 
                                                                        
!dbug loop only                                                         
!dbug do i=1,nskip !cdbug                                               
!dbug    PRINT *,                                                       
!dbug+   'cdbug skip info: line=',iskips(1,i),' 1st/num=',iskips(2,i),  
!dbug+   iskips(3,i)                                                    
!dbug end do                                                            
                                                                        
 8000 continue 
      return 
 9001 format(A) 
      END subroutine SD2FG_ds                                           
!CC                                                                     
      SUBROUTINE SD2FG_d1(kline,nsta1,nsta2,nchk,ngap,ngap1,NUM,ier) 
!-----789012345678901234567890123456789012345678901234567890123456789012
!                                                                       
!     routine to get skip info. pertaining to this line.                
!                                                                       
!     kline  = current receiver line                                    
!     nsta1  = 1st stn this line                                        
!     nsta2  = last stn this line                                       
!     nchk   = # stns this line (from ch set descr)                     
!     ngap   = # stns in the gap                                        
!     ngap1  = 1st stn in the gap                                       
!     NUM    = # skips found here (RETURNED)                            
!     ier    = error flag for problems here                             
!            = 0, okay                                                  
!            = 1, error                                                 
!                                                                       
!-----info saved from SEGD file header (except for 1st 3 items)         
!-----  itf=trace counter in "file", ifh=flag for 1st file              
!-----  iff=file counter                                                
!     jfile = field file#                                               
!     jset  = #channel sets                                             
!     isets = channel set info:                                         
!            isets(1,*)=1, for seis --- else aux.                       
!            isets(2,*)=#tr in this set                                 
!     nline = #lines (# sets info in lines)                             
!     lines = line info:                                                
!             lines(1,*)=line#                                          
!             lines(2,*)=#ch sets this line                             
!             lines(3,*)=1st ch set# this line                          
!             lines(4,*)=1st station # this line                        
!             lines(5,*)=last station # this line                       
!             lines(6,*)=# stations for this line                       
!             lines(7,*)=# stations in gap                              
!             lines(8,*)=1st station in gap                             
!             lines(9,*)=# of skipped stations this line                
!     shot  = the source location:                                      
!             shot(1) = source line #                                   
!             shot(2) = source sp #                                     
!     jbad  = a flag for a profile with "bad" headers                   
!           = 0, okay                                                   
!           = 1, bad                                                    
!     nskip = number of triplets in iskips                              
!     iskips= skip info as read in (may contain irrelevant skips)       
!             Triplet values: (1,*)=line#, (2,*)=1st stn, (3,*)=number  

      integer :: ier,i1,i2,i1g,i2g,kline,ksip
      integer :: nchk,ngap,ngap1,ns,ns2,nsta1,nsta2,ntest,num
!                                                                       
            !initialize result                                          
      NUM=0 
      ier=0 
!                                                                       
      i1=nsta1 
      i2=nsta2 
      if(ngap.gt.0) then 
       i1g=ngap1 
       i2g=ngap1+ngap-1 
      end if 
                    !search each skip triplet                           
      DO NS=1,NSKIP 
                                      !found skip(s) for this line      
       if(iskips(1,ns).eq.kline) then 
!-------skips found may be out of current range                         
        do ns2=1,iskips(3,ns) 
         ksip = iskips(2,ns)+ns2-1 
                                              !this skip is in range    
         if(ksip.ge.i1 .and. ksip.le.i2) then 
                             !see if skip is in gap -- error?           
          if(ngap.gt.0) then 
           if(ksip.ge.i1g .and. ksip.le.i2g) then 
            write(icpr,*)'SD2FG_d1 - skip station is in gap!!' 
            ier=1 
            return 
           end if 
          end if 
          NUM=NUM+1 
!dbug     write(icpr,*)'found skip for line=',kline,' stn=',ksip !cdbug 
         end if 
               !ns2                                                     
        end do 
       end if 
             !NS                                                        
      END DO 
      if(num.gt.0) then 
                              !number of stns if no gap nor skips       
        ntest = nsta2-nsta1+1 
        if( (ntest-ngap) .eq. nchk) then 
!        1         2         3         4         5         6         7  
!23456789012345678901234567890123456789012345678901234567890123456789012
         write(icpr,*)'SD2FG_d1: There should not be any skips for line '&
     &,  kline, ' but we found ',num                                    
         ier=1 
         return 
        end if 
      end if 
      return 
      END subroutine SD2FG_d1                                          
!CC                                                                     
      SUBROUTINE SD2FG_d2(nl,ilist,nseg) 
!-----789012345678901234567890123456789012345678901234567890123456789012
!                                                                       
!     routine to get segments for this line.                            
!                                                                       
!     nl     = index for lines array (i.e. which line)                  
!     ilist  = 2-dimesional list of line segments                       
!              ilist(1,*) = 1st stn. in segment                         
!              ilist(2,*) = last stn. in segment                        
!     nseg   = # of segments                                            
!                                                                       
!-----info saved from SEGD file header (except for 1st 3 items)         
!-----  itf=trace counter in "file", ifh=flag for 1st file              
!-----  iff=file counter                                                
!     jfile = field file#                                               
!     jset  = #channel sets                                             
!     isets = channel set info:                                         
!            isets(1,*)=1, for seis --- else aux.                       
!            isets(2,*)=#tr in this set                                 
!     nline = #lines (# sets info in lines)                             
!     lines = line info:                                                
!             lines(1,*)=line#                                          
!             lines(2,*)=#ch sets this line                             
!             lines(3,*)=1st ch set# this line                          
!             lines(4,*)=1st station # this line                        
!             lines(5,*)=last station # this line                       
!             lines(6,*)=# stations for this line                       
!             lines(7,*)=# stations in gap                              
!             lines(8,*)=1st station in gap                             
!             lines(9,*)=# of skipped stations this line                
!     shot  = the source location:                                      
!             shot(1) = source line #                                   
!             shot(2) = source sp #                                     
!     jbad  = a flag for a profile with "bad" headers                   
!           = 0, okay                                                   
!           = 1, bad                                                    
!     nskip = number of triplets in iskips                              
!     iskips= skip info as read in (may contain irrelevant skips)       
!             Triplet values: (1,*)=line#, (2,*)=1st stn, (3,*)=number  
!                                                                       
      integer :: i,ilist(2,50),ist,isw,ixt,i1,i2,i1g,i2g,j
      integer :: ka,kline,klist(50),ksip,last,ngap,ngap1,nl,ns,ns2,nseg,num,nxt


                  !true when in a segment                               
      logical :: iam 
            !initialize skip count                                      
      NUM=0 
!                                                                       
      kline=lines(1,nl) 
      i1=   lines(4,nl) 
      i2=   lines(5,nl) 
      ngap= lines(7,nl) 
      ngap1=lines(8,nl) 
      if(ngap.gt.0) then 
       i1g=ngap1 
       i2g=ngap1+ngap-1 
      end if 
                    !search each skip triplet                           
      DO NS=1,NSKIP 
                                      !found skip(s) for this line      
       if(iskips(1,ns).eq.kline) then 
!-------skips found may be out of current range                         
        do ns2=1,iskips(3,ns) 
         ksip = iskips(2,ns)+ns2-1 
                                              !this skip is in range    
         if(ksip.ge.i1 .and. ksip.le.i2) then 
                             !see if skip is in gap -- error?           
          if(ngap.gt.0) then 
           if(ksip.ge.i1g .and. ksip.le.i2g) then 
            print*,'SD2FG_d2 ABORT - skip station is in gap!!' 
                                                                  !!'   
            WRITE(icpr,9001)'SD2FG_d2 ABORT - skip station is in gap '
            stop 
           end if 
          end if 
          NUM=NUM+1 
          klist(num)=ksip 
!dbug     write(icpr,*)'found skip for line=',kline,' stn=',ksip !cdbug 
         end if 
               !ns2                                                     
        end do 
       end if 
             !NS                                                        
      END DO 
!---check # of skips                                                    
      if(num.ne.lines(9,nl)) then 
        print *,'SD2FG_d2: Error, got diff. # skips than SD2FGd1.' 
        print *,'SD2FG_d1=',lines(9,nl),' SD2FG_d2=',num 
        print*,'abort - punt for now' 
        write(icpr,9001)'SD2FG_d2: Error, got diff. # skips than SD2FG_d1.&
     &'                                                                 
        write(icpr,9001)'SD2FG_d1=',lines(9,nl),' SD2FG_d2=',num 
        write(icpr,9001)'abort - punt for now' 
        stop 
      end if 
!---Add gap stns to skips list                                          
      if(ngap.gt.0) then 
        do i=1,ngap 
          num=num+1 
          klist(num)=ngap1+i-1 
        end do 
      end if 
                         !arbitrary dimension                           
      if(num.gt.50) then 
        print *,'SD2FG_d2: too many skips = ',num 
        write(icpr,9001)'arbitrary limit 50 per line' 
        stop 
      else if(num.ge.2) then 
!----------sort the skips  (bubble sort).                               
        do j=1,num 
         isw=0 
         do 30 i=2,num 
          if (klist(i).gt.klist(i-1)) go to 30 
          ka=klist(i) 
          klist(i)=klist(i-1) 
          klist(i-1)=ka 
          isw=1 
   30    continue 
         if (isw.eq.0) go to 50 
        end do 
   50   continue 
      end if 
!----build list of segments                                             
!                                                                       
      nxt=1 
      ixt=klist(nxt) 
      ist=i1 
      last=i1 
      iam=.true. 
      nseg=0 
      do i=i1+1,i2 
                           !use i                                       
         if(ixt.gt.i) then 
                        !add to seg.                                    
           if(iam) then 
             last=i 
                        !start seg.                                     
           else 
             iam=.true. 
             ist=i 
             last=i 
           end if 
                                !don't use i                            
         else if(ixt.eq.i) then 
                        !save seg., initialize search for next          
           if(iam) then 
             nseg=nseg+1 
             ilist(1,nseg)=ist 
             ilist(2,nseg)=last 
!dbug        print *,'seg=',ist,',',last !cdbug                         
             iam=.false. 
                        !still in gap, do nothing                       
           else 
           end if 
           nxt=nxt+1 
           if(nxt.le.num) then 
             ixt=klist(nxt) 
           else 
                      !won't hit this                                   
             ixt=i2+1 
           end if 
              !shoudln't get here                                       
         else 
           stop 'oops' 
         end if 
      end do 
                   !finish pending seg.                                 
      if(iam) then 
        nseg=nseg+1 
        ilist(1,nseg)=ist 
        ilist(2,nseg)=last 
!dbug   print *,'last seg=',ist,',',last !cdbug                         
      end if 
      return 
 9001 FORMAT(A) 
      END subroutine SD2FG_d2                                          
!CC                                                                     
      SUBROUTINE SD2FG_d3(nl,iknt,istn,ier) 
!-----789012345678901234567890123456789012345678901234567890123456789012
!                                                                       
!     routine to get station for this trace.                            
!                                                                       
!     nl     = index for lines array (i.e. which line)                  
!     iknt   = count for trace # within line                            
!     istn   = station# (returned)                                      
!     ier    = error flag (returned)                                    
!            =0,okay    =1,couldn't get istn                            
!                                                                       
!-----info saved from SEGD file header (except for 1st 3 items)         
!-----  itf=trace counter in "file", ifh=flag for 1st file              
!-----  iff=file counter                                                
!     jfile = field file#                                               
!     jset  = #channel sets                                             
!     isets = channel set info:                                         
!            isets(1,*)=1, for seis --- else aux.                       
!            isets(2,*)=#tr in this set                                 
!     nline = #lines (# sets info in lines)                             
!     lines = line info:                                                
!             lines(1,*)=line#                                          
!             lines(2,*)=#ch sets this line                             
!             lines(3,*)=1st ch set# this line                          
!             lines(4,*)=1st station # this line                        
!             lines(5,*)=last station # this line                       
!             lines(6,*)=# stations for this line                       
!             lines(7,*)=# stations in gap                              
!             lines(8,*)=1st station in gap                             
!             lines(9,*)=# of skipped stations this line                
!     shot  = the source location:                                      
!             shot(1) = source line #                                   
!             shot(2) = source sp #                                     
!     jbad  = a flag for a profile with "bad" headers                   
!           = 0, okay                                                   
!           = 1, bad                                                    
!     nskip = number of triplets in iskips                              
!     iskips= skip info as read in (may contain irrelevant skips)       
!             Triplet values: (1,*)=line#, (2,*)=1st stn, (3,*)=number  
!                                                                       
                            !save line segments for current line        
      integer :: i1,i2,ier,iknt,ilist(2,50),istn,lastf,lastl
      integer :: ngap,ngap1,nl,num,numtot,ns,ns2,nseg,nxt 
      save ilist,nseg,lastf,lastl 
                        !last file for which SD2FGd2 was called         
      data lastf/-9999/ 
                        !last file for which SD2FGd2 was called         
      data lastl/-9999/ 
!                                                                       
            !initialize bad                                             
      ier=1 
                                               !get info                
      if(nl.ne.lastl .OR. jfile.ne.lastf) then 
        call SD2FG_d2(nl,ilist,nseg) 
        lastf=jfile 
        lastl=nl 
      end if 
      numtot=0 
                   !find station for this trace                         
      do ns=1,nseg 
                      !1st stn this seg                                 
       i1=ilist(1,ns) 
                      !last stn this seg                                
       i2=ilist(2,ns) 
       num=i2-i1+1 
                                     !trace is in this segment          
       if((numtot+num).ge.iknt) then 
         istn= (iknt-numtot-1) + i1 
         ier=0 
         go to 100 
       end if 
       numtot=numtot+num 
      end do 
!---print error message                                                 
      if(ier.eq.1) then 
        print *,'SD2FG_d3: error finding stn for trace=',iknt,           &
     &          ' of line=',lines(1,nl),' for file=',jfile              
                                !cdbug                                  
        print*,'SD2FG_d3 failed' 
        write(icpr,9001)'SD2FG_d3: error finding stn for trace=',iknt,   &
     &          ' of line=',lines(1,nl),' for file=',jfile              
                                         !cdbug                         
        write(icpr,9001)'SD2FG_d3 failed' 
        stop 
      end if 
               !come here with istn                                     
  100 continue 
      return 
 9001 FORMAT(A) 
      END subroutine SD2FG_d3                                           
      SUBROUTINE SD2FG_CLEAR(IRAY,NUM) 
      integer :: i,num
      integer :: IRAY(NUM)
      DO I=1,NUM 
        IRAY(I)=0 
      ENDDO 
      RETURN 
      END subroutine SD2FG_CLEAR                                          
!***********************************************************************
      SUBROUTINE SD2FG_FILEIN 
!CC   SUBROUTINE SD2FG_FILEIN (CHARS,NCHARS,NSCR,NITEM,IEXSW)           
!  FILE NAME TO READ                                                    
      CHARACTER FULLNAME*60 
      CHARACTER FILEIN*60 
      CHARACTER *80 CMD 
      CHARACTER reelno*10 
!                                                                       
      integer :: i,i1,i2,irb,istat,itmpn,itmpn2,j,k,kntf,lastnc,nc,ncfn
      irppp=51 
      irp=52 
      ipp=53 
      icpr=54 
      ifnames=55 
      inp=56 
      itmpn=21 
      itmpn2=22 
      PRINT*,' ENTER NAME OF INPUT FILE' 
!                                                                       
  100 READ(5,9001,end=110)FILEIN 
  110 IF(FILEIN(1:4).EQ.'EXIT')STOP 
      IF(FILEIN.EQ.' ') THEN 
        PRINT*,'MUST ENTER A FILE NAME' 
        PRINT*,'ENTER EXIT TO TERMINATE' 
        GO TO 100 
      ENDIF 
      IF(filein(1:5).eq.'FILE=')THEN 
        nc=len_trim(filein)
        open(unit=ifnames,file=filein(6:nc),status='old',iostat=istat) 
        if(istat.ne.0)go to 777
        go to 500 
      ENDIF 
      k=index(filein,'*')      
      if(k.eq.0)then
        cmd='cp ' // trim(filein) // ' SD2FG.FILES'
      else
        cmd='ls ' // filein(1:k-1) // '*.HDR > SD2FG.FILES' 
      endif
      call putsys_cmd(trim(cmd))
      open(unit=itmpn,file='SD2FG.FILES',status='old',iostat=istat) 
      if(istat.ne.0)go to 777
      OPEN (UNIT=itmpn2,ERR=778,file='SD2FG.TEMP',status='new')
      if(istat.ne.0)go to 778     
!                                                                       
!          Get out reel number portion of name so we can sort by it     
!!      kntf=0 
  300 read(itmpn,9001,end=350)fullname 
      ncfn=len_trim(fullname)
      IRB=index(fullname,'/',.true.) 
!          There may be an underscore in the directory name, so search  
!           backwards                                                   
      do i=ncfn,1,-1 
        if(fullname(i:i).eq.'_')then 
          I1=I 
          go to 310 
        endif 
      enddo 
  310 continue 
      I2=index(fullname(IRB:),'.') +IRB -2 
      lastnc=len_trim(fullname)
      IF((I2-I1+1).GT.10)THEN 
        Print*,' Reel number portion of file name is greater than 10 ch &
     &aracters'                                                         
        Print*,fullname 
        STOP 
      ENDIF 
      IF(lastnc.gt.60)THEN 
        Print*,' Fill name greater than 60 characters' 
        Print*,fullname 
        STOP 
      ENDIF 
      reelno='0000000000' 
      K=10 
      DO J=I2,I1,-1 
        reelno(K:K)=fullname(J:J) 
        K=K-1 
      ENDDO 
      write(itmpn2,9002)fullname,reelno 
      go to 300 
  350 continue 
      rewind itmpn2 
!!      close(unit=itmpn2,status='keep') 
      close(unit=itmpn,status='delete') 
!            sort columns 61-70
      cmd='sort -k 61,70 SD2FG.TEMP > SD2FG.SORTED'
      call putsys_cmd(trim(cmd))
      close(unit=itmpn2,status='delete') 
      open(unit=ifnames,file='SD2FG.SORTED',status='old',iostat=istat) 
      if(istat.ne.0)go to 777
      rewind ifnames 
!                                                                       
!                                                                       
  500 RETURN 
  777 CONTINUE 
      PRINT*,' ERROR OPENING FILE' 
      STOP 
  778 CONTINUE 
      PRINT*,' ERROR OPENING FILE FOR WRITE' 
      STOP 
!                                                                       
 9001 format(A) 
 9002 format(A60,A10) 
      END subroutine SD2FG_FILEIN                                           
!***********************************************************************
      SUBROUTINE SD2FG_DATASHEETS 
      CHARACTER FULLNAME*60 
      CHARACTER FILEIN*60 
      CHARACTER *80 CMD 
      CHARACTER reelno*10 
      CHARACTER *1 ANS 
!                                                                       
      LOGICAL :: QUEST

      integer :: istat,j,k,knt,nc
 
      PRINT*,' READ IN ANCILLARY DATA FROM DATA SHEETS (Y/N)' 
      READ(5,9001)ANS 
      IF(ANS.EQ.'N'.OR.ANS.EQ.'n')RETURN 
      PRINT*,' ENTER NAME OF DATA SHEET FILE' 
!                                                                       
  100 READ(5,9001,end=110)FILEIN 
  110 IF(FILEIN(1:4).EQ.'EXIT')STOP 
      IF(FILEIN.EQ.' ') THEN 
        PRINT*,'MUST ENTER A FILE NAME' 
        PRINT*,'ENTER EXIT TO TERMINATE' 
        GO TO 100 
      ENDIF 
      nc=len_trim(filein)
      open(unit=41,file=FILEIN,status='old',iostat=istat) 
      if(istat.ne.0)go to 777
      KNT=0 
  200 CONTINUE 
      KNT=KNT+1 
      IF(KNT.GT.1000)THEN 
        PRINT*,' DATA SHEET ARRAYS DIMENSIONED FOR 1000' 
        PRINT*,' You must increase before you can continue' 
        STOP 
      ENDIF 
      READ(41,9002,END=500)IFILE(KNT),SHOT1(KNT),SHOT2(KNT) 
      GO TO 200 
  500 CONTINUE 
      CLOSE(41) 
      RETURN 
  777 CONTINUE 
      PRINT*,' ERROR OPENING FILE' 
      STOP 
!                                                                       
 9001 format(A) 
 9002 format(I4,12X,F5.1,1X,F5.1) 
      END subroutine SD2FG_DATASHEETS                                          
!***********************************************************************
      SUBROUTINE SD2FG_FILEOUT 
!  SET-UP THE FILE                                                      
      CHARACTER(len=60) :: FILEOUT 
      integer :: istat,k

      fileout=' ' 
!                                                                       
  100 PRINT*, 'ENTER NAME OF OUTPUT FILE' 
      READ(5,9001,end=110)fileout 
  110 continue 
      IF(FILEOUT(1:4).EQ.'EXIT')STOP 
      IF(FILEOUT.EQ.' ') THEN 
        PRINT*,'MUST ENTER FILE NAME' 
        PRINT*,'ENTER EXIT TO TERMINATE' 
        GO TO 100 
      ENDIF 
      K=INDEX(fileout,'.') 
      if(K.ne.0)then 
        print*,' DO NOT use an extension in the output file name' 
        print*,' SD2FG will create two files, one with extension .RPPP' 
        print*,' and one with extension .CPR' 
        print*,' Enter EXIT if you wish to terminate the program' 
        go to 100 
      endif 
!                                                                       
!       open for write                                                  
      OPEN (UNIT=irppp,iostat=istat,file=trim(FILEOUT) // '.RPPP',          &
     &      status='NEW')
      if(istat.ne.0)go to 810                                 
      OPEN (UNIT=irp,iostat=istat,file=trim(FILEOUT) // '.RP',      &
     &      status='NEW')
      if(istat.ne.0)go to 810
      OPEN (UNIT=ipp,iostat=istat,file=trim(FILEOUT) // '.PP',           &
     &      status='NEW')
      if(istat.ne.0)go to 810
      OPEN (UNIT=icpr,iostat=istat,file=trim(FILEOUT) // '.CPR',        &
     &      status='NEW')
      if(istat.ne.0)go to 810                                 
!                                                                       
      RETURN 
!                                                                       
  810 PRINT*,'ERROR OPENING FILE' 
      STOP 
 9001 format(A) 
      END subroutine SD2FG_FILEOUT                                          
      SUBROUTINE SD2FG_AUX 
      implicit none
!                                                                       
!        Print out Aux channels if first time in                        
!        Compare to see that Aux channels have not changed              
!                                                                       
       LOGICAL :: first 
       INTEGER :: AUX1,AUX2,auxp,i,j,k,kaux1
!                                                                       
      DATA first/.TRUE./ 
!                                                                       
!-----  itf=trace counter in "file", ifh=flag for 1st file              
!-----  iff=file counter                                                
!     jfile = field file#                                               
!     jset  = #channel sets                                             
!     isets = channel set info:                                         
!            isets(1,*)=1, for seis --- else aux.                       
!            isets(2,*)=#tr in this set                                 
!     nline = #lines (# sets info in lines)                             
!     lines = line info:                                                
!             lines(1,*)=line#                                          
!             lines(2,*)=#ch sets this line                             
!             lines(3,*)=1st ch set# this line                          
!             lines(4,*)=1st station # this line                        
!             lines(5,*)=last station # this line                       
!             lines(6,*)=# stations for this line                       
!             lines(7,*)=# stations in gap                              
!             lines(8,*)=1st station in gap                             
!             lines(9,*)=# of skipped stations this line                
!     shot  = the source location:                                      
!             shot(1) = source line #                                   
!             shot(2) = source sp #                                     
!     jbad  = a flag for a profile with "bad" headers                   
!           = 0, okay                                                   
!           = 1, bad                                                    
!     nskip = number of triplets in iskips                              
!     iskips= skip info as read in (may contain irrelevant skips)       
!             Triplet values: (1,*)=line#, (2,*)=1st stn, (3,*)=number  
      aux1=0 
      aux2=0 
        DO 100 I=1,JSET 
          IF(isets(1,I).NE.1)THEN 
            IF(aux1.EQ.0)THEN 
!               Find first sequential aux number                        
               DO K=1,I 
                 aux1=aux1+isets(2,K) 
               ENDDO 
               auxp=aux1 
               go to 100 
            ELSE 
!               Find last aux number                                    
               aux2=0 
               DO K=1,I 
                 aux2=aux2+isets(2,K) 
               ENDDO 
               IF((aux2-auxp).ne.1)THEN 
                 Print*,' Aux not in sequential channels for file ',    &
     &jfile                                                             
                 Write(icpr,*)' Aux not in sequential channels for fi&
     &le ',jfile                                                        
                 PRINT*,' Channel set info follows' 
                 WRITE(icpr,9001)' Channel set info follows' 
                 DO J=1,jset 
                   Print*,J,isets(1,J),isets(2,J) 
                   Write(icpr,*)J,isets(1,J),isets(2,J) 
                 ENDDO 
                 go to 8000 
               ENDIF 
               auxp=aux2 
            ENDIF 
          ENDIF 
  100   CONTINUE 
 8000 CONTINUE 
      IF(first)THEN 
        Print 9002,aux1,aux2 
        write(icpr,9002)aux1,aux2 
        kaux1=aux1 
        first=.FALSE. 
      ELSE 
        IF(aux1.ne.kaux1.or.aux2.ne.aux2)THEN 
          Print*,' Change in aux chanels for file ',jfile 
          print*,' aux1 = ',aux1,' aux2 = ',aux2 
          Write(icpr,*)' Change in aux chanels for file ',jfile 
          write(icpr,9002)aux1,aux2 
        ENDIF 
      ENDIF 
      RETURN 
 9001 FORMAT(A) 
 9002 FORMAT(/,' Aux channels are from ',I6,' to ',I6,/) 
      END subroutine SD2FG_AUX                                           
      subroutine sd2fg_compuseis(cfile,lasttime) 
                                                                        
      character(len=8) :: cfrec,cerec,firstaux,lastaux,line,firstline,&
     &cstart,cend,firstrec                                              
      character(len=3200) :: spread 
      character(len=64) :: cfile 
      character(len=1) :: ch 
                                                                        
      integer :: i,icol,iend,ierec,ifirstline,ifirstrec,ifirstrp,ifrec
      integer :: iline,isorline,istart,j,jfileprev,k
      integer :: hexnums(32),lasttime,nc,ncsets,next,numx
      real    :: source
      data jfileprev/-999/,ifirstrp/1/ 
!                                                                       
      if(lasttime.eq.1)then 
!          write out the last pp card                                   
        write(ipp,1004)source,isorline,ifirstrec,ifirstline,            &
     &jfileprev,jfileprev                                               
        return 
      endif 
                                                                        
!          Get the file number                                          
      read(cfile(1:4),9001)jfile 
      if(jfileprev.eq.-999)jfileprev=jfile 
!                                                                       
      if(jfileprev.ne.jfile)then 
!          Write the pp card       
         call string_cc2ii(firstrec,ifirstrec)
         call string_cc2ii(firstline,ifirstline)
         write(ipp,1004)source,isorline,ifirstrec,ifirstline,           &
     &jfileprev,jfileprev                                               
         jfileprev=jfile 
         ifirstrp=1 
      endif 
!                                                                       
!          Get the number of extended header cards                      
      read(cfile(61:62),9002)next 
!                                                                       
!          Get the number of channel sets                               
      read(cfile(57:58),9002)ncsets 
!                                                                       
!          Read passed the channel sets                                 
      do I=1,ncsets+1 
        read(inp,9003)cfile 
      enddo 
!                                                                       
!          First card extended header - Get source and line# of source  
      read(cfile(25:28),9001)isorline 
      read(cfile(29:32),9007)source 
!                                                                       
!          Skip next two cards                                          
      do I=1,2 
        read(inp,9003)cfile 
      enddo 
!                                                                       
!          The receiver pattern in in an ascii free format on the       
!          remainder of the extended header                             
!                                                                       
!          First - convert the hex cards to ascii and put into a        
!          continuous character array                                   
      spread=' ' 
      k=1 
      do I=1,next-4 
        read(inp,9005)hexnums 
        do J=1,32 
          ch=char(hexnums(J)) 
          if(hexnums(J).le.32)ch=' ' 
          spread(K:K)=ch 
          k=k+1 
        enddo 
      enddo 
!                                                                       
!          Now pull out the pattern                                     
      icol=0 
      k=index(spread(icol+1:),'LINE=')+icol 
      if(k.eq.icol)then 
        print*,' Can''t find line number' 
        stop 
      endif 
      k=k+5 
      if(spread(k:k+2).eq.'AUX')then 
        icol=k 
        k=index(spread(icol+1:),'START(') + icol 
        icol=k 
        k=index(spread(icol+1:),',') + icol 
        icol=icol+5 
        nc=k-icol-1 
        firstaux(1:nc)=spread(icol+1:k-1) 
        icol=k 
        k=index(spread(icol+1:),'END') + icol 
        icol=k+3 
        k=index(spread(icol+1:),',') + icol 
        nc=k-icol-1 
        lastaux(1:nc)=spread(icol+1:k-1) 
        icol=k 
        write(icpr,*)'Aux channels for file ',jfile,' = ',firstaux,'-', &
     &lastaux                                                           
      endif 
  500 continue 
      k=index(spread(icol+1:),'LINE=')+icol 
      if(k.eq.icol)go to 8000 
      icol=k+4 
      k=index(spread(icol+1:),':') + icol 
      nc=k-icol-2 
      line=' ' 
      line(1:nc)=spread(icol+1:k-2) 
      if(ifirstrp.eq.1)then 
        firstline=line 
      endif 
      icol=k 
      k=index(spread(icol+1:),'START(') + icol 
      icol=k+5 
      k=index(spread(icol+1:),',') + icol 
      nc=k-icol-1 
      cstart(1:nc)=spread(icol+1:k-1) 
      icol=k 
      k=index(spread(icol+1:),')') + icol 
      nc=k-icol-2 
      cfrec=' ' 
      cfrec(1:nc)=spread(icol+2:k-1) 
      if(ifirstrp.eq.1)then 
        firstrec=cfrec 
        ifirstrp=0 
      endif 
      icol=k 
      k=index(spread(icol+1:),'END(') + icol 
      icol=k+3 
      k=index(spread(icol+1:),',') + icol 
      nc=k-icol-1 
      cend(1:nc)=spread(icol+1:k-1) 
      icol=k 
      k=index(spread(icol+1:),')') + icol 
      nc=k-icol-2 
      cerec(1:nc)=spread(icol+2:k-1) 
      icol=k 
!                                                                       
!          See if the counts match between receivers and channels     
      call string_cc2ii(cfrec,ifrec)  
      call string_cc2ii(cerec,ierec)
      call string_cc2ii(cstart,istart)
      call string_cc2ii(cend,iend)
!                                                                       
      numx=ierec-ifrec+1 
!                                                                       
!          Write the rp card  
      call string_cc2ii(line,iline)                                          
      write(irp,1005)jfile,real(ifrec),iline,numx,1,1,1,0.0,0.0,0.0 
!                                                                       
!                                                                       
      go to 500 
                                                                        
                                                                        
                                                                        
 8000 return 
 8001 continue 
      print*,' ALF2NUMI ERROR' 
      stop 
                                                                        
 1004  format  (f7.2,   1x,i4,2x,i4,3x,1x,i4,2x,i4,41x,2x,i4) 
!               ------- ----- -------- ----- ----- --- --------         
!               SP2     LINE2    SP3   LINE3 IPAT2        IG            
!               a7      a5       a9    a5    a6           a7            
!               *7      *4       *7    *4    *3           *5            
!                                          need *4                      
!                                                                       
!                                                                       
 1005  format(1x,i4,3x,'X   ',f9.2,i6,i7,i6,i8,i7,f9.0,f8.0,f8.0) 
!                                                                       
 9001 format(i4) 
 9002 format(i2) 
 9003 format(A) 
 9004 format(f6.2) 
 9005 format(32Z2) 
 9006 format(I6) 
 9007 format(f4.0) 

      end subroutine sd2fg_compuseis
      end module sd2fg_module
      program sd2fg
      use sd2fg_module

      character(len=100),save :: SD2FG_IDENT = &
'$Id: sd2fg.f90,v 1.3 2004/06/29 18:35:57 Goodger prod sps $'

      call sd2fg_main


      end program sd2fg


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!




