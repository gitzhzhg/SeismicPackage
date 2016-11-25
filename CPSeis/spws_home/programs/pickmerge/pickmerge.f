C***************************** COPYRIGHT NOTICE ********************************
C<license>
C-------------------------------------------------------------------------------
C Copyright (c) 2007 ConocoPhillips Company
C
C Permission is hereby granted, free of charge, to any person obtaining a copy
C of this software and associated documentation files (the "Software"), to deal
C in the Software without restriction, including without limitation the rights
C to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
C copies of the Software, and to permit persons to whom the Software is
C furnished to do so, subject to the following conditions:
C
C The above copyright notice and this permission notice shall be included in all
C copies or substantial portions of the Software.
C
C THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
C IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
C FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
C AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
C LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
C OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
C SOFTWARE.
C-------------------------------------------------------------------------------
C</license>
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
c        1         2         3         4         5         6         7  |
c23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C                                         
C  Application name:  PICKMERGE 
C            Author:  GREG LAZEAR
C      Last revised:  August 3, 1998    
C
C  Purpose:       Reads two refraction static pick files and creates
C                 a third by using the geometry data from the first
C                 file and the pick times from the second file. This
C                 allows the geometry (JD) to be modified while keeping
C                 picks from manual or interactive editing.
C
C-----------------------------------------------------------------------
C                           NOTES FOR USERS
C
C  1. In order to correct the geometry (JD) on a data set and still 
C     retain the picks made interactively, perform the following 
C     sequence:
C               - correct the JD and run SCRS batch picking to create
C                 a new pickfile with the correct geometry.
C               - run this program PICKMERGE and give it three file
C                 names.
C                      First is the new pickfile with good geometry
C                      Second is the old pickfile which has the picks
C                        to be used
C                      Third is the name of the new pickfile which has
C                        the new geometry and correct picks
C               - use the output pickfile in future SCRS solutions or
C                 editing.
C
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC        
C-----------------------------------------------------------------------
C                            REVISION HISTORY
C
C     Date      Author       Description
C     ----      ------       -----------
C 2.  8/3/98   Stoeckley  Remove references to READONLY and SHARED in
C                           Fortran open statement.
C 1.  2/2/90   Lazear     Original version
C-----------------------------------------------------------------------
C                          PROGRAMMING NOTES
C
C-----------------------------------------------------------------------
C      LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C                             IN THIS MODULE
C
C-----------------------------------------------------------------------
C            LIST OF ALL EXTERNALS REFERENCED BY THIS MODULE
C
C-----------------------------------------------------------------------
C\END DOC
      PROGRAM PICKMERGE
C
      PARAMETER (INCH=1000, LFN1=7, LFN2=8, LFN3=9)
      CHARACTER VAXFILE1*80, VAXFILE2*80, VAXFILE3*80, TEMP*80
      DIMENSION SHOT(10),IARRIV(INCH),IXGP(INCH),IYGP(INCH),IOFF(INCH),
     >  IELEV(INCH),IBAD(INCH),XPW(100),YPW(100),PKWN(1800)
C
  10  PRINT*,'ENTER THE NAME OF THE PICKFILE WITH CORRECT GEOMETRY '
      VAXFILE1= ' '
      READ(5,'(A80)',END=15) VAXFILE1
      IF(VAXFILE1.NE.' ') GO TO 17
  15  STOP
  17  CALL OPENFILE (LFN1,VAXFILE1,'READ',*777)
C
      PRINT*,'ENTER THE NAME OF THE PICKFILE WITH CORRECT PICKS '
      READ(5,'(A80)') VAXFILE2
      CALL OPENFILE (LFN2,VAXFILE2,'READ',*777)
C
      PRINT*,'ENTER NAME FOR THE OUTPUT FILE (DEFAULT = FIRST NAME)'
      TEMP= ' '
      VAXFILE3= VAXFILE1
      READ(5,'(A80)',END=20) TEMP
      IF(TEMP.NE.' ') VAXFILE3= TEMP
  20  CONTINUE
      CALL OPENFILE (LFN3,VAXFILE3,'WRITE',*777)
C
C  READ PARAMETERS FROM THE TWO FILES TO CREATE THE OUTPUT PARAMETER SET
C
      REWIND LFN1
      REWIND LFN2
      REWIND LFN3
      READ(LFN1,5001) JGRP,ID1,MXXGP,MXYGP,MNXGP,MNYGP,MXO,INCGP,RINC,
     >  NCH,D2,ID3,ID4,ID5,ID6,ID7,ID8
      READ(LFN2,5001) JGRP2,ITMAX,JD1,JD2,JD3,JD4,JD5,JD6,D7,NCH2,TRSH,
     >  IZC,NXPW,NYPW,NPWOFF,IPKWHX,IPKWHY
C
C   NUMBER OF CHANNELS AND SHOT GROUPS MUST AGREE FOR THE TWO FILES
C
      IF(JGRP.NE.JGRP2) THEN
         PRINT*,'THE TWO FILES DO NOT HAVE THE SAME NUMBER OF SHOTS'
         STOP
      END IF
      PRINT*,'NUMBER OF SHOT FILES = ',JGRP
      IF(NCH.NE.NCH2) THEN
         PRINT*,'THE TWO FILES DO NOT HAVE THE SAME NUMBER OF CHANNELS',
     >    ' PER SHOT'
         STOP
      END IF
      PRINT*,'NUMBER OF CHANNELS PER SHOT FILE = ',NCH
C
C   KEEP THE PICKWINDOW PARAMETERS FROM THE FILE WITH GOOD PICKS
C
      READ(LFN1,5005) (XPW(I),I=1,ID4)
      READ(LFN1,5005) (YPW(I),I=1,ID5)
      READ(LFN1,5006) (PKWN(I),I=1,ID6*3*ID4*ID5)
C
      READ(LFN2,5005) (XPW(I),I=1,NXPW)
      READ(LFN2,5005) (YPW(I),I=1,NYPW)
      READ(LFN2,5006) (PKWN(I),I=1,NPWOFF*3*NXPW*NYPW)
C
C   OUTPUT PARAMETERS TO THE NEW PICKFILE
C
      WRITE(LFN3,5001) JGRP,ITMAX,MXXGP,MXYGP,MNXGP,MNYGP,MXO,INCGP,
     > RINC,NCH,TRSH,IZC,NXPW,NYPW,NPWOFF,IPKWHX,IPKWHY
      WRITE(LFN3,5005) (XPW(I),I=1,NXPW)
      WRITE(LFN3,5005) (YPW(I),I=1,NYPW)
      WRITE(LFN3,5006) (PKWN(I),I=1,NPWOFF*3*NXPW*NYPW)      
      PRINT*,'FILE PARAMETERS HAVE BEEN WRITTEN TO NEW FILE'
C
C  READ SHOT RECORDS FROM EACH FILE AND OUTPUT THE MERGED DATA TO NEW FILE
C
      PRINT*,'MERGING PICKS AND GEOMETRY DATA'
      DO 30 I=1,JGRP
      READ(LFN2,5002) (SHOT(IC),IC=1,10),(IARRIV(IC),IXGP(IC),IYGP(IC),
     >  IOFF(IC),IELEV(IC),IC=1,NCH)
      READ(LFN1,5002) (SHOT(IC),IC=1,10),(IBAD(IC),IXGP(IC),IYGP(IC),
     >  IOFF(IC),IELEV(IC),IC=1,NCH)
      WRITE(LFN3,5002) (SHOT(IC),IC=1,10),(IARRIV(IC),IXGP(IC),IYGP(IC),
     >  IOFF(IC),IELEV(IC),IC=1,NCH)
      IF(MOD(I,100).EQ.0) PRINT*,I,' SHOT RECORDS MERGED'
  30  CONTINUE
      PRINT*,'MERGING OF FILES COMPLETE'
C
 777  CLOSE (LFN1)
      CLOSE (LFN2)
      CLOSE (LFN3)
C
 5001 FORMAT(8I6,/,F6.1,I6,F6.3,3X,A3,5I4)
 5002 FORMAT(5F10.3,/,5F10.3,/,(5I6))
 5005 FORMAT(5(1X,F9.0))
 5006 FORMAT(3(1X,F9.3))
C
      STOP
      END
C
      SUBROUTINE OPENFILE (LFN,VAXFILE,RW,*)
C     OPEN A VAX FILE FOR READ OR WRITE.
C     LFN = LOCAL FILE NAME (UNIT NUMBER).
C     VAXFILE = VAX FILE NAME.
C     RW = 'READ' TO OPEN AN EXISTING VAX FILE FOR READ.
C     RW = 'WRITE' (OR ANYTHING ELSE) TO OPEN A NEW VAX FILE FOR WRITE.
C     VAX FILE NAME WILL BE EXPANDED TO FULL DESCRIPTION.
C     PRINTS MESSAGE AND USES ALTERNATE RETURN UPON ERROR.
C----------DIMENSION STATEMENTS.
      CHARACTER*(*) VAXFILE,RW
      CHARACTER*80 FULLNAME
      LOGICAL QUEST
C----------OPEN FOR WRITE.
!!!   IF (RW.NE.'READ') OPEN (UNIT=LFN,ERR=999,NAME=VAXFILE,
!!!  $   TYPE='NEW',FORM='FORMATTED',ACCESS='SEQUENTIAL')
      IF (RW.NE.'READ') OPEN (UNIT=LFN,ERR=999,file=VAXFILE,
     $   status='NEW',FORM='FORMATTED',ACCESS='SEQUENTIAL')
C----------EXPAND NAME AND CHECK ON EXISTANCE.
      INQUIRE (FILE=VAXFILE,NAME=FULLNAME,ERR=999,EXIST=QUEST)
      VAXFILE=FULLNAME
      IF (.NOT.QUEST) GO TO 999
C----------OPEN FOR READ.
!!!   IF (RW.EQ.'READ') OPEN (UNIT=LFN,ERR=999,NAME=VAXFILE,
!!!  $   TYPE='OLD',FORM='FORMATTED',ACCESS='SEQUENTIAL')
      IF (RW.EQ.'READ') OPEN (UNIT=LFN,ERR=999,file=VAXFILE,
     $   status='OLD',FORM='FORMATTED',ACCESS='SEQUENTIAL')
ccc  $   TYPE='OLD',FORM='FORMATTED',ACCESS='SEQUENTIAL',
ccc  $   READONLY,SHARED)
C----------WE ARE SUCCESSFUL.
      RETURN
C----------WE ARE NOT SUCCESSFUL.
999   PRINT *, VAXFILE
      PRINT *, 'ERROR ON ATTEMPT TO OPEN ABOVE FILE FOR ',RW
      RETURN 1
      END
