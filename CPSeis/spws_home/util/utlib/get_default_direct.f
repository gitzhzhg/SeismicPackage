      SUBROUTINE GET_DEFAULT_DIRECT (DEF_DIR,NCIDI,DEF_DEV,NCIDV,
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
     +SUBDIR_SCR,ROOTDIR_SCR,*)
C***************************** COPYRIGHT NOTICE ********************************
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
C\USER DOC
C-----------------------------------------------------------------------
C                         CRAY PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C  Process name: GET_DEFAULT_DIRECT[ORY]
C        Author: John B. Sinton
C  Last revised: 90/01/30
C
C       Purpose: To get the default directory and device.
C----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date      Author     Description
C     ----      ------     -------------
C  4.
C  3. 90/01/30; JB Sinton; Added SUBDIR_SCR and ROOTDIR_SCR 
C                          parameters.
C  2. 87/06/11; JB Sinton; Added def_device.
C  1. 87/04/03; JB Sinton; First working version.
C
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C        CALL GET_DEFAULT_DIRECT (DEF_DIR,NCIDI,DEF_DEV,NCIDV,
C     +                           SUBDIR_SCR,ROOTDIR_SCR,*)
C
C  Name        I/O   Valid   Description
C
C  DEF_DIR     OUT  chr*80   String containing the default directory.
C                            Example: '[SINTON.TMV]'
C  NCIDI       OUT  integer  Number of characters in DEF_DIR.
C  DEF_DEV     OUT  chr*80   String containing the default device
C                            Example: 'USER13:' or 'SCRATCH0:'
C  NCIDV       OUT  integer  Number of characters in DEF_DEV.
C  SUBDIR_SCR  OUT  logical  =.TRUE. if SCRATCH0: exists for DEF_DIR.
C                            =.FALSE. if not.
C  ROOTDIR_SCR OUT  logical  =.TRUE. if SCRATCH0: exists for the root 
C                              directory of DEF_DIR.
C                            =.FALSE. if not.
C  *           ---           Alternate error return if DEF_DIR or 
C                            DEF_DEV could not be determined.
C-----------------------------------------------------------------------
C                                 NOTES
C
C-----------------------------------------------------------------------
C\END DOC
C
      CHARACTER    SCRFILE*64, FILENAME*256
      CHARACTER*(*) DEF_DIR ,DEF_DEV
      LOGICAL SUBDIR_SCR, ROOTDIR_SCR
      NCHAR = 0
      CALL CFILL (DEF_DIR ,' ')
      CALL CFILL (FILENAME,' ')
      CALL CFILL (DEF_DEV,' ')
      SCRFILE= 'SCRATCH0:[]JUNK.JUNK'
      CALL LIB_GET_LUN(LFN)
      OPEN (LFN,FILE=SCRFILE(:20),STATUS='SCRATCH',ERR=1)
 1    INQUIRE(FILE=SCRFILE,EXIST=SUBDIR_SCR)
      CLOSE (UNIT=LFN,STATUS='DELETE')
      SCRFILE= 'JUNK.JUNK'
C     OPEN (LFN,FILE=SCRFILE(:9),STATUS='SCRATCH',ERR=900,IOSTAT=IOS)
      OPEN (LFN,FILE=SCRFILE(:9),ERR=900,IOSTAT=IOS)
      INQUIRE (FILE=SCRFILE(:9),NAME=FILENAME)
      CLOSE (UNIT=LFN,STATUS='DELETE')
      ILSB = INDEX(FILENAME,'[')
      IRSB = INDEX(FILENAME,']')
      IPER = INDEX(FILENAME,'.')
      ICOL = INDEX(FILENAME,':')
      DEF_DIR = FILENAME(ILSB:IRSB)
      NCIDI   = IRSB-ILSB+1
      IF (ICOL.EQ.ILSB-1) THEN
       DEF_DEV = FILENAME(:ICOL)
       NCIDV = ICOL
      ENDIF
      ROOTDIR_SCR = .FALSE.
      IF (IPER.GT.IRSB) THEN
       ROOTDIR_SCR = SUBDIR_SCR
      ELSE
       SCRFILE= 'SCRATCH0:['//FILENAME(ILSB+1:IPER-1)//']JUNK.JUNK'
       LF = 10+(IPER-ILSB-1)+10
C      OPEN (LFN,FILE=SCRFILE(:LF),STATUS='SCRATCH',ERR=5)
       OPEN (LFN,FILE=SCRFILE(:LF),ERR=5)
 5     INQUIRE(FILE=SCRFILE,EXIST=ROOTDIR_SCR)
       CLOSE (UNIT=LFN,STATUS='DELETE') 
      ENDIF
      RETURN
 900  PRINT *,'=>G_D_D: error in opening file, IOS=',IOS
      GOTO 999
 901  PRINT *,'=>G_D_D; error in file name.'
 999  PRINT *,'  NAME=',filename
      PRINT '('' Press return to continue''$)'
      READ (5,'(A1)') FILENAME(1:1)
      RETURN 1
      END
