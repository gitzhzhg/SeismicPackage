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
C                         CRAY PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C                      CPS, ICP, and GWS PRIMITIVE
C
C  Primitive name:  READ_CPS_STATFILE     (read/write a CPS static file)
C          Author:  TOM STOECKLEY , 91/03/28
C    Last revised:  91/03/28
C
C  Purpose:         Read, write, add, replace, and delete a CPS static
C                   file.  Also inquire about the existence of a file.
C
C  Related documentation:    See OPEN_TEXT_FILE and STATRII.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C ---> To READ an existing CPS static file:
C ---> To WRITE a CPS static file (add or replace):
C ---> To ADD a new CPS static file (replace not allowed):
C ---> To REPLACE an existing CPS static file (add not allowed):
C ---> To DELETE an existing CPS static file:
C ---> To INQUIRE about the existance of a CPS static file:
C
C    CALL    READ_CPS_STATFILE (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,***)
C    CALL   WRITE_CPS_STATFILE (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,***)
C    CALL     ADD_CPS_STATFILE (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,***)
C    CALL REPLACE_CPS_STATFILE (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,***)
C    CALL  DELETE_CPS_STATFILE (MSG,FILENAME,EXT,FULL)
C    CALL INQUIRE_CPS_STATFILE (MSG,FILENAME,EXT,FULL)
C
C     *** = TYPE,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD,S,CARD
C
C  Name     IN/OUT  Type  Valid    Description
C  ----     ------  ----  -----    -----------
C  MSG      output  char           Returns 'OK' if there is no error.
C                                  Otherwise returns an error message.
C                                  Recommend char*80.
C
C  FILENAME input   char           Name of a CPS static file.
C
C  EXT      input   char           If not blank, this is an extension
C                                  to be added to the file name if it
C                                  does not already have one.  The
C                                  period should not be included.
C
C  FULL     output  char           Full expanded file name, including
C                                  disk, directories, extension, and
C                                  version number.  Recommend char*80.
C                                  It is OK for FULL to be the same
C                                  variable as FILENAME.
C
C  NMAX     input   int   >=NX*NY  Dimension of array S.
C  NCMAX    input   int   >=NCARD  Dimension of array CARD.
C
C  TYPE     either  char*8 any     Type of static file.
c  NHX      either  int    1-NWIH  Header word of static X ground pos.
c  NHY      either  int    0-NWIH  Header word of static Y ground pos.
c  NHX2     either  int    0-NWIH  Second X header word for S=R file.
c  NHY2     either  int    0-NWIH  Second Y header word for S=R file.
c  X1       either  real   any     X ground position of first static.
c  Y1       either  real   any     Y ground position of first static.
c  XINC     either  real   not 0   X ground position increment.
c  YINC     either  real   not 0   Y ground position increment.
c  NX       either  int    >=1     Number of X ground positions.
c  NY       either  int    >=1     Number of Y ground posns (=1 for 2D).
c  NCARD    either  int    >=0     Number of comment cards.
c  S        either  real           Array of NX*NY static values.
c  CARD     either  char*80        Array of NCARD comment cards.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. See documentation in STATRII for details of the CPS static file
C     format and the following static file parameters:
C     TYPE,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD,S,CARD.
C
C  2. One comment card is added to array CARD (and NCARD is incremented
C     by one) containing details of the read or write performed.
C
C  3. Unit number 77 is used for all reads and writes.  It is opened
C     after entering this subroutine, and closed before exiting.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C 91/03/28  Stoeckley    Initial version.
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY, AND COMMON BLOCK NAMES IN THIS MODULE
C
C  Subroutines:     READ_CPS_STATFILE
C  Functions:       none
C  Entry points:    WRITE_CPS_STATFILE    ADD_CPS_STATFILE
C                   REPLACE_CPS_STATFILE  DELETE_CPS_STATFILE
C                   INQUIRE_CPS_STATFILE
C  Common blocks:   none
C_______________________________________________________________________
C                 EXTERNALS REFERENCED BY THIS MODULE
C
C      DATE     OPEN_TEXT_FILE      STATRII  STATRCC  STATREAD
C      TIME                         STATWII  STATWCC  STATWRIT
C-----------------------------------------------------------------------
C\END DOC

      SUBROUTINE READ_CPS_STATFILE (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,
     $   TYPE,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD,S,CARD)

C----------DIMENSION STATEMENTS FOR ARGUMENTS.

      INTEGER NMAX,NCMAX,NHX,NHY,NHX2,NHY2,NX,NY,NCARD
      REAL X1,Y1,XINC,YINC,S(*)
      CHARACTER*(*) MSG,FILENAME,EXT,FULL
      CHARACTER*8 ACTION,TYPE
      CHARACTER*80 CARD(*)

C----------OTHER DIMENSION STATEMENTS.

      PARAMETER (LFN=77)
      CHARACTER*10 DDDD,TTTT

C----------READ AN EXISTING FILE.

      ACTION='READ'
      CALL OPEN_TEXT_FILE (MSG,LFN,FILENAME,EXT,ACTION,FULL)
      IF (MSG.NE.'OK') RETURN
      CALL STATRII (LFN,TYPE,NHX,NHY,NHX2,NHY2,
     $   X1,Y1,XINC,YINC,NX,NY,*333,NCARD)
      IF (NX*NY.GT.NMAX) GO TO 111
      IF (NCARD.GE.NCMAX) GO TO 222
      IF (NCARD.GT.0) CALL STATRCC (LFN,CARD,NCARD,*444)
      NCARD=NCARD+1
      CALL DATE (DDDD)
      CALL TIME (TTTT)
      CARD(NCARD)=DDDD//TTTT//ACTION//FULL
      CALL STATREAD (LFN,NX,NY,S,*555)
      GO TO 50

C----------WRITE A NEW FILE (ADD OR REPLACE).

      ENTRY WRITE_CPS_STATFILE (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,
     $   TYPE,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD,S,CARD)

      ACTION='WRITE'
      GO TO 10

C----------ADD A NEW FILE (REPLACE NOT ALLOWED).

      ENTRY ADD_CPS_STATFILE (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,
     $   TYPE,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD,S,CARD)

      ACTION='ADD'
      GO TO 10

C----------REPLACE A FILE (ADD NOT ALLOWED).

      ENTRY REPLACE_CPS_STATFILE (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,
     $   TYPE,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD,S,CARD)

      ACTION='REPLACE'
10    CALL OPEN_TEXT_FILE (MSG,LFN,FILENAME,EXT,ACTION,FULL)
      IF (MSG.NE.'OK') RETURN
      IF (NX*NY.GT.NMAX) GO TO 111
      IF (NCARD.GE.NCMAX) GO TO 222
      NCARD=NCARD+1
      CALL DATE (DDDD)
      CALL TIME (TTTT)
      CARD(NCARD)=DDDD//TTTT//ACTION//FULL
      CALL STATWII (LFN,TYPE,NHX,NHY,NHX2,NHY2,
     $   X1,Y1,XINC,YINC,NX,NY,*333)
      CALL STATWCC (LFN,CARD,NCARD,*444)
      CALL STATWRIT (LFN,NX,NY,S,*555)
      GO TO 50

C----------DELETE A FILE.

      ENTRY DELETE_CPS_STATFILE (MSG,FILENAME,EXT,FULL)

      ACTION='DELETE'
      CALL OPEN_TEXT_FILE (MSG,LFN,FILENAME,EXT,ACTION,FULL)
      RETURN

C----------INQUIRE ABOUT A FILE.

      ENTRY INQUIRE_CPS_STATFILE (MSG,FILENAME,EXT,FULL)

      ACTION='INQUIRE'
      CALL OPEN_TEXT_FILE (MSG,LFN,FILENAME,EXT,ACTION,FULL)
      RETURN

C----------FINISH UP.

50    CLOSE (LFN)
      RETURN

C----------ERROR.

111   MSG='DIMENSION OF STATIC VALUES ARRAY TOO SMALL FOR '//ACTION
      GO TO 50
222   MSG='DIMENSION OF COMMENT CARD ARRAY TOO SMALL FOR '//ACTION
      GO TO 50
333   MSG='ERROR IN STATRII OR STATWII DURING '//ACTION
      GO TO 50
444   MSG='ERROR IN STATRCC OR STATWCC DURING '//ACTION
      GO TO 50
555   MSG='ERROR IN STATREAD OR STATWRIT DURING '//ACTION
      GO TO 50
      END
