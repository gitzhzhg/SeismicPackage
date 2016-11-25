C*******************************************************************************
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
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*******************************************************************************
C\USER DOC
C-----------------------------------------------------------------------
C                   INTERACTIVE CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name: ICPOPMF
C        Author: R. S. DAY
C  Last revised: 92/12/09
C
C  Purpose:      IO ROUTINES THAT OPEN AND CLOSE CONOCO VELOCITY MODEL
C                ICP MODEL , AND GWS GENERIC PICK FILES.
C                
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE ICPOPMF(LUNGMF,IOS,NAMEN,IO,FTYPE)
C       SUBROUTINE ICPCLMF(LUNGMF,IOS)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C LUNGMF......INTEGER   OUT      FORTRAN LOGICAL UNIT NUMBER
C IOS.........INTEGER   OUT      RETURN ERROR STATUS
C NAMEN.......CHARACTER IN       FILE NAME
C IO..........CHARACTER IN       R OR W FOR TYPE OF FILE ACCESS
C FTYPE.......CHARACTER OUT      FILE TYPE= ICP, GWS OR CVM or ???
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. WILL RETURN THE FILE TYPE WHEN IT IS OPENED.
C 2. CHECKS FOR EXISTENCE OF A FILE
C-----------------------------------------------------------------------
C\END DOC
CCC
      SUBROUTINE ICPOPMF(LUNGMF,IOS,NAMEN,IO,FTYPE)
      INTEGER       LUNGMF,IOS,ICRAY
      CHARACTER*(*) NAMEN,IO,FTYPE
      CHARACTER     LINE*120,TYPE*4
      CHARACTER     OPSTAT*8,OPFORM*16,OPACC*16
      LOGICAL       JESUIS,OUVERT
      IOS=0
      LUNGMF=0
      INQUIRE(FILE=NAMEN,OPENED=OUVERT,EXIST=JESUIS)

      ICRAY = 0   ! hardwired for non-cray
      NRECL = 80  !irrelevant for non-cray sequential files
      OPACC ='SEQUENTIAL'
      OPFORM='FORMATTED'
      IF(JESUIS) THEN
         IF(IO.EQ.'W' .OR. 'IO'.EQ.'w') THEN
           OPSTAT='NEW'
           IOP=1
           CALL RMODOPEN(ICRAY,LUNGMF,NAMEN,OPFORM,OPSTAT,
     +                   OPACC, NRECL,*10)
           IOP=0
 10        CONTINUE
           IF(IOP.NE.0) THEN
            IOS=3
            LUNGMF=0
           END IF
           RETURN
         ELSE
           IF(OUVERT) THEN
             INQUIRE(FILE=NAMEN,NUMBER=LUN)
             IF(LUN.EQ.0) THEN
               IOS=5
               RETURN
             END IF
             LUNGMF=LUN
             IOS=1
           ELSE
             IOP=1
             OPSTAT='OLD'
             CALL RMODOPEN(ICRAY,LUNGMF,NAMEN,OPFORM,OPSTAT,
     +                   OPACC, NRECL,*10)
             IOP=0
 20          CONTINUE
C            CALL GETLUN(LUNGMF,IOG)
C            OPEN(UNIT=LUNGMF,FILE=NAMEN,ACCESS='SEQUENTIAL',
C    +       STATUS='OLD',FORM='FORMATTED',IOSTAT=IOP,READONLY)
             IF(IOP.NE.0) THEN
               IOS=4
               LUNGMF=0
               RETURN
             END IF
           END IF
           REWIND (LUNGMF)
           FTYPE ='???'
           DO 30 I=1,2
             READ(LUNGMF,'(A)') LINE
             IF(INDEX(LINE(1:),'*LINE').NE.0)   FTYPE='ICP'
             IF(INDEX(LINE(1:),'*HEADER').NE.0) FTYPE='CVM'
             IF(INDEX(LINE(1:),'*PICKS').NE.0)  FTYPE='GWS'
 30        CONTINUE
           REWIND (LUNGMF)
           RETURN
         END IF
      ELSE  !File does not exist
         IF(IO(1:1).EQ.'R' .OR. IO(1:1).EQ.'r') THEN
           IOS=2
           RETURN
         ELSE
           OPSTAT='NEW'
           IOP=1
           CALL RMODOPEN(ICRAY,LUNGMF,NAMEN,OPFORM,OPSTAT,
     +                   OPACC, NRECL,*40)
           IOP=0
 40        CONTINUE
C          CALL GETLUN(LUNGMF,IOG)
C          OPEN(UNIT=LUNGMF,FILE=NAMEN,ACCESS='SEQUENTIAL',STATUS='NEW',
C    +     FORM='FORMATTED',IOSTAT=IOP,CARRIAGECONTROL='LIST')
           IF(IOP.NE.0) THEN
             IOS=4
             LUNGMF=0
             RETURN
           END IF
         END IF
      END IF
      RETURN
      END
CCC
      SUBROUTINE ICPCLMF(LUNGMF,IOS)
      INTEGER ICRAY,IOS,LUNGMF
      IOS=0
      ICRAY=0  !HARDWIRED FOR NON CRAY
      IF(LUNGMF.EQ.0) THEN
        IOS=1
        RETURN
      END IF
      CALL RMODCLOS(ICRAY,LUNGMF)
      LUNGMF=0
      RETURN
      END
