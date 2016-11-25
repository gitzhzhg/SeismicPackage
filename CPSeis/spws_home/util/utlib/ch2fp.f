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
C  Process name: CH2FP
C        Author: R.S.DAY
C       Written: 90/07/11
C  Last revised: 90/07/11   Day
C
C  Purpose:      EXTRACT THE FLOATING POINT NUMBERS FROM AN ASCII STRING
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE CH2FP(C,NVALS,A)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C C...........CHARACTER INPUT    INPUT STRING
C NVALS.......INTEGER   OUT      NUMBER OF NUMERIC VALUES RETURNED
C A...........REAL      OUT      ARRAY OR F.P. NUMBERS
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. RETURNS INTEGER AND REALS AS FLOATING POINT
C
C 2. CH2FP TAKES AN ASCII RECORD AND RETURNS THE NUMBER OF FLOATING
C    POINT NUMBERS AND THEIR VALUES. THE NUMBERS MUST BE DELIMITED BY
C    AT LEAST ONE BLANK, NULL, EQUAL SIGN OR COMMA ON EITHER SIDE.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date     Author     Description
C     ----     ------     -----------
C 1.  90/07/11 Day        Original Version
C-----------------------------------------------------------------------
C EXTERNAL CALLS:ALF2NUMR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C\END DOC
CCC
      SUBROUTINE CH2FP(C,NVALS,A)
      DIMENSION     A(*)
      CHARACTER*(*) C
      CHARACTER     SUBSTR*32
C*****
      LENC=LEN(C)
      IC=32
C
      SUBSTR=' '
      NUM=0
      NCH=0
      DO 50 J=1,LENC
       IF(C(J:J).EQ.CHAR(32) .OR. C(J:J).EQ.CHAR(0) .OR.
     +    C(J:J).EQ.',' .OR. C(J:J).EQ.'=') THEN
         IF(NCH.GT.0) THEN
           NUM=NUM+1
           CALL ALF2NUMR(SUBSTR,NCH,A(NUM),*75,*85)
         END IF
         NCH=0
         SUBSTR=' '
         GO TO 50
 75      NUM=MAX(NUM-1,0)    !ERROR
         NCH=0
         SUBSTR=' '
         GO TO 50
 85      NUM=MAX(NUM-1,0)    !BLANK
         NCH=0
         SUBSTR=' '
         GO TO 50
       ELSE
         NCH=NCH+1
         SUBSTR(NCH:NCH)=C(J:J)
       END IF
 50   CONTINUE
C
      IF(NCH.GT.0) THEN
         NUM=NUM+1
         CALL ALF2NUMR(SUBSTR,NCH,A(NUM),*90,*90)
         NVALS=NUM
         RETURN
      END IF
      NVALS=NUM
      RETURN
 90   NVALS=MAX(NUM-1,0)
      RETURN 
      END
CCC
