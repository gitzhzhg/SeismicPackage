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
C\USER DOC
C-----------------------------------------------------------------------
C                         CRAY PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C  Process name:  ALF2NUMR
C        Author:  Bob Baumel
C  Last revised:  88/07/03
C
C  Purpose:  To obtain the numeric value (real or integer) represented
C            by an alphanumeric field.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C                           ----------------
C                   This routine has two entry points;
C          ALF2NUMR for real values, ALF2NUMI for integer values:
C
C             CALL ALF2NUMR (FIELD,NCHARS, VALUE,*ERR,*BLNK)
C             CALL ALF2NUMI (FIELD,NCHARS,IVALUE,*ERR,*BLNK)
C
C    FIELD  is the alphanumeric data whose numeric value is desired.
C    NCHARS is the number of characters of FIELD to be scanned.
C    VALUE  is the real value returned by entry ALF2NUMR.
C    IVALUE is the integer value returned by entry ALF2NUMI.
C    *ERR   is an alternate return in case of error.
C    *BLNK  is an alternate return executed when FIELD is blank.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. This routine runs on both the Cray and Vax, but performs slightly
C    differently on the two machines.  For example, the input variable
C    FIELD may be of type character OR Hollerith on the Cray, but must
C    be a CHARACTER variable on the Vax.
C
C 2. The Cray version of this routine does not alter the value of FIELD,
C    but the Vax version rewrites FIELD. (This can be useful in EZED
C    screen sessions.)  The Vax version makes the following changes to
C    FIELD: the data is left-justified, embedded blanks (or nulls) are
C    removed, and trailing nulls are replaced by blanks.
C
C 3. In determining the value of FIELD, all blanks and nulls (leading,
C    embedded, or trailing) are ignored.
C
C 4. You needn't always bother setting a positive value of NCHARS, as
C    setting NCHARS=0 gets you a default: If FIELD is of type CHARACTER
C    (on either Cray or Vax), the default is to use the entire length
C    of FIELD.  When FIELD contains Hollerith data (valid only on Cray)
C    the default is to read one machine word (8 bytes), so you have to
C    set NCHARS>0 if you want to read more than 8 characters.
C
C 5. The *BLNK return is included because a blank field need not always
C    be treated as equal to zero.  For example, the blank field might
C    signal you to use a default value quite different from zero.
C-----------------------------------------------------------------------
C No external references called, other than standard fortran functions.
C-----------------------------------------------------------------------
C\END DOC
      SUBROUTINE ALF2NUMR(FIELD,NCHARS,VALUE,*,*)
      PARAMETER (NMAX=80)
      CHARACTER FIELD*(*),TEMP*(NMAX),FMTR*8,FMTI*8
      DATA FMTR/'(F  .0)'/ , FMTI/'(I  )'/
      IENTRY = 1
      GO TO 1
      ENTRY ALF2NUMI(FIELD,NCHARS,IVALUE,*,*)
      IENTRY = 2
   1  IF (NCHARS.GT.0 .AND. LEN(FIELD).GT.0)  THEN
        LCHARS = MIN (NCHARS,LEN(FIELD))
      ELSE
        LCHARS = MAX (NCHARS,LEN(FIELD))
      END IF
      IF (LCHARS.LT.1)  LCHARS = 8
      N = 0
      DO 2 I=1,LCHARS
        ITEST = ICHAR(FIELD(I:I))
        IF (ITEST.NE.0 .AND. ITEST.NE.32)  THEN
          N = N+1
          IF (N.GT.NMAX)  RETURN 1
          TEMP(N:N) = FIELD(I:I)
        END IF
   2  CONTINUE
C
C  Note: The following code is not used in the Cray version:
C
      DO 4 I=1,N
   4  FIELD(I:I) = TEMP(I:I)
      DO 5 I=N+1,LCHARS
   5  FIELD(I:I) = ' '
C
      IF (N.EQ.0)  RETURN 2
      IF (IENTRY.EQ.1)  THEN
        WRITE (FMTR(3:4),'(I2)') N
        READ (TEMP(1:N),FMTR,ERR=6) VALUE
      ELSE
        WRITE (FMTI(3:4),'(I2)') N
        READ (TEMP(1:N),FMTI,ERR=6) IVALUE
      END IF
      RETURN
   6  RETURN 1
      END
