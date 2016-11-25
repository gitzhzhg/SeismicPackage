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
C  Process name:  NUM2ALFR
C        Author:  Bob Baumel
C  Last revised:  88/07/05
C
C  Purpose:  To produce an alphanumeric field representing a given
C            (real or integer) numeric value.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C                           ----------------
C                   This routine has two entry points;
C          NUM2ALFR for real values, NUM2ALFI for integer values:
C
C                CALL NUM2ALFR ( VALUE,FIELD,*ERR,NCHARS)
C                CALL NUM2ALFI (IVALUE,FIELD,*ERR,NCHARS)
C
C          VALUE  is the real value input to entry NUM2ALFR.
C          IVALUE is the integer value input to entry NUM2ALFI.
C          FIELD  is the alphanumeric data returned by the routine.
C          *ERR   is an alternate return in case of error.
C          NCHARS is the number of non-blank characters returned
C                    in FIELD (value returned by the subroutine).
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. This routine can be used on both the Cray and Vax.
C
C 2. The FIELD variable must be of type CHARACTER in the routine that
C    calls NUM2ALFR or NUM2ALFI.
C
C 3. The routine tries to make optimal use of the available field width
C    without displaying more significant figures than are consistent
C    with machine precision.  Some particular features:
C
C    a) The result is always left-justified and filled with blanks on
C       the right.
C
C    b) The routine tries hard to avoid E format (for real values).
C       It resorts to E format only if it cannot fit the value into
C       the available field width (with at least 2 significant digits)
C       WITHOUT using E format.
C                                                                      
C    c) In accord with proper number-writing practice, the routine     
C       does display one zero before the decimal point when the number
C       has absolute value smaller than one.
C
C    d) Trailing zeros (after the decimal point) are NOT displayed.
C
C    e) If a real value evaluates to an integer (to the precision of
C       the display), it is displayed as an integer; i.e. without a
C       decimal point.
C
C    f) If the number is an exact machine zero (real or integer), it
C       is displayed as a single digit '0' at the left-hand edge of
C       the field.
C
C    g) The routine tries to display as many significant figures as
C       possible, within the available field width, but will not
C       exceed a predetermined upper limit hard-wired as a parameter
C       (currently set to 7 in both the Cray and Vax versions).
C
C 4. An error return will occur if the routine cannot fit the value
C    into the available field width with suitable precision (at least
C    two significant figures for reals).  For real values, FIELD should
C    be at least 4 characters long.  You should be able to represent
C    any real value without hitting an error return if FIELD is at
C    least 8 characters in length.
C
C 5. When an error return occurs, the variable FIELD is returned filled
C    with BLANKS.
C-----------------------------------------------------------------------
C No external references called, other than standard fortran functions.
C-----------------------------------------------------------------------
C\END DOC
      SUBROUTINE NUM2ALFR (VALUE,FIELD,*,NCHARS)
      PARAMETER (NMAX=80,NSIGMAX=7)
      CHARACTER FIELD*(*),TEMP*(NMAX),FMT*16
      LOGICAL POINT
C
      IF (VALUE.EQ.0.)  THEN
        NCHARS = 1
        FIELD = '0'
        RETURN
      END IF
      LFIELD = LEN(FIELD)
      IF (VALUE.LT.0.)  THEN
        FIELD(1:1) = '-'
        NSIGN = 1
        TVAL = -VALUE
      ELSE
        NSIGN = 0
        TVAL = VALUE
      END IF
      IPASS = 1
  10  NSIG = MIN(LFIELD-NSIGN-1,NSIGMAX)
      IF (NSIG.LT.2)  GO TO 999
      WRITE (FMT,100) NSIG+7,NSIG
 100  FORMAT ( '(E' , I2 , '.' , I2 , ')' )
      WRITE (TEMP,FMT,ERR=999) TVAL
      READ (TEMP(NSIG+5:),'(I3)',ERR=999) NEXP
      IF (NEXP.LE.0)  THEN                        ! Very Small Number
        NDEC = MIN(LFIELD-NSIGN-2,NSIGMAX-NEXP)
        IF (NDEC+NEXP.LT.2)  GO TO 70
        IF (NDEC+3.GT.NMAX)  GO TO 70
        WRITE (FMT,200) NDEC+3,NDEC
 200    FORMAT ( '(F' , I2 , '.' , I2 , ')' )
        WRITE (TEMP,FMT,ERR=999) TVAL
        DO 20 I=1,NDEC+2
  20    FIELD(NSIGN+I:NSIGN+I) = TEMP(1+I:1+I)
        IF (FIELD(NSIGN+1:NSIGN+1).EQ.' ') FIELD(NSIGN+1:NSIGN+1) = '0'
        NCHARS = NSIGN+2+NDEC
        POINT = .TRUE.
      ELSE IF (NEXP.GT.NSIG)  THEN                ! Very Large Number
        IF (NSIGN+NEXP.GT.LFIELD+1)  GO TO 70
        IF (NSIG.LT.NSIGMAX .AND. IPASS.EQ.1)  THEN
          NSIG = NSIG + 1
          WRITE (FMT,100) NSIG+7,NSIG
          WRITE (TEMP,FMT,ERR=999) TVAL
          READ (TEMP(NSIG+5:),'(I3)',ERR=999) NEXP
          IF (NEXP.LT.NSIG)  THEN
            IPASS = 2
            GO TO 10
          END IF
        END IF
        IF (NSIGN+NEXP.GT.LFIELD)  GO TO 70
        DO 30 I=1,NSIG
  30    FIELD(NSIGN+I:NSIGN+I) = TEMP(3+I:3+I)
        DO 40 I=NSIG+1,NEXP
  40    FIELD(NSIGN+I:NSIGN+I) = '0'
        NCHARS = NSIGN+NEXP
        POINT = .FALSE.
      ELSE                                     ! Moderate Size Number
        DO 50 I=1,NEXP
  50    FIELD(NSIGN+I:NSIGN+I) = TEMP(3+I:3+I)
        FIELD(NSIGN+NEXP+1:NSIGN+NEXP+1) = '.'
        DO 60 I=NEXP+1,NSIG
  60    FIELD(NSIGN+1+I:NSIGN+1+I) = TEMP(3+I:3+I)
        NCHARS = NSIGN+1+NSIG
        POINT = .TRUE.
      END IF
      GO TO 90
C
C  Come here if need E format
C
  70  NDEC = MIN(LFIELD-NSIGN-6,NSIGMAX-1)
      IF (NDEC.LT.1)  GO TO 999
      WRITE (FMT,300) NDEC+7,NDEC
 300  FORMAT ( '(1PE' , I2 , '.' , I2 , ')' )
      WRITE (TEMP,FMT,ERR=999) TVAL
      DO 80 I=1,NDEC+6
  80  FIELD(NSIGN+I:NSIGN+I) = TEMP(1+I:1+I)
      NCHARS = NSIGN+NDEC+6
      POINT = .FALSE.
C
C  Fill tail end of field with blanks
C                                                 
  90  DO 110 I=NCHARS+1,LFIELD
 110  FIELD(I:I) = ' '
      IF (.NOT.POINT)  RETURN
C
C  Remove trailing zeros if appropriate
C
      DO 120 I=NCHARS,NSIGN+2,-1
        IF (FIELD(I:I).EQ.'0')  THEN
          FIELD(I:I) = ' '
        ELSE
          IF(FIELD(I:I).EQ.'.')  THEN
            FIELD(I:I) = ' '
            NCHARS = I - 1
          ELSE
            NCHARS = I
          END IF
          RETURN
        END IF
 120  CONTINUE
      NCHARS = I
      RETURN
C***********************************************************************
      ENTRY NUM2ALFI (IVALUE,FIELD,*,NCHARS)
      IF (IVALUE.EQ.0)  THEN
        NCHARS = 1
        FIELD = '0'
        RETURN
      END IF
      LFIELD = LEN(FIELD)
      IF (LFIELD.GT.NMAX)  GO TO 999
      WRITE (FMT,400) LFIELD
 400  FORMAT ( '(I' , I2 , ')' )
      WRITE (TEMP,FMT,ERR=999) IVALUE
      IF (TEMP(1:1).EQ.'*')  GO TO 999
      NCHARS = 0
      DO 130 I=1,LFIELD
        IF (TEMP(I:I).NE.' ')  THEN
          NCHARS = NCHARS+1
          FIELD(NCHARS:NCHARS) = TEMP(I:I)
        END IF
 130  CONTINUE
      DO 140 I=NCHARS+1,LFIELD
 140  FIELD(I:I) = ' '
      RETURN
C
C  Error return
C
 999  NCHARS = 0
      FIELD = ' '
      RETURN 1
      END
