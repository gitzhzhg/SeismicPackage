        INTEGER FUNCTION JULIAN (JYR,JMO,JDA,JHO,JMI,JSE) 
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
C  Process name: JULIAN [seconds]
C        Author: John B Sinton
C  Last revised: 88/06/15
C  Revisions:    98/02/01
C  2. 97/10/16  RS Day     Base date changed from 1900 to 1968
C                          so we won't overflow on 32bit machines
C                          2 digit years < 68 will be placed in 2000,
C                          otherwise in 1900s. 4 digit years are honored.
C  1. 88/06/15; JB Sinton, First version.
C
C  Purpose: To compute Julian seconds relative to a base date
C
C-----------------------------------------------------------------------
C\END DOC
C
      INTEGER JYR,JMO,JDA,JDAY,JHO,JMI,JSE
      INTEGER BDATE
      BDATE=1968
      IF(JYR.LT.68) THEN
        JYR=JYR+2000
      ELSE IF(JYR.LE.99) THEN
        JYR=JYR+1900
      ENDIF
      IF (JYR.GT.BDATE) JYR = JYR-BDATE
      NLDAY = 1 + JYR/4
      JDAY = JDA
      DO I=1,JMO-1
         if      (i.eq.2) then
          jday = 28+jday       !month=Febuary.
         else if (i.eq.4.or.i.eq.6.or.i.eq.9.or.i.eq.11) then
          jday = 30+jday       !month=Sept., April, June, or Novem.
         else
          jday = 31+jday
         endif
      ENDDO
      julian = nlday*86400 +        !# of seconds in leap day.
     +         jyr  *31536000 +     !# of seconds in year.
     +         jday *86400 +        !# of seconds in a day.
     +         jho  *3600 +         !# of seconds in an hour.
     +         jmi  *60   +         !# of seconds in a minute.
     +         jse                  !# of seconds.
        RETURN
        END

