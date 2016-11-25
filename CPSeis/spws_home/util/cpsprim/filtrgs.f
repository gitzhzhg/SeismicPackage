       SUBROUTINE FILTRGS (A,LA,B,LB,O,LO,IFLAG,ISHIFT)
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
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO INC.
C
C                        C P S   P R I M I T I V E
C
CPrimitive name: FILTRGS
C        Author: Bob Baumel
C  Date Written: 90/12/07
C  Last revised: 90/12/07
C
C    Purpose: This is a FORTRAN version of the Cray-supplied (BENCHLIB)
C             routine FILTGS which does correlations and convolutions.
C             The real FILTGS (written in CAL) is faster, but sometimes
C             results in OPERAND RANGE ERRORS.  If your process has a
C             tendency to bomb in FILTGS, then try replacing calls to
C             FILTGS by FILTRGS, which is slower but safer.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C           CALL FILTRGS (A,LA,B,LB,O,LO,IFLAG,ISHIFT)
C
C Where:
C Name    Type*  Valid   Description       *Type: I=IN, O=OUT, B=BOTH
C ----    ----   -----   -----------
C A        I     real    FILTER array.
C LA       I     int>0   Length of "A" array.
C B        I     real    DATA array.
C LB       I     int>0   Length of "B" array.
C O       O,B*   real    OUTPUT array (*Type depends on IFLAG setting).
C LO       I     int>0   Length of "O" array.
C IFLAG    I     int     = 0:  Correlation result is summed with
C                              PREVIOUS contents of "O" array;
C                        else: Correlation result returned in "O".
C ISHIFT   I     int     Shift before correlating (see below).
C
C                       ACTION OF SUBROUTINE
C
C             O(i)   =   SUM  A(j) * B(j+i-1+ISHIFT)
C                         j
C
C   NOTE: As is clear from the above formula, this subroutine actually
C   does CORRELATION rather than convolution.  If you want convolution,
C   then before calling FILTRGS, reverse the order of the filter ("A")
C   coefficients, and figure out the appropriate value of ISHIFT.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date      Author    Description
C     ----      ------    -----------
C  1. 90/12/07  B Baumel  Took the equivalent Fortran code for FILTGS
C                         (which was in the FILTGS doc file for over a
C                         year) and made it into CPS primitive FILTRGS.
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES IN THIS MODULE
C Subroutine: FILTRGS
C-----------------------------------------------------------------------
C                  EXTERNALS REFERENCED BY THIS MODULE
C None
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C
C  STORAGE       - 0
C  HEAP(dynamic) - 0
C-----------------------------------------------------------------------
C\END DOC
       DIMENSION A(LA),B(LB),O(LO)
       IF (IFLAG .NE. 0)  THEN
          DO 1 I=1,LO
   1      O(I) = 0.
       END IF
       DO 3 J=1,LA
          JSHIFT = (ISHIFT-1) + J
          DO 2 I=MAX(1,1-JSHIFT),MIN(LO,LB-JSHIFT)
             O(I) = O(I) + A(J)*B(I+JSHIFT)
   2      CONTINUE
   3   CONTINUE
       RETURN
       END
