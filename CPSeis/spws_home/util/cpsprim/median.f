       SUBROUTINE MEDIAN (ARRAY,N,AMED,GUESS)
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
C                        CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                               CONOCO INC.
C
C                        C P S   P R I M I T I V E
C
CPrimitive name:  MEDIAN
C        Author:  Bob Baumel  (earlier version by Mike Howard)
C  Last revised:  89/06/29
C
C  Purpose:  Find the median value of a list of unsorted values.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C       CALL MEDIAN (ARRAY,N,AMED [,GUESS] )
C
C Name   Type*   Valid     Description      *Type: I=IN, O=OUT, B=BOTH
C ----   ----    -----     -----------
C ARRAY    I   real-array  Array of values for which median is desired.
C N        I     int>=0    Number of values in ARRAY.
C AMED     O      real     Returned median value.
C GUESS    I      real     OPTIONAL initial guess for the median.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. This routine uses an iterative method to find the median.  Given
C    a guess for the median, it counts the numbers of values bigger and
C    smaller than the guess, and uses this data to update the guess.
C
C 2. The algorithm USUALLY converges to the median in order LOG N iter-
C    ations, but for "pathologically" distributed data, it COULD take
C    as many as N-1 iterations.
C
C 3. You may optionally specify a starting guess for the iteration by
C    specifying a 4th argument to the subroutine.  If you call the
C    routine with only 3 arguments, the starting guess will be the
C    average of the biggest and smallest values in ARRAY.
C
C 4. The current version of this routine is based loosely on an earlier
C    version by Mike Howard which was modified from an algorithm in the
C    book "Numerical Recipes" by William H. Press, et al.  The algorithm
C    has now been extensively reworked and no longer includes the book's
C    original iteration formula!  The previous version of this routine
C    did not always return the correct answer!
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C 89/06/29  Bob Baumel   New Algorithm.
C 89/03/22  Harold Ball  Moved from OLD CPS to current system.
C 88/05/12  Mike Howard  OLD CPS version.
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES IN THIS MODULE
C     Subroutine:  MEDIAN
C-----------------------------------------------------------------------
C                  EXTERNALS REFERENCED BY THIS MODULE
C  ISMIN   ISMAX   NUMARG
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C  STORAGE       - 0
C  HEAP(dynamic) - 0
C-----------------------------------------------------------------------
C\END DOC
       REAL ARRAY(N)
       IF (N.EQ.0) THEN
         AMED = 0.
         RETURN
       ENDIF
       I1 = ISMIN(N,ARRAY,1)
       A1 = ARRAY(I1)
       I2 = ISMAX(N,ARRAY,1)
       A2 = ARRAY(I2)
       IF (A1.EQ.A2) THEN
         AMED = A1
         RETURN
       ENDIF
C      IF (NUMARG().EQ.4) THEN
C        AA = GUESS
C      ELSE
         AA = (A1+A2) * 0.5
C      ENDIF
       N1 = 1
       N2 = N
  10   CONTINUE
       NLE = 0
       X1 = A1
       X2 = A2
       DO 20 J=1,N
         IF (ARRAY(J).LE.AA) THEN
           NLE = NLE + 1
           X1 = MAX(X1,ARRAY(J))
         ELSE
           X2 = MIN(X2,ARRAY(J))
         ENDIF
  20   CONTINUE
       NLE2 = 2 * NLE
       IF (NLE2.LT.N-1) THEN
         A1 = X2
         N1 = NLE + 1
         GOTO 30
       ELSEIF (NLE2.GT.N+1) THEN
         A2 = X1
         N2 = NLE
         GOTO 30
       ELSEIF (NLE2.EQ.N) THEN
         AMED = (X1+X2) * 0.5
       ELSEIF (NLE2.LT.N) THEN
         AMED = X2
       ELSE
         AMED = X1
       ENDIF
       RETURN
  30   IF (A1.EQ.A2) THEN
         AMED = A1
         RETURN
       ENDIF
       AA = A1 + (A2-A1)*REAL(N+1-2*N1)/REAL(2*(N2-N1))
       GOTO 10
       END
