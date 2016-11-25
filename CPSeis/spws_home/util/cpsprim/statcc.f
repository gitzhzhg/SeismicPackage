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
C  Process name:  STATCC  (STATIC TRACE SHIFT BY CUBIC INTERPOLATION) 
C        Author:  TOM STOECKLEY , 21 NOVEMBER 1988
C  Last revised:                              
C
C  Purpose:       Shift a trace by a specified amount.
C
C-----------------------------------------------------------------------
C                   INPUT PARAMETERS: Passed as arguments
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C        
C        CALL STATCC (SHIFT,N,A,B)            
C
C  SHIFT = Static shift in sample point units (Can be fractional).
c          (Positive value shifts trace down.)  
C  N = Number of trace samples to shift.
C  A = Starting address of trace to shift (not changed by this routine).
C  B = Starting address of shifted trace (must be different address
c      from A).
C_______________________________________________________________________
C                                NOTES
C  1. This routine uses cubic (4-point) interpolation.  The trace
c      amplitude and its derivative are continuous. 
C_______________________________________________________________________
C      LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C                             IN THIS MODULE
C
C  Contains the following entry point:  STATCC.
C-----------------------------------------------------------------------
C            LIST OF ALL EXTERNALS REFERENCED BY THIS MODULE
C                              none
C_______________________________________________________________________
C\END DOC
      SUBROUTINE STATCC (SHIFT,N,A,B) 
C----------DIMENSION STATEMENTS.
      DIMENSION A(N),B(N) 
C----------GET STARTED. 
      NN=N
      SHFT=SHIFT 
C----------SHIFT IS NEGATIVE. 
      IF (SHFT.LT.0.) THEN
           L=-SHFT 
           F=-SHFT-L 
           G=1.-F
           LA=L-1
C----------SHIFT IS POSITIVE. 
      ELSE IF (SHFT.GT.0.) THEN
           L=-SHFT 
           G=SHFT+L                  
           F=1.-G
           LA=L-2
C----------SHIFT IS ZERO. 
      ELSE
           DO 10 I=1,NN
10         B(I)=A(I)    
           RETURN
      END IF
C----------GET WEIGHTS. 
      LB=LA+1 
      LC=LA+2 
      LD=LA+3 
      F2=F**2 
      G2=G**2 
      DENOM=F2+G2+1.
      WA=-G2*F/DENOM
      WB=(G2*F+G2+G)/DENOM
      WC=(F2*G+F2+F)/DENOM
      WD=-F2*G/DENOM
C----------GET READY TO APPLY SHIFT. 
      I1=MAX0(1,1-LA) 
      I2=MIN0(NN,NN-LD) 
      IF (I1.GT.I2) THEN         !   CALL CLEAR (B,NN)
           DO 6 I=1,NN
6          B(I)=0.
           RETURN
      END IF
      IF (I1.GT.1) THEN          !   CALL CLEAR (B(1),I1-1) 
           DO 7 I=1,I1-1
7          B(I)=0.
      END IF
      IF (I2.LT.N) THEN          !   CALL CLEAR (B(I2+1),NN-I2)
           DO 8 I=I2+1,NN
8          B(I)=0.
      END IF
C----------APPLY SHIFT. 
      DO 30 I=I1,I2 
30    B(I)=WA*A(I+LA)+WB*A(I+LB)+WC*A(I+LC)+WD*A(I+LD)
      RETURN
      END 
