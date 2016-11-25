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
C  Process name:  DYNCC  (DYNAMIC TRACE SHIFT BY CUBIC INTERPOLATION) 
C        Author:  TOM STOECKLEY , 1 DECEMBER 1988
C  Last revised:  7 May 1993
C
C  Purpose:       Shift a trace dynamically, using an array of input
c                 times and corresponding output times.  Can be used
c                 for NMO, depth conversion, time-variant statics, etc.
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
C 93/05/07  Stoeckley    Fixed one card to keep indices on output trace
C                          from going negative when index arrays have
C                          negative indices.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C                               
C        CALL DYNCC (DOP,N,TA,TB,NVAL,A,   B,BMUTE)
C
C  DOP   = Doppler stretch factor greater than one.
C  N     = Length of index arrays TA and TB.
c  TA(N) = Array of trace indices on input trace.
c  TB(N) = Array of corresponding trace indices on output trace.
C  NVAL    = Number of trace values.
c  A(NVAL) = Input trace (not changed by this routine).
c  B(NVAL) = Output trace (must not be same address as input trace).
C  BMUTE = Mute index (due to doppler mute) to be placed into header
c           word 2.  Output trace is dead above this index.
C_______________________________________________________________________
C                                NOTES
C  Uses cubic (4-point) interpolation.
c  Can be used for any combination of static and dynamic shifts.
c  Input and output trace indices are linearly interpolated within
c      range  (TA(i),TB(i))  to  (TA(i+1),TB(i+1)) .
c  Values of TA and TB beyond the trace boundaries are OK.
c  Output trace is dead before TB(1) and after TB(N).
c  Output trace is dead in regions of stretch or compression exceeding
c      the value of DOP.
c  Output trace is dead in regions of time reversals.
c  Trace indices are related to trace times as follows:
c      INDEX = 1.0 + (TIME-TSTRT)/DT    where TSTRT and DT are globals.
c      These indices can be fractional.
C_______________________________________________________________________
C      LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C                             IN THIS MODULE
C
C  Subroutines, functions, and entry points:  DYNCC.
C
C  Common blocks:  none
C-----------------------------------------------------------------------
C            LIST OF ALL EXTERNALS REFERENCED BY THIS MODULE
C
C                               none
C-----------------------------------------------------------------------
C\END DOC
      SUBROUTINE DYNCC (DOP,N,TA,TB,NVAL,A,   B,BMUTE)
C----------DIMENSION AND DATA STATEMENTS. 
      PARAMETER (GLO=2.)
      DIMENSION TA(N),TB(N),A(NVAL),B(NVAL)
C----------SET UP NEEDED CONSTANTS.
      DOPP3=1./DOP    
      DOPP4=DOP 
      GUP=NVAL-2
      HUP=NVAL
C----------INITIALIZE VARIABLES. 
      DO 2 I=1,NVAL              
2     B(I)=0.
      NFIRST=NVAL 
      HLO=0. 
C----------GO THRU OUTER LOOP.           
      DO 50 J=2,N                                                   
      BLO=TB(J-1)
      BUP=TB(J)
      IF (BUP.LE.BLO) GO TO 50
      ALO=TA(J-1)
      AUP=TA(J)
      SLOPE=(AUP-ALO)/(BUP-BLO) 
      IF (SLOPE.LT.DOPP3.OR.SLOPE.GT.DOPP4) GO TO 50
CCC   ILO=MAX1(BLO+(AMAX1(ALO,GLO)-ALO)/SLOPE,HLO+1.) 
      ILO=MAX1(BLO+(AMAX1(ALO,GLO)-ALO)/SLOPE,HLO)+1
      IUP=MIN1(BLO+(AMIN1(AUP,GUP)-ALO)/SLOPE,HUP)
cccc  HLO=IUP         this card replaced by the following card 5/7/93
      HLO=MAX0(IUP,0)
      IF (IUP.LT.ILO) GO TO 50
      NFIRST=MIN0(NFIRST,ILO) 
      AAA=ALO+(ILO-BLO)*SLOPE-SLOPE
C----------GO THRU INNER LOOP.
      DO 41 I=ILO,IUP 
      AAA=AAA+SLOPE   
      L=AAA
      F=AAA-L
      G=1.-F
      B(I)=(   -    G*G*F             * A(L-1)
     $         +   (G*G*F + G*G + G)  * A(L  )
     $         +   (F*F*G + F*F + F)  * A(L+1)
     $         -    F*F*G             * A(L+2)    ) / (F*F + G*G + 1.)
41    CONTINUE
50    CONTINUE
C----------SET MUTE HEADER WORD.
      BMUTE=NFIRST 
      RETURN
      END 
