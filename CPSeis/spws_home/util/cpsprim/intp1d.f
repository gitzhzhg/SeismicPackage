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
C  Process name: Interpolate
C        Author: John B. Sinton
C  Last revised: 88/06/18
C  Revisions:
C  7. 88/06/18; B Baumel; Make INTP1D still more efficient.
C  6. 88/05/17; JB Sinton; Improved efficiency of INTP1D (see notes).
C  5. 87/12/10; JB Sinton; Corrected Bracewell interpolators.
C  4. 87/12/10; DW Hanson add CINTBRC entry for complex vectors.
C  3. 87/11/11; Tom Hill and B Baumel; Improve efficiency of INT2DR and
C               INT2DI entries.
C  2. 86/10/29; B  Baumel; Put in correct version of bilinear interpola-
C               tion; correct documentation for INT2DR, INT2DI.
C  1. 86/07/23; JB Sinton; Add Bracewell interpolation.
C
C  Purpose: To interpolate an function.  
C
C  1. Entry point INTP1D: Interpolate a 1D function Y(X) to a new 1D 
C     function YP(XP). This is a linear interpolation. Y(X) can be
C     an unevely sampled function.  YP(XP) is evenly sampled.        
C                        
C  2. Entry point INT2DR: Interpolate a 3D function TAB(Z,XB,YB) to 
C     a 1D function F(Z,X=XF,Y=YF).   This entry point is for bi-
C     linearly interpolating REAL functions.  TAB(Z,XB,YB) can be an
C     unevenly sampled function in XB and YB.  Interpolation is per-
C     formed only on the variables XB and YB.
C
C  3. Entry point INT2DI: Interpolate a 3D function ITAB(Z,XB,YB) to 
C     a 1D function IF(Z,X=XF,Y=YF). This entry point is for bi-
C     linearly interpolating INTEGER functions.  ITAB(Z,XB,YB) can be
C     an unevenly sampled function in XB and YB.  Interpolation is
C     performed only on the variables XB and YB.
C
C  4. Entry point INTERP: Linearly interpolate evenly sampled 1D data 
C     by a constant stride. Both input and output functions must 
C     be evenly sampled.
C
C  5. Entry point INTBRC: Interpolate evenly sampled 1D data using
C     a 5 point Bracewell convolutional operator.  This interpolates
C     using a cubic spline approximation to a sinc function.  Both input
C     and output functions must be evenly samples.
C
C-----------------------------------------------------------------------
C                                 NOTES
C  1. See the programmers documentation for detailed use of these 
C     routines.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C        CALL INTP1D (X,Y,N,XP,YP,NP,XS,XE)          1D linear uneven.
C        CALL INT2DR (XB,YB,NX,NY,TAB,NR,XF,YF,F)    Bi-linear uneven.
C        CALL INT2DI (XB,YB,NX,NY,ITAB,NI,XF,YF,IF)  Bi-linear uneven.
C        CALL INTERP (INTRP,NT,IFLG,H,G,TR,ETR)      1D linear even.
C        CALL INTBRC (INTRP,NT,IFLG,B,TR,ETR)        1D Bracewell even.
C        CALL CINTBRC(INTRP,NT,IFLG,B,TR,ETR)        1D Complex Bracewell even.
C
C  Arguments for INTP1D:
C  X      - Input array of X coordinates for Y.
C  Y      - Input array of values to interpolate with.
C  N      - # of values in X and Y.
C           NOTE: X must be ordered from the smallest to largest value.
C  XP     - Output array of X coordinates for YP.
C  YP     - Output array of interpolate values.
C  NP     - # of values in XP and YP.
C  XS     - Starting XP value.
C  XE     - Ending XP value.
C  Arguments for INT2DR:
C  XB     - Input array of X coordinates for TAB.
C  YB     - Input array of Y coordinates for TAB.
C  NX     - # of values in XB.
C  NY     - # of values in YB.
C  TAB    - Real 3D matrix of values to interpolate with. 
C           Dimensioned to NR*NX*NY.
C  NR     - # of output values in F.                                   
C  XF     - Input X coordinate to interpolate TAB to.
C  YF     - Input Y coordinate to interpolate TAB to.
C  F      - Output array of interpolated values (size=NR).
C  Arguments for INT2DI:
C  XB     - Input array of X coordinates for ITAB.
C  YB     - Input array of Y coordinates for ITAB.
C  NX     - # of values in XB.
C  NY     - # of values in YB.
C  ITAB   - Integer 3D matrix of values to interpolate with. 
C           Dimensioned to NR*NX*NY.
C  NR     - # of output values in F.
C  XF     - Input X coordinate to interpolate ITAB to.
C  YF     - Input Y coordinate to interpolate ITAB to.
C  IF     - Output array of interpolated values.
C  Arguments for INTERP:
C  INTRP  - Interpolation factor, eg. =4 then compute NT*4 values.
C  NT     - # of values in array TR.
C  IFLG   - Processing switch.
C           =-1, then set up H and G and return.
C           = 0, then set up H and G, interpolate TR and return.
C           = 1, then use previous H and G to interpolate TR.
C  H      - Work array, do not alter. Must be dimensioned INTRP.
C  G      - Work array, do not alter. Must be dimensioned INTRP.
C  TR     - Input array of values. ==>Dimensioned to NT+1.<==
C  ETR    - Output array of interpolated values (size=INTRP*NT).
C  Arguments for INTBRC:
C  INTRP  - Interpolation factor, eg. =4 then compute NT*4 values.
C  NT     - # of values in array TR.
C  IFLG   - Processing switch.
C           =-1, then set up H and G and return.
C           = 0, then set up H and G, interpolate TR and return.
C           = 1, then use previous H and G to interpolate TR.
C  B      - Work array, do not alter. Must be dimensioned INTRP*5.
C  TR     - Input array of values.  ==>Dimensioned to NT+2.<==
C  ETR    - Output array of interpolated values (size=INTRP*NT).
C  Arguments for CINTBRC:
C  INTRP  - Interpolation factor, eg. =4 then compute NT*4 values.
C  NT     - # of values in array TR.
C  IFLG   - Processing switch.
C           =-1, then set up H and G and return.
C           = 0, then set up H and G, interpolate TR and return.
C           = 1, then use previous H and G to interpolate TR.
C  B      - Work array, do not alter. Must be dimensioned INTRP*5.
C  CTR    - Complex input array of values.  ==>Dimensioned to NT+2.<==
C  CETR   - Complex output array of interpolated values (size=INTRP*NT).
C
C_______________________________________________________________________
C                         MEMORY REQUIREMENTS
C  Storage - none
C  Scratch - none
C  Parms   - none
C-----------------------------------------------------------------------
C                                 NOTES
C
C  1. These routines do not call any other routines.
C  2. To use the entry points INTP1D, INT2DR, and INT2DI the user must
C     build the input function, Y, TAB, and ITAB first.  Then each time
C     call one of these routines each time an interpolated 1D output 
C     function is desired.  The arrays TAB and ITAB are set up so that
C     the Z coordinate index comes first, then the X coordinate, and 
C     last the Y coordinate. See diagram below.
C                                           Y
C                  +----+-----+--+-------+  1
C                  +----+-----+--+-------+  2
C                  |    |     |  |       |  |
C                  +----+-----+--*-------*  3
C                  |    |     |  |       |  |
C                  |    |     |  |   .   |  |
C                  |    |     |  |       |  |
C                  +----+-----+--*-------*  4
C             X----1----2-----3--4-------5--+-->
C                                           |
C                                           V
C
C     Below each "+" hangs NR values on a even grid in the Z coor-
C     dinate.  Note that the X/Y grid can be rectangular.
C     If the "." represents the position where you need to inter-
C     polate (I)TAB to then INT2DR(I) will use the values of (I)TAB
C     at the "*"'s to complete the bi-linear interpolation.  At 
C     each Z-coordinate level 4 values will be used to obtain the
C     corresponding value at the "." position.
C     
C  3. To use the entry points INTERP and INTBRC the user must initially 
C     call either routine with IFLG=-1 or =0.  This will set up the 
C     working arrays for these interpolators.  After the first call 
C     these routines can be call again with the same value of INTRP 
C     without setting up the work arrays.  The work arrays should NOT 
C     be altered any time after they have been set up.
C
C  4. N O T E       ====> VERY IMPORTANT <====
C     The TR array in INTERP must be dimensioned to NT+1.
C     The TR array in INTBRC must be dimensioned to NT+2.
C-----------------------------------------------------------------------
C\END DOC
C
      SUBROUTINE INTP1D (X,Y,N,XP,YP,NP,XS,XE)
      DIMENSION X(*),Y(*),XP(*),YP(*)
      DIMENSION XB(*),YB(*),F(*),IF(*),TAB(NR,NX,NY),ITAB(NI,NX,NY)
      DIMENSION TR(*),ETR(*),H(*),G(*),B(INTRP,*)
      COMPLEX CTR(*),CETR(*) ,C1,C2
      IF (NP.GT.1)  THEN
        DXP = (XE-XS)/(NP-1)
      ELSE
        DXP = 0.
      END IF
      DO 5 IP=1,NP
        XP(IP) = XS + (IP-1)*DXP
  5   CONTINUE
      ISI = 1
      DO 15 IP=1,NP
C-----------------------------------------------------------------------
C   Note: The following search loop (which increments the variable ISI)
C   is intentionally written as a NON-VECTOR loop, as it typically goes
C   through too few iterations to justify the overhead of a vector loop.
C-----------------------------------------------------------------------
 10     IF (XP(IP).GT.X(ISI))  THEN
          IF (ISI.EQ.N)  GO TO 20
          ISI = ISI + 1
          GO TO 10
        END IF
        IF (ISI.EQ.1)  THEN
          YP(IP) = Y(1)
        ELSE
          YP(IP) = Y(ISI-1) + (XP(IP)-X(ISI-1))*(Y(ISI)-Y(ISI-1))
     *                                         /(X(ISI)-X(ISI-1))
        END IF
 15   CONTINUE
      RETURN
 20   DO 25 JP=IP,NP
        YP(JP) = Y(N)
 25   CONTINUE
      RETURN
C                     
C************************************************
      ENTRY INT2DR (XB,YB,NX,NY,TAB,NR,XF,YF,F)
C************************************************
C
C  ****  INTEPOLATE Y LOCATION YF FROM YB(NY)
      YFL = MIN ( MAX(YF,YB(1)) , YB(NY) )
      DO 302 IL2=2,NY
        IF(YFL.LT.YB(IL2)) GOTO 304
 302  CONTINUE
      IL2 = NY
      IL1 = NY
      FAC1 = 0.0
      GO TO 305
 304  CONTINUE
      IL1 = IL2 - 1
      FAC1 = (YFL-YB(IL1)) /(YB(IL2)-YB(IL1))

C  ****  INTERPOLATE X LOCATION XF FROM XB(NX)
 305  XFL = MIN ( MAX(XF,XB(1)) , XB(NX) )
      DO 306 IB2 = 2, NX
        IF(XFL.LT.XB(IB2)) GOTO 308
 306  CONTINUE
      IB2 = NX
      IB1 = NX
      FAC2 = 0.0
      GO TO 309
 308  CONTINUE
      IB1 = IB2 - 1
      FAC2 = (XFL-XB(IB1)) / (XB(IB2)-XB(IB1))

C  ****  GET INTERPOLATED REAL FUNCTION FROM TABLE
 309  FX4 = FAC1*FAC2
      FX3 = FAC1 - FX4
      FX2 = FAC2 - FX4
      FX1 = 1.0 - FAC1 - FX2
      DO 310 J = 1, NR
        F(J) =   FX1*TAB(J,IB1,IL1) +
     +           FX2*TAB(J,IB2,IL1) +
     +           FX3*TAB(J,IB1,IL2) +
     +           FX4*TAB(J,IB2,IL2)
  310 CONTINUE
      RETURN
C
C**************************************************
      ENTRY INT2DI (XB,YB,NX,NY,ITAB,NI,XF,YF,IF)
C**************************************************
C
C  ****  INTEPOLATE Y LOCATION YF FROM YB(NY)
      YFL = MIN ( MAX(YF,YB(1)) , YB(NY) )
      DO 402 IL2=2,NY
        IF(YFL.LT.YB(IL2)) GOTO 404
 402  CONTINUE
      IL2 = NY
      IL1 = NY
      FAC1 = 0.0
      GO TO 405
 404  CONTINUE
      IL1 = IL2 - 1
      FAC1 = (YFL-YB(IL1)) /(YB(IL2)-YB(IL1))

C  ****  INTERPOLATE X LOCATION XF FROM XB(NX)
 405  XFL = MIN ( MAX(XF,XB(1)) , XB(NX) )
      DO 406 IB2 = 2, NX
        IF(XFL.LT.XB(IB2)) GOTO 408
 406  CONTINUE
      IB2 = NX
      IB1 = NX
      FAC2 = 0.0
      GO TO 409
 408  CONTINUE
      IB1 = IB2 - 1
      FAC2 = (XFL-XB(IB1)) / (XB(IB2)-XB(IB1))

C  ****  GET INTERPOLATED INTEGER FUNCTION FROM TABLE
 409  FX4 = FAC1*FAC2
      FX3 = FAC1 - FX4
      FX2 = FAC2 - FX4
      FX1 = 1.0 - FAC1 - FX2
      DO 410 J = 1, NI
        IF(J) =   FX1*ITAB(J,IB1,IL1) +
     +            FX2*ITAB(J,IB2,IL1) +
     +            FX3*ITAB(J,IB1,IL2) +
     +            FX4*ITAB(J,IB2,IL2)
  410 CONTINUE
      RETURN
C
C************************************************
      ENTRY INTERP(INTRP,NT,IFLG,H,G,TR,ETR)
C************************************************
C
C     IF (IFLG.LE.0 .OR. NUMARG().EQ.5) THEN
      IF (IFLG.LE.0 ) THEN
       DO 500 I=1,INTRP
        H(I) = 1.-(I-1.)/INTRP
        G(I) = 1.-H(I)
 500   CONTINUE
C      IF (IFLG.LT.0 .OR. NUMARG().EQ.5) RETURN
       IF (IFLG.LT.0 ) RETURN
      ENDIF
      TR(NT+1) = TR(NT)
      DO 501 K=1,INTRP
       L = K
       DO 501 I=1,NT
        ETR(L) = H(K)*TR(I)+G(K)*TR(I+1)
        L = L+INTRP
 501  CONTINUE
      RETURN
C
C************************************************
      ENTRY INTBRC (INTRP,NT,IFLG,B,TR,ETR)
C************************************************
C
C     IF (IFLG.LE.0 .OR. NUMARG().EQ.4) THEN
      IF (IFLG.LE.0 ) THEN
       RINTRP = 1./INTRP
       DO 600 I=1,INTRP
 600   B(I,1) = 0.0
       DO 610 J=2,5
        IF      (J.EQ.2.OR.J.EQ.5) THEN
         DO 601 I=1,INTRP
          D = ABS(J-3.-(I-1)*RINTRP)
          B(I,J) = 2. - 4*D + 2.5*D**2 - 0.5*D**3
 601     CONTINUE
        ELSE IF (J.EQ.3.OR.J.EQ.4) THEN
         DO 602 I=1,INTRP
          D = ABS(J-3.-(I-1)*RINTRP)
          B(I,J) = 1. -       2.5*D**2 + 1.5*D**3
 602     CONTINUE
        ENDIF
 610   CONTINUE
c       DO 611 I=1,INTRP
c 611   PRINT '('' =>INTBRC; i='',i3,5f6.2)', i,(b(i,j),j=1,5)
C      IF (IFLG.LT.0 .OR. NUMARG().EQ.4) RETURN
       IF (IFLG.LT.0 ) RETURN
      ENDIF
      TR(NT+1) = TR(NT)
      TR(NT+2) = TR(NT)
C
C**********************************************************
C S1 and S2 are linear extrapolations of 1st 2 data points.
      S1 = 2.*TR(1)-   TR(2)
      S2 = 3.*TR(1)-2.*TR(2)
C**********************************************************
C
      DO 620 K=1,INTRP
       L = k 
       ETR(L) = B(K,1)*S2      + B(K,2)*S1      + B(K,3)*TR(1) +
     +          B(K,4)*TR(2  ) + B(K,5)*TR(2  )
       L = L+INTRP
       ETR(L) = B(K,1)*S1      + B(K,2)*TR(1  ) + B(K,3)*TR(2) +
     +          B(K,4)*TR(3  ) + B(K,5)*TR(4  )
       L = L+INTRP
       DO 620 I=3,NT
        ETR(L) = B(K,1)*TR(I-2) + B(K,2)*TR(I-1) + B(K,3)*TR(I) +
     +           B(K,4)*TR(I+1) + B(K,5)*TR(I+2)
        L = L+INTRP
 620  CONTINUE
      RETURN
C
C************************************************
      ENTRY CINTBRC (INTRP,NT,IFLG,B,CTR,CETR)
C************************************************
C
C     IF (IFLG.LE.0 .OR. NUMARG().EQ.4) THEN
      IF (IFLG.LE.0 ) THEN
       RINTRP = 1./INTRP
       DO 700 I=1,INTRP
 700   B(I,1) = 0.0
       DO 710 J=2,5
        IF      (J.EQ.2.OR.J.EQ.5) THEN
         DO 701 I=1,INTRP
          D = ABS(J-3.-(I-1)*RINTRP)
          B(I,J) = 2. - 4*D + 2.5*D**2 - 0.5*D**3
 701     CONTINUE
        ELSE IF (J.EQ.3.OR.J.EQ.4) THEN
         DO 702 I=1,INTRP
          D = ABS(J-3.-(I-1)*RINTRP)
          B(I,J) = 1. -       2.5*D**2 + 1.5*D**3
 702     CONTINUE
        ENDIF
 710   CONTINUE
c       DO 711 I=1,INTRP
c 711   PRINT '('' =>INTBRC; i='',i3,5f6.2)', i,(b(i,j),j=1,5)
C      IF (IFLG.LT.0 .OR. NUMARG().EQ.4) RETURN
       IF (IFLG.LT.0 ) RETURN
      ENDIF
      CTR(NT+1) = CTR(NT)
      CTR(NT+2) = CTR(NT)
C
C**********************************************************
C C1 and C2 are linear extrapolations of 1st 2 data points.
      C1 = 2.*CTR(1)-   CTR(2)
      C2 = 3.*CTR(1)-2.*CTR(2)
C**********************************************************
C
      DO 720 K=1,INTRP
       L = K 
       CETR(L) = B(K,1)*C2       + B(K,2)*C1      + B(K,3)*CTR(1) +
     +           B(K,4)*CTR(2  ) + B(K,5)*CTR(2  )
       L = L+INTRP
       CETR(L) = B(K,1)*C1       + B(K,2)*CTR(1  ) + B(K,3)*CTR(2) +
     +           B(K,4)*CTR(3  ) + B(K,5)*CTR(4  )
       L = L+INTRP
       DO 720 I=3,NT
        CETR(L) = B(K,1)*CTR(I-2) + B(K,2)*CTR(I-1) + B(K,3)*CTR(I) +
     +            B(K,4)*CTR(I+1) + B(K,5)*CTR(I+2)
        L = L+INTRP
 720  CONTINUE
      RETURN
      END
