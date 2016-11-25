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
C                       C P S   P R I M I T I V E
C
C  Primitive name:  VELGEN    (GENERATE VELOCITY FUNCTION TYPES)
C          Author:  TOM STOECKLEY , 13 JUNE 1989
C    Last revised:  
C
C  Purpose:         This primitive (on both the CRAY and the VAX) is
C                   used to generate several types of velocity functions
C                   from one input type.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C                                                            
C         CALL VELGEN (TYPE,N,X,V,   T,DEPTH,VRMS,VAV,VINT,IERR)
C
C  Name    Type    Valid     Description   (Type: I=IN, O=OUT, B=BOTH)
C  ----    ----    -----     -----------
C  TYPE    IN                Type of input velocity function (char*4).
C                            (On CRAY, this can be char*8 or hollerith.)
C                  VTRM  -->      RMS VELOCITY versus 2-WAY TIME.
C                  VTAV  -->  AVERAGE VELOCITY versus 2-WAY TIME.
C                  VTIN  --> INTERVAL VELOCITY versus 2-WAY TIME.
C                  VZRM  -->      RMS VELOCITY versus DEPTH.
C                  VZAV  -->  AVERAGE VELOCITY versus DEPTH.
C                  VZIN  --> INTERVAL VELOCITY versus DEPTH.
C                  VTNM  --> NMO (STACKING) VELOCITY versus 2-WAY TIME.
C                            See notes 1 and 2.
C                  
C  N       IN      >=2       Number of velocity/time or velocity/depth
C                            pairs (length of all arrays).
C
C  X       IN                For TYPE=VT**, array of 2-way times.
C                            For TYPE=VZ**, array of depths.
C                            (** means any 2 characters)
C
C  V       IN                For TYPE=**RM, array of RMS velocities.
C                            For TYPE=**AV, array of AVERAGE velocities.
C                            For TYPE=**IN, array of INTERVAL velocities.
C                            For TYPE=**NM, array of NMO velocities.
C
C  T       OUT               Array of times (same as X for TYPE=VT**).
C
C  DEPTH   OUT               Array of depths (same as X for TYPE=VZ**).
C
C  VRMS    OUT               Array of RMS vels (same as V for TYPE=**RM
C                            or TYPE=**NM).
C
C  VAV     OUT               Array of AV vels (same as V for TYPE=**AV).
C
C  VINT    OUT               Array of INT vels (same as V for TYPE=**IN).
C
C  IERR    OUT               Error condition.  Set to 1 if an error
C                            occurs during conversion.  Otherwise set
C                            to 0.  See notes 3 and 4.
C_______________________________________________________________________
C                                NOTES
C
C  1. This primitive is identical on the CRAY and the VAX, with the
C     exception that TYPE can be either type CHARACTER or Hollerith
C     (type INTEGER or REAL) on the Cray, but must be type CHARACTER
C     on the VAX.
C
C  2. Note that the velocity function types begin with the characters
C     VT or VZ, depending on whether the picks are versus time or
C     depth.  Also note that there is no allowance for NMO (stacking)
C     velocity versus depth.  If TYPE=VTNM is specified, the conversion
C     is performed as if TYPE=VTRM (that is, a stacking velocity
C     function is treated as if it were an RMS velocity function).
C
C  3. If an error occurs upon conversion, then (in addition to setting
C     IERR to 1) some of the output values will be set to zero.  All
C     output values can be printed, which means that IERR need not be
C     interrogated if the user intends to use the converted velocities
C     for a display only. 
C
C  4. A conversion error (causing IERR to be set to 1) will occur if:
C        (a) TYPE is invalid.
C        (b) Depth or time is negative.
C        (c) Depth or time is not monotonically increasing.
C        (d) Velocity is zero or negative.
C        (e) Conversion from RMS or NMO velocity causes a negative
C                argument for the SQRT function.
C
C  5. This primitive does not require that the first time (or depth)
C     pick be zero.  It is permissible for the first pick to
C     correspond to the bottom of the first layer.
C
C  6. This primitive is used on the VAX by VELF and VMCC, and on the
C     CRAY by HADC.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY, AND COMMON BLOCK NAMES IN THIS MODULE
C
C  Subroutines:     VELGEN
C  Functions:       none
C  Entry points:    none
C  Common blocks:   none
C_______________________________________________________________________
C                 EXTERNALS REFERENCED BY THIS MODULE
C
C                              none
C-----------------------------------------------------------------------
C                       MEMORY REQUIREMENTS
C
C  Storage         -  0
C  Heap (dynamic)  -  0
C-----------------------------------------------------------------------
C\END DOC
      SUBROUTINE VELGEN (TYPE,N,X,V,   T,DEPTH,VRMS,VAV,VINT,IERR)
      DIMENSION X(N),V(N),T(N),DEPTH(N),VRMS(N),VAV(N),VINT(N)
      CHARACTER*4 TYPE  
C----------GET STARTED.
      IERR=0
      VRMS(1)=V(1)
      VAV(1)=V(1) 
      VINT(1)=V(1)
C----------GET THE TIME FOR THE FIRST PICK.
      IF (V(1).LE.0..OR.X(1).LT.0.) THEN
           IERR=1
           T(1)=0.
      ELSE IF (TYPE(1:2).EQ.'VT') THEN
           T(1)=X(1) 
      ELSE IF (TYPE(1:2).EQ.'VZ') THEN
           T(1)=2.*X(1)/V(1) 
      ELSE
           IERR=1
           T(1)=0.
      END IF
C----------FINISH UP WITH THE FIRST PICK.
      DEPTH(1)=V(1)*T(1)/2. 
      IF (N.LE.1) THEN
           IERR=1
           RETURN
      END IF
C----------GO THROUGH THE LOOP. 
      DO 50 K=2,N 
      L=K-1 
C----------GET THE TIME.
      IF (V(K).LE.0.) THEN
           IERR=1
           T(K)=0.
      ELSE IF (TYPE(1:2).EQ.'VT') THEN
           T(K)=X(K) 
      ELSE IF (TYPE.EQ.'VZIN') THEN
           T(K)=T(L)+2.*(X(K)-X(L))/V(K) 
      ELSE IF (TYPE.EQ.'VZAV') THEN
           T(K)=2.*X(K)/V(K) 
      ELSE IF (TYPE.EQ.'VZRM') THEN
           AAAA=V(K)**2
           BBBB=-T(L)*(V(K)**2+V(L)**2)/2.
           CCCC=T(L)**2*V(L)**2/4.-(X(K)-X(L))**2
           ABCD=BBBB**2-4.*AAAA*CCCC
           IF (ABCD.GE.0.) THEN
                T(K)=2.*(-BBBB+SQRT(ABCD))/(2.*AAAA)
           ELSE 
                IERR=1
                T(K)=0.
           END IF
      ELSE 
           IERR=1
           T(K)=0.
      END IF
      DELTA=T(K)-T(L) 
C----------GET THE INTERVAL VELOCITY.
      IF (DELTA.LE.0.) THEN
           IERR=1
           VINT(K)=0.
      ELSE IF (TYPE(3:4).EQ.'IN') THEN
           VINT(K)=V(K)
      ELSE IF (TYPE(3:4).EQ.'AV') THEN
           VINT(K)=(V(K)*T(K)-V(L)*T(L))/DELTA
      ELSE IF (TYPE(3:4).EQ.'RM'.OR.TYPE(3:4).EQ.'NM') THEN
           ABCD=V(K)**2*T(K)-V(L)**2*T(L)
           IF (ABCD.GT.0.) THEN
                VINT(K)=SQRT(ABCD/DELTA)      
           ELSE
                IERR=1
                VINT(K)=0.
           END IF
      ELSE
           IERR=1
           VINT(K)=0.
      END IF
C----------GET RMS VELOCITY AND AVERAGE VELOCITY AND DEPTH.
      IF (DELTA.LE.0..OR.T(K).LE.0..OR.T(L).LT.0.) THEN
           IERR=1
           VRMS(K)=0.
           VAV(K)=0.
           DEPTH(K)=0.
      ELSE
           VRMS(K)=SQRT((VRMS(L)**2*T(L)+VINT(K)**2*DELTA)/T(K)) 
           VAV(K)=(VAV(L)*T(L)+VINT(K)*DELTA)/T(K) 
           DEPTH(K)=DEPTH(L)+VINT(K)*DELTA/2.
      END IF
50    CONTINUE
      RETURN
      END 
