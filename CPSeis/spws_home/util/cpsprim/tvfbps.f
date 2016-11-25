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
C                       CONOCO PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C  Subroutine name: TVFBPS
C          Authors: Bob Baumel and Shein Wang
C     Last revised: 88/10/21
C
C  Purpose:  Create a bandpass filter in the frequency domain.
C            This routine does NOT do any FFT's.  It is assumed that
C            the user will apply the filter in the frequency domain
C            and/or do any required FFT to get to the time domain.
C-----------------------------------------------------------------------
C                         CALLING SEQUENCE
C
C      CALL TVFBPS (AMP,NF,DF,F1,F2,TP1,TP2,PHASE,ITAPER,FILTER,CR)
C
C  Name     Valid   Description
C        ----------------- INPUT ARGUMENTS ----------------------
C  AMP     real>0   Amplitude of filter in pass band.
C  NF       int>0   # of Frequencies in filter (starting from freq=0).
C  DF      real>0   Frequency increment in Hz.
C  F1        real   Low  freq limit (center of taper) in Hz.        
C  F2     real>F1   High freq limit (center of taper) in Hz.
C  TP1    real>=0   Length of taper  (low  freq end)  in Hz.
C  TP2    real>=0   Length of taper  (high freq end)  in Hz.
C  PHASE     real   Phase of filter in degrees (ignored if CR='R').
C  ITAPER     1,2   Taper shape:  1=linear;  2=cosine.
C  CR      'C','R'  Switch (CHAR*1) saying whether FILTER array is
C                   COMPLEX or REAL.  You may OMIT this argument;
C                   if so, the default is COMPLEX.
C        ----------------- OUTPUT ARGUMENT ----------------------
C  FILTER           COMPLEX (or REAL, as specified by CR aragument)
C                   array of length NF to contain the computed filter
C                   in the frequency domain.
C-----------------------------------------------------------------------
C                                NOTES
C
C 1. This routine is very robust.  You can set F1 negative if you don't
C    want a lower limit to the pass-band; you can set F2 greater than
C    Nyquist if you don't want an upper limit.  You can set either TP1
C    or TP2 to zero if you want a sharp cutoff.  Also, you can overlap
C    the tapers at the low and high frequency ends (for example: F1=10,
C    F2=50,TP1=20,TP2=70); in this case, the two taper functions are
C    multiplied together.
C
C 2. The low  frequency taper extends from  F1-TP1/2  to  F1+TP1/2.
C    The high frequency taper extends from  F2-TP2/2  to  F2+TP2/2.
C
C 3. The AMP argument sets the scale of the filter;  i.e., the value in
C    the middle of the pass-band (assuming the tapers don't overlap) is
C    AMP * (COS(PHASE) + I*SIN(PHASE)).  You can set AMP so that you
C    recover correct amplitudes after forward and inverse FFT's.  For
C    the FFT routines on the Cray, you would typically set AMP equal to
C    1.0/NPOW2  or  0.5/NPOW2,  where  NPOW2  is the power of two used
C    in the FFT.  If you don't care about amplitudes, just set AMP=1.0.
C-----------------------------------------------------------------------
C                             REVISION HISTORY
C     Date      Author       Description
C     ----      ------       -----------
C 3.  88/10/21; B Baumel;    Add CR switch for complex/real filter.
C 2.  88/08/17; B Baumel;    New calling sequence, allow only two
C                            taper types: linear and cosine.
C 1.  86/07/05; Baumel & Wang; Original version.
C-----------------------------------------------------------------------
C\END DOC
C*    SUBROUTINE TVFBPS(AMP,NF,DF,F1,F2,TP1,TP2,PHASE,ITAPER,FILTER,CR)
      SUBROUTINE TVFBPS(AMP,NF,DF,F1,F2,TP1,TP2,PHASE,ITAPER,FILTER)
      COMPLEX PHFACT,FILTER(*)
      CHARACTER*1 CR,CR1
      PARAMETER (CONVRT = 3.1415926535898/180.)
      PARAMETER (HFPI =.5*3.1415926535898)
C*    POINTER (IRFILT,RFILT(*))
C
      TPFUN1(X) = X                         ! linear taper function
      TPFUN2(X) = SIN(HFPI*X) ** 2          ! cosine taper function
C
      W1 = MAX(TP1,0.)
      W2 = MAX(TP2,0.)
      Q1 = F1 - .5*W1
      Q2 = F1 + .5*W1
      Q3 = F2 - .5*W2
      Q4 = F2 + .5*W2
      CR1 = 'C'
C*      IF (NUMARG().EQ.11) THEN
C*        IF (CR.EQ.'R' .OR. CR.EQ.'r') THEN
C*          CR1 = 'R'
C*          IRFILT = LOC(FILTER)
C*        END IF
C*      END IF
      IF (CR1.EQ.'C')
     *  PHFACT = CMPLX (AMP*COS(CONVRT*PHASE), AMP*SIN(CONVRT*PHASE))
      IF (W1.GT.0.) W1 = 1./W1
      IF (W2.GT.0.) W2 = 1./W2
C              
      IF (CR1.EQ.'C' .AND. ITAPER.NE.2) THEN   ! Complex, linear taper
        DO 1 I=1,NF
           F = (I-1)*DF
           IF (F.LT.Q1 .OR. F.GT.Q4) THEN
              FILTER(I) = (0.,0.)
           ELSE
              FILTER(I) = PHFACT
              IF (F .LT. Q2)  FILTER(I)=FILTER(I)*TPFUN1(W1*(F-Q1))
              IF (F .GT. Q3)  FILTER(I)=FILTER(I)*TPFUN1(W2*(Q4-F))
           END IF  
   1    CONTINUE
      ELSE IF (CR1.EQ.'C') THEN                ! Complex, cosine taper
        DO 2 I=1,NF
           F = (I-1)*DF
           IF (F.LT.Q1 .OR. F.GT.Q4) THEN
              FILTER(I) = (0.,0.)
           ELSE
              FILTER(I) = PHFACT
              IF (F .LT. Q2)  FILTER(I)=FILTER(I)*TPFUN2(W1*(F-Q1))
              IF (F .GT. Q3)  FILTER(I)=FILTER(I)*TPFUN2(W2*(Q4-F))
           END IF
   2    CONTINUE
C*      ELSE IF (ITAPER.NE.2) THEN               ! Real, linear taper
C*        DO 3 I=1,NF
C*           F = (I-1)*DF
C*           IF (F.LT.Q1 .OR. F.GT.Q4) THEN
C*              RFILT(I) = 0.
C*           ELSE
C*              RFILT(I) = AMP
C*              IF (F .LT. Q2)  RFILT(I)=RFILT(I)*TPFUN1(W1*(F-Q1))
C*              IF (F .GT. Q3)  RFILT(I)=RFILT(I)*TPFUN1(W2*(Q4-F))
C*           END IF
C*   3    CONTINUE
C*      ELSE                                     ! Real, cosine taper
C*        DO 4 I=1,NF
C*           F = (I-1)*DF
C*           IF (F.LT.Q1 .OR. F.GT.Q4) THEN
C*              RFILT(I) = 0.
C*           ELSE
C*              RFILT(I) = AMP
C*              IF (F .LT. Q2)  RFILT(I)=RFILT(I)*TPFUN2(W1*(F-Q1))
C*              IF (F .GT. Q3)  RFILT(I)=RFILT(I)*TPFUN2(W2*(Q4-F))
C*           END IF
C*   4    CONTINUE
      END IF
      RETURN
      END
