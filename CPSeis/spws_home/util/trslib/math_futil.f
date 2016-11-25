
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
ccccccccccccccccccc file math_futil.f cccccccccccccccccccccc




      SUBROUTINE TRIPLESORT (Y,X,R,N)
C     SORTS THE 3 LINKED ARRAYS Y(N),X(N),R(N) INTO ASCENDING ORDER.
C     real    array y(n) (e.g. y-coordinate) is the the primary   sort key.
C     real    array x(n) (e.g. x-coordinate) is the the secondary sort key.
C     integer array r(n) (e.g. record number) goes along for the ride.
C     MADE FROM CONSEIS SUBROUTINE SORTER.
      implicit none
      real y(*),x(*),ytemp,xtemp
      integer r(*),n,m,k,j,i,l,rtemp
C------------------------------------------GET STARTED.
      M=N
   20 M=M/2
      IF (M.EQ.0) RETURN
      K=N-M
      DO J=1,K
           I=J+M
   49      L=I
           I=I-M
           if (y(i).gt.y(l).or.
     $        (y(i).eq.y(l).and.x(i).gt.x(l)))then
C------------------------------------------SWITCH INDICES I AND L.
                ytemp=y(i)
                xtemp=x(i)
                rtemp=r(i)
                y(i)=y(l)
                x(i)=x(l)
                r(i)=r(l)
                y(l)=ytemp
                x(l)=xtemp
                r(l)=rtemp
C------------------------------------------KEEP WORKING.
                IF (I.GT.M) GO TO 49
           end if
      END DO
      GO TO 20
      END




      subroutine insert_into_float_array (index,value,x,n)
c     inserts value into given index of array x(n) and increments n.
c     does nothing if index < 1 or index > n+1.

      implicit none
      integer index,n,i
      real value,x(*)

      if (index.lt.1.or.index.gt.n+1) return
      if (index.le.n) then
           do i=n,index,-1
                x(i+1)=x(i)
           end do
      end if
      x(index)=value
      n=n+1
      return
      end
  




      subroutine remove_from_float_array (index,x,n)
c     removes value from given index of array x(n) and decrements n.
c     does nothing if index < 1 or index > n.

      implicit none
      integer index,n,i
      real x(*)

      if (index.lt.1.or.index.gt.n) return
      if (index.lt.n) then
           do i=index+1,n
                x(i-1)=x(i)
           end do
      end if
      n=n-1
      return
      end
  



c      SUBROUTINE CLEAR_REALS (A,N)
c      REAL A(*)
c      DO 10 I=1,N
c10    A(I)=0.
c      RETURN
c      END



c      SUBROUTINE CLEAR_CHARS (A,N)
c      CHARACTER*(*) A(*)
c      DO 10 I=1,N
c10    A(I)=' '
c      RETURN
c      END





c      SUBROUTINE FILL_REALS (A,N,VALUE)
c      REAL A(*)
c      DO 10 I=1,N
c10    A(I)=VALUE
c      RETURN
c      END



c      SUBROUTINE FILL_CHARS (A,N,VALUE)
c      CHARACTER*(*) A(*),VALUE
c      DO 10 I=1,N
c10    A(I)=VALUE
c      RETURN
c      END





c      SUBROUTINE MOVE_REALS (A,B,N)
c      REAL A(*),B(*)
c      DO 10 I=1,N
c10    B(I)=A(I)
c      RETURN
c      END





c      SUBROUTINE MOVE_CHARS (A,B,N)
c      CHARACTER*(*) A(*),B(*)
c      DO 10 I=1,N
c10    B(I)=A(I)
c      RETURN
c      END








      FUNCTION RUNNING (I,Z,N,NRUN,ENDFLAG)
C     RETURNS RUNNING AVERAGE OF POINT Z(I) OUT OF ARRAY Z(N).
C     NRUN = NUMBER OF POINTS TO INCLUDE IN RUNNING AVERAGE.
C     ENDFLAG = 1 (or otherwise) FOR TRUNCATED END RANGE 
C     ENDFLAG = 2 FOR SHIFTED END RANGE 
C     ENDFLAG = 3 FOR EXTENDED END RANGE
C     ENDFLAG = 4 FOR NARROWED END RANGE (GRADED ENDS)
      INTEGER ENDFLAG
      DIMENSION Z(*)
C--------------------------------------GET HALF-LENGTH OF RUNNING AVERAGE.
      IF (N.EQ.0) THEN
           RUNNING=0.
           RETURN
      ELSE IF (ENDFLAG.EQ.4) THEN                       ! narrowed.
           MRUN=MIN0(NRUN/2,N-I,I-1) 
      ELSE
           MRUN=NRUN/2 
      ENDIF
C---------------------------------GET LOWER AND UPPER LIMITS FOR THE AVERAGE.
      IF (ENDFLAG.EQ.2) THEN                            ! shifted. 
            JA=MAX0(1,MIN0(I-MRUN,N-2*MRUN))
            JB=MIN0(N,MAX0(I+MRUN,1+2*MRUN))
      ELSE IF (ENDFLAG.EQ.3) THEN                       ! extended.
            JA=I-MRUN 
            JB=I+MRUN 
      ELSE
            JA=MAX0(I-MRUN,1) 
            JB=MIN0(I+MRUN,N) 
      END IF
C--------------------------------------------CALCULATE THE AVERAGE. 
      SUM=0.
      DO J=JA,JB 
           IF (ENDFLAG.EQ.3) THEN                       ! extended.
                 JJ=MIN0(N,MAX0(1,J))
           ELSE
                 JJ=J
           END IF
           SUM=SUM+Z(JJ) 
      END DO
      RUNNING=SUM/(JB-JA+1) 
      RETURN
      END 



c      FUNCTION junk_RUNAV (I,Z,N,NRUN,IEND)
cC     RETURNS RUNNING AVERAGE OF POINT Z(I) OUT OF ARRAY Z(N).
cC     NRUN = NUMBER OF POINTS TO INCLUDE IN RUNNING AVERAGE.
cC     IEND = TT FOR TRUNCATED END RANGE 
cC            SS FOR SHIFTED END RANGE 
cC            EE FOR EXTENDED END RANGE
cC            NN FOR NARROWED END RANGE (GRADED ENDS)
c      CHARACTER*(*) IEND
c      DIMENSION Z(N)
cC----------GET LOWER AND UPPER POINT LIMITS FOR THE AVERAGE.
c      MRUN=NRUN/2 
c      IF (IEND.EQ.'NN') MRUN=MIN0(NRUN/2,N-I,I-1) 
c      JA=MAX0(I-MRUN,1) 
c      JB=MIN0(I+MRUN,N) 
c      IF (IEND.EQ.'SS') JA=MAX0(1,MIN0(I-MRUN,N-2*MRUN))
c      IF (IEND.EQ.'SS') JB=MIN0(N,MAX0(I+MRUN,1+2*MRUN))
c      IF (IEND.EQ.'EE') JA=I-MRUN 
c      IF (IEND.EQ.'EE') JB=I+MRUN 
cC----------CALCULATE THE AVERAGE. 
c      SUM=0.
c      DO 72 J=JA,JB 
c      JJ=J
c      IF (IEND.EQ.'EE') JJ=MIN0(N,MAX0(1,J))
c72    SUM=SUM+Z(JJ) 
c      junk_RUNAV=SUM/(JB-JA+1) 
c      RETURN
c      END 
c






      FUNCTION AMULT (N,A,B)
      REAL A(*),B(*) 
      AMULT=0.
      DO 10 I=1,N 
10    AMULT=AMULT+A(I)*B(I) 
      RETURN
      END 





      SUBROUTINE LEAST (NTERMS,TERMS,Z,WEIGHT)
C     ELEGANT LEAST SQUARES SOLVER, DEVELOPED FROM PROGRAM WULSQR 
C        AND FUNCTION NECSD, BOTH SUPPLIED BY SLAVEK RUCINSKI.
C     FOR EACH OBSERVATION -- CALL LEAST (NTERMS,TERMS,Z,WEIGHT)
C     TO GET RESULTS --       CALL LEAST (0,   COEF,ECOEF,SIGMA)
C     EXAMPLE --  TO SOLVE  Z=A+B*X+C*X*Y  FOR  COEF=A,B,C
C                 SET  TERMS=1,X,X*Y  FOR EACH OBSERVATION  Z . 
C     WORKS FOR UP TO 10 EQUATIONS AND UNKNOWNS.
      DIMENSION TERMS(10),Z(10),H(11,11)
      SAVE N,NCOEF,NCOEF1,H,ZERO,ONE
      DATA ZERO,ONE/0.,1./
      DATA N/0/ 
C----------GET STARTED. 
      IF (NTERMS.EQ.0) GO TO 300
      IF (N.GE.1) GO TO 200 
C----------INITIALIZE FOR FIRST OBSERVATION.
      NCOEF=NTERMS
      NCOEF1=NTERMS+1 
      DO 10 J=1,NCOEF1
      DO 10 K=J,NCOEF1
10    H(J,K)=ZERO 
C----------ADD IN NEXT OBSERVATION. 
200   N=N+1 
      DO 20 J=1,NCOEF 
      H(J,NCOEF1)=H(J,NCOEF1)+TERMS(J)*Z(1)*WEIGHT
      DO 20 K=J,NCOEF 
20    H(J,K)=H(J,K)+TERMS(J)*TERMS(K)*WEIGHT
      H(NCOEF1,NCOEF1)=H(NCOEF1,NCOEF1)+Z(1)*Z(1)*WEIGHT
      RETURN
C----------FIRST STEP OF ARRAY OPERATIONS.
300   WEIGHT=-1.
      DO 40 J=1,NCOEF1
      DO 40 K=J,NCOEF1
      S=H(J,K)
      IF (J.EQ.1) GO TO 32
      JJ=J-1
      DO 30 L=1,JJ
30    S=S-H(L,J)*H(L,K) 
32    IF (K.EQ.J) GO TO 34
      H(J,K)=S/HH 
      GO TO 40
34    IF (S.GT.ZERO) GO TO 36 
      IF (J.NE.NCOEF1) GO TO 99 
      HH=ZERO 
      GO TO 38
36    HH=SQRT(S)
38    H(J,J)=HH 
40    CONTINUE
C----------SECOND STEP OF ARRAY OPERATIONS. 
      DO 60 J=1,NCOEF 
      HH=H(J,J) 
      DO 60 K=1,J 
      IF (K.NE.J) GO TO 48
      H(J+1,K)=ONE/HH 
      GO TO 60
48    S=ZERO
      JJ=J-1
      DO 50 L=K,JJ
50    S=S-H(L,J)*H(L+1,K) 
      H(J+1,K)=S/HH 
60    CONTINUE
C----------GET RESULTS FOR COEF(TERMS),ECOEF(Z),SIGMA(WEIGHT).
      WEIGHT=H(NCOEF1,NCOEF1)/SQRT(AMAX0(N-NCOEF,1))
      DO 80 J=1,NCOEF 
      S=ZERO
      W=ZERO
      DO 70 L=J,NCOEF 
      S=S+H(L,NCOEF1)*H(L+1,J)
70    W=W+H(L+1,J)**2 
      TERMS(J)=S
80    Z(J)=SQRT(W)*WEIGHT 
99    N=0 
      RETURN
      END 





      SUBROUTINE LEASTD (NTERMS,TERMS,Z,WEIGHT)
C     SAME AS SUBROUTINE LEAST EXCEPT DOUBLE PRECISION.
C     INPUT PARAMETER TERMS (AND COEF) IS DOUBLE PRECISION.
C     ELEGANT LEAST SQUARES SOLVER, DEVELOPED FROM PROGRAM WULSQR 
C        AND FUNCTION NECSD, BOTH SUPPLIED BY SLAVEK RUCINSKI.
C     FOR EACH OBSERVATION -- CALL LEAST (NTERMS,TERMS,Z,WEIGHT)
C     TO GET RESULTS --       CALL LEAST (0,   COEF,ECOEF,SIGMA)
C     EXAMPLE --  TO SOLVE  Z=A+B*X+C*X*Y  FOR  COEF=A,B,C
C                 SET  TERMS=1,X,X*Y  FOR EACH OBSERVATION  Z . 
C     WORKS FOR UP TO 10 EQUATIONS AND UNKNOWNS.
      DIMENSION Z(10)
      DOUBLE PRECISION TERMS(10),H(11,11)
      DOUBLE PRECISION S,W,HH,ZERO,ONE
      SAVE N,NCOEF,NCOEF1,H,ZERO,ONE
      DATA ZERO,ONE/0.D0,1.D0/
      DATA N/0/ 
C----------GET STARTED. 
      IF (NTERMS.EQ.0) GO TO 300
      IF (N.GE.1) GO TO 200 
C----------INITIALIZE FOR FIRST OBSERVATION.
      NCOEF=NTERMS
      NCOEF1=NTERMS+1 
      DO 10 J=1,NCOEF1
      DO 10 K=J,NCOEF1
10    H(J,K)=ZERO 
C----------ADD IN NEXT OBSERVATION. 
200   N=N+1 
      DO 20 J=1,NCOEF 
      H(J,NCOEF1)=H(J,NCOEF1)+TERMS(J)*Z(1)*WEIGHT
      DO 20 K=J,NCOEF 
20    H(J,K)=H(J,K)+TERMS(J)*TERMS(K)*WEIGHT
      H(NCOEF1,NCOEF1)=H(NCOEF1,NCOEF1)+Z(1)*Z(1)*WEIGHT
      RETURN
C----------FIRST STEP OF ARRAY OPERATIONS.
300   WEIGHT=-1.
      DO 40 J=1,NCOEF1
      DO 40 K=J,NCOEF1
      S=H(J,K)
      IF (J.EQ.1) GO TO 32
      JJ=J-1
      DO 30 L=1,JJ
30    S=S-H(L,J)*H(L,K) 
32    IF (K.EQ.J) GO TO 34
      H(J,K)=S/HH 
      GO TO 40
34    IF (S.GT.ZERO) GO TO 36 
      IF (J.NE.NCOEF1) GO TO 99 
      HH=ZERO 
      GO TO 38
36    HH=DSQRT(S)
38    H(J,J)=HH 
40    CONTINUE
C----------SECOND STEP OF ARRAY OPERATIONS. 
      DO 60 J=1,NCOEF 
      HH=H(J,J) 
      DO 60 K=1,J 
      IF (K.NE.J) GO TO 48
      H(J+1,K)=ONE/HH 
      GO TO 60
48    S=ZERO
      JJ=J-1
      DO 50 L=K,JJ
50    S=S-H(L,J)*H(L+1,K) 
      H(J+1,K)=S/HH 
60    CONTINUE
C----------GET RESULTS FOR COEF(TERMS),ECOEF(Z),SIGMA(WEIGHT).
      WEIGHT=H(NCOEF1,NCOEF1)/SQRT(AMAX0(N-NCOEF,1))
      DO 80 J=1,NCOEF 
      S=ZERO
      W=ZERO
      DO 70 L=J,NCOEF 
      S=S+H(L,NCOEF1)*H(L+1,J)
70    W=W+H(L+1,J)**2 
      TERMS(J)=S
80    Z(J)=DSQRT(W)*WEIGHT 
99    N=0 
      RETURN
      END 






      SUBROUTINE LEASQ1 (N,X,Z,NCOEF,   COEF,ECOEF,SIGMA) 
C     DOES LEAST SQUARES FIT OF POWER-LAW POLYNOMIAL
C        TO N POINTS (X,Z). 
C     NCOEF = NUMBER OF COEFFICIENTS -COEF- TO USE AND RETURN.
C     ALSO RETURNS COEFFICIENT ERRORS -ECOEF- AND -SIGMA-.
      DIMENSION X(N),Z(N),COEF(NCOEF),ECOEF(NCOEF)
      DO 30 I=1,N 
      CALL POLYA (X(I),NCOEF,COEF)
30    CALL LEAST (NCOEF,COEF,Z(I),1.) 
      CALL LEAST (0,   COEF,ECOEF,SIGMA)
      RETURN
      END 






      SUBROUTINE POLYA (X,NTERMS,TERMS) 
      DIMENSION TERMS(NTERMS) 
      TERMS(1)=1. 
      IF (NTERMS.LE.1) RETURN 
      DO 10 I=2,NTERMS
10    TERMS(I)=X*TERMS(I-1) 
      RETURN
      END 



                       





      FUNCTION TERP1 (XTERP,X,L,T)
C     L POSITIVE GIVES FLAT EXTRAPOLATION.
C     L NEGATIVE GIVES SLOPING EXTRAPOLATION. 
C     L ZERO     returns zero.
      DIMENSION X(*),T(*) 
      if (L.eq.0) then
           terp1=0.
           return
      end if
      LA=1
      LB=IABS(L)
      XRANGE=X(LB)-X(LA)
6     IF (LB-LA.LE.1) GO TO 100 
      LC=(LA+LB)/2
      IF (XRANGE*(X(LC)-XTERP).GE.0.) GO TO 7 
      LA=LC 
      GO TO 6 
7     LB=LC 
      GO TO 6 
100   TERP1=YROOT(XTERP,X(LA),X(LB),T(LA),T(LB))
      IF (L.LT.0) RETURN
      IF (XRANGE*(X(1)-XTERP).GE.0.) TERP1=T(1) 
      IF (XRANGE*(X(L)-XTERP).LE.0.) TERP1=T(L) 
      RETURN
      END 

      


      subroutine terpfill (x,y,n,  xmin,xmax,  nsamp,array)
c     fills array(nsamp) with interpolated values from y(n).
c     x(n) = input abscissae.
c     y(n) = input ordinates.
c     array(nsamp) = output ordinates with uniform abscissa spacing.
c     abscissa xmin corresponds to ordinate array(1).
c     abscissa xmax corresponds to ordinate array(nsamp).
      implicit none
      integer n,nsamp,i
      real x(n),y(n),xmin,xmax,array(nsamp),xterp,terp1,dx

      dx=(xmax-xmin)/max(nsamp-1,1)
      do i=1,nsamp
           xterp=xmin+(i-1)*dx
           array(i)=terp1(xterp,x,n,y)
      end do
      return
      end
      



      SUBROUTINE SPLINE (S,N,X,Y,E,   C,Q,SIGMA)
C     DETERMINES COEFFICIENTS OF CUBIC SPLINE INTERPOLATION + SMOOTHING.
C     S=0. CAUSES INTERPOLATION.
C     S=1. CAUSES CURVE TO MISS POINTS BY AMOUNT OF ERRORS E (ON AVERAGE
C     C(4,N) = RESULTING COEFFICIENTS.
C     Q(7,N) = SCRATCH SPACE. 
C     EXTRAPOLATION FOR VALUES OF X EXCEEDING X(N) IS STRAIGHT LINE.
C     EXTRAPOLATION FOR VALUES OF X BELOW X(1) WILL BE STRAIGHT LINE IF 
C        ONLY THE FIRST TWO COEFFICIENTS C(1,1) AND C(2,1) ARE USED.
      DIMENSION X(N),Y(N),E(N),C(4,N),Q(7,N)

cccccccc fix for n=1 applied 10/27/97:
      if (n.eq.1) then
           c(1,1)=y(1)
           c(2,1)=0.0
           c(3,1)=0.0
           c(4,1)=0.0
           sigma=0.0
           return
      end if
cccccccc Note about the above fix:
cccccccc This routine has always worked in va and vel with n=1 (returning
cccccccc the above values) (except returning sigma=NaN), even though nn
cccccccc (=n-1=0) is used as an index in the calculations.
cccccccc But for the new va, with identical input values, c(2,1) was being
cccccccc set to 2.09833.

ccc debug print    print *, s,n,x,y,e

      DO 5 J=1,7
      DO 5 I=1,N
5     Q(J,I)=0. 
      T=N*ABS(S)
      NN=N-1
      P7=X(2)-X(1)
      P5=(Y(2)-Y(1))/P7 
      DO 10 I=2,NN
      J=I+1 
      P6=P7 
      P7=X(J)-X(I)
      P4=P5 
      P5=(Y(J)-Y(I))/P7 
      C(1,I)=P5-P4
      Q(4,I)=2.*(P6+P7)/3.
      Q(5,I)=P7/3.
      Q(3,I)=E(I-1)/P6
      Q(1,I)=E(J)/P7
      Q(2,I)=-E(I)/P6-E(I)/P7 
10    CONTINUE
      Q(1,1)=0. 
      Q(2,N)=0. 
      Q(3,N)=0. 
      DO 20 I=2,NN
      J=I+1 
      C(2,I)=Q(1,I)**2+Q(2,I)**2+Q(3,I)**2
      C(3,I)=Q(1,I)*Q(2,J)+Q(2,I)*Q(3,J)
      IF (I.EQ.NN) GO TO 20 
      C(4,I)=Q(1,I)*Q(3,J+1)
20    CONTINUE
      C(4,NN)=0.
      Q1=0. 
      Q2=-T 
30    P4=0. 
      DO 40 I=2,NN
      J=I-2 
      IF (J.EQ.0) GO TO 35
      Q(3,J)=P6*Q(1,J)
      P4=P6*Q(3,J)
35    K=I-1 
      Q(2,K)=P5*Q(1,K)
      Q(1,I)=1./(Q1*C(2,I)+Q(4,I)-P5*Q(2,K)-P4) 
      Q(6,I)=C(1,I)-Q(2,K)*Q(6,K) 
      IF (J.EQ.0) GO TO 37
      Q(6,I)=Q(6,I)-Q(3,J)*Q(6,J) 
37    P5=Q1*C(3,I)+Q(5,I)-P7*Q(2,K) 
      P6=P7 
      P7=C(4,I)*Q1
40    CONTINUE
      Q(6,1)=0. 
      Q(6,N)=0. 
      Q(6,NN)=Q(1,NN)*Q(6,NN)-Q(2,NN)*Q(6,N)
      IF (N.LT.4) GO TO 55
      NNN=NN-1
      DO 50 II=2,NNN
      I=N-II
      Q(6,I)=Q(1,I)*Q(6,I)-Q(2,I)*Q(6,I+1)
      Q(6,I)=Q(6,I)-Q(3,I)*Q(6,I+2) 
50    CONTINUE
55    P4=0. 
      P7=0. 
      DO 60 I=1,NN
      J=I+1 
      P6=P7 
      P7=(Q(6,J)-Q(6,I))/(X(J)-X(I))
      Q(7,I)=(P7-P6)*E(I)**2
      P4=P4+Q(7,I)*(P7-P6)
60    CONTINUE
      Q(7,N)=-P7*E(N)**2
      P4=P4-Q(7,N)*P7 
      P6=Q2 
      Q2=P4*Q1*Q1 
      IF (Q2.GE.T) GO TO 75 
      IF (Q2.LE.P6) GO TO 75
      P5=0. 
      P7=(Q(7,2)-Q(7,1))/(X(2)-X(1))
      DO 70 I=2,NN
      P6=P7 
      J=I+1 
      P7=(Q(7,J)-Q(7,I))/(X(J)-X(I))
      J=I-1 
      P6=P7-P6-Q(2,J)*Q(1,J)
      J=J-1 
      IF (J.EQ.0) GO TO 65
      P6=P6-Q(3,J)*Q(1,J) 
65    P5=P5+P6*Q(1,I)*P6
      Q(1,I)=P6 
70    CONTINUE
      P7=P4-Q1*P5 
      IF (P7.LE.0.) GO TO 75
      Q1=Q1+(T-Q2)/((Q1+SQRT(ABS(T/P4)))*P7)
      GO TO 30
75    DO 80 I=1,N 
      C(1,I)=Y(I)-Q1*Q(7,I) 
      C(3,I)=Q(6,I) 
80    CONTINUE
      DO 90 I=1,NN
      J=I+1 
      P7=X(J)-X(I)
      C(4,I)=(C(3,J)-C(3,I))/(3.*P7)
      C(2,I)=(C(1,J)-C(1,I))/P7-(P7*C(4,I)+C(3,I))*P7 
90    CONTINUE
      P7=X(N)-X(NN) 
      C(2,N)=(3.*C(4,NN)*P7+2.*C(3,NN))*P7+C(2,NN)
      C(4,N)=0. 
C----------GET STANDARD DEVIATION OF POINTS FROM CURVE. 
      SIGMA=0.
      DO 95 I=1,N 
95    SIGMA=SIGMA+(C(1,I)-Y(I))**2
      SIGMA=SQRT(SIGMA/NN)

ccc  debug print       print *, c,q,sigma 

      RETURN
      END 







      FUNCTION YSPLINE (IWHICH,XWANT,X,C,N) 
C     ARRAY X MUST BE IN STRICTLY ASCENDING ORDER.
C     EXTRAPOLATIONS ARE STRAIGHT LINE. 
C     IWHICH=1 RETURNS VALUE AT XWANT.
C     IWHICH=2 RETURNS DERIVATIVE AT XWANT. 
C     IWHICH=3 RETURNS SECOND DERIVATIVE AT XWANT.
C     IWHICH=4 RETURNS INTEGRAL FROM X(1) TO XWANT. 
      DIMENSION X(N),C(4,N) 
C----------FIRST THINGS FIRST.
      IF (IWHICH.GE.4) GO TO 300
      IF (XWANT.LT.X(1)) GO TO 200
C----------GET VALUE OF J BETWEEN 1 AND N.
      J=1 
      I=N+1 
6     IF (I-J.EQ.1) GO TO 100 
      K=(J+I)/2 
      IF (XWANT.LE.X(K)) GO TO 7
      J=K 
      GO TO 6 
7     I=K 
      GO TO 6 
C----------GET VALUE OR DERIVATIVE OR SECOND DERIVATIVE.
100   DX=XWANT-X(J) 
      IF (IWHICH.LE.1) YSPLINE=((C(4,J)*DX+C(3,J))*DX+C(2,J))*DX+C(1,J) 
      IF (IWHICH.EQ.2) YSPLINE=(3.*C(4,J)*DX+2.*C(3,J))*DX+C(2,J) 
      IF (IWHICH.EQ.3) YSPLINE=6.*C(4,J)*DX+2.*C(3,J) 
      RETURN
C----------EXTRAPOLATE BELOW THE RANGE IN X.
200   DX=XWANT-X(1) 
      IF (IWHICH.LE.1) YSPLINE=C(2,1)*DX+C(1,1) 
      IF (IWHICH.EQ.2) YSPLINE=C(2,1) 
      IF (IWHICH.EQ.3) YSPLINE=0. 
      RETURN
C----------GET INTEGRAL EXTRAPOLATED BELOW THE RANGE IN X.
300   IF (XWANT.GT.X(1)) GO TO 400
      DX=XWANT-X(1) 
      YSPLINE=(0.5*C(2,1)*DX+C(1,1))*DX 
      RETURN
C----------GET INTEGRAL.
400   YSPLINE=0.
      J=0 
410   J=J+1 
      IF (J.EQ.N) DX=XWANT-X(J) 
      IF (J.LT.N) DX=AMIN1(X(J+1),XWANT)-X(J) 
      YSPLINE=YSPLINE+(((0.25*C(4,J)*DX+C(3,J)/3.)*DX 
     $   +0.5*C(2,J))*DX+C(1,J))*DX 
      IF (J.EQ.N) RETURN
      IF (XWANT.LE.X(J+1)) RETURN 
      GO TO 410 
      END 








      FUNCTION ZIP1 (WX,H)
      DIMENSION WX(9),H(*)
      JX=WX(9)
      AA=0. 
      DO 40 IX=1,JX 
      LX=WX(IX+4) 
40    AA=AA+WX(IX)*H(LX)
      ZIP1=AA 
      RETURN
      END 






      SUBROUTINE PITY (XWANT,X,N,   W)
C     GIVEN XWANT AND ABSCISSAE X(1)...X(N), RETURNS W(1)...W(9)
C        SO THAT YWANT = W(1)*Y(L1)+W(2)*Y(L2)+...+W(J)*Y(LJ)  WHERE
C        W(1),W(2),W(3),W(4) = WEIGHTS
C        W(5),W(6),W(7),W(8) = SUBSCRIPTS = L1,L2,L3,L4 
C        W(9) = NUMBER OF WEIGHTS AND SUBSCRIPTS TO USE = J = 1, 2, 3, O
C     ARRAY X MUST BE IN ASCENDING OR DESCENDING ORDER WITH NO VALUES EQ
C     DOES FLAT EXTRAPOLATION OUTSIDE THE RANGE OF THE ABSCISSAE. 
      DIMENSION X(N),W(9) 
C----------LOOK FOR NEEDED POINTS AND FIND EXACT MATCH IF POSSIBLE. 
      I=1 
      IF (N.LE.1) GO TO 950 
      F=1.
      IF (X(N).LT.X(1)) F=-1. 
      DO 20 I=1,N 
      IF (F*(X(I)-XWANT)) 20,950,8
8     IF (I.EQ.N) GO TO 13
      IF (I-2) 950,40,300 
20    CONTINUE
C----------WE HAVE AN EXACT MATCH.
      I=N 
950   W(5)=I
      W(1)=1. 
      W(9)=1
      RETURN
C----------WE HAVE ONE POINT BEFORE AND ONE POINT AFTER.
17    W(5)=I-1
      W(6)=I
      W(9)=2
      W(2)=(XWANT-X(I-1))/(X(I)-X(I-1)) 
      W(1)=1.-W(2)
      RETURN
13    IF (N.EQ.2) GO TO 17
C----------WE HAVE TOTAL OF 3 POINTS. 
40    W(9)=3
      A3=0. 
      R3=0. 
      IF (I.GT.2) GO TO 310 
C----------WE HAVE 1 POINT BEFORE AND 2 POINTS AFTER. 
      W(5)=I+1
      W(6)=I
      W(7)=I-1
      XA=X(I+1) 
      XB=X(I) 
      XC=X(I-1) 
      GO TO 320 
C----------WE HAVE 2 POINTS BEFORE AND 2 POINTS AFTER.
300   W(9)=4
      XD=X(I+1) 
310   XC=X(I) 
      XB=X(I-1) 
      XA=X(I-2) 
      W(8)=I+1
      W(7)=I
      W(6)=I-1
      W(5)=I-2
320   J=W(9)
      A1=(XC-XWANT)**2
      A2=(XC-XB)**2 
      IF (J.EQ.4) A3=(XWANT-XB)**2
      S=A1+A2+A3
      R1=A1*(XWANT-XA)/(XB-XA)
      R2=A2*(XWANT-XB)/(XC-XB)
      IF (J.EQ.4) R3=A3*(XWANT-XC)/(XD-XC)
      W(1)=(A1-R1)/S
      W(2)=(R1+A2-R2)/S 
      W(3)=(R2+A3-R3)/S 
      IF (J.EQ.4) W(4)=R3/S 
      RETURN
      END 






      FUNCTION YROOT (XROOT,XA,XB,YA,YB)
      IF (XA.EQ.XB) GO TO 10
      YROOT=YA+(XROOT-XA)*(YB-YA)/(XB-XA) 
      RETURN
10    YROOT=(YA+YB)/2.
      RETURN
      END 
                 





      SUBROUTINE CURVE (DEPTH,VINT,N,ANGLE,   OFFSET,TIME,IERR) 
C     RETURNS EMERGENT OFFSET AND TIME FOR DOWNGOING ANGLE (RADIANS). 
C     RETURNS IERR=1 IF CRITICAL ANGLE EXCEEDED OR ANGLE.LT.0 OR .GE.90.
      DIMENSION DEPTH(N),VINT(N)
C----------GET STARTED. 
      SINA=SIN(ANGLE) 
      COSA=COS(ANGLE) 
      OFFSET=0.
      TIME=0.
      IF (SINA.LT.0..OR.COSA.LE.0.) GO TO 30
      IF (DEPTH(1).GT.0.) THEN
           OFFSET=2.*DEPTH(1)*SINA/COSA 
           TIME=2.*DEPTH(1)/COSA/VINT(1)
      END IF
      IF (N.LE.1) RETURN
C----------GO THRU THE LOOP.
      DO 10 I=2,N 
      SINA=SINA*VINT(I)/VINT(I-1) 
      COSA2=1.-SINA**2
      IF (COSA2.LE.0.) GO TO 30 
      COSA=SQRT(COSA2)
      OFFSET=OFFSET+2.*(DEPTH(I)-DEPTH(I-1))*SINA/COSA
10    TIME=TIME+2.*(DEPTH(I)-DEPTH(I-1))/COSA/VINT(I) 
      IERR=0
      RETURN
30    IERR=1
      RETURN
      END 









      SUBROUTINE CURVE2 (T,D,VINT,N,SINAI,TWANT,
     $   XLAT,TMIG,TIME,DMIG,SINA,VII)
C     GIVEN SINE OF SURFACE DIP ANGLE, RETURNS THE LATERAL SHIFT, 
C        UNMIGRATED TIME, AND SINE OF FINAL DIP ANGLE.
C     ALSO RETURNS MIGRATED TIME, MIGRATED DEPTH, AND INTERVAL VELOCITY.
C     RETURNS SINA=999 IF CRITICAL ANGLE EXCEEDED (DIP > 90). 
C     IN BETWEEN THE RETURNED VALUES OF TIME, THE CORRECT DIPS ARE
C        THOSE CORRESPONDING TO THE DEEPER DISPLAYED TIME, JUST AS
C        THE CORRECT INTERVAL VELOCITIES ARE THOSE CORRESPONDING TO 
C        THE DEEPER DISPLAYED TIME. 
C     TWANT=0 RETURNS RESULTS FOR MIGRATED TIME T(N). 
C     TWANT=POSITIVE RETURNS RESULTS FOR MIGRATED TIME TWANT. 
C     TWANT=NEGATIVE RETURNS RESULTS FOR UNMIGRATED TIME -TWANT.
      DIMENSION T(N),D(N),VINT(N) 
C----------GET STARTED. 
      I=1 
      SINA=SINAI
      IF (SINA.GE.1.) GO TO 30
      XLAT=0.
      TIME=0.
      IF (D(I).GT.0.) THEN
           COSA=SQRT(1.-SINA**2)
           XLAT=D(I)*SINA/COSA
           TIME=2.*D(I)/VINT(I)/COSA
      END IF
C----------GO THRU THE LOOP.
      DO 10 I=2,N 
      SINA=SINAI*VINT(I)/VINT(1)
      IF (SINA.GE.1.) GO TO 30
      COSA=SQRT(1.-SINA**2) 
      OLDX=XLAT 
      OLDT=TIME 
      XLAT=XLAT+(D(I)-D(I-1))*SINA/COSA 
      TIME=TIME+2.*(D(I)-D(I-1))/VINT(I)/COSA 
      IF (TWANT.GT.0..AND.T(I).GE.TWANT) GO TO 20 
      IF (TWANT.LT.0..AND.TIME.GE.-TWANT) GO TO 22
10    CONTINUE
      TMIG=T(N) 
      DMIG=D(N) 
      VII=VINT(N) 
      RETURN
C----------WE HIT THE CRITICAL ANGLE (DIP>90).
30    XLAT=0.
      TIME=0.
      TMIG=T(N) 
      DMIG=D(N) 
      VII=VINT(N) 
      SINA=999. 
      RETURN
C----------INTERPOLATE TO DESIRED TIME. 
20    FACTOR=(TWANT-T(I-1))/(T(I)-T(I-1)) 
      GO TO 24
22    FACTOR=(-TWANT-OLDT)/(TIME-OLDT)
24    TMIG=T(I-1)+FACTOR*(T(I)-T(I-1))
      DMIG=D(I-1)+FACTOR*(D(I)-D(I-1))
      TIME=OLDT+FACTOR*(TIME-OLDT)
      XLAT=OLDX+FACTOR*(XLAT-OLDX)
      VII=VINT(I) 
      RETURN
      END 




      subroutine densify_c (terpflag,stepflag,endflag,
     $             dx,nwant,dz,ncoef,nrun,
     $             n,x,z,  nn,xx,zz,  nmax,q,  buffer,ierr)
C     This entry is to be called from c-language, since it returns
C       a null-terminated hollerith string rather than a Fortran
C       character variable.
      implicit none
      integer terpflag,stepflag,endflag
      integer nwant,ncoef,nrun,n,nn,nmax,ierr,buffer(*)
      REAL dx,dz,X(*),Z(*),XX(*),ZZ(*),Q(*)
      character*80 msg
      call densify (terpflag,stepflag,endflag,
     $             dx,nwant,dz,ncoef,nrun,
     $             n,x,z,  nn,xx,zz,  nmax,q,  msg,ierr)
      call convert_cc2hh (msg,0,   buffer,0)
      return
      end



      subroutine densify_nonlin_c (terpflag,stepflag,endflag,
     $             dx1,dx2,nwant,dz,ncoef,nrun,
     $             n,x,z,  nn,xx,zz,  nmax,q,  buffer,ierr)
C     This entry is to be called from c-language, since it returns
C       a null-terminated hollerith string rather than a Fortran
C       character variable.
      implicit none
      integer terpflag,stepflag,endflag
      integer nwant,ncoef,nrun,n,nn,nmax,ierr,buffer(*)
      REAL dx1,dx2,dz,X(*),Z(*),XX(*),ZZ(*),Q(*)
      character*80 msg
      call densify_nonlin (terpflag,stepflag,endflag,
     $             dx1,dx2,nwant,dz,ncoef,nrun,
     $             n,x,z,  nn,xx,zz,  nmax,q,  msg,ierr)
      call convert_cc2hh (msg,0,   buffer,0)
      return
      end



      subroutine densify_nonlin (terpflag,stepflag,endflag,
     $             dx1,dx2,nwant,dz,ncoef,nrun,
     $             n,x,z,  nn,xx,zz,  nmax,q,  msg,ierr)
c  Densifies or smooths array (x,z) and puts results into (xx,zz).
C  dx1   (for stepflag=1) = SAMPLE INTERVAL FOR DENSIFYING (top).
C  dx2   (for stepflag=1) = SAMPLE INTERVAL FOR DENSIFYING (bottom).
C  nwant (for stepflag=2) = number of output picks wanted.
C  dz      (for terpflag=3) = AVERAGE DEVIATION OF SPLINE CURVE FROM PICKS.
C  ncoef   (for terpflag=4) = NUMBER OF COEFFICIENTS IN POLYNOMIAL.
C  nrun    (for terpflag=5,-5) = NUMBER OF POINTS TO USE IN RUNNING AVERAGE.
C  endflag (for terpflag=5,-5) = END EFFECT FLAG FOR RUNNING AVERAGE.
C  X(N),Z(N)     = the abscissae and ordinates of the original points.
C  XX(NN),ZZ(NN) = the abscissae and ordinates of the returned points.
C  nmax          = maximum allowed size of NN.
C  Q(12*NMAX)    = SCRATCH ARRAY.
C  msg  = error message (blank if no error).
C  ierr = error flag (0 if no error) (any error returns an unchanged function).
C  stepflag=1 uses DX1 and DX2 to set the increment for the new abscissae.
C  stepflag=2 uses NWANT to set the increment for the new abscissae.
C  stepflag=3 retains the current abscissae.
C  stepflag=4 uses the abscissae already in xx (nn and xx are preset).
C    (for stepflag=1,2,3: RESAMPLING STARTS AT X(1) AND FINISHES AT X(N))
C  terpflag=1 USES LINEAR INTERPOLATION with flat extrapolation.
C               (terpflag=-1 same except uses sloping extrapolation)
C               (HAS NO EFFECT IF stepflag=3)
C  terpflag=2 USES 4-POINT CUBIC INTERPOLATION.
C               (HAS NO EFFECT IF stepflag=3)
C  terpflag=3 USES RELAXED SPLINE INTERPOLATION (needs dz).
C               (HAS NO EFFECT IF dz=0 AND stepflag=3)
C  terpflag=4 USES POLYNOMIAL FIT (needs ncoef).
C               (HAS NO EFFECT IF ncoef=N AND stepflag=3)
C               (RETURNS AN AVERAGE IF ncoef=1)  (NN MIGHT AS WELL = 1)
C  terpflag=5 USES LINEAR INTERP PLUS RUNNING AVERAGE (needs nrun and endflag).
C               (terpflag=-5 same except uses sloping extrapolation)
C               (THERE IS NO LINEAR INTERPOLATION IF stepflag=3)
C               (THERE IS NO RUNNING AVERAGE IF nrun=1)
C  terpflag=6 FILLS IN POINTS WITHOUT CHANGING EARTH MODEL.
C               This means that a new point with abscissa lying between
C               X(i) and X(i+1) will have an ordinate equal to Z(i+1).
C               (HAS NO EFFECT IF stepflag=3)

      implicit none
      integer terpflag,stepflag,endflag
      integer nwant,ncoef,nrun,n,nn,nmax,ierr,   ncoef2,K
      REAL dx1,dx2,dz,X(*),Z(*),XX(*),ZZ(*),Q(*)
      real slope,dxuse1,dxuse2,sigma,dx
      character*(*) msg
      real zip1,terp1,yspline,running
C--------------------------get desired increment and number of points.
      msg=' '
      ierr=0
      if ((terpflag.lt.1.or.terpflag.gt.6).and.
     $    terpflag.ne.-1.and.terpflag.ne.-5) then
            msg='error in densify - terpflag must be 1,2,3,4,5,6,-1,-5'
            go to 999
      else if (stepflag.lt.1.or.stepflag.gt.4) then
            msg='error in densify - stepflag must be between 1 and 4'
            go to 999
      else if (endflag.lt.1.or.endflag.gt.4) then
            msg='error in densify - endflag must be between 1 and 4'
            go to 999
      else if (n.eq.0) then
            nn=0
            return
      else if (n.lt.0) then
            msg='error in densify - input number cannot be negative'
            go to 999
      else if (n.gt.nmax) then
            msg='error in densify - input number too large'
            go to 999
      else if (stepflag.eq.1) then            ! use DX1 and DX2 to resample.
            if (dx1.le.0..or.dx2.le.0.) then
                 msg='error in densify - zero or negative increment'
                 go to 999
            end if
            dxuse1=dx1
            dxuse2=dx2
ccccc       NN=1+nint((X(N)-X(1))/dxuse1)            ! old method.
            slope=0.0
            if (x(n).gt.x(1)) slope=(dxuse2-dxuse1)/(x(n)-x(1))
            nn=nmax+1
            DO K=1,nmax
                  if (k.eq.1) then
                       xx(k)=x(1)
                  else if (nn.eq.nmax+1) then
                       dx=dxuse1+(xx(k-1)-xx(1))*slope
                       xx(k)=xx(k-1)+dx
                       if (xx(k).gt.x(n)-0.5*dx) then
                            xx(k)=x(n)
                            nn=k
                       end if
                  end if
            end do
      else if (stepflag.eq.2) then                 ! use NWANT to resample.
            if (nwant.le.0) then
                 msg='error in densify - number wanted must exceed zero'
                 go to 999
            end if
            dxuse1=(X(N)-X(1))/max(nwant-1,1)
            dxuse2=dxuse1
            if (dxuse1.le.0.) then
                msg='error in densify - abscissae values not increasing'
                go to 999
            end if
            NN=NWANT
      else if (stepflag.eq.3) then                 ! retain current picks.
            dxuse1=0.
            dxuse2=0.
            NN=N
      else if (stepflag.eq.4) then          ! use picks already in xx(nn).
            dxuse1=0.
            dxuse2=0.
            if (terpflag.eq.6) then
              msg='error in densify - terpflag=6 and stepflag=4 illegal'
              go to 999
            end if
      else
           msg='error in densify - illegal value of stepflag'
           go to 999
      end if
      if (nn.le.0) then
           msg='error in densify - output number must exceed zero'
           go to 999
      else if (nn.gt.nmax) then
           msg='error in densify - output number too large'
           go to 999
      end if
C--------------------------initialize the resampling.
      if (terpflag.eq.3) then                             ! spline.
          DO K=1,N
               Q(K)=1.
          END DO
          CALL SPLINE (dz,N,X,Z,Q,  Q(N+1),Q(5*N+1),SIGMA)
      else if (terpflag.eq.4) then                        ! polynomial.
          NCOEF2=MAX0(1,MIN0(N,ncoef))
ccccc     CALL LEASQ1 (N,X,Z,NCOEF2,   Q,Q(N+1),SIGMA)
          call feed (ncoef2,x,z,n)
          sigma = 0.0
      else if (terpflag.eq.6) then                        ! without change.
          CALL DENSIFY2_nonlin                            ! resets NN.
     $              (dxuse1,dxuse2,nmax,  N,X,Z,  NN,XX,ZZ,  msg,ierr) 
          return
      end if
C----------GO THROUGH THE LOOP.
      slope=0.0
      if (x(n).gt.x(1)) slope=(dxuse2-dxuse1)/(x(n)-x(1))
      DO K=1,NN
        if (stepflag.eq.1) then
            continue
        else if (k.eq.nn.and.stepflag.ne.4) then
            xx(k)=x(n)
        else if (dxuse1.gt.0.) then                   ! dxuse1=dxuse2 here.
            XX(K)=X(1)+dxuse1*(K-1)
        else if (stepflag.eq.3) then                  ! retain current picks.
            XX(K)=X(K)
        end if
        if (terpflag.eq.2) then                       ! cubic.
            CALL PITY (XX(K),X,N,  Q)
            ZZ(K)=ZIP1(Q,Z)
        else if (terpflag.eq.3) then                  ! spline.
            ZZ(K)=YSPLINE(1,XX(K),X,Q(N+1),N)
        else if (terpflag.eq.1.or.terpflag.eq.5) then ! linear or linear+run.
            ZZ(K)=TERP1(XX(K),X,N,Z)
        else if (terpflag.eq.-1.or.terpflag.eq.-5) then ! linear or linear+run.
            ZZ(K)=TERP1(XX(K),X,-N,Z)   ! sloping extrapolation.
        end if
      END DO
      if (terpflag.eq.4) then
          call feed (0,xx,zz,nn)
      end if
C----------FINISH UP.
      if (terpflag.eq.5.or.terpflag.eq.-5) then        ! linear+run.
           DO K=1,NN
                Q(K)=RUNNING(K,ZZ,NN,nrun,endflag)
           END DO
           DO K=1,NN
                ZZ(K)=Q(K)
           END DO
      end if
      ierr=0
      return
C--------------------------------------error.
999   nn=n
      ierr=1
      if (nn.le.0.or.nn.gt.nmax) return
      do k=1,nn
           xx(k)=x(k)
           zz(k)=z(k)
      end do
      return
      END 





      subroutine densify (terpflag,stepflag,endflag,
     $             dx,nwant,dz,ncoef,nrun,
     $             n,x,z,  nn,xx,zz,  nmax,q,  msg,ierr)
c  Densifies or smooths array (x,z) and puts results into (xx,zz).
C  dx    (for stepflag=1) = SAMPLE INTERVAL FOR DENSIFYING.
C  nwant (for stepflag=2) = number of output picks wanted.
C  dz      (for terpflag=3) = AVERAGE DEVIATION OF SPLINE CURVE FROM PICKS.
C  ncoef   (for terpflag=4) = NUMBER OF COEFFICIENTS IN POLYNOMIAL.
C  nrun    (for terpflag=5,-5) = NUMBER OF POINTS TO USE IN RUNNING AVERAGE.
C  endflag (for terpflag=5,-5) = END EFFECT FLAG FOR RUNNING AVERAGE.
C  X(N),Z(N)     = the abscissae and ordinates of the original points.
C  XX(NN),ZZ(NN) = the abscissae and ordinates of the returned points.
C  nmax          = maximum allowed size of NN.
C  Q(12*NMAX)    = SCRATCH ARRAY.
C  msg  = error message (blank if no error).
C  ierr = error flag (0 if no error) (any error returns an unchanged function).
C  stepflag=1 uses DX to set the increment for the new abscissae.
C  stepflag=2 uses NWANT to set the increment for the new abscissae.
C  stepflag=3 retains the current abscissae.
C  stepflag=4 uses the abscissae already in xx (nn and xx are preset).
C    (for stepflag=1,2,3: RESAMPLING STARTS AT X(1) AND FINISHES AT X(N))
C  terpflag=1 USES LINEAR INTERPOLATION with flat extrapolation.
C               (terpflag=-1 same except uses sloping extrapolation)
C               (HAS NO EFFECT IF stepflag=3)
C  terpflag=2 USES 4-POINT CUBIC INTERPOLATION.
C               (HAS NO EFFECT IF stepflag=3)
C  terpflag=3 USES RELAXED SPLINE INTERPOLATION (needs dz).
C               (HAS NO EFFECT IF dz=0 AND stepflag=3)
C  terpflag=4 USES POLYNOMIAL FIT (needs ncoef).
C               (HAS NO EFFECT IF ncoef=N AND stepflag=3)
C               (RETURNS AN AVERAGE IF ncoef=1)  (NN MIGHT AS WELL = 1)
C  terpflag=5 USES LINEAR INTERP PLUS RUNNING AVERAGE (needs nrun and endflag).
C               (terpflag=-5 same except uses sloping extrapolation)
C               (THERE IS NO LINEAR INTERPOLATION IF stepflag=3)
C               (THERE IS NO RUNNING AVERAGE IF nrun=1)
C  terpflag=6 FILLS IN POINTS WITHOUT CHANGING EARTH MODEL.
C               This means that a new point with abscissa lying between
C               X(i) and X(i+1) will have an ordinate equal to Z(i+1).
C               (HAS NO EFFECT IF stepflag=3)

      implicit none
      integer terpflag,stepflag,endflag
      integer nwant,ncoef,nrun,n,nn,nmax,ierr,   ncoef2,K
      REAL dx,dz,X(*),Z(*),XX(*),ZZ(*),Q(*),   dx2,sigma
      character*(*) msg
cccc  real zip1,terp1,amult,yspline,running
      real zip1,terp1,yspline,running

C--------------------------get desired increment and number of points.
      msg=' '
      ierr=0
      if ((terpflag.lt.1.or.terpflag.gt.6).and.
     $    terpflag.ne.-1.and.terpflag.ne.-5) then
            msg='error in densify - terpflag must be 1,2,3,4,5,6,-1,-5'
            go to 999
      else if (stepflag.lt.1.or.stepflag.gt.4) then
            msg='error in densify - stepflag must be between 1 and 4'
            go to 999
      else if (endflag.lt.1.or.endflag.gt.4) then
            msg='error in densify - endflag must be between 1 and 4'
            go to 999
      else if (n.eq.0) then
            nn=0
            return
      else if (n.lt.0) then
            msg='error in densify - input number cannot be negative'
            go to 999
      else if (n.gt.nmax) then
            msg='error in densify - input number too large'
            go to 999
      else if (stepflag.eq.1) then                 ! use DX to resample.
            if (dx.le.0.) then
                 msg='error in densify - zero or negative increment'
                 go to 999
            end if
            DX2=DX
            NN=1+nint((X(N)-X(1))/DX2)
      else if (stepflag.eq.2) then                 ! use NWANT to resample.
            if (nwant.le.0) then
                 msg='error in densify - number wanted must exceed zero'
                 go to 999
            end if
            DX2=(X(N)-X(1))/max(nwant-1,1)
            if (dx2.le.0.) then
                msg='error in densify - abscissae values not increasing'
                go to 999
            end if
            NN=NWANT
      else if (stepflag.eq.3) then                 ! retain current picks.
            DX2=0.
            NN=N
      else if (stepflag.eq.4) then          ! use picks already in xx(nn).
            DX2=0.
            if (terpflag.eq.6) then
              msg='error in densify - terpflag=6 and stepflag=4 illegal'
              go to 999
            end if
      else
           msg='error in densify - illegal value of stepflag'
           go to 999
      end if
      if (nn.le.0) then
           msg='error in densify - output number must exceed zero'
           go to 999
      else if (nn.gt.nmax) then
           msg='error in densify - output number too large'
           go to 999
      end if
C--------------------------initialize the resampling.
      if (terpflag.eq.3) then                             ! spline.
          DO K=1,N
               Q(K)=1.
          END DO
          CALL SPLINE (dz,N,X,Z,Q,  Q(N+1),Q(5*N+1),SIGMA)
      else if (terpflag.eq.4) then                        ! polynomial.
          NCOEF2=MAX0(1,MIN0(N,ncoef))
ccccc     CALL LEASQ1 (N,X,Z,NCOEF2,   Q,Q(N+1),SIGMA)
          call feed (ncoef2,x,z,n)
          sigma = 0.0
      else if (terpflag.eq.6) then                        ! without change.
          CALL DENSIFY2 (dx2,nmax,  N,X,Z,  NN,XX,ZZ,  msg,ierr) 
          return
      end if
C----------GO THROUGH THE LOOP.
      DO K=1,NN
        if (k.eq.nn.and.stepflag.ne.4) then
            xx(k)=x(n)
        else if (dx2.gt.0.) then
            XX(K)=X(1)+DX2*(K-1)
        else if (stepflag.eq.3) then                  ! retain current picks.
            XX(K)=X(K)
        end if
        if (terpflag.eq.2) then                       ! cubic.
            CALL PITY (XX(K),X,N,  Q)
            ZZ(K)=ZIP1(Q,Z)
        else if (terpflag.eq.3) then                  ! spline.
            ZZ(K)=YSPLINE(1,XX(K),X,Q(N+1),N)
ccccc   else if (terpflag.eq.4) then                  ! polynomial.
ccccc       CALL POLYA (XX(K),NCOEF2,Q(N+1))
ccccc       ZZ(K)=AMULT(NCOEF2,Q,Q(N+1))
        else if (terpflag.eq.1.or.terpflag.eq.5) then ! linear or linear+run.
            ZZ(K)=TERP1(XX(K),X,N,Z)
        else if (terpflag.eq.-1.or.terpflag.eq.-5) then ! linear or linear+run.
            ZZ(K)=TERP1(XX(K),X,-N,Z)   ! sloping extrapolation.
        end if
      END DO
      if (terpflag.eq.4) then
          call feed (0,xx,zz,nn)
      end if
C----------FINISH UP.
      if (terpflag.eq.5.or.terpflag.eq.-5) then        ! linear+run.
           DO K=1,NN
                Q(K)=RUNNING(K,ZZ,NN,nrun,endflag)
           END DO
           DO K=1,NN
                ZZ(K)=Q(K)
           END DO
      end if
      ierr=0
      return
C--------------------------------------error.
999   nn=n
      ierr=1
      if (nn.le.0.or.nn.gt.nmax) return
      do k=1,nn
           xx(k)=x(k)
           zz(k)=z(k)
      end do
      return
      END 






c      SUBROUTINE junk_DENSE (OPT,DX,C,M,MM,CC, N,X,Z, NN,XX,ZZ, NNN,Q)
cC     DENSIFIES OR SMOOTHS ARRAY (X,Z) AND PUTS RESULTS INTO (XX,ZZ).
cC     C  (for OPT=S) = AV ERROR BY WHICH SPLINE CURVE MAY DEVIATE FROM PICKS.
cC     M  (for OPT=F) = NUMBER OF COEFFICIENTS IN POLYNOMIAL.
cC     MM (for OPT=R) = NUMBER OF POINTS TO USE IN RUNNING AVERAGE.
cC     CC (for OPT=R) = END EFFECT CODE = TT,SS,EE,NN.  (ADD LL AND GG LATER)
cC     Q = SCRATCH ARRAY (DIMENSIONED 12*NNN).
cC     NNN =  maximum allowed size of NN.
cC     DX = SAMPLE INTERVAL FOR DENSIFYING.
cC            DX=0 MEANS DO NOT RESAMPLE.
cC            DX<0 MEANS NN (RATHER THAN DX) SPECIFIES THE RESAMPLING.
cC            (DX<0 AND NN=N RESAMPLES TO EQUAL SPACING W/O CHANGING N)
cC            (ALL RESAMPLING STARTS AT X(1) AND DOES NOT EXCEED X(N))
cC     OPT=L DENSIFIES WITH LINEAR INTERPOLATION.
cC            (HAS NO EFFECT IF DX=0)
cC     OPT=C DENSIFIES WITH 4-POINT CUBIC INTERPOLATION.
cC            (HAS NO EFFECT IF DX=0)
cC     OPT=S DENSIFIES WITH RELAXED SPLINE INTERPOLATION.
cC            C = AV ERROR BY WHICH SPLINE CURVE MAY DEVIATE FROM PICKS.
cC            (HAS NO EFFECT IF C=0 AND DX=0)
cC     OPT=F DENSIFIES WITH POLYNOMIAL FIT.
cC            M = NUMBER OF COEFFICIENTS IN POLYNOMIAL.
cC            (HAS NO EFFECT IF M=N AND DX=0)
cC            (RETURNS AN AVERAGE IF M=1)  (NN MIGHT AS WELL = 1)
cC     OPT=R DENSIFIES WITH LINEAR INTERP FOLLOWED BY RUNNING AVERAGE.
cC            MM = NUMBER OF POINTS TO USE IN RUNNING AVERAGE.
cC            CC = END EFFECT CODE = TT,SS,EE,NN.  (ADD LL AND GG LATER)
cC            (THERE IS NO LINEAR INTERPOLATION IF DX=0)
cC            (THERE IS NO RUNNING AVERAGE IF MM=1)
cC     OPT=M DENSIFIES BY FILLING IN POINTS WITHOUT CHANGING EARTH MODEL.
cC            This means that a new point with abscissa lying between
cC            X(i) and X(i+1) will have an ordinate equal to Z(i+1).
cC            (HAS NO EFFECT IF DX=0)
cC     RETURNS Q(1)=0 IF OPT=L OR OPT=M; otherwise returns Q(1)=1.
c      DIMENSION X(*),Z(*),XX(*),ZZ(*),Q(*)
c      CHARACTER*(*) OPT,CC
cC----------GET STARTED.
c      if (N.EQ.0) then
c           NN=0
c           go to 99
c      end if
cC----------DENSIFY THE VELOCITY FUNCTION.
c      IF (OPT.EQ.'S') CALL FILL_reals (Q,N,1.)
c      IF (OPT.EQ.'S') CALL SPLINE (C,N,X,Z,Q,  Q(N+1),Q(5*N+1),SIGMA)
c      IF (OPT.EQ.'F') NCOEF=MAX0(1,MIN0(N,M))
c      IF (OPT.EQ.'F') CALL LEASQ1 (N,X,Z,NCOEF,   Q,Q(N+1),SIGMA)
c      IF (OPT.EQ.'M') CALL junk_DENSE2 (N,X,Z,DX,NN,XX,ZZ)
c      IF (OPT.EQ.'M') GO TO 99
cC----------GO THROUGH THE LOOP.
c      DO 47 K=1,NN
c      IF (DX.EQ.0.) XX(K)=X(K)
c      IF (DX.GT.0.) XX(K)=X(1)+DX*(K-1)
c      IF (DX.LT.0.) XX(K)=X(1)+(K-1)*(X(N)-X(1))/(NN-1)
c      IF (OPT.EQ.'C') CALL PITY (XX(K),X,N,  Q)
c      IF (OPT.EQ.'C') ZZ(K)=ZIP1(Q,Z)
c      IF (OPT.EQ.'L'.OR.OPT.EQ.'R') ZZ(K)=TERP1(XX(K),X,N,Z)
c      IF (OPT.EQ.'F') CALL POLYA (XX(K),NCOEF,Q(N+1))
c      IF (OPT.EQ.'F') ZZ(K)=AMULT(NCOEF,Q,Q(N+1))
c      IF (OPT.EQ.'S') ZZ(K)=YSPLINE(1,XX(K),X,Q(N+1),N)
c47    CONTINUE
c      IF (OPT.NE.'R') GO TO 99
c      DO 48 K=1,NN
c48    Q(K)=junk_RUNAV(K,ZZ,NN,MM,CC)
c      CALL MOVE_reals (Q,ZZ,NN)
cC----------FINISH UP.
c99    Q(1)=1.
c      IF (OPT.EQ.'L'.OR.OPT.EQ.'M') Q(1)=0.
c      RETURN
c      END
c
   





      SUBROUTINE DENSIFY2 (dx,nmax,  N,X,Z,  NN,XX,ZZ,  msg,ierr) 
C     FILL IN POINTS WITHOUT REMOVING ANY POINTS. 
C     USED NORMALLY FOR DENSIFYING AN INTERVAL VELOCITY FUNCTION WITHOUT
C       CHANGING THE EARTH MODEL.
C            This means that a new point with abscissa lying between
C            X(i) and X(i+1) will have an ordinate equal to Z(i+1).
C     CALLED FROM SUBROUTINE DENSIFY, but also callable from elsewhere. 
C     X(N),Z(N) are the abscissae and ordinates of the original points.
C     XX(N),ZZ(N) are the abscissae and ordinates of the returned points.
C     any error returns an unchanged function.
C     returns ierr=1 if an error occurs; otherwise 0.

      implicit none
      integer nmax,n,nn,ierr,i
      REAL dx,X(*),Z(*),XX(*),ZZ(*),a
      character*(*) msg
C-----------------------------------------GET STARTED. 
      ierr=0
      msg=' '
      NN=0
      if (n.eq.0) return
      if (dx.le.0.) then
           msg='error in densify2 - zero or negative increment'
           go to 999
      end if
C-----------------------------------------GO THRU LOOP.
      A=0. 
      DO I=1,N 
           NN=NN+1 
           if (nn.gt.nmax) go to 999
           XX(NN)=X(I) 
           ZZ(NN)=Z(I) 
           IF (I.EQ.N) RETURN
5          A=A+DX 
           IF (A.LT.X(I)+DX/2.) GO TO 5
           IF (A.GT.X(I+1)-DX/2.) GO TO 20 
           NN=NN+1 
           if (nn.gt.nmax) then
             msg='error in densify2 - trying to create too many points'
             go to 999
           end if
           XX(NN)=A    
           ZZ(NN)=Z(I+1) 
           GO TO 5 
20         A=A-DX 
      END DO
      RETURN
C--------------------------------------error.
999   nn=n
      do i=1,nn
           xx(i)=x(i)
           zz(i)=z(i)
      end do
      ierr=1
      return
      END 

           



      SUBROUTINE DENSIFY2_nonlin
     $                    (dx1,dx2,nmax,  N,X,Z,  NN,XX,ZZ,  msg,ierr) 
C     FILL IN POINTS WITHOUT REMOVING ANY POINTS. 
C     USED NORMALLY FOR DENSIFYING AN INTERVAL VELOCITY FUNCTION WITHOUT
C       CHANGING THE EARTH MODEL.
C            This means that a new point with abscissa lying between
C            X(i) and X(i+1) will have an ordinate equal to Z(i+1).
C     CALLED FROM SUBROUTINE DENSIFY, but also callable from elsewhere. 
C     X(N),Z(N) are the abscissae and ordinates of the original points.
C     XX(N),ZZ(N) are the abscissae and ordinates of the returned points.
C     any error returns an unchanged function.
C     returns ierr=1 if an error occurs; otherwise 0.

      implicit none
      integer nmax,n,nn,ierr,i
      REAL dx1,dx2,X(*),Z(*),XX(*),ZZ(*),a,dx,slope
      character*(*) msg
C-----------------------------------------GET STARTED. 
      ierr=0
      msg=' '
      NN=0
      if (n.eq.0) return
      if (dx1.le.0..or.dx2.le.0.) then
           msg='error in densify2_nonlin - zero or negative increment'
           go to 999
      end if
C-----------------------------------------GO THRU LOOP.
      slope=0.0
      if (x(n).ne.x(1)) slope=(dx2-dx1)/(x(n)-x(1))
      A=0. 
      DO I=1,N 
           dx=dx1+(x(i)-x(1))*slope
           NN=NN+1 
           if (nn.gt.nmax) go to 999
           XX(NN)=X(I) 
           ZZ(NN)=Z(I) 
           IF (I.EQ.N) RETURN
5          A=A+DX
           IF (A.LT.X(I)+DX/2.) GO TO 5
           IF (A.GT.X(I+1)-DX/2.) GO TO 20 
           NN=NN+1 
           if (nn.gt.nmax) then
             msg=
     $   'error in densify2_nonlin - trying to create too many points'
             go to 999
           end if
           XX(NN)=A    
           ZZ(NN)=Z(I+1) 
           GO TO 5 
20         A=A-DX
      END DO
      RETURN
C--------------------------------------error.
999   nn=n
      do i=1,nn
           xx(i)=x(i)
           zz(i)=z(i)
      end do
      ierr=1
      return
      END 

           


      SUBROUTINE DENSIFY3 (dx,nmax,  N,X,Z,  NN,XX,ZZ,  ierr) 
C     FILL IN POINTS WITHOUT REMOVING ANY POINTS. 
C     Differs from densify2 in two ways:
C          (1) does not have msg argument (so can be called from c).
C          (2) the ordinates are linearly interpolated (hence the
C                 earth model is changed).
C     USED NORMALLY FOR DENSIFYING AN NMO, RMS, OR AVERAGE VELOCITY 
C       FUNCTION WITHOUT LOSING THE KNEES OF THE FUNCTION.
C     NOT CALLED FROM SUBROUTINE DENSIFY. 
C     X(N),Z(N) are the abscissae and ordinates of the original points.
C     XX(NN),ZZ(NN) are the abscissae and ordinates of the returned points.
C     any error returns an unchanged function.
C     returns ierr=1 if an error occurs; otherwise 0.

      implicit none
      integer nmax,n,nn,ierr,i
      REAL dx,X(*),Z(*),XX(*),ZZ(*),a,terp1
      real dx2                                          ! new 2/25/99
      integer npredict                                  ! new 2/25/99
C-----------------------------------------GET STARTED. 
      ierr=0
      NN=0
      if (n.eq.0) return
      if (dx.le.0.) then
           go to 999
      end if
      dx2 = dx                                          ! new 2/25/99
      npredict = n + (x(n)-x(1))/dx2 + 1                ! new 2/25/99
      if (npredict.gt.nmax) dx2 = npredict*dx2/nmax     ! new 2/25/99
C-----------------------------------------GO THRU LOOP.
      A=0. 
      DO I=1,N 
           NN=NN+1 
           if (nn.gt.nmax) go to 999
           XX(NN)=X(I) 
           IF (I.EQ.N) go to 21
!5          A=A+DX                                      ! old
!           IF (A.LT.X(I)+DX/2.) GO TO 5                ! old
!           IF (A.GT.X(I+1)-DX/2.) GO TO 20             ! old
5          A=A+DX2                                      ! new 2/25/99
           IF (A.LT.X(I)+DX2/2.) GO TO 5                ! new 2/25/99
           IF (A.GT.X(I+1)-DX2/2.) GO TO 20             ! new 2/25/99
           NN=NN+1 
           if (nn.gt.nmax) then
             go to 999
           end if
           XX(NN)=A    
           GO TO 5 
20         A=A-DX 
      END DO
21    do i=1,nn
           zz(i)=terp1(xx(i),x,n,z)
      end do
      RETURN
C--------------------------------------error.
999   nn=n
      do i=1,nn
           xx(i)=x(i)
           zz(i)=z(i)
      end do
      ierr=1
      return
      END 

           


c      SUBROUTINE junk_DENSE2 (N,T,V,DT,NN,TT,VV) 
cC     FILL IN POINTS WITHOUT CHANGING EARTH MODEL.
cC            This means that a new point with abscissa lying between
cC            T(i) and T(i+1) will have an ordinate equal to V(i+1).
cC     CALLED FROM SUBROUTINE DENSE. 
cC     T(N),V(N) are the abscissae and ordinates of the original points.
cC     TT(N),VV(N) are the abscissae and ordinates of the returned points.
cC     DT=0 returns an unchanged function.
cC     DT>0 sets the increment for the new abscissae.
cC     DT<0 is a flag to set the increment to the average input increment.
c      DIMENSION T(*),V(*),TT(*),VV(*) 
cC----------GET STARTED. 
c      if (n.eq.0) then
c           nn=0
c           return
c      end if
c      IF (DT.EQ.0.) THEN
c           CALL MOVE_reals (T,TT,N) 
c           CALL MOVE_reals (V,VV,N) 
c           RETURN 
c      END IF
c      DT2=DT
c      IF (DT.LT.0.) DT2=(T(N)-T(1))/(NN-1)
cC----------GO THRU LOOP.
c      NN=0
c      TIME=0. 
c      DO 20 I=1,N 
c      NN=NN+1 
c      TT(NN)=T(I) 
c      VV(NN)=V(I) 
c      IF (I.EQ.N) RETURN
c5     TIME=TIME+DT2 
c      IF (TIME.LT.T(I)+DT2/2.) GO TO 5
c      IF (TIME.GT.T(I+1)-DT2/2.) GO TO 20 
c      NN=NN+1 
c      TT(NN)=TIME 
c      VV(NN)=V(I+1) 
c      GO TO 5 
c20    TIME=TIME-DT2 
c      RETURN
c      END 
c
           




c      SUBROUTINE CURVFIT (OPT,NCOEF,NRUN,IEND,ZMISS,N,X,Z,Q) 
cC     FITS CURVE TO POINTS (X,Z) AND RETURNS DESIRED POINTS ON CURVE. 
cC     ARGUMENTS IN ENTRY CURVFIT MUST REMAIN UNCHANGED FOR ENTRY ANSWER.
cC     Q = SCRATCH ARRAY (DIMENSIONED AT LEAST 12*N OR 2*NCOEF OR N+9).
cC     OPT=L (OR UNRECOGNIZED) DOES LINEAR INTERPOLATION.
cC     OPT=C DOES 4-POINT CUBIC INTERPOLATION.  NEEDS Q(9)
cC     OPT=S DOES RELAXED SPLINE INTERPOLATION.  NEEDS Q(12*N) 
cC          ZMISS = AV ERROR BY WHICH SPLINE CURVE MAY DEVIATE FROM PICK.
cC     OPT=F DOES POLYNOMIAL FIT.  NEEDS Q(2*NCOEF) 
cC          NCOEF = NUMBER OF COEFFICIENTS IN POLYNOMIAL.
cC     OPT=R DOES CUBIC INTERPOLATION FROM A RUNNING AVERAGE.  NEEDS Q(N).
cC          NRUN = NUMBER OF POINTS TO USE IN RUNNING AVERAGE. 
cC          IEND = END EFFECT CODE = TT,SS,EE,NN.
c      DIMENSION X(N),Z(N),Q(*)
c      CHARACTER*(*) OPT,IEND
cC----------DO THE CURVE FITTING.
c      IF (OPT.EQ.'S') THEN
c           CALL FILL_reals (Q,N,1.) 
c           CALL SPLINE (ZMISS,N,X,Z,Q,  Q(N+1),Q(5*N+1),SIGMA)
c      ELSE IF (OPT.EQ.'F') THEN 
cCCCCCCC           DO 10 I=1,N
cCCCCCCC10         CALL FEED (NCOEF,X(I)/100.,Z(I)/10000.)
c           CALL FEED (NCOEF,X,Z,N)
cCCCC           CALL POLYA (X(I),NCOEF,Q)   
cCCCC10         CALL LEAST (NCOEF,Q,Z(I),1.) 
cCCCC           CALL LEAST (0,Q,Q(NCOEF+1),SIGMA)
c      ELSE IF (OPT.EQ.'R') THEN 
c           DO 20 I=1,N
c20         Q(I)=junk_RUNAV(I,Z,N,NRUN,IEND)
c      END IF
c      RETURN
cC----------RETURN A POINT ON THE FITTED CURVE.
c      ENTRY ANSWER (OPT,NCOEF,NRUN,IEND,ZMISS,N,X,Z,Q,  XX,YY)
c      IF (OPT.EQ.'C') THEN
c           CALL PITY (XX,X,N,  Q) 
c           YY=ZIP1(Q,Z) 
c      ELSE IF (OPT.EQ.'F') THEN 
c           CALL FEED (0,XX,YY,1)
cCCCCCCC           CALL FEED (0,XX/100.,YY)
cCCCCCCC           YY=10000.*YY
cCCCC           CALL POLYA (XX,NCOEF,Q(N+1))
cCCCC           YY=AMULT(NCOEF,Q,Q(N+1))
c      ELSE IF (OPT.EQ.'S') THEN                    
c           YY=YSPLINE(1,XX,X,Q(N+1),N)
c      ELSE IF (OPT.EQ.'R') THEN 
c           CALL PITY (XX,X,N,   Q(N+1)) 
c           YY=ZIP1(Q(N+1),Q)
c      ELSE
c           YY=TERP1(XX,X,N,Z) 
c      END IF
c      RETURN
c      END 




      SUBROUTINE FEED (NCOEF,X,Y,N)
C     DOUBLE PRECISION LINEAR POLYNOMIAL LEAST SQUARES ROUTINE.
C     ALSO NORMALIZES THE COORDINATES TO REDUCE EXPONENT RANGE.
C     COORDS X AND Y ARE LINEARLY TRANSFORMED (INTERNAL TO THIS SUBROUTINE
C        ONLY) TO FALL BETWEEN VALUES 1 AND 2.
C     CALL FEED (NCOEF,X,Y,N) FOR ALL OF THE INPUT POINTS.
C     CALL FEED (0,X,Y,N) TO GET ORDINATES OF A SET OF POINTS ON FITTED CURVE.
C----------DIMENSION STATEMENTS.
      DIMENSION X(N),Y(N),ECOEF(10)
c     DOUBLE PRECISION TERMS(10),COEF(10),SUM,TERM,ZERO,ONE
      DOUBLE PRECISION TERMS(10),COEF(10),TERM,ZERO,ONE
      SAVE NC,COEF,TERMS,ZERO,ONE,AX,BX,AY,BY
      DATA ZERO,ONE/0.D0,1.D0/
C----------GET STARTED.
      IF (NCOEF.LE.0) GO TO 60
      NC=MIN0(NCOEF,10)
C----------FIND NORMALIZERS.
      XLO=X(1)
      XUP=X(1)
      YLO=Y(1)
      YUP=Y(1)
      DO 2 I=1,N
      XLO=AMIN1(X(I),XLO)
      XUP=AMAX1(X(I),XUP)
      YLO=AMIN1(Y(I),YLO)
2     YUP=AMAX1(Y(I),YUP)
      IF (XUP.EQ.XLO) THEN
           BX=1.
           AX=0.
      ELSE
           BX=1./(XUP-XLO)
           AX=1-BX*XLO
      END IF
      IF (YUP.EQ.YLO) THEN
           BY=1.
           AY=0.
      ELSE
           BY=1./(YUP-YLO)
           AY=1-BY*YLO
      END IF
C----------GATHER THE DATA.
      DO 20 I=1,N
      XNEW=AX+BX*X(I)
      YNEW=AY+BY*Y(I)
      TERMS(1)=ONE
      IF (NC.GT.1) THEN
           DO 10 J=2,NC
10         TERMS(J)=XNEW*TERMS(J-1) 
      END IF
20    CALL LEASTD (NC,TERMS,YNEW,1.)
C----------DO THE CURVE FITTING.
      CALL LEASTD (0,COEF,ECOEF,SIGMA)
      RETURN
C----------GET A SET OF POINTS ON THE FITTED CURVE.
60    DO 90 I=1,N
      XNEW=AX+BX*X(I)
      YNEW=ZERO
      TERM=ONE
      DO 80 J=1,NC
      YNEW=YNEW+COEF(J)*TERM
80    TERM=TERM*XNEW
90    Y(I)=(YNEW-AY)/BY
      RETURN
      END

