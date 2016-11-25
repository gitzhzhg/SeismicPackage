
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
c                      static_fortran.f
c                      subdirectory stat


cC     INTERACTIVE STATIC EDITING PROGRAM.
cC----------DIMENSION AND DATA STATEMENTS.
c      PARAMETER (NNNN=2000000,KKKK=5000)
cc     PARAMETER (NNNN=2,KKKK=5)
c      DIMENSION S(NNNN),S2(NNNN),S3(NNNN),SAVE(NNNN)
cCCC   DIMENSION S(NX,NY)   !   S(IX,IY)=S(I)  WHERE  I=IX+(IY-1)*NX.
c      CHARACTER*8 OPTION,TYPE
c      CHARACTER*80 CARD(KKKK),VAXFILE
c      save s,s2,s3,save,card
cC----------INITIALIZATION.
c1     PRINT *, 'TYPE  READ   TO READ A CPS STATIC FILE      (DEFAULT)'
c      PRINT *, 'TYPE  BUILD  TO BUILD A NEW STATIC FILE'
c      PRINT *, 'TYPE  CYBER  TO READ A GENERIC STATIC FILE',
c     $                                ' (simple columns)'
cCCC   PRINT *, 'TYPE  SISC   TO WORK WITH SISC STATIC FILES'
c      PRINT *, 'TYPE  X      TO EXIT THE PROGRAM'
c      OPTION='READ'
c      CALL HITC (OPTION)
c      IF (OPTION.EQ.'READ'.OR.OPTION.EQ.'BUILD'.OR.OPTION.EQ.'X'
c     $    .OR.OPTION.EQ.'CYBER') GO TO 5
cCCC   IF (OPTION.EQ.'SISC') CALL SISC (S,S2,S3,SAVE,NNNN,CARD,KKKK)
c      GO TO 1
cC----------LIST MENU.
c2     PRINT *, 'TYPE  L   TO LIST STATIC FILE ATTRIBUTES'
c      PRINT *, 'TYPE  P   TO PLOT THE FILE'
c      PRINT *, 'TYPE  H   TO MAKE A HORIZONTAL PLOT'
c      PRINT *, 'TYPE  E   TO EDIT THE FILE'
c      PRINT *, 'TYPE  G   TO GRADE STATIC VALUES BETWEEN TWO GROUND',
c     $                               ' POSITIONS'
c      PRINT *, 'TYPE  GG  TO GRADE STATIC VALUES OVER A RECTANGULAR',
c     $                               ' AREA'
c      PRINT *, 'TYPE  I   TO INTEGRATE THE FILE'
c      PRINT *, 'TYPE  R   TO REMOVE A RUNNING AVERAGE'
c      PRINT *, 'TYPE  S   TO SMOOTH THE FILE'
c      PRINT *, 'TYPE  N   TO REPLACE NILS WITH INTERPOLATED VALUES'
c      PRINT *, 'TYPE  NX  TO INTERPOLATE NILS IN X DIRECTION',
c     $                                            ' (no extrapolation)'
c      PRINT *, 'TYPE  NY  TO INTERPOLATE NILS IN Y DIRECTION',
c     $                                            ' (no extrapolation)'
c      PRINT *, 'TYPE  NN  TO INTERPOLATE NILS ONLY FROM NEARBY VALUES'
c      PRINT *, 'TYPE  Z   TO REPLACE NILS WITH ZEROES'
c      PRINT *, 'TYPE  V1  TO REPLACE NILS WITH SPECIFIED VALUE'
c      PRINT *, 'TYPE  V2  TO REPLACE SPECIFIED VALUES WITH NILS'
c      PRINT *, 'TYPE  M   TO MULTIPLY THE STATIC VALUES BY A CONSTANT'
c      PRINT *, 'TYPE  A   TO ADD A CONSTANT TO THE STATIC VALUES'
c      PRINT *, 'TYPE  C   TO CHANGE THE GROUND POSITIONS'
c      PRINT *, 'TYPE  T   TO TRUNCATE (OR EXTEND OR RESAMPLE) THE FILE'
c      PRINT *, 'TYPE  J   TO JOIN OR COMPARE FILE WITH ANOTHER FILE'
c      PRINT *, 'TYPE  SAVE    TO SAVE A CPS STATIC FILE'
c      PRINT *, 'TYPE  FOREIGN TO SAVE A GENERIC STATIC FILE',
c     $                                            ' (simple columns)'
c      PRINT *, 'TYPE  READ    TO READ ANOTHER CPS STATIC FILE'
c      PRINT *, 'TYPE  BUILD   TO BUILD A NEW STATIC FILE'
c      PRINT *, 'TYPE  CYBER   TO READ A GENERIC STATIC FILE',
c     $                                            ' (simple columns)'
cCCC   PRINT *, 'TYPE  SISC    TO WORK WITH SISC STATIC FILES'
c      PRINT *, 'TYPE  DELETE  TO DELETE ALL E,G,GG,I,R,S,N,NX,NY,NN,Z,',
c     $                                          'V1,V2,M,A'
c      PRINT *, '                     DONE SINCE LAST C,T,J,READ,BUILD'
c      PRINT *, 'TYPE  HARD    TO MAKE A HARD COPY PRINTER-PLOT'
c      PRINT *, 'TYPE  X  TO EXIT THE PROGRAM'
c4     OPTION=' '
c      CALL HITC (OPTION)
cC----------ACT ON THE CHOSEN OPTION.
c5     IF (OPTION.EQ.'L') THEN
c           CALL OPTIONL (6,VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,
c     $        XINC,YINC,NX,NY,CARD,NCARD,S)
c      ELSE IF (OPTION.EQ.'P') THEN
c           CALL OPTIONP (1,X1,Y1,XINC,YINC,NX,NY,S,SAVE,S3)
c      ELSE IF (OPTION.EQ.'H') THEN
c           CALL OPTIONH (1,X1,Y1,XINC,YINC,NX,NY,S,SAVE,S3)
c      ELSE IF (OPTION.EQ.'E') THEN
c           CALL OPTIONE (X1,Y1,XINC,YINC,NX,NY,S,NCARD,CARD)
c      ELSE IF (OPTION.EQ.'G') THEN
c           CALL OPTIONG (X1,Y1,XINC,YINC,NX,NY,S,NCARD,CARD)
c      ELSE IF (OPTION.EQ.'GG') THEN
c           CALL OPTIONGG (X1,Y1,XINC,YINC,NX,NY,S,NCARD,CARD)
c      ELSE IF (OPTION.EQ.'I') THEN
c           CALL OPTIONI (TYPE,S,S2,NX,NY,CARD,NCARD)
c           CALL OPTIONN (S,NX,NY,CARD,NCARD)
c      ELSE IF (OPTION.EQ.'R'.OR.OPTION.EQ.'S') THEN
c           CALL OPTIONR (OPTION,S,S2,XINC,YINC,NX,NY,CARD,NCARD)
c      ELSE IF (OPTION.EQ.'N') THEN
c           CALL OPTIONN (S,NX,NY,CARD,NCARD)
c      ELSE IF (OPTION.EQ.'NX') THEN
c           CALL OPTIONNX (S,NX,NY,CARD,NCARD)
c      ELSE IF (OPTION.EQ.'NY') THEN
c           CALL OPTIONNY (S,NX,NY,CARD,NCARD)
c      ELSE IF (OPTION.EQ.'NN') THEN
c           CALL OPTIONNN (S,S3,NX,NY,CARD,NCARD)
c      ELSE IF (OPTION.EQ.'Z') THEN
c           CALL OPTIONZ (S,NX,NY,CARD,NCARD)
c      ELSE IF (OPTION.EQ.'V1') THEN
c           CALL OPTIONV1 (S,NX,NY,CARD,NCARD)
c      ELSE IF (OPTION.EQ.'V2') THEN
c           CALL OPTIONV2 (S,NX,NY,CARD,NCARD)
c      ELSE IF (OPTION.EQ.'M') THEN
c           CALL OPTIONM (S,NX,NY,CARD,NCARD)
c      ELSE IF (OPTION.EQ.'A') THEN
c           CALL OPTIONA (S,NX,NY,CARD,NCARD)
c      ELSE IF (OPTION.EQ.'C') THEN
c           CALL OPTIONC (NHX,NHY,NHX3,NHY3,
c     $        X1,Y1,XINC,YINC,NX,NY,S,CARD,NCARD)
c           CALL MOVE (S,SAVE,NX*NY)
c           NCAVE=NCARD
c      ELSE IF (OPTION.EQ.'T') THEN
c           CALL OPTIONT (X1,Y1,XINC,YINC,NX,NY,S,NCARD,CARD,S2)
c           CALL MOVE (S,SAVE,NX*NY)
c           NCAVE=NCARD
c      ELSE IF (OPTION.EQ.'J') THEN
c           CALL OPTIONJ (TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
c     $        NX,NY,S,NCARD,CARD,SAVE,S2,S3,NNNN,KKKK)
c           CALL MOVE (S,SAVE,NX*NY)
c           NCAVE=NCARD
c      ELSE IF (OPTION.EQ.'SAVE') THEN
c           CALL SFILE (VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
c     $        NX,NY,CARD,NCARD,S)
c      ELSE IF (OPTION.EQ.'FOREIGN') THEN
c           CALL FFILE (VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
c     $        NX,NY,CARD,NCARD,S)
c      ELSE IF (OPTION.EQ.'READ') THEN
c           CALL RFILE (VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
c     $        NX,NY,CARD,NCARD,S,NNNN,KKKK)
c           IF (VAXFILE.EQ.' ') GO TO 1
c           CALL MOVE (S,SAVE,NX*NY)
c           NCAVE=NCARD
c      ELSE IF (OPTION.EQ.'BUILD') THEN
c           CALL BFILE (VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
c     $        NX,NY,CARD,NCARD,S,NNNN)
c           CALL MOVE (S,SAVE,NX*NY)
c           NCAVE=NCARD
cCCC   ELSE IF (OPTION.EQ.'SISC') THEN
cCCC        CALL OPTIONS (OPTION,VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,S2,
cCCC  $        X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S,A,NNNN)
c      ELSE IF (OPTION.EQ.'CYBER') THEN
c           CALL CFILE (VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
c     $        NX,NY,CARD,NCARD,S,S2,S3,NNNN)
c           IF (VAXFILE.EQ.' ') GO TO 1
c           CALL MOVE (S,SAVE,NX*NY)
c           NCAVE=NCARD
c      ELSE IF (OPTION.EQ.'DELETE') THEN
c           CALL MOVE (SAVE,S,NX*NY)
c           NCARD=NCAVE
c      ELSE IF (OPTION.EQ.'HARD') THEN
c           CALL HFILE (VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
c     $        NX,NY,CARD,NCARD,S)
cC----------EXIT PROGRAM.
c      ELSE IF (OPTION.EQ.'X') THEN
c           STOP
cC----------GO BACK FOR ANOTHER OPTION.
c      ELSE
c           GO TO 2
c      END IF
c      PRINT *, 'CHOOSE DESIRED OPTION (OR HIT RETURN FOR MENU)'
c      GO TO 4
cC----------ERROR.
cccc999   PRINT *, 'ERROR HAS OCCURRED'
cccc      GO TO 1
c      END
                                





      SUBROUTINE OPTIONP (IFLAG,X1,Y1,XINC,YINC,NX,NY,S1,S2,S3)
C     PLOT STATIC FILE VERTICALLY.
C     IFLAG=1: 1ST PLOT IS NEW VALUES; 2ND PLOT IS OLD VALUES.
C     IFLAG=2: 1ST PLOT IS FIRST FILE; 2ND PLOT IS SECOND FILE.
C     IFLAG=3: 1ST PLOT IS FIRST 2 FILES; 2ND PLOT IS THIRD FILE.
C----------DIMENSION STATEMENTS.
      DIMENSION S1(NX,NY),S2(NX,NY),S3(NX,NY)
      CHARACTER*8 Q
C----------GET STARTED.
      IF (IABS(IFLAG).NE.3) CALL LIMITS (NX*NY,S1,S2,S2,   SLO,SUP)
      IF (IABS(IFLAG).EQ.3) CALL LIMITS (NX*NY,S1,S2,S3,   SLO,SUP)
      IF (NX.EQ.1) GO TO 222
C----------WE WILL PLOT IN X DIRECTION.
111   IY=1
5     IB=0
3     CALL IPLOT (NX,IA,IB,   X1,XINC,'XGP     ')
      IF (IA.EQ.0) GO TO 9
C----------PRINT ONE PAGE OF PLOT.
      CALL STATHEAD (SLO,SUP,IFLAG)
      Y=Y1+(IY-1)*YINC                                      
      DO 20 IX=IA,IB
      X=X1+(IX-1)*XINC
20    CALL STATLINE (IX,IA,IB,X,Y,S1(IX,IY),S2(IX,IY),S3(IX,IY),
     $   SLO,SUP,IFLAG)
      GO TO 3
C----------GO TO NEXT Y GROUND POSITION.
9     IF (NY.EQ.1) RETURN
      CALL IPICK (NY,IY,   Y1,YINC,'YGP     ')
      IF (IY.GT.0) GO TO 5
C----------DECIDE ON NEXT ACTION.
      PRINT *, 'DO YOU WISH TO PLOT IN THE Y DIRECTION?  (DEFAULT N)'
      Q='N'
      CALL HITQ (Q)
      IF (Q.EQ.'N') RETURN
C----------WE WILL PLOT IN Y DIRECTION.
222   IX=1
51    IB=0
31    CALL IPLOT (NY,IA,IB,   Y1,YINC,'YGP     ')
      IF (IA.EQ.0) GO TO 91
C----------PRINT ONE PAGE OF PLOT.
      CALL STATHEAD (SLO,SUP,-IFLAG)
      X=X1+(IX-1)*XINC                          
      DO 21 IY=IA,IB
      Y=Y1+(IY-1)*YINC                                      
21    CALL STATLINE (IY,IA,IB,X,Y,S1(IX,IY),S2(IX,IY),S3(IX,IY),
     $   SLO,SUP,-IFLAG)
      GO TO 31
C----------GO TO NEXT X GROUND POSITION.
91    IF (NX.EQ.1) RETURN
      CALL IPICK (NX,IX,   X1,XINC,'XGP     ')
      IF (IX.GT.0) GO TO 51
C----------DECIDE ON NEXT ACTION.
      PRINT *, 'DO YOU WISH TO PLOT IN THE X DIRECTION?  (DEFAULT N)'
      Q='N'
      CALL HITQ (Q)
      IF (Q.EQ.'N') RETURN
      GO TO 111
      END

         




                 

      SUBROUTINE STATHEAD (SLO,SUP,IFLAG)
C     CALL STATHEAD TO PRINT HEADER OF STATIC FILE PLOT.
C     CALL STATLINE TO PRINT ONE LINE OF STATIC FILE PLOT.
C     IFLAG=+-1: 1ST PLOT IS OLD VALUES, 2ND PLOT IS NEW VALUES.
C     IFLAG=+-2: 1ST PLOT IS FIRST FILE, 2ND PLOT IS SECOND FILE.
C     IFLAG=+-3: 1ST PLOT IS BOTH FILES, 2ND PLOT IS COMBINED FILE.
C     POSITIVE IFLAG USES CHARACTER X; NEGATIVE IFLAG USES CHARACTER Y.
C----------DIMENSION STATEMENTS.
      PARAMETER (K=4,L=6,ZNIL=-1.E-30)
      CHARACTER*8 A(K*L+1),B(K*L+1),LETTER
      CHARACTER*1 SECOND
      CHARACTER*5 SHOW5,cshow5,cshow52,cshow53
      CHARACTER*10 SHOW10,cshow10
C----------PRINT PLOT HEADER.
      CALL PLOTLINE (K,L,DUMMY,SLO,SUP,'(F8.0)',A)
      DO 7 I=2,K*L
      IF (A(I)(1:1).EQ.'.') A(I)=' '
7     CONTINUE
      A(K*L+1)=' '
      A(1)(8:8)=' '
      IF (IABS(IFLAG).EQ.1) PRINT 3001, A
      IF (IABS(IFLAG).EQ.2) PRINT 3002, A
      IF (IABS(IFLAG).EQ.3) PRINT 3003, A
      RETURN
C----------PRINT ONE LINE OF PLOT.
      ENTRY STATLINE (II,IA,IB,X,Y,S1,S2,S3,SLO,SUP,IFLAG)
      SECOND=' '
      IF (MOD(II-IA,4).EQ.0.OR.II.EQ.IB) SECOND='.'
C----------PREPARE FIRST PLOT.
      IF (IABS(IFLAG).EQ.3) THEN
           LETTER='1'
           IF (S1.EQ.S2) LETTER='B'
           IF (S1.EQ.ZNIL) LETTER='N'
           LETTER(2:2)=SECOND
           CALL PLOTLINE (K,L,S1,SLO,SUP,LETTER,A)
           LETTER='2'
           IF (S1.EQ.S2) LETTER='B'
           IF (S2.EQ.ZNIL) LETTER='N'
           LETTER(2:2)='B'
           IF (S1.EQ.ZNIL.OR.S2.EQ.ZNIL) LETTER(2:2)='N'
           CALL PLOTLINE (K,L,S2,SLO,SUP,LETTER,A)
      ELSE
           LETTER='X'
           IF (IFLAG.LT.0) LETTER='Y'
           IF (S1.NE.S2) LETTER='E'
           IF (S1.NE.S2.AND.IABS(IFLAG).EQ.2) LETTER='1'
           IF (S1.EQ.ZNIL) LETTER='N'
           LETTER(2:2)=SECOND
           CALL PLOTLINE (K,L,S1,SLO,SUP,LETTER,A)
      END IF
C----------PREPARE SECOND PLOT.
      LETTER='X'
      IF (IFLAG.LT.0) LETTER='Y'
      IF (IABS(IFLAG).EQ.3) THEN
           SSS=S3
           IF (S3.EQ.S1) LETTER='1'
           IF (S3.EQ.S2) LETTER='2'
           IF (S3.EQ.S1.AND.S3.EQ.S2) LETTER='B'
      ELSE
           IF (S1.NE.S2) LETTER='O'
           IF (S1.NE.S2.AND.IABS(IFLAG).EQ.2) LETTER='2'
           SSS=S2
      END IF
      IF (SSS.EQ.ZNIL) LETTER='N'
      LETTER(2:2)=SECOND
      CALL PLOTLINE (K,L,SSS,SLO,SUP,LETTER,B)
C----------PRINT THE LINE OF PLOT.
      IF (IABS(IFLAG).LE.2) THEN
           cshow10=show10(s1)
           cshow5=show5(s2)
           PRINT 4000, NINT(X),NINT(Y),cSHOW10   ,A,cSHOW5   ,B
c          PRINT 4000, NINT(X),NINT(Y),SHOW10(S1),A,SHOW5(S2),B
      ELSE
           cshow5=show5(s1)
           cshow52=show5(s2)
           cshow53=show5(s3)
           PRINT 4001, NINT(X),NINT(Y),cSHOW5   ,cSHOW52  ,A,
     $        cSHOW53  ,B
c          PRINT 4001, NINT(X),NINT(Y),SHOW5(S1),SHOW5(S2),A,
c    $        SHOW5(S3),B
      END IF
      RETURN
C----------FORMAT STATEMENTS.
3001  FORMAT ('    XGP  YGP',6X,A8,24A1,5X,'        OLD VALUES       ')
3002  FORMAT ('    XGP  YGP',6X,A8,24A1,5X,'   SECOND STATIC FILE    ')
3003  FORMAT ('    XGP  YGP',6X,A8,24A1,5X,'  COMBINED STATIC FILE   ')
cc4000  FORMAT (1X,I6,I5,1X,A10,1X,25A1,A5,1X,25A1)
cc4001  FORMAT (1X,I6,I5,1X,A5,A5,1X,25A1,A5,1X,25A1)
4000  FORMAT (1X,I7,I4,1X,A10,1X,25A1,A5,1X,25A1)
4001  FORMAT (1X,I7,I4,1X,A5,A5,1X,25A1,A5,1X,25A1)
      END                                
                   



      SUBROUTINE STATIC_OPTION_R
     $                  (IOPTION,S,NX,NY,xinc,yinc,
     $                   tp,xrun,yrun,iendflag,A)
C     REMOVE RUNNING AVERAGE (OR SMOOTH THE FILE). 
C     ioption = 1 removes running average.
C     ioption = 2 smooths the file.
c     s(nx,ny) = static values.
c     xinc and yinc = increment (in ground position units) in static file.
c     tp = trim percentage (0.0 = mean, 100.0 = median).
c     xrun and yrun = length of running average (in ground position units).
c     iendflag = end option (1 or 2 or 3 or 4).
c     a(*) = scratch array of size maximum(nx and ny).
C----------DIMENSION AND DATA STATEMENTS.
      implicit none
      integer ioption,nx,ny,iendflag                        ! arguments
      real S(NX,NY),xinc,yinc,tp,xrun,yrun,A(*)             ! arguments
      integer iendflag_n,iendflag_t,iendflag_e,iendflag_s   ! local
      parameter (iendflag_n=1)                              ! local
      parameter (iendflag_t=2)                              ! local
      parameter (iendflag_e=3)                              ! local
      parameter (iendflag_s=4)                              ! local
      integer nxpoint,nypoint,ix,iy                         ! local
      real av,runav                                         ! local
      CHARACTER*8 IEND                                      ! local
      DATA IEND/'NN'/                                       ! local
C----------ASK QUESTIONS.
      if (iendflag.eq.iendflag_t) iend='TT'
      if (iendflag.eq.iendflag_s) iend='SS'
      if (iendflag.eq.iendflag_e) iend='EE'
      if (iendflag.eq.iendflag_n) iend='NN'
      NXPOINT=NINT(XRUN/XINC)
      NYPOINT=NINT(YRUN/YINC)
C----------DO THE WORK IN THE X DIRECTION.
      DO 80 IY=1,NY
      CALL MOVE (S(1,IY),A,NX)
      DO 40 IX=1,NX 
      AV=RUNAV(IX,A,NX,NXPOINT,IEND,TP)
      IF (IOPTION.EQ.1) S(IX,IY)=A(IX)-AV 
      IF (IOPTION.EQ.2) S(IX,IY)=AV
40    CONTINUE
80    CONTINUE
C----------ASK QUESTIONS.
      IF (NY.EQ.1) GO TO 95
C----------DO THE WORK IN THE Y DIRECTION.
      DO 90 IX=1,NX
      DO 48 IY=1,NY
48    A(IY)=S(IX,IY)
      DO 50 IY=1,NY 
      AV=RUNAV(IY,A,NY,NYPOINT,IEND,TP)
      IF (IOPTION.EQ.1) S(IX,IY)=A(IY)-AV 
      IF (IOPTION.EQ.2) S(IX,IY)=AV
50    CONTINUE
90    CONTINUE
95    continue
      RETURN
      END






c      SUBROUTINE STATIC_OPTION_I (S,NX,NY,A)
cC     INTEGRATE THE FILE, PRESERVING THE NILS.
cC----------DIMENSION AND DATA STATEMENTS.
c      implicit none
c      integer nx,ny                           ! arguments
c      real S(NX,NY),A(*)                      ! arguments
c      real znil,skeep,sss                     ! local
c      PARAMETER (ZNIL=-1.E-30)                ! local
c      integer ix,iy                           ! local
cC----------DO THE WORK.
c      IF (NX.EQ.1) RETURN
c      DO 80 IY=1,NY
c      CALL MOVE (S(1,IY),A,NX)
c      SKEEP=S(1,IY)
c      S(1,IY)=0. 
c      DO 40 IX=2,NX 
c      SSS=S(IX,IY) 
c      S(IX,IY)=S(IX-1,IY)+SKEEP 
c40    SKEEP=SSS
cc     DO 50 IX=1,NX
c      DO 50 IX=1,NX-1  ! last nil on each x-line should not be preserved.
c      IF (A(IX).EQ.ZNIL) S(IX,IY)=ZNIL
c50    CONTINUE
c80    CONTINUE
c      RETURN
c      END





      SUBROUTINE STATIC_OPTION_I (S,NX,NY)
C     INTEGRATE THE FILE, PRESERVING THE NILS.
C----------DIMENSION AND DATA STATEMENTS.
      implicit none
      integer nx,ny                           ! arguments
      real S(NX,NY)                           ! arguments
      real znil,sum,temp                      ! local
      PARAMETER (ZNIL=-1.E-30)                ! local
      integer ix,iy                           ! local
C----------DO THE WORK.
      IF (NX.EQ.1) RETURN
      DO IY=1,NY
           SUM=0.0
           DO IX=1,NX 
                temp=s(ix,iy)
                if (temp.ne.znil.or.ix.eq.nx) then
                     s(ix,iy)=sum
                     sum=sum+temp
                end if
           end do
      end do
      RETURN
      END






      SUBROUTINE STATIC_OPTION_N (S,NX,NY)
C     REPLACE NILS WITH INTERPOLATED VALUES.
C     EACH INLINE (CONSTANT Y) IS INTERPOLATED FIRST.
C     THEN INTERPOLATION IS PERFORMED IN THE CROSSLINE DIRECTION.
C     IF ALL NILS, NILS ARE REPLACED BY ZEROES.
C----------DIMENSION AND DATA STATEMENTS.
      implicit none
      integer nx,ny                          ! arguments
      real S(NX,NY)                          ! arguments
      real znil                              ! local
      integer kount,ix,iy,ia,ib,j            ! local
      PARAMETER (ZNIL=-1.E-30)               ! local
C----------COUNT THE NILS.
      KOUNT=0
      DO 5 IY=1,NY
      DO 5 IX=1,NX
      IF (S(IX,IY).EQ.ZNIL) KOUNT=KOUNT+1
5     CONTINUE
      IF (KOUNT.EQ.0) RETURN
C----------FIRST INTERPOLATE EACH LINE SEPARATELY.
      DO 80 IY=1,NY
C----------CHECK FOR ALL NILS (AND RESET TO ZERO IF ONLY ONE LINE).
      DO 7 IX=1,NX 
      IF (S(IX,IY).NE.ZNIL) GO TO 11
7     CONTINUE
      IF (NY.GT.1) GO TO 80
      DO 9 IX=1,NX
9     S(IX,IY)=0.
      GO TO 80
C----------WE HAVE AT LEAST ONE NON-NIL VALUE.
11    S(1,IY)=S(IX,IY) 
      DO 12 IX=NX,1,-1
      IF (S(IX,IY).NE.ZNIL) GO TO 13
12    CONTINUE
CCC   WE SHOULD NOT GET TO HERE.
13    S(NX,IY)=S(IX,IY) 
      DO 20 IX=2,NX 
      IF (S(IX-1,IY).NE.ZNIL.AND.S(IX,IY).EQ.ZNIL) THEN 
           IA=IX-1
      ELSE IF (S(IX-1,IY).EQ.ZNIL.AND.S(IX,IY).NE.ZNIL) THEN
           IB=IX
           DO 15 J=IA+1,IB-1 
15         S(J,IY)=S(IA,IY)+(J-IA)*(S(IB,IY)-S(IA,IY))/(IB-IA) 
      END IF
20    CONTINUE
80    CONTINUE
C----------NOW INTERPOLATE IN THE CROSSLINE DIRECTION.
      IF (NY.EQ.1) GO TO 99
      DO 1180 IX=1,NX
C----------CHECK FOR ALL NILS (AND RESET TO ZERO IF ONLY ONE LINE).
      DO 117 IY=1,NY 
      IF (S(IX,IY).NE.ZNIL) GO TO 111
117   CONTINUE
      DO 119 IY=1,NY
119   S(IX,IY)=0.
      GO TO 1180
C----------WE HAVE AT LEAST ONE NON-NIL VALUE.
111   S(IX,1)=S(IX,IY) 
      DO 112 IY=NY,1,-1
      IF (S(IX,IY).NE.ZNIL) GO TO 113
112   CONTINUE
CCC   WE SHOULD NOT GET TO HERE.
113   S(IX,NY)=S(IX,IY) 
      DO 120 IY=2,NY 
      IF (S(IX,IY-1).NE.ZNIL.AND.S(IX,IY).EQ.ZNIL) THEN 
           IA=IY-1
      ELSE IF (S(IX,IY-1).EQ.ZNIL.AND.S(IX,IY).NE.ZNIL) THEN
           IB=IY
           DO 115 J=IA+1,IB-1 
115        S(IX,J)=S(IX,IA)+(J-IA)*(S(IX,IB)-S(IX,IA))/(IB-IA) 
      END IF
120   CONTINUE
1180  CONTINUE
C----------GENERATE COMMENT CARD.
99    continue
      RETURN
      END





      SUBROUTINE STATIC_OPTION_NX (S,NX,NY)
C     REPLACE NILS WITH INTERPOLATED VALUES IN X DIRECTION.
C     EACH INLINE (CONSTANT Y) IS INTERPOLATED BUT NOT EXTRAPOLATED.
C     ANY INLINE CONTAINING ALL NILS IS NOT CHANGED.
C----------DIMENSION AND DATA STATEMENTS.
      implicit none
      integer nx,ny                                 ! arguments
      real S(NX,NY)                                 ! arguments
      real znil                                     ! local
      PARAMETER (ZNIL=-1.E-30)                      ! local
      integer kount,ix,iy,kx1,kx2,ia,ib,j           ! local
C----------INTERPOLATE EACH INLINE SEPARATELY.
      KOUNT=0
      DO 80 IY=1,NY
C----------FIND FIRST AND LAST NON-NIL VALUE.
      kx1=0
      kx2=0
      DO 7 IX=1,NX 
      IF (S(IX,IY).NE.ZNIL.and.kx1.eq.0) kx1=ix
      IF (S(IX,IY).NE.ZNIL             ) kx2=ix
7     CONTINUE
      IF (kx1.eq.0.or.kx2.eq.0.or.kx1.eq.kx2.or.kx2.eq.kx1+1) GO TO 80
C----------WE HAVE SOMETHING TO INTERPOLATE.
      DO 20 IX=kx1+1,kx2
      IF (S(IX-1,IY).NE.ZNIL.AND.S(IX,IY).EQ.ZNIL) THEN 
           IA=IX-1
      ELSE IF (S(IX-1,IY).EQ.ZNIL.AND.S(IX,IY).NE.ZNIL) THEN
           IB=IX
           DO 15 J=IA+1,IB-1 
           kount=kount+1
15         S(J,IY)=S(IA,IY)+(J-IA)*(S(IB,IY)-S(IA,IY))/(IB-IA) 
      END IF
20    CONTINUE
80    CONTINUE
C----------GENERATE COMMENT CARD.
      RETURN
      END





      SUBROUTINE STATIC_OPTION_NY (S,NX,NY)
C     REPLACE NILS WITH INTERPOLATED VALUES IN Y DIRECTION.
C     EACH CROSSLINE (CONSTANT X) IS INTERPOLATED BUT NOT EXTRAPOLATED.
C     ANY CROSSLINE CONTAINING ALL NILS IS NOT CHANGED.
C----------DIMENSION AND DATA STATEMENTS.
      implicit none
      integer nx,ny                                 ! arguments
      real S(NX,NY)                                 ! arguments
      real znil                                     ! local
      PARAMETER (ZNIL=-1.E-30)                      ! local
      integer kount,ix,iy,ky1,ky2,ia,ib,j           ! local
C----------INTERPOLATE EACH CROSSLINE SEPARATELY.
      KOUNT=0
      DO 80 IX=1,NX
C----------FIND FIRST AND LAST NON-NIL VALUE.
      ky1=0
      ky2=0
      DO 7 IY=1,NY 
      IF (S(IX,IY).NE.ZNIL.and.ky1.eq.0) ky1=iy
      IF (S(IX,IY).NE.ZNIL             ) ky2=iy
7     CONTINUE
      IF (ky1.eq.0.or.ky2.eq.0.or.ky1.eq.ky2.or.ky2.eq.ky1+1) GO TO 80
C----------WE HAVE SOMETHING TO INTERPOLATE.
      DO 20 Iy=ky1+1,ky2
      IF (S(IX,IY-1).NE.ZNIL.AND.S(IX,IY).EQ.ZNIL) THEN 
           IA=Iy-1
      ELSE IF (S(IX,IY-1).EQ.ZNIL.AND.S(IX,IY).NE.ZNIL) THEN
           IB=Iy
           DO 15 J=IA+1,IB-1 
           kount=kount+1
15         S(IX,j)=S(Ix,Ia)+(J-IA)*(S(Ix,Ib)-S(Ix,Ia))/(IB-IA) 
      END IF
20    CONTINUE
80    CONTINUE
C----------GENERATE COMMENT CARD.
      RETURN
      END





c put this stuff into help for static_option_nn:
c
c      SUBROUTINE OPTIONNN (S,S3,NX,NY,CARD,NCARD)
cC     REPLACE NILS ONLY FROM NEARBY INTERPOLATED VALUES.
cC     S3 is scratch space.
cC----------DIMENSION AND DATA STATEMENTS.
c      PARAMETER (ZNIL=-1.E-30)
c      DIMENSION S(NX,NY),S3(NX,NY)
c      CHARACTER*80 CARD(*)
c      CHARACTER*4 Q
c      logical iyminus,iyplus,iyok
c      logical ixminus,ixplus,ixok
cC----------ASK QUESTIONS.
c      print *, 'Nil values will be replaced by interpolated values'
c      print *, 'only if non-nil values exist within a specified'
c      print *, 'search distance in each direction from the nil value.'
c      print *, 'A search distance of 0 means not to search in that',
c     $                                               ' direction.'
c      print *, 'A search distance of 1 means to search only one point',
c     $                                               ' away,'
c      print *, '   in both the negative and positive directions.'
c      print *, 'A search distance of 2 means to search 2 points away',
c     $                                                 ' (etc).'
c      print *, 'Search distances should be kept small, since all'
c      print *, 'non-nil values within the search range will be used.'
c      PRINT *, 'TYPE SEARCH DISTANCE IN X DIRECTION  (DEFAULT 1)'
c      IXDIST=1
c      CALL HITI (IXDIST)
c      PRINT *, 'TYPE SEARCH DISTANCE IN Y DIRECTION  (DEFAULT 1)'
c      IYDIST=1
c      CALL HITI (IYDIST)
c      PRINT *, 'DO YOU WISH TO REQUIRE NON-NIL VALUES ON EACH SIDE?'
c      print *, 'If you choose N, the nil value will be replaced'
c      print *, 'regardless of where the non-nil values are within the',
c     $                                               ' search range.'
c      print *, 'If you choose Y, the nil value will be replaced'
c      print *, 'only if there are non-nil values in both the positive'
c      print *, 'and negative X directions, or if there are non-nil'
c      print *, 'values in both the positive and negative Y directions.'
c      PRINT *, 'DO YOU WISH TO REQUIRE NON-NIL VALUES ON EACH SIDE?',
c     $                                            ' (DEFAULT Y)'
c      Q='Y'
c      CALL HITQ (Q)






      SUBROUTINE STATIC_OPTION_NN (S,NX,NY,ixdist,iydist,require,s3)
C     REPLACE NILS ONLY FROM NEARBY INTERPOLATED VALUES.
C     S3 is scratch space.
C----------DIMENSION AND DATA STATEMENTS.
      implicit none
      integer nx,ny,ixdist,iydist,require            ! arguments
      real S(NX,NY),S3(NX,NY)                        ! arguments
      real znil,sum,weight,w,sqrt,xdist,ydist        ! local
      PARAMETER (ZNIL=-1.E-30)                       ! local
      CHARACTER*4 Q                                  ! local
      logical iyminus,iyplus,iyok                    ! local
      logical ixminus,ixplus,ixok                    ! local
      integer ix,iy,ix2,iy2,k,kount                  ! local
      integer ixmin,ixmax,iymin,iymax                ! local
C----------ASK QUESTIONS.
      if (require.eq.0) q='N'
      if (require.ne.0) q='Y'
C----------DO THE WORK.
      CALL MOVE (S,S3,NX*NY)
      KOUNT=0
      DO IY=1,NY 
      DO IX=1,NX
      if (s(ix,iy).ne.znil) go to 200
      iymin=max(iy-iydist,1)
      iymax=min(iy+iydist,ny)
      ixmin=max(ix-ixdist,1)
      ixmax=min(ix+ixdist,nx)
      k=0
      DO IY2=iymin,iymax
      DO IX2=ixmin,ixmax
      if (s(ix2,iy2).ne.znil) k=k+1
      end do
      end do
      if (k.eq.0) go to 200
      if (q.eq.'Y') then
           iyminus=.false.
           iyplus=.false.
           ixminus=.false.
           ixplus=.false.
           DO IY2=iymin,iy
           if (s(ix,iy2).ne.znil) iyminus=.true.
           end do
           DO IY2=iy,iymax
           if (s(ix,iy2).ne.znil) iyplus=.true.
           end do
           DO Ix2=ixmin,ix
           if (s(ix2,iy).ne.znil) ixminus=.true.
           end do
           DO Ix2=ix,ixmax
           if (s(ix2,iy).ne.znil) ixplus=.true.
           end do
           iyok=(iyminus.and.iyplus)
           ixok=(ixminus.and.ixplus)
           if(.not.iyok.and..not.ixok) go to 200
      end if
      sum=0.0
      weight=0.0
      DO IY2=iymin,iymax
      DO IX2=ixmin,ixmax
      if (s(ix2,iy2).ne.znil) then
           xdist=ix2-ix
           ydist=iy2-iy
           w=1.0/sqrt(xdist*xdist+ydist*ydist)
           weight=weight+w
           sum=sum+w*s(ix2,iy2)
      end if
      end do
      end do
      if (weight.gt.0.0) then
           s3(ix,iy)=sum/weight
           kount=kount+1
      end if
200   continue
      END DO
      END DO
      CALL MOVE (S3,S,NX*NY)
C----------GENERATE COMMENT CARD.
      RETURN
      END





      SUBROUTINE OPTIONZ (S,NX,NY,CARD,NCARD)
C     REPLACE NILS WITH ZEROES.
C----------DIMENSION AND DATA STATEMENTS.
      PARAMETER (ZNIL=-1.E-30)
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------DO THE WORK.
      KOUNT=0
      DO 5 IY=1,NY
      DO 5 IX=1,NX
      IF (S(IX,IY).EQ.ZNIL) THEN
           S(IX,IY)=0.
           KOUNT=KOUNT+1
      END IF
5     CONTINUE
C----------GENERATE COMMENT CARD.
      PRINT *, KOUNT,' NILS HAVE BEEN REPLACED WITH ZEROES'
      IF (KOUNT.EQ.0) RETURN
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) KOUNT
1000  FORMAT ('ISEP- REPLACE',I7,' NILS WITH ZEROES')
      RETURN
      END




      SUBROUTINE OPTIONV1 (S,NX,NY,CARD,NCARD)
C     REPLACE NILS WITH ANY SPECIFIED VALUE.
C----------DIMENSION AND DATA STATEMENTS.
      PARAMETER (ZNIL=-1.E-30)
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------ASK QUESTIONS.
      PRINT *, 'TYPE VALUE TO REPLACE NILS WITH  (DEFAULT 0)'
      VALUE=0.
      CALL HITX (VALUE)
C----------DO THE WORK.
      KOUNT=0
      DO 5 IY=1,NY
      DO 5 IX=1,NX
      IF (S(IX,IY).EQ.ZNIL) THEN
           S(IX,IY)=VALUE
           KOUNT=KOUNT+1
      END IF
5     CONTINUE
C----------GENERATE COMMENT CARD.
      PRINT *, KOUNT,' NILS HAVE BEEN REPLACED WITH ', VALUE
      IF (KOUNT.EQ.0) RETURN
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) KOUNT,VALUE
1000  FORMAT ('ISEP- REPLACE',I7,' NILS WITH VALUE',G13.5)
      RETURN
      END




      SUBROUTINE OPTIONV2 (S,NX,NY,CARD,NCARD)
C     REPLACE SPECIFIED RANGE OF VALUES WITH NILS.
C----------DIMENSION AND DATA STATEMENTS.
      PARAMETER (ZNIL=-1.E-30)
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------ASK QUESTIONS.
      PRINT *, 'TYPE MINIMUM VALUE TO REPLACE WITH NILS (DEFAULT 0)'
      VMIN=0.
      CALL HITX (VMIN)
      PRINT *, 'TYPE MAXIMUM VALUE TO REPLACE WITH NILS (DEFAULT 0)'
      VMAX=0.
      CALL HITX (VMAX)
C----------DO THE WORK.
      KOUNT=0
      DO 5 IY=1,NY
      DO 5 IX=1,NX
      IF (S(IX,IY).GE.VMIN.and.S(IX,IY).LE.VMAX) THEN
           S(IX,IY)=ZNIL
           KOUNT=KOUNT+1
      END IF
5     CONTINUE
C----------GENERATE COMMENT CARD.
      PRINT *, KOUNT,' VALUES HAVE BEEN REPLACED WITH NIL'
      IF (KOUNT.EQ.0) RETURN
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) KOUNT
1000  FORMAT ('ISEP- REPLACE',I7,' VALUES WITH NIL')
      RETURN
      END







      SUBROUTINE OPTIONM (S,NX,NY,CARD,NCARD)
C     MULTIPLY THE STATIC VALUES BY A CONSTANT.
C----------DIMENSION AND DATA STATEMENTS.
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------ASK QUESTIONS.
      PRINT *, 'TYPE CONSTANT TO MULTIPLY STATIC VALUES BY  (DEFAULT 1)'
      CONST=1.
      CALL HITX (CONST)
      IF (CONST.EQ.1.) RETURN
C----------DO THE WORK.
      DO 80 IY=1,NY
      DO 80 IX=1,NX
80    S(IX,IY)=S(IX,IY)*CONST
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) CONST
1000  FORMAT ('ISEP- MULTIPLY STATIC VALUES BY CONSTANT',G13.5)
      PRINT *, 'STATIC VALUES HAVE BEEN MULTIPLIED BY ',CONST
      RETURN
      END






      SUBROUTINE OPTIONA (S,NX,NY,CARD,NCARD)
C     ADD A CONSTANT TO THE STATIC VALUES.
C----------DIMENSION AND DATA STATEMENTS.
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------ASK QUESTIONS.
      PRINT *, 'TYPE CONSTANT TO ADD TO STATIC VALUES  (DEFAULT 0)'
      CONST=0.
      CALL HITX (CONST)
      IF (CONST.EQ.0.) RETURN
C----------DO THE WORK.
      DO 80 IY=1,NY
      DO 80 IX=1,NX
80    S(IX,IY)=S(IX,IY)+CONST
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) CONST
1000  FORMAT ('ISEP- ADD CONSTANT',G13.5,' TO STATIC VALUES')
      PRINT *, 'CONSTANT ',CONST,' HAS BEEN ADDED TO STATIC VALUES'
      RETURN
      END






      SUBROUTINE STATIC_OPTION_C (x1old,x2old,x1new,x2new,
     $                            y1old,y2old,y1new,y2new,
     $                            X1,Y1,XINC,YINC,NX,NY,S,CARD,NCARD)
C     CHANGE THE GROUND POSITIONS.
c     x1,xinc,y1,yinc are changed by this subroutine.
C----------DIMENSION AND DATA STATEMENTS.
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------GET STARTED.
      XEND=X1+(NX-1)*XINC
      YEND=Y1+(NY-1)*YINC
ccc   WRITE (LFN,4001) NHX,NHY,NHX2,NHY2,X1,Y1,XEND,YEND,XINC,YINC,NX,NY
      print      4001, NHX,NHY,NHX2,NHY2,X1,Y1,XEND,YEND,XINC,YINC,NX,NY
4001  FORMAT ('           X AND Y HEADER WORDS =              ',I8,I14,
     $                              3X,2I4/
     $        '           STARTING X AND Y GROUND POSITIONS = ',2G14.6/
     $        '             ENDING X AND Y GROUND POSITIONS = ',2G14.6/
     $        '           X AND Y GROUND POSITION INCREMENTS =',2G14.6/
     $        '           NUMBER OF X AND Y GROUND POSITIONS =',I8,I14/)
C----------CHANGE THE X GROUND POSITIONS.
2     CALL OPTIONC1 ('present X',   X1OLD,X1NEW)
      CALL OPTIONC1 ('other  X ',   X2OLD,X2NEW)
      IF (X2OLD.EQ.X1OLD) THEN
           PRINT *, 'THE TWO OLD GROUND POSITIONS MUST BE DIFFERENT --',
     $        ' TRY AGAIN'
           GO TO 2
      END IF
      SLOPE=(X2NEW-X1NEW)/(X2OLD-X1OLD)
      X1=X1NEW+(X1-X1OLD)*SLOPE
      XINC=XINC*SLOPE
      CALL STATIC_REVERSE (X1,Y1,XINC,YINC,NX,NY,S)
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) 'X',X1OLD,X2OLD,X1NEW,X2NEW
1000  FORMAT ('ISEP- ',A1,' GPs ',2F12.2,'  CHANGED TO ',2F12.2)
C----------CHANGE THE Y GROUND POSITIONS.
      IF (NY.GT.1) THEN
4     CALL OPTIONC1 ('present Y',   Y1OLD,Y1NEW)
      CALL OPTIONC1 ('other  Y ',   Y2OLD,Y2NEW)
      IF (Y2OLD.EQ.Y1OLD) THEN
           PRINT *, 'THE TWO OLD GROUND POSITIONS MUST BE DIFFERENT --',
     $        ' TRY AGAIN'
           GO TO 4
      END IF
      SLOPE=(Y2NEW-Y1NEW)/(Y2OLD-Y1OLD)
      Y1=Y1NEW+(Y1-Y1OLD)*SLOPE
      YINC=YINC*SLOPE
      CALL STATIC_REVERSE (X1,Y1,XINC,YINC,NX,NY,S)
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) 'Y',Y1OLD,Y2OLD,Y1NEW,Y2NEW
      END IF
C----------GET HEADER WORDS.
      NZX=NHX
      NZY=NHY
      NZX2=NHX2
      NZY2=NHY2
30    PRINT *, 'TYPE FOUR HEADER WORD NUMBERS IF YOU WISH TO CHANGE',
     $   ' THEM.'
      PRINT *, 'OTHERWISE HIT RETURN.'
      PRINT *, 'The most common are:'
      PRINT *, '   Group file:   9  0  0  0'
      PRINT *, '  Source file:  33 34  0  0   or   33  0  0  0  ',
     $   ' or   46 0  0  0'
      PRINT *, 'Receiver file:  35 36  0  0   or   35  0  0  0  ',
     $   ' or   47 0  0  0'
      PRINT *, '     S=R file:  35 36 33 34   or   35  0 33  0  ',
     $   ' or   47 0 46  0'
      CALL HITIIII2 (NHX,NHY,NHX2,NHY2)
      IF (NHX.EQ.0) THEN
           PRINT *, 'FIRST HEADER WORD CANNOT BE ZERO -- TRY AGAIN'
           GO TO 30
      END IF
      IF (NHY.EQ.0.AND.NY.GT.1) THEN
           PRINT *, 'SECOND HEADER WORD CANNOT BE ZERO -- TRY AGAIN'
           GO TO 30
      END IF
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),2000) NZX,NZY,NZX2,NZY2,NHX,NHY,NHX2,NHY2
2000  FORMAT ('ISEP- HEADERS CHANGED FROM ',4I4,'   TO ',4I4)
      RETURN
      END





      SUBROUTINE STATIC_REVERSE (X1,Y1,XINC,YINC,NX,NY,S)
C     REVERSE THE STATIC FILE IF XINC OR YINC IS NEGATIVE.
      implicit none
      real x1,y1,xinc,yinc                  ! arguments
      integer nx,ny                         ! arguments
      real S(NX,NY)                         ! arguments
      integer nx2,ix2,ix,iy,ny2,iy2         ! local
      real sss                              ! local
C----------REVERSE THE FILE (IN X DIRECTION) IF NECESSARY.
      IF (NX.GT.1.AND.XINC.LT.0.0) THEN
           NX2=(NX+1)/2
           DO 10 IY=1,NY
           DO 10 IX=1,NX2
           IX2=NX-IX+1
           SSS=S(IX,IY)
           S(IX,IY)=S(IX2,IY)
10         S(IX2,IY)=SSS
           X1=X1+(NX-1)*XINC
           XINC=-XINC
c          PRINT *, 'STATIC FILE REVERSED BECAUSE THE',
c    $        ' X GROUND POSITIONS ARE REVERSED'
      END IF
C----------REVERSE THE FILE (IN Y DIRECTION) IF NECESSARY.
      IF (NY.GT.1.AND.YINC.LT.0.0) THEN
           NY2=(NY+1)/2
           DO 11 IX=1,NX
           DO 11 IY=1,NY2
           IY2=NY-IY+1
           SSS=S(IX,IY)
           S(IX,IY)=S(IX,IY2)
11         S(IX,IY2)=SSS
           Y1=Y1+(NY-1)*YINC
           YINC=-YINC
c          PRINT *, 'STATIC FILE REVERSED BECAUSE THE',
c    $        ' Y GROUND POSITIONS ARE REVERSED'
      END IF
      if (xinc.lt.0.0) xinc=-xinc
      if (yinc.lt.0.0) yinc=-yinc
      RETURN
      END





      SUBROUTINE OPTIONC1 (MSG,   XOLD,XNEW)
C     CALLED FROM OPTIONC.
      PARAMETER (ZIP=-123.456E-27)
      CHARACTER*9 MSG
3     PRINT 4002, MSG
4002  FORMAT (' Type any ',A9,' ground position, and what you want to',
     $   ' change it to:')
      XOLD=ZIP
      XNEW=ZIP
      CALL HITXX (XOLD,XNEW)
      IF (XOLD.EQ.ZIP.OR.XNEW.EQ.ZIP) THEN
           PRINT *, 'YOU MUST SUPPLY TWO NUMBERS -- TRY AGAIN'
           GO TO 3
      END IF
      RETURN
      END






      FUNCTION RUNAV (I,Z,N,NPOINT,IEND,TP) 
C     RETURNS RUNNING AVERAGE OF POINT Z(I) OUT OF ARRAY Z(N).
C     NPOINT = NUMBER OF POINTS TO INCLUDE IN RUNNING AVERAGE.
C     IEND = TT FOR TRUNCATED END RANGE 
C            SS FOR SHIFTED END RANGE 
C            EE FOR EXTENDED END RANGE
C            NN FOR NARROWED END RANGE (GRADED ENDS)
      PARAMETER (NXY=901,ZNIL=-1.E-30)
      DIMENSION Z(N),X(NXY),Y(NXY)          
      CHARACTER*8 IEND
C----------GET PRELIMINARY VALUE OF RUNNING AVERAGE.
      RUNAV=Z(I)
C----------GET NUMBER OF POINTS IN ONE SIDE OF RUNNING AVERAGE. 
      NPOINT2=MIN0(NPOINT,NXY)
      MPOINT=NPOINT2/2
      IF (MPOINT.LE.0) RETURN 
      IF (IEND.EQ.'NN') MPOINT=MIN0(MPOINT,N-I,I-1) 
C----------GET LOWER AND UPPER POINT LIMITS FOR THE AVERAGE.
      JA=MAX0(I-MPOINT,1) 
      JB=MIN0(I+MPOINT,N) 
      IF (IEND.EQ.'SS') JA=MAX0(1,MIN0(I-MPOINT,N-2*MPOINT))
      IF (IEND.EQ.'SS') JB=MIN0(N,MAX0(I+MPOINT,1+2*MPOINT))
      IF (IEND.EQ.'EE') JA=I-MPOINT 
      IF (IEND.EQ.'EE') JB=I+MPOINT 
C----------LOAD UP THE ARRAY TO AVERAGE.
      KSUM=0
      DO 72 J=JA,JB 
      JJ=J
      IF (IEND.EQ.'EE') JJ=MIN0(N,MAX0(1,J))
      IF (Z(JJ).EQ.ZNIL) GO TO 72 
      KSUM=KSUM+1 
      X(KSUM)=Z(JJ) 
72    CONTINUE
      IF (KSUM.EQ.0) RETURN 
C----------USE ONLY HALF OF EACH END POINT FOR EVEN NPOINT. 
      IF (NPOINT2.EQ.2*MPOINT.AND.KSUM.GE.2) THEN 
           X(1)=(X(1)+X(KSUM))/2. 
           KSUM=KSUM-1
      END IF
C----------GET THE AVERAGE OR TRIMMED MEAN VALUE. 
      RUNAV=QTRIM(TP,KSUM,X,Y)
      RETURN
      END 








      FUNCTION QTRIM (TP,N,X,Y) 
C     RETURNS TRIMMED MEAN OF ARRAY X(N). 
C     TP = TRIM PERCENT (0=MEAN, 100=MEDIAN). 
C     ARRAY Y(N) IS NEEDED FOR SCRATCH SPACE. 
C     UPON RETURN, ARRAY X(N) IS REARRANGED INTO ASCENDING ORDER. 
C     UPON RETURN, ARRAY Y(N) = ARRAY X(N). 
      DIMENSION X(N),Y(N) 
      NTP=(N-1)*TP/200.+0.5 
      NTP=MIN0((N-1)/2,MAX0(0,NTP)) 
      IA=1+NTP
      IB=N-NTP
      IF (NTP.GT.0) CALL QSORT22 (N,X,Y)
      SUM=0.
      DO 20 I=IA,IB 
20    SUM=SUM+X(I)
      QTRIM=SUM/(IB-IA+1) 
      RETURN
      END 








      SUBROUTINE OPTIONE (X1,Y1,XINC,YINC,NX,NY,S,NCARD,CARD)
C     EDIT THE FILE.
C----------DIMENSION STATEMENTS.
      PARAMETER (ZNIL=-1.E-30,NEDIT=7)
      DIMENSION S(NX,NY),KEDIT(NEDIT)
      CHARACTER*80 CARD(*)
      CHARACTER*4 BUF,Q
C----------GET STARTED.
      IEDIT=0
      JEDIT=0
      XEND=X1+(NX-1)*XINC
      YEND=Y1+(NY-1)*YINC
      YGP=Y1
      IY=1
      IF (NY.EQ.1) GO TO 8
C----------GET DESIRED Y GROUND POSITION. 
5     PRINT 1000, 'Y',Y1,YEND,'Y',YGP
1000  FORMAT (' STARTING AND ENDING ',A1,' GROUND POSITIONS ARE ',
     $   F8.1,1X,F8.1/
     $   ' TYPE ',A1,' GROUND POSITION WHERE EDIT WILL OCCUR',
     $   ' (or X to quit)  (DEFAULT ',F8.1,')')
      CALL HITXC3 (YGP,BUF,'X','X','X')
      IF (BUF.EQ.'X') GO TO 80
      IY=NINT((YGP-Y1)/YINC)+1
      IF (IY.LT.1.OR.IY.GT.NY) GO TO 80
      YGP=Y1+(IY-1)*YINC
C----------GET DESIRED X GROUND POSITION.
8     XGP=X1-XINC
9     IDEFAULT=0
10    XGP=XGP+XINC
      IF (XGP.GT.XEND+0.5*XINC) GO TO 8
      PRINT 1000, 'X',X1,XEND,'X',XGP
      CALL HITXC3 (XGP,BUF,'X','X','X')
      IF (BUF.EQ.'X') GO TO 70
            IF (IDEFAULT.EQ.0) THEN
                 PRINT *, 'DO YOU WISH TO EDIT THE GROUND POSITIONS',
     $                   ' CONSECUTIVELY?  (DEFAULT N)'
                 Q='N'
                 CALL HITQ (Q)
                 IDEFAULT=1
                 IF (Q.EQ.'Y') IDEFAULT=999
            END IF
CCCCCC      IF (BUF.EQ.' ') IDEFAULT=IDEFAULT+1
      IX=NINT((XGP-X1)/XINC)
11    IX=IX+1
      IF (IX.LT.1.OR.IX.GT.NX) GO TO 8
      XGP=X1+(IX-1)*XINC
C----------EDIT THE FILE. 
      PRINT 2000, YGP,XGP,S(IX,IY)
2000  FORMAT (' YGP=',F8.1,7X,'XGP=',F8.1,10X,'STATIC=',G13.5)
3000  FORMAT (38X,'NEW STATIC=',G13.5/)
      PRINT *, 'TYPE NEW STATIC VALUE (OR RETURN IF OK)', 
     $   '  (or N for nil)  (or X if finished)'
      CALL HITXC3 (S(IX,IY),BUF,'X','N','N')
      IF (BUF.EQ.'X') GO TO 9
      IF (BUF.EQ.' ') GO TO 68
C----------WE HAVE A NEW STATIC VALUE.
      IF (BUF.EQ.'N') S(IX,IY)=ZNIL
      PRINT 3000, S(IX,IY)
      JEDIT=JEDIT+1
      IEDIT=IEDIT+1
      KEDIT(IEDIT)=NINT(XGP)
C----------GENERATE COMMENT CARD.
      IF (IEDIT.EQ.NEDIT) THEN
           IF (JEDIT.LE.10) THEN   !!! TURNS OFF EXCESSIVE NUMBER OF CARDS.
                NCARD=NCARD+1
                WRITE (CARD(NCARD),4000) NINT(YGP),(KEDIT(I),I=1,IEDIT)
           END IF
           IEDIT=0
      END IF
68    IF (IDEFAULT.GT.4) GO TO 11
      GO TO 10
C----------GENERATE COMMENT CARD.
70    IF (IEDIT.GT.0) THEN
           NCARD=NCARD+1
           WRITE (CARD(NCARD),4000) NINT(YGP),(KEDIT(I),I=1,IEDIT)
4000       FORMAT ('ISEP- EDIT  YGP=',I7,' XGP=',7I7)
           IEDIT=0
      END IF
      IF (NY.GT.1) GO TO 5
C----------FINISH UP AND RETURN.
80    IF (JEDIT.EQ.0) PRINT *, 'NO EDITING DONE'
      IF (JEDIT.GT.0) PRINT *, 'EDITING FINISHED'
      RETURN
      END





      SUBROUTINE OPTIONG (X1,Y1,XINC,YINC,NX,NY,S,NCARD,CARD)
C     GRADE THE FILE.
C----------DIMENSION STATEMENTS. 
      PARAMETER (ZNIL=-1.E-30)
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------GET STARTED.
      XEND=X1+(NX-1)*XINC
      YEND=Y1+(NY-1)*YINC
      IY=1
      IXA=1
      IXB=NX
      YGP=Y1
      XGPA=X1
      XGPB=XEND
C----------GET DESIRED Y GROUND POSITION. 
      PRINT *, 'STARTING AND ENDING Y GROUND POSITIONS ARE ',Y1,YEND
      PRINT *, 'TYPE Y GROUND POSITION WHERE GRADE WILL OCCUR',
     $   '  (DEFAULT ',YGP,')'
      CALL HITX (YGP)
      IY=NINT((YGP-Y1)/YINC)+1
      IF (IY.LT.1.OR.IY.GT.NY) GO TO 90
      YGP=Y1+(IY-1)*YINC
C----------GET STARTING X GROUND POSITION.
      PRINT *, 'STARTING AND ENDING X GROUND POSITIONS ARE ',X1,XEND
      PRINT *, 'TYPE X GROUND POSITION TO START GRADE',
     $   '  (DEFAULT ',XGPA,')'
      CALL HITX (XGPA)
      IXA=NINT((XGPA-X1)/XINC)+1
      IF (IXA.LT.1.OR.IXA.GT.NX) GO TO 90
      XGPA=X1+(IXA-1)*XINC
C----------GET ENDING X GROUND POSITION.
      PRINT *, 'TYPE X GROUND POSITION TO END GRADE',
     $   '  (DEFAULT ',XGPB,')'
      CALL HITX (XGPB)
      IXB=NINT((XGPB-X1)/XINC)+1
      IF (IXB.LE.IXA.OR.IXB.GT.NX) GO TO 90
      XGPB=X1+(IXB-1)*XINC
C----------PRINT GROUND POSITIONS AND STATIC VALUES..
      PRINT *, 'YGP=',YGP,'   XGP=',XGPA,'   STARTING STATIC=',S(IXA,IY)
      PRINT *, 'YGP=',YGP,'   XGP=',XGPB,'     ENDING STATIC=',S(IXB,IY)
C----------GET STARTING STATIC VALUE.
      PRINT *, 'TYPE NEW STARTING STATIC VALUE (OR RETURN IF OK)',
     $   ' (OR 999 FOR NIL) (OR 888 TO CANCEL)'
      SA=S(IXA,IY)
      CALL HITX (SA) 
      IF (SA.EQ.888.) GO TO 90
      IF (SA.EQ.999.) SA=ZNIL 
C----------GET ENDING STATIC VALUE.
      PRINT *, 'TYPE NEW ENDING STATIC VALUE (OR RETURN IF OK)',
     $   ' (OR 999 FOR NIL) (OR 888 TO CANCEL)'
      SB=S(IXB,IY)
      CALL HITX (SB) 
      IF (SB.EQ.888.) GO TO 90
      IF (SB.EQ.999.) SB=ZNIL 
C----------GRADE THE FILE BETWEEN TWO GROUND POSITIONS. 
      DS=(SB-SA)/(IXB-IXA)
      DO 56 IX=IXA,IXB
56    S(IX,IY)=SA+(IX-IXA)*DS
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) NINT(YGP),NINT(XGPA),NINT(XGPB)
1000  FORMAT ('ISEP- GRADE YGP=',I6,' XGP=',I6,' TO ',I6)
      PRINT *, 'GRADING FINISHED'
      RETURN
90    PRINT *, 'GRADING CANCELLED'
      RETURN
      END





            

      SUBROUTINE STATIC_OPTION_GG (S,NX,NY,ixa,ixb,iya,iyb)
C     GRADE THE FILE.
C----------DIMENSION STATEMENTS. 
      implicit none
      integer nx,ny,ixa,ixb,iya,iyb             ! arguments
      real S(NX,NY)                             ! arguments
      real znil,saa,sba,sab,sbb                 ! local
      real slopeya,slopeyb,sa,sb,slopex         ! local
      integer ix,iy                             ! local
      PARAMETER (ZNIL=-1.E-30)                  ! local
C----------GET STARTED.
      SAA=S(IXA,IYA)
      SBA=S(IXB,IYA)
      SAB=S(IXA,IYB)
      SBB=S(IXB,IYB)
C----------GRADE THE FILE BETWEEN FOUR CORNER GROUND POSITIONS.
      SLOPEYA=(SAB-SAA)/max(IYB-IYA,1)
      SLOPEYB=(SBB-SBA)/max(IYB-IYA,1)
      DO IY=IYA,IYB
      sa=saa+(iy-iya)*slopeya
      sb=sba+(iy-iya)*slopeyb
      SLOPEX=(SB-SA)/max(IXB-IXA,1)
      DO IX=IXA,IXB
      S(IX,IY)=SA+(IX-IXA)*SLOPEX
      END DO
      END DO
C----------GENERATE COMMENT CARD.
      RETURN
      END





            



      SUBROUTINE IPLOT (N,IA,IB,   X1,XINC,MSG)
C     RETURNS IA AND IB FOR INTERACTIVE PLOT DO-LOOP RANGE. 
C     CALL JUST BEFORE PRINTING HEADER FOLLOWED BY DO LOOP. 
C     SET IB=0 BEFORE CALLING THIS ROUTINE FIRST TIME.
C     PLOT IS FINISHED WHEN RETURNED IA=0.
C     EXAMPLE OF USE -- 
C             IB=0
C           3 CALL IPLOT (N,IA,IB,   X1,XINC,MSG)
C             IF (IA.EQ.0) GO TO 9
C             PRINT *, 'HEADER FOR PLOT'
C             DO 5 I=IA,IB
C           5 PRINT *, I,X(I),Y(I)
C             GO TO 3 
C           9 CONTINUE
C----------DIMENSION STATEMENTS.
      PARAMETER (NPAGE=19)
      CHARACTER*8 MSG
C----------DO THE WORK. 
      IA=IB+1 
      IF (IA.GT.N) IA=0 
      IF (IB.EQ.0) GO TO 60 
      IF (N.LE.NPAGE) GO TO 80
      XEND=X1+(N-1)*XINC
      XGP=X1+(IA-1)*XINC
      IX1=NINT(X1)
      IXEND=NINT(XEND)
      IZERO=MIN0(IX1-1,0)
      IXGP=NINT(XGP)
      IF (IXGP.LT.IX1) IXGP=IZERO
      PRINT 1000, MSG,IXGP,IX1,IXEND,IZERO
      KEEP=IXGP
      CALL HITI (IXGP)
      IF (IXGP.LE.IZERO) GO TO 80
      IA=NINT((IXGP-X1)/XINC)+1
      IA=MIN0(N,MAX0(1,IA))
      IF (IXGP.EQ.KEEP) GO TO 60 
      IA=MAX0(1,MIN0(IA,N-NPAGE+1)) 
60    IB=MIN0(IA+NPAGE-1,N) 
      RETURN
80    IA=0
      IB=0 
      RETURN
C----------ALTERNATE ENTRY FOR SINGLE VALUE.
      ENTRY IPICK (N,IA,   X1,XINC,MSG)
      IA=IA+1
      IF (IA.GT.N) IA=0
      XEND=X1+(N-1)*XINC
      XGP=X1+(IA-1)*XINC
      IX1=NINT(X1)
      IXEND=NINT(XEND)
      IZERO=MIN0(IX1-1,0)
      IXGP=NINT(XGP)
      IF (IXGP.LT.IX1) IXGP=IZERO
      PRINT 2000, MSG,IXGP,IX1,IXEND,IZERO
      CALL HITI (IXGP)
      IF (IXGP.LE.IZERO) GO TO 90
      IA=NINT((IXGP-X1)/XINC)+1
      IA=MIN0(N,MAX0(1,IA))
      RETURN
90    IA=0
      RETURN
C----------FORMAT STATEMENTS.
1000  FORMAT (' TYPE NEXT ',A8,6X,'(DEFAULT',I6,')',
     $   6X,'(',I4,' TO',I6,',  OR',I4,' TO QUIT)')
2000  FORMAT (' TYPE ANOTHER ',A8,3X,'(DEFAULT',I6,')',
     $   6X,'(',I4,' TO',I6,',  OR',I4,' TO QUIT)')
      END 




                        


                   
      SUBROUTINE LIMITS (N,X1,X2,X3,   XLO,XUP)
C     GET LIMITING VALUES. 
      DIMENSION X1(N),X2(N),X3(N)
      XLO=X1(1)
      XUP=X1(1)
      DO 10 I=1,N
      XLO=AMIN1(X1(I),X2(I),X3(I),XLO)
10    XUP=AMAX1(X1(I),X2(I),X3(I),XUP)
      IF (XUP.EQ.XLO) XUP=XLO+0.0001
      RETURN
      END






      SUBROUTINE PLOTLINE (K,L,X,XLO,XUP,LETTER,A) 
C----------DIMENSION STATEMENTS.
      CHARACTER*8 LETTER,A(*)
      CHARACTER*8 BUF
C----------GET STARTED.
      N=K*L+1 
      IF (K.EQ.0) N=L+1 
      IF (LETTER(1:1).NE.'(') GO TO 40 
C----------BUILD HEADER ARRAY.
      DO 5 I=1,N
5     A(I)=' ' 
      IF (K.EQ.0) RETURN
      DO 10 I=K,1,-1
      Y=XLO+I*(XUP-XLO)/K 
      WRITE  (BUF,LETTER) Y
      DO 8 J=1,8 
      JJ=MAX0(I*L-7+J,1)
8     A(JJ)=BUF(J:J)
10    CONTINUE
      WRITE (BUF,LETTER) XLO 
      READ (BUF,'(A8)') A(1) 
      RETURN
C----------BUILD PLOT LINE ARRAY. 
40    INDEX=1.5+(X-XLO)/(XUP-XLO)*(N-1) 
      INDEX=MIN0(N,MAX0(INDEX,1)) 
C----------INITIALIZE ARRAY AND CHOOSE WHICH CHARACTER TO PLOT.
      IF (LETTER(2:2).EQ.' '.OR.LETTER(2:2).EQ.'.') THEN
           DO 20 I=1,N 
20         A(I)=LETTER(2:2)
           IF (K.NE.0) THEN
                DO 50 I=1,N,L 
50              A(I)=':'
           END IF
           KK=1
      ELSE IF (A(INDEX).EQ.' '.OR.A(INDEX).EQ.'.'
     $                        .OR.A(INDEX).EQ.':') THEN
           KK=1
      ELSE
           KK=2
      END IF
C----------ADD THE POINT TO THE GRAPH.
      A(INDEX)=LETTER(KK:KK)
      RETURN
      END









      SUBROUTINE QSORT22 (N,A,B)
      DIMENSION A(N),B(N) 
C     SORTS REAL ARRAY A(N) INTO ASCENDING ORDER AND PUTS RESULTS 
C        INTO BOTH A AND B. 
C     THE SECOND ARRAY IS NEEDED FOR SCRATCH SPACE. 
C----------GET STARTED. 
      KK=0
      NN=1
1     NN=2*NN 
      KK=KK+1 
      IF (N.GT.NN) GO TO 1
      L1=1
C----------DO THE SORT (TIME PROPORTIONAL TO N LOG N).
      DO 50 II=1,KK 
      L11=L1
      L1=2*L1 
      DO 10 I=1,N,L1
      I1=I
      I2=I+L11
      I11=MIN0(I1+L11-1,N)
      I22=MIN0(I2+L11-1,N)
      J2=MIN0(I+L1-1,N) 
      DO 25 J=I,J2
      IF (I1.GT.I11) GO TO 19 
      IF (I2.GT.I22) GO TO 20 
      IF (A(I1).LT.A(I2)) GO TO 20
19    B(J)=A(I2)
      I2=I2+1 
      GO TO 25
20    B(J)=A(I1)
      I1=I1+1 
25    CONTINUE
10    CONTINUE
50    CALL MOVE (B,A,N) 
      RETURN
      END 







      SUBROUTINE PLOTL (XLO,XUP,YLO,YUP,LY) 
C     INITIALIZES ALL OF COMMON BLOCKS PLOTZ1 AND PLOTZ2.
C     CALL THIS ROUTINE BEFORE CALLING PLOTXY, PLOTY, PLOTX.
C     XLO,XUP = LIMITS ON X-AXIS.
C     YLO,YUP = LIMITS ON Y-AXIS.
C     LY = NUMBER OF ROWS IN THE PLOT.
C     COMMON BLOCK  PLOTZ1  EXISTS SO THAT AXIS ARRAYS  XAXIS,YAXIS  CAN
C        BE ALTERED.
C     COMMON BLOCK  PLOTZ2  EXISTS SO THAT  PLOTXY  CAN BE SUPPLEMENTED
C        OR BYPASSED BY ADDING POINTS DIRECTLY TO CHARACTER ARRAY  P.
C     LY  NEGATIVE ASSUMES THE RESULTS WILL BE PLOTTED ON A TERMINAL 
C        RATHER THAN LINE PRINTER.
C     LY  ZERO CHANGES THE LIMITS BUT DOES NOT ZERO OUT THE GRAPH OR 
C        CHANGE THE NUMBER OF ROWS OR COLUMNS.
C----------DIMENSION STATEMENTS.
      COMMON/PLOTZ1/XAXIS(121),YAXIS(60),DX,DY,CCC,RRR,NROWS,NCOL 
      COMMON/PLOTZ2/P
      CHARACTER*121 P(60)
C----------CHOOSE THE NUMBER OF ROWS AND COLUMNS.
      IF (LY.NE.0) NROWS=MIN0(60,MAX0(IABS(LY),2)) 
      IF (LY.GT.0) NCOL=121
      IF (LY.LT.0) NCOL=65                     
C----------SET UP THE STARTING POINTS AND INCREMENTS.
      DX=(XUP-XLO)/(NCOL-1) 
      DY=(YUP-YLO)/(NROWS-1)
      IF (DX.EQ.0.) DX=1./(NCOL-1) 
      IF (DY.EQ.0.) DY=1./(NROWS-1) 
      CCC=1.0-XLO/DX
      RRR=1.0+YUP/DY
C----------GENERATE THE AXIS VALUES.
      DO 5 IC=1,121 
5     XAXIS(IC)=XLO+(IC-1)*DX 
      DO 7 IR=1,60
7     YAXIS(IR)=YUP-(IR-1)*DY 
C----------INITIALIZE THE PLOT ARRAY.
      IF (LY.EQ.0) RETURN
      DO 8 IR=1,60
8     P(IR)=' '
      DO 10 IR=1,60,3
      DO 10 IC=1,121
10    P(IR)(IC:IC)='.'
      DO 11 IC=1,121
11    P(NROWS)(IC:IC)='.'
      DO 9 IR=2,60
      DO 9 IC=1,121,8
9     P(IR)(IC:IC)=':'
      RETURN
      END 


             





      SUBROUTINE PLOTXY (CHAR,X,Y)
C     PLOT POINT  (XA,YA)  USING CHARACTER  CHAR(1:1).
C     USES CHAR(2:2) IF PLOTTING OVER ANOTHER ALREADY-PLOTTED CHARACTER
C        THAT IS DIFFERENT FROM CHAR(1:1), BUT ONLY IF CHAR(2:2) IS NOT
C        BLANK.
C----------DIMENSION STATEMENTS.
      COMMON/PLOTZ1/XAXIS(121),YAXIS(60),DX,DY,CCC,RRR,NROWS,NCOL 
      COMMON/PLOTZ2/P
      CHARACTER*121 P(60)
      CHARACTER*2 CHAR
      CHARACTER*1 TEST
C----------DO THE WORK.
      IC=NINT(CCC+X/DX) 
      IR=NINT(RRR-Y/DY)
      IC=MIN0(NCOL,MAX0(IC,1))                             
      IR=MIN0(NROWS,MAX0(IR,1)) 
      TEST=P(IR)(IC:IC)
      IF (TEST.EQ.' '.OR.TEST.EQ.'.'.OR.TEST.EQ.':'
     $   .OR.TEST.EQ.CHAR(1:1).OR.CHAR(2:2).EQ.' ') THEN
           P(IR)(IC:IC)=CHAR(1:1)
      ELSE
           P(IR)(IC:IC)=CHAR(2:2)
      END IF
      RETURN
      END 






                 

      SUBROUTINE PLOTY (FMT) 
C     PRINTS GRAPH AND ORDINATE VALUES. 
C     CALL ROUTINES IN THIS ORDER -- PLOTL,PLOTXY,PLOTY,PLOTX.
C     FMT = FORMAT FOR PRINTING ORDINATES -- EXAMPLE:  '(F10.3)' .
C----------DIMENSION STATEMENTS.
      COMMON/PLOTZ1/XAXIS(121),YAXIS(60),DX,DY,CCC,RRR,NROWS,NCOL 
      COMMON/PLOTZ2/P
      CHARACTER*121 P(60)
      CHARACTER*(*) FMT
      CHARACTER*10 T
C----------PRINT GRAPH. 
      DO 40 IR=1,NROWS
      WRITE (T,FMT) YAXIS(IR)
      IF (T(10:10).EQ.'.') T(10:10)=' '
40    PRINT 200, T,(P(IR)(IC:IC),IC=1,NCOL) 
200   FORMAT (1X,A10,2X,121A1)
      RETURN
      END 

                 






      SUBROUTINE PLOTX2 (MSG,FMT)
C     PRINTS ABSCISSA VALUES HORIZONTALLY.
C     FMT = FORMAT FOR PRINTING ORDINATES -- EXAMPLE:  '(F8.3)' .
C----------DIMENSION STATEMENTS.
      COMMON/PLOTZ1/XAXIS(121),YAXIS(60),DX,DY,CCC,RRR,NROWS,NCOL 
      CHARACTER*(*) MSG,FMT
      CHARACTER*8 BUFFER,LLL(121)
C----------DO THE WORK.
      DO 20 IC=1,NCOL,8 
      WRITE (BUFFER,FMT) XAXIS(IC)
      LLL(IC)=BUFFER
      IF (BUFFER(8:8).EQ.'.') LLL(IC)=' '//BUFFER(1:7)
20    CONTINUE
      PRINT 600, MSG,(LLL(IC),IC=1,NCOL,8) 
600   FORMAT (1X,A5,16A8) 
      RETURN
      END 
                  



      subroutine TRUNK (ICHOICE,inil,X1,Y1,XINC,YINC,NX,NY,S,
     $   XX1,YY1,XXINC,YYINC,NXX,NYY,SS,   CARD,NCARD)
      return
      end


      SUBROUTINE STATIC_OPTION_T (ICHOICE,INIL,S,X1,Y1,XINC,YINC,NX,NY,
     $                            SS,XX1,YY1,XXINC,YYINC,NXX,NYY)
C     TRUNCATE OR EXTEND STATIC FILE, OR CHANGE INCREMENT.
C     ICHOICE=1 USES NEAREST VALUES (NILS PRESERVED).
C     ICHOICE=2 INTERPOLATES (NILS USED AS NEARLY ZERO).
C     INIL=1 EXTRAPOLATES WITH EDGE VALUE.
C     INIL=2 EXTRAPOLATES WITH NILS.
C     X1,Y1,XINC,YINC,NX,NY = ATTRIBUTES FOR INPUT FILE.
C     XX1,YY1,XXINC,YYINC,NXX,NYY = ATTRIBUTES FOR OUTPUT FILE.
C     ARRAY S = INPUT FILE.
C     ARRAY SS = OUTPUT FILE.
C----------DIMENSION STATEMENTS.
      implicit none
      integer ichoice,inil,nx,ny,nxx,nyy             ! arguments
      real x1,y1,xinc,yinc,xx1,yy1,xxinc,yyinc       ! arguments
      real S(NX,NY),SS(NXX,NYY)                      ! arguments
      real znil,hd,x,y,siy,sjy                       ! local
      integer ixx,iyy,ix,iy,jx,jy                    ! local
      PARAMETER (ZNIL=-1.E-30)                       ! local
c     CHARACTER*9 CHOICE(2)
c     CHARACTER*4 NIL(2)
c     DATA CHOICE/'NEAR VALS','INTERP'/
c     DATA NIL/'EDGE','NILS'/
C----------PRESET OUTPUT ARRAY TO NILS.
      IF (INIL.GE.2) THEN
           DO 5 IXX=1,NXX
           DO 5 IYY=1,NYY
5          SS(IXX,IYY)=ZNIL
      END IF
C----------USE NEAREST VALUES.
      IF (ICHOICE.LE.1) THEN
      DO 31 IXX=1,NXX
         HD=XX1+(IXX-1)*XXINC
         IX=NINT((HD-X1)/XINC)+1
         IF (INIL.GE.2) THEN
              IF (IX.LT.1.OR.IX.GT.NX) GO TO 31
         END IF
         IX=MIN0(NX,MAX0(IX,1))
      DO 30 IYY=1,NYY
         HD=YY1+(IYY-1)*YYINC
         IY=NINT((HD-Y1)/YINC)+1
         IF (INIL.GE.2) THEN
              IF (IY.LT.1.OR.IY.GT.NY) GO TO 30
         END IF
         IY=MIN0(NY,MAX0(IY,1))
      SS(IXX,IYY)=S(IX,IY)
30    CONTINUE
31    CONTINUE
C----------INTERPOLATE.
      ELSE
      DO 41 IXX=1,NXX
         HD=XX1+(IXX-1)*XXINC
         X=(HD-X1)/XINC+1.
         IX=X
         JX=IX+1
         IF (INIL.GE.2) THEN
              IF (JX.LT.1.OR.IX.GT.NX) GO TO 41
         END IF
         IX=MIN0(NX,MAX0(IX,1))
         JX=MIN0(NX,MAX0(JX,1))
      DO 40 IYY=1,NYY
         HD=YY1+(IYY-1)*YYINC
         Y=(HD-Y1)/YINC+1.
         IY=Y
         JY=IY+1
         IF (INIL.GE.2) THEN
              IF (JY.LT.1.OR.IY.GT.NY) GO TO 40
         END IF
         IY=MIN0(NY,MAX0(IY,1))
         JY=MIN0(NY,MAX0(JY,1))
      SIY=S(IX,IY)+(X-IX)*(S(JX,IY)-S(IX,IY))
      SJY=S(IX,JY)+(X-IX)*(S(JX,JY)-S(IX,JY))
      SS(IXX,IYY)=SIY+(Y-IY)*(SJY-SIY)
40    CONTINUE
41    CONTINUE   
      END IF
C----------ADD COMMENT CARDS.
c     NCARD=NCARD+1
c     XEND=X1+(NX-1)*XINC
c     YEND=Y1+(NY-1)*YINC
c     WRITE (CARD(NCARD),1000) 'OLD',NINT(X1),NINT(XEND),
c    $   NINT(XINC),NINT(Y1),NINT(YEND),NINT(YINC),CHOICE(ICHOICE),
c    $   NIL(INIL)
c     NCARD=NCARD+1
c     XEND=XX1+(NXX-1)*XXINC
c     YEND=YY1+(NYY-1)*YYINC
c     WRITE (CARD(NCARD),1000) 'NEW',NINT(XX1),NINT(XEND),
c    $   NINT(XXINC),NINT(YY1),NINT(YEND),NINT(YYINC),CHOICE(ICHOICE),
c    $   NIL(INIL)
c1000 FORMAT ('ISEP- TRUNC ',A3,'  XGP=',3I6,' YGP=',3I6,2X,A9,1X,A4)
      RETURN
      END







      SUBROUTINE OPTIONT (X1,Y1,XINC,YINC,NX,NY,S,NCARD,CARD,SS)
C     TRUNCATE OR EXTEND OR RESAMPLE STATIC FILE BY ASKING QUESTIONS.
C----------DIMENSION STATEMENTS.
      DIMENSION S(*),SS(*)
      CHARACTER*80 CARD(*)
C----------ASK QUESTIONS REGARDING X AND Y GROUND POSITIONS.
      CALL GPQUEST ('X',X1,XINC,NX,   XX1,XXINC,NXX)
      CALL GPQUEST ('Y',Y1,YINC,NY,   YY1,YYINC,NYY)
      IF (XX1.EQ.X1.AND.YY1.EQ.Y1.AND.XXINC.EQ.XINC.AND.
     $   YYINC.EQ.YINC.AND.NXX.EQ.NX.AND.NYY.EQ.NY) RETURN
      CALL HOW (ICHOICE)
C----------DO THE TRUNCATION AND/OR EXTENSION AND/OR RESAMPLING.
      CALL TRUNK (ICHOICE,0,X1,Y1,XINC,YINC,NX,NY,S,
     $   XX1,YY1,XXINC,YYINC,NXX,NYY,SS,   CARD,NCARD)
C----------COPY NEW STUFF TO OLD LOCATION.
      X1=XX1
      Y1=YY1
      XINC=XXINC
      YINC=YYINC
      NX=NXX
      NY=NYY
      CALL MOVE (SS,S,NX*NY)
      RETURN                 
      END





      SUBROUTINE GPQUEST (CHAR,X1,XINC,NX,   XX1,XXINC,NXX)
C     GET NEW GROUND POSITION ATTRIBUTES BY ASKING QUESTIONS.
      CHARACTER*1 CHAR
      CHARACTER*8 Q
C----------ASK QUESTION REGARDING GROUND POSITION INCREMENT.
      PRINT *, CHAR,' GROUND POSITION INCREMENT IS ',XINC
      PRINT *, 'TYPE NEW INCREMENT   (OR RETURN IF OK)'
20    XXINC=XINC
      CALL HITX (XXINC)
      IF (XXINC.LE.0.) THEN
           PRINT *, 'VALUE CANNOT BE ZERO OR LESS -- TRY AGAIN'
           GO TO 20
      END IF
C----------ASK QUESTION REGARDING GROUND POSITION RANGE.
30    XEND=X1+(NX-1)*XINC
      PRINT *, CHAR,' GROUND POSITION RANGE IS ',X1,' TO ',XEND
      PRINT *, 'TYPE NEW STARTING GROUND POSITION   (OR RETURN IF OK)'
      XX1=X1
      CALL HITX (XX1)
      PRINT *, 'TYPE NEW ENDING GROUND POSITION   (OR RETURN IF OK)'
      XXEND=XEND
      CALL HITX (XXEND)
C----------CHECK FOR VALID INFORMATION.
      NXX=NINT((XXEND-XX1)/XXINC)+1
      IF (NXX.LE.0) THEN
           PRINT *, 'ZERO OR NEGATIVE NUMBER OF GROUND POSITIONS',
     $   ' -- TRY AGAIN'
           GO TO 30
      END IF
C----------FINISH UP.
      IF (AMOD(XX1-X1,XINC).EQ.0..AND.AMOD(XX1-X1,XXINC).EQ.0.) RETURN
      PRINT *, 'THE DIFFERENCE BETWEEN THE OLD AND NEW STARTING ',
     $   CHAR,' GROUND POSITIONS'
      PRINT *, 'IS NOT AN INTEGRAL MULTIPLE OF THE GROUND POSITION',
     $   ' INCREMENT.'
      PRINT *, 'DO YOU WANT TO CHANGE THE NEW GROUND ',
     $   'POSITION RANGE?  (DEFAULT Y)'
      Q='Y'
      CALL HITQ (Q)
      IF (Q.EQ.'Y') GO TO 30
      RETURN
      END



                              
           
                      

      SUBROUTINE OPTIONJ (TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
     $   NX,NY,S,NCARD,CARD,S1,S2,S3,NNNN,KKKK)
C     JOIN OR COMPARE WITH ANOTHER FILE.
C     S = INPUT FILE (INPUT AND OUTPUT).
C     S1 = RESAMPLED INPUT FILE (EXTENDED IF NECESSARY).
C     S2 = RESAMPLED SECOND FILE (EXTENDED AND RESAMPLED IF NECESSARY).
C     S3 = COMBINED FILE (COMBINATION OF S1 AND S2).
C     CARD = COMMENT CARDS.
C----------DIMENSION AND DATA STATEMENTS.
      DIMENSION S(NNNN),S1(NNNN),S2(NNNN),S3(NNNN)
      CHARACTER*8 OPTION,TYPE,TYPE2,Q
      CHARACTER*80 CARD(*)
      CHARACTER*80 VAXFILE
C----------READ A SECOND STATIC FILE.
      PRINT *, 'NOW LET''S READ A SECOND CPS STATIC FILE'
      CALL RFILE (VAXFILE,TYPE2,NHX2,NHY2,NHX4,NHY4,XX1,YY1,XXINC,YYINC,
     $   NXX,NYY,CARD(NCARD+2),NDUMMY,S3,NNNN,KKKK)
      IF (VAXFILE.EQ.' ') RETURN
      CARD(NCARD+1)='ISEP- ****** BEGIN CARDS FROM SECOND FILE ******'
      NCARD1=NCARD+NDUMMY+2
      CARD(NCARD1)='ISEP- ****** END OF CARDS FROM SECOND FILE ******'
      IFLAG=2
C----------MAKE THE TWO FILES MATCH.
      PRINT 1000, 'FIRST ',NHX,NHY,NHX3,NHY3,TYPE
      PRINT 1000, 'SECOND',NHX2,NHY2,NHX4,NHY4,TYPE2
1000  FORMAT (' HEADER WORDS FOR ',A6,' FILE ARE:  ',2I3,3X,2I3,
     $   '   TYPE=',A8)
      CALL MATCH (X1,Y1,XINC,YINC,NX,NY,S,   S1,
     $     XX1,YY1,XXINC,YYINC,NXX,NYY,S3,   S2,
     $     XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,   CARD,NCARD1)
C----------CHOOSE OPERATION.
2     PRINT *, ' '
      PRINT *, '     P = PLOT THE TWO FILES'
      PRINT *, '     H = MAKE HORIZONTAL PLOTS OF THE TWO FILES'
      PRINT *, '  HELP = see note regarding options N,AV,ADD,SUB,SPLICE'
      PRINT *, '     N = REPLACE NIL''S (BOTH FILES) WITH INTERPOLATED',
     $                       ' VALUES'
      PRINT *, '     Z = REPLACE NIL''S (BOTH FILES) WITH ZEROES'
      PRINT *, '    AV = AVERAGE THE FILES TOGETHER'
      PRINT *, '   ADD = ADD THE FILES TOGETHER'
      PRINT *, '   SUB = SUBTRACT SECOND FILE FROM FIRST FILE'
      PRINT *, 'SPLICE = SPLICE THE FILES TOGETHER' 
      PRINT *, '  DONE = FINISHED BUILDING COMBINED FILE' 
      PRINT *, 'CHOOSE ONE OF THE ABOVE FILE-COMBINING OPTIONS' 
      OPTION=' '
      CALL HITC (OPTION)
C----------ACT ON DESIRED OPERATION.
      IF (OPTION.EQ.'P') THEN                
           CALL OPTIONP (IFLAG,XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,
     $        S1,S2,S3)
           CALL HIT
      ELSE IF (OPTION.EQ.'H') THEN
           CALL OPTIONH (IFLAG,XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,
     $        S1,S2,S3)
           IF (NXXX.EQ.1.OR.NYYY.EQ.1) CALL HIT
      ELSE IF (OPTION.EQ.'HELP') THEN
           CALL OPTHELP
      ELSE IF (OPTION.EQ.'N') THEN
cc         CALL OPTIONN (S1,NXXX,NYYY,CARD,NCARD)
cc         CALL OPTIONN (S2,NXXX,NYYY,CARD,NCARD)
      ELSE IF (OPTION.EQ.'Z') THEN
           CALL OPTIONZ (S1,NXXX,NYYY,CARD,NCARD)
           CALL OPTIONZ (S2,NXXX,NYYY,CARD,NCARD)
      ELSE IF (OPTION.EQ.'AV') THEN
           CALL OPTAV (S1,S2,S3,NXXX,NYYY,CARD,NCARD1)
           IFLAG=3
      ELSE IF (OPTION.EQ.'ADD') THEN
           CALL OPTADD (S1,S2,S3,NXXX,NYYY,CARD,NCARD1)
           IFLAG=3       
      ELSE IF (OPTION.EQ.'SUB') THEN
           CALL OPTSUB (S1,S2,S3,NXXX,NYYY,CARD,NCARD1)
           IFLAG=3
      ELSE IF (OPTION.EQ.'SPLICE') THEN
           CALL SPLICE (X1,Y1,XINC,YINC,NX,NY,S1,
     $        XX1,YY1,XXINC,YYINC,NXX,NYY,S2,
     $        XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,S3,
     $        CARD,NCARD1)
           IFLAG=3
C----------GO BACK TO MENU. 
      END IF
      IF (OPTION.NE.'DONE') GO TO 2
C----------FINISHED.
      IF (IFLAG.EQ.2) RETURN
      PRINT *, 'TYPE  Y  TO CONTINUE WITH THE NEW COMBINED FILE',
     $                      '  (DEFAULT)'
      PRINT *, 'TYPE  N  TO CONTINUE WITH THE UNCHANGED SINGLE FILE'
      Q='Y'
      CALL HITQ (Q)
      IF (Q.EQ.'N') THEN
           PRINT *, 'WE ARE CONTINUING WITH THE UNCHANGED SINGLE FILE'
           RETURN
      END IF
C----------REPLACE ORIGINAL FILE WITH NEW COMBINED FILE.
      NCARD=NCARD1
      X1=XXX1
      Y1=YYY1
      XINC=XXXINC
      YINC=YYYINC
      NX=NXXX
      NY=NYYY
      CALL MOVE (S3,S,NX*NY)
      PRINT *, 'WE ARE CONTINUING WITH THE NEW COMBINED FILE'
      RETURN
      END




      SUBROUTINE OPTHELP
C----------TYPE INFORMATION THE USER NEEDS TO KNOW.
      PRINT *, ' '
      PRINT *, 'When performing operations AV, ADD, SUB, and SPLICE,',
     $   ' NIL''s are treated as shown'
      PRINT *, 'in the following table (where A and B represent any',
     $   ' non-NIL values at a given'
      PRINT *, 'ground position):'
      PRINT *, ' '
      PRINT *, 'FILE1  FILE2  AV result  ADD result  SUB result  ',
     $   'SPLICE result (graded regions)'
      PRINT *, '-----  -----  ---------  ----------  ----------  ',
     $   '------------------------------'
      PRINT *, '  A      B     (A+B)/2      A+B         A-B      ',
     $   'WA*A+WB*B    (WA and WB are'
      PRINT *, '  A     NIL       A         2*A         NIL      ',
     $   '    A        variable weights)'    
      PRINT *, ' NIL     B        B         2*B         NIL      ',
     $   '    B'
      PRINT *, ' NIL    NIL      NIL        NIL         NIL      ',
     $   '   NIL'
      PRINT *, ' '
      PRINT *, 'If you do not want NIL''s to be treated as above:'
      PRINT *, ' (1) use option N to replace NIL''s by interpolated'//
     $                          ' values, or'
      PRINT *, ' (2) use option Z to replace NIL''s by zeroes.'
      CALL HIT
      RETURN
      END





      SUBROUTINE OPTAV (S1,S2,S3,NX,NY,CARD,NCARD)
C     AVERAGE S1 AND S2 AND PUT RESULT INTO S3.
C----------DIMENSION STATEMENTS.
      DIMENSION S1(*),S2(*),S3(*)
      CHARACTER*80 CARD(*)
      PARAMETER (ZNIL=-1.E-30)
C----------DO THE WORK.
      N=NX*NY
      DO 10 I=1,N
      IF (S1(I).NE.ZNIL.AND.S2(I).NE.ZNIL) THEN
           S3(I)=0.5*(S1(I)+S2(I))
      ELSE IF (S1(I).EQ.ZNIL) THEN
           S3(I)=S2(I)
      ELSE
           S3(I)=S1(I)
      END IF
10    CONTINUE
C----------ADD A COMMENT CARD.
      NCARD=NCARD+1
      CARD(NCARD)='ISEP- SECOND FILE AVERAGED WITH THIS FILE'
      PRINT *, 'SECOND FILE AVERAGED WITH THIS FILE'
      RETURN
      END







      SUBROUTINE OPTADD (S1,S2,S3,NX,NY,CARD,NCARD)
C     ADD S1 AND S2 AND PUT RESULT INTO S3.
C----------DIMENSION STATEMENTS.
      DIMENSION S1(*),S2(*),S3(*)
      CHARACTER*80 CARD(*)
      PARAMETER (ZNIL=-1.E-30)
C----------DO THE WORK.
      N=NX*NY
      DO 10 I=1,N
      IF (S1(I).NE.ZNIL.AND.S2(I).NE.ZNIL) THEN
           S3(I)=S1(I)+S2(I)
      ELSE IF (S1(I).EQ.ZNIL.AND.S2(I).NE.ZNIL) THEN
           S3(I)=2.*S2(I)
      ELSE IF (S2(I).EQ.ZNIL.AND.S1(I).NE.ZNIL) THEN
           S3(I)=2.*S1(I)
      ELSE
           S3(I)=ZNIL
      END IF
10    CONTINUE
C----------ADD A COMMENT CARD.
      NCARD=NCARD+1
      CARD(NCARD)='ISEP- SECOND FILE ADDED TO THIS FILE'
      PRINT *, 'SECOND FILE ADDED TO THIS FILE'
      RETURN           
      END







      SUBROUTINE OPTSUB (S1,S2,S3,NX,NY,CARD,NCARD)
C     SUBTRACT S2 FROM S1 AND PUT RESULT INTO S3.
C----------DIMENSION STATEMENTS.
      DIMENSION S1(*),S2(*),S3(*)
      CHARACTER*80 CARD(*)
      PARAMETER (ZNIL=-1.E-30)
C----------DO THE WORK.
      N=NX*NY
      DO 10 I=1,N
      IF (S1(I).NE.ZNIL.AND.S2(I).NE.ZNIL) THEN
           S3(I)=S1(I)-S2(I)
      ELSE
           S3(I)=ZNIL
      END IF
10    CONTINUE   
C----------ADD A COMMENT CARD.
      NCARD=NCARD+1
      CARD(NCARD)='ISEP- SECOND FILE SUBTRACTED FROM THIS FILE'
      PRINT *, 'SECOND FILE SUBTRACTED FROM THIS FILE'
      RETURN
      END
          




      SUBROUTINE MATCH (X1,Y1,XINC,YINC,NX,NY,S,   T,
     $     XX1,YY1,XXINC,YYINC,NXX,NYY,SS,   TT,
     $     XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,   CARD,NCARD)
C     MATCH THE RANGES OF TWO STATIC FILES BY ASKING QUESTIONS.
C     X1,Y1,XINC,YINC,NX,NY,S = FIRST STATIC FILE (T OUTPUT).
C     XX1,YY1,XXINC,YYINC,NXX,NYY,SS = SECOND STATIC FILE (TT OUTPUT).
C     XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY = MATCHED ATTRIBUTES (OUTPUT).
C----------DIMENSION STATEMENTS.
      DIMENSION S(*),T(*),SS(*),TT(*)
      CHARACTER*80 CARD(*)
C----------DRAW THE PICTURES.
      CALL PICTURE ('X',  X1,XINC,NX,  XX1,XXINC,NXX,
     $    XXX1,XXXINC,NXXX)
      CALL PICTURE ('Y',  Y1,YINC,NY,  YY1,YYINC,NYY,
     $    YYY1,YYYINC,NYYY)
C----------NO CHANGES NEEDED.
      IF (XX1.EQ.X1.AND.YY1.EQ.Y1.AND.XXINC.EQ.XINC.AND.
     $   YYINC.EQ.YINC.AND.NXX.EQ.NX.AND.NYY.EQ.NY) THEN
           CALL MOVE (S,T,NX*NY)
           CALL MOVE (SS,TT,NXX*NYY)
C----------CHANGE BOTH FILES.
      ELSE
           PRINT *, 'If the ground position RANGES of these two',
     $        ' files do not match,'
           PRINT *, '  the files will be extended with NILs (as',
     $        ' necessary).'
           PRINT *, 'If the ground position INCREMENTS of these two',
     $        ' files do not match,'
           PRINT *, '  the second file will be resampled.'
           CALL HOW (ICHOICE)
           CALL TRUNK (ICHOICE,1,XX1,YY1,XXINC,YYINC,NXX,NYY,SS,
     $        XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,TT,   CARD,NCARD)
           CALL TRUNK (ICHOICE,1,X1,Y1,XINC,YINC,NX,NY,S,
     $        XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,T,   CARD,NCARD)
      END IF
      RETURN                     
      END






      SUBROUTINE HOW (ICHOICE)
C     FIND OUT HOW TO RESAMPLE.
10    PRINT *, 'WHILE ADJUSTING GROUND POSITION RANGE AND/OR',
     $   ' INCREMENT:'
      PRINT *, 'TYPE  1  TO USE NEAREST VALUES  (NILS PRESERVED)',
     $                           '     (DEFAULT)'
      PRINT *, 'TYPE  2  TO INTERPOLATE  (NILS TREATED AS NEARLY ZERO)'
      ICHOICE=1
      CALL HITI (ICHOICE)
      IF (ICHOICE.NE.1.AND.ICHOICE.NE.2) GO TO 10
      RETURN
      END








      SUBROUTINE PICTURE (WHICH,  X1,XINC,NX,  XX1,XXINC,NXX,
     $    XXX1,XXXINC,NXXX)
C     PRINT PICTURE OF X OR Y GROUND POSITION RANGES OF TWO FILES AND
C        THEIR COMBINATION.
C     RETURNS THE LIMITS OF THE COMBINED FILE.
C        XXX1 IS SET TO X1 +- MULTIPLE OF XINC.
C        XXXINC IS SET TO XINC.
C----------DIMENSION STATEMENTS.
      CHARACTER*(*) WHICH 
C----------DO THE WORK.
      XEND=X1+(NX-1)*XINC
      XXEND=XX1+(NXX-1)*XXINC
      XXX1=AMIN1(X1,XX1)
      XXXEND=AMAX1(XEND,XXEND)
      XXXINC=XINC
      IX=1+(XXX1-X1)/XINC
      XXX1=X1+(IX-1)*XINC
      NXXX=1+NINT((XXXEND-XXX1)/XXXINC)
      XXXEND=XXX1+(NXXX-1)*XXXINC
      IF (NX.EQ.1.AND.NXX.EQ.1.AND.NXXX.EQ.1.AND.X1.EQ.XX1) RETURN
C----------PRINT PICTURE.
      PRINT 1000, WHICH
      CALL PICLINE (XXX1,XXXEND,'FIRST','1',X1,XINC,NX)
      CALL PICLINE (XXX1,XXXEND,'SECOND','2',XX1,XXINC,NXX)
      CALL PICLINE (XXX1,XXXEND,'COMBINED','3',XXX1,XXXINC,NXXX)
C----------FINISH UP. 
      GPA=AMAX1(X1,XX1)
      GPB=AMIN1(XEND,XXEND)
      GPC=(GPA+GPB)/2.
      IF (GPA.LE.GPB) THEN
         PRINT *, 'FIRST AND SECOND FILES OVERLAP FROM ',GPA,' TO ',GPB
         PRINT *, 'CENTER OF OVERLAP IS AT GROUND POSITION ',GPC 
      ELSE
         PRINT *, 'FIRST AND SECOND FILES DO NOT OVERLAP' 
      END IF
      RETURN
C----------FORMAT STATEMENTS.
1000  FORMAT (/7X,'FILE  START',14X,A1,' GROUND POSITIONS',13X,
     $   'STOP  INCR  #VALS')
      END





                         
      SUBROUTINE PRETTY (X1,XINC,NX,  XX1,XXINC,NXX,
     $    XXX1,XXXINC,NXXX,S3)
C     PRINT PICTURE OF GROUND POSITION RANGES OF TWO FILES AND
C        THEIR SPLICING INFORMATION.
C----------DIMENSION STATEMENTS.
      DIMENSION S3(NXXX)
C----------DO THE WORK.
      PRINT 1000
      XXXEND=XXX1+(NXXX-1)*XXXINC
      CALL PRETTY2 (XXX1,XXXEND,'FIRST','1',X1,XINC,NX)
      CALL PRETTY2 (XXX1,XXXEND,'SECOND','2',XX1,XXINC,NXX)
      CALL PRETTY3 (XXX1,XXXEND,'SPLICED',XXX1,XXXINC,NXXX,S3)
      RETURN
C----------FORMAT STATEMENTS.
1000  FORMAT (/7X,'FILE  START',16X,'GROUND POSITIONS',13X,
     $   'STOP')
      END





                         
      SUBROUTINE PRETTY2 (XLO,XUP,WORD,CHAR,X1,XINC,NX)
C     PRINT ONE LINE OF PICTURE OF STATIC FILE GROUND POSITION RANGE.
C----------DIMENSION STATEMENTS.
      PARAMETER (N=40)
      CHARACTER*(*) WORD,CHAR
c     CHARACTER*1 H(N),CHAR2
      CHARACTER*1 H(N)
      DIMENSION S(NX)
C----------ARITHMETIC STATEMENT FUNCTION.
      INDEX(X)=1+NINT((N-1)*(X-XLO)/AMAX1(XUP-XLO,1.))
C----------DO THE WORK. 
      XEND=X1+(NX-1)*XINC
      I1=INDEX(X1)
      I2=INDEX(XEND)
      DO 10 I=1,N
10    H(I)=' '
      DO 11 I=I1,I2
11    H(I)=CHAR
      PRINT 2000, WORD,NINT(X1),H,NINT(XEND)
      RETURN
C----------MORE WORK.
      ENTRY PRETTY3 (XLO,XUP,WORD,X1,XINC,NX,S)
      XEND=X1+(NX-1)*XINC
      DO 20 I=1,N
20    H(I)=' '
      DO 30 IX=1,NX
      X=X1+(IX-1)*XINC
      I=INDEX(X)
      IF (S(IX).GE.1.) THEN
           H(I)='1'
      ELSE IF (S(IX).LE.0.) THEN
           H(I)='2'
      ELSE
           H(I)='-'
      END IF
30    CONTINUE 
      PRINT 2000, WORD,NINT(XLO),H,NINT(XUP)
      RETURN
C----------FORMAT STATEMENT.
2000  FORMAT (1X,A10,I7,1X,40A1,1X,I7)
      END

                      


                         
      SUBROUTINE PICLINE (XLO,XUP,WORD,CHAR,X1,XINC,NX)
C     PRINT ONE LINE OF PICTURE OF STATIC FILE GROUND POSITION RANGE.
C----------DIMENSION STATEMENTS.
      PARAMETER (N=40)
      CHARACTER*(*) WORD,CHAR
      CHARACTER*1 H(N)
C----------ARITHMETIC STATEMENT FUNCTION.
      INDEX(X1)=1+NINT((N-1)*(X1-XLO)/AMAX1(XUP-XLO,1.))
C----------DO THE WORK.
      XEND=X1+(NX-1)*XINC
      I1=INDEX(X1)
      I2=INDEX(XEND)
      DO 10 I=1,N
10    H(I)=' '
      DO 11 I=I1,I2
11    H(I)=CHAR
      PRINT 2000, WORD,NINT(X1),H,NINT(XEND),NINT(XINC),NX
      RETURN
C----------FORMAT STATEMENT.
2000  FORMAT (1X,A10,I7,1X,40A1,1X,I7,I6,I7)
      END

                      



      CHARACTER*5 FUNCTION SHOW5 (STAT) 
C     RETURNS FORMATTED STATIC VALUE (NEAREST INTEGER OR NIL). 
C     VALUE SHOULD BE PRINTED WITH FORMAT A5.
      PARAMETER (ZNIL=-1.E-30)
      SHOW5='  NIL' 
      IF (STAT.NE.ZNIL) WRITE (SHOW5,'(I5)') NINT(STAT) 
      RETURN
      END              




      CHARACTER*10 FUNCTION SHOW10 (STAT) 
C     RETURNS FORMATTED STATIC VALUE (OR NIL). 
C     VALUE SHOULD BE PRINTED WITH FORMAT A10.
      PARAMETER (ZNIL=-1.E-30)
      SHOW10='       NIL' 
      IF (STAT.NE.ZNIL) WRITE (SHOW10,'(F10.3)') STAT
      RETURN
      END 





      SUBROUTINE SPLICE (X1,Y1,XINC,YINC,NX,NY,S1,
     $   XX1,YY1,XXINC,YYINC,NXX,NYY,S2,
     $   XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,S3,
     $   CARD,NCARD)
C     SPLICE TWO FILES TOGETHER.
C----------DIMENSION STATEMENTS.
      PARAMETER (ZNIL=-1.E-30)
      DIMENSION S1(NXXX,NYYY),S2(NXXX,NYYY),S3(NXXX,NYYY)
      DIMENSION XY(2)
      CHARACTER*80 CARD(*)
      CHARACTER*8 Q
C----------WARN USER ABOUT RESTRICTION.
      PRINT *, 'AT PRESENT, SPLICING CAN BE DONE ONLY FOR 2-D FILES'
      IF (NYYY.GT.1) THEN
           PRINT *, 'THEREFORE WE CANNOT SPLICE THESE FILES'
           RETURN
      ELSE
           PRINT *, 'THEREFORE WE ARE OK IN THIS CASE'
      END IF
C----------DRAW THE PICTURES.
      CALL PICTURE ('X',  X1,XINC,NX,  XX1,XXINC,NXX,
     $    XXX1,XXXINC,NXXX)
      XEND=X1+(NX-1)*XINC
      XXEND=XX1+(NXX-1)*XXINC
C----------SPLICE THE FILES TOGETHER. 
      GPA=AMAX1(X1,XX1)
      GPB=AMIN1(XEND,XXEND)
      IF (GPA.GT.GPB) GO TO 20
41    PRINT *, 'TYPE NUMBER OF SPLICE LOCATIONS DESIRED -- 1 OR 2'
      PRINT *, '      (OR TYPE 0 TO MERGE FILES TOGETHER'
      PRINT *, '  WITH AUTOMATIC GRADING IN OVERLAPPING AREAS)'
      PRINT *, '                (no default)'
      NSP=-999
      CALL HITI (NSP) 
      IF (NSP.EQ.1) GO TO 50
      IF (NSP.EQ.2) GO TO 60
      IF (NSP.EQ.0) GO TO 70
      GO TO 41
C----------THE FILES DO NOT OVERLAP.
20    PRINT *, 'SINCE THESE FILES DO NOT OVERLAP, AVERAGE THEM INSTEAD.'
      RETURN
C----------ONLY ONE SPLICE LOCATION DESIRED.
50    GPC=NINT((2.*GPA+GPB)/3.) 
      GPD=NINT((GPA+2.*GPB)/3.)
      GPE=9999999. 
      GPF=9999999. 
      PRINT *, 'TYPE GROUND POSITION OF START OF GRADED SPLICE',
     $   '  (DEFAULT ',NINT(GPC),')' 
      CALL HITX (GPC)
      PRINT *, 'TYPE GROUND POSITION OF END OF GRADED SPLICE', 
     $   '  (DEFAULT ',NINT(GPD),')' 
      CALL HITX (GPD)
      IF (GPA.LE.GPC.AND.GPC.LT.GPD.AND.GPD.LE.GPB) GO TO 61
      PRINT *, 'INVALID COMBINATION -- TRY AGAIN' 
      GO TO 50
C----------TWO SPLICE LOCATIONS DESIRED.
60    GPC=NINT((7.*GPA+GPB)/8.)
      GPD=NINT((3.*GPA+GPB)/4.)
      GPE=NINT((GPA+3.*GPB)/4.)
      GPF=NINT((GPA+7.*GPB)/8.)
      PRINT *, 'TYPE GROUND POSITION OF START OF FIRST GRADED SPLICE', 
     $   '  (DEFAULT ',NINT(GPC),')' 
      CALL HITX (GPC)
      PRINT *, 'TYPE GROUND POSITION OF END OF FIRST GRADED SPLICE', 
     $   '  (DEFAULT ',NINT(GPD),')' 
      CALL HITX (GPD)
      PRINT *, 'TYPE GROUND POSITION OF START OF SECOND GRADED SPLICE',
     $   '  (DEFAULT ',NINT(GPE),')' 
      CALL HITX (GPE)
      PRINT *, 'TYPE GROUND POSITION OF END OF SECOND GRADED SPLICE',
     $   '  (DEFAULT ',NINT(GPF),')' 
      CALL HITX (GPF)
      IF (GPA.LE.GPC.AND.GPC.LE.GPD.AND.GPD.LT.GPE
     $   .AND.GPE.LE.GPF.AND.GPF.LE.GPB) GO TO 61 
      PRINT *, 'INVALID COMBINATION -- TRY AGAIN' 
      GO TO 60
C----------MERGE WITH GRADING.
70    ntaper=10
      nshear=3
      print *, 'type maximum number of points to taper over (>=0)',
     $   '  (default ',ntaper,')'
      call hiti (ntaper)
      print *, 'type number of points to ignore next to a nil (>=0)',
     $   '  (default ',nshear,')'
      call hiti (nshear)
      ntaper=max(ntaper,0)
      nshear=max(nshear,0)
      do ixxx=1,nxxx
           if (s1(ixxx,1).eq.znil.and.s2(ixxx,1).eq.znil) then
                s3(ixxx,1)=znil
           else if (s1(ixxx,1).eq.znil) then
                s3(ixxx,1)=s2(ixxx,1)
           else if (s2(ixxx,1).eq.znil) then
                s3(ixxx,1)=s1(ixxx,1)
           else
                jmin=min(ixxx-ntaper,1)
                jmax=min(ixxx+ntaper,nxxx)
                do j=ixxx-1,jmin,-1
                     if (s1(j,1).eq.znil) go to 741
                end do
                j=jmin-1
741             ifirst1=j+1
                do j=ixxx+1,jmax
                     if (s1(j,1).eq.znil) go to 751
                end do
                j=jmax+1
751             ilast1=j-1
                do j=ixxx-1,jmin,-1
                     if (s2(j,1).eq.znil) go to 761
                end do
                j=jmin-1
761             ifirst2=j+1
                do j=ixxx+1,jmax
                     if (s2(j,1).eq.znil) go to 771
                end do
                j=jmax+1
771             ilast2=j-1
                ndist1=min(ixxx-ifirst1,ilast1-ixxx)
                ndist2=min(ixxx-ifirst2,ilast2-ixxx)
                nshearmod=min(nshear,ndist1,ndist2)
                w1=ndist1-nshearmod+1
                w2=ndist2-nshearmod+1
                s3(ixxx,1)=(w1*s1(ixxx,1)+w2*s2(ixxx,1))/(w1+w2)
           end if
      end do
      RETURN
C----------FINISH MAKING DECISIONS.
61    PRINT *, 'WHICH FILE SHOULD SUPPLY THE LOWEST GROUND POSITIONS',
     $   ' --  1 OR 2  ?' 
      KLO=0 
      CALL HITI (KLO)       
      IF (KLO.NE.1.AND.KLO.NE.2) GO TO 61 
      KUP=3-KLO 
C----------SET THE WEIGHTS INTO THE OUTPUT FILE.
      IC=NINT((GPC-XXX1)/XXXINC)+1
      ID=NINT((GPD-XXX1)/XXXINC)+1
      IE=NINT((GPE-XXX1)/XXXINC)+1
      IF=NINT((GPF-XXX1)/XXXINC)+1
      XY(KLO)=1.
      IF (KLO.EQ.2) XY(KLO)=0.
      XY(KUP)=1.-XY(KLO)
      DO 65 IXXX=1,NXXX
      IF (IXXX.LE.IC.OR.IXXX.GE.IF) GO TO 63
      IF (IXXX.GE.ID.AND.IXXX.LE.IE) GO TO 64 
      IF (IXXX.LE.ID) WEIGHT=(IXXX-IC)/AMAX0(ID-IC,1)
      IF (IXXX.GT.ID) WEIGHT=(IF-IXXX)/AMAX0(IF-IE,1)
      S3(IXXX,1)=(1.-WEIGHT)*XY(KLO)+WEIGHT*XY(KUP) 
      GO TO 65
63    S3(IXXX,1)=XY(KLO)
      GO TO 65
64    S3(IXXX,1)=XY(KUP)
65    CONTINUE
C----------VERIFY THE DECISION.
      CALL PRETTY (X1,XINC,NX,  XX1,XXINC,NXX,  XXX1,XXXINC,NXXX,S3)
      PRINT *, 'ARE THE SPLICING PARAMETERS OK?  (DEFAULT Y)'
      Q='Y'
      CALL HITQ (Q)
      IF (Q.EQ.'N') GO TO 41
C----------DO THE GRADED SPLICING.
      DO 75 IXXX=1,NXXX
      WEIGHT=S3(IXXX,1)
      IF (WEIGHT.GE.1.) THEN
           VALUE=S1(IXXX,1)
      ELSE IF (WEIGHT.LE.0.) THEN
           VALUE=S2(IXXX,1)
      ELSE IF (S1(IXXX,1).EQ.ZNIL) THEN
           VALUE=S2(IXXX,1)
      ELSE IF (S2(IXXX,1).EQ.ZNIL) THEN
           VALUE=S1(IXXX,1)
      ELSE           
           VALUE=(1.-WEIGHT)*S2(IXXX,1)+WEIGHT*S1(IXXX,1)
      END IF
75    S3(IXXX,1)=VALUE
      RETURN
      END






      SUBROUTINE OPTIONH (IFLAG,X1,Y1,XINC,YINC,NX,NY,S1,S2,S3)
C     PLOT STATIC FILES HORIZONTALLY.
C     IFLAG=1: 1ST PLOT IS NEW VALUES; 2ND PLOT IS OLD VALUES.
C     IFLAG=2: 1ST PLOT IS FIRST FILE; 2ND PLOT IS SECOND FILE.
C     IFLAG=3: 1ST PLOT IS FIRST 2 FILES; 2ND PLOT IS THIRD FILE.
C----------DIMENSION STATEMENTS.
      PARAMETER (LY=-10,ZNIL=-1.E-30)
      DIMENSION S1(NX,NY),S2(NX,NY),S3(NX,NY)
c     CHARACTER*1 CHAR,CHAR1,CHAR2
      CHARACTER*8 Q    
C----------GET STARTED.
      IF (IFLAG.NE.3) CALL LIMITS (NX*NY,S1,S2,S2,   SLO,SUP)
      IF (IFLAG.EQ.3) CALL LIMITS (NX*NY,S1,S2,S3,   SLO,SUP)
      IF (NX.EQ.1) GO TO 222
C----------WE WILL PLOT IN X DIRECTION.
111   XLO=X1
      XUP=X1+(NX-1)*XINC
      PRINT 2000, 'X',NINT(XLO),NINT(XUP)
      CALL HITXX (XLO,XUP)
      IA=NINT((XLO-X1)/XINC)+1
      IB=NINT((XUP-X1)/XINC)+1
      IA=MIN0(NX,MAX0(IA,1))      
      IB=MIN0(NX,MAX0(IB,1))      
      XLO=X1+(IA-1)*XINC
      XUP=X1+(IB-1)*XINC
      IY=1
5     Y=Y1+(IY-1)*YINC                                      
      IF (IFLAG.EQ.1) PRINT 1001, 'YGP=',NINT(Y)
      IF (IFLAG.EQ.2) PRINT 1002, 'YGP=',NINT(Y)
      IF (IFLAG.EQ.3) PRINT 1003, 'YGP=',NINT(Y)
C----------PRINT FIRST PLOT.
      CALL PLOTL (XLO,XUP,SLO,SUP,LY) 
      DO 20 IX=IA,IB
      X=X1+(IX-1)*XINC
20    CALL PLOT1 (IFLAG,X,S1(IX,IY),S2(IX,IY),S3(IX,IY))
      CALL PLOTY ('(F10.0)')
      CALL PLOTX2 ('XGP=','(F8.0)')
C----------PRINT SECOND PLOT.
      CALL PLOTL (XLO,XUP,SLO,SUP,LY) 
      DO 21 IX=IA,IB
      X=X1+(IX-1)*XINC
21    CALL PLOT2 (IFLAG,X,S1(IX,IY),S2(IX,IY),S3(IX,IY))
      CALL PLOTY ('(F10.0)')
C----------GO TO NEXT Y GROUND POSITION.
      IF (NY.EQ.1) RETURN
      CALL IPICK (NY,IY,   Y1,YINC,'YGP     ')
      IF (IY.GT.0) GO TO 5
C----------DECIDE ON NEXT ACTION.
      PRINT *, 'DO YOU WISH TO PLOT IN THE Y DIRECTION?  (DEFAULT N)'
      Q='N'
      CALL HITQ (Q)
      IF (Q.EQ.'N') RETURN
C----------WE WILL PLOT IN Y DIRECTION.
222   YLO=Y1
      YUP=Y1+(NY-1)*YINC
      PRINT 2000, 'Y',NINT(YLO),NINT(YUP)
      CALL HITXX (YLO,YUP)
      IA=NINT((YLO-Y1)/YINC)+1
      IB=NINT((YUP-Y1)/YINC)+1
      IA=MIN0(NY,MAX0(IA,1))      
      IB=MIN0(NY,MAX0(IB,1))      
      YLO=Y1+(IA-1)*YINC
      YUP=Y1+(IB-1)*YINC
      IX=1
51    X=X1+(IX-1)*XINC                                      
      IF (IFLAG.EQ.1) PRINT 1001, 'XGP=',NINT(X)
      IF (IFLAG.EQ.2) PRINT 1002, 'XGP=',NINT(X)
      IF (IFLAG.EQ.3) PRINT 1003, 'XGP=',NINT(X)
C----------PRINT FIRST PLOT.
      CALL PLOTL (YLO,YUP,SLO,SUP,LY) 
      DO 30 IY=IA,IB
      Y=Y1+(IY-1)*YINC
30    CALL PLOT1 (-IFLAG,Y,S1(IX,IY),S2(IX,IY),S3(IX,IY))
      CALL PLOTY ('(F10.0)')
      CALL PLOTX2 ('YGP=','(F8.0)')
C----------PRINT SECOND PLOT.
      CALL PLOTL (YLO,YUP,SLO,SUP,LY) 
      DO 31 IY=IA,IB
      Y=Y1+(IY-1)*YINC
31    CALL PLOT2 (-IFLAG,Y,S1(IX,IY),S2(IX,IY),S3(IX,IY))
      CALL PLOTY ('(F10.0)')
C----------GO TO NEXT X GROUND POSITION.
      IF (NX.EQ.1) RETURN
      CALL IPICK (NX,IX,   X1,XINC,'XGP     ')
      IF (IX.GT.0) GO TO 51
C----------DECIDE ON NEXT ACTION.
      PRINT *, 'DO YOU WISH TO PLOT IN THE X DIRECTION?  (DEFAULT N)'
      Q='N'
      CALL HITQ (Q)
      IF (Q.EQ.'N') RETURN
      GO TO 111
C----------FORMAT STATEMENTS.
1001  FORMAT (15X,A4,I6,20X,'(OLD VALUES ON BOTTOM GRAPH)')
1002  FORMAT (15X,A4,I6,20X,'(SECOND FILE ON BOTTOM GRAPH)')
1003  FORMAT (15X,A4,I6,20X,'(COMBINED FILE ON BOTTOM GRAPH)')
2000  FORMAT (' TYPE MINIMUM AND MAXIMUM ',A1,' GP''S TO PLOT ',
     $   '(2 NUMBERS)  (DEFAULT',2I6,')')
      END





      SUBROUTINE PLOT1 (IFLAG,X,S1,S2,S3)
C     CALL PLOT1 TO ADD POINT TO FIRST PLOT.
C     CALL PLOT2 TO ADD POINT TO SECOND PLOT.
C     IFLAG=+-1: 1ST PLOT IS OLD VALUES, 2ND PLOT IS NEW VALUES.
C     IFLAG=+-2: 1ST PLOT IS FIRST FILE, 2ND PLOT IS SECOND FILE.
C     IFLAG=+-3: 1ST PLOT IS BOTH FILES, 2ND PLOT IS COMBINED FILE.
C     POSITIVE IFLAG USES CHARACTER X; NEGATIVE IFLAG USES CHARACTER Y.
C----------DIMENSION STATEMENTS.
      CHARACTER*2 CHAR,CHAR1,CHAR2
      PARAMETER (ZNIL=-1.E-30)
C----------FIRST PLOT.                       
      IF (IABS(IFLAG).LE.2) THEN
           CHAR='X'
           IF (IFLAG.LT.0) CHAR='Y'
           IF (S1.NE.S2) CHAR='E'
           IF (S1.NE.S2.AND.IABS(IFLAG).EQ.2) CHAR='1'
           IF (S1.EQ.ZNIL) CHAR='N'
           CALL PLOTXY (CHAR,X,S1)
      ELSE
           CHAR1='1B'
           CHAR2='2B'
           IF (S1.EQ.ZNIL) CHAR1='NN'
           IF (S2.EQ.ZNIL) CHAR2='NN'
           CALL PLOTXY (CHAR1,X,S1)
           CALL PLOTXY (CHAR2,X,S2)
      END IF
      RETURN
C----------SECOND PLOT.
      ENTRY PLOT2 (IFLAG,X,S1,S2,S3)
      CHAR='X'
      IF (IFLAG.LT.0) CHAR='Y'
      IF (IABS(IFLAG).LE.2) THEN
           IF (S1.NE.S2) CHAR='O'
           IF (S1.NE.S2.AND.IABS(IFLAG).EQ.2) CHAR='2'
           IF (S2.EQ.ZNIL) CHAR='N'
           CALL PLOTXY (CHAR,X,S2)
      ELSE
           IF (S3.EQ.S1) CHAR='1'
           IF (S3.EQ.S2) CHAR='2'
           IF (S3.EQ.S1.AND.S3.EQ.S2) CHAR='B'
           IF (S3.EQ.ZNIL) CHAR='N'
           CALL PLOTXY (CHAR,X,S3)
      END IF
      RETURN
      END


c  move (code)
c  hitxc3 (code)
c  hitc (code)
c  hiti (code)
c  hitq (code)
c  hitx (code)
c  hitx2 (code)
c  hit (code)
c  hitc2 (code)
c  hitxx (code)
c  rfile (code)
c  hitiiii2 (code)


      SUBROUTINE MOVE (A,B,N)
      DIMENSION A(N),B(N)
      DO 10 I=1,N
10    B(I)=A(I)
      RETURN
      END




      SUBROUTINE HIT
      CHARACTER*(*) C,C1,C2,C3
      CHARACTER*80 BUF
C----------READ CARRIAGE RETURN.
      PRINT *, '(HIT RETURN TO CONTINUE)'
      ENTRY HIT2
      READ (*,*)
      RETURN
C----------READ Y(YES) OR N(NO).
      ENTRY HITQ2 (C)
      PRINT *, 'DEFAULT=', C
      ENTRY HITQ (C)
      ASSIGN 20 TO JUMP
      GO TO 888
20    READ (BUF,'(A1)',ERR=777) C
      call convert_to_upper (c)
      IF (C.EQ.'Y'.OR.C.EQ.'N') RETURN
      PRINT *, 'TRY AGAIN  (Y OR N ALLOWED)'
      GO TO 888
C----------READ INTEGER.
      ENTRY HITI2 (I)
      PRINT *, 'DEFAULT=',I
      ENTRY HITI (I)
      ASSIGN 30 TO JUMP
      GO TO 888
30    READ (BUF,*,ERR=777) I
      RETURN
C----------READ FLOATING POINT NUMBER.
      ENTRY HITX2 (X)
      PRINT 1000, X
1000  FORMAT (' DEFAULT=',G13.5)
      ENTRY HITX (X)
      ASSIGN 40 TO JUMP
      GO TO 888
40    READ (BUF,*,ERR=777) X
      RETURN
C----------READ CHARACTER WORD.
      ENTRY HITC2 (C)
      PRINT *, 'DEFAULT=',C
      ENTRY HITC (C)
      ASSIGN 50 TO JUMP
      GO TO 888
50    READ (BUF,'(A)',ERR=777) C
      call convert_to_upper (c)
      RETURN
C----------READ CHARACTER WORD without converting to upper case.
      ENTRY HITC2_keep (C)
      PRINT *, 'DEFAULT=',C
      ENTRY HITC_keep (C)
      ASSIGN 51 TO JUMP
      GO TO 888
51    READ (BUF,'(A)',ERR=777) C
      RETURN
C----------READ TWO FLOATING POINT NUMBERS.
      ENTRY HITXX2 (X,Y)
      PRINT *, 'DEFAULT=',X,Y
      ENTRY HITXX (X,Y)
      ASSIGN 60 TO JUMP
      GO TO 888
60    READ (BUF,*,ERR=777) X,Y
      RETURN
C----------READ TWO INTEGERS.
      ENTRY HITII2 (I,J)
      PRINT *, 'DEFAULT=',I,J
      ENTRY HITII (I,J)
      ASSIGN 70 TO JUMP
      GO TO 888
70    READ (BUF,*,ERR=777) I,J
      RETURN
C----------READ FOUR INTEGERS.
      ENTRY HITIIII2 (I,J,K,L)
      PRINT *, 'DEFAULT=',I,J,K,L
      ENTRY HITIIII (I,J,K,L)
      ASSIGN 80 TO JUMP
      GO TO 888
80    READ (BUF,*,ERR=777) I,J,K,L
      RETURN
C----------READ FLOATING POINT NUMBER OR SELECTED CHARACTER.
C          RETURNS C=' ' IF DEFAULT IS TAKEN.
C          RETURNS C=C1 OR C2 OR C3 IF ONE OF THESE IS TYPED.
C          RETURNS C='.' (AND CHANGED X) IF NUMBER IS TYPED.
      ENTRY HITXC3 (X,C,C1,C2,C3)
      C=' '
      ASSIGN 90 TO JUMP
      GO TO 888
90    READ (BUF,*,ERR=91) XX
      X=XX
      C='.'
      RETURN
91    READ (BUF,'(A)',ERR=777) C
      call convert_to_upper (c)
      IF (C.NE.C1.AND.C.NE.C2.AND.C.NE.C3) GO TO 777
      RETURN
C----------READ A LINE FROM THE SCREEN.
777   PRINT *, 'ERROR - TRY AGAIN'
888   READ (*,'(A80)') BUF
      IF (BUF.EQ.' ') RETURN
      GO TO JUMP
      END




      SUBROUTINE RFILE (VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,
     $          X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S,NNNN,KKKK)
C     ISEP SUBROUTINE FOR READING STATIC FILES.
C     USES UNIT NUMBER 77 FOR ALL READS.
C     RETURNS VAXFILE=' ' IF NO READ WAS DONE.
C----------DIMENSION AND DATA STATEMENTS.
      PARAMETER (LFN=77,ZNIL=-1.E-30)
      DIMENSION S(*)
      CHARACTER*(*) VAXFILE
      CHARACTER*80 CARD(*)
      CHARACTER*25 DDDDTTTT
      CHARACTER*8 TYPE
c     DATA IIII/23719/   ! removed 4/12/95
C----------GET STATIC FILE NAME TO READ.
ccccccccccccccccccccccccccc      CALL LISTPREV1 (DDDDTTTT)
777   CLOSE (LFN)
      PRINT *, 'TYPE NAME OF CPS STATIC FILE TO READ',
     $   '  (OR HIT RETURN TO QUIT)'
      VAXFILE=' '
      READ (*,1000,END=111) VAXFILE
1000  FORMAT (A80)
      IF (VAXFILE.EQ.' ') RETURN
      IF (VAXFILE.EQ.'NONE'.or.vaxfile.eq.'none') GO TO 333
C----------READ STATIC FILE.
ccccccccccccccccccccccccccc      CALL OPENFILE (LFN,VAXFILE,'READ',*777)
      CALL STATRII (LFN,TYPE,NHX,NHY,NHX2,NHY2,
     $      X1,Y1,XINC,YINC,NX,NY,*777,NCARD)
      IF (NX*NY.GT.NNNN) THEN
           PRINT *, 'STATIC FILE TOO LARGE TO FIT IN MEMORY'
           GO TO 777
      END IF
C stm
      IF (NCARD.GT.KKKK-50) THEN
           PRINT*,'NCARD= ',NCARD
           PRINT*,'TOO MANY COMMENT CARDS - last ones lost'
           NCARD=KKKK-50
      ENDIF
C stm
      CALL STATRCC (LFN,CARD,NCARD,*777)
      CALL STATREAD (LFN,NX,NY,S,*777)
      NCARD=NCARD+1
      CARD(NCARD)='ISEP- '//DDDDTTTT//'READ '//VAXFILE
      CLOSE (LFN)
ccccccccccccccccccccccccccc      CALL LISTPREV3 ('FOUND   ','STATIC  ',VAXFILE)
111   RETURN
C----------GENERATE TEST STATIC FILE.
333   TYPE='RESID'
      NHX=35
      NHY=36
      NHX2=0
      NHY2=0
      X1=20.     
      Y1=30.
      XINC=2.
      YINC=3.
      NX=26
      NY=37
      CARD(1)='ISEP- TEST FILE GENERATED WITH RANDOM NUMBERS'
      NCARD=1
      DO 20 J=1,NY
      DO 20 I=1,NX
      INDEX=I+(J-1)*NX
c20   S(INDEX)=40.*(RAN(IIII)-0.5)  ! removed 4/12/95
20    S(INDEX)=40.*(RANDOM() -0.5)  ! added   4/12/95 to make ansi-standard
      RETURN
      END




