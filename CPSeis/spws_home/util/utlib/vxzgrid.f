C*******************************************************************************
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
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*******************************************************************************
C\USER DOC
C-----------------------------------------------------------------------
C                   INTERACTIVE CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name: VXZGRID
C        Author: R.S.DAY
C       Written: 90/07/09
C  Last revised: 91/05/31   Day
C
C  Purpose:      DEFINE A GRIDDED MODEL V(X,Z) ON A REGULAR 
C                OUTPUT GRID. THE MODEL IS SAVED IN A DISK FILE.
C                EACH RECORD IN THE DISK FILE CONTAINS NZ VALUES FOR ONE
C                OF THE X GRID VALUES.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE VXZGRID(PMIN,PMAX,LUNO,NX,DX,XMIN,NZ,DZ,ZMIN,ISO,WRK)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C PMIN........REAL      OUT      MIN VALUE OF ALL PARAMETERS
C PMAX........REAL      OUT      MAX VALUE OF ALL PARAMETERS
C LUNO........INTEGER   INPUT    LUN OF FORTRAN DIRECT ACCESS FILE.
C NX..........INTEGER   INPUT    NUMBER OF X GRID POINTS.
C DX..........REAL      INPUT    SIZE OF X GRID INTERVAL.
C XMIN........REAL      INPUT    X(I)=XMIN + (I-1)*DX
C NZ..........INTEGER   INPUT    NUMBER OF Z GRID POINTS.
C DZ..........REAL      INPUT    SIZE OF Z GRID INTERVAL.
C ZMIN........REAL      INPUT    Z(I)=ZMIN + (I-1)*DZ
C ISO.........INTEGER   OUTPUT   RETURN ERROR STATUS ( OK=0 )
C WRK().......REAL      INPUT    SCRATCH BUFFER SPACE (NZ+150 WORDS)
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. THE OUTPUT GRID IS DEFINED BY THE FOLLOWING PARAMETERS.
C    NX,DX,XMIN,NZ,DZ,ZMIN,NY,DY,YMIN
C
C 2. V(X,Z) MODELS ONLY.  IT IS ASSUMED THAT THE NAMED COMMON
C    CDEF1 CONTAINS INFORMATION ABOUT CELLS IN THE MODEL.
C    WE ARE GIVEN INFORMATION THAT DEFINES 1 OR MORE POLYGONAL
C    AREAS THAT FILL PART OR ALL OF THE MODEL SPACE DEFINED BY
C    THE OUTPUT GRID.
C    NCELL......NUMBER OF POLYGONS.
C    NXCELL.....NUMBER OF POINTS IN A POLYGON.
C    XCELL()....X COORDINATES OF THE POLYGON BOUNDARYS
C    YCELL()....Y COORDINATES OF THE POLYGON BOUNDARYS
C    IXCELL(I)..INDEX INTO ARRAYS XCELL AND YCELL FOR I'TH
C    POLYGON. (ADD 1 TO GET STARTING LOCATION)
C    NXCELL(I)..NUMBER OF COORDINATE PAIRS DEFINING THE I'TH
C               POLYGONAL REGION.
C
C 3. THE NAMED COMMON VDEF1 MUST ALSO CONTAIN INFORMATION ON THE
C    VELOCITY FUNCTIONS THAT WILL BE USED TO DEFINE THE VALUES OF
C    V(X,Z) AT THE GRID POINTS. 
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date     Author      Description
C     ----     ------      -----------
C 6.  91/05/31 DAY         VXZINTV modified.
C 5.  91/02/11 Day         Corrections made in VXZINTV. VDEF1 consistent
C 4.  91/01/25 Day         Changed from RAYWHERE to CELLWHER call.
C 3.  90/08/28 Day         Increased cdef 1 common size
C 2.  90/08/24 Day         Added conditioning to cell boundarys.
C 1.  90/07/09 Day         Original version
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C      LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C-----------------------------------------------------------------------
C NAMED COMMONS :CWSBUF          CDEF1           VDEF1
C
C EXTERNAL CALLS:CELLWHER        GETLUN          VXZUT1
C EXTERNAL CALLS:RETLUN
C
C SUBROUTINES   :VXZGRID
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C\END DOC
C*******************************************************************************
C\USER DOC
C-----------------------------------------------------------------------
C                   INTERACTIVE CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name: VXZUT1
C        Author:
C  Last revised:
C
C  Purpose:
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE VXZUT1(NX,DX,XMIN,NZ,DZ,ZMIN,XPOS,NTAB,IC,ZXING,NS,
C      +           VSEG,VRET,JAT)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C NX..........
C DX..........
C XMIN........
C NZ..........
C DZ..........
C ZMIN........
C XPOS........
C NTAB........
C IC..........
C ZXING.......
C NS..........
C VSEG........
C VRET........
C JAT.........
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C      LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C-----------------------------------------------------------------------
C NAMED COMMONS :CDEF1           VDEF1
C
C EXTERNAL CALLS:VXZUT2              VXZCOL
C
C SUBROUTINES   :VXZUT1
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C\END DOC
C*******************************************************************************
C\USER DOC
C-----------------------------------------------------------------------
C                   INTERACTIVE CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name: VXZUT2
C        Author:
C  Last revised:
C
C  Purpose:      HELPS VXZGRID COMPUTE A 2D GRIDDED DEPTH MODEL
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE VXZUT2(X,XV,ZV,NV, IC,ZC,NS,ZS)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C X...........
C XV..........
C ZV..........
C NV..........
C IC..........
C ZC..........
C NS..........
C ZS..........
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C      LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C-----------------------------------------------------------------------
C SUBROUTINES   : VXZUT2
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C\END DOC
C\USER DOC
C-----------------------------------------------------------------------
C                   INTERACTIVE CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name: VXZCOL
C        Author:
C  Last revised:
C
C  Purpose:      COMPUTE THE VELOCITY VALUES AT POSITION X0 FOR A
C                MATERIAL WITH ID NUMBER MID.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE VXZCOL(NZ,DZ,ZMIN, MID,JAT,VRET, X0,IC,ZC,NS,ZS)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C NZ..........
C DZ..........
C ZMIN........
C MID.........
C JAT.........
C VRET........
C X0..........
C IC..........
C ZC..........
C NS..........
C ZS..........
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C      LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C-----------------------------------------------------------------------
C EXTERNAL CALLS:VXZGETV             VXZINTV
C
C SUBROUTINES   :VXZCOL
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C\END DOC
C\USER DOC
C-----------------------------------------------------------------------
C                   INTERACTIVE CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name: VXZGETV
C        Author:
C  Last revised:
C
C  Purpose:      GET VELOCITY FUNCTION FOR MATERIAL ID MID.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE VXZGETV(MID,JAT,NCRD,ITYP,XV,ZV,VAL)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C MID.........
C JAT.........
C NCRD........
C ITYP........
C XV..........
C ZV..........
C VAL.........
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C      LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C-----------------------------------------------------------------------
C NAMED COMMONS :VDEF1
C
C SUBROUTINES   :VXZGETV
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C\END DOC
C\USER DOC
C-----------------------------------------------------------------------
C                   INTERACTIVE CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name: VXZINTV
C        Author:
C  Last revised:
C
C  Purpose:      COMPUTE VELOCITY VALUE AT (X0,Z0) FROM CONTROL DATA
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE VXZINTV(NZ,DZ,ZMIN, X0,Z0,Z1, NCRD,XV,ZV,VAL,ITYP,VRE
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C NZ..........
C DZ..........
C ZMIN........
C X0..........
C Z0..........
C Z1..........
C NCRD........
C XV..........
C ZV..........
C VAL.........
C ITYP........
C VRET........
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C      LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C-----------------------------------------------------------------------
C SUBROUTINES   :VXZINTV
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C\END DOC
CCC
C ALGORITHM  : FOR EACH POLYGON, DETERMINE IF ANY SEGMENTS OF THE
C              POLYGON STRADDLE THE CURRENT BASEMENT POSITION.
      SUBROUTINE VXZGRID(PMIN,PMAX,LUNO,NX,DX,XMIN,NZ,DZ,ZMIN,ISO,WRK)
c      INCLUDE 'VXXX.CMN'
      CHARACTER LINE*72
      PARAMETER (MAXC=99,MAXS=99,MAXP=2000,MAXA=99,MC=3500,MAXV=99)
      COMMON/CDEF1/   RLIMITS(4),NCELL,IXCELL(MAXA),NXCELL(MAXA),
     +                XCELL(MC),YCELL(MC),IDCELL(MC),
     +                KCRDS,CBUF(3*MAXA)
      COMMON/VDEF1/   JDOF,JCRDS,VBUF(8*MAXV),
     +                NVCLSS,CVAL(MAXV),CMEM(MAXV),INOCC(MAXV)
      DIMENSION       WRK(*),NTAB(99)
C
C CHECK THAT NECESSARY PARAMETERS ARE DEFINED AND IN RANGE.
C HAS A PROPER GRID AND MODEL WINDOW BEEN DEFINED?
      ISO=0
      IF(XMIN.LT.MIN(RLIMITS(1),RLIMITS(2))) ISO=ISO+1
      IF(ZMIN.LT.MIN(RLIMITS(3),RLIMITS(4))) ISO=ISO+1
      IF(DX.EQ.0 .OR. DZ.EQ.0) THEN
       ISO=ISO+1
       RETURN
      END IF
      NX=ABS(NX)
      XTES=XMIN + (NX-1)*DX
      XS=MIN(RLIMITS(1),RLIMITS(2))
      XB=MAX(RLIMITS(1),RLIMITS(2))
      IF(XS.EQ.XB)  ISO=ISO+1
      EPS=.0001*(XB-XS)
      IF(XTES.GT.XB .OR. XTES.LT.XS) THEN
        ISO=ISO+1
        RETURN
      END IF
      IF(NX.GT.5000 .OR. NZ.GT.2000) ISO=ISO+1
      NZ=ABS(NZ)
      ZTES=ZMIN + (NZ-1)*DZ
      ZS=MIN(RLIMITS(3),RLIMITS(4))
      ZB=MAX(RLIMITS(3),RLIMITS(4))
      IF(ZS.EQ.ZB)  ISO=ISO+1
      EPS=.0001*(ZB-ZS)
      IF(ZTES.GT.ZB .OR.ZTES.LT. ZS) THEN
        ISO=ISO+1
        RETURN
      END IF
C
C DOES THERE APPEAR TO BE DATA AVAILABLE FOR GRIDDING?
      IF(KCRDS.NE. NCELL .OR. KCRDS.EQ.0) ISO=ISO+1
      IF( JCRDS.EQ.0) ISO=ISO+1
      IF(ISO.NE.0) THEN
        RETURN
      END IF
C
C BUILD A TABLE BETWEEN SEQUENTIAL CELL NUMBER AND THE MATERIAL ID
C NTAB(I)=CELL NUMBER OF THE ITH SEQUENTIAL CELL
      XMAX=XMIN+(NX-1)*DX
      ZMAX=ZMIN+(NZ-1)*DZ
      GXMAX=MAX(XMAX,XMIN)
      GXMIN=MIN(XMIN,XMAX)
      GZMAX=MAX(ZMAX,ZMIN)
      GZMIN=MIN(ZMIN,ZMAX)
      DO 25 I=1,NCELL
        X0=CBUF(3*I-2)       !POINT INSIDE A CELL
        Z0=CBUF(3*I-1)       !POINT INSIDE A CELL
        MID=CBUF(3*I)        !MAT. ID FOR CELL CONTAINING X0,Z0.
        CALL CELLWHER(X0,Z0,IS0,NTAB(I),XS0,ZS0,RS0,NCELL,
     +        IXCELL,NXCELL,XCELL,YCELL,*150)
        
        DO 33 J=1,NXCELL(I)  !CONDITION THE BOUNDARY COORDINATES
         PROXX= ABS(  GXMAX-XCELL(J+IXCELL(I))  )
         IF(PROXX.LT. .5) XCELL(J+IXCELL(I))=GXMAX 
         PROXX= ABS(  XCELL(J+IXCELL(I)) - GXMIN )
         IF(PROXX.LT. .5) XCELL(J+IXCELL(I))=GXMIN
         PROXZ= ABS(  GZMAX-YCELL(J+IXCELL(I))  )
         IF(PROXZ.LT. .5) YCELL(J+IXCELL(I))=GZMAX 
         PROXZ= ABS(  YCELL(J+IXCELL(I)) - GZMIN )
         IF(PROXZ.LT. .5) YCELL(J+IXCELL(I))=GZMIN
 33     CONTINUE
 25   CONTINUE
C
C
C  LOOP OVER THE OUTPUT GRID LOCATIONS  XMIN.....XMIN+(NX-1)*DX
C  LOOP OVER POLYGONS   1....NCELL
C       FIND INTERSECTIONS WITH EACH POLYGON
      NCNT=0
      DO 100 J=1,JDOF
      PMIN=1.0E+20
      PMAX=-1.0E+10
      DO 90 IX=1,NX
        XPOS=XMIN+(IX-1)*DX
        NCNT=NCNT+1

        CALL VXZUT1(NX,DX,XMIN,NZ,DZ,ZMIN,XPOS,NTAB,IC,WRK(NZ+1),NS,
     +           WRK(NZ+101),WRK,J)
C
C SCAN THE COLUMN FOR MIN AND MAX PARAMETER VALUES.
        DO 80 K=1,NZ
         PMIN=MIN(PMIN,WRK(K))
         PMAX=MAX(PMAX,WRK(K))
 80     CONTINUE
         IF(PMIN.EQ.0) THEN
          WRITE(6,*) 'ERROR: COLUMN=',IX,' ROW=',K
          WRITE(6,*) 'ERROR: A ZERO VALUE WAS DETECTED IN V(X,Z)'
         END IF
        WRITE(LUNO,REC=NCNT)(WRK(K),K=1,NZ)
 90   CONTINUE  !COLUMN LOOP
 100  CONTINUE  !ATTRIBUTE OR D.O.F. LOOP
      RETURN
 150  CONTINUE
      ISO=1
      RETURN
      END
CCC
      SUBROUTINE VXZUT1(NX,DX,XMIN,NZ,DZ,ZMIN,XPOS,NTAB,IC,ZXING,NS,
     +           VSEG,VRET,JAT)
c      INCLUDE 'VXXX.CMN'
      PARAMETER (MAXC=99,MAXS=99,MAXP=2000,MAXA=99,MC=3500,MAXV=99)
      COMMON/CDEF1/   RLIMITS(4),NCELL,IXCELL(MAXA),NXCELL(MAXA),
     +                XCELL(MC),YCELL(MC),IDCELL(MC),
     +                KCRDS,CBUF(3*MAXA)
      COMMON/VDEF1/   JDOF,JCRDS,VBUF(8*MAXV),
     +                NVCLSS,CVAL(MAXV),CMEM(MAXV),INOCC(MAXV)
      DIMENSION       NTAB(*),VRET(*),ZXING(*),VSEG(*)

C
C INITIALIZE THE VECTOR WHICH STORES A COLUMN OF THE MODEL.
        DO 15 I=1,NZ
 15     VRET(I)=0
C
C LOOP OVER THE CELLS IN THE MODEL.
        DO 70 I=1,NCELL
         MID=CBUF(3*I)
         IC0=NTAB(I)           !CELL NUMBER CONSISTENT WITH MID
         JPNTR=IXCELL(IC0)+1
         NVERT=NXCELL(IC0)-1
         CALL VXZUT2(XPOS,XCELL(JPNTR),YCELL(JPNTR),NVERT,
     +   IC,ZXING,NS,VSEG)
         CALL VXZCOL(NZ,DZ,ZMIN, MID,JAT,VRET, XPOS,IC,ZXING,NS,VSEG)
 70     CONTINUE
C
        RETURN
        END
CCC
      SUBROUTINE VXZUT2(X,XV,ZV,NV, IC,ZC,NS,ZS)
      DIMENSION XV(*),ZV(*),ZC(*),ZS(*)
C PURPOSE :  VXZUT2 FINDS THE INTERSECTIONS BETWEEN A VERTICAL LINE
C            AT POSITION X AND A POLYGON AT THE VERTICES XV(),ZV().
C
C X..........X COORDINATE POSITION WE ARE AT. (USE SAME UNITS FOR XV )
C NV.........NUMBER OF VERTEX POINTS IN THE POLYGON UNDER STUDY
C XV().......X VERTEX POINTS DEFINING THE POLYGON
C ZV().......Z VERTEX POINTS DEFINING THE POLYGON
C IC.,.......NUMBER OF TIMES A LINE AT X CROSSES POLYGON SEGMENTS
C ZC(ic).....LIST OF Z-COORDINATE POINTS WHERE INTERSECTIONS OCCUR.
C            WILL BE ORDERED FROM SMALLEST TO LARGEST
C NS.........NUMBER OF VERTICAL POLYGON SEGMENTS HIT AT X
C ZS(ns).....NUMBER OF END POINTS OF VERTICAL SEGMENTS.
C            NS SHOULD BE EVEN. IC CAN BE 1 OR 2.
C            IC=1 IMPLIES WE ARE AT AN ACUTE ANGLE VERTEX.
C SEGMENT END POINTS AND VERTICAL SEGMENTS ARE SPECIAL CASES
C
C                 X10
C           X0 ...........X1
C                          .   X21
C                            .
C                   ............
C                  X3          X2
C                        X32
C
      IC=0
      NS=0
C
      IF (NV.LE.0) RETURN

      DO 50 J=1,NV
       J0=MOD(NV+J-2,NV) + 1
       J1=J
       J2=MOD(J,NV) + 1
       J3=MOD(J+1,NV) + 1
       X1=XV(J1)
       X2=XV(J2)

       IF( X1.LT.X2 ) THEN       ! X1 < X2
        IF(X.LT.X1 .OR. X.GT.X2) GO TO 50 !NO INTERSECTION
        X21=XV(J2)-XV(J1)
        IF(X.EQ.X1) THEN
         X10=XV(J1)-XV(J0)
         IF(X10.EQ.0) GO TO 50
         IC=IC+1
         ZC(IC)=ZV(J1)
        ELSE IF(X.EQ.X2) THEN
         X32=XV(J3)-XV(J2)
         IF(X32*X21.GE.0) GO TO 50
         IC=IC+1
         ZC(IC)=ZV(J2)
        ELSE
         IC=IC+1
         ALPHA = (X-X1)/(X21)
         ZC(IC)= ZV(J1) + ALPHA*(ZV(J2)-ZV(J1))
         END IF
        GO TO 50
       END IF

       IF(X2.LT.X1) THEN    ! X1 > X2
        IF(X.LT.X2 .OR. X.GT.X1) GO TO 50 !NO INTERSECTION
        X21=XV(J2)-XV(J1)
        IF(X.EQ.X1) THEN
         X10=XV(J1)-XV(J0)
         IF(X10.EQ.0) GO TO 50
         IC=IC+1
         ZC(IC)=ZV(J1)
        ELSE IF(X.EQ.X2) THEN
         X32=XV(J3)-XV(J2)
         IF(X32*X21.GE.0) GO TO 50
         IC=IC+1
         ZC(IC)=ZV(J2)
        ELSE
         IC=IC+1
         ALPHA=(X-X1)/(X21)
         ZC(IC)= ZV(J1) + ALPHA*(ZV(J2)-ZV(J1))
        END IF
        GO TO 50
       END IF

       IF(X1.EQ.X2) THEN    ! X1 = X2      !VERTICAL SEGMENT
        IF(X.NE.X1) GO TO 50               !NO INTERSECTION
        ZS(NS+1)=MIN(ZV(J1),ZV(J2))
        ZS(NS+2)=MAX(ZV(J1),ZV(J2))
        NS=NS+2
        X10=XV(J1)-XV(J0)
        X32=XV(J3)-XV(J2)
        IF(X10*X32.GE.0) GO TO 50
        IC=IC+1
        ZC(IC)=ZS(NS)
       END IF

 50    CONTINUE
C
C ORDER THE CROSSINGS FROM SMALLEST TO LARGEST
       IF(IC.LE.1) RETURN
       DO 60 I=1,IC
       DO 60 J=I+1,IC
        IF(ZC(J).LT.ZC(I)) THEN
        TMP=ZC(J)
        ZC(J)=ZC(I)
        ZC(I)=TMP
        END IF
 60    CONTINUE
C
      RETURN
      END
CCC
      SUBROUTINE VXZCOL(NZ,DZ,ZMIN, MID,JAT,VRET, X0,IC,ZC,NS,ZS)
      DIMENSION ZC(*),ZS(*),VRET(*)
      DIMENSION XV(20),ZV(20),VAL(20)
C PURPOSE:  VXZCOL SETS THE ELEMENTS OF AN ARRAY VRET. THE Ith
C           ELEMENT OF VRET IS THE GRIDDED VALUE ASSOCIATED WITH
C           THE Ith Z-GRID POINT OF THE MESH DEFINED BY NZ,DZ,ZMIN.
C NOTES  :  CALL VXZUT2 PRIOR TO VXZCOL SO IC,ZC,NS,ZS ARE SET.
C NZ.........DIMENSION OF THE Z GRID
C DZ.........Z-GRID SIZE
C ZMIN.......STARTING POINT ON Z GRID
C MID........MATERIAL ID FILLING THE POLYGON BEING PROCESSED.
C JAT........ATTRIBUTE NUMBER TO SET FOR THE POLYGON
C IC.,.......NUMBER OF TIMES A LINE AT X CROSSES POLYGON SEGMENTS
C ZC(ic).....LIST OF Z-COORDINATE POINTS WHERE INTERSECTIONS OCCUR
C NS.........NUMBER OF VERTICAL POLYGON SEGMENTS HIT AT X
C ZS(ns).....NUMBER OF END POINTS OF VERTICAL SEGMENTS. SHOULD BE EVEN.
C
C SET THE GRID POINTS FOR THE CURRENT POLYGON AND POSITION X
      IF(IC.GT. 0 .OR. NS.GT.0) THEN
        CALL VXZGETV(MID,JAT, NCRD,ITYP,XV,ZV,VAL)
        IF(NCRD.EQ.0) GO TO 86
      ELSE
        GO TO 85
      END IF
C
      IF(IC.LE.1) GO TO 72
      DO 70 J=1,IC,2
       CALL VXZINTV(NZ,DZ,ZMIN, X0,ZC(J),ZC(J+1),
     +      NCRD,XV,ZV,VAL,ITYP,VRET)
 70   CONTINUE
C
C PROCESS THE VERTICAL SEGMENTS FOR CURRENT POLYGON AND POSITION X.
 72   IF(NS.LE.1) GO TO 85
      DO 80 J=1,NS,2
       CALL VXZINTV(NZ,DZ,ZMIN, X0,ZS(J),ZS(J+1),
     +      NCRD,XV,ZV,VAL,ITYP,VRET)
 80   CONTINUE
 85   RETURN
 86   WRITE(6,*) 'NO CARDS FOUND FOR MID=',MID
      RETURN
      END
CCC
C PURPOSE :  RETURN PARAMETERS THAT ARE STORED IN THE NAMED COMMON BLOCK
C            VDEF1.
C  MID.......MATERIAL ID REQUESTED. INPUT
C  NCRD......NUMBER OF CARDS RETURNED
C  ITYP......MATERIAL TYPE RETURNED
C            <0 WHEN AN ERROR IS DETECTED
C            1...ISOTROPIC, 2....LATERAL VARIATIONS ONLY
C            3...VRTICAL VARIATIONS ONLY
C  XV()......X COORDINATE VALUES
C  ZV()......Z COORDINATE VALUES
C  VAL().....PARAMETER VALUE AT THE COORDINATES XV,ZV.
      SUBROUTINE VXZGETV(MID,JAT,NCRD,ITYP,XV,ZV,VAL)
c      INCLUDE 'VXXX.CMN'
      PARAMETER (MAXC=99,MAXS=99,MAXP=2500,MAXA=99,MC=2000,MAXV=99)
      COMMON/VDEF1/   JDOF,JCRDS,VBUF(8*MAXV),NVCLSS,CVAL(MAXV),
     +                CMEM(MAXV),INOCC(MAXV)
      DIMENSION       XV(*),ZV(*),VAL(*)
      NCRD=0
      ITYP=-1
      IF(JDOF .LE.0 .OR. JAT.GT.JDOF)  RETURN
      IF(JCRDS.LE.0)  RETURN

      DO 20 J=1,JCRDS
       MPNT=(J-1)*(4+JDOF)
       ID=VBUF(MPNT+3)
       IF(ID.EQ.MID) THEN
        NCRD=NCRD+1
        IF(NCRD.EQ.1) ITYPO    =NINT(VBUF(MPNT+4))
        XV(NCRD)=VBUF(MPNT+1)
        ZV(NCRD)=VBUF(MPNT+2)
        ITYP    =NINT(VBUF(MPNT+4))
        VAL(NCRD)=VBUF(MPNT+4+JAT)
        IF(ITYP.NE. ITYPO) GO TO 55
       END IF
 20   CONTINUE

      RETURN
 55   CONTINUE
      ITYP=-2
      RETURN
      END
CCC
      SUBROUTINE VXZINTV(NZ,DZ,ZMIN, X0,Z0,Z1, NCRD,XV,ZV,VAL,ITYP,VRET)
      DIMENSION VRET(*),XV(*),ZV(*),VAL(*)
C----------------------------------------------------------------------
C RETURN MATERIAL PARAMETERS AT THE GRID POSITIONS:
C     ( X0, Z= ZMIN + (I-1)DZ ) , 0<I<=NZ
C 
C NZ,DZ,ZMIN..DEFINES THE DEPTH  GRID
C NCRD.....NUMBER OF CARDS DEFINING THE VELOCITY FUNCTION.
C ITYP.....FLAG INDICATING THE TYPE OF VELOCITY FUNCTION.
C          1 implies constant velocity layer.
C          2 implies horizontal velocity changes.
C          3 implies vertical velocity changes.
C          4 implies x and z velocity variation.
C X0.......X COORDINATE WHERE THE FUNCTION IS TO BE EVALUATED.
C Z0,Z1....THE Z-INTERVAL WHERE THE VELOCITY FUNCTION IS TO BE
C          EVALUATED.
C XV().....X COORDINATE ARRAY WITH CONTROL POINTS
C ZV().....Z COORDINATE ARRAY WITH CONTROL POINTS
C VAL()....VALUES OF THE PARAMETERS AT THE CONTROL POINTS
C VRET()...ARRAY WITH THE PARAMETER VALUES  FOR THE NZ GRID POINTS.
C----------------------------------------------------------------------
C      IZ0=((Z0-ZMIN)/DZ) + 1
C      IZ0=NINT((Z0-ZMIN)/DZ) + 1
      ZZ0 = ((Z0-ZMIN)/DZ) + 1.
      ZZ1 = ((Z1-ZMIN)/DZ) + 1.
      IF(ZZ0.LE.ZZ1) THEN        !DAY 5/31/91
        IZ1=ZZ1
        IZ0=ZZ0
        IF(ZZ0.GE.2.0) IZ0=(ZZ0 + 1.)
      ELSE
        IZ0=ZZ0
        IZ1=ZZ1
        IF(ZZ1.GE.2.0) IZ1=(ZZ1+1.)
      END IF
      IZ0=MAX(1,IZ0)
      IZ0=MIN(NZ,IZ0)
C      IZ1=NINT((Z1-ZMIN)/DZ) + 1
C      IZ1=((Z1-ZMIN)/DZ) + 1
      IZ1=MAX(1,IZ1)
      IZ1=MIN(NZ,IZ1)
      INC=1
      IF(IZ0.GT.IZ1) THEN
        INC=-1
      END IF

      GO TO (20,30,40,50),ITYP
C
 20   DO 21 I=IZ0,IZ1,INC
 21   VRET(I)=VAL(1)            !CONSTANT ATTRIBUTES LAYER
      RETURN
C
 30   CONTINUE
      IL=1                         !DAY 5/31/91
      IR=NCRD
      IF(XV(1).GT.XV(NCRD)) THEN
       IL=NCRD
       IR=1
      END IF
      IF(X0.LE.XV(IL)) THEN      !LATERAL VARIATION OF ATTRIBUTES LAYER
        VT=VAL(IL)
      ELSE IF(X0.GE.XV(IR)) THEN
        VT=VAL(IR)
      ELSE
        DO 35 J=2,NCRD
C        IF(X0.LT.XV(J)) THEN                 
        IF( (X0-XV(J))*(X0-XV(J-1)).LE.0) THEN
          SPAN=XV(J)-XV(J-1)
          YING=(X0-XV(J-1))/SPAN
          VT=VAL(J-1)  + (VAL(J)-VAL(J-1))*YING
          GO TO 36
        END IF
 35   CONTINUE
      END IF
 36   DO 37 I=IZ0,IZ1,INC
 37   VRET(I)=VT                !SAME ATTRIBUTE FOR ALL Z
      RETURN
C
 40   SPANI=1.0/(ZV(2)-ZV(1))   !VERTICAL ATTRIBUTE VARIATION ONLY
      DO 42 IZ=IZ0,IZ1,INC
        ZVAL=ZMIN + (IZ-1)*DZ   !R DAY 2/11/91
        YING=(ZVAL- ZV(1))*SPANI
        YING=MIN(1.0,YING)
        YING=MAX(0.0,YING)
        VRET(IZ)=VAL(1) + (VAL(2)-VAL(1))*YING
 42   CONTINUE
      RETURN
C
 50   CONTINUE                  !X & Z ATTRIBUTE CHANGES. 4 CONTROL POINTS.
      IF(NCRD.NE.4) THEN
       RETURN
      END IF
      IF(X0.LE.XV(1)) THEN      !USE POINTS 1 & 4
        VT  =VAL(1)
        VB  =VAL(2)
        SPAN=ZV(2)-ZV(1)
        ZB  =ZV(2)
      ELSE IF(X0.GE.XV(4) ) THEN
        VT  =VAL(4)
        VB  =VAL(3)
        SPAN=ZV(4)-ZV(3)
        ZB  =ZV(3)
      ELSE
        J2=J+J-1
        JJ=J+J-1
        YING=(X0-XV(1))/(XV(4)-XV(1))
        VT=VAL(1) + YING*(VAL(4)-VAL(1))
        ZT=ZV(1 ) + (ZV(4)-ZV(1))*YING
        VB=VAL(2) + YING*(VAL(3 )-VAL(2))
        ZB=ZV(2)  + (ZV(3)-ZV(2))*YING
        SPAN=1.0/(ZT-ZB)       !R DAY 2/11/91
      END IF
 56   DO 58 IZ=IZ0,IZ1,INC
        ZVAL=ZMIN + (IZ-1)*DZ  !R DAY 2/11/91
        YING=(ZVAL-ZB)/SPAN
        YING=MIN(1.0,YING)
        YING=MAX(0.0,YING)
        VRET(IZ)=VB + YING*(VT-VB)
 58   CONTINUE
      RETURN
      END
