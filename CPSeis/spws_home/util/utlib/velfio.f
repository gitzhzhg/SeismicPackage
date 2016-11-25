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
C  Process name: VXZFINT
C        Author: R.S. DAY
C  Last revised: 90/8/19  DAY
C
C  Purpose:      INTERPOLATE VELOCITY FUNCTIONS AT CONTROL POINTS TO THE
C                REQUESTED OUTPUT POSITION.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE VXZFINT(JAT,XBAS,YBAS,VO,NVX,NVY,NPTS,NPNTR,
C      +           ZF,VF,XB,YB,VWRK)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C JAT.........INTEGER    IN       CURRENT ATTRIBUTE NUMBER
C XBAS........REAL       IN       COORDINATE TO INTERPOLATE TO.
C YBAS........REAL       IN       COORDINATE TO INTERPOLATE TO.
C VO()........REAL       OUT      VELOCITY VECTOR AT THE OUTPUT POINT.
C NVX.........INTEGER   IN       NUMBER OF UNIQUE X POINTS IN XB.
C NVY.........INTEGER   IN       NUMBER OF UNIQUE Y POINTS IN YB.
C NPTS(I).....INTEGER   IN       NUMBER OF VALUES IN FUNCTION I
C NPNTR(I)....INTEGER   IN       MEMORY LOCATION OF FUNCTION I
C ZF(J).......REAL      IN       Z-COORDINATES OF THE VELOCITY FUNCTIONS
C VF(J).......REAL      IN       VELOCITY VALUES OF THE FUNCTIONS. CAN
C                                CONTAIN MULTIPLE ATTRIBUTES.
C XB(I).......REAL      IN       SURFACE LOCATION OF VELOCITY FUNCTION
C YB(I).......REAL      IN       SURFACE LOCATION OF VELOCITY FUNCTION
C VWRK........REAL      IN       SCRATCH BUFFER OF SIZE 2*NZ
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. TAKES THE VELOCITY FUNCTION DATA AS INPUT AND RETURNS A VERTICAL
C    COLUMN OF THE DEPTH MODEL IN THE OUTPUT ARRAY VO.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C 90/08/16  DAY          CREATION DATE
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C      LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C-----------------------------------------------------------------------
C NAMED COMMONS :VXZBLD1
C
C EXTERNAL CALLS:SSTRANS3
C
C SUBROUTINES   :VXZFINT
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C\END DOC
CCC
      SUBROUTINE VXZFINT(JAT,XBAS,YBAS,VO,NVX,NVY,NPTS,NPNTR,
     +           ZF,VF,XB,YB,VWRK)
      CHARACTER  LFILE*80,INODE*16,IUSER*16,IFILE*80,OFILE*80
      COMMON/VXZBLD1/ NXG,XGINC,XGMIN,NYG,YGINC,YGMIN,
     +                NZG,ZGINC,ZGMIN,MOFF1,MOFF2,MOFF3,
     +                MOFF4,MOFF5,MOFF6,MOFF7,
     +                NATTR,IOVXZ,VELMIN,LREC,
     +                MTYPE,TRACES,PFLAG,MAXTR
     +                LFILE,INODE,IUSER,IFILE,OFILE,DFILE,ZPARM
      REAL     XB(*),YB(*),ZF(*),VF(*)
      REAL     VO(*),XBAS,YBAS,DZ,ZMIN
      REAL     W1(8),VWRK(*)
      INTEGER  JAT,NVX,NVY,JM1(8),NPTS(*),NPNTR(*),PFLAG
      SAVE     IPT,JPT,NAYBOR,W1,JM1
       NFUN=NVX*NVY
       DO 19 K=1,NVY
        JY=(K-1)*NVX + 1
        IF( YB(JY).GE. YBAS) THEN
        JPT=K
        GO TO 20
        END IF
 19    CONTINUE
       JPT=NVY+1
 20    CONTINUE
C
       DO 21 K=1,NVX
        IF(XB(K).GE.XBAS) THEN
        IPT=K
        GO TO 23
        END IF
 21    CONTINUE
       IPT=NVX+1
 23    CONTINUE
       JJ=MIN(JPT,NVY)
       II=MIN(IPT,NVX)
       KPT=(JJ-1)*NVX + II     !grid point of box corner containing xbas,ybas
C
C--Perform a weighted sum over nearest functions to
C--produce the 'average' velocity function at (xbas,ybas)
C--then interpolate to the fine dt time grid
C--VO........Velocity function interpolated to the requested basement
C--JM1(i)....Number of the i'th closest V-function to pt. (xbas,ybas)
C--W1(i).....Weighting factor for velocity function jm1(i)
C
      NAYBOR=1
      W1(1) =1.0
      JM1(1)=KPT
      IF(IPT.EQ.1) THEN
        IF(JPT.GT.1 .AND.JPT.LE.NVY) THEN
         NAYBOR=2
         JM1(2)=KPT-NVX
         DN=1.0/( YB(KPT) - YB(KPT-NVX) )
         S= YBAS - YB(KPT-NVX)
         R= YB(KPT)-YBAS
         W1(1) = S*DN
         W1(2) = R*DN
        END IF
      ELSE IF(IPT.LE.NVX) THEN
        DM=1.0/( XB(KPT) - XB(KPT-1) )
        P= XBAS - XB(KPT-1)
        Q= XB(KPT)-XBAS
        IF(JPT.EQ.1 .OR.JPT.GE.NVY+1) THEN
         NAYBOR=2
         JM1(2)=KPT-1
         W1(1) = P*DM
         W1(2) = Q*DM
        ELSE
         NAYBOR=4
         JM1(2)=KPT-1
         JM1(3)=JM1(2)-NVX
         JM1(4)=JM1(3)+1
         DXY=DM/( YB(KPT) - YB(KPT-NVX) )
         S= YBAS - YB(KPT-NVX)
         R= YB(KPT)-YBAS
         W1(1) = S*P*DXY
         W1(2) = Q*S*DXY
         W1(3) = Q*R*DXY
         W1(4) = P*R*DXY
        END IF
      ELSE
        IF(JPT.GT.1 .AND.JPT.LE.NVY) THEN
         NAYBOR=2
         JM1(2)=KPT-NVX
         DN=1.0/( YB(KPT) - YB(KPT-NVX) )
         S= YBAS - YB(KPT-NVX)
         R= YB(KPT)-YBAS
         W1(1) = S*DN
         W1(2) = R*DN
        END IF
      END IF
C
C
 29     CONTINUE
        DO 30 J=1,NZ            !Initialize vfun
 30     VO(J)=0.0
C
C Interpolate between neighboring functions.
C 1st interpolate to depth grid, then do spatial interpolation.
        IOP=2
        IF(MTYPE.LT.0) IOP=0
        NTOT=NPNTR(NFUN)+NPTS(NFUN)
        DO 37 I2=1,NAYBOR
         IFUN=NPNTR(JM1(I2))+1
         JFUN=IFUN+(JAT-1)*NTOT
         CALL SSTRANS3(IOP,NPTS(JM1(I2)),ZF(IFUN),VF(JFUN),NZ,VWRK,
     +                ZMIN,DZ)
         DO 38 I1=1,NZ
         VO(I1) = VO(I1) + VWRK(I1)*W1(I2)
 38      CONTINUE
 37     CONTINUE
C
      RETURN
      END
C***************************** COPYRIGHT NOTICE ********************************
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
C  Process name:  VELFIO_OP           : Open  input-output file
C                 VELFIO_CL           : Close input-output file
C                 VELFIO_RD           : Get Header & V-function
C                 VELFIO_WR           : Write header & V-function
C                 VELFIO_PR           : Set a print flag
C        Author:  R.S. DAY
C  Last revised:  93/06/28   Day
C       Purpose:  To manage CPS velocity function files for input and 
C                 output in an ansii standard way.
C-----------------------------------------------------------------------
C                             REVISION HISTORY
C 2) 93/06/28: R.S. Day, Created by modifying io_cpsvf.f in utlib
C 1) 92/05/22: TR Stoeckley, Change file extension VEL to lower case.
C-----------------------------------------------------------------------
C                             CALLING SEQUENCE
C      ISTAT = VELFIO_OP(LFN,FILE_NAME,IOSTAT,NVPP,NVF,NHX,NHY)
C      CALL VELFIO_CL(LFN,IOSTAT)
C      CALL VELFIO_RD(LFN,NVPP,VN,NVP,X,Y,CTVF,T,V,
C     +        PROJECT,LINE_NAME,REC_DATE,PROJ_DATE,USERID,COMMENT,
C     +        SV)
C      CALL VELFIO_WR(LFN,NVPP,VN,NVP,X,Y,CTVF,T,V,
C     +        PROJECT,LINE_NAME,REC_DATE,PROJ_DATE,USERID,COMMENT,
C     +        SV)
C      CALL VELFIO_PR(PRINT_ON) 
C
C LFN        - Fortran logical file number, 1-999; INTEGER.
C FILE_NAME  - Name of the velocity file for input or output.
C              On return FILE_NAME will contain the complete name
C              including all references to disks and directories
C              if the actual argument is of type CHARACTER.
C IOSTAT     - 'READ' OR 'WRITE'; CHARACTER.
C NVPP       - Number of points in each velocity pair; INTEGER. 
C              This parameter is returned by OPEN_CPS_VELFILE.
C              This parameter must be passed to all other calls.
C NVF        - Number of velocity functions in the file; INTEGER.
C              This parameter is returned by OPEN_CPS_VELFILE.
C VN         - Velocity function name; CHARACTER*8.
C NVP        - Number of pairs in the velocity function; INTEGER.
C X          - X coordinate of the velocity function; REAL.
C Y          - Y coordinate of the velocity function; REAL.
C NHX        - CPS trace header word containing X coord; INTEGER.
C NHY        - CPS trace header word containing Y coord; INTEGER.
C--------------- VELOCITY FUNCTION ARRAYS ------------------
C T(i)       - Array that holds 'TIME' values; REAL.
C V(i)       - Array that holds the 'VELOCITY' values; REAL.
C SV()       - Array that holds optional values when NVPP>2; REAL.
C              If NVPP>3, then SV will look like a multidimensional
C              array, ie. SV(NVPP,NVP).
C---------- OPTIONAL FUNCTION CHARACTERISTICS --------------
C CTVF       - Type of velocity function; CHARACTER*4.
C              If CTVF=' ', then default is 'VTRM' for time,RMS.
C              See VELF for other possibilities.
C PROJECT    - Project name; CHARACTER*10. Starts in column 40.
C LINE_NAME  - Line name; CHARACTER*10. Starts IN COLUMN 51
C REC_DATE   - Recording date; CHARACTER*5. 
C PROJ_DATE  - Project data; CHARACTER*5.
C COMMENT    - Comment; CHARACTER*15.
C-----------------------------------------------------------------------
C                                 NOTES
C 1. Entry CPSVF_PR should be called to tell these routines how to
C    communicate error messages. PRINT_ON=0 for print out and non-zero
C    to supress print out of messages.
C
C 2. VELFIO_OP will open the file and read/write the GLOBAL record card
C    which contains the NVPP,NVF,NHX,NHY parameters.
C    If NHX and NHY are not in the file, their values are returned as 0.
C    The following actions will happen if IOSTAT='READ':
C     If the file is already open and attached to LFN then it will be
C      rewound with no futher action, ie. NVF and NVPP are not reset.
C     If the file is already open, but not attached to LFN then an 
C       error return will be executed.
C     If the file does not exist then an error return will be executed.
C     If the GLOBAL record does not exist the file will be closed and
C      an error return will be executed.
C    The following actions will happen if IOSTAT='WRITE':
C     If the file already exists then an error return will be executed.
C
C 3. VELFIO_WR writes all the information for a single 
C    velocity function to a file. VELFIO_RD reads 1 function
C
C 4. Output files will always have a *GLOBAL header card.  The first
C    numeric field of this card is NVPP, the second is NVF, the
C    third is a (defunct) version number used by these routines, the
C    fourth is NHX, and the fifth is NHY.
C
C 5. Link to POGUN::USER1:[CFE.LIBS]UTLIB/L,EZEDLIB/L
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C\END DOC
      INTEGER FUNCTION VELFIO_OP(LFN,FILE_NAME,IOSTAT,NVPP,NVF,NHX,NHY)
        INTEGER       LFN,NVPP,NVF,NHX,NHY
        CHARACTER*(*) FILE_NAME ,IOSTAT
        CHARACTER FILE_NAME_NEW*120 ,LINE*120 ,MESSAGE*80 ,RFN*120
        CHARACTER*10 FMT     ! Added to remove non-ansi parts from formats.
        INTEGER VERSION
        LOGICAL PRTON
        LOGICAL EXIST ,ANSWER
        INTEGER PRINT_ON 
        COMMON /VELFLAG/ PRINT_ON

        VELFIO_OP= 0
        FILE_NAME_NEW=FILE_NAME
        CALL ADDEXT(FILE_NAME_NEW,'vel')         
        CALL FLNBC (FILE_NAME_NEW,NCIFN,ICHAR(' '))
        INQUIRE (FILE=FILE_NAME_NEW ,OPENED=ANSWER ,EXIST=EXIST 
     +  ,NUMBER=LFNINQ)
        LUSC = INDEX(FILE_NAME_NEW,';')
        IF (ANSWER) THEN
         IF      (IOSTAT(:1).EQ.'R' .AND. LFNINQ.EQ.LFN) THEN
          REWIND LFN
          READ (LFN,'(A80)',ERR=700,END=900) LINE
          MESSAGE = 'Previouly opened file rewound.'
          RETURN
         ELSE IF (IOSTAT(:1).EQ.'R') THEN
          VELFIO_OP= 2
          MESSAGE = 'velfio_op: File already opened to another LFN.'
          GOTO 50
         ELSE 
          VELFIO_OP= 2
          MESSAGE = 'velfio_op:You cannot write to that file.'
          GOTO 50
         ENDIF
        ENDIF 

        IF (IOSTAT(:1).EQ.'R' .AND. .NOT.EXIST) THEN
         VELFIO_OP= 2
         MESSAGE = 'velfio_op: File does not exist.'
         GOTO 50
        ELSE IF (IOSTAT(:1).NE.'R' .AND. EXIST .AND. LUSC.NE.0) THEN
         VELFIO_OP= 2
         MESSAGE = 'velfio_op: File already exists. No Overwrites.'
         GOTO 50
        ENDIF

        IF (IOSTAT(:1).EQ.'R') THEN
         OPEN (LFN,FILE=FILE_NAME_NEW,FORM='FORMATTED',STATUS='OLD')
         READ (LFN,'(A80)',ERR=700,END=900) LINE
         IF (LINE(:9).EQ.'*GLOBALS:') THEN
            NHX=0
            NHY=0
            READ (LINE(11:),'(5I10)',END=10) NVPP,NVF,VERSION,NHX,NHY
 10       CONTINUE
         ELSE
          CALL VELFIO_CL (LFN,IOSTAT)
          VELFIO_OP= 2
          MESSAGE = 'velfio_op: First card does not contain *GLOBALS:'
          GOTO 50
         ENDIF
        ELSE 
         OPEN (LFN,FILE=FILE_NAME_NEW,FORM='FORMATTED',STATUS='UNKNOWN',
     $         ERR=700)
         WRITE (LFN,'(''*GLOBALS: '',5I10)') NVPP,NVF,VERSION,NHX,NHY
        ENDIF 
        RETURN
 50     CONTINUE
        IF(PRINT_ON .EQ. 0) WRITE(6,*) MESSAGE
        RETURN
 700    CONTINUE
        VELFIO_OP = 1
        MESSAGE = 'velfio_op: error occurred during open or read'
        IF(PRINT_ON .EQ. 0) WRITE(6,*) MESSAGE
        RETURN
 900    CONTINUE
        VELFIO_OP = 2
        MESSAGE = 'velfio_op: end of file during read'
        IF(PRINT_ON .EQ. 0) WRITE(6,*) MESSAGE
        RETURN
        END
CCC
        SUBROUTINE VELFIO_RD(LFN,NVPP,VN,NVP,X,Y,CTVF,T,V,
     +        PROJECT,LINE_NAME,REC_DATE,PROJ_DATE,USERID,COMMENT,
     +        SV)
        INTEGER       LFN,NVPP,NVP,PRINT_ON
        REAL          SV(*),T(*),V(*),X,Y 
        CHARACTER*(*) VN,CTVF,USERID,COMMENT
        CHARACTER*(*) PROJECT,LINE_NAME,REC_DATE,PROJ_DATE
        CHARACTER     LINE*120,CTVFO*4,MESSAGE*120
        COMMON /VELFLAG/ PRINT_ON

        READ (LFN,'(A120)',ERR=700,END=900) LINE 
        NCPV = INDEX(LINE(9:),'V')
C       CALL FFOCH (LINE(9:),NCPV,'V')                   
        IF (NCPV.NE.0) THEN
         CTVF = LINE(9+NCPV-1:9+NCPV+2)
        ELSE
         CTVF = 'VTRM'
        ENDIF
        VN        = LINE(:8)
        PROJECT   = LINE(40:49)
        LINE_NAME = LINE(51:60)
        REC_DATE  = LINE(62:66)
        PROJ_DATE = LINE(68:72)
        USERID    = LINE(74:76)
        COMMENT   = LINE(78:92)
        READ (LINE(9:),'(1X,I4,1X,F9.1,1X,F9.1)',END=110) NVP,X,Y
 110    CONTINUE
        IF (NVPP.EQ.2) THEN
         READ (LFN,*,ERR=700,END=900) (T(I),V(I),I=1,NVP)
        ELSE
         J = NVPP-2
         READ (LFN,*,ERR=700,END=900) 
     +   (T(I),V(I),(SV(K+(I-1)*J),K=1,J),I=1,NVP)
        ENDIF
        RETURN
 900    CONTINUE
        REWIND LFN     !EOF on input.
        IERRSTAT = 2   
        MESSAGE = ' '  
        GOTO 990
 700    MESSAGE = 'VELFIO_RD: Error in reading velocity function.'
        IERRSTAT = 1
        GOTO 990
 990    CONTINUE 
        IF (MESSAGE(:1).NE.' '.AND.PRINT_ON .EQ.0) THEN  !No message if ' '.
          PRINT *,MESSAGE
        ENDIF
        IF (IERRSTAT.EQ.0) RETURN        !Normal return
        RETURN
        END
C
CCC********************************************************************
        SUBROUTINE VELFIO_WR(LFN,NVPP,VN,NVP,X,Y,CTVF,T,V,
     +        PROJECT,LINE_NAME,REC_DATE,PROJ_DATE,USERID,COMMENT,
     +        SV)
        INTEGER       LFN,NVPP,NVP,PRINT_ON
        REAL          SV(*),T(*),V(*),X,Y 
        CHARACTER*(*) VN ,CTVF,USERID,COMMENT
        CHARACTER*(*) PROJECT,LINE_NAME,REC_DATE,PROJ_DATE
        CHARACTER     LINE*120,CTVFO*4,FMT*12
        CHARACTER*13  CT(100),CV(100)
        COMMON /VELFLAG/ PRINT_ON
        IF (CTVF(:1).EQ.' ') THEN
         CTVFO = 'VTRM'
        ELSE
         CTVFO = CTVF
        ENDIF
        WRITE (LINE(9:),'(1X,I4,1X,F9.1,1X,F9.1,1X,A4)') NVP,X,Y,CTVFO
        LINE(:8)    = VN 
        LINE(39:39) = ' '
        LINE(40:)   = PROJECT
        LINE(51:)   = LINE_NAME
        LINE(62:)   = REC_DATE
        LINE(68:)   = PROJ_DATE
        LINE(74:)   = USERID
        LINE(78:)   = COMMENT
        WRITE (LFN,'(A120)',ERR=902) LINE

        DO 10 I=1,NVP
         VMAX = MAX( ABS(V(I)) ,VMAX )
 10     CONTINUE
C
C Convert array values to character
        MAXCT = 0
        MAXCV = 0
        DO 15 I=1,NVP
         CT(I) = '             '
         CV(I) = '             '
         CALL NUM2ALFR (T(I),CT(I),*902,NCT)
         IF (VMAX.GT.0) THEN
          CALL NUM2ALFI (NINT(V(I)),CV(I),*902,NCV)
         ELSE
          CALL NUM2ALFR (     V(I) ,CV(I),*902,NCV)
         ENDIF
         MAXCT = MAX( MAXCT ,NCT )
         MAXCV = MAX( MAXCV ,NCV )
 15     CONTINUE

        IF (NVPP.EQ.2) THEN
         NVPC  = 80/(MAXCT+MAXCV+3)
         WRITE (FMT,'(A1,I3,A4)') '(',NVPC,'(A))'
         WRITE (LFN,FMT,ERR=902) 
     +   (' '//CT(I  )(:MAXCT)//' '//CV(I  )(:MAXCV)//',',I=1,NVP-1),
     +    ' '//CT(NVP)(:MAXCT)//' '//CV(NVP)(:MAXCV)
        ELSE
         J = NVPP-2
         write (FMT,'(A1,I3,A6)') '(',j,'E13.5)'
         WRITE (LFN,FMT,ERR=902) 
     +   (' '//CT(I)(:MAXCT)//' '//CV(I)(:MAXCV)//', ',
     +   (SV(K+(I-1)*J),K=1,J),I=1,NVP)
        ENDIF
        RETURN
 902    CONTINUE
        IERRSTAT = 3
        LINE = 'VELFIO_WR: Error in writing velocity functions.'
        IF(PRINT_ON.EQ.0) WRITE(6,*) LINE
        CALL VELFIO_CL (LFN,IOSTAT)
        RETURN
        END
CCC
        SUBROUTINE VELFIO_CL (LFN,IOSTAT)
        INTEGER  LFN,IOSTAT
        CLOSE (UNIT=LFN ,STATUS='KEEP' ,IOSTAT=IOS)
        RETURN 
        END
CCC
        SUBROUTINE VELFIO_PR(PRTON)
        INTEGER PRTON,PRINT_ON
        COMMON /VELFLAG/ PRINT_ON
C PRINT_ON = 0 for print & non-zero 0 for no print
        DATA  PRINT_ON/1/ 
        PRINT_ON = PRTON
        RETURN
        END
