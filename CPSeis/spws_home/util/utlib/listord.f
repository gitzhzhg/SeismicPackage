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
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C\USER DOC
C-----------------------------------------------------------------------
C                         CRAY PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C                        C P S   P R I M I T I V E
C
CPrimitive name:  LISTORD, LISTGRD, LISTSRT
C        Author:  JB Sinton, Paul Hauge, Richard Day
C  Last revised:  89/0922
C
C  Purpose:  
C   1. LISTORD: Returns two pointer arrays for sorting a data set.
C               Sorting is based on the (X,Y) coordinates of the 
C               data set.  This routine can also complete the 
C               LISTGRD function.
C   2. LISTGRD: Returns the coordinates of the rectangular grid 
C               which encompasses the data set.
C   3. LISTSRT: Sort a data set, in place, given the two pointer
C               arrays.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C         
C         CALL LISTORD (IOPT, NFUN, IER, XBF,YBF, NFX,NFY, LIST,LINV, 
C        +               FX,FY)
C         CALL LISTORD (IOPT, NFUN, IER, XBF,YBF, NFX,NFY, LIST,LINV, 
C        +               FX,FY)
C         CALL LISTSRT (IOPT, NFUN, IER, LIST,LINV, VALUE, NV, WORK)
C
C Name    Type*  Valid  Description         *Type: I=IN, O=OUT, B=BOTH
C ----    ----   -----  -----------
C IOPT     I      INT   This parameter is used by LISTORD and LISTGRD
C                       only.  LISTSRT does not use this argument.
C                       IOPT controls what LISTORD returns.
C                       =1, return LIST in accending order.
C                       =2, return LIST, FX, and FY in accending order.
C                       =3, return LIST in decending order.
C                       =4, return LIST, FX, and FY in decending order.
C                       <0, return LINV also.
C                       IOPT controls how the grid search is started
C                       in LISTGRD (or LISTORD).
C                       <3, then XBF and YBF are initially assumed 
C                           ordered increasing.
C                       >2, then XBF and YBF are initially assumed 
C                           ordered decreasing.
C                       If the initial ordering does not result in
C                       a rectangular grid, the opposite ordering is
C                       tried.  If neither ordering results in a 
C                       rectangular grid, IER will be set (see below).
C NFUN     I     INT>0  Number of values in lists XBF and YBF, and
C                       the number of functions in two dimensional
C                       array VALUE.
C IER      O      INT   Return status.  For all values of IER>0
C                       control will return to the calling program
C                       at the error, so returned values are not 
C                       complete.
C                       =0, then operations completed without error.
C                       =1, then LINV could not be properly built.
C                       =2, then XBF's not increasing.
C                       =3, then XBF's not on a rectangular grid.
C                       =4, then number of functions not representative
C                        of a rectangular grid.
C                       =5, then XBF's different for each YBF.
C                       =6, I don't understand your data!
C XBF      I      REAL  List of X coordinates to sort, length NFUN.
C YBF      I      REAL  List of Y coordinates to sort, length NFUN.
C NFX      O      INT   Number of X coordinates in the grid.
C NFY      O      INT   Number of Y coordinates in the grid.
C LIST     B      INT   Array of pointers that will sort the XBFs and
C                       YBFs into increasing order. The XBFs will be
C                       varying most rapidly.
C LINV     B      INT   Array of pointers that will unsort the sorted
C                       XBFs and YBFs.
C FX       O      REAL  Array of X coordinates of the rectangular grid.
C FY       O      REAL  Array of Y coordinates of the rectangular grid.
C VALUE    B     I or R A two dimensional array sort.
C NV       I     INT>0  Number of values in the first dimension of
C                       array VALUE.
C WORK     B     I or R Work array of size NV.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. Use the routine LISTORD to compute the pointers for ordering an
C    input set of (X,Y) coordinates.  These (X,Y) coordinates can belong
C    to any set of functions.  Let's say that you have an array of data
C    A(10,20) for which you also have coordinates XBF(20) and YBF(20). 
C    Array A contains 10 values for each (X,Y) coordinate.  To sort the 
C    functions in A so that the X and Y coordinates are in increasing or
C    decreasing order call LISTORD to obtain LIST, and LINV.  Then call 
C    LISTSRT to sort A.  the calling sequence is:
C
C        CALL LISTORD(-2,20,IER,XBF,YBF,NFX,NFY,LIST,LINV,FX,FY)
C        CALL LISTSRT(0,20,IER,LIST,LINV,A,10,WORK)
C
C    In this example ABS(IOPT) = 2, so FX and FY are return with the 
C    grid coordinates.  LISTORD differs from LISTGRD in that XBF and 
C    YBF do not need to order, since LISTORD will compute the sorting 
C    necessary to order XBF and YBF.
C    
C 2. Routine LISTGRD may be called to return the grid coordinates.  It
C    assumes that the arrays XBF, and YBF are already in increasing or
C    decreasing order.  Array LIST is used, but LINV is ignored. 
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C 89/09/22  JB Sinton    Added IOPT=1-4 options.
C 89/08/28  JB Sinton    Fixed bug in LISTSRT.
C 89/06/29  JB Sinton    Fixed bug in LISTSRT.
C 89/06/13  JB Sinton    First version modified from HADC.
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES IN THIS MODULE
C
C  ENTRYS: LISTORD, LISTGRD, LISTSRT
C-----------------------------------------------------------------------
C                  EXTERNALS REFERENCED BY THIS MODULE
C
C NONE
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C
C  STORAGE       - none
C  HEAP(dynamic) - none
C-----------------------------------------------------------------------
C\END DOC
      SUBROUTINE LISTORD( IOPT, NFUN, IER, XBF,YBF, NFX,NFY, LIST,LINV, 
     +                    FX,FY )
C
C
C           RE- ORDER LOGIC
C
C       Patterned after R. Day's logic in " STRETCH " .
C
      DIMENSION    LIST(*), LINV(*), XBF(*), YBF(*) ,FX(*),FY(*)
      DIMENSION    VALUE(NV,NFUN), WORK(*)
C
      IER  =  0
      JOPT = IOPT
      IF (ABS(JOPT).LT.1 .OR. ABS(JOPT).GT.4) GOTO 999
      DO 1 K=1,NFUN
       LIST(K)=K
  1   CONTINUE
C
C   Reorder the velocity functions by basement order.
C   ybf controls the slow dimension, and xbf controls
C   the fast dimension for the sort. Make a list of
C   the sorted order and save XBF,YBF, and VRVA in a
C   scratch area.
C
      IF (ABS(JOPT).LT.3) THEN
       DO 3 J=1,NFUN
        DO 2 I=J,NFUN
         IF ( YBF(LIST(J)).GT.YBF(LIST(I)) ) THEN
          ITMP=LIST(J)
          LIST(J)=LIST(I)
          LIST(I)=ITMP
         ELSEIF ( YBF(LIST(J)).EQ.YBF(LIST(I)) ) THEN
          IF (XBF(LIST(J)) .GT. XBF(LIST(I)) ) THEN
           ITMP=LIST(J)
           LIST(J)=LIST(I)
           LIST(I)=ITMP
          ENDIF
         ENDIF
  2     CONTINUE
  3    CONTINUE
      ELSE
       DO 5 J=1,NFUN
        DO 4 I=J,NFUN
         IF ( YBF(LIST(J)).LT.YBF(LIST(I)) ) THEN
          ITMP=LIST(J)
          LIST(J)=LIST(I)
          LIST(I)=ITMP
         ELSEIF ( YBF(LIST(J)).EQ.YBF(LIST(I)) ) THEN
          IF (XBF(LIST(J)) .LT. XBF(LIST(I)) ) THEN
           ITMP=LIST(J)
           LIST(J)=LIST(I)
           LIST(I)=ITMP
          ENDIF
         ENDIF
  4     CONTINUE
  5    CONTINUE
      ENDIF
C
C     LIST( J ) has now been rearranged.
C           We also need the inverse array, LINV( J )
C
      IF (JOPT.LT.0) THEN
       DO  110  J = 1,NFUN
        DO  100 I = 1,NFUN
         IF( LIST(I) .EQ. J ) GO TO 105
  100   CONTINUE
        IER = 1
  105   CONTINUE
        LINV( J ) = I
  110  CONTINUE
       IF (IER.NE.0) RETURN
      ENDIF
C
C
C   --   Now do thorough check of Basement values in X and Y direction
C            to see if they lie on semi-regular grid.
C 
      IPASS= 0
      GOTO 200
      ENTRY LISTGRD (IOPT, NFUN, IER, XBF,YBF, NFX,NFY, LIST,LINV,
     +               FX,FY)
      JOPT = MAX( 2, ABS(IOPT) )
      IF (JOPT.GE.3) JOPT = 4
      IPASS= 0
      DO 150 I=1,NFUN
        LIST(I) = I
 150  CONTINUE
 200  CONTINUE
      IER  = 0
      NFX  =  0
      NFY  =  0
      IX   =  1
      IY   =  1
      J1   =  LIST( 1 )
      Y0   =  YBF( J1 )
      J2   =  J1
      IF     (ABS(JOPT).LT.1) THEN
       GOTO 999
      ELSEIF (ABS(JOPT).EQ.1 .OR. ABS(JOPT).EQ.3) THEN
       RETURN
      ELSEIF (ABS(JOPT).EQ.2) THEN
       DO  220  I = 2,NFUN
        J  =  LIST( I )
        J1 =  LIST(I-1)
        IF( YBF(J) .EQ. Y0 ) THEN
          IF(XBF(J) .GT. XBF(J1)) THEN
            IX = IX + 1
          ELSE
            IER  =  2
            GO TO 399
          ENDIF
          IF( IY .GE. 2 ) THEN
            J2  =  LIST( I - NFX  )
            IF(XBF(J) .EQ. XBF(J2)) THEN
               CONTINUE
            ELSE           !   Error in X values of CDP
              IER = 3
              GO TO 399
            ENDIF
          ENDIF
        ELSE IF(YBF(J).GT.Y0) THEN  !  1-st element in next row.
          IF( IY .EQ. 1) THEN
            NFX  =  IX
            NFY  =  NFUN / NFX
            NTM  =  NFX * NFY
            IF( NTM .NE. NFUN ) THEN
              IER  =  4
              GO TO 399
            ENDIF
            DO 210 K=1,NFX
             FX(K) = XBF(LIST(K))
 210        CONTINUE
          ELSE IF( IX .NE. NFX ) THEN
            IER  =  5
            GO TO 399
          ENDIF
          FY (IY) = Y0
          IX  =  1 
          IY  =  IY + 1
          Y0  =  YBF( J ) 
        ELSE
          IER  =  6
          GO TO 399
        ENDIF
 220   CONTINUE
      ELSE 
       DO  240  I = 2,NFUN
        J  =  LIST( I )
        J1 =  LIST(I-1)
        IF( YBF(J) .EQ. Y0 ) THEN
          IF(XBF(J) .LT. XBF(J1)) THEN
            IX = IX + 1
          ELSE
            IER  =  2
            GO TO 399
          ENDIF
          IF( IY .GE. 2 ) THEN
            J2  =  LIST( I - NFX  )
            IF(XBF(J) .EQ. XBF(J2)) THEN
               CONTINUE
            ELSE           !   Error in X values of CDP
              IER = 3
              GO TO 399
            ENDIF
          ENDIF
        ELSE IF(YBF(J).LT.Y0) THEN  !  1-st element in next row.
          IF( IY .EQ. 1) THEN
            NFX  =  IX
            NFY  =  NFUN / NFX
            NTM  =  NFX * NFY
            IF( NTM .NE. NFUN ) THEN
              IER  =  4
              GO TO 399
            ENDIF
            DO 230 K=1,NFX
             FX(K) = XBF(LIST(K))
 230        CONTINUE
          ELSE IF( IX .NE. NFX ) THEN
            IER  =  5
            GO TO 399
          ENDIF
          FY (IY) = Y0
          IX  =  1 
          IY  =  IY + 1
          Y0  =  YBF( J ) 
        ELSE
          IER  =  6
          GO TO 399
        ENDIF
 240   CONTINUE
      ENDIF
C
C  --  Above loop does not define NFX and NFY when all YFBs are equal.
C        So handle this case here.
C
      IF( NFX .EQ. 0) THEN
       NFX  = NFUN
       NFY  =  1
       FY(1) = YBF(1)
       DO 390 K=1,NFX
        FX(K) = XBF(LIST(K))
 390   CONTINUE
      ELSE
       FY(NFY) = YBF(J)
      ENDIF
      RETURN
 399  CONTINUE
      IPASS = IPASS+1
      IF (JOPT.LT.3) THEN
        JOPT = 4
      ELSE
        JOPT = 2
      ENDIF
      IF (IPASS.EQ.1) GOTO 200
      RETURN
C
C  --  This entry point will do an inplace sort of the values in array
C      VALUE given LIST and LINV. 
C
      ENTRY LISTSRT (IOPT, NFUN, IER, LIST,LINV, VALUE, NV, WORK)
      IER = 0
      INEW = 0
      DO 400 I=NFUN,1,-1
       IF (LIST(I).EQ.I) THEN
        LIST(I) = -LIST(I)
       ELSE
        INEW = I
       ENDIF
  400 CONTINUE
      IF (INEW.NE.0) THEN
       DO 410 J=1,NV
  410  WORK(J) = VALUE(J,LIST(INEW))
       K = 0
       IUSED = 0       
  420  CONTINUE
        K = K+1
        IOLD = LIST(INEW)
        IF (IOLD.GT.0) THEN
         DO 430 J=1,NV 
          VSAVE = VALUE(J,INEW) 
          VALUE(J,INEW) = WORK(J)
          WORK(J) = VSAVE
  430    CONTINUE
         LIST(INEW) = -LIST(INEW)
         IUSED = IUSED+1
         INEW = LINV(INEW)
        ELSE
         INEW = INEW+1
         IF (INEW.GT.NFUN) GOTO 450
         IF (LIST(INEW).GT.0) THEN
          DO 440 J=1,NV
 440      WORK(J) = VALUE(J,LIST(INEW))
         ENDIF
        ENDIF
        IF (IUSED.EQ.NFUN) GOTO 450
       GOTO 420
 450   CONTINUE
      ENDIF
      DO 460 I=1,NFUN
 460  LIST(I) = ABS(LIST(I))
      RETURN
 999  CONTINUE
      PRINT*,' =>LISTORD: IOPT=',JOPT,' is not valid: ABS(IOPT) must be 
     +1,2,3 or 4.'
      IER = 1
      RETURN
      END 
