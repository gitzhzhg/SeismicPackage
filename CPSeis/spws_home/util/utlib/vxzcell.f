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
C  Process name: VXZCELL
C        Author: R.S.DAY
C       Written: 90/07/11
C  Last revised: 91/10/03   Day
C
C  Purpose:      USED TO HELP CONTRUCT DEPTH MODELS.
C                TAKES DIGITIZED HORIZON INFORMATION AND IDENTIFIES
C                THE BOUNDARYS OF CLOSED CELLULAR REGIONS IN A MODEL.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE VXZCELL(ISTAT,MWRK,WRK)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C ISTAT.......INTEGER   OUTPUT   RETURN ERROR STATUS
C MWRK........INTEGER   INPUT    SIZE OF WORKING BUFFER SPACE
C WRK.........REAL      INPUT    WORKING BUFFER OF SIZE MWRK
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. DETERMINE THE X,Z POINTS THAT MAKE UP CELL BOUNDARYS.
C    WILL PLACE INFORMATION ABOUT THE CELLS INTO CDEF1.
C    INFORMATION FROM /GMF1/ IS USED TO COMPUTE DATA FOR /CDEF1/
C 2. FOLLOW VXZCELL BY A CALL TO VXZGRID
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date     Author     Description
C     ----     ------     -----------
C 4.  91/10/03 Day        Argument list of CELL was changed.
C 3.  90/08/28 Day        Increased size of common cdef1
C 2.  90/08/24 Day        Increased tolerance factor in call to cell
C 1.  90/07/11 Day        Original Version
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. MAKES USE OF DOUG HANSONS CELL SUBROUTINE
C    UPDATE INFORMATION IN ALL ARRAYS AFTER WE RETURN FROM CELL
C-----------------------------------------------------------------------
C      LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C-----------------------------------------------------------------------
C NAMED COMMONS :VDEF1           CDEF1           GMF1        GMF2
C
C EXTERNAL CALLS:CELL
C
C SUBROUTINES   :VXZCELL
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C\END DOC
CCC
      SUBROUTINE VXZCELL(ISTAT,MWRK,WRK)
      PARAMETER (MAXC=99,MAXS=99,MAXP=2000,MAXA=99,MC=3500,MAXV=99)
      INTEGER         CLID,CMEM,CVAL
      CHARACTER       CLLAB*30,LINE*80
      DIMENSION       WRK(*)
      COMMON/CDEF1/   RLIM(4),NCELL,IXCELL(MAXA),NXCELL(MAXA),
     +                XCELL(MC),YCELL(MC),IDCELL(MC),
     +                KCRDS,CBUF(3*MAXA)
      COMMON/GMF1/    NCLASS,CLID(MAXC),NSUBCL(MAXC),
     +                NSEG,NPTSUBCL(MAXS),NPNTR(MAXS),IBTYP(MAXS),
     +                NUM,NATTR,X1(MAXP),Y1(MAXP),A1(2*MAXP)
      COMMON/GMF2/    CLLAB(MAXC)
C
      ISTAT=0
      RTOL=.0001*( ABS(RLIM(2)-RLIM(1)) )
      IPRINT=1
      LPRINT=6
      NSEG=0
      DO 22 I=1,NCLASS
      DO 19 J=1,NSUBCL(I)
        NSEG=NSEG+1
        IBTYP(NSEG)=CLID(I)
 19   CONTINUE
 22   CONTINUE
      IF(NSEG.EQ.0) THEN
        IERR=-1
        LINE='VXZCELL: NO SEGMENTS TO PROCESS?  '
        GO TO 1001
      END IF
      IF(RLIM(1).EQ.RLIM(2)) THEN
        IERR=-2
        LINE='VXZCELL: NO MODEL WINDOW DEFINED '
        GO TO 1001
      END IF
      CALL CELL(RTOL,RLIM(1),RLIM(2),RLIM(3),RLIM(4)
     +,NSEG,IBTYP,NPNTR,NPTSUBCL,X1,Y1
     +,MAXA,MC,NCELL,IXCELL,NXCELL,XCELL,YCELL,IDCELL
     +,MWRK,WRK,IPRINT,LPRINT,IERR,*1001)
      NCLASS=1
      CLID(NCLASS)=IBTYP(1)
      NSUBCL(NCLASS)=1
      DO 33 M=2,NSEG
        IF(IBTYP(M).NE.IBTYP(M-1)) THEN
         NCLASS=NCLASS+1
         CLID(NCLASS)=IBTYP(M)
         NSUBCL(NCLASS)=1
        ELSE
         NSUBCL(NCLASS)=NSUBCL(NCLASS)+1
        END IF
 33   CONTINUE
      RETURN
C
1001  CONTINUE
      ISTAT=IERR
      WRITE(6,*) LINE,IERR
      
      RETURN
      END
