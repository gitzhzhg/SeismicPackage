C***********************************************************************
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
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                 C P S   S U P P O R T  R O U T I N E S
C    Primitive name:  See below
C  Source directory:  ~spws/cell/   file cellwrap.f
C           Library:  libcell.a
C           Written:  92/10/12   by:  R.S.Day
C      Last revised:             by:
C
C  Purpose: The routines in this file are designed to be an interface
C           between some C and Fortran language routines.
C   The file cell.f was written by Doug Hanson. 
C - The C-language routines are in the file .
C-----------------------------------------------------------------------
C ROUTINES defined in the current file(wrapper routines)
C     CELL_W(...)
C     CELLWHER_W(..)
C     CELLPNT_W(...)
C     CELLPOLY_W(..)
C See documentation for cell routines by D. Hanson
C
C-----------------------------------------------------------------------
C\END DOC
C**********************************************************************/
      SUBROUTINE CELL_W(XMIN,XMAX,ZMIN,ZMAX,
     + NB,IXB,NXB,XB,ZB,
     +MC,MXC,NC,IXC,NXC,XC,ZC,MWORK,WORK,ISTAT)
      REAL          XMIN,XMAX,ZMIN,ZMAX
      REAL          XB(*),ZB(*),XC(*),ZC(*),WORK(*)
      INTEGER       NB,IXB(*),NXB(*)
      INTEGER       IXC(*),NXC(*)
      INTEGER       MC,MXC,NC,ISTAT
      ISTAT= 0
      CALL CELL(XMIN,XMAX,ZMIN,ZMAX,
     + NB,IXB,NXB,XB,ZB,
     + MC,MXC,NC,IXC,NXC,XC,ZC,MWORK,WORK,ISTAT)
C
      RETURN
      END

      SUBROUTINE CELLWHER_W(X0,Z0,IS0,IC0,XS0,ZS0,RS0,
     + NC,IXC,NXC,XC,ZC,ISTAT)
      REAL          X0,Z0,XS0,ZS0,RS0
      REAL          XC(*),ZC(*)
      INTEGER       NC,IXC(*),NXC(*),IS0,IC0
      INTEGER       ISTAT
      ISTAT = 1
      CALL CELLWHER(X0,Z0,IS0,IC0,XS0,ZS0,RS0,
     + NC,IXC,NXC,XC,ZC,*90)
      ISTAT = 0
      RETURN
 90   CONTINUE
      RETURN
      END

      SUBROUTINE CELLPNT_W(X0,Z0,NX,X,Z,INS,ISTAT)
      REAL          X0,Z0
      REAL          X(*),Z(*)
      INTEGER       NX,INS
      INTEGER       ISTAT
      ISTAT = 1
      CALL CELLPNT(X0,Z0,NX,X,Z,INS,*90)
      ISTAT = 0
      RETURN
 90   CONTINUE
      RETURN
      END

      SUBROUTINE CELLPOLY_W(X0,Z0,NX,X,Z,INS,ISTAT)
c  inside ins = 1  outside ins = 0
      REAL          X0,Z0
      REAL          X(*),Z(*)
      INTEGER       NX,INS
      INTEGER       ISTAT
      ISTAT = 1
      CALL CELLPOLY(X0,Z0,NX,X,Z,INS,*90)
      ISTAT = 0
      RETURN
 90   CONTINUE
      RETURN
      END

