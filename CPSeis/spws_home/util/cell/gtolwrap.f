
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
C***********************************************************************
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                 C P S   S U P P O R T  R O U T I N E S
C    Primitive name:  See below
C  Source directory:  ~spws/util/model/   file gtolwrap.f
C           Library:  cell.a
C           Written:  92/10/12   by:  R.S.Day
C      Last revised:             by:
C
C  Purpose: The routines in this file are designed to be an interface
C           between some C and Fortran language routines.
C   The file gtol.f was written by Doug Hanson. 
C-----------------------------------------------------------------------
C ROUTINES defined in the current file(wrapper routines)
C     GTOLGTON_W(...)
C
C-----------------------------------------------------------------------
C\END DOC
C**********************************************************************/
C23456789012345678901234567890123456789012345678901234567890123456789012
      SUBROUTINE GTOLGTON_W(ICZ,IDIR
     +,NV,IMV,ITV,IXV,NXV,XV,ZV,VEL,NC,IMC,IXC,NXC,XC,ZC
     +,NXG,XGMIN,XGINC,NZG,ZGMIN,ZGINC,VG,MWORK,WORK,LU,ISTAT)
      CHARACTER CZ*16
      INTEGER IDIR,NV,IMV(*),ITV(*),IXV(*),NXV(*)
      REAL    XV(*),ZV(*),VEL(*)
      INTEGER NC,IMC(*),IXC(*),NXC(*),NXG,NZG,LU
      REAL    XC(*),ZC(*),XGMIN,XGINC,ZGMIN,ZGINC
      REAL    VG(*),WORK(*)
      INTEGER      SIZEOF_REAL

      ISTAT = 0
      CALL CONVERT_HH2CC(ICZ,0,CZ,0)

      CALL GTOLGTON(ICZ,IDIR
     1,NV,IMV,ITV,IXV,NXV,XV,ZV,VEL,NC,IMC,IXC,NXC,XC,ZC
     1,NXG,XGMIN,XGINC,NZG,ZGMIN,ZGINC,VG,MWORK,WORK,LU,*90)

      N = LEN(CZ)/SIZEOF_REAL()
      CALL CONVERT_CC2HH(CZ,0,ICZ,-N)
      RETURN
 90   CONTINUE
      ISTAT = 1
      RETURN
      END

