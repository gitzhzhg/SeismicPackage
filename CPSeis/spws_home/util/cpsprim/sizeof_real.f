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
C                        CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                               CONOCO INC.
C
C                        C P S   P R I M I T I V E
C
C Name   : SIZEOF_REAL()
C Purpose: return the number of bytes in a fortran real
C Author : R. Day
C Date   : 97/12/01
C 
C Function Definition:        ( Language = F )
C    INTEGER FUNCTION SIZEOF_REAL()
C-------------------------------------------------------------
C NOTES:
C  1.
C-------------------------------------------------------------
C                           REVISION HISTORY
C
C     DATE      WHO         DESCRIPTION
C     --------  --------    --------------------------------------
C 1. 97/12/01   Day         Put on system as a fortran routine.
C                           Replaces function in sizeof_string.c
C-----------------------------------------------------------------------
C\END DOC
      FUNCTION SIZEOF_REAL()
      INTEGER SIZ_REAL,SIZEOF_REAL
      REAL    A(2)
      SIZEOF_REAL=4
      SIZEOF_REAL=SIZ_REAL(A(1),A(2))
      RETURN
      END

