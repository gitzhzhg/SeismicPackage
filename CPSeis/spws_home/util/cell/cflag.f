C23456789012345678901234567890123456789012345678901234567890123456789012
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
C       SUBROUTINE CFLAG
C***************************** COPYRIGHT NOTICE ********************************
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLIBW2ED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C
CPrimitive name: CFLAG
C        Author: D W Hanson
C       Written: 94/01/26
C  Last revised: 94/01/26
C
C  Purpose:  To read and write velocity models.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C      CALL CFLAGPUT(ICRAY)
C      CALL CFLAGGET(ICRAY)
C
C ARGUMENTS
C Name    Type*  Valid  Description         *Type: I=IN, O=OUT, B=BOTH
C ----    ----   -----  -----------
C ICRAY    B     0,1    Flag indicating machine type 0 = vax 1 = cray
C-----------------------------------------------------------------------
C                                 NOTES
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date     Author      Description
C     ----     ------      -----------
C 1.  94/01/26 Hanson      Original version
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES IN THIS MODULE
C
C   SUBROUTINE NAMES IN THIS MODULE
C CFLAGPUT
C 
C   ENTRY NAMES IN THIS MODULE
C CFLAGGET
C
C 
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C
C  STORAGE       - NONE
C  HEAP(dynamic) - NONE
C-----------------------------------------------------------------------
C\END DOC
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cflagput(jcray)
      implicit none
      integer icray,jcray
      save icray
      data icray/0/    ! icray = 0 vax icray=1 cray
      icray = jcray
      return
      entry cflagget(jcray)
      jcray = icray
      return
      end
