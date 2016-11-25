CCC
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
C Obtain the address of the globals common block
      SUBROUTINE GET_INITHIS_ADDR(ADDR)
      PARAMETER (MAXIPN=199)
      INTEGER  ADDR
      INTEGER  LOCATION
      EXTERNAL LOCATION
C------COMMON TO HOLD LIST OF PROCESS NAMES AND HISTORY CARD COUNT
      INTEGER       NPROCN,NPROCC,NHRBAD
      CHARACTER     PROCN*8
      COMMON /INITHIS/ NPROCN, PROCN(0:MAXIPN),NPROCC(0:MAXIPN),NHRBAD
      ADDR= LOCATION(NPROCN)
      RETURN
      END

