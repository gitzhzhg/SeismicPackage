      SUBROUTINE LAYSRA_W(IFILE,IUNITS,ITORZ,
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
     + XMIN,XMAX,ZMIN,ZMAX,
     + NL,IVL,IXL,NXL,XL,ZL,
     + NV,ITV,IXV,NXV,XV,ZV,VEL)
      CHARACTER CFILE*80,CUNITS*16,TORZ*8
      INTEGER   LUN
C Wrapper for c-language access to LAYSRA()
C  CREATE GRIDDED Z AND V IN SIERRA FORMAT
C  NOTE WE HAVE NL LAYERS
C  THE FIRST IS AT THE TOP AND THE LAST AT THE BOTTOM
C  SIERRA DOESN'T WANT EITHER THE TOP OR BOTTOM
C  IF ZMIN < 0. PASS OUT LAYERS IN REVERSE ORDER
      CALL CONVERT_HH2CC(IFILE,0,CFILE,0)
      CALL CONVERT_HH2CC(IUNITS,0,CUNITS,0)
      CALL CONVERT_HH2CC(ITORZ,0,TORZ,0)
      CALL GETLUN(LUN,ISTAT)
      IF(LUN.LE.0) RETURN

      CALL LAYSRA(LUN,CFILE,CUNITS,TORZ,
     + XMIN,XMAX,ZMIN,ZMAX,
     + NL,IVL,IXL,NXL,XL,ZL,
     + NV,ITV,IXV,NXV,XV,ZV,VEL)

      CLOSE(LUN)
      RETURN
      END
