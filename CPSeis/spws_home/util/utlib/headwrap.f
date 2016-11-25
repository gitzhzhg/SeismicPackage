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
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO INC.
C
C                        C P S   P R I M I T I V E
C
CPrimitive names: HEADGETH_W, HEADPUTH_W
C        Author: Harlan
C  Date Written: 92/01/16
C  Last revised: 92/09/09 Hanson
C
C     Purpose: These routines read (write) parameters from (to) a header
C              file for grid or layer models.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C     CALL QSET(1)   (call if you will be on the Cray)
C     CALL HEADGETH(FILE,COORDX,COORDY,TRANS,TYPE,
C    &  N1,N2,N3,O1,O2,O3,D1,D2,D3, HEAD)
C
C Where: same source works on Vax and Cray
C Name    Type*  Valid  Description         *Type: I=IN, O=OUT, B=BOTH
C ----    ----   -----  -----------
C HEAD   CHARACTER*(*) Name of ASII header file (only input parameter)
C FILE   CHARACTER*(*) Name of file containing grid or layer model.
C COORDX CHARACTER*(*) Name of X coordinate
C COORDY CHARACTER*(*) Name of Y coordinate.
C COORDZ CHARACTER*(*) Name of Z coordinate (TIME or DEPTH).
C TRANS  CHARACTER*(*) Name of transform file.
C TYPE   CHARACTER*(*) Type of file containing grid or layer model.
C N1     INTEGER  Dimension of fastest (vertical) direction (NZ)
C N2     INTEGER  Dimension of 2nd fastest (horizontal) direction (NX)
C N3     INTEGER  Dimension of slowest (crossline) direction (NY)
C O1     REAL     Coordinate of first vertical sample (ZMIN)
C O2     REAL     Coordinate of first horizontal sample (XMIN)
C                 in units of COORDX
C O3     REAL     Coordinate of first crossline sample (YMIN)
C                 in units of COORDY
C D1     REAL     Sampling interval for vertical direction (ZINC)
C D2     REAL     Sampling interval for horizontal direction (XINC)
C                 in units of COORDX
C D3     REAL     Sampling interval for crossline direction (YINC)
C                 in units of COORDY
C-----------------------------------------------------------------------
C                                 NOTES
C 1.  Set any defaults for parameters before calling HEADGETH.  HEADGETH
C     will change values of these parameters if they are specified in
C     the parameter file.
C 2.  The following groups are written and read as synonyms:
C     O1,ZMIN; O2,XMIN; O3,YMIN; D1,DZ,ZINC; D2,DX,XINC; D3,DY,YINC.
C 3.  Specify parameters in the header file according to the following
C rules.  Use the format "NAME=VALUE", where NAME is the name of the
C parameter, and VALUE the value specified for the parameter--either
C an integer, real, or character string.  The following are all valid:
C         NX=1001     TRANS=FILE.TRANS      N3 =100  ZMIN=0  n2=50.
C       TYPE = GRID         FILE = "FILE.GRID"  Garbage
C        trash    XcoORDinate= XBASEMENT   junk    D1=4.e-3
C        (D2=10.,D3=1.)
C Exponenial notation is allowed. These specifications can appear in
C any order, with or without separating commas.   You may also mix any
C other text or documentation which you find useful.  Upper and lower
C case are not distinguished in the names of parameters; however, the
C value of a character string is case sensitive.  You can add spaces
C around the equals signs. To avoid unnecessary decoding, all
C parameters should appear in the first 200 lines of the file.   If
C you specify a parameter more than once, then the last specification
C encountered will be used. A character string can be optionally
C bracketed by double quotes.  Decimals are optional, for reals or
C integers.  The end of a parameter value can designated by a blank, a
C comma, a semicolon, or a right parenthesis.  The beginning of a
C parameter name can be designated by a blank, a comma, a semicolon, or
C a left parenthesis.
C
C 4.  You can get and put individual parameters with other names by
C     calling the following subroutines.
C    HEADGVOP, HEADPVOP: open a header file for getting or putting.
C    HEADGVCL, HEADPVCL: close a header file after getting or putting.
C    HEADGVR, HEADPVR: get or put a real parameter.
C    HEADGVI, HEADPVI: get or put an imaginary parameter.
C    HEADGVC, HEADPVC: get or put a character parameter.
C      Look at the source if you want to try.  HEADGETH and
C      HEADPUTH show the style.  If they are useful, these subroutines
C      will be individually documented (call Harlan at x6053)
C 5.  The header file will not be closed until you call HEADGVCL()
C     or HEADPVCL(), or call this subroutine again.  HEADPVOP and
C     HEADGVOP close any header file that was previously opened before
C     opening a new one
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date      Author    Description
C     ----      ------    -----------
C 5.  92/09/09  Hanson    Add GVRC,GVIC, GVCC, to read value from card
C                         MLIN, GLIN to control how many lines to 
C                         read in GV2.  FORL, GORL to control how
C                         many occurences of value to look for
C 4.  92/03/12  Harlan    Slight change to comments in header file.
C 3.  92/02/20  Harlan    Rearrange lines nicely in output header file.
C 2.  92/01/16  Harlan    Add COORDZ.  Don't close parameter file.
C 1.  92/01/14  Harlan    Original Version
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES IN THIS MODULE
C        SUBROUTINES:
C HEADPUTH,
C HEADADSU, HEADCADP, HEADCAPS, HEADCCAT, HEADCCPY, HEADCHSU,
C HEADCLFT, HEADCLOF, HEADDCOD, HEADDEVN, HEADFNEW, HEADGETC,
C HEADGV1, HEADGV2, HEADGVR, HEADHEYU, HEADLENL, HEADLENP,
C HEADLENR, HEADPVR, HEADQMAC, HEADGVRC
C        ENTRIES:
C HEADGETH,
C HEADCLOD, HEADDEV6, HEADDEVO, HEADFEND, HEADGTC1, HEADGTC2, HEADGVC,
C HEADGVCL, HEADGVDN, HEADGVI, HEADGVND, HEADGVOP, HEADHEYD, HEADOPEF,
C HEADOPIF, HEADPVC, HEADPVCL, HEADPVDN, HEADPVI, HEADPVOP, HEADQSET,
C HEADGVIC, HEADGVCC, HEADPLIN, HEADGLIN, HEADPORL, HEADGORL
C        NO FUNCTIONS OR COMMON BLOCKS.
C-----------------------------------------------------------------------
C                  EXTERNALS REFERENCED BY THIS MODULE
C CLOSFIL, GETLUN, OPNFIL  (used only on the Cray)
C                          (referenced but not used on the Vax)
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C
C  STORAGE       - 0
C  HEAP(dynamic) - 0
C-----------------------------------------------------------------------
C\END DOC
      SUBROUTINE HEADGETH_W(DFILE,CXI,CYI,CZI,TFILE,TYPE,
     &  N1,N2,N3,O1,O2,O3,D1,D2,D3, HFILE)
      CHARACTER FILE*80,HEAD*80,TRANS*80
      CHARACTER COORDX*16,COORDY*16,COORDZ*16,CTYPE*16
      INTEGER DFILE(*),CXI(*),CYI(*),CZI(*),TFILE(*),HFILE(*),TYPE(*)
      INTEGER N1,N2,N3
      REAL O1,O2,O3,D1,D2,D3
      INTEGER      SIZEOF_REAL

      CALL CONVERT_HH2CC(DFILE,0,FILE,0)
      CALL CONVERT_HH2CC(HFILE,0,HEAD,0)
      CALL CONVERT_HH2CC(TFILE,0,TRANS,0)
      CALL CONVERT_HH2CC(TYPE,0,CTYPE,0)
      CALL CONVERT_HH2CC(CXI,0,COORDX,0)
      CALL CONVERT_HH2CC(CYI,0,CORRDY,0)
      CALL CONVERT_HH2CC(CZI,0,CORRDZ,0)

      CALL HEADGETH(FILE,COORDX,COORDY,COORDZ,TRANS,CTYPE,
     &  N1,N2,N3,O1,O2,O3,D1,D2,D3, HEAD)

      N = LEN(FILE)/SIZEOF_REAL()
      CALL CONVERT_CC2HH(FILE,0,DFILE,-N)
      N = LEN(HEAD)/SIZEOF_REAL()
      CALL CONVERT_CC2HH(HEAD,0,HFILE,-N)
      N = LEN(TRANS)/SIZEOF_REAL()
      CALL CONVERT_CC2HH(TRANS,0,TFILE,-N)
      N = LEN(COORDX)/SIZEOF_REAL()
      CALL CONVERT_CC2HH(CTYPE,0,TYPE,-N)
      CALL CONVERT_CC2HH(COORDX,0,CXI,-N)
      CALL CONVERT_CC2HH(COORDY,0,CYI,-N)
      CALL CONVERT_CC2HH(COORDZ,0,CZI,-N)
      RETURN
      END

C23456789012345678901234567890123456789012345678901234567890123456789012
      SUBROUTINE HEADPUTH_W(DFILE,CXI,CYI,CZI,TFILE,TYPE,
     &  N1,N2,N3,O1,O2,O3,D1,D2,D3, HFILE)
      INTEGER DFILE(*),CXI(*),CYI(*),CZI(*),TFILE(*),HFILE(*),TYPE(*)
      CHARACTER FILE*80,HEAD*80,TRANS*80
      CHARACTER COORDX*16,COORDY*16,COORDZ*16,CTYPE*16
      INTEGER N1,N2,N3
      REAL O1,O2,O3,D1,D2,D3

      CALL CONVERT_HH2CC(DFILE,0,FILE,0)
      CALL CONVERT_HH2CC(HFILE,0,HEAD,0)
      CALL CONVERT_HH2CC(TFILE,0,TRANS,0)
      CALL CONVERT_HH2CC(TYPE,0,CTYPE,0)
      CALL CONVERT_HH2CC(CXI,0,COORDX,0)
      CALL CONVERT_HH2CC(CYI,0,CORRDY,0)
      CALL CONVERT_HH2CC(CZI,0,CORRDZ,0)

      CALL HEADPUTH(FILE,COORDX,COORDY,COORDZ,TRANS,CTYPE,
     &  N1,N2,N3,O1,O2,O3,D1,D2,D3, HEAD)
      RETURN
      END
