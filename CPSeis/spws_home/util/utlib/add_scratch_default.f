      SUBROUTINE ADD_SCRATCH_DEFAULT (IFN,EXT)
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
C***************************** COPYRIGHT NOTICE ********************************
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
C                       CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C                        C P S   P R I M I T I V E
C
CPrimitive name: ADD_SCRATCH_DEFAULT 
C        Author: John Sinton 
C  Date Written: Feb. 8, 1990
C  Last revised: 02/08/90 
C
C  Purpose: Adds scratch and Default Directory to a given File Name 
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C         CALL xxxx (ARG1, , ,...)
C      [or Y = xxxx (ARG1, , ,...) if a FUNCTION]
C ARG1
C Name    Type*  Valid  Description         *Type: I=IN, O=OUT, B=BOTH
C ----    ----   -----  -----------
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author    Description
C     ----      ------    -----------
C 2. 
C 1.  02/08/90  Sinton    Original Version
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES IN THIS MODULE
C
C-----------------------------------------------------------------------
C                  EXTERNALS REFERENCED BY THIS MODULE
C
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C
C  STORAGE       - 
C  HEAP(dynamic) - 
C-----------------------------------------------------------------------
C\END DOC
      CHARACTER*(*) IFN,EXT
      CHARACTER DEF_DIR*80, DEF_DEV*80
      LOGICAL SCR_DEVICE, ROOT_DEVICE
      MODE = 0
      GOTO 1
      ENTRY  CHECK_SCRATCH_DEFAULT (IFN,IFLAG)
      MODE = 1
 1    CONTINUE
      SCR_DEVICE  = .FALSE.
      CALL GET_DEFAULT_DIRECT (DEF_DIR,NCDD,DEF_DEV,NCDV,
     +SCR_DEVICE,ROOT_DEVICE,*10)
      GOTO 20
 10   CONTINUE
      NCDD = 0
      NCDV = 0
      DEF_DIR = ' '
      DEF_DEV = ' '
 20   CONTINUE
      CALL STR_UPCASE (IFN,IFN) 
      IF (MODE.EQ.1) THEN
       IFLAG = 0
       IF (NCDD.GT.0 .AND. .NOT.SCR_DEVICE) THEN
        IDSK = INDEX(IFN,'SCRATCH0:')
        ISBL = INDEX(IFN,'[')
        ISBR = INDEX(IFN,']')
        IF (ISBL.NE.0.AND.IDSK.GT.0) THEN
         IF (IFN(ISBL:ISBR).EQ.DEF_DIR(:NCDD)) IFLAG = 1
        ENDIF
       ENDIF
       RETURN
      ENDIF
      CALL ADDEXT(IFN,'BYT')
      CALL FLNSC(IFN,LNBC,ICHAR(' '))
      INOD = INDEX(IFN,'::')
      IDSK = INDEX(IFN,':')
      ISBL = INDEX(IFN,'[')
      IAST = INDEX(IFN,'*')
C
C***********************************************************************
C23456789*123456789*123456789*123456789*123456789*123456789*123456789*12
C     If file name has an '*' in the first character, then use root
C     directory.  The file name in IFN can not contain a directory! 
C
      IF (IAST.GT.0) THEN
       ISBL = INDEX(DEF_DIR,'.')
       IF (ISBL.EQ.0) THEN
        ISBL = NCDD
       ELSE
        DEF_DIR(ISBL:ISBL) = ']'
       ENDIF
       IFC = 2
       NUMCHR = LNBC-1 + ISBL 
       SCR_DEVICE = ROOT_DEVICE
      ELSE
       IF (INOD.EQ.0.AND.ISBL.EQ.0.AND.IDSK.EQ.0) THEN
        ISBL = NCDD
       ELSE
        ISBL = -1
       ENDIF
       NUMCHR = LNBC + ISBL
       IFC = 1 
      ENDIF
      IF (ISBL.EQ.-1) THEN
      ELSEIF (SCR_DEVICE) THEN
       IF (NUMCHR+9.GT.LEN(IFN))
     + CALL CRT_MESS (23,'MCPLT: warning you file name is too long!') 
       IFN = 'SCRATCH0:'//DEF_DIR(:ISBL)//IFN(IFC:LNBC)
      ELSE
       IF (NUMCHR.GT.LEN(IFN))
     + CALL CRT_MESS (23,'MCPLT: warning you file name is too long!') 
       IFN = DEF_DIR(:ISBL) // IFN(IFC:LNBC)
      ENDIF
      RETURN
      END
