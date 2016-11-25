      SUBROUTINE VALID_PLOT_NODE (NLOC,NLOCA)
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
CPrimitive name: VALID_PLOT_NODE 
C        Author: Peterson 
C  Date Written: 90/04/24
C  Last revised: 90/07/11 
C
C     Purpose: This routine is designed to be a general purpose check
C     of the legal VAX nodes for networking a plot.
C-----------------------------------------------------------------------
C                                 NOTES
C 1. When this routine in modified, UTLIB will need to be rebuilt so the
C    latest version will be available to the CFE loading prodcedures.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C     Date      Author    Description
C     ----      ------    -----------
C 3.  90/07/11  Peterson  Add POESP1 node and change HONAE to HOWWE.
C 2.  90/05/30  Peterson  Remove HOAEO from valid plot nodes.
C 1.  90/04/24  Peterson  Original Version
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C           CALL VALID_PLOT_NODE (NLOC,NLOCA)
C    Where:
C Name    Type*  Valid  Description         *Type: I=IN, O=OUT, B=BOTH
C ----    ----   -----  -----------
C NLOC    Char*8        User requested plot node
C NLOCA   Returned      0 = Legitimate VAX Plot Node.
C                       1 = Illegal Plot Node
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
      CHARACTER NLOC*8
      NLOCA = 0
      IF (NLOC.EQ.'HOWWE' ) RETURN
      IF (NLOC.EQ.'LBCRD' ) RETURN
      IF (NLOC.EQ.'POGSDD') RETURN
      IF (NLOC.EQ.'POISP' ) RETURN
      IF (NLOC.EQ.'PO8600') RETURN
      IF (NLOC.EQ.'POESP1') RETURN
      NLOCA = 1
      CALL CRT_MESS (23,
     *' ERROR, NLOC = HOWWE, LBCRD, POGSDD, POISP, PO8600 or POESP1')
      RETURN
      END     
