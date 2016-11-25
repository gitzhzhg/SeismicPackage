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
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name:  EDIT
C        Author:  Bill Menger
C  Last revised:  89/11/07
C  Revisions:
C   4. 89/11/07; Bob Baumel; Simplified code.
C   3. 89/03/03; Bob Baumel; Logical name VAX_PROG_DIR for directory
C                containing the EDTKEYS.GBL file.
C   2. 88/05/05; Bob Baumel; Split out from deeper in front end code.
C   1. ??/??/??; Bill Menger; Original version.
C
C  Purpose:  Call the EVV editor from within a Fortran program without
C            spawning a subprocess to do it.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C          CALL  EDIT (FILENAME)
C
C   where  FILENAME  is a character variable or constant containing
C   the name of the VAX file to be edited by EVV.  The routine can be
C   called with a BLANK value of FILENAME, in which case EVV is started
C   up in buffer MAIN with no currently active file.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. When a program calls this routine more than once ,then
C    when EVV starts up for the 2nd or subsequent time, the user will
C    get the possibly disturbing (or confusing) question:
C                 PF4 is not DO. Redefine PF4 as DO?
C    The user should simply hit carriage return when this question
C    appears; the editor will then work perfectly normally.
C
C 2. This routine assumes the TPU object code for EVV to be in file
C    VAX_PROG_DIR:EDTKEYS.GBL
C-----------------------------------------------------------------------
C\END DOC
        SUBROUTINE EDIT(FILE)
        INTEGER*4  LOCRV,TPU_TPU
        CHARACTER  CARD*132,FILE*(*)
        CARD = 'TPU/SECTION=VAX_PROG_DIR:EDTKEYS.GBL '//FILE
        LOCRV = TPU_TPU(CARD)
C       CALL LIB$SIGNAL (%VAL(LOCRV))
        RETURN
        END
