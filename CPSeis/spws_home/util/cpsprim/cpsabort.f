      SUBROUTINE CPSABORT (MSG)
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
C        1         2         3         4         5         6         7  |
C23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C  Process name:  CPSABORT    (ABORT THE EXECUTION OF A CPS JOB)         
C        Author:  Tom Stoeckley 
C       Written:  88/10/19
C  Last revised:  88/10/19
C
C  Purpose:       This primitive can be called to abort the execution
c                 of a CPS job with an error message and history file
c                 printout.  
C-----------------------------------------------------------------------
C                   INPUT PARAMETERS: Passed as arguments
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC                                                             
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C     Date     Author       Description
C     ----     ------       -----------
C 1.  88/10/19 Stoeckely    Original
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C        CALL CPSABORT (MESSAGE)           
C
C  MESSAGE = a character variable or constant of any length, containing
c            a message to be printed.
C_______________________________________________________________________
C                                NOTES
C
c  1. This is an alternative to the alternate error return (RETURN 1
c     in CPS processes), and will be more convenient when used within 
c     subroutines which do not have an error return.
C_______________________________________________________________________
C
C  Contains the following entry point:  CPSABORT.
C_______________________________________________________________________
C
C  Calls externals:  REMARK, BHIST, PHIST, ABORT.
C-----------------------------------------------------------------------
C\END DOC
      CHARACTER MSG*(*)
      PRINT *, MSG
      PRINT *, 'CPS--ERROR EXIT FROM CPSABORT'
C     CALL REMARK (' CPS--ERROR EXIT FROM CPSABORT')
C     CALL BHIST (999)
C     CALL PHIST
C     CALL ABORT (' CPS:ERR')
      WRITE(6,*) ' CPS:ERR' 
      RETURN
      END
