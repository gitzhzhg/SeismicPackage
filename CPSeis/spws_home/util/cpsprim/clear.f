         SUBROUTINE CLEAR(AR,ISIZE)
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
C\USER DOC
C-----------------------------------------------------------------------
C                         CRAY PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C  Process name:  CLEAR
C        Author:  Mike Howard
C  Last revised:  May 28, 1986 -- Bill Menger
C
C  Purpose:       Set a trace to 0.0
C                
C-----------------------------------------------------------------------
C                           INPUT PARAMETERS: passed as arguments
C        
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C        CALL   CLEAR(AR,ISIZE)
C  AR    = ADDRESS OF YOUR TRACE
C  ISIZE = NUMBER OF WORDS TO SET TO 0.0
C  
C  
C_______________________________________________________________________
C                                 NOTES
C_______________________________________________________________________
C\END DOC
         DIMENSION AR(ISIZE)
 
         DO 10 I = 1,ISIZE
10       AR(I)   = 0.0
 
         RETURN
         END
