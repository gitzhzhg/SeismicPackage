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
C                         CRAY PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name:  TRIM_STRING
C        Author:  Bill Menger
C  Last revised:  ?? (Extracted from CPSBLD.FOR on 88/09/12)
C
C  Purpose:  Remove blanks from a string and convert to uppercase.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C        CALL TRIM_STRING (INPUT,OUTPUT,LENGTH)
C
C  where:
C    INPUT is the input character variable.
C    OUTPUT is the output character variable.
C    LENGTH is the number of non-blank characters in the string.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. The OUTPUT string consists of all the non-blank characters from
C    the INPUT string, converted to uppercase, compressed to the left
C    side of the field, with blank fill to the right.
C
C 2. LENGTH will always be returned as at least 1, even if INPUT was
C    entirely blank.
C-----------------------------------------------------------------------
C\END DOC
        subroutine trim_string (input,output,length)
        character*(*) input,output
C       integer*4 str$upcase
        length = 0
        m = len(output)
        do 10 i=1,len(input)
         if(input(i:i).ne.' ') then
          if(length.eq.m) then
           print*,' TRIM_STRING: WARNING- TRUNCATED STRING TO ',m,
     *            ' CHARACTERS.'
           goto 30
          endif
          length = length + 1
          output(length:length) = input(i:i)
         endif
 10     continue
        do 20 i=length+1,m
 20     output(i:i) = ' '
 30     length = max(1,length)
        call str_upcase(output,output)
        return
        end
