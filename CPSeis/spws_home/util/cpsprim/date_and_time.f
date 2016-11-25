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
c        1         2         3         4         5         6         7  |
c23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E
C                             fortran code
C                  designed to be called from fortran
C
C    Primitive name:  DATE_AND_TIME     
C  Source directory:  primitives/memory
C           Library:  conlib          
C           Written:  95/04/12   by:  Tom Stoeckley
C      Last revised:  98/12/04   by:  R. Day
C
C  Purpose:       brief reason for its existance
C
C  Related Documentation:  To return a character string which contains
C                          the date and time.  This subroutine interfaces
C                          with the C-language function time_date_string
C                          in the cprim library.  This subroutine should
C                          be used instead of the Fortran routines DATE
C                          and TIME for ANSI-standard portability.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2. 98/12/04  Day        Added date_and_timef, which calls
C                          time_date_stringf
C  1. 95/04/12  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C                       ROUTINES IN THIS PRIMITIVE
C
C  Subroutines:           date_and_time  date_and_timef
C  Functions:             none
C  Subroutine entries:    none
C  Function entries:      none
C  C-language functions:  date_and_time_crou
C  Common blocks:         none
C  Include files:         none
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C
C              time_date_string           convert_hh2cc  
C              getpoint_character         time_date_stringf
C-----------------------------------------------------------------------
C\END DOC


      subroutine date_and_time (cvar)

      implicit none
      character*(*) cvar
      integer buffer(60)

      call date_and_time_crou (buffer)
      call convert_hh2cc (buffer,60,   cvar,0)
      return
      end

      subroutine date_and_timef(cvar,flag)
      implicit none
      character*(*) cvar
      integer buffer(60),flag

      call time_date_stringf(buffer,flag)
      call convert_hh2cc (buffer,60,   cvar,0)
      return
      end



