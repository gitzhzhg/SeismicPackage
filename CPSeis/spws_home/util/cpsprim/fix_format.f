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
C        1         2         3         4         5         6         7  |
C23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E
C
C    Primitive name:  FIX_FORMAT   (fix a DCODE/NCODE format variable)
C  Source directory:  primitives/prim_io
C           Library:  conlib
C           Written:  92/05/11  by:  Tom Stoeckley
C      Last revised:  92/05/11  by:  Tom Stoeckley
C
C  Purpose: This is a routine for fixing the hollerith fields in a
C           DCODE/NCODE format variable.  The purpose is to provide
C           the means to help make CPS processes ANSI-standard, so
C           that they will work on any machine with any word size.
C
C  Note:    A preferable alternative to using this routine is (1) to
C           change all hollerith variables to true character variables
C           in the DCODE common block, (2) to make every character 
C           variable a multiple of 8 bytes, and (3) to make sure that 
C           the DCODE/NCODE format variable always explicitly specifies 
C           the length of each character variable.
C
C  Related Documentation:  
C-----------------------------------------------------------------------
C        THE VARIOUS ROUTINES IN THIS UTILITY ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
C  Parameters are real or integer based on Fortram conventions.
C  Arrays, hollerith, and character parameters are identified as such.
C-----------------------------------------------------------------------
C  To fix the hollerith fields in the DCODE/NCODE format variable:
C                                b   o
C              CALL FIX_FORMAT (FMT,IERR) 
C
C  FMT  = format to be modified (character).
C  IERR = error return (0 means no error).
C
C  The format is changed to specify the correct number of characters 
C     in hollerith variables, regardless of the length of a word on 
C     the machine.
c  Replaces #nn with mmm,
c     where  nn  = number of words (exactly two digits right justified),
c     and    mmm = number of characters (three digits).
c     This converts the number of words in a hollerith array to the
c     number of characters in the array.
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date     Author       Description
C     ----     ------       -----------
C 2.
C 1.  92/05/11 Stoeckley    Initial version.
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY, AND COMMON BLOCK NAMES IN THIS MODULE
C
C  Subroutines:     FIX_FORMAT
C  Functions:       none
C  Entry points:    none
C  Common blocks:   none
C_______________________________________________________________________
C                 EXTERNALS REFERENCED BY THIS MODULE
C
C           sizeof_integer             convert_squeeze_blanks
C-----------------------------------------------------------------------
C\END DOC


      subroutine fix_format (fmt,ierr)

      implicit none
      character*(*) fmt
      integer ierr,i,j,k,l,sizeof_integer

      j=sizeof_integer()
      l=len(fmt)
      do i=1,l-2
           if (fmt(i:i).eq.'#') then
                read (fmt(i+1:i+2),'(I2)',err=999) k
                write(fmt(i  :i+2),'(I3)',err=999) k*j
           end if
      end do
      call convert_squeeze_blanks (fmt)
      ierr=0
      return

999   print *, 'FIX_FORMAT: error encountered'
      ierr=1
      return
      end

