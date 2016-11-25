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
C                     SEISMIC PROCESSING WORKSTATION
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E
C
C    Primitive name:  GETS_OFFSET (get offset index for permanent storage)
C  Source directory:  primitives/memory
C           Library:  CONLIB
C           Written:  92/06/09  by:  Stoeckley
C      Last revised:  92/06/09  by:  Stoeckley
C
C  Purpose: This routine allocates CPS permanent storage (like GETS),
C           and also returns an offset index for use in referencing
C           this memory.  This helps make possible the use of pointers 
C           in ANSI-standard Fortran CPS processes on any machine.
C
C  Related Documentation:  This routine is a combination of GETS and
C                          OFFSET_INDEX_WORDS.  It is similar to the
C                          routines GETSCR_OFFSET, ALLOC_WORDS_OFFSET, 
C                          and ALLOC_BYTES_OFFSET.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE 
C
C                                  out          out   in
C              CALL GETS_OFFSET (IOFFSET,ARRAY,IPOINT,N)
C
C  Type     Name     Description  
C  ----     ----     -----------
C  integer  IOFFSET  Offset index (returned).
C
C  real or  ARRAY    Any array dimensioned at least (1) in static 
C  integer           memory (in a SAVE statement or a common block).
C                    This array will be used to reference the
C                    allocated permanent storage.
C
C  integer  IPOINT   Pointer (returned) to space in the CPS STORAGE 
C                    common block.
C
C  integer  N        Number of (real or integer) words to allocate.
C-----------------------------------------------------------------------
C                               NOTES
C
C  1. To reference index I of the allocated variable:
C                ARRAY(IOFFSET+I)
C     The subscript IOFFSET+I will cause a reference to the correct
C     location in permanent memory, whether or not that memory lies 
C     within the range of array ARRAY.
C
C  2. It is not necessary for the STORAGE common block to be
C     included in the CPS code where the allocated memory is
C     referenced.
C
C  3. The array ARRAY can be used for any other purpose.  Its contents
C     are not affected by references to permanent storage in the
C     STORAGE common block.
C
C  4. This primitive is equivalent to the following two calls:
C       CALL GETS               (IPOINT,N)             ! returns IPOINT
C       CALL OFFSET_INDEX_WORDS (IOFFSET,ARRAY,IPOINT) ! returns IOFFSET
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date     Author       Description
C     ----     ------       -----------
C 2.
C 1.  92/06/09 Stoeckley    Initial version.
C-----------------------------------------------------------------------
C                       ROUTINES IN THIS PRIMITIVE
C
C  Subroutines:        GETS_OFFSET
C  Functions:          none
C  Subroutine entries: none
C  Function entries:   none
C  Common blocks:      none
C  Include files:      none
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C                      gets    offset_index_words
C-----------------------------------------------------------------------
C\END DOC


      subroutine gets_offset (ioffset,array,ipoint,n)
      implicit none
      integer ioffset,array,ipoint,n

      call gets (ipoint,n)
      call offset_index_words (ioffset,array,ipoint)
      return
      end

