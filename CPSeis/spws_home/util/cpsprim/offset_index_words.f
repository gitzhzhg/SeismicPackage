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
C    Primitive name:  OFFSET_INDEX_WORDS         (get offset index)
C  Source directory:  primitives/memory
C           Library:  CONLIB
C           Written:  92/07/27  by:  Stoeckley
C      Last revised:  92/07/27  by:  Stoeckley
C
C  Purpose: This routine makes possible the use of pointers in ANSI-
C           standard Fortran on any machine with any word size.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE 
C
C                             out           in
C   CALL OFFSET_INDEX_WORDS (IOFFSET,ARRAY,IPOINT)  ! non-character array
C   CALL OFFSET_INDEX_BYTES (IOFFSET,ACHAR,IPOINT)  ! character*1 array
C
C  Type     Name     Description  
C  ----     ----     -----------
C  integer  IOFFSET  Offset index (returned).
C
C  real or  ARRAY    Any array dimensioned at least (1) in static 
C  integer           memory (in a SAVE statement or a common block).
C                    This array will be used to reference the
C                    allocated storage.
C
C  char*1   ACHAR    Any array dimensioned at least (1) in static 
C                    memory (in a SAVE statement or a common block).
C                    The first element of this array must begin on
C                    a word boundary.  Each element of ACHAR must
C                    have the same length as each element in the
C                    allocated storage.  This array will be used 
C                    to reference the allocated storage.
C
C  integer  IPOINT   Pointer previously returned by any routine which
C                    allocates storage, such as the following:
C                         GETS     malloc     ALLOC_WORDS    HPALLOC
C                         GETSCR   calloc     ALLOC_BYTES
C                    (GETS, GETSCR, and HPALLOC cannot return pointers
C                    to character variables.)
C-----------------------------------------------------------------------
C                               NOTES
C
C  1. To reference index I of the allocated variable, simply use:
C             ARRAY(IOFFSET+I)     or     ACHAR(IOFFSET+I)
C     The subscript IOFFSET+I will cause a reference to the correct
C     location in memory, even though that memory will lie outside 
C     the range of array ARRAY or ACHAR.
C
C  2. The array ARRAY or ACHAR can be used for any other purpose, if
C     desired.  Its contents are not affected by references to the 
C     allocated storage, since the referenced memory lies completely 
C     outside the range of array ARRAY or ACHAR.
C
C  3. The only time that allocated memory will overlap the range of 
C     array ARRAY would be when memory is allocated by GETS or GETSCR, 
C     and ARRAY is in the STORAGE or SCRATCH common block.  However,
C     there is no need to put ARRAY into either of these common blocks,
C     and it is not necessary to include these common blocks in the
C     CPS code where the GETS or GETSCR memory is referenced.
C
C  4. The offset index will not work in general for double precision 
C     variables, because this offset has a 50% chance of corresponding 
C     to an odd number of (integer or real) words, which on most 
C     machines would include half of a double precision word.
C
C  5. The offset index will not work in general for character variables
C     with a length of more than one character per array element,
C     because there would be very little chance that the offset index
C     would turn out to be an even integer.  The only way the offset
C     index could be guaranteed to be an even integer would be:
C       (1) if the pointer pointed to a word boundary, and
C       (2) if the character array used to reference the dynamic memory 
C            started on a word boundary. and
C       (3) if the number of characters in an element of the array
C            corresponded to an one computer word.
C     In practice, (1) may be OK since memory allocations probably
C     always start on word boundaries, and (2) may be OK if you take 
C     care not to put the character variable in a common block after 
C     another character variable that does not end on a word boundary.
C     However, (3) can never be simultaneously satisfied on all machines 
C     of different word sizes.
C     
C-----------------------------------------------------------------------
C                              EXAMPLES
C
C  1. To allocate real or integer memory:
C              CALL ALLOC_WORDS   (IPOINT,N)  ! dynamic storage
C         -or- CALL GETS          (IPOINT,N)  ! CPS permanent storage
C         -or- CALL GETSCR        (IPOINT,N)  ! CPS scratch storage
C     followed by:
C              CALL OFFSET_INDEX_WORDS (IOFFSET,ARRAY,IPOINT)
C     where N = number of words of memory needed, and ARRAY is any
C     real or integer array dimensioned at least (1) in a SAVE statement 
C     or a common block.
C
C  2. To allocate character memory:
C              CALL ALLOC_BYTES   (IPOINT,N*L)  ! dynamic storage
C     followed by:
C              CALL OFFSET_INDEX_BYTES (IOFFSET,ACHAR,IPOINT)
C     where N = number of words of memory needed, and ACHAR is any
C     character*1 array dimensioned at least (1) in a SAVE statement 
C     or a common block.
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date     Author       Description
C     ----     ------       -----------
C 2.
C 1.  92/07/27 Stoeckley    Initial version.
C-----------------------------------------------------------------------
C                       ROUTINES IN THIS PRIMITIVE
C
C  Subroutines:        OFFSET_INDEX_WORDS    OFFSET_INDEX_BYTES
C  Functions:          none
C  Subroutine entries: none
C  Function entries:   none
C  Common blocks:      none
C  Include files:      none
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C             location           subtract_integer_pointers   
C             clocation          subtract_byte_pointers
C-----------------------------------------------------------------------
C\END DOC


      subroutine offset_index_words (ioffset,array,ipoint)
c     get offset index for non-character variable.

      implicit none
c     integer ioffset,array,ipoint,location,a(2)
      integer ioffset,array,ipoint,location,subtract_integer_pointers
c     save a

c     ioffset=(  ipoint         - location(array)  )/
c    $        (  location(a(2)) - location(a(1))   )
      ioffset=subtract_integer_pointers(ipoint,location(array))
      return
      end



      subroutine offset_index_bytes (ioffset,achar,ipoint)
c     get offset index for character*1 variable.

      implicit none
c     integer ioffset,ipoint,location,clocation,sizeof_integer,a(2)
      integer ioffset,ipoint,clocation,subtract_byte_pointers
      character*(*) achar
c     save a

c     ioffset=(  ipoint         - clocation(achar)  )/
c    $        (  location(a(2)) - location(a(1))    )
c     ioffset=ioffset*sizeof_integer()
c     ioffset=ioffset*sizeof_integer()/len(achar)
      ioffset=subtract_byte_pointers(ipoint,clocation(achar))
      return
      end

