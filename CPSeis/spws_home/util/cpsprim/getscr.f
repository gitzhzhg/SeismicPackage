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
C                         CRAY PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C  Process name:  GETSCR    (GET SCRATCH)
C        Author:  TOM STOECKLEY , 18 OCTOBER 1988
C  Last revised:  97/12/xx   R.S.Day
C
C  Purpose:       Alternate way to get scratch storage analogous to
C                 GETS, without using COMMON/SCRATCH directly.  Also
C                 simplifies bookkeeping of pointer values.
C
C  Related Documentation:  GETS
C-----------------------------------------------------------------------
C                   INPUT PARAMETERS: Passed as arguments
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC                                                             
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C 97/12/xx  R.S.Day      Added entries for convenience and fixed logic for
C                        machines that use now-word pointers. Replaced
C                        LOC by LOCATION for portability.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C        CALL GETSCR (IPOINT,N)           !return pointer
C        CALL GETSCR_LEFT(NUM_WRDS_LEFT)  !return no. of words left
C        CALL GETSCR_INIT()               !reset to beginning of scratch
C        CALL GETSCR_GET(IPOINT,N)        !same as getscr
C        CALL GETSCR_SET(IPOINT)          !set counter to ipoint
C
C  IPOINT = pointer to scratch array (returned).
C
C  N = number of values needed for the scratch array (input).
C_______________________________________________________________________
C                                NOTES
C
C  1. Use  CALL GETSCR (0,0)  to start allocating scratch space at the
c     beginning of the scratch common block.  This must be your first
c     call to GETSCR.  This call does not actually allocate space; it
c     just initializes a pointer.
c
c  2. Use  CALL GETSCR (IPOINT,N)  to allocate N words of scratch space
c     and return pointer.  This can be done separately for each pointer
c     you need.  The space is allocated from the scratch common block
c     immediately after the previous space allocated.  The program
c     aborts if not enough space is left.
c
c  3. Use  CALL GETSCR (IPOINT,0)  to start allocating scratch space
c     beginning at pointer IPOINT.  IPOINT must have been previously
c     returned by GETSCR, or calculated from a returned pointer.  The
c     program aborts if IPOINT is out of range.  This call does not 
c     actually allocate space; it just sets a pointer.
c
c  4. Use  CALL GETSCR (LEFT,-1)  to return the number of words of
c     scratch space still available for allocation.
c
c  5. You must be sure to complete your scratch space allocations 
c     before calling another process internally, which might be using
c     this same primitive, unless you use the option described in
c     note 3 above.
c
c  6. Example of use:    POINTER (IIIA,A(NA)),(IIIB,B(NB))
C                        CALL GETSCR (0,0)
c                        CALL GETSCR (IIIA,NA)
C                        CALL GETSCR (IIIB,NB)
C
C  7. If you call another process internally, and both you and the
C     internal process need separate (non-overlapping) scratch areas,
C     you can call the setup of the internally-called process first,
C     then continue to get scratch in your own process without calling
C     GETSCR (0,0) first.  This presupposes that the internally called
C     process does not call GETSCR(0,0) or GETSCR(IPOINT,0) after
C     getting its storage.
C_______________________________________________________________________
C
C  Subroutines: GETSCR     GETSCR_GET
C   Entries   : GETSCR_SET GETSCR_INIT GETSCR_LEFT
C Common Block: SCRATCH, SCRSIZ.
C   Externals : increment_integer_pointer  location
C_______________________________________________________________________
C\END DOC
      SUBROUTINE GETSCR (IPOINT,N)
      INTEGER IPOINT,N
C----------INITIALIZE POINTER TO BEGINNING OF COMMON BLOCK.
      IF (N.EQ.0.AND.IPOINT.EQ.0) THEN
           CALL GETSCR_INIT()
C----------ALLOCATE A SCRATCH ARRAY.
      ELSE IF (N.GT.0) THEN
           CALL GETSCR_GET(IPOINT,N)
C----------INITIALIZE POINTER TO DESIRED LOCATION.
      ELSE IF (N.EQ.0) THEN
           CALL GETSCR_SET(IPOINT)
C----------FIND OUT HOW MUCH SPACE IS LEFT FOR ALLOCATION.
      ELSE                 ! IF (N.LT.0) THEN
           CALL GETSCR_LEFT(IPOINT)
      END IF
      RETURN
      END
CCC
C----------ALLOCATE A SCRATCH ARRAY.
      SUBROUTINE GETSCR_GET(IPOINT,N)
      IMPLICIT NONE
      INTEGER IPOINT,N, I,JSTART,JNEXT,JSTOP,A(2)
      INTEGER ISCRATCH,ISTORAGE,IWIDTH
      INTEGER INCREMENT_INTEGER_POINTER,LOCATION
      REAL DUMMY
      COMMON/SCRATCH/DUMMY
      COMMON/SCRSIZ/ISCRATCH,ISTORAGE,IWIDTH
      SAVE JSTART,JSTOP,JNEXT
C     JSTART = LOCATION OF FIRST WORD IN SCRATCH COMMON BLOCK.
C     JSTOP = LOCATION OF LAST WORD IN SCRATCH COMMON BLOCK PLUS ONE.
C     JNEXT = LOCATION OF NEXT POINTER TO BE ALLOCATED.
       IPOINT=JNEXT
       JNEXT=INCREMENT_INTEGER_POINTER(JNEXT,N)
       IF (JNEXT.GT.JSTOP) CALL CPSABORT 
     $   ('GETSCR: INSUFFICIENT SCRATCH SPACE AVAILABLE')
      RETURN
C----------FIND OUT HOW MUCH SPACE IS LEFT FOR ALLOCATION.
      ENTRY GETSCR_LEFT(I)
       I = (JSTOP-JNEXT)/ABS(LOCATION(A(1)) - LOCATION(A(2)))
      RETURN
C----------INITIALIZE POINTER TO BEGINNING OF COMMON BLOCK.
      ENTRY GETSCR_INIT()
       JSTART=LOCATION(DUMMY)
       JSTOP=INCREMENT_INTEGER_POINTER(JSTART,ISCRATCH)
       JNEXT=JSTART
      RETURN
C----------INITIALIZE POINTER TO DESIRED LOCATION.
      ENTRY GETSCR_SET(IPOINT)
        IF (IPOINT.LT.JSTART.OR.IPOINT.GE.JSTOP)
     +    CALL CPSABORT ('GETSCR: POINTER OUT OF RANGE')
        JNEXT=IPOINT
      RETURN
      END
