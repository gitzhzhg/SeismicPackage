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
C\USER DOC
C-----------------------------------------------------------------------
C                         CRAY PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C  PROCESS NAME: GETSTOR (GET STORAGE)
C        AUTHOR: MIKE HOWARD  
C  LAST REVISED: 97/12/xx
C  REVISIONS:
C   5. 97/12/xx  Day  Fixed pointer arithmetic so it will work on machines
C                     which use non-word pointers. Added entry points so
C                     there is better symmetry between GETS and GETSCR.
C                     Using LOCATION rather than LOC for portability.
C                     GETS actually conflicts with c-lang. gets on some
C                     machines so is better to call getstor_get.
C   4. 93/12/29; Douglas Hanson, Add GETSL entry to return remaining
C      storage space
C   3. 89/04/04  D W HANSON VAX VERSION FOR FRONT END USE.
C   2. 88/08/18; BOB BAUMEL, MODIFY SO ISTORAGE IN COMMON BLOCK SCRSIZ
C                IS ACTUAL SIZE OF STORAGE RATHER THAN A POINTER.
C   1. 86/05/28; BILL MENGER
C
C  PURPOSE:  SET UP ADDRESS AND RESERVE MEMORY FOR USER IN THE STORAGE
C            COMMON BLOCK.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C          CALL  GETSTOR(IP,N)                !same as GETS
C          CALL  GETSL(NUM_WRDS_LEFT)         !remaining words
C          CALL  GETS(IP,N)                   !return pointer for N words
C          CALL  GETSTOR_LEFT(NUM_WRDS_LEFT)  !remaining words
C          CALL  GETSTOR_INIT()               !reset the pointer to start
C          CALL  GETSTOR_GET(IP,N)            !same as GETS
C          CALL  GETSTOR_SET(IP)              !set pointer to IP
C
C  ARGUMENT RETURNED BY GETSTOR:
C     IP = MEMORY ADDRESS WHERE YOUR ARRAY WILL START.  THIS SHOULD BE
C          USED IN A POINTER STATEMENT TO POINT TO YOUR ARRAY.
C  ARGUMENT YOU MUST SUPPLY TO GETSTOR:
C     N  = NUMBER OF WORDS OF STORAGE YOU ARE REQUESTING.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. PROCESSES CAN RESERVE SPACE IN THE STORAGE COMMON BLOCK FOR DATA
C    (TYPICALLY TABLES) THAT IS NEEDED THROUGH THE DURATION OF THE JOB,
C    AND MAY NOT BE OVERWRITTEN BY OTHER PROCESSES.
C
C 2. IF THE NUMBER OF WORDS YOU REQUEST (N) IS GREATER THAN THE SPACE
C    CURRENTLY AVAILABLE IN THE STORAGE COMMON BLOCK, THE JOB ABORTS
C    WITH AN INFORMATIVE ERROR MESSAGE.
C-----------------------------------------------------------------------
C   Subroutines :  GETS       GETSTOR_GET
C   Entries     :  GETSL      GETSTOR_INIT   GETSTOR_SET   GETSTOR_LEFT
C   Common Block:  STORAGE    SCRSIZ         SCRATCH
C   Externals   :  INCREMENT_INTEGER_POINTER LOCATION
C-----------------------------------------------------------------------
C\END DOC
      SUBROUTINE GETS(IP,N)
      IMPLICIT NONE
      INTEGER  IP,N
C  ****
C  ****  SETS POINTER IP FOR N WORDS OF STORAGE
C  ****
      CALL GETSTOR_GET(IP,N)
      RETURN
C
C return how much storage is left
      ENTRY GETSL(N)
       CALL GETSTOR_LEFT(N)
      RETURN
      END
CCC
C
C increment the storage pointer
      SUBROUTINE GETSTOR_GET(IP,N)
      IMPLICIT NONE
      INTEGER IP,N
C  ****
C  ****  SETS POINTER IP FOR N WORDS OF STORAGE
C  ****
      COMMON /SCRSIZ/ ISCRSIZEp,ISTOSIZEp,IWIDTHp
      INTEGER   ISCRSIZEp,ISTOSIZEp,IWIDTHp
C...... ISCRSIZEp The size of the SCRATCH common block
C...... ISTOSIZEp The size of the STORAGE common block
C...... IWIDTHp   The size of the SCRATCH common block

      COMMON /SCRATCH/ SCRBUFp(24000)
      REAL      SCRBUFp
C...... SCRBUFp   The scratch memory buffer. Volatile memory.

      COMMON /STORAGE/ ISTOPNTRp,STOBUFp(24000)
      REAL      STOBUFp
      INTEGER   ISTOPNTRp
C...... STOBUFp   The scratch memory buffer. Non-Volatile memory.
C...... ISTOPNTRp Pointer to the current end of storage.
      INTEGER IS0,INCREMENT_INTEGER_POINTER,LOCATION,NLEFT
      ENTRY GETSTOR(IP,N)
      IP = ISTOPNTRp
      IF(N.EQ.0) RETURN
      IS0 = LOCATION(STOBUFp)
      NLEFT = ISTOSIZEp - ((ISTOPNTRp-IS0) /
     +    ABS(LOCATION(STOBUFp(1)) - LOCATION(STOBUFp(2))))
      IF (N.GT. NLEFT .AND. ISTOSIZEp.NE.0) THEN
       PRINT*,' GETS ERROR ---- Exceeded declared size of storage in '//
     *        'main program.  '
       PRINT*,'              Storage pointer before this call= ',
     * ISTOPNTRp
       PRINT*,'              You asked for ',N,' words.'
       PRINT*,'              There are only ',NLEFT,
     *        ' words left in storage.'
       print*, 'I DIED IN GETS.'
       CALL ABORT
       !CALL ABORT(' I DIED  IN GETS.')
      ENDIF
      ISTOPNTRp=INCREMENT_INTEGER_POINTER(ISTOPNTRp,N)
      RETURN
C
C return how much storage is left
      ENTRY GETSTOR_LEFT(N)
      IS0 = LOCATION(STOBUFp)
      N = ISTOSIZEp - ((ISTOPNTRp-IS0) /
     +    ABS(LOCATION(STOBUFp(1)) - LOCATION(STOBUFp(2))))
      RETURN
C
C set the storage pointer
      ENTRY GETSTOR_SET(IP)
      IS0 = LOCATION(STOBUFp)
      IF (IP.GT.INCREMENT_INTEGER_POINTER(IS0,ISTOSIZEp) 
     + .OR. IP.LT.IS0) THEN
        print*, ' I DIED  IN GETSTOR_SET.'
        CALL ABORT
      ELSE
        ISTOSIZEp = IP
      ENDIF
      RETURN
C
C reset the storage pointer to the beginning of the block
      ENTRY GETSTOR_INIT()
      ISTOPNTRp = LOCATION(STOBUFp)
      RETURN
      END

