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
C      Process name:  GETS               (Get Storage)
C  Source directory:  primitives/memory
C           Library:  conlib
C            Author:  Mike Howard
C      Last revised: 92/06/12   Stoeckley
C
C  Purpose:  Set up address and reserve memory for user in the STORAGE
C            common block.
C-----------------------------------------------------------------------
C   This is a special version which should not go into Cray batch CPS
C                 because of the hardwired common block.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C          CALL  GETS(IP,N)
C          CALL  GETSNEW()
C
C  Argument returned by GETS:
C     IP = Memory address where your array will start.  This should be
C          used in a POINTER statement to point to your array.
C  Argument you must supply to GETS:
C     N  = Number of words of STORAGE you are requesting.
C  GETSNEW resets the storage pointer to the beginning of the block
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. Processes can reserve space in the STORAGE common block for data
C    (typically tables) that is needed through the duration of the job,
C    and may not be overwritten by other processes.
C
C 2. If the number of words you request (N) is greater than the space
C    currently available in the STORAGE common block, the job aborts
C    with an informative error message.
C-----------------------------------------------------------------------
C                      REVISION HISTORY
C   5. 93/10/21  R.Day, Added #include for cpscom.inc
C   4. 92/06/12; Tom Stoeckley, Change returned pointer to be a REAL
C                   pointer on any machine.  Previously was a real
C                   pointer only on the Cray (word-addressable), but
C                   not on a byte-addressable machine.  Also change
C                   LOC to LOCATION.
C   3. 1991-92;  Richard Day, Hard-wire common block to 18000 words.
C   2. 88/08/18; Bob Baumel, Modify so ISTORAGE in common block SCRSIZ
C                is actual size of storage rather than a pointer.
C   1. 86/05/28; Bill Menger
C-----------------------------------------------------------------------
C    Contains the following subroutines:    GETS 
C    Contains the following entries:        GETSNEW
C    Contains the following common blocks:  STORAGE  SCRSIZ
C    Calls the following externals:         LOCATION
C                                     increment_integer_pointer
C-----------------------------------------------------------------------
C\END DOC
      SUBROUTINE GETS(IP,N)
C  ****
C  ****  SETS POINTER IP FOR N WORDS OF STORAGE
C  ****
C#include <cpscom.inc>
      COMMON /SCRSIZ/ ISCRSIZEp,ISTOSIZEp,IWIDTHp
      INTEGER   ISCRSIZEp,ISTOSIZEp,IWIDTHp
C...... ISCRSIZEp The size of the SCRATCH common block
C...... ISTOSIZEp The size of the SCRATCH common block
C...... IWIDTHp   The size of the SCRATCH common block

      COMMON /SCRATCH/ SCRBUFp(24000)
      REAL      SCRBUFp
C...... SCRBUFp   The scratch memory buffer. Volatile memory.

      COMMON /STORAGE/ ISTOPNTRp,STOBUFp(24000)
      REAL      STOBUFp
      INTEGER   ISTOPNTRp
C...... STOBUFp   The scratch memory buffer. Non-Volatile memory.
C...... ISTOPNTRp Pointer to the current end of storage.
      SAVE IS0                                                          
      DATA IS0/-1000000/
C
      IS0 = LOCation(STOBUFp)
      IP = ISTOPNTRp
      IF (increment_integer_pointer(ISTOPNTRp,N).GT.
     $  increment_integer_pointer(IS0,ISTOSIZEp) .AND. ISTOSIZEp.NE.0)
     $  THEN
       PRINT*,' GETS ERROR ---- Exceeded declared size of storage in '//
     *        'main program.  '
       PRINT*,'              Storage pointer before this call= ',
     *ISTOPNRTp
       PRINT*,'              You asked for ',n,' words.' 
       PRINT*,'              There are only ',ISTOSIZEp-ISTOPNTRp+is0,
     *        ' words left in storage.'
C      CALL ABORT(' I DIED  IN GETS.') 
       WRITE(6,*) ' I DIED  IN GETS.' 
      ELSE ! now uses C pointer arithmetic
       ISTOPNTRp=INCREMENT_INTEGER_POINTER(ISTOPNTRp,N)
      ENDIF
      RETURN
      ENTRY GETSNEW()
      IS0 = LOCation(STOBUFp)
      ISTOPNTRp = IS0
      RETURN
      END
