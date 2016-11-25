      SUBROUTINE PUTG (PROC)
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
C  Process name: PUTG and GETG,     GLOBAL DATA BASE MANAGER.
C        Author: John B. Sinton
C  Last revised: 90/06/07  Howard
C
C  Purpose: To put data in the global data base (PUTG) and to get data
C           from the global data base (GETG). 
C
C-----------------------------------------------------------------------
C                                 NOTES
C
C  1. These routines operate by generating global blocks associated
C     with a process number, which is supplied in their calls.
C
C  2. The first value in the global common block /GLOBALS/ is never
C     altered or saved.  Only the last 13 values will be processed.
C
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C        CALL PUTG  (PROC)  Put new data in data base.
C        CALL GETG  (PROC)  Get old data from data base.
C
C  Name   Valid    Description
C
C  PROC   int>0    Process number of the calling routine.
C-----------------------------------------------------------------------
C                                 NOTES
C
C  1. PUTG transfers all globals from the common block /GLOBALS/ to 
C     the next available address in the global data base.  This address
C     is saved within PUTG for use with GETG.  Nothing will be transfer-
C     ed if PROC<1 or PROC>199.  The globals will be overwritten if 
C     PUTG is call more than once with the save PROC.
C
C  2. GETG transfers all globals from the global data base to the common
C     block /GLOBALS/ using the address stored for the calling process.
C     Nothing will be transfered if PROC<1, PROC>199, or if PUTG was not
C     called before GETG for the same PROC.
C
C  3. While process numbers can go up to MAXIPN (199), at most 50
C     different processes may call PUTG in any one CPS job.
C
C-----------------------------------------------------------------------
C\END DOC
      PARAMETER (MAXIPN = 199)
      COMMON /GLOBALS/ NWIH,IGLOBALS(13)
      COMMON /GLOBDB0/      JGLOBALS(650)
      COMMON /GLOBDB1/ IADD(MAXIPN)
      INTEGER PROC
      DATA NADD /1/
C
C--------------------------------------------------------------------
C  ****
C  ****  PUTG ENTRY POINT DOES THE FOLLOWING THINGS.
C  ****  1) TRANSFERS GLOBALS FROM /GLOBALS/ TO /GLOBDB0/ STARTING AT
C  ****     IADD(PROC) = NADD
C  ****  2) IF GLOBALS HAVE PREVIOUSLY BEEN SAVED THEN OVERWRITE THE
C  ****     EXISTING ONES.
C  ****  3) IF GLOBALS HAVE NOT BEEN SAVED THE ADD THE NEW ONES AND
C  ****     UPDATE THE NEXT ADDRESS NADD.
C  ****
      IF (PROC.LT.1 .OR. PROC.GT.MAXIPN) RETURN
      IF (IADD(PROC).EQ.0) THEN
       IADD(PROC) = NADD
       NADD = NADD+13
      ENDIF
      DO 10 I=1,13
       JGLOBALS(IADD(PROC)+I-1) = IGLOBALS(I)
 10   CONTINUE
      RETURN
C
C--------------------------------------------------------------------
C
      ENTRY GETG (PROC)
C  ****
C  ****  GETG ENTRY POINT DOES THE FOLLOWING THINGS.
C  ****  1) TRANSFERS GLOBALS FROM /GLOBDB0/ STARTING AT LOCATION
C           IADD(PROC) TO /GLOBALS/.
C  ****
      IF (PROC.LT.1 .OR. PROC.GT.MAXIPN) RETURN
      IF (IADD(PROC).EQ.0)            RETURN
      DO 20 I=1,13
       IGLOBALS(I) = JGLOBALS(IADD(PROC)+I-1)
 20   CONTINUE
      RETURN 
      END
