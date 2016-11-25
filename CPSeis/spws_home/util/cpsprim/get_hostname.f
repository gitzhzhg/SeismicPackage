      SUBROUTINE GET_HOSTNAME(HOST)
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
C  Process name:  GET_HOSTNAME
C        Author:  KAREN GOODGER
C  Last revised:  96/09/09
C
C  PURPOSE:  Get the hostname of current machine.                    
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C                           ----------------
C
C                CALL GET_HOSTNAME (HOST)
C
C           HOST is the hostname returned - character variable         
C-----------------------------------------------------------------------
C                                 NOTES
C
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC                                                                      
C-----------------------------------------------------------------------        
C                          REVISION HISTORY                                     
C     Date      Author     Description                                          
C     -----     ------     -----------                                          
C 02. 96/09/09  K Goodger  Change to use GETENV                        
C 01. 95/11/27  K Goodger  Original Version.                           
c
C-----------------------------------------------------------------------        
C\END DOC                                                                        
C
C      
      CHARACTER *(*) HOST
      CHARACTER TMP*8
      INTEGER CPSF_GETENV
ccc   DATA ITMPFIL2/'TMPFIL2'L/
ccc   ISTAT=ISHELL('hostname > TMPFIL2')
ccc   CLOSE(ITMPFIL2)
ccc   READ(ITMPFIL2,'(A)',END=10035)HOST
10035 CONTINUE
ccc   istat=ISHELL('rm TMPFIL2')
      TMP='HOST'
      ISTAT=CPSF_GETENV(TMP,HOST)
      IF(HOST(1:2).EQ.'  ') THEN
       TMP='HOSTNAME'
       ISTAT=CPSF_GETENV(TMP,HOST)
      ENDIF

      RETURN
      END
