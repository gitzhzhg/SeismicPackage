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
C                       CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C                        C P S   P R I M I T I V E
C
CPrimitive name: DBRITR     DeBRIghten TRace
C        Author: John Reed
C  Last revised: 26/06/89
C
C  Purpose:  Apply debrightening to individual events that exceed
C            debrightening threshold.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C         CALL DBRITR (TRNOHD,NT,DBMAX,ITPR)
C Arg 
C Name    Type*  Valid  Description         *Type: I=IN, O=OUT, B=BOTH
C ----    ----   -----  -----------
C TRNOHD  BOTH   ARRAY  1-D Input trace array
C NT      IN     INT    # points in trace array 
C DBMAX   IN     REAL   Debrightening threshold
C ITPR    IN     INT    Length (in samples) of debrighten taper
C
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. When called, DBRITR will find the largest event over DBMAX, apply   
C    a scaling factor to bring this event back to DBMAX and apply a ramp
C    of length ITPR to each side of this event.  This process will be
C    iterated until there are no events over DBMAX.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C 26/06/89  John Reed    New debrighten algorithm written as a primitive
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES IN THIS MODULE
C   DBRITR
C-----------------------------------------------------------------------
C                  EXTERNALS REFERENCED BY THIS MODULE
C    ISAMAX
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C
C  STORAGE       - 0
C  HEAP(dynamic) - 
C-----------------------------------------------------------------------
C\END DOC
      SUBROUTINE DBRITR(TRNOHD,NT,DBMAX,ITPR)
      DIMENSION TRNOHD(NT)
 40   CONTINUE
      IBIG = ISAMAX(NT,TRNOHD(1),1)        ! Find largest point
      ABIG=ABS(TRNOHD(IBIG))
      IF(ABIG .LE. DBMAX) GO TO 90         !Quit if none above threshold
      RFACT=DBMAX/ABIG*.95
      DO 45 J=MAX(IBIG-ITPR+1,1),MIN(IBIG+ITPR-1,NT)
        XJ=ABS(FLOAT(J-IBIG))/FLOAT(ITPR)
        FACT=RFACT+(1.0-RFACT)*XJ
        TRNOHD(J)=TRNOHD(J)*FACT           !Apply ramp function to event
 45   CONTINUE
      GO TO 40                             !Go look for another point
C
 90   CONTINUE                                                   
      RETURN          
      END
