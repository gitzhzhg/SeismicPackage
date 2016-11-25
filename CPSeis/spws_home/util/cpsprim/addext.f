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
C                        CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO INC.
C
C                      V A X   P R I M I T I V E
C
CPrimitive name:  ADDEXT
C        Author:  Bob Baumel
C  Date written:  88/07/13
C  Last revised:  91/4/1   R DAY
C
C    Purpose:  Add extender to a VAX file name, or replace extender.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C                   CALL ADDEXT (FNAME,EXT)
C                   CALL ADDEXT_REPLACE (FNAME,EXT)
C                   CALL ADDEXTRP (FNAME,EXT)
C
C  For all routines:
C      FNAME is a character variable (any length) containing the file
C         name to be modified.
C      EXT is a character variable or constant (any length) containing
C         the extension to add to FNAME.
C
C  Entry point ADDEXT adds the extender EXT to file name FNAME in the
C     event that FNAME doesn't already have an extender, but leaves
C     FNAME unchanged if it does already have an extender.
C  Entry point ADDEXT_REPLACE puts the extender EXT into FNAME, whether
C     or not FNAME already has an extender.  (Thus, it REPLACES the
C     extender in the event that FNAME already has one.)
C  The name ADDEXTRP is synonymous with ADDEXT_REPLACE.  This shorter
C     form of the name was first introduced on the CRAY side, but is
C     now usable on the VAX as well.
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C   5. 92/5/20 ; T.Stoeckley; Fix error caused by using FLNBC.
C   4. 91/4/01 ; R. Day ; Used FLNBC in addext_replace to find strin end 
C   3. 90/12/07; B Baumel; Introduce the name ADDEXTRP (previously
C                introduced on Cray side) as synonym for ADDEXT_REPLACE.
C   2. 88/07/22; B Baumel; Add the ADDEXT_REPLACE entry point; recognize
C                the '>' character, as well as ']', as a possible
C                right-hand delimeter of a directory name.
C   1. 88/07/13; B Baumel; Separate routine from other front-end code
C                where it had been hidden.
C-----------------------------------------------------------------------
C\END DOC
        subroutine addext (fname,ext)
        character*(*) fname,ext
        ientry = 1
        go to 1
        entry addextrp (fname,ext)
        entry addext_replace (fname,ext)
        ientry = 2
   1    iclose = index (fname,']')
        if (iclose.eq.0)  iclose = index (fname,'>')
        idot = index (fname(iclose+1:),'.')
        if (idot.gt.0 .and. ientry.eq.1)  return
        isem = index (fname(iclose+1:),';')
        if (isem .gt. 0)  then
          if (idot.eq.0)  idot = isem
          fname = fname(:iclose+idot-1)//'.'//ext//fname(iclose+isem:)
        else
          if (idot.gt.0)  then
            fname = fname(:iclose+idot)//ext
          else
c           ilast = index (fname(iclose+1:),' ')
            call flnbc (fname(iclose+1:),ilast,0)
            if (ilast.eq.0)  return
c           fname = fname(:iclose+ilast-1)//'.'//ext
            fname = fname(:iclose+ilast)//'.'//ext
          end if
        end if
        return
        end
