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
C    Primitive name:  GET_NPARM    (get number of parameters)
C  Source directory:  primitives/memory
C           Library:  conlib
C           Written:  92/05/11  by:  Tom Stoeckley
C      Last revised:  97/10/26  by:  Richard Day
C
C  Purpose: This is a routine for getting the number of parameters
C           in a common block (or portion therof).  It is particularly
C           useful for the DCODE common block in CPS processes.  The
C           returned parameter can be used for calling PUTP without
C           the programmer having to determine whether it is correct
C           every time the common block is changed.
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
C  To get the number of words in a (portion of a) common block:
C                                i     i     o
C              CALL GET_NPARM (APARM,ZPARM,NPARM)
C
C  APARM = first parameter to include in the set (non-character).
C  ZPARM = parameter immediately following last parameter to include
C             in the set (non-character).
C  NPARM = number of words including APARM but not including ZPARM.
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date     Author       Description
C     ----     ------       -----------
C 4.  97/10/26 Day          Went back to original version. It ports better
C 3.  92/08/05 Stoeckley    Fix arguments to subtract_real_pointers.
C 2.  92/07/27 Stoeckley    Change to call subtract_real_pointers.
C 1.  92/05/11 Stoeckley    Initial version.
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY, AND COMMON BLOCK NAMES IN THIS MODULE
C
C  Subroutines:     GET_NPARM
C  Functions:       none
C  Entry points:    none
C  Common blocks:   none
C_______________________________________________________________________
C                 EXTERNALS REFERENCED BY THIS MODULE
C               location          subtract_real_pointers
C-----------------------------------------------------------------------
C\END DOC



c     subroutine get_nparm (aparm,zparm,   nparm)
c     implicit none
c     real aparm,zparm
c     integer nparm,location,subtract_real_pointers
c
c     nparm=subtract_real_pointers(location(zparm),location(aparm))
c     return
c     end




cccccccc the original version of this routine was:

      subroutine get_nparm (aparm,zparm,   nparm)
      implicit none
      real aparm,zparm,a(2)
      integer nparm,location
      save a
c
      nparm=(  location(zparm)-location(aparm)  )/
     $      (  location(a(2)) -location(a(1))   )
      return
      end




ccccccccc another version could be this C routine:

c      void get_nparm_(float* aparm, float* zparm, long* nparm)
c      {
c          *nparm = zparm - aparm;
c      }


