/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
/*
C***************************** COPYRIGHT NOTICE ********************************
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
C                     SEISMIC PROCESSING WORKSTATION
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E
C                                c code
C                  designed to be called from fortran
C
C    Primitive name:  GET_MACHINE            (get machine)
C  Source directory:  primitives/misc
C           Library:  conlib         
C           Written:  94/11/29  by:  Tom Stoeckley
C      Last revised:  97/12/12  by:  R.S.Day
C
C  Purpose:   This routine returns an integer which represents the
C             machine on which the program is running.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C       THE VARIOUS ROUTINES IN THIS PRIMITIVE ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
C-----------------------------------------------------------------------
C  To get the machine on which the program is running:
C
c                 imachine = get_machine()
C        
C  integer imachine = one of the following values (returned).
C
C          imachine         explanation
C          --------         -----------
C             1             CRAY PVP
C             2             DEC VMS
C             3             DEC ultrix
C             4             IBM
C             5             SUN
C             6             HP
C             7             SGI
C             8             CRAYMPP
C             0             some other machine
C
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2. 97/12/12  Day        More machines recognized
C  1. 94/11/29  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C                       ROUTINES IN THIS PRIMITIVE
C
C  Functions:        get_machine
C  Include files:    none
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C
C                              none
C-----------------------------------------------------------------------
C\END DOC
*/
#include "c2f_interface.h"

#if (VMS || _AIX || __hpux)
#define get_machine_             get_machine
#endif

#ifdef NEED_CAPITALS
#define get_machine_             GET_MACHINE
#endif


#ifdef _CRAY1
#define imachine 1
#elif VMS
#define imachine 2
#elif ultrix
#define imachine 3
#elif _AIX
#define imachine 4
#elif sun
#define imachine 5
#elif __hpux
#define imachine 6
#elif __sgi
#define imachine 7
#elif _CRAYMPP
#define imachine 8
#elif i686
#define imachine 9
#elif i386
#define imachine 9
#else
#define imachine 0
#endif


int get_machine_()
{
  return (int)imachine;
}



