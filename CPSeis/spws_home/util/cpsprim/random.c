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
C    Primitive name:  RANDOM            (get a random number)
C  Source directory:  primitives/memory
C           Library:  conlib         
C           Written:  95/04/12  by:  Tom Stoeckley
C      Last revised:  95/04/12  by:  Tom Stoeckley
C
C  Purpose:   Get a random number (floating point value between 0.0 and
C             1.0).  The function RANDOM should be used instead of RANF
C             or RAN, which appear not to be ANSI-standard.  The
C             functions RANDOM and RANDOM_SEED interface to the
C             standard C functions RAND and SRAND, respectively.
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
C  To get a random number:
C             o  
c            rnum = random()
c
C  real rnum = random number between 0.0 and 1.0.
C
C-----------------------------------------------------------------------
C  To set a new seed for random number generation:
C                               i
c            call random_seed (seed)
c
C  integer seed = a positive integer (greater than zero).
C
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. These routines should be used instead of non-ANSI routines such
C     as RAN and RANF found on some machines.
C
C  2. The function RANDOM, like RAN and RANF, returns a floating point
C     number between 0.0 and 1.0, whereas the C function it interfaces
C     to returns a random integer between 0 and RAND_MAX.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2.
C  1. 95/04/12  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C                       ROUTINES IN THIS PRIMITIVE
C
C  Functions:        random      random_seed
C  Include files:    none
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C
C                           rand  srand
C-----------------------------------------------------------------------
C\END DOC
*/

#include "c2f_interface.h"

#include <stdlib.h>

#if (VMS || _AIX || __hpux)
#define random_           random
#define random_seed_      random_seed
#endif

#ifdef NEED_CAPITALS
#define random_           RANDOM
#define random_seed_      RANDOM_SEED
#endif

#ifdef sun
#include "spws_sunos.h"
#endif


float random_(void)
{
  int inum = rand();
  float rnum = (float)inum / (float)RAND_MAX;
  return rnum;
}


void random_seed_(int seed)
{
  srand((unsigned int)seed);
}



