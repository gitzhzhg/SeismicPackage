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
C    Primitive name:  SUBTRACT_POINTERS           (subtract pointers)
C  Source directory:  primitives/memory
C           Library:  CONLIB         
C           Written:  92/07/22  by:  Tom Stoeckley
C      Last revised:  92/07/22  by:  Tom Stoeckley
C
C  Purpose:   Get the difference between two pointers to integer 
C             variables, real variables, double precision variables,
C             ir bytes (single characters).
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
C  To get the number of integer words between two pointers:
C  To get the number of real words between two pointers:
C  To get the number of double precision words between two pointers:
C  To get the number of bytes (single characters) between two pointers:
C          o                                   i      i
C       ioffset = subtract_integer_pointers (jpoint,ipoint)
C       ioffset = subtract_real_pointers    (jpoint,ipoint)
C       ioffset = subtract_double_pointers  (jpoint,ipoint)
C       ioffset = subtract_byte_pointers    (jpoint,ipoint)
c
C  integer jpoint  = first pointer in statement jpoint-ipoint.
C  integer ipoint  = second pointer in statement jpoint-ipoint.
C  integer ioffset = difference (number of units) between the pointers.
C
C  These are all integer functions.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. Note that you cannot simply subtract one pointer from another
C     to get the difference (number of words) between them, since
C     since pointers are byte addresses on some machines (most 32-bit 
C     machines), and word addresses (with a byte offset in the most 
C     significant digits) on others (such as the Cray).  Therefore, 
C     you should use these routines, which use C-language pointer 
C     arithmetic.
C
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2.
C  1. 92/07/22  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C                       ROUTINES IN THIS PRIMITIVE
C
C  Functions:        subtract_integer_pointers
C                    subtract_real_pointers
C                    subtract_double_pointers
C                    subtract_byte_pointers
C  Include files:    none
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C
C                                none
C-----------------------------------------------------------------------
C\END DOC
*/

#include "c2f_interface.h"

#include <stdlib.h>


#if (VMS || _AIX || __hpux)
#define subtract_integer_pointers_ subtract_integer_pointers
#define subtract_real_pointers_    subtract_real_pointers
#define subtract_double_pointers_  subtract_double_pointers
#define subtract_byte_pointers_    subtract_byte_pointers
#endif

#ifdef NEED_CAPITALS
#define subtract_integer_pointers_ SUBTRACT_INTEGER_POINTERS
#define subtract_real_pointers_    SUBTRACT_REAL_POINTERS
#define subtract_double_pointers_  SUBTRACT_DOUBLE_POINTERS
#define subtract_byte_pointers_    SUBTRACT_BYTE_POINTERS
#define double                     float          /* see note below */
#endif


int subtract_integer_pointers_(int** jpoint, int** ipoint)
{    return (int)( (*jpoint) - (*ipoint) );    }

int subtract_real_pointers_(float** jpoint, float** ipoint)
{    return (int)( (*jpoint) - (*ipoint) );    }

int subtract_double_pointers_(double** jpoint, double** ipoint)
{    return (int)( (*jpoint) - (*ipoint) );    }

int subtract_byte_pointers_(char** jpoint, char** ipoint)
{    return (int)( (*jpoint) - (*ipoint) );    }

/* 
   Type double precision is the same as type real in Cray Unicos Fortran, 
   whereas type double is not same as type float in Cray Unicos C-language.
*/


