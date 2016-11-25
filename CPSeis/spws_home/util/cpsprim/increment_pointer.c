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
C    Primitive name:  INCREMENT_POINTER           (increment pointer)
C  Source directory:  primitives/memory
C           Library:  CONLIB         
C           Written:  92/07/22  by:  Tom Stoeckley
C      Last revised:  92/07/22  by:  Tom Stoeckley
C
C  Purpose:   Increment the pointer to an integer variable, a real
C             variable, or a double precision variable by a specified
C             number of words.  Or increment the pointer to a character
C             by a specified number of characters (bytes).
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
C  To increment the pointer to an integer variable:
C  To increment the pointer to a real variable:
C  To increment the pointer to a double precision variable:
C  To increment the pointer to a byte (single character):
C         o                                   i       i
c       jpoint = increment_integer_pointer (ipoint,increment)
c       jpoint = increment_real_pointer    (ipoint,increment)
c       jpoint = increment_double_pointer  (ipoint,increment)
c       jpoint = increment_byte_pointer    (ipoint,increment)
c
C  integer ipoint    = pointer to increment.
C  integer increment = number of units to increment (negative to decrement).
C  integer jpoint    = new pointer (returned).
C
C  These are all integer functions.
C  The number of units is the number of words (of the specified type)
C    for the first three routines, and the number of bytes (or single
C    characters) for the last routine.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. Note that you cannot simply add/subtract an integer to/from a
C     pointer in Fortran, since pointers are byte addresses on some 
C     machines (most 32-bit machines), and word addresses (with a byte
C     offset in the most significant digits) on others (such as the
C     Cray).  Therefore, you should use these routines, which use 
C     C-language pointer arithmetic.
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
C  Functions:        increment_integer_pointer
C                    increment_real_pointer
C                    increment_double_pointer
C                    increment_byte_pointer
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


#if(VMS || _AIX || __hpux)
#define increment_integer_pointer_ increment_integer_pointer
#define increment_real_pointer_    increment_real_pointer
#define increment_double_pointer_  increment_double_pointer
#define increment_byte_pointer_    increment_byte_pointer
#endif

#ifdef NEED_CAPITALS
#define increment_integer_pointer_ INCREMENT_INTEGER_POINTER
#define increment_real_pointer_    INCREMENT_REAL_POINTER 
#define increment_double_pointer_  INCREMENT_DOUBLE_POINTER
#define increment_byte_pointer_    INCREMENT_BYTE_POINTER
#define double                     float          /* see note below */
#endif


long increment_integer_pointer_(long** ipoint, long* increment)
{    return (long)( (*ipoint) + (*increment) );    }

long increment_real_pointer_(float** ipoint, long* increment)
{    return (long)( (*ipoint) + (*increment) );    }

long increment_double_pointer_(double** ipoint, long* increment)
{    return (long)( (*ipoint) + (*increment) );    }

long increment_byte_pointer_(char** ipoint, long* increment)
{    return (long)( (*ipoint) + (*increment) );    }

/* 
   Type double precision is the same as type real in Cray Unicos Fortran, 
   whereas type double is not same as type float in Cray Unicos C-language.
*/


