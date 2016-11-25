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
C    Primitive name:  ALLOC_WORDS              (allocate words of memory)
C  Source directory:  primitives/memory
C           Library:  conlib
C           Written:  92/07/22   by:  Tom Stoeckley
C      Last revised:  92/07/22   by:  Tom Stoeckley
C
C  Purpose:   Dynamically allocate or re-allocate a desired number of 
C             elements of memory for an array of any desired type.  Also
C             you can free the memory which you allocated.
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
C  To dynamically allocate memory:
C                             o    i
c       call alloc_words   (ipoint,n)     ! reals and integers
c       call alloc_doubles (ipoint,n)     ! double precision
c       call alloc_bytes   (ipoint,n)     ! bytes (single characters)
c
C  integer n       = number of elements of the specified variable type
C                      to allocate.
C  integer ipoint  = pointer to allocated memory (returned).
C
C  Note that, for character arrays, n is the number of bytes or
c  single characters to allocate.  This is equal to the number of
C  character variables times the length of each variable.
C-----------------------------------------------------------------------
C  To re-allocate memory previously allocated by one of the above calls:
C                               b    i
c       call realloc_words   (ipoint,n)     ! reals and integers
c       call realloc_doubles (ipoint,n)     ! double precision
c       call realloc_bytes   (ipoint,n)     ! bytes (single characters)
c
C  integer n       = number of elements of the specified variable type
C                      to re-allocate (i.e. the new number required).
C  integer ipoint  = pointer to re-allocated memory (changed).
C-----------------------------------------------------------------------
C  To free memory allocated or re-allocated by one of the above calls:
C                              i
c       call dealloc_memory (ipoint)
c
C  integer ipoint = pointer to allocated memory.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. These routines can serve as ANSI-standard replacements for
C     machine-specific calls such as the Cray HPALLOC and HPDEALLC
C     routines.
C
C  2. Allocated memory will be initialized to binary zeroes.  If a 
C     memory allocation fails, the returned pointer will be zero.
C
C  3. The contents of re-allocated memory are unchanged up to the
C     minimum of the old and new sizes.  If the new size is larger,
C     the new space is not initialized.  If a memory re-allocation 
C     fails, the returned pointer will be zero, and the original pointer 
C     value (which you will have to save in another variable before the 
C     reallocation call) will still point to the un-altered previously 
C     allocated memory.
C
C  4. Note that the arguments and usage of dynamic memory allocation
C     match those for allocating CPS scratch and permanent storage:
C            call getscr (ipoint,n)    ! CPS scratch storage
C            call gets   (ipoint,n)    ! CPS permanent storage
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
C  Functions:        alloc_words      realloc_words      dealloc_memory
C                    alloc_doubles    realloc_doubles
C                    alloc_bytes      realloc_bytes
C  Include files:    none
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C
C                    calloc    realloc    free
C-----------------------------------------------------------------------
C\END DOC
*/

#include "c2f_interface.h"

#include <stdlib.h>

#if (VMS || _AIX || __hpux)
#define alloc_words_         alloc_words
#define alloc_doubles_       alloc_doubles
#define alloc_bytes_         alloc_bytes
#define realloc_words_       realloc_words
#define realloc_doubles_     realloc_doubles
#define realloc_bytes_       realloc_bytes
#define dealloc_memory_      dealloc_memory
#endif

#ifdef NEED_CAPITALS
#define alloc_words_         ALLOC_WORDS
#define alloc_doubles_       ALLOC_DOUBLES
#define alloc_bytes_         ALLOC_BYTES
#define realloc_words_       REALLOC_WORDS
#define realloc_doubles_     REALLOC_DOUBLES
#define realloc_bytes_       REALLOC_BYTES
#define dealloc_memory_      DEALLOC_MEMORY
#define double               float         /* see note below */
#endif


/* we assume here that size of float = size of int */


void alloc_words_(void** ipoint, int* number)
{      *ipoint = calloc((size_t)1, (size_t)(*number * sizeof(float)));  }

void alloc_doubles_(void** ipoint, int* number)
{      *ipoint = calloc((size_t)1, (size_t)(*number * sizeof(double))); }

void alloc_bytes_(void** ipoint, int* number)
{      *ipoint = calloc((size_t)1, (size_t)*number);                    }



void realloc_words_(void** ipoint, int* number)
{      *ipoint = realloc(*ipoint, (size_t)(*number * sizeof(float)));  }

void realloc_doubles_(void** ipoint, int* number)
{      *ipoint = realloc(*ipoint, (size_t)(*number * sizeof(double))); }

void realloc_bytes_(void** ipoint, int* number)
{      *ipoint = realloc(*ipoint, (size_t)*number);                    }



void dealloc_memory_(void** ipoint)
{      free(*ipoint);   }
 


/*
   Type double precision is the same as type real in Cray Unicos Fortran,
   whereas type double is not same as type float in Cray Unicos C-language.
*/
 
