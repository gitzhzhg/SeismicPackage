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
C    Primitive name:  CHAR_INTO_BUFFER      (put character into buffer)      
C  Source directory:  primitives/character
C           Library:  conlib         
C           Written:  92/07/22  by:  Tom Stoeckley
C      Last revised:  92/07/22  by:  Tom Stoeckley
C
C  Purpose:       Put a single character into a given location in a
C                 hollerith buffer, or get a single character from a
C                 given location in a hollerith buffer.
C
C  Related Documentation:  Needed by primitive CONVERT_CC2HH.
C-----------------------------------------------------------------------
C       THE VARIOUS ROUTINES IN THIS PRIMITIVE ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
C-----------------------------------------------------------------------
C  To put a character into a hollerith buffer:
C  To get a character from a hollerith buffer:
C                               i        i     b
c       call char_into_buffer (cvar,   index, buffer)
c       call char_from_buffer (buffer, index, cvar)
C                               i        i     o
C  character*1 cvar  = character to put into, or get from, buffer.
C  integer index     = position of character in buffer (1 = first character).
C  integer buffer(*) = hollerith buffer containing characters.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
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
C  Functions:          char_into_buffer    char_from_buffer
C  Include files:      none
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
#define char_into_buffer_       char_into_buffer
#define char_from_buffer_       char_from_buffer
#endif

#ifdef NEED_CAPITALS
#define char_into_buffer_       CHAR_INTO_BUFFER
#define char_from_buffer_       CHAR_FROM_BUFFER
#endif


void char_into_buffer_(int* value, int* index, char* buffer)
{       buffer[*index-1] = (char)(*value);   }

void char_from_buffer_(char* buffer, int* index, int* value)
{      *value = (int)buffer[*index-1];   }

