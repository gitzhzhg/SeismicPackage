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
C    Primitive name:  SIZEOF_STRING               (get size of string)
C  Source directory:  primitives/character
C           Library:  conlib         
C           Written:  92/07/22  by:  Tom Stoeckley
C      Last revised:  97/12/01  by:  R.S. Day
C
C  Purpose:   Get the size (in bytes or single characters) of a
C             null-terminated hollerith string.  Also get the size 
C             of an integer variable, a real variable, or a double 
C             precision variable (in bytes or single characters) on
C             the current machine.  
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
C  To get the size of a null-terminated hollerith string:
C  To get the size of an integer variable:
C  To get the size of a real variable:
C  To get the size of a double precision variable:
C         o                       i
c       isize = sizeof_string  (buffer)
c       isize = sizeof_integer ()
c       isize = sizeof_real    ()
c       isize = sizeof_double  ()
C        
C  integer buffer(*) = null-terminated hollerith string.
C  integer isize     = size of variable in bytes (returned).
C
C  These are all integer functions.  The last three have no arguments.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2. 97/12/01  Day        sizeof_real changed to siz_real. sizeof_real
C                          is now in fortran and calls siz_real
C  1. 92/07/22  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C                       ROUTINES IN THIS PRIMITIVE
C
C  Functions:        sizeof_string     sizeof_integer
C                    sizeof_real       sizeof_double
C  Include files:    none
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C
C                              strlen
C-----------------------------------------------------------------------
C\END DOC
*/

#include "c2f_interface.h"

#include <stdlib.h>


#if (VMS || _AIX || __hpux)
#define sizeof_string_             sizeof_string
#define sizeof_integer_            sizeof_integer
#define sizeof_real_               sizeof_real
#define siz_real_                  siz_real
#define sizeof_double_             sizeof_double
#endif

#ifdef NEED_CAPITALS
#define sizeof_string_             SIZEOF_STRING
#define sizeof_integer_            SIZEOF_INTEGER
#define sizeof_real_               SIZEOF_REAL
#define siz_real_                  SIZ_REAL
#define sizeof_double_             SIZEOF_DOUBLE
#define double                     float          /* see note below */
#endif


int sizeof_string_ (char *buffer) {  return (int)strlen(buffer);  }
int sizeof_integer_()             {  return sizeof(int);          }
/*
int sizeof_real_   ()             {  return sizeof(float);         }
*/
int sizeof_double_ ()             {  return sizeof(double);        }
int siz_real_(char *a, char *b) {
 int n;
 return n = (b>a) ? b-a : a-b;
}

/* 
   Type double precision is the same as type real in Cray Unicos Fortran, 
   whereas type double is not same as type float in Cray Unicos C-language.
*/


