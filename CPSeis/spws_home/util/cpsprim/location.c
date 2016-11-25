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
C    Primitive name:  LOCATION           (get the location of a variable)
C  Source directory:  primitives/memory   
C           Library:  conlib         
C           Written:  92/07/22  by:  Tom Stoeckley
C      Last revised:  92/07/22  by:  Tom Stoeckley
C
C  Purpose:   Get the location (address) of a variable of any type.
C             The returned location is a pointer to the variable.
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
C  To get the location of a variable:
C         o                  i
c       ipoint = location  (var)       ! any non-character variable
c       ipoint = clocation (var)       ! character variable
c
C  var            = variable whose location you want.
C  integer ipoint = location of variable (returned).
C
C  These are both integer functions.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. These routines should be used instead of non-ANSI routines such
C     as LOC and %LOC found on some machines.
C
C  2. The separate routine for character variables is needed for
C     machine independence, because character variables are sometimes
C     passed differently from non-character variables.  For example,
C     on VMS machines, character variables are passed by descriptor
C     rather than by reference (i.e. by addess).
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
C  Functions:        location     clocation
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

#ifdef VMS
#include <descrip.h>
#endif

#if (VMS || _AIX || __hpux)
#define location_      location
#define clocation_     clocation
#endif

#ifdef NEED_CAPITALS
#define location_      LOCATION
#define clocation_     CLOCATION      
#endif



long location_(void* value)    { return (long)value; }


long clocation_(char* value)
{
#ifdef VMS
    struct dsc$descriptor_s *descriptor = (struct dsc$descriptor_s*)value;
    return (long)descriptor->dsc$a_pointer;
#else
    return (long)value;
#endif
}


