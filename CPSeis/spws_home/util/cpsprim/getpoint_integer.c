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
C                       CONOCO PROCESSING SYSTEM
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E
C                                c code     
C                  designed to be called from fortran
C
C    Primitive name:  GETPOINT_INTEGER   (get integer from pointer)          
C  Source directory:  primitives/memory
C           Library:  conlib          
C           Written:  92/07/29   by:  Tom Stoeckley
C      Last revised:  92/07/31   by:  Tom Stoeckley
C
C  Purpose:       Get a variable from a location pointed to by a
C                 pointer and an index, or put a variable into
C                 this location.  The variable can be of type integer,
C                 real, or double precision, or it can be a character
C                 variable of any length, or it can be a hollerith
C                 string of any length.
C
C  Related Documentation:  Some of the code documented here resides
C                          in primitive GETPOINT_CHARACTER.
C-----------------------------------------------------------------------
C       THE VARIOUS ROUTINES IN THIS PRIMITIVE ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
C-----------------------------------------------------------------------
C  To get a variable from a pointer location:
C                                   i      i     i     o     i
C        call getpoint_integer   (ipoint,index,       ivar)
C        call getpoint_real      (ipoint,index,       fvar)
C        call getpoint_double    (ipoint,index,       dvar)
C        call getpoint_character (ipoint,index,       cvar)
C        call getpoint_string    (ipoint,index,nchari,hvar,ncharv)
C
C  To put a variable into a pointer location:
C                                  i     i      i      i     i
C        call putpoint_integer   (ivar,       ipoint,index)
C        call putpoint_real      (fvar,       ipoint,index)
C        call putpoint_double    (dvar,       ipoint,index)
C        call putpoint_character (cvar,       ipoint,index)
C        call putpoint_string    (hvar,ncharv,ipoint,index,nchari)
C
C  integer       ivar = value to get or put.
C  real          fvar = value to get or put.
C  double        dvar = value to get or put.
C  character*(*) cvar = value to get or put.
C  integer       hvar = hollerith string to get or put.
C
C  integer     ipoint = pointer to the index=1 location in memory.
C  integer      index = desired indexed location in memory.
C  integer     ncharv = number of characters in hvar.
C  integer     nchari = number of characters in location given by
C                           ipoint and index.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. If you are accessing the location of a scalar in the memory
C     location pointed to by ipoint, you should set index=1.
C
C  2. If you are accessing an element of an array beginning in the
C     memory location pointed to by ipoint, you should set index to
C     the element you want, where index=1 refers to the first element
C     in the array.
C
C  3. If you are accessing a character variable, the length of cvar
C     must match the length of the character element(s) in the memory
C     location pointed to by ipoint.
C
C  4. If you are accessing a hollerith string, the number of characters
C     in hvar (or the available space in hvar) is given by ncharv, and
C     the number of characters (or the available space) in the location
C     specified by ipoint and index is given by nchari.  The number of
C     characters moved will be the minimum of ncharv and nchari.  If
C     the destination location has more room than the origin location,
C     the excess characters will be set to blanks.
C
C  5. The code for GETPOINT_CHARACTER and PUTPOINT_CHARACTER is Fortran
C     code residing in the primitive GETPOINT_CHARACTER.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  3.
C  2. 92/07/31  Stoeckley  Change functions ..._float to ..._real.
C  1. 92/07/29  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C                       ROUTINES IN THIS PRIMITIVE
C
C  Functions:          getpoint_integer    putpoint_integer
C                      getpoint_real       putpoint_real   
C                      getpoint_double     putpoint_double 
C                      getpoint_string     putpoint_string 
C
C  Header files:       stdlib.h
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
#define getpoint_integer_                getpoint_integer
#define putpoint_integer_                putpoint_integer
#define getpoint_real_                   getpoint_real 
#define putpoint_real_                   putpoint_real 
#define getpoint_double_                 getpoint_double 
#define putpoint_double_                 putpoint_double 
#define getpoint_string_                 getpoint_string 
#define putpoint_string_                 putpoint_string 
#endif

#ifdef NEED_CAPITALS
#define getpoint_integer_                GETPOINT_INTEGER
#define putpoint_integer_                PUTPOINT_INTEGER
#define getpoint_real_                   GETPOINT_REAL 
#define putpoint_real_                   PUTPOINT_REAL 
#define putpoint_double_                 PUTPOINT_DOUBLE 
#define getpoint_double_                 GETPOINT_DOUBLE 
#define getpoint_string_                 GETPOINT_STRING 
#define putpoint_string_                 PUTPOINT_STRING 
#define double                           float         /* see note below */
#endif


/*
   Type double precision is the same as type real in Cray Unicos Fortran,
   whereas type double is not same as type float in Cray Unicos C-language.
*/
  

void getpoint_integer_(long** ipoint, long* index, long* value)
{      *value = *((*ipoint)+(*index)-1);   }
void putpoint_integer_(long* value, long** ipoint, long* index)
{      *((*ipoint)+(*index)-1) = *value;   }


void getpoint_real_(float** ipoint, long* index, float* value)
{      *value = *((*ipoint)+(*index)-1);   }
void putpoint_real_(float* value, float** ipoint, long* index)
{      *((*ipoint)+(*index)-1) = *value;   }


void getpoint_double_(double** ipoint, long* index, double* value)
{      *value = *((*ipoint)+(*index)-1);   }
void putpoint_double_(double* value, double** ipoint, long* index)
{      *((*ipoint)+(*index)-1) = *value;   }


void getpoint_string_(char** ipoint, long* index, long* nchari,
                 char* value, long* ncharv)
{      long i, ncharm, index2;
       index2 = (*index-1)*(*nchari);
       if (*nchari > *ncharv) ncharm = *ncharv; else ncharm = *nchari;
       for (i=0; i<*ncharv; i++)   *(value+i) = ' ';        
       for (i=0; i< ncharm; i++)   *(value+i) = *((*ipoint)+index2+i);        
}


void putpoint_string_(char* value, long* ncharv,
                 char** ipoint, long* index, long* nchari)
{      long i, ncharm, index2;
       index2 = (*index-1)*(*nchari);
       if (*nchari > *ncharv) ncharm = *ncharv; else ncharm = *nchari;
       for (i=0; i<*nchari; i++)   *((*ipoint)+index2+i) = ' ';        
       for (i=0; i< ncharm; i++)   *((*ipoint)+index2+i) = *(value+i);        
}

