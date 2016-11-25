/****
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
!!!
!!!           Primitive C++ Template for Fortran to C Interfaces
!!!
!!!-----------------------------------------------------------------------------
!!!                   REVISION HISTORY FOR THIS TEMPLATE
!!!
!!!     Date        Author     Description
!!!     ----        ------     -----------
!!!  3. 2002-10-23  Stoeckley  Minor history doc and ident string changes.
!!!  2. 2002-05-16  Stoeckley  Add this template revision history.
!!!  1. 2001-10-23  Stoeckley  Initial version.
!!!
!!!-----------------------------------------------------------------------------
!!!
!!! This file can be used as a template for Fortran to C++ interfaces.
!!! See c2f_interface.h (and suki_wrapper.f90) for details.
!!!
!!! This primitive named SUKI is a working example of a C++ style class
!!! which is called from a Fortran-style wrapper class named SUKI_WRAPPER.
!!! See the SUKI_WRAPPER primitive for additional information.
!!!
!!!-----------------------------------------------------------------------------
****/


/****
!<CPS_v1 type=PRIMITIVE"/>
****/
//------------------------------- suki.cc ----------------------------------//
//------------------------------- suki.cc ----------------------------------//
//------------------------------- suki.cc ----------------------------------//
 
       // other files are:  suki.hh  suki_wrapper.f90  suki_crou.cc

/****


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : SUKI
! Category   : --> math, filters, io, etc. (subdirectory)
! Written    : 2001-01-01   by: NNNN
! Revised    : 2001-01-01   by: NNNN
! Maturity   : beta
! Purpose    : --> description for single-line context-sensitive help.
! Portability: No known limitations.
!
!!!  --> Choose the category from this list of subdirectories:
!!!
!!!      character     math          moves      io           velocity
!!!      filters       memory        packs      sorts        miscellaneous
!!!      main_prog     migrations    plot       synthetics
!!!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
!
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!        i = value required upon INPUT.
!        o = value set by the routine upon OUTPUT.
!        b = value BOTH required upon input and changed upon output.
!
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!               o
!             suki = new Suki();
!
!               o                  o    i    b
!              iii = suki->solve (fff, ddd, ccc);
!
!                    delete suki;
!
!
! Suki      *suki  = pointer to the SUKI object.
! float      fff[] = description of this output array.
! double     ddd   = description of this input variable.
! char      *ccc   = description of this input/output string.
! int        iii   = description of this returned variable.
!
! fff must be dimensioned at least [SUKI_NFFF].
! ccc must be dimensioned at least [SUKI_NCCC].
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                            REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2001-01-01  NNNN       Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>
****/


//-------------------------- start of module ------------------------------//
//-------------------------- start of module ------------------------------//
//-------------------------- start of module ------------------------------//


char SUKI_IDENT[100] =
"$Id: suki.cc,v 1.3 2002/10/24 13:11:43 Stoeckley custom sps $";

#include "suki.hh"     // need to prefix the file name with a subdirectory.
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <assert.h>


//------------------------------ create -----------------------------------//
//------------------------------ create -----------------------------------//
//------------------------------ create -----------------------------------//


Suki::Suki()
         :
            _iii    (5),
            _ddd    (123.456789e-33)
{
  for(int i = 0; i < SUKI_NFFF; i++)
       {
       _fff[i] = 123.456 * i;
       }

  strcpy(_ccc, "hello planet");
}


//------------------------------ delete -----------------------------------//
//------------------------------ delete -----------------------------------//
//------------------------------ delete -----------------------------------//


Suki::~Suki()
{
}


//------------------------------- solve ------------------------------------//
//------------------------------- solve ------------------------------------//
//------------------------------- solve ------------------------------------//


int  Suki::solve (float fff[], double ddd, char *ccc)
{
  char temp[SUKI_NCCC];
  int iii;
  int i;

  printf(" received by suki:           ddd = %.11f\n", ddd);
  printf(" received by suki:           ccc = %s   \n", ccc);
  printf("\n");

  for(i = 0; i < SUKI_NFFF; i++)
       {
       fff[i] = _fff[i];
       }

  strcpy(temp, _ccc);
  strcpy(_ccc,  ccc);
  strcpy( ccc, temp);
  _ddd = ddd;
  iii  = _iii;

  printf(" returning from suki:        fff =");

  for(i = 0; i < SUKI_NFFF; i++)
       {
       printf(" %f", fff[i]);
       }

  printf("\n");
  printf(" returning from suki:        ddd = %.11f\n", ddd);
  printf(" returning from suki:        ccc = %s   \n", ccc);
  printf(" returning from suki:        iii = %d   \n", iii);

  return iii;
}


//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//

