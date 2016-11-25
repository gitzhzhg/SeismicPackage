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
!!!          Auxiliary C++ Template for Fortran to C++ Interfaces
!!!
!!!-----------------------------------------------------------------------------
!!!                   REVISION HISTORY FOR THIS TEMPLATE
!!!
!!!     Date        Author     Description
!!!     ----        ------     -----------
!!!  3. 2002-10-23  Stoeckley  Minor history doc and ident string changes.
!!!  2. 2002-05-16  Stoeckley  Add brief doc section and this template
!!!                             revision history.
!!!  1. 2001-10-23  Stoeckley  Initial version.
!!!
!!!-----------------------------------------------------------------------------
!!!
!!! This file can be used as a template for Fortran to C++ interfaces.
!!! See c2f_interface.h (and suki_wrapper.f90) for details. 
!!!
!!!-----------------------------------------------------------------------------
****/


/****
!<CPS_v1 type="AUXILIARY_FILE"/>
****/
//----------------------------- suki_crou.cc ------------------------------//
//----------------------------- suki_crou.cc ------------------------------//
//----------------------------- suki_crou.cc ------------------------------//

        // other files are:  suki.cc  suki.hh  suki_wrapper.f90

/****


!<brief_doc>
!-------------------------------------------------------------------------------
!                   C P S   A U X I L I A R Y   F I L E
!
! Name       : SUKI_CROU
! Category   : --> should match the main file of this primitive.
! Written    : 2001-01-01   by: NNNN
! Revised    : 2001-01-01   by: NNNN
! Maturity   : beta
! Purpose    : --> should match the main file of this primitive.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY 
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2001-01-01  NNNN       Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


//--------------------------- start of coding ------------------------------//
//--------------------------- start of coding ------------------------------//
//--------------------------- start of coding ------------------------------//


char SUKI_CROU_IDENT[100] =
"$Id: suki_crou.cc,v 1.3 2002/10/24 13:14:46 Stoeckley custom sps $";

#include "suki.hh"
#include "c2f_interface.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


//---------------------- fortran spelling adjustments ----------------------//
//---------------------- fortran spelling adjustments ----------------------//
//---------------------- fortran spelling adjustments ----------------------//


#ifdef NEED_UNDERSCORE
#define suki_crou_create    suki_crou_create_
#define suki_crou_delete    suki_crou_delete_
#define suki_crou_solve     suki_crou_solve_
#elif defined NEED_CAPITALS
#define suki_crou_create    SUKI_CROU_CREATE
#define suki_crou_delete    SUKI_CROU_DELETE
#define suki_crou_solve     SUKI_CROU_SOLVE
#endif

extern "C" {


//------------------------------ create ----------------------------------//
//------------------------------ create ----------------------------------//
//------------------------------ create ----------------------------------//


void suki_crou_create (Suki **cpoint)
{
  *cpoint = new Suki();
}


//------------------------------ delete ----------------------------------//
//------------------------------ delete ----------------------------------//
//------------------------------ delete ----------------------------------//


void suki_crou_delete (Suki **cpoint)
{
  delete *cpoint;
}


//------------------------------ solve ----------------------------------//
//------------------------------ solve ----------------------------------//
//------------------------------ solve ----------------------------------//


INTEGER suki_crou_solve (Suki **cpoint, REAL fff[], DOUBLE *ddd, char *ccc)
{
  int   iii;
  float fff9[SUKI_NFFF];
  int   i;

  iii = (*cpoint)->solve(fff9, (double)(*ddd), ccc);
  for(i = 0; i < SUKI_NFFF; i++) { fff[i] = (REAL)fff9[i]; }
  return (INTEGER)iii;
}


//-------------------------- end of functions -----------------------------//
//-------------------------- end of functions -----------------------------//
//-------------------------- end of functions -----------------------------//


}   // extern "C"


//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//

