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
!!!        Primitive C++ Wrapper Template for Fortran to C++ Interfaces
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
!!! See c2f_interface.h (and below) for details.
!!!
!!! This primitive named ANDREW_WRAPPER is a working example of a C++ style
!!! wrapper class which interfaces to the Fortran-style class named ANDREW.
!!!
!!!-----------------------------------------------------------------------------
!!!
!!! The primitives ANDREW_WRAPPER and ANDREW do no useful work.
!!! The primitives ANDREW_WRAPPER and ANDREW include following illustrations:
!!!
!!!  (1) use of a creatable/deletable Fortran90 class with a hidden data
!!!        structure.
!!!  (2) capability of the C++ code to store Fortran90 pointer to the Fortran90
!!!        object.
!!!  (3) conversions of variables between Fortran and C++ variable types.
!!!  (4) dealing correctly with char strings passed between C++ and Fortran.
!!!
!!! The following files are used (in the order called):
!!!
!!!   andrew_wrapper.hh    C++ wrapper class header file.
!!!   andrew_wrapper.cc    C++ wrapper class implementation file.
!!!   andrew_frou.f90      Private auxiliary file.
!!!   andrew.f90           File containing the original Fortran class.
!!!
!!! To use the original Fortran class ANDREW from C++, call this C++ wrapper
!!! class ANDREW_WRAPPER, which looks like any other C++ class.  For example:
!!!
!!!          #include "andrew_wrapper.hh"
!!!          main()
!!!          {
!!!            AndrewWrapper *andrew = new AndrewWrapper();
!!!            andrew->solve();
!!!            delete andrew;
!!!          }
!!!
!!!-----------------------------------------------------------------------------
****/


/****
!<CPS_v1 type="PRIMITIVE"/>
****/
//--------------------------- andrew_wrapper.cc ---------------------------//
//--------------------------- andrew_wrapper.cc ---------------------------//
//--------------------------- andrew_wrapper.cc ---------------------------//

    // other files are:  andrew_wrapper.hh  andrew.f90  andrew_frou.f90

/****


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : ANDREW_WRAPPER
! Category   : --> should match the main file of this primitive.
! Written    : 2001-01-01   by: NNNN
! Revised    : 2001-01-01   by: NNNN
! Maturity   : beta
! Purpose    : --> should match the main file of this primitive.
! Portability: No known limitations.
!
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
!                 o
!               andrew = new AndrewWrapper();
!
!                   o                    o    i    b
!                  iii = andrew->solve (fff, ddd, ccc);
!
!                        delete andrew;
!
! AndrewWrapper *andrew = pointer to the ANDREW_WRAPPER object.
! float          fff[]  = description of this output array.
! double         ddd    = description of this input variable.
! char          *ccc    = description of this input/output string.
! int            iii    = description of this returned variable.
!
! fff must be dimensioned at least [ANDREW_WRAPPER_NFFF].
! ccc must be dimensioned at least [ANDREW_WRAPPER_NCCC].
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


char ANDREW_WRAPPER_IDENT[100] =
"$Id: andrew_wrapper.cc,v 1.3 2002/10/24 13:03:18 Stoeckley custom sps $";

#include "andrew_wrapper.hh"   // need to prefix file name with a subdirectory.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


//---------------------- fortran spelling adjustments ----------------------//
//---------------------- fortran spelling adjustments ----------------------//
//---------------------- fortran spelling adjustments ----------------------//


#if NEED_UNDERSCORE
#define andrew_frou_create    andrew_frou_create_
#define andrew_frou_delete    andrew_frou_delete_
#define andrew_frou_solve     andrew_frou_solve_
#elif NEED_CAPITALS
#define andrew_frou_create    ANDREW_FROU_CREATE
#define andrew_frou_delete    ANDREW_FROU_DELETE
#define andrew_frou_solve     ANDREW_FROU_SOLVE
#endif


//-------------------------- fortran prototypes ----------------------------//
//-------------------------- fortran prototypes ----------------------------//
//-------------------------- fortran prototypes ----------------------------//


extern "C"
{
  void andrew_frou_create (F90Pointer *fpoint);
  void andrew_frou_delete (F90Pointer *fpoint);
  int  andrew_frou_solve  (F90Pointer *fpoint,
                              REAL fff[], DOUBLE *ddd, char *ccc);
}


//----------------------------- create -------------------------------------//
//----------------------------- create -------------------------------------//
//----------------------------- create -------------------------------------//


AndrewWrapper::AndrewWrapper()
{
  andrew_frou_create(&_fpoint);
}


//----------------------------- delete -------------------------------------//
//----------------------------- delete -------------------------------------//
//----------------------------- delete -------------------------------------//


AndrewWrapper::~AndrewWrapper()
{
  andrew_frou_delete(&_fpoint);
}


//----------------------------- solve -------------------------------------//
//----------------------------- solve -------------------------------------//
//----------------------------- solve -------------------------------------//


int AndrewWrapper::solve (float fff[], double ddd, char *ccc)
{
  REAL   fff9[ANDREW_WRAPPER_NFFF];
  DOUBLE ddd9 = (DOUBLE)ddd;
  int    iii  = (int)andrew_frou_solve(&_fpoint, fff9, &ddd9, ccc);
  for(int i = 0; i < ANDREW_WRAPPER_NFFF; i++) { fff[i] = (float)fff9[i]; }
  return iii;
}


//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//

