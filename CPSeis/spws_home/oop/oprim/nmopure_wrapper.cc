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

//-------------------------- nmopure_wrapper.cc ---------------------------//
//-------------------------- nmopure_wrapper.cc ---------------------------//
//-------------------------- nmopure_wrapper.cc ---------------------------//

//           implementation file for the NmopureWrapper class
//                      not derived from any class
//                          subdirectory oprim


#include "oprim/nmopure_wrapper.hh"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//


#if NEED_UNDERSCORE
#define nmopure_frou_create             nmopure_frou_create_
#define nmopure_frou_delete             nmopure_frou_delete_
#define nmopure_frou_velfun             nmopure_frou_velfun_
#define nmopure_frou_apply              nmopure_frou_apply_
#elif NEED_CAPITALS
#define nmopure_frou_create             NMOPURE_FROU_CREATE
#define nmopure_frou_delete             NMOPURE_FROU_DELETE
#define nmopure_frou_velfun             NMOPURE_FROU_VELFUN  
#define nmopure_frou_apply              NMOPURE_FROU_APPLY 
#endif


//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//

extern "C" {

void nmopure_frou_create (F90Pointer *fpoint, const INTEGER *ndpt,
                          const REAL *tstrt, const REAL *dt,
                          const INTEGER *action, const INTEGER *order,
                          const INTEGER *terpmode, const REAL *doppler,
                          const INTEGER *tracemute, INTEGER *error, char *msg);

void nmopure_frou_delete (F90Pointer *fpoint);

void nmopure_frou_velfun (F90Pointer *fpoint,
            const INTEGER *npicks2, const REAL *tpicks2, const REAL *vpicks2,
            const INTEGER *npicks4, const REAL *tpicks4, const REAL *vpicks4,
                          INTEGER *error, char *msg);

void nmopure_frou_apply  (F90Pointer *fpoint, REAL *offset,
                          REAL *offnew, const INTEGER *ndpt,
                          REAL *tr, INTEGER *mtop, INTEGER *mbottom);

}   // end extern "C"



//------------------ constructor and destructor -----------------------//
//------------------ constructor and destructor -----------------------//
//------------------ constructor and destructor -----------------------//


NmopureWrapper::NmopureWrapper (int ndpt, float tstrt, float dt, int action,
                                int order, int terpmode, float doppler,
                                int tracemute, int *error, char *msg)
              :
                  _ndpt   (ndpt)
{
  assert(sizeof(INTEGER) == sizeof(int  ));          // to simplify coding.
  assert(sizeof(REAL   ) == sizeof(float));          // to simplify coding.

  nmopure_frou_create (&_fpoint, &ndpt, &tstrt, &dt, &action, &order,
                       &terpmode, &doppler, &tracemute, error, msg);
}



NmopureWrapper::~NmopureWrapper()
{
  nmopure_frou_delete(&_fpoint);
}


//----------------------------- velfun ------------------------------------//
//----------------------------- velfun ------------------------------------//
//----------------------------- velfun ------------------------------------//


void NmopureWrapper::velfun
               (int npicks2, const float *tpicks2, const float *vpicks2,
                int npicks4, const float *tpicks4, const float *vpicks4,
                int *error, char *msg)
{
  nmopure_frou_velfun (&_fpoint, &npicks2, tpicks2, vpicks2,
                                 &npicks4, tpicks4, vpicks4, error, msg);
}


//----------------------------- apply ------------------------------------//
//----------------------------- apply ------------------------------------//
//----------------------------- apply ------------------------------------//


void NmopureWrapper::apply (float *offset, float *offnew, float *tr,
                            int *mtop, int *mbottom)
{
  nmopure_frou_apply (&_fpoint, offset, offnew, &_ndpt, tr, mtop, mbottom);
}


//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//

