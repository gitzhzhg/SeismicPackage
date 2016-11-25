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

//---------------------- local_value.cc -------------------------//
//---------------------- local_value.cc -------------------------//
//---------------------- local_value.cc -------------------------//

//          implementation file for the LocalValue class
//                  not derived from any class
//                      subdirectory oprim


   // this class stores a value which might be any one of
   // several different data types.  when created, this class
   // contains an integer with value zero.  at any time,
   // a new value of any type can be stored, replacing the
   // previous value.  an attempt to retrieve a value of the
   // wrong type will assert.


#include "oprim/local_value.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>




//---------------- constructor -------------------------------//
//---------------- constructor -------------------------------//
//---------------- constructor -------------------------------//

LocalValue::LocalValue()
         :
               _type       (_IVAR)
{
  _var.ivar = 0;
}



//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//


LocalValue::~LocalValue()
{
  privateFree();
}



//---------------------- private free ------------------------//
//---------------------- private free ------------------------//
//---------------------- private free ------------------------//

           // private.

void LocalValue::privateFree()
{
  if(_type >= 8)
      {
      assert(_var.avar);
      free(_var.avar);
      }
}



//----------------------- get values -------------------------//
//----------------------- get values -------------------------//
//----------------------- get values -------------------------//

              // public.

int LocalValue::getIvar() const
{
  assert(_type == _IVAR);
  return _var.ivar;
}


float LocalValue::getFvar() const
{
  assert(_type == _FVAR);
  return _var.fvar;
}


double LocalValue::getDvar() const
{
  assert(_type == _DVAR);
  return _var.dvar;
}


const char *LocalValue::getCvar() const
{
  assert(_type >= 0);
  if(_type >= 8) return _var.avar;
  return _var.cvar;
}
  

void
LocalValue::getValue(int *ivar, float *fvar, double *dvar, char *cvar)
const
{
                   if(ivar) *ivar    =    0;
                   if(fvar) *fvar    =  0.0;
                   if(dvar) *dvar    =  0.0;
                   if(cvar)  cvar[0] = '\0';
  switch(_type)
     {
     case _IVAR:  assert(ivar); *ivar = _var.ivar;       break;
     case _FVAR:  assert(fvar); *fvar = _var.fvar;       break;
     case _DVAR:  assert(dvar); *dvar = _var.dvar;       break;
     default:     assert(cvar); strcpy(cvar, getCvar()); break;
     }
}



//----------------------- set values -------------------------//
//----------------------- set values -------------------------//
//----------------------- set values -------------------------//

             // public.

void LocalValue::setIvar(int value)
{
  privateFree();
  _type = _IVAR;
  _var.ivar = value;
}


void LocalValue::setFvar(float value)
{
  privateFree();
  _type = _FVAR;
  _var.fvar = value;
}


void LocalValue::setDvar(double value)
{
  privateFree();
  _type = _DVAR;
  _var.dvar = value;
}


void LocalValue::setCvar(const char *value)
{
  assert(value);
  privateFree();
  _type = strlen(value);
  if(_type >= 8)
      {
      _var.avar = (char*)malloc(_type + 1);
      strcpy(_var.avar, value);
      }
  else
      {
      strcpy(_var.cvar, value);
      }
}



//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//

