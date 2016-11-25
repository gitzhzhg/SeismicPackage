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

//------------------------ vf_edit_nearsample.cc ---------------------------//
//------------------------ vf_edit_nearsample.cc ---------------------------//
//------------------------ vf_edit_nearsample.cc ---------------------------//

//            implementation file for the VfEditNearsample class
//                  derived from the VfEditBase class
//                         subdirectory vf


#include "vf/vf_edit_nearsample.hh"
#include "vf/vf_edit_sort.hh"
#include "vf/vf_edit_names.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_kernal.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_function.hh"
#include "vf/vf_constants.hh"
#include "trslib.h"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>



//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//


VfEditNearsample::VfEditNearsample(VfUtilities *utilities)
           : VfEditBase(INFORM_TOTAL, "resample to grid"),
             _utilities           (utilities),
             _type                (VTNM),
             _which_abscissae     (VfKernal::NEAREST_ABSCISSAE),
             _x1                  (0.0),
             _y1                  (0.0),
             _xinc                (100.0),
             _yinc                (100.0),
             _nx                  (11),
             _ny                  (11)
{
  assert(_utilities);
}



VfEditNearsample::~VfEditNearsample()
{
}



//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//

       // public.

void   VfEditNearsample::setResampleType   (int   value)
{
  assert(!VfUtilities::abscissaIsThickness(value));
  _type = value;
}


void   VfEditNearsample::setWhichAbscissae   (int   value)
{
  assert(value == VfKernal::NEAREST_ABSCISSAE ||
         value == VfKernal::RESTRICTED_ABSCISSAE);
  _which_abscissae = value;
}


void   VfEditNearsample::setX1             (float value)
{
  _x1 = value;
}


void   VfEditNearsample::setY1             (float value)
{
  _y1 = value;
}


void   VfEditNearsample::setXinc           (float value)
{
  if(value <= 0.0) return;
  _xinc = value;
}


void   VfEditNearsample::setYinc           (float value)
{
  if(value <= 0.0) return;
  _yinc = value;
}


void   VfEditNearsample::setNx          (long  value)
{
  if(value <= 0) return;
  _nx = value;
}


void   VfEditNearsample::setNy          (long  value)
{
  if(value <= 0) return;
  _ny = value;
}


void   VfEditNearsample::setXend        (float value)
{
  if(value < _x1) return;
  _nx = NearestInteger((value - _x1) / _xinc) + 1;
}


void   VfEditNearsample::setYend        (float value)
{
  if(value < _y1) return;
  _ny = NearestInteger((value - _y1) / _yinc) + 1;
}



//-------------------------- virtual check ---------------------------//
//-------------------------- virtual check ---------------------------//
//-------------------------- virtual check ---------------------------//

        // public virtual function overriding VfEditBase.

int VfEditNearsample::virtualCheck(VfKernal *kernal, char *msg)
{
  if(kernal->numVelocityFunctionsWithTypeErrors(_type) > 0)
      {
      strcpy(msg, "some velocity functions have pre-existing errors"); 
      return TRUE;
      }

/*****
////////// needed only if all picks must match:
  float tolerance = 0.001;
  if(_utilities->abscissaIsDepth(_type)) tolerance = 10.0;
  float timedepth[MAXPICKS];
  long npicks = kernal->velfun(0)->numPicks(); 
  for(long ipick = 0; ipick < npicks; ipick++)
      {
      timedepth[ipick] = kernal->velfun(0)->getAbscissa(ipick, _type);
      }

  long nfun = kernal->numVelocityFunctions();
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      VfFunction *velfun = kernal->velfun(ifun);
      long npicks2 = velfun->numPicks();
      if(npicks2 != npicks)
          {
          strcpy(msg, "all velocity functions must have same number of picks"); 
          return TRUE;
          }
      for(long ipick = 0; ipick < npicks; ipick++)
          {
          float abscissa = velfun->getAbscissa(ipick, _type);
          if(AbsoluteValue(abscissa - timedepth[ipick]) >= tolerance)
              {
              if(_utilities->abscissaIsTime(_type))
                strcpy(msg, "velocity functions do not have same time values");
              else
                strcpy(msg, "velocity functions do not have same depth values");
              return TRUE;
              }
          }
      }
  if(npicks == 0)
      {
      strcpy(msg, "velocity functions have no picks"); 
      return TRUE;
      }
*****/

  strcpy(msg, "resampling velocity functions to a grid...");
  return FALSE;
}



//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//
//----------------------- virtual edit ---------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditNearsample::virtualEdit (VfKernal *kernal, char *msg)
{
  VfInformer *informer = kernal->informer();
  long ix, iy, index;

////////// calculate resampled velocities.

  informer->showMessage("calculating resampled velocities...");
  float abscissae[MAXPICKS];
  float ordinates[MAXPICKS];
  VfFunction **functions = new VfFunction * [_ny * _nx];
  index = 0;
  for(iy = 0; iy < _ny; iy++)
      {
  for(ix = 0; ix < _nx; ix++)
      {
      float xloc = _x1 + ix * _xinc;
      float yloc = _y1 + iy * _yinc;
      float xnorm = 1.0;
      float ynorm = 1.0;
      long npicks = kernal->getInterpolatedVelocityFunction
             (_which_abscissae,
              abscissae, ordinates, xloc, yloc, _type, TRUE, xnorm, ynorm);
      functions[index] = new VfFunction(_utilities);
      functions[index]->setXloc(xloc);
      functions[index]->setYloc(yloc);
      functions[index]->setDefaultType(_type);
      functions[index]->resetNumPicks(npicks, abscissae, ordinates, _type);
      index++;
      }
      }

////////// create new velocity functions.

  informer->showMessage("creating new velocity functions...");
  kernal->resetNumVelocityFunctions(_nx * _ny);
  index = 0;
  for(iy = 0; iy < _ny; iy++)
      {
  for(ix = 0; ix < _nx; ix++)
      {
      VfFunction *velfun = kernal->velfun(index);
      kernal->velfun(index)->copyVelocityFunctionContents(functions[index]);
      delete functions[index];
      index++;
      }
      }
  delete [] functions;

////////// sort functions and reset names of functions.

  VfEditSort *sort = new VfEditSort();
  int error = sort->checkForErrors(kernal, msg);
  informer->showMessage(msg);
  if(!error) error = sort->editKernal(kernal, msg);
  informer->showMessage(msg);
  delete sort;

  VfEditNames *names = new VfEditNames();
  error = names->checkForErrors(kernal, msg);
  informer->showMessage(msg);
  if(!error) error = names->editKernal(kernal, msg);
  informer->showMessage(msg);
  delete names;

////////// finished.

  long kounte = kernal->numVelocityFunctionsWithErrors();
  if(kounte > 0)
      {
      sprintf(msg, "velocity functions resampled to grid with %d errors",
                                              kounte);
      }
  else
      {
      sprintf(msg, "velocity functions successfully resampled to grid");
      }
  return (kounte > 0);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

