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

//-------------------------- vf_edit_coords.cc ---------------------------//
//-------------------------- vf_edit_coords.cc ---------------------------//
//-------------------------- vf_edit_coords.cc ---------------------------//

//            implementation file for the VfEditCoords class
//                  derived from the VfEditBase class
//                         subdirectory vf


#include "vf/vf_edit_coords.hh"
#include "vf/vf_kernal.hh"
#include "vf/vf_function.hh"
#include "oprim/grid_transform.hh"
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


VfEditCoords::VfEditCoords()
           : VfEditBase(INFORM_TOTAL, "edit coords"),
             _transform           (NULL),
             _direction           (DIRECTION_FORWARD)
{
  _transform = new GridTransform();
}



VfEditCoords::~VfEditCoords()
{
  delete _transform;
}



//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//


void   VfEditCoords::setDirection (int direction)
{
  assert(direction == DIRECTION_FORWARD ||
         direction == DIRECTION_REVERSE ||
         direction == INTEGERIZE        ||
         direction == SWAP_COORDS       ||
         direction == SWAP_COORDS_AND_HEADERS);
  _direction = direction;
}



//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//
//---------------------- virtual check -------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditCoords::virtualCheck (VfKernal* /*kernal*/, char *msg)
{
  strcpy(msg, "editing velocity function coords...");
  return FALSE;
}



//----------------------- virtual edit ----------------------------------//
//----------------------- virtual edit ----------------------------------//
//----------------------- virtual edit ----------------------------------//

        // public virtual function overriding VfEditBase.

int VfEditCoords::virtualEdit (VfKernal *kernal, char *msg)
{
  long nfun = kernal->numVelocityFunctions();
  long kount = 0;
  for(long ifun = 0; ifun < nfun; ifun++)
      {
      if(reject(kernal, ifun, NULL)) continue;
      VfFunction *velfun = kernal->velfun(ifun);
      float xloc = velfun->getXloc();
      float yloc = velfun->getYloc();
      float xnew;
      float ynew;
      if(_direction == DIRECTION_FORWARD)
          {
          xnew = _transform->getXgridCoordFromFloats(xloc, yloc);
          ynew = _transform->getYgridCoordFromFloats(xloc, yloc);
          }
      else if(_direction == DIRECTION_REVERSE)
          {
          xnew = _transform->getXlocCoordFromFloats(xloc, yloc);
          ynew = _transform->getYlocCoordFromFloats(xloc, yloc);
          }
      else if(_direction == INTEGERIZE)
          {
          xnew = (float)NearestInteger(xloc);
          ynew = (float)NearestInteger(yloc);
          }
      else       // SWAP_COORDS or SWAP_COORDS_AND_HEADERS
          {
          xnew = yloc;
          ynew = xloc;
          }
      velfun->setXloc(xnew);
      velfun->setYloc(ynew);
      kount++;
      }
  if(_direction == SWAP_COORDS_AND_HEADERS)
      {
      int nhx = kernal->getNhx();
      int nhy = kernal->getNhy();
      kernal->setNhx(nhy);
      kernal->setNhy(nhx);
      }
  sprintf(msg, "coords edited on %ld velocity functions", kount);
  return FALSE;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

