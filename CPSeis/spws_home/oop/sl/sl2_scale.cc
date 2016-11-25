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

//---------------------- sl2_scale.cc ---------------------------------//
//---------------------- sl2_scale.cc ---------------------------------//
//---------------------- sl2_scale.cc ---------------------------------//

//         implementation file for the SL2Scale class
//             derived from the SLSmartForm class
//                      subdirectory sl

#include "sl/sl2_scale.hh"
#include <Xm/Form.h>
#include "cprim.h"


//---------------- constructors -----------------------------------//
//---------------- constructors -----------------------------------//
//---------------- constructors -----------------------------------//

SL2Scale::SL2Scale (SLDelay *slparent, char *name, long ident, char *label,
                                  long direction, Boolean drag)
           : SLSmartForm(slparent, name),
              _scale  (NULL),
              _left   (NULL),
              _right  (NULL)
{
  constructorHelper(ident, label, direction, drag);
}


SL2Scale::SL2Scale (Widget   wparent, char *name, long ident, char *label,
                                  long direction, Boolean drag)
           : SLSmartForm(wparent, name),
              _scale  (NULL),
              _left   (NULL),
              _right  (NULL)
{
  constructorHelper(ident, label, direction, drag);
}



//------------------ destructor -------------------------//
//------------------ destructor -------------------------//
//------------------ destructor -------------------------//

SL2Scale::~SL2Scale(void)
{
  delete _scale;
  delete _left;
  delete _right;
}


//---------------------- constructor helper -------------------//
//---------------------- constructor helper -------------------//
//---------------------- constructor helper -------------------//

void SL2Scale::constructorHelper(long ident, char *label,
                                    long direction, Boolean drag)
{
  _scale = new SLpScale(this, "scale", ident, label, direction, drag);
  if(direction == SLpScale::_VERTICAL)
       {
       _left  = new SLpArrow(this, "left" ,-1, SLpArrow::_DOWN);
       _right = new SLpArrow(this, "right", 1, SLpArrow::_UP);
       }
  else
       {
       _left  = new SLpArrow(this, "left" ,-1, SLpArrow::_LEFT);
       _right = new SLpArrow(this, "right", 1, SLpArrow::_RIGHT);
       }

  _left ->setAtrap     (arrowTrap       , this);
  _right->setAtrap     (arrowTrap       , this);
  _left ->setupSenseFun(updateLeftSense , this);
  _right->setupSenseFun(updateRightSense, this);

  if(direction == SLpScale::_VERTICAL)
       {
       attach(_right, this, this,   this,  NULL);
       attach(_scale, this, this, _right, _left);
       attach(_left , this, this,   NULL,  this);
       }
  else
       {
       attach(_left , this ,   NULL, this, this);
       attach(_scale, _left, _right, this, this);
       attach(_right,  NULL,  this , this, this);
       }
}


//--------------------- arrow trap -----------------------//
//--------------------- arrow trap -----------------------//
//--------------------- arrow trap -----------------------//

void SL2Scale::arrowTrap(void *data, long ident)
{
  SL2Scale *gui = (SL2Scale*)data;
  gui->_scale->callItrap(gui->_scale->ivar() + ident);
}



//------------------- update arrow sensitivities ---------------//
//------------------- update arrow sensitivities ---------------//
//------------------- update arrow sensitivities ---------------//

       // must be called AFTER scale is updated

long SL2Scale::updateLeftSense(void *data)
{
  SL2Scale *gui = (SL2Scale*)data;
  return (gui->_scale->ivar() > gui->_scale->imin() &&
          gui->_scale->sense());
}


long SL2Scale::updateRightSense(void *data)
{
  SL2Scale *gui = (SL2Scale*)data;
  return (gui->_scale->ivar() < gui->_scale->imax() &&
          gui->_scale->sense());
}


//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
