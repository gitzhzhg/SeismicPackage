
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
//--------------------- grid_lines.cc ------------------------//
//--------------------- grid_lines.cc ------------------------//
//--------------------- grid_lines.cc ------------------------//

//         implementation file for the GridLines class
//               derived from the FgInform class
//                      subdirectory geom


#include "geom/grid_lines.hh"
#include "geom/field_geometry.hh"
#include "oprim/grid_transform.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


//----------------- constructor and destructor ---------------------//
//----------------- constructor and destructor ---------------------//
//----------------- constructor and destructor ---------------------//

GridLines::GridLines(FieldGeometry *fg)
           :
             FgInform(fg),
                 _transform            (NULL),
                 _transform_drag_init  (NULL),
                 _using_grid_system    (FALSE),
                 _n                    (0),
                 _x                    (NULL),
                 _y                    (NULL)
{
  _transform = new GridTransform();
  _fg->getTestingGridTransformValues(_transform);
}



GridLines::~GridLines()
{
  freePoints();
  if(_transform_drag_init) delete _transform_drag_init;
  delete _transform;
}



//---------------------- post new grid transform -------------------//
//---------------------- post new grid transform -------------------//
//---------------------- post new grid transform -------------------//

    // public virtual functions overriding FgInform.

void GridLines::postNewGridTransform(FieldGeometry *)
{
  assert(!_transform_drag_init);
  //// may not be needed.
}


void GridLines::postNewTestingGridTransform(FieldGeometry *)
{
  assert(!_transform_drag_init);
  freePoints();
  _fg->getTestingGridTransformValues(_transform);
}



//---------------------- small public functions -------------------//
//---------------------- small public functions -------------------//
//---------------------- small public functions -------------------//


void GridLines::saveNewTransform()     // pass-thru to _fg.
{
  assert(!_transform_drag_init);
  _fg->setGridTransformValues();
}



void GridLines::resetTestingTransform()     // pass-thru to _fg.
{
  assert(!_transform_drag_init);
  _fg->resetTestingGridTransformValues();
}



void GridLines::buttonDown()
{
  assert(!_transform_drag_init);
  _transform_drag_init = new GridTransform();
  _transform_drag_init->setGridTransformValues(_transform);
}


void GridLines::buttonMotion()
{
  assert(_transform_drag_init);
  _transform->setGridTransformValues(_transform_drag_init);
}


void GridLines::buttonUp()
{
  assert(_transform_drag_init);
  _transform->setGridTransformValues(_transform_drag_init);
  delete _transform_drag_init;
  _transform_drag_init = NULL;
}



//--------------------- calculate grid lines -------------------//
//--------------------- calculate grid lines -------------------//
//--------------------- calculate grid lines -------------------//

           // public.

void GridLines::calculateGridLines (float x1, float y1,
                                    float x2, float y2, long stride)
{
  if(_using_grid_system)
       calculateUsingGridCoords(x1, y1, x2, y2, x1, y2, x2, y1, stride, FALSE);
  else
       calculateUsingDistCoords(x1, y1, x2, y2, x1, y2, x2, y1, stride, FALSE);
}


void GridLines::calculateActualGridLines (float x1, float y1,
                                          float x2, float y2, long stride)
{
  if(_using_grid_system)
       calculateUsingGridCoords(x1, y1, x2, y2, x1, y2, x2, y1, stride, TRUE);
  else
       calculateUsingDistCoords(x1, y1, x2, y2, x1, y2, x2, y1, stride, TRUE);
}



//----------------- small private functions ------------------//
//----------------- small private functions ------------------//
//----------------- small private functions ------------------//


void GridLines::freePoints()
{
  if(_x) delete [] _x;
  if(_y) delete [] _y;
  _x = NULL;
  _y = NULL;
  _n = 0;
}



void GridLines::fetch()
{
}



void GridLines::save()
{
  disableMessages();
  _fg->setTestingGridTransformValues(_transform);
  enableMessages();
}




void GridLines::maybeConvertToDistance(float *x, float *y)  const
{
  if(_using_grid_system)
      {
      float xloc = (float)_fg->getXlocCoord(*x, *y);
      float yloc = (float)_fg->getYlocCoord(*x, *y);
      *x = xloc;
      *y = yloc;
      }
}



void GridLines::calculateUsingGridCoords
                        (float xgrid1, float ygrid1,
                         float xgrid2, float ygrid2,
                         float xgrid3, float ygrid3,
                         float xgrid4, float ygrid4,
                         long stride, int actual)
{
  float xloc1 = (float)_fg->getXlocCoord(xgrid1, ygrid1);
  float yloc1 = (float)_fg->getYlocCoord(xgrid1, ygrid1);
  float xloc2 = (float)_fg->getXlocCoord(xgrid2, ygrid2);
  float yloc2 = (float)_fg->getYlocCoord(xgrid2, ygrid2);
  float xloc3 = (float)_fg->getXlocCoord(xgrid3, ygrid3);
  float yloc3 = (float)_fg->getYlocCoord(xgrid3, ygrid3);
  float xloc4 = (float)_fg->getXlocCoord(xgrid4, ygrid4);
  float yloc4 = (float)_fg->getYlocCoord(xgrid4, ygrid4);
  calculateUsingDistCoords(xloc1, yloc1,
                           xloc2, yloc2,
                           xloc3, yloc3,
                           xloc4, yloc4, stride, actual);
  for(long i = 0; i < _n; i++)
      {
      float xgrid = (float)_fg->getXgridCoord(_x[i], _y[i]);
      float ygrid = (float)_fg->getYgridCoord(_x[i], _y[i]);
      _x[i] = xgrid;
      _y[i] = ygrid;
      }
}



void GridLines::calculateUsingDistCoords
                        (float xloc1, float yloc1,
                         float xloc2, float yloc2,
                         float xloc3, float yloc3,
                         float xloc4, float yloc4, long stride, int actual)
{
  float xgrid1, ygrid1, xgrid2, ygrid2, xgrid3, ygrid3, xgrid4, ygrid4;
  if(actual)
      {
      xgrid1 = (float)_fg->getXgridCoord(xloc1, yloc1);
      ygrid1 = (float)_fg->getYgridCoord(xloc1, yloc1);
      xgrid2 = (float)_fg->getXgridCoord(xloc2, yloc2);
      ygrid2 = (float)_fg->getYgridCoord(xloc2, yloc2);
      xgrid3 = (float)_fg->getXgridCoord(xloc3, yloc3);
      ygrid3 = (float)_fg->getYgridCoord(xloc3, yloc3);
      xgrid4 = (float)_fg->getXgridCoord(xloc4, yloc4);
      ygrid4 = (float)_fg->getYgridCoord(xloc4, yloc4);
      }
  else
      {
      xgrid1 = (float)_fg->getTestingXgridCoord(xloc1, yloc1);
      ygrid1 = (float)_fg->getTestingYgridCoord(xloc1, yloc1);
      xgrid2 = (float)_fg->getTestingXgridCoord(xloc2, yloc2);
      ygrid2 = (float)_fg->getTestingYgridCoord(xloc2, yloc2);
      xgrid3 = (float)_fg->getTestingXgridCoord(xloc3, yloc3);
      ygrid3 = (float)_fg->getTestingYgridCoord(xloc3, yloc3);
      xgrid4 = (float)_fg->getTestingXgridCoord(xloc4, yloc4);
      ygrid4 = (float)_fg->getTestingYgridCoord(xloc4, yloc4);
      }
  privateGetGridLines(xgrid1, ygrid1,
                      xgrid2, ygrid2,
                      xgrid3, ygrid3,
                      xgrid4, ygrid4, stride);
  for(long i = 0; i < _n; i++)
      {
      float xloc, yloc;
      if(actual)
          {
          xloc = (float)_fg->getXlocCoord(_x[i], _y[i]);
          yloc = (float)_fg->getYlocCoord(_x[i], _y[i]);
          }
      else
          {
          xloc = (float)_fg->getTestingXlocCoord(_x[i], _y[i]);
          yloc = (float)_fg->getTestingYlocCoord(_x[i], _y[i]);
          }
      _x[i] = xloc;
      _y[i] = yloc;
      }
}



//-------------------- adjust range --------------------------//
//-------------------- adjust range --------------------------//
//-------------------- adjust range --------------------------//

#define EXPAND 1.1

static void adjust_range(float xgrid1, float xgrid2,
                         float xgrid3, float xgrid4,
                         long *ix1, long *ix2)
{
  float xmina = MinimumValue(xgrid1, xgrid2);
  float xmaxa = MaximumValue(xgrid1, xgrid2);
  float xminb = MinimumValue(xgrid3, xgrid4);
  float xmaxb = MaximumValue(xgrid3, xgrid4);
  float xmin  = MinimumValue(xmina , xminb );
  float xmax  = MaximumValue(xmaxa , xmaxb );
  float add = 0.5 * (EXPAND - 1.0) * (xmax - xmin) + 0.5;
  *ix1 = NearestInteger(xmin - add) - 1;
  *ix2 = NearestInteger(xmax + add) + 1;
}



//------------------- private get grid lines --------------------//
//------------------- private get grid lines --------------------//
//------------------- private get grid lines --------------------//

           // private.

void GridLines::privateGetGridLines
                        (float xgrid1, float ygrid1,
                         float xgrid2, float ygrid2,
                         float xgrid3, float ygrid3,
                         float xgrid4, float ygrid4,
                         long stride)
{
  assert(stride >= 1);
  freePoints();
  long ix1, iy1, ix2, iy2;
  adjust_range(xgrid1, xgrid2, xgrid3, xgrid4, &ix1, &ix2);
  adjust_range(ygrid1, ygrid2, ygrid3, ygrid4, &iy1, &iy2);
  long ix, iy;
  for(ix = ix1; ix <= ix2; ix += stride) { _n += 2; }
  for(iy = iy1; iy <= iy2; iy += stride) { _n += 2; }
  if(_n == 0) return;

  _x = new float [_n];
  _y = new float [_n];

  long kount = 0;
  int flip = TRUE;
  for(ix = ix1; ix <= ix2; ix += stride)
      {
      flip = !flip;
                        _x[kount] = ix + 0.5;
                        if(flip) _y[kount] = iy1 + 0.5;
                        else     _y[kount] = iy2 + 0.5;
      kount++;
                        _x[kount] = ix + 0.5;
                        if(flip) _y[kount] = iy2 + 0.5;
                        else     _y[kount] = iy1 + 0.5;
      kount++;
      }
  for(iy = iy1; iy <= iy2; iy += stride)
      {
      flip = !flip;
                        _y[kount] = iy + 0.5;
                        if(flip) _x[kount] = ix1 + 0.5;
                        else     _x[kount] = ix2 + 0.5;
      kount++;
                        _y[kount] = iy + 0.5;
                        if(flip) _x[kount] = ix2 + 0.5;
                        else     _x[kount] = ix1 + 0.5;
      kount++;
      }
}



//--------------- change the local transform ---------------------//
//--------------- change the local transform ---------------------//
//--------------- change the local transform ---------------------//

        // public.


void GridLines::defineOrigin(float x, float y)
{
  freePoints();
  fetch();
  maybeConvertToDistance(&x, &y);
  float xgrid = 0.0;
  float ygrid = 0.0;
  _transform->defineOrigin(xgrid, ygrid, x, y);
  save();
}



void GridLines::changeOrigin(float x1, float y1, float x2, float y2)
{
  freePoints();
  fetch();
  maybeConvertToDistance(&x1, &y1);
  maybeConvertToDistance(&x2, &y2);
  double xorigin = _transform->getXorigin() + x2 - x1;
  double yorigin = _transform->getYorigin() + y2 - y1;
  _transform->setXorigin(xorigin);
  _transform->setYorigin(yorigin);
  save();
}



void GridLines::defineRotationAngle(float x1, float y1, float x2, float y2)
{
  freePoints();
  fetch();
  maybeConvertToDistance(&x1, &y1);
  maybeConvertToDistance(&x2, &y2);
  _transform->defineRotationAngle(x1, y1, x2, y2);
  save();
}



void GridLines::refineRotationAngle(float x, float y)
{
  freePoints();
  fetch();
  maybeConvertToDistance(&x, &y);
  _transform->refineRotationAngle(x, y);
  save();
}



void GridLines::refineBinCenter(float x, float y)
{
  freePoints();
  fetch();
  maybeConvertToDistance(&x, &y);
  _transform->refineBinCenter(x, y);
  save();
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

