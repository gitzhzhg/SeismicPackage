#include "cube/cube_amp_params.hh"
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
#include "cube/cube_master.hh"
#include "cube/cube.hh"
#include "cube/cube_display.hh"

#define NOT_SET_VALUE     -5.0
#define NOT_SET_VALUE_INT -5
#define SET_VALUE_INT     -6

CubeAmpParams::CubeAmpParams () :
     CubeVisitor (),
     _amp            (NOT_SET_VALUE),
     _norm_state     (NOT_SET_VALUE_INT),
     _master_state   (NOT_SET_VALUE_INT),
     _display_state  (NOT_SET_VALUE_INT)
{
}

CubeAmpParams::~CubeAmpParams()
{
}

void CubeAmpParams::setExternalAmp (double amp)
{
  _amp = amp;
  _master_state  = SET_VALUE_INT;
}

void CubeAmpParams::setNormType (int norm_state)
{
  _norm_state    = norm_state;
  _display_state = SET_VALUE_INT;
}

void CubeAmpParams::visitCube (Cube *cube)
{
  CubeMaster *cube_master;

  if (_amp != NOT_SET_VALUE) {
    if (_master_state != NOT_SET_VALUE_INT) {
      cube->setExternalAmp (_amp);
      _master_state = NOT_SET_VALUE_INT;
    }
// every cube slice needs to be told to replot when it can be
    cube->setTimeSliceToReplot      ();
    cube->setInlineSliceToReplot    ();
    cube->setCrosslineSliceToReplot ();
  }

  if (_norm_state != NOT_SET_VALUE_INT) {
    if (_display_state != NOT_SET_VALUE_INT) {
// each cube display norm type needs to be reset but only one time
      cube_master = CubeMaster::instance ();
      CubeDisplay *cube_display;
      void *x;
      for (cube_display = cube_master->top(&x); cube_display;
        cube_display = cube_master->next(&x)) {
        cube_display->setNormType (_norm_state);
      }
      _display_state = NOT_SET_VALUE_INT;
    }
// every cube slice needs to be told to replot when it can be
    cube->setTimeSliceToReplot      ();
    cube->setInlineSliceToReplot    ();
    cube->setCrosslineSliceToReplot ();
  }
}
