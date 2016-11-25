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
#ifndef CUBE_AMP_PARAMS_HH
#define CUBE_AMP_PARAMS_HH

#include "cube/cube_visitor.hh"

class CubeAmpParams : public CubeVisitor {

public:
  CubeAmpParams ();				// constructor

  ~CubeAmpParams ();				// destructor

  void setExternalAmp				// set the external amplitude
    (double amp);				//   given the external amp

  void setNormType				// set the amp norm type
    (int norm_state);				//   given the amp norm type

  void visitCube				// apply parameters to cube
    (Cube *cube);				//   given the cube

private:

  double
    _amp;					// external amplitude

  int
    _norm_state,				// amplitude normalization typ
    _master_state,				// master cube state of change
    _display_state;				// cube display state of chang

};
#endif
