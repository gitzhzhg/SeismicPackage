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
#include "oprim/xy_data.hh"
#include "oprim/base_data.hh"

#include <assert.h>

XYData::XYData (BaseData *data, long id) :
  _xs  (0),
  _ys  (0)
{
  assert (data);
  _num_xys = data->getNumPts (id);
  if (_num_xys > 0) {
    _xs = new float[_num_xys];
    _ys = new float[_num_xys];
    int k2;
    for (k2 = 0; k2 < _num_xys; k2++) {
      _xs[k2] = data->getX (k2, id);
      _ys[k2] = data->getY (k2, id);
    }
  }
}

XYData::~XYData ()
{
  if (_xs) delete [] _xs;
  if (_ys) delete [] _ys;
}

// return 0 if the data are the same
int XYData::compare (BaseData *data, long id)
{
  assert (data);

  int retval;
  if (data->getNumPts (id) == _num_xys) {
    int k2;
    for (k2 = 0, retval = 0; !retval && k2 < _num_xys; k2++) {
      if (data->getX(k2,id) != _xs[k2] ||
	  data->getY(k2,id) != _ys[k2]   ) retval = 1;
    }
  }
  else {
    retval = 1;
  }
  return retval;
}
