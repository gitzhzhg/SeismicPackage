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
#ifndef HORIZONS_VECT_STYLE_HH
#define HORIZONS_VECT_STYLE_HH

#include "vect/vector.hh"

class HorizonsVectStyle
{
public:
  enum {
    HORIZON_MARKER_WIDTH = 0
  };

  enum {
    NO = 0,
    YES = 1,
    DEFAULT_HORIZON_WIDTH = 2,
    ACTIVE_HORIZON_WIDTH = 5,
    HORIZON_MARKER_SIZE = 7
  };

  enum {
    OVER_LINE,
    OVER_BOTH,
    OVER_MARK
  };

  HorizonsVectStyle () {}

  virtual ~HorizonsVectStyle () {}

protected:

private:

};

#endif
