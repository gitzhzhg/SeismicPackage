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
// class that creates a COLOR_INFO structure
#ifndef COLOR_DESCRIPTOR_HH
#define COLOR_DESCRIPTOR_HH

#include "wproc.h"
#include <Xm/Xm.h>

class ColorDescriptor {

public:
   ColorDescriptor				// constructor
    (Widget widget,				//   widget of the display
     int ncols);				//   number of colors

  ~ColorDescriptor ();				// destructor

  ColorInfo *get ()				// returns the structure
    { return _color_info; }

  int numColors ();				// return number of colors

  long allocateColorCells ();			// allocate the colormap

  void freeColorCells ();			// free the colormap

  int status ()					// status flag
    { return (int)(_color_info != NULL); }

private:
  ColorInfo
    *_color_info;				// internal COLOR_INFO struct

  class ColorInfoSegment
    *_color_info_seg;				// internal COLOR_INFO struct

  Widget
    _widget;					// widget of the display

  int
    _ncols;

};

#endif
