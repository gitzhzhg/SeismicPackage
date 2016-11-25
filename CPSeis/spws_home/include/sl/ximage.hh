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
#ifndef XIMAGE_CC
#define XIMAGE_CC

#include "wproc.h"
#include <Xm/Xm.h>

class Ximage {

public:
  Ximage
    (ColorInfo *col);

  ~Ximage ();

  XImage *update
    (XImage *ximage = NULL);

  XImage *allocate
    (XImage *ximage = NULL);

  XImage *deallocate
    (XImage *ximage = NULL);

  XImage *createPixmap
  (Ximage *from_ximage_ptr,
   int depth = 1);

  Boolean matches
    (Ximage *ximage_ptr);

  Boolean copyData
    (Ximage *ximage_ptr);

  Boolean changed
    (XImage *ximage = NULL);

  XImage *create
    (XImage *ximage = NULL,
     ColorInfo *col = NULL);

  XImage *assignTo
    (XImage *ximage = NULL);

  class Paintset *paintset ();

  class Colorset *colorset ();

  ColorInfo *colorInfo ();

  void setColorInfo
    (ColorInfo *col);

  XImage *get ();

  void set
    (XImage *ximage = NULL,
     ColorInfo *col = NULL);

private:
  void destroy ();

  Boolean dataAllocated
    (XImage *ximage = NULL);

  XImage
    *_ximage;

  ColorInfo
    *_col;

  class Paintset
    *_paintset;

  class Colorset
    *_colorset;

  class ColorInfoSegment
    *_col_segment;

  char
    *_data;

  unsigned long
    _red_mask,
    _grn_mask,
    _blu_mask;

  int
    _depth,
    _format,
    _offset,
    _xsize,
    _ysize,
    _pad,
    _bytes,
    _byte_order,
    _unit,
    _bit_order,
    _pixel_size;
};

#endif
