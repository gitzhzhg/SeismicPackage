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
#ifndef PIXMAP_SET_HH
#define PIXMAP_SET_HH

#include "wproc.h"

#include <Xm/Xm.h>

class PixmapSet {

public:
  PixmapSet
    (class PlotImage *plot,           // owner of this class
     class Ximage *ximage,
     int *current_ptr,                // where user class stores current ptr
     Pixmap *pixmaps,                 // user class array of pixmaps
     int num_pixmaps);                // size of pixmaps array

  virtual ~PixmapSet ();

  virtual void init
    (ColorInfo *col);

  enum {
    NOT_SET = -1,
    USE_CURRENT = -2,
    USE_DEFAULT = -3
  };

  void setRange
    (int width,
     int height,
     int xlo = 0,
     int ylo = 0,
     int index = USE_CURRENT);

  void clearRange
    (int index = USE_CURRENT);

  int modifyRange
    (int width,
     int height,
     int *use_width,
     int *use_height,
     int xlo = 0,
     int ylo = 0,
     int index = USE_CURRENT);

  int currentIndex ();

  int count ();

  Pixmap get
    (int index = USE_CURRENT);

  XImage *ximage ();

  void markChanged
    (int index = USE_CURRENT);

  int changed
    (int index = USE_CURRENT);

  int redrawingEnabled ();

  void setRedrawingEnabled
    (int flag);

  void redrawIfNecessary ();

  void colorInfoChangedImmediately
    (ColorInfo *col);

  void colorInfoChangedFromApply
    (ColorInfo *col);

  class Colorset *colorsetIs ();

  ColorInfo *colorInfo () { return _col; }

  PlotImage *plotImage () { return _plot; }

  int indexOK
    (int test_index = USE_CURRENT,
     int *index = NULL);

private:
  int verifyIndex
    (int index);

  virtual void apply ();

  class PlotImage
    *_plot;

  class ColorInfoSegment
    *_col_segment;

  ColorInfo
    *_col;

  Ximage
    *_ximage;

  Pixmap
    *_pixmaps;

  int
    *_current_ptr,
    _num_pixmaps,
    _redrawing_enabled,
    *_changed,
    *_xsize,
    *_ysize,
    _def_xoffset,
    _def_yoffset,
    _def_xsize,
    _def_ysize;
};

#endif
