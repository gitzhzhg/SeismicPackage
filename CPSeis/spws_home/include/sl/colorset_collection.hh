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
#ifndef _COLORSET_COLLECTION_HH_
#define _COLORSET_COLLECTION_HH_

#include "wproc.h"

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>

class ColorsetCollection
{
public:
  static class Colorset *fetch
    (ColorInfo *col);

  static class Colorset *fetchExisting
    (ColorInfo *col);

  static void remove
    (ColorInfo *col = NULL);

  static int allocate
    (ColorInfo *col);

  static void clear
    (ColorInfo *col);

  static void store
    (ColorInfo *col,
     XColor *colors = NULL);              // gray scale if no colors given

  static void retrieve
    (ColorInfo *col,
     XColor **colors);

  static void transferPixels
    (ColorInfo *from);

  static int readOnly
    (ColorInfo *col);

  static int readOnly
    (Widget widget);

  static void setRecolorCurrentOnly
    (int flag = TRUE);

  static int recolorCurrentOnly ();

  static void shareWith
    (ColorInfo *col = NULL);

private:
  static void update
    (class Colorset *from,
     ColorInfo *to);

  static void setColors
    (class Colorset *to,
     const XColor colors[]);

  static void getColors
    (class Colorset *to,
     XColor colors[]);

  static int shared
    (class Colorset *colorset);

  static int allocate
    (class Colorset *colorset,
     ColorInfo *col);

  static int findExisting
    (ColorInfo *col);

  static const char *nameIs
    (class Paintset *paintset,
     ColorInfo *col);

  static void print ();

  static int
    NUM,
    RECOLOR_CURRENT_ONLY;

  static Colorset
    *SHARED_COLORSET;

};

#endif
