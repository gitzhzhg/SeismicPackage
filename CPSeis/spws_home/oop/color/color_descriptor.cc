// class that creates a COLOR_INFO structure
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
#include "color/color_descriptor.hh"
#include "sl/colorset_collection.hh"
#include "sl/color_info_set.hh"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

ColorDescriptor::ColorDescriptor (Widget widget, int ncols) :
  _widget  (widget),
  _ncols   (ncols)
{
// allocate memory for the color information structure
  _color_info = (COLOR_INFO *)malloc (sizeof(COLOR_INFO));

  assert (_color_info);

// get the widget's color map
  Colormap color_map;
  XtVaGetValues (_widget, XmNcolormap, &color_map, NULL);

// initially populate the color information structure
  _color_info->numplanes = 0;
  _color_info->cmap      = color_map;
  _color_info->shared    = False;
  _color_info->cnum      = 0;
  _color_info->colorsafe = False;

  int k2;
  for (k2 = 0; k2 <  24; _color_info->pmsk[k2++] = 0);
  for (k2 = 0; k2 < 500; _color_info->pix [k2++] = 0);

  _color_info_seg = new ColorInfoSegment (this,
    ColorInfoSet::COLORDESCRIPTOR);
  ColorInfoCollection::fetch (_color_info, _color_info_seg,
    ColorInfoSet::SENDER);
}

ColorDescriptor::~ColorDescriptor ()
{
  freeColorCells ();
  ColorInfoCollection::remove (_color_info);
  ColorInfoCollection::remove (_color_info_seg);
  CheckColorSegments::deleteSegment (_color_info_seg);
  ColorsetCollection::remove (_color_info);
  free (_color_info);
}

int ColorDescriptor::numColors ()
{
  return _ncols;
}

long ColorDescriptor::allocateColorCells ()
{
// does not support multiple planes
  assert (_color_info->numplanes == 0);

// deallocate anything previously allocated
  if (_color_info->cnum > 0) freeColorCells ();

// allocate the given # of colors
  _color_info->cnum = _ncols;

  int error = ColorsetCollection::allocate (_color_info);

  if (!_color_info->colorsafe) {
    printf ("ColorDescriptor encountered status %d allocating %d colors\n",
      error, _color_info->cnum);
    freeColorCells ();
  }
  return _color_info->colorsafe;
}

void ColorDescriptor::freeColorCells ()
{
  ColorsetCollection::clear (_color_info);
  assert (_color_info->cnum == 0);
}
