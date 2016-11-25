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
#include "pixmap_set.hh"
#include "plot_image.hh"

#include "sl/ximage.hh"
#include "sl/color_info_set.hh"
#include "sl/colorset_collection.hh"
#include "sl/colorset.hh"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

PixmapSet::PixmapSet (PlotImage *plot, Ximage *ximage, int *current_ptr,
  Pixmap *pixmaps, int num_pixmaps) :
  _plot               (plot),
  _ximage             (ximage),
  _pixmaps            (pixmaps),
  _current_ptr        (current_ptr),
  _num_pixmaps        (num_pixmaps),
  _col_segment        (NULL),
  _col                (NULL),
  _redrawing_enabled  (TRUE),
  _changed            (NULL),
  _xsize              (NULL),
  _ysize              (NULL)
{
  assert (ximage && current_ptr && pixmaps && num_pixmaps > 0);

  _col_segment = new ColorInfoSegment (this, ColorInfoSet::PIXMAPSET);

  _changed = (int *)malloc (num_pixmaps*sizeof(int));
  _xsize   = (int *)malloc (num_pixmaps*sizeof(int));
  _ysize   = (int *)malloc (num_pixmaps*sizeof(int));

  int k2;
  for (k2 = 0; k2 < num_pixmaps; k2++) {
    _changed[k2] = FALSE;
    _xsize[k2] = 0;
    _ysize[k2] = 0;
  }
}

PixmapSet::~PixmapSet ()
{
  if (_col) {
    ColorInfoSet *set = ColorInfoCollection::fetchIfExisting (_col);
    if (set) set->removeSharedElement (_col_segment);
  }

  CheckColorSegments::deleteSegment (_col_segment);

  free (_changed);
  free (_xsize);
  free (_ysize);
}

void PixmapSet::init (ColorInfo *col)
{
  if (col == _col) return;

  ColorInfoSet *set;
  int role;
  if (_col) {
    set = ColorInfoCollection::fetchIfExisting (_col);
    if (set) {
      role = set->role (_col_segment);
      set->removeSharedElement (_col_segment);
    }
    else {
      role = ColorInfoSet::TWO_WAY;
    }
  }
  else if (col) {
    role = ColorInfoSet::TWO_WAY;
  }

  _col = col;

  if (_col) {
    set = ColorInfoCollection::fetchExisting (_col);
    set->addSharedElement (_col_segment, role);
    colorInfoChangedImmediately (_col);
  }
}

// when one writes into a Pixmap, this should be done
//
//   originally tried to make this a high-water marker but that's not how
//   XPixmaps work
void PixmapSet::setRange (int width, int height, int xlo, int ylo,
  int index)
{
  index = verifyIndex (index);
  int xsize = xlo + width;
  if (_xsize[index] != xsize) {
    /* if (_xsize[index] < xsize) */ _xsize[index] = xsize;
    _changed[index] = TRUE;
    ColorInfoCollection::setLoopService ();
  }
  int ysize = ylo + height;
  if (_ysize[index] != ysize) {
    _changed[index] = TRUE;
    ColorInfoCollection::setLoopService ();
    /* if (_ysize[index] < ysize) */ _ysize[index] = ysize;
  }
}

// should do for every XFreePixmap
void PixmapSet::clearRange (int index)
{
  index = verifyIndex (index);
  if (_xsize[index] != 0) {
    _xsize[index] = 0;
    _changed[index] = TRUE;
    ColorInfoCollection::setLoopService ();
  }
  if (_ysize[index] != 0) {
    _ysize[index] = 0;
    _changed[index] = TRUE;
    ColorInfoCollection::setLoopService ();
  }
}

// modifies range when using Colorset::getPixels to avoid BADMATCH XERROR,
//   return TRUE if padding necessary
int PixmapSet::modifyRange (int width, int height, int *use_width,
  int *use_height, int xlo, int ylo, int index)
{
  index = verifyIndex (index);
  int retval = FALSE;

  if (xlo + width > _xsize[index]) {
    retval = TRUE;
    if (use_width) *use_width = _xsize[index] - xlo;
  }
  else if (use_width) {
    *use_width = width;
  }

  if (ylo + height > _ysize[index]) {
    retval = TRUE;
    if (use_height) *use_height = _ysize[index] - ylo;
  }
  else if (use_height) {
    *use_height = height;
  }

  if (use_width && *use_width < 1) {
    *use_width = 0;
  }

  if (use_height && *use_height < 1) {
    *use_height = 0;
  }

  return retval;
}

int PixmapSet::currentIndex ()
{
  return *_current_ptr;
}

int PixmapSet::count ()
{
  return _num_pixmaps;
}

Pixmap PixmapSet::get (int index)
{
  index = verifyIndex (index);

  return _pixmaps[index];
}

XImage *PixmapSet::ximage ()
{
  return _ximage->get ();
}

void PixmapSet::markChanged (int index)
{
  index = verifyIndex (index);
  _changed[index] = TRUE;
  ColorInfoCollection::setLoopService ();
}

int PixmapSet::changed (int index)
{
  index = verifyIndex (index);
  int retval = _changed[index];
  _changed[index] = FALSE;
  return retval;
}

int PixmapSet::redrawingEnabled ()
{
  return _redrawing_enabled;
}

void PixmapSet::setRedrawingEnabled (int flag)
{
  _redrawing_enabled = flag; 
}

void PixmapSet::redrawIfNecessary ()
{
  apply ();
}

// when the ColorInfo changes, mark all the pixmaps out of data
void PixmapSet::colorInfoChangedImmediately (ColorInfo *col)
{
  assert (col == _col);

  // pixels must of changed in order for this to be called
  //
  // mark all of the pixmaps as out-of-date
  if (_num_pixmaps > 0) {
    int k2;
    for (k2 = 0; k2 < _num_pixmaps; k2++) {
      if (_pixmaps[k2]) {
        _changed[k2] = TRUE;
	ColorInfoCollection::setLoopService ();
      }
    }
    // this fixes a bug with SeisColorPop and SeisCbarPop working together
    //   before this fix, the first time SeisCbarPop was applied after
    //   SeisColorPop had been applied, the combo would still exist when
    //   checked in refreshMain and the window would not be updated with
    //   the first SeisCbarPop change
    Colorset *colorset = colorsetIs ();
    if (colorset) {
      colorset->clearCombosWith (colorset->existingPixmap(this));
    }
  }
}

// when an apply button is pressed, this may get called
void PixmapSet::colorInfoChangedFromApply (ColorInfo *col)
{
  apply (); // redraw only the currently displayed pixmap
}

Colorset *PixmapSet::colorsetIs ()
{
  return ColorsetCollection::fetchExisting (_col);
}

int PixmapSet::indexOK (int test_index, int *index)
{
  if (test_index == USE_CURRENT) test_index = currentIndex ();
  if (test_index > -1 && test_index < _num_pixmaps) {
    if (index) *index = test_index;
    return TRUE;
  }
  else {
    if (index) *index = -1;
    return FALSE;
  }
}

int PixmapSet::verifyIndex (int index)
{
  assert (indexOK(index,&index));
  return index;
}

void PixmapSet::apply ()
{
  int index = currentIndex ();
  assert (index > -1 && index < _num_pixmaps);
  if (_changed[index] && _redrawing_enabled) {
    _changed[index] = FALSE; // cut off recursive calls
    _plot->redrawImage (index, FALSE);
  }
}
