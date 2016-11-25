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
//--------------------------- colorset.cc --------------------------------//
//--------------------------- colorset.cc --------------------------------//
//--------------------------- colorset.cc --------------------------------//

//              implementation file for the Colorset class
//                      not derived from any class
//                           subdirectory sl


//---------------------------- start -------------------------------------//
//---------------------------- start -------------------------------------//
//---------------------------- start -------------------------------------//


#include "sl/colorset.hh"
#include "sl/paintset.hh"
#include "sl/color_info_set.hh"
#include "pixmap_set.hh"
#include "image_data_analysis.hh"

#include "plot_image.hh"
#include "str.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>


//-------------------- constructor and destructor ------------------------//
//-------------------- constructor and destructor ------------------------//
//-------------------- constructor and destructor ------------------------//


Colorset::Colorset(Paintset *paintset)
            :
               _paintset              (paintset),
               _display               (NULL),
               _ncolors               (0),
               _pixels                (NULL),
               _plane                 (0),
               _black                 (0),
               _white                 (0),
               _first_value           (0.0),
               _last_value            (0.0),
               _changed               (FALSE),
	       _old_ncolors           (0),
               _old_reds              (NULL),
               _old_greens            (NULL),
               _old_blues             (NULL),
               _gc_overlay            (0),
               _gc_underlay           (0),
               _gc_white              (0),
               _gc_bitmap             (0),
               _pixmaps               (NULL),
	       _alloc_pixmaps         (0),
               _num_pixmaps           (0),
               _combos                (NULL),
               _alloc_combos          (0),
               _num_combos            (0),
	       _transferred_in        (FALSE),
               _shared                (NULL),
               _shared_ncolors        (0),
               _shared_inited         (FALSE)
{
  assert(_paintset);
  _display = _paintset->display();

  Window window = _paintset->rootWindow();
  Pixmap bitmap = XCreatePixmap(_display, window, 1, 1, 1);

  _gc_overlay  = XCreateGC(_display, window, 0, NULL);
  _gc_underlay = XCreateGC(_display, window, 0, NULL);
  _gc_white    = XCreateGC(_display, window, 0, NULL);
  _gc_bitmap   = XCreateGC(_display, bitmap, 0, NULL);

  XFreePixmap(_display, bitmap);

  _black = _paintset->black();
  _white = _paintset->white();

  XSetPlaneMask  (_display, _gc_overlay,  AllPlanes);
  XSetPlaneMask  (_display, _gc_underlay, AllPlanes);
  XSetForeground (_display, _gc_overlay,  _black);
  XSetBackground (_display, _gc_overlay,  _white);
  XSetForeground (_display, _gc_underlay, _black);
  XSetBackground (_display, _gc_underlay, _white);
  XSetForeground (_display, _gc_white,    _white);
  XSetBackground (_display, _gc_white,    _white);
  XSetForeground (_display, _gc_bitmap,   0);
  XSetBackground (_display, _gc_bitmap,   0);
}



Colorset::~Colorset()
{
  freeColorbar();
  freeOldColorbar();

  XFreeGC(_display, _gc_overlay);
  XFreeGC(_display, _gc_underlay);
  XFreeGC(_display, _gc_white);
  XFreeGC(_display, _gc_bitmap);

  int k2;
  if (_alloc_combos) {
    if (_num_combos) {
      for (k2 = 0; k2 < _num_combos; k2++) {
	delete _combos[k2];
      }
      free (_combos);
    }
  }

  if (_alloc_pixmaps) {
    if (_num_pixmaps) {
      for (k2 = 0; k2 < _num_pixmaps; k2++) {
	delete _pixmaps[k2];
      }
      free (_pixmaps);
    }
  }
}


//---------------------------- print colorbar -----------------------------//
//---------------------------- print colorbar -----------------------------//
//---------------------------- print colorbar -----------------------------//


void Colorset::printColorbar()  const
{
  printf("\n");
  printf("Allocated Colorbar:\n");
  printf("\n");
  printf("plane mask  = %8lx\n", _plane);
  printf("black pixel = %8lx\n", _black);
  printf("white pixel = %8lx\n", _white);
  printf("num colors  = %d\n", _ncolors);
  printf("\n");
  for(int icolor = 0; icolor < _ncolors; icolor++)
      {
      Pixel pixel = _pixels[icolor];
      int   red   = _paintset->getRedColor  (pixel);
      int   green = _paintset->getGreenColor(pixel);
      int   blue  = _paintset->getBlueColor (pixel);
      float value = getValue(icolor);
      printf( "value = %13f  icolor = %3d  pixel = %08lx   red = %5d \
  green = %5d   blue = %5d\n",
           value, icolor, pixel, red, green, blue);
      }
  printf("\n");
}


int Colorset::colorbarHasChanged ()
{
  if (!_changed) {
    return FALSE;
  }
  else {
    if (readOnly()) {
      // at some point previously, the colorbar changed
      //   before the _changed flag is reset, mark all the
      //   pixmaps that this object knows about as changed
      //   so when they are to be used, they can be remade
      //   similarly, clear all of the previously made _combos
      int k2;
      for (k2 = 0; k2 < _num_pixmaps; k2++) {
	_pixmaps[k2]->markChanged ();
      }
      for (k2 = 0; k2 < _num_combos; k2++) {
	_combos[k2]->clear ();
      }
    }
    _changed = FALSE;
    return TRUE;
  }
}



//------------------ allocate and free colorbar ---------------------------//
//------------------ allocate and free colorbar ---------------------------//
//------------------ allocate and free colorbar ---------------------------//


int Colorset::allocateColorbar (int ncolors, int allocate_plane)
{
  freeColorbar();
  if(ncolors == 0) return FALSE;

  _ncolors = ncolors;
  _pixels  = new Pixel [_ncolors + 2]; // use last two for black & white

  int do_allocate_plane = (allocate_plane && !readOnly());

  Pixel *plane = do_allocate_plane ? &_plane : NULL;

// KC changed this 01/20/05 hoping to save some pixels for va
/*
//////////////// Tom's code ///////////////////
  int error = _paintset->allocateColorCells(_ncolors + 2, _pixels, plane);
//////////////// Tom's code ///////////////////
*/
//////////////// KC's code ///////////////////
  int error;
  if (_shared) {
    error = allocateSharedColorbar (plane);
  }
  else {
    error = _paintset->allocateColorCells (_ncolors, _pixels, plane);
  }
//////////////// KC's code ///////////////////
  if (error) {
    delete [] _pixels, _pixels = NULL;
    _ncolors = 0;
    return TRUE;
  }

  if(do_allocate_plane)
      {
/*
//////////////// Tom's code ///////////////////
      _black = _pixels[_ncolors];
      _white = _pixels[_ncolors + 1];
      _paintset->setColor (&_black, 0, 0, 0);
      _paintset->setColor (&_white, BRIGHTEST, BRIGHTEST, BRIGHTEST);
//////////////// Tom's code ///////////////////
*/
//////////////// KC's code ///////////////////
      _black = _paintset->black ();
      _white = _paintset->white ();
//////////////// KC's code ///////////////////
      XSetPlaneMask  (_display, _gc_overlay,   _plane);
      XSetPlaneMask  (_display, _gc_underlay, ~_plane);
      XSetForeground (_display, _gc_overlay,   AllPlanes);     // or _plane.
      XSetBackground (_display, _gc_overlay,   0);
      XSetForeground (_display, _gc_white,     _white);
      XSetBackground (_display, _gc_white,     _white);
      }
  _transferred_in = FALSE;
  return FALSE;
}



void Colorset::freeColorbar()
{
// KC changed this 01/20/05 hoping to save some pixels for va
/*
//////////////// Tom's code ///////////////////
  if(_ncolors > 0) _paintset->freeColorCells(_ncolors + 2, _pixels, _plane);
//////////////// Tom's code ///////////////////
*/
//////////////// KC's code ///////////////////
  if (_shared_ncolors > 0) {
    freeSharedColorbar ();
  }
  else if (_ncolors > 0 && !_transferred_in) {
    // if the pixels were transferred in, then they should be freed
    //   by the freeColorbar from some other Colorset
    _paintset->freeColorCells (_ncolors, _pixels, _plane);
  }
//////////////// KC's code ///////////////////

  if(_pixels) delete [] _pixels;

  _ncolors = 0;
  _pixels  = NULL;
  _plane   = 0;
//_changed = TRUE; // use colorChanged() later
  _black   = _paintset->black();
  _white   = _paintset->white();

  XSetPlaneMask  (_display, _gc_overlay,  AllPlanes);
  XSetPlaneMask  (_display, _gc_underlay, AllPlanes);
  XSetForeground (_display, _gc_overlay,  _black);
  XSetBackground (_display, _gc_overlay,  _white);
  XSetForeground (_display, _gc_white,    _white);
  XSetBackground (_display, _gc_white,    _white);
}

void Colorset::transferPixels (int ncolors, Pixel *pixels, int allocate_plane)
{
  // this is extremely dangerous
  // only for dynamic colors and trying to recover from an error
  //   used for expedience since SeisColor does not use a pointer for
  //   ColorInfo
  assert ((!readOnly()) && (ncolors <= _paintset->numColors()));

  if (ncolors > _ncolors) {
    _ncolors = ncolors;
    delete [] _pixels;
    _pixels = new Pixel[_ncolors + 2]; // use last two for black & white
  }

  int k2;
  for (k2 = 0; k2 < ncolors; k2++) {
    // warning: there is no check for the validity of the given pixels!!!!
    _pixels[k2] = pixels[k2];
  }

  if (allocate_plane) {
    _black = _paintset->black ();
    _white = _paintset->white ();
    XSetPlaneMask  (_display, _gc_overlay,   _plane);
    XSetPlaneMask  (_display, _gc_underlay, ~_plane);
    XSetForeground (_display, _gc_overlay,   AllPlanes);     // or _plane.
    XSetBackground (_display, _gc_overlay,   0);
    XSetForeground (_display, _gc_white,     _white);
    XSetBackground (_display, _gc_white,     _white);
  }
}

void Colorset::shareWith (Colorset *shared)
{
  _shared = shared;
}

int Colorset::allocateSharedColorbar (Pixel *plane)
{
  int error;
  assert (_shared != NULL && !_shared_inited);
  int num = _paintset->numAvailableColors (plane!=NULL?1:0);
  if (num > _ncolors) num = _ncolors;
  _shared_ncolors = 0;
  if (_ncolors <= num + _shared->numColors()) {
    // the _ncolors required fits within the colors available for use
    if (num > 0) {
      // use up all the pixels that are available in Paintset *_paintset
      error = _paintset->allocateColorCells (num, _pixels, plane);
    }
    else {
      error = FALSE;
    }
    if (!error) {
      _shared_ncolors = _ncolors - num;
    }
  }
  else {
    error = TRUE;
  }
  return error;
}

void Colorset::freeSharedColorbar ()
{
  // the shared pixels will be freed by the freeColorbar from
  //   Colorset *_shared
  int num = _ncolors - _shared_ncolors;
  if (num > 0) _paintset->freeColorCells (num, _pixels, _plane);
  _shared_ncolors = 0;
  _shared_inited = FALSE;
}


//---------------------------- get values ---------------------------------//
//---------------------------- get values ---------------------------------//
//---------------------------- get values ---------------------------------//


int Colorset::numAvailableColors()  const
{
  return _paintset->numAvailableColors();
}


float Colorset::getValue(int icolor)  const
{
  assert(icolor >= 0 && icolor <= _ncolors);
  if(_ncolors <= 1) return _first_value;
  return _first_value + icolor * (_last_value - _first_value) / _ncolors;
}


int Colorset::getRedColor (int icolor)  const
{
  assert(icolor >= 0 && icolor < _ncolors);
  return _paintset->getRedColor(_pixels[icolor]);
}


int Colorset::getGreenColor (int icolor)  const
{
  assert(icolor >= 0 && icolor < _ncolors);
  return _paintset->getGreenColor(_pixels[icolor]);
}


int Colorset::getBlueColor (int icolor)  const
{
  assert(icolor >= 0 && icolor < _ncolors);
  return _paintset->getBlueColor(_pixels[icolor]);
}


Pixel Colorset::getPixel (int icolor)  const
{
  assert(icolor >= 0 && icolor < _ncolors);
  return _pixels[icolor];
}


Pixel Colorset::getPixel (float value)  const
{
  if(_ncolors == 0)
      {
      if(value > 0.5 * (_first_value + _last_value)) return _black;
      return _white;
      }

  int icolor;
  if(_last_value == _first_value)
      {
      icolor = _ncolors / 2;
      }
  else
      {
      icolor = (int)(float(_ncolors) * (value - _first_value)
                               / (_last_value - _first_value));
      }
  icolor = ConstrainValue(icolor, 0, _ncolors - 1);
  return _pixels[icolor];
}

int Colorset::getColor (float value) const
{
  if (_ncolors == 0) {
    // return an invalid index
    return -3;
  }

  int icolor;
  if (_last_value == _first_value) {
    // return an invalid index
    return -2;
  }
  else {
    icolor = (int)(float(_ncolors) * (value - _first_value)
                             / (_last_value - _first_value));
  }
  if (icolor != ConstrainValue(icolor,0,_ncolors-1)) {
    // return an invalid index
    return -1;
  }
  return icolor;
}

int Colorset::getColor (Pixel pixel)  const
{
  int retval;

  int k2;
  for (k2 = 0, retval = -1; k2 < _ncolors; k2++) {
    if (pixel == _pixels[k2]) {
      retval = k2;
      k2 = _ncolors;
    }
  }
  return retval;
}


int Colorset::readOnly()  const
{
  return _paintset->readOnly();
}


void Colorset::getColor (int icolor, int *red, int *green, int *blue)  const
{
  assert(icolor >= 0 && icolor < _ncolors);
  _paintset->getColor(_pixels[icolor], red, green, blue);
}


void Colorset::getColors (int red  [],
                          int green[],
                          int blue [])  const
{
  _paintset->getColors(_ncolors, _pixels, red, green, blue);
}


//-------------------------- set values ---------------------------------//
//-------------------------- set values ---------------------------------//
//-------------------------- set values ---------------------------------//


void Colorset::setRedColor (int icolor, int red)
{
  assert(icolor >= 0 && icolor < _ncolors);
  assert ((!_shared && _shared_ncolors == 0) || _shared_inited);
  _paintset->setRedColor(&_pixels[icolor], red);
  if (readOnly() && colorChanged(icolor)) {
    _changed = TRUE;
    ColorInfoCollection::setLoopService ();
  }
}


void Colorset::setGreenColor (int icolor, int green)
{
  assert(icolor >= 0 && icolor < _ncolors);
  assert ((!_shared && _shared_ncolors == 0) || _shared_inited);
  _paintset->setGreenColor(&_pixels[icolor], green);
  if (readOnly() && colorChanged(icolor)) {
    _changed = TRUE;
    ColorInfoCollection::setLoopService ();
  }
}


void Colorset::setBlueColor (int icolor, int blue)
{
  assert(icolor >= 0 && icolor < _ncolors);
  assert ((!_shared && _shared_ncolors == 0) || _shared_inited);
  _paintset->setBlueColor(&_pixels[icolor], blue);
  if (readOnly() && colorChanged(icolor)) {
    _changed = TRUE;
    ColorInfoCollection::setLoopService ();
  }
}


void Colorset::setColor (int icolor, int  red, int  green, int  blue)
{
  assert(icolor >= 0 && icolor < _ncolors);
  assert ((!_shared && _shared_ncolors == 0) || _shared_inited);
  _paintset->setColor(&_pixels[icolor], red, green, blue);
  if (readOnly() && colorChanged(icolor)) {
    _changed = TRUE;
    ColorInfoCollection::setLoopService ();
  }
}


void Colorset::setColors (const int red  [],
                          const int green[],
                          const int blue [])
{
  if (_shared && _shared_ncolors > 0 && !_shared_inited) {
    setSharedColors (red, green, blue);
  }
  else {
    _paintset->setColors (_ncolors, _pixels, red, green, blue);
  }
  if (readOnly() && colorsChanged()) {
    _changed = TRUE;
    ColorInfoCollection::setLoopService ();
  }
}

void Colorset::setSharedColors (const int red  [],
                                const int green[],
                                const int blue [])
{
  if (_shared_ncolors > 0 && !_shared_inited) {
    assert (!readOnly() && _shared);
    int num = _ncolors - _shared_ncolors;
    _paintset->setColors (num, _pixels, red, green, blue);
    int *used_indices = (int *)malloc (_shared_ncolors*sizeof(int));
    int k2, k3, more;
    for (k2 = num, k3 = 0, more = 0; k2 < _ncolors; k2++, k3++) {
      // try to match a pixel to the color in Colorset *_shared
      _pixels[k2] = _shared->matchPixelByColor (&used_indices[k3], red[k2],
        green[k2], blue[k2]);
      if (used_indices[k3] < 0) {
	// did not find a match
        more++;
      }
    }
    if (more) {
      // one or more pixels were not matched, so arbitrarily pick the pixels
      //   from the smallest to the largest taking great care not to 
      //   reuse the same pixel
      int current = 0;
      int used;
      int possible_conflict = more < _ncolors;
      for (k2 = 0;
        more > 0 && k2 < _shared_ncolors && current < _shared->_ncolors;
	k2++) {
	for (;used_indices[k2] < 0 && current < _shared->_ncolors;
          current++) {
	  for (k3 = 0, used = FALSE;
            possible_conflict && !used && k3 < _shared_ncolors; k3++) {
	    used = current == used_indices[k3];
	  }
	  if (!used) {
	    more--;
	    _pixels[k2] = _shared->_pixels[current];
	    used_indices[k2] = current;
	  }
	}
	// we assumed any pixel could be satisfied in Colorset *_shared
	assert (!(used_indices[k2] < 0));
      }
      // we assumed there were enough pixels in Colorset *_shared to satisfy
      assert (more < 1);
    }
    free (used_indices);
    _shared_inited = TRUE;
  }
  else {
    // either not the first time, or no _shared_ncolors to obtain
    _paintset->setColors (_ncolors, _pixels, red, green, blue);
  }
}

void Colorset::freeOldColorbar ()
{
  if (_old_ncolors > 0) {
    // clear out the old
    free (_old_reds  ), _old_reds   = NULL;
    free (_old_greens), _old_greens = NULL;
    free (_old_blues ), _old_blues  = NULL;
    _old_ncolors = 0;
  }
}

void Colorset::reallocateOldColorbar ()
{
  if (_ncolors != _old_ncolors) {
    unsigned short *old_reds, *old_greens, *old_blues;
    int k2;
    if (_ncolors > 0) {
      old_reds   = (unsigned short *)malloc (_ncolors*sizeof(unsigned short));
      old_greens = (unsigned short *)malloc (_ncolors*sizeof(unsigned short));
      old_blues  = (unsigned short *)malloc (_ncolors*sizeof(unsigned short));
    }
    if (_ncolors > _old_ncolors) {
      // retain the existing old colors
      for (k2 = 0; k2 < _old_ncolors; k2++) {
	old_reds  [k2] = _old_reds  [k2];
	old_greens[k2] = _old_greens[k2];
	old_blues [k2] = _old_blues [k2];
      }
    }
    else if (_old_ncolors > _ncolors) {
      if (_ncolors > 0) {
	// save a portion of the old colors
	for (k2 = 0; k2 < _ncolors; k2++) {
	  old_reds  [k2] = _old_reds  [k2];
	  old_greens[k2] = _old_greens[k2];
	  old_blues [k2] = _old_blues [k2];
	}
      }
    }
    freeOldColorbar ();
    if (_ncolors > 0) {
      _old_reds   = old_reds  ;
      _old_greens = old_greens;
      _old_blues  = old_blues ;
    }
    _old_ncolors = _ncolors;
  }
}

void Colorset::rememberColors ()
{
  int k2;
  for (k2 = 0; k2 < _ncolors; k2++) {
    rememberColor (k2);
  }
}

void Colorset::rememberColor (int icolor)
{
  reallocateOldColorbar ();
  Pixel pixel = _pixels[icolor];
  _old_reds  [icolor] = (short)_paintset->  getRedColor (pixel);
  _old_greens[icolor] = (short)_paintset->getGreenColor (pixel);
  _old_blues [icolor] = (short)_paintset-> getBlueColor (pixel);
}

int Colorset::colorsChanged ()
{
  int retval;

  if (_old_ncolors == _ncolors) {
    int k2;
    for (k2 = 0, retval = FALSE; !retval && k2 < _ncolors; k2++) {
      retval = colorChanged (k2);
    }
    if (retval) {
      rememberColors ();
    }
  }
  else {
    retval = TRUE;
    rememberColors ();
  }
  return retval;
}

int Colorset::colorChanged (int icolor)
{
  int retval;

  assert (icolor < _ncolors);
  Pixel pixel = _pixels[icolor];
  if (icolor                          >= _old_ncolors       ||
      _paintset->getRedColor  (pixel) != _old_reds  [icolor] ||
      _paintset->getGreenColor(pixel) != _old_greens[icolor] ||
      _paintset->getBlueColor (pixel) != _old_blues [icolor]   ) {
    
    retval = TRUE;
    if (icolor == 0) {
      int itest = 0;
    }
    rememberColor (icolor);
  }
  else {
    retval = FALSE;
  }
  return retval;
}

Pixel Colorset::matchPixelByColor (int *index, int red, int green, int blue)
{
  Pixel retval;
  int k2, loc_red, loc_green, loc_blue;
  for (k2 = 0, *index = -1; k2 < _ncolors; k2++) {
    loc_red   = _paintset->getRedColor   (_pixels[k2]);
    loc_green = _paintset->getGreenColor (_pixels[k2]);
    loc_blue  = _paintset->getBlueColor  (_pixels[k2]);
    if (red   == loc_red   &&
	green == loc_green &&
        blue  == loc_blue    ) {
      *index = k2;
      return _pixels[k2];
    }
  }
  return 0;
}


//--------------------------- static functions ----------------------------//
//--------------------------- static functions ----------------------------//
//--------------------------- static functions ----------------------------//


static int static_get_depth (Display *display, Drawable drawable)
{
  Window root;
  int x, y;
  unsigned int width, height, bwidth, depth;

  assert (drawable);
  Status status = XGetGeometry(display, drawable, &root, &x, &y,
                               &width, &height, &bwidth, &depth);
  assert(status);
  return (int)depth;
}


static int static_get_width (Display *display, Drawable drawable)
{
  Window root;
  int x, y;
  unsigned int width, height, bwidth, depth;

  assert (drawable);
  Status status = XGetGeometry(display, drawable, &root, &x, &y,
                               &width, &height, &bwidth, &depth);
  assert(status);
  return (int)width;
}


static int static_get_height (Display *display, Drawable drawable)
{
  Window root;
  int x, y;
  unsigned int width, height, bwidth, depth;

  assert (drawable);
  Status status = XGetGeometry(display, drawable, &root, &x, &y,
                               &width, &height, &bwidth, &depth);
  assert(status);
  return (int)height;
}


static void static_get_size (Display *display, Drawable drawable,
  int *x, int *y, int *depth, int *width, int *height)
{
  Window root;
  int ax, ay;
  unsigned int awidth, aheight, bwidth, adepth;

  assert (drawable);

  Status status = XGetGeometry (display, drawable, &root, &ax, &ay,
    &awidth, &aheight, &bwidth, &adepth);

  assert (status);

  *x      =      ax;
  *y      =      ay;
  *depth  = (int)adepth;
  *width  = (int)awidth;
  *height = (int)aheight;
}


//------------------------ private paint drawable --------------------------//
//------------------------ private paint drawable --------------------------//
//------------------------ private paint drawable --------------------------//


void Colorset::privatePaintDrawable
                       (Drawable drawable,      // pixmap or window or BITMAP.
                        Pixel data[],           // freed by this function.
                        int xsize, int ysize,   // size of data array.
                        int xlo,   int ylo,     // in data array.
                        int width, int height,  // in data and destination.
                        int xlo2,  int ylo2)    // in destination drawable.
{
  assert(data);

  if(width  < 0) { width  = -width ; xlo = xlo - width ; xlo2 = xlo2 - width ; }
  if(height < 0) { height = -height; ylo = ylo - height; ylo2 = ylo2 - height; }

  if(width  == 0) width  = xsize - xlo;
  if(height == 0) height = ysize - ylo;

  if(width <= 0 || height <= 0) { free(data); return; }

  Visual  *visual  = _paintset->visual();
  int      depth   = static_get_depth(_display, drawable);
  int      format  = ZPixmap;
  int      offset  = 0;
  int      pad     = 32;
  int      bytes   = 0;
  GC       gc      = (depth == 1 ? _gc_bitmap : _gc_white);

  XImage *image = XCreateImage(_display, visual, depth, format, offset,
                               (char*)data, xsize, ysize, pad, bytes);
  for(int indx = 0; indx < xsize*ysize; indx++)
      {
      Pixel pixel = data[indx];
      int   iy    = indx / xsize;
      int   ix    = indx - iy * xsize;
      XPutPixel(image, ix, iy, pixel);        // changes data[indx].
      }

  XPutImage (_display, drawable, gc, image,
             xlo, ylo, xlo2, ylo2, width, height);

  XDestroyImage(image);        // also deallocates DATA.
}


void Colorset::paintRectangle
                     (Drawable drawable,       // pixmap or window or BITMAP.
                      GC gc,
                      int xlo,   int ylo,      // in destination drawable.
                      int width, int height)   // in destination drawable.
{
  if(width  < 0) { width  = -width ; xlo = xlo - width ; }
  if(height < 0) { height = -height; ylo = ylo - height; }

  if(width  == 0) width  = static_get_width (_display, drawable) - xlo;
  if(height == 0) height = static_get_height(_display, drawable) - ylo;

  if(width <= 0 || height <= 0) return;

  // assume Foreground and Background have already been set on display/GC.
  XFlush (_display);
  XFillRectangle (_display, drawable, gc, xlo, ylo, width, height);
  XFlush (_display);
}



void Colorset::paintBits
                     (Drawable drawable,       // pixmap or window or BITMAP.
                      const char bits[],       // bits 1 (black) or 0 (white).
      /* AA */        int xsize, int ysize,    // number of bits in bits array.
                      int xlo,   int ylo,      // in bits array.
                      int width, int height,   // in bits and destination.
                      int xlo2,  int ylo2)     // in destination drawable.
{
  assert (bits);
  Pixel *data = (Pixel*)malloc(xsize * ysize * sizeof(Pixel));

  int depth = static_get_depth(_display, drawable);

  Pixel black = (depth == 1 ? AllPlanes : (_plane ? _white | _plane : _black));
  Pixel white = (depth == 1 ?    0      : _white);
  
  int bytes_per_line = ((((xsize + 7) / 8) + 3) / 4) * 4;
  for (int k2 = 0, offset = 0, indx = 0; k2 < ysize; k2++,
    offset += bytes_per_line) {
    for (int k3 = 0; k3 < xsize; k3++) {
      int byte_index = k3 / 8;
      int bit_index = k3 - 8 * byte_index;
      char mask = ( (char)1 << (char)(7 - bit_index) );
      char blackwhite = (bits[offset+byte_index] & mask);
      if(blackwhite) { data[indx] = black; }
      else           { data[indx] = white; }
      indx++;
    }
  }

  privatePaintDrawable (drawable, data, xsize, ysize,
                        xlo, ylo, width, height, xlo2, ylo2);
}



void Colorset::paintDrawable
                     (Drawable drawable,       // pixmap or window or BITMAP.
                      const char blackwhite[], // TRUE (black) or FALSE (white).
      /* AA */        int xsize, int ysize,    // size of blackwhite array.
                      int xlo,   int ylo,      // in blackwhite array.
                      int width, int height,   // in blackwhite and destination.
                      int xlo2,  int ylo2)     // in destination drawable.
{
  assert (blackwhite);
  Pixel *data = (Pixel*)malloc(xsize * ysize * sizeof(Pixel));

  int depth = static_get_depth(_display, drawable);

  Pixel black = (depth == 1 ? AllPlanes : (_plane ? _white | _plane : _black));
  Pixel white = (depth == 1 ?    0      : _white);

  for(int indx = 0; indx < xsize*ysize; indx++)
      {
      if(blackwhite[indx]) { data[indx] = black; }
      else                 { data[indx] = white; }
      }

  privatePaintDrawable (drawable, data, xsize, ysize,
                        xlo, ylo, width, height, xlo2, ylo2);
}


void Colorset::paintDrawable
                     (Drawable drawable,       // pixmap or window to paint.
                      const float source[],    // values in source data.
      /* BB */        int xsize, int ysize,    // size of source array.
                      int xlo,   int ylo,      // in source array.
                      int width, int height,   // in source and destination.
                      int xlo2,  int ylo2)     // in destination drawable.
{
  assert(source);
  Pixel *data = (Pixel*)malloc(xsize * ysize * sizeof(Pixel));

  for(int indx = 0; indx < xsize*ysize; indx++)
      {
      float value = source[indx];
      data[indx]  = getPixel(value);
      }

  privatePaintDrawable (drawable, data, xsize, ysize,
                        xlo, ylo, width, height, xlo2, ylo2);
}



void Colorset::paintDrawable
                     (Drawable drawable,       // pixmap or window to paint.
                      const int indices[],     // indices into the colorbar.
      /* BB */        int xsize, int ysize,    // size of indices array.
                      int xlo,   int ylo,      // in indices array.
                      int width, int height,   // in indices and destination.
                      int xlo2,  int ylo2)     // in destination drawable.
{
  assert(indices);
  Pixel *data = (Pixel*)malloc(xsize * ysize * sizeof(Pixel));

  for(int indx = 0; indx < xsize*ysize; indx++)
      {
      int icolor = indices[indx];
      data[indx]  = getPixel(icolor);
      }

  privatePaintDrawable (drawable, data, xsize, ysize,
                        xlo, ylo, width, height, xlo2, ylo2);
}


void Colorset::paintDrawable
                 (Drawable drawable,           // pixmap or window to paint.
                  const unsigned int pixels[], // XImage pixels are 32 bits.
      /* CC */    int xsize, int ysize,        // size of pixels array.
                  int xlo,   int ylo,          // in pixels array.
                  int width, int height,       // in pixels and destination.
                  int xlo2,  int ylo2)         // in destination drawable.
{
  assert(pixels);
  Pixel *data = (Pixel*)malloc(xsize * ysize * sizeof(Pixel));

  for(int indx = 0; indx < xsize*ysize; indx++)
      {
      data[indx] = pixels[indx];
                    // copied because DATA will be automatically deallocated.
      }

  privatePaintDrawable (drawable, data, xsize, ysize,
                        xlo, ylo, width, height, xlo2, ylo2);
}


void Colorset::copyDrawable
                    (Drawable drawable,      // pixmap or window or BITMAP.
                     Drawable source,        // pixmap or window or BITMAP.
                     int xlo  , int ylo   ,  // in source drawable.
                     int width, int height,  // in source and destination.
                     int xlo2 , int ylo2  )  // in destination drawable.
{
  if(width  < 0) { width  = -width ; xlo = xlo - width ; xlo2 = xlo2 - width ; }
  if(height < 0) { height = -height; ylo = ylo - height; ylo2 = ylo2 - height; }

  if(width  == 0) width  = static_get_width (_display, source) - xlo;
  if(height == 0) height = static_get_height(_display, source) - ylo;

  if(width <= 0 || height <= 0) return;

  int depth = static_get_depth(_display, source);
  GC  gc    = (depth == 1 ? _gc_bitmap : _gc_white);

  XCopyArea (_display, source, drawable, gc,
             xlo, ylo, width, height, xlo2, ylo2);
}


void Colorset::clearDrawable
                     (Drawable drawable,       // pixmap or window or BITMAP.
                      int xlo,   int ylo,      // in destination drawable.
                      int width, int height)   // in destination drawable.
{
  if(width  < 0) { width  = -width ; xlo = xlo - width ; }
  if(height < 0) { height = -height; ylo = ylo - height; }

  if(width  == 0) width  = static_get_width (_display, drawable) - xlo;
  if(height == 0) height = static_get_height(_display, drawable) - ylo;

  if(width <= 0 || height <= 0) return;

  int depth = static_get_depth(_display, drawable);
  GC  gc    = (depth == 1 ? _gc_bitmap : _gc_white);

  paintRectangle (drawable, gc, xlo, ylo, width, height);
}


//------------------------- overlays and underlays ------------------------//
//------------------------- overlays and underlays ------------------------//
//------------------------- overlays and underlays ------------------------//


void Colorset::putOverlay
                  (Drawable drawable,       // pixmap or window to paint.
                   Pixmap overlay,          // BITMAP to copy into drawable.
                   int xlo,   int ylo,      // in overlay.
                   int width, int height,   // in overlay and destination.
                   int xlo2,  int ylo2)     // in destination drawable.
                                            // (overlay was painted by AA)
                                            // (underlay remains in place)
{
  if(width  < 0) { width  = -width ; xlo = xlo - width ; xlo2 = xlo2 - width ; }
  if(height < 0) { height = -height; ylo = ylo - height; ylo2 = ylo2 - height; }

  if(width  == 0) width  = static_get_width (_display, overlay) - xlo;
  if(height == 0) height = static_get_height(_display, overlay) - ylo;

  if(width <= 0 || height <= 0) return;

  XCopyPlane (_display, overlay, drawable, _gc_overlay,
              xlo, ylo, width, height, xlo2, ylo2, 1);
}



void Colorset::putUnderlay
                  (Drawable drawable,     // pixmap or window to paint.
                   Pixmap underlay,       // pixmap to copy to drawable.
                   int xlo,   int ylo,    // in underlay.
                   int width, int height, // in underlay and destination.
                   int xlo2,  int ylo2)   // in destination drawable.
                                          // (underlay was painted by BB or CC)
                                          // (overlay remains in place)
{
  if(width  < 0) { width  = -width ; xlo = xlo - width ; xlo2 = xlo2 - width ; }
  if(height < 0) { height = -height; ylo = ylo - height; ylo2 = ylo2 - height; }

  if(width  == 0) width  = static_get_width (_display, underlay) - xlo;
  if(height == 0) height = static_get_height(_display, underlay) - ylo;

  if(width <= 0 || height <= 0) return;

  XCopyArea (_display, underlay, drawable, _gc_underlay,
             xlo, ylo, width, height, xlo2, ylo2);
}



void Colorset::clearOverlay
                  (Drawable drawable,       // pixmap or window to clear.
                   int xlo,   int ylo,      // in destination drawable.
                   int width, int height)   // in destination drawable.
                                            // (overlay is cleared)
                                            // (underlay remains in place)
{
  if(width  < 0) { width  = -width ; xlo = xlo - width ; }
  if(height < 0) { height = -height; ylo = ylo - height; }

  if(width  == 0) width  = static_get_width (_display, drawable) - xlo;
  if(height == 0) height = static_get_height(_display, drawable) - ylo;

  if(width <= 0 || height <= 0) return;

  Pixmap overlay = XCreatePixmap (_display, drawable, width, height, 1);

/// The above pixmap seems not to be initialized to anything.
/// Therefore XFillRectangle is used to initialize it.

  paintRectangle (overlay, _gc_bitmap, 0, 0, width, height);

  XCopyPlane     (_display, overlay, drawable, _gc_overlay,
                  0, 0, width, height, xlo, ylo, 1);

  XFreePixmap    (_display, overlay);
}



void Colorset::clearUnderlay
                  (Drawable drawable,       // pixmap or window to clear.
                   int xlo,   int ylo,      // in destination drawable.
                   int width, int height)   // in destination drawable.
                                            // (underlay is cleared)
                                            // (overlay remains in place)
{
  if(width  < 0) { width  = -width ; xlo = xlo - width ; }
  if(height < 0) { height = -height; ylo = ylo - height; }

  if(width  == 0) width  = static_get_width (_display, drawable) - xlo;
  if(height == 0) height = static_get_height(_display, drawable) - ylo;

  if(width <= 0 || height <= 0) return;

  Pixmap underlay = XCreatePixmap (_display, drawable, width, height,
                                   _paintset->visualDepth());

/// The above pixmap does seem to be initialized to zeroes.
/// But just in case, XFillRectangle is used to initialize it.

  paintRectangle (underlay, _gc_white, 0, 0, width, height);

  XCopyArea      (_display, underlay, drawable, _gc_underlay,
                  0, 0, width, height, xlo, ylo);

  XFreePixmap    (_display, underlay);
}


void Colorset::storeXColor (int icolor, XColor color)
{
  setColor (icolor, color.red, color.green, color.blue);
}


void Colorset::storeXColor (int icolor)
{
  int gray = icolor * BRIGHTEST / (_ncolors - 1);
  setColor (icolor, gray, gray, gray);
}

// makes call to XQueryColor in Paintset
XColor Colorset::queryXColor (int icolor)  const
{
  XColor retval;

  assert (icolor >= 0 && icolor < _ncolors);

  _paintset->getXColor (_pixels[icolor], &retval);
  return retval;
}

// avoids any call to XQueryColor
XColor Colorset::getXColor (int icolor) const
{
  XColor retval;
  assert (icolor >= 0 && icolor < _ncolors);

  // simply build up an XColor from what Paintset remembers
  Pixel pixel  = _pixels[icolor];
  retval.pixel = pixel;
  retval.red   = _paintset->getRedColor   (pixel);
  retval.green = _paintset->getGreenColor (pixel);
  retval.blue  = _paintset->getBlueColor  (pixel);
  retval.flags = DoRed | DoGreen | DoBlue;
  retval.pad   = 32;
  return retval;
}

int Colorset::synchronize (Colorset *from)
{
  int retval;

  if (from == this) return TRUE;

  freeColorbar ();
  if (from->_ncolors > 0) {
    int error = allocateColorbar (from->_ncolors, from->_plane?TRUE:FALSE);
    if (retval = !error) {
      int *red, *green, *blue;
      red   = (int *)malloc (_ncolors*sizeof(int));
      green = (int *)malloc (_ncolors*sizeof(int));
      blue  = (int *)malloc (_ncolors*sizeof(int));
      from->getColors (red, green, blue);
      setColors (red, green, blue);
      free (red);
      free (green);
      free (blue);
    }
  }
  else {
    retval = TRUE;
  }
  return retval;
}

int Colorset::planeAllocated ()
{
  return _plane > 0; // it appeared that _paintset's value was too volatile
  //return _paintset->numAllocatedPlanes() > 0;
}

Pixel *Colorset::getPixels (Pixmap source,         // pixmap or BITMAP.
                            int width, int height, // subimage to get
                            int xlo,   int ylo)    // in source Pixmap
{
  if (!source) return NULL;

  if (width < 0) {
    width =      -width;
    xlo   = xlo - width;
  }
  if (height < 0) {
    height =      -height;
    ylo    = ylo - height;
  }

  if (width <= 0 || height <= 0) return NULL;

  Pixel plane_mask = AllPlanes;
  int   format     = ZPixmap;

  XImage *image = XGetImage (_display, source, xlo, ylo, width, height,
    plane_mask, format);

  int num_pixels = width * height;
  Pixel *retval = (Pixel *)malloc (num_pixels*sizeof(Pixel));

  int indx, ix, iy;
  for (indx = 0; indx < num_pixels; indx++) {
    iy = indx / width;
    ix = indx - iy * width;
    retval[indx] = XGetPixel (image, ix, iy); // why do this?
  }

  XDestroyImage (image);
  return retval;
}

Pixel *Colorset::padPixels (int xsize, int ysize, Pixel *pixels, int width,
  int height, int xlo, int ylo)
{
  Pixel *retval;

  if (width < 0) {
    width =      -width;
    xlo   = xlo - width;
  }
  if (height < 0) {
    height =      -height;
    ylo    = ylo - height;
  }

  if (xsize <= 0 || ysize <= 0) {
    if (pixels) free (pixels);
    retval = NULL;
    pixels = retval;
    return retval;
  }

  if (width > 0 && height > 0) {
    if (!pixels) {
      width = 0;
      height = 0;
    }
    assert (xlo + width  <= xsize);
    assert (ylo + height <= ysize);
  }

  retval = (Pixel *)malloc (xsize*ysize*sizeof(Pixel));

  int ix, iy, indx, indx2;
  for (iy = 0, indx = 0; iy < ylo; iy++) {
    for (ix = 0; ix < xsize; ix++, indx++) {
      retval[indx] = _white;
    }
  }
  for (indx2 = 0; iy < height; iy++) {
    for (ix = 0; ix < xlo; ix++, indx++) {
      retval[indx] = _white;
    }
    for (;ix < width; ix++, indx++, indx2++) {
      retval[indx] = pixels[indx2];
    }
    for (; ix < xsize; ix++, indx++) {
      retval[indx] = _white;
    }
  }
  for (; iy < ysize; iy++) {
    for (ix = 0; ix < xsize; ix++, indx++) {
      retval[indx] = _white;
    }
  }

  free (pixels);
  pixels = retval;

  return retval;
}

// clear the overlay Pixmap where the underlay does not extend which
//   requires each Pixmap to have the same origin and depth
void Colorset::padPixmap (PixmapSet *overlay, PixmapSet *underlay)
{
  if (!overlay || !underlay) return;

  int oxo, oyo, odepth, owidth, oheight, uxo, uyo, udepth, uwidth, uheight;
  Pixmap opixmap =  overlay->get ();
  Pixmap upixmap = underlay->get ();

  static_get_size (_display, opixmap, &oxo, &oyo, &odepth, &owidth, &oheight);
  static_get_size (_display, upixmap, &uxo, &uyo, &udepth, &uwidth, &uheight);
  assert (oxo == uxo && oyo == uyo && odepth == 1 && udepth > 1);

  GC gc = (odepth == 1 ? _gc_bitmap : _gc_white);

  int xlo, ylo, width, height;
  if (owidth > uwidth) {
    xlo = oxo + uwidth;
    ylo = oyo;
    width = owidth - uwidth;
    height = oheight;
    paintRectangle (opixmap, gc, xlo, ylo, width, height);
  }
  if (oheight > uheight) {
    xlo = oxo;
    ylo = oyo + uheight;
    width = owidth;
    height = oheight - uheight;
    paintRectangle (opixmap, gc, xlo, ylo, width, height);
  }
}


void Colorset::paintRectangle
                     (PixmapSet *pixmap_set,   // set of pixmaps
                      GC gc,
                      int xlo,   int ylo,      // in destination drawable.
                      int width, int height)   // in destination drawable.
{
  clearRectangle (pixmap_set, xlo, ylo, width, height);
  paintRectangle (pixmap(pixmap_set), gc, xlo, ylo, width, height);
  clearCombosWith (pixmap(pixmap_set));
}



void Colorset::paintBits
                     (PixmapSet *pixmap_set,   // set of pixmaps
                      const char bits[],       // bits 1 (black) or 0 (white).
      /* AA */        int xsize, int ysize,    // number of bits in bits array.
                      int xlo,   int ylo,      // in bits array.
                      int width, int height,   // in bits and destination.
                      int xlo2,  int ylo2)     // in destination drawable.
{
  clearRectangle (pixmap_set, xlo, ylo, width, height);
  paintBits (pixmap(pixmap_set), bits, xsize, ysize, xlo, ylo, width,
    height, xlo2, ylo2);
  //clearCombosWith (pixmap(pixmap_set)); // apparently redundant
}



void Colorset::paintDrawable
                     (PixmapSet *pixmap_set,   // set of pixmaps
                      const char blackwhite[], // TRUE (black) or FALSE (white).
      /* AA */        int xsize, int ysize,    // size of blackwhite array.
                      int xlo,   int ylo,      // in blackwhite array.
                      int width, int height,   // in blackwhite and destination.
                      int xlo2,  int ylo2)     // in destination drawable.
{
  paintDrawable (pixmap(pixmap_set), blackwhite, xsize, ysize, xlo,
    ylo, width, height, xlo2, ylo2);
}



void Colorset::paintDrawable
                     (PixmapSet *pixmap_set,   // set of pixmaps
                      const float source[],    // values in source data.
      /* BB */        int xsize, int ysize,    // size of source array.
                      int xlo,   int ylo,      // in source array.
                      int width, int height,   // in source and destination.
                      int xlo2,  int ylo2)     // in destination drawable.
{
  clearRectangle (pixmap_set, xlo, ylo, width, height);
  paintDrawable (pixmap(pixmap_set), source, xsize, ysize, xlo, ylo,
    width, height, xlo2, ylo2);
  clearCombosWith (pixmap(pixmap_set));
}



void Colorset::paintDrawable
                     (PixmapSet *pixmap_set,   // set of pixmaps
                      const int indices[],     // indices into the colorbar.
      /* BB */        int xsize, int ysize,    // size of indices array.
                      int xlo,   int ylo,      // in indices array.
                      int width, int height,   // in indices and destination.
                      int xlo2,  int ylo2)     // in destination drawable.
{
  clearRectangle (pixmap_set, xlo, ylo, width, height);
  paintDrawable (pixmap(pixmap_set), indices, xsize, ysize, xlo, ylo,
    width, height, xlo2, ylo2);
  clearCombosWith (pixmap(pixmap_set));
}


void Colorset::paintDrawable
                 (PixmapSet *pixmap_set,       // set of pixmaps
                  const unsigned int pixels[], // XImage pixels are 32 bits.
      /* CC */    int xsize, int ysize,        // size of pixels array.
                  int xlo,   int ylo,          // in pixels array.
                  int width, int height,       // in pixels and destination.
                  int xlo2,  int ylo2)         // in destination drawable.
{
  clearRectangle (pixmap_set, xlo, ylo, width, height);
  paintDrawable (pixmap(pixmap_set), pixels, xsize, ysize, xlo, ylo,
    width, height, xlo2, ylo2);
  clearCombosWith (pixmap(pixmap_set));
}


void Colorset::copyDrawable
                    (Drawable drawable,      // pixmap or window or BITMAP.
                     PixmapSet *pixmap_set,  // set of pixmaps
                     int xlo  , int ylo   ,  // in source drawable.
                     int width, int height,  // in source and destination.
                     int xlo2 , int ylo2  )  // in destination drawable.
{
  // this the whole reason to have the combos
  copyDrawable (drawable, pixmap(pixmap_set), xlo, ylo, width, height, xlo2,
    ylo2);
}


void Colorset::clearDrawable
                     (PixmapSet *pixmap_set,   // set of pixmaps
                      int xlo,   int ylo,      // in destination drawable.
                      int width, int height)   // in destination drawable.
{
  clearRectangle (pixmap_set, xlo, ylo, width, height);
  clearDrawable (pixmap(pixmap_set), xlo, ylo, width, height);
  clearCombosWith (pixmap(pixmap_set));
}


void Colorset::putOverlay
                  (Drawable drawable,       // pixmap or window to paint.
                   PixmapSet *pixmap_set,   // set of pixmaps
                   int xlo,   int ylo,      // in overlay.
                   int width, int height,   // in overlay and destination.
                   int xlo2,  int ylo2)     // in destination drawable.
                                            // (overlay was painted by AA)
                                            // (underlay remains in place)
{
  putOverlay (drawable, pixmap(pixmap_set), xlo, ylo, width, height, xlo2,
    ylo2);
//  clearCombosWith (pixmap(pixmap_set)); // usually only on an expose!
}



void Colorset::putUnderlay
                  (Drawable drawable,     // pixmap or window to paint.
                   PixmapSet *pixmap_set, // set of pixmaps
                   int xlo,   int ylo,    // in underlay.
                   int width, int height, // in underlay and destination.
                   int xlo2,  int ylo2)   // in destination drawable.
                                          // (underlay was painted by BB or CC)
                                          // (overlay remains in place)
{
  clearRectangle (pixmap_set, xlo, ylo, width, height);
  putUnderlay (drawable, pixmap(pixmap_set), xlo, ylo, width, height, xlo2,
    ylo2);
  clearCombosWith (pixmap(pixmap_set));
}



void Colorset::clearOverlay
                  (PixmapSet *pixmap_set,   // set of pixmaps
                   int xlo,   int ylo,      // in destination drawable.
                   int width, int height)   // in destination drawable.
                                            // (overlay is cleared)
                                            // (underlay remains in place)
{
  clearRectangle (pixmap_set, xlo, ylo, width, height);
  clearOverlay (pixmap(pixmap_set), xlo, ylo, width, height);
  clearCombosWith (pixmap(pixmap_set));
}



void Colorset::clearUnderlay
                  (PixmapSet *pixmap_set,   // set of pixmaps
                   int xlo,   int ylo,      // in destination drawable.
                   int width, int height)   // in destination drawable.
                                            // (underlay is cleared)
                                            // (overlay remains in place)
{
  clearRectangle (pixmap_set, xlo, ylo, width, height);
  clearUnderlay (pixmap(pixmap_set), xlo, ylo, width, height);
  clearCombosWith (pixmap(pixmap_set));
}



void Colorset::putOverlayOnPixels (
                  Drawable drawable,       // pixmap or window to paint.
                  Pixmap overlay,          // BITMAP to get and put on pixels.
                  Pixel *data,             // pixels (freed by this function).
                  int xlo,   int ylo,      // in overlay.
                  int width, int height,   // size of data, overlay, and dest.
                  int xlo2,  int ylo2)     // in destination drawable.
{
  assert(data);

  Pixel *overlay_data = getPixels (overlay, width, height, xlo, ylo);

  if (!overlay_data) {
    free (data);
    return;
  }

  // insert black in the data if the overlay_data is nonzero
  //   it seems like the test should be if the overlay_data is not white then
  //   use the black pixel but experience shows that 0 is for blank overlay
  //   data and 1U is for an overlay pixel showing
  int indx;
  for (indx = 0; indx < width*height; indx++) {
    if (overlay_data[indx]) {
      data[indx] = _black;
    }
  }

  free (overlay_data);

  privatePaintDrawable (drawable, data, width, height, 0, 0, width, height,
    xlo2, ylo2);
}

void Colorset::copyOverlayOnUnderlay (
                     Drawable drawable,      // pixmap or window or BITMAP.
                     PixmapSet *overlay,     // Overlay PixmapSet
                     PixmapSet *underlay,    // Underlay PixmapSet
                     int xlo  , int ylo   ,  // in source drawable.
                     int width, int height,  // in source and destination.
                     int xlo2 , int ylo2  )  // in destination drawable.
{
  assert (overlay && underlay);

  if (width  < 0) {
     width  = -width ; xlo = xlo - width ; xlo2 = xlo2 - width ;
  }
  if (height < 0) {
    height = -height; ylo = ylo - height; ylo2 = ylo2 - height;
  }

  if (width  == 0) width  = static_get_width  (_display, underlay->get())
    - xlo;
  if (height == 0) height = static_get_height (_display, underlay->get())
    - ylo;

  if (width <= 0 || height <= 0) return;

  int depth = static_get_depth (_display, underlay->get());
  assert (depth > 1);

  // combinePixmaps does the heavy lifting
  XCopyArea (_display, combinePixmaps(overlay,underlay), drawable, _gc_white,
    xlo, ylo, width, height, xlo2, ylo2);
}



#define INCR 10
Pixmap Colorset::pixmap (PixmapSet *pixmap_set, int *index)
{
  pixmap_set->redrawIfNecessary (); // is Pixmap prepared to be used

  Pixmap retval;
  if (retval = existingPixmap(pixmap_set,index)) return retval;

  if (_num_pixmaps == _alloc_pixmaps) {
    int alloc_pixmaps = _alloc_pixmaps + INCR;
    SpecificPixmap **pixmaps = (SpecificPixmap **)malloc (
      alloc_pixmaps*sizeof(SpecificPixmap *));

    int k2;
    for (k2 = 0; k2 < _num_pixmaps; k2++) {
      pixmaps[k2] = _pixmaps[k2];
    }
    if (_pixmaps) free (_pixmaps);
    _pixmaps = pixmaps;
    _alloc_pixmaps = alloc_pixmaps;
  }
  _pixmaps[_num_pixmaps] = new SpecificPixmap (pixmap_set);
  int indx = _num_pixmaps++;
  if (index) *index = indx;
  return _pixmaps[indx]->get ();
}

Pixmap Colorset::existingPixmap (PixmapSet *pixmap_set, int *index)
{
  int k2;
  for (k2 = 0; k2 < _num_pixmaps; k2++) {
    if (_pixmaps[k2]->matches(pixmap_set)) {
      if (index) *index = k2;
      return _pixmaps[k2]->get ();
    }
  }
  return 0;
}

void Colorset::removePixmap (PixmapSet *pixmap_set)
{
  int k2, index;
  for (k2 = 0, index = -1; k2 < _num_pixmaps; k2++) {
    if (_pixmaps[k2]->matches(pixmap_set)) {
      index = k2;
      delete _pixmaps[k2];
      k2 = _num_pixmaps;
    }
  }
  
  if (index > -1) {
    for (k2 = index+1; k2 < _num_pixmaps; k2++) {
      _pixmaps[k2-1] = _pixmaps[k2];
    }
    _num_pixmaps--;
    _pixmaps[_num_pixmaps] = NULL;
  }
  else {
    return;
  }
}

Pixmap Colorset::combinePixmaps (PixmapSet *overlay, PixmapSet *underlay,
  int *index)
{
  assert (overlay && underlay);

  int k2;
  for (k2 = 0; k2 < _num_combos; k2++) {
    if (_combos[k2]->matches(overlay,underlay)) {
      if (index) *index = k2;
      return *(_combos[k2]->get());
    }
  }

  if (_num_combos == _alloc_combos) {
    int alloc_combos = _alloc_combos + INCR;
    PixmapCombo **combos = (PixmapCombo **)malloc (
      alloc_combos*sizeof(PixmapCombo *));
    for (k2 = 0; k2 < _num_combos; k2++) {
      combos[k2] = _combos[k2];
    }
    if (_combos) free (_combos);
    _combos = combos;
    _alloc_combos = alloc_combos;
  }
  _combos[_num_combos] = new PixmapCombo (_display, overlay, underlay);
  int indx = _num_combos++;
  if (index) *index = indx;
  return *(_combos[indx]->get());
}

void Colorset::removePixmapCombo (PixmapSet *overlay, PixmapSet *underlay)
{
  int k2, index;
  for (k2 = 0, index = -1; k2 < _num_combos; k2++) {
    if (_combos[k2]->matches(overlay,underlay)) {
      index = k2;
      delete _combos[k2];
      k2 = _num_combos;
    }
  }
  
  if (index > -1) {
    for (k2 = index+1; k2 < _num_combos; k2++) {
      _combos[k2-1] = _combos[k2];
    }
    _num_combos--;
    _combos[_num_combos] = NULL;
  }
  else {
    return;
  }
}


void Colorset::clearCombosWith (Pixmap pixmap)
{
  if (!pixmap) return;

  int k2;
  for (k2 = 0; k2 < _num_combos; k2++) {
    if (_combos[k2]->matches(pixmap)) {
      _combos[k2]->clear ();
    }
  }
}

// The reason for this function is that when an underlay pixmap is reused
//   it was set to black out beyond the width and height of the current
//   dimensions. My attempt to remedy the bizare problem is to initialize
//   as white the region from the currnet underlay pixmap dimensions to the
//   maximum extent of any existing pixmaps. What a bunch of Voodoo computer
//   science! 
void Colorset::clearRectangle (PixmapSet *pixmap_set, int xlo, int ylo,
  int width, int height)
{
  int now = 1;
  if (now == 1) return;
  if (_num_pixmaps > 0) {
    Pixmap given_pixmap = pixmap (pixmap_set);
    if (!given_pixmap) return;
    unsigned int depth = static_get_depth (_display, given_pixmap);
    GC  gc    = (depth == 1 ? _gc_bitmap : _gc_white);
 
    Window root;
    int x, y;
    unsigned int loc_width, loc_height, bwidth, max_width, max_height;
    Pixmap loc_pixmap;
    Status status;

    // find the maximum sized pixmap
    max_width = 0;
    max_height = 0;
    int k2;
    for (k2 = 0; k2 < _num_pixmaps; k2++) {
      loc_pixmap = _pixmaps[k2]->get ();
      if (loc_pixmap) {
        status = XGetGeometry (_display, loc_pixmap, &root, &x, &y,
	  &loc_width, &loc_height, &bwidth, &depth);
        if (status) {
	  if (loc_width  > max_width ) max_width  = loc_width;
	  if (loc_height > max_height) max_height = loc_height;
	}
      }
    }
    if (max_width > width || max_height > height) {
      int loc_xlo, loc_ylo;
      if (max_width  > width ) {
        loc_ylo = ylo;
      }
      else {
	loc_ylo = height;
      }
      if (max_height > height) {
        loc_xlo = xlo;
      }
      else {
	loc_xlo = width;
      }
      paintRectangle (given_pixmap, gc, loc_xlo, loc_ylo, max_width,
        max_height);
    }
  }
}

int Colorset::comboExists (PixmapSet *overlay, PixmapSet *underlay)
{
  int k2;
  for (k2 = 0; k2 < _num_combos; k2++) {
    if (_combos[k2]->matches (overlay,underlay) && _combos[k2]->exists()) {
      return TRUE;
    }
  }
  return FALSE;
}


SpecificPixmap::SpecificPixmap (PixmapSet *pixmap_set) :
  _pixmap_set  (pixmap_set)
{
  assert (pixmap_set);
}

SpecificPixmap::~SpecificPixmap ()
{
}

int SpecificPixmap::matches (PixmapSet *pixmap_set)
{
  if (pixmap_set == _pixmap_set                                 &&
      pixmap_set->currentIndex() == _pixmap_set->currentIndex()   ) {
    return TRUE;
  }
  else {
    return FALSE;
  }
}

Pixmap SpecificPixmap::get ()
{
  return _pixmap_set->get (_pixmap_set->currentIndex());
}

void SpecificPixmap::markChanged ()
{
  _pixmap_set->markChanged (_pixmap_set->currentIndex());
}




PixmapCombo::PixmapCombo (Display *display, PixmapSet *overlay,
  PixmapSet *underlay) :
  _display   (display),
  _overlay   (overlay),
  _underlay  (underlay),
  _combo     (0)
{
  assert (overlay && underlay);
  _oindex =  overlay->currentIndex ();
  _uindex = underlay->currentIndex ();
}

PixmapCombo::~PixmapCombo ()
{
  if (_combo) XFreePixmap (_display, _combo);
}

int PixmapCombo::matches (PixmapSet *overlay, PixmapSet *underlay)
{
  if ( overlay                 ==  _overlay &&
       overlay->currentIndex() ==  _oindex  &&
      underlay                 == _underlay &&
      underlay->currentIndex() == _uindex     ) {
    return TRUE;
  }
  else {
    return FALSE;
  }
}

int PixmapCombo::matches (Pixmap pixmap)
{
  if (( _overlay->indexOK(_oindex)       &&
        _overlay->get (_oindex) == pixmap  ) ||
      (_underlay->indexOK(_uindex)       &&
       _underlay->get(_uindex) == pixmap   )   ) {
    return TRUE;
  }
  else {
    return FALSE;
  }
}

int PixmapCombo::exists ()
{
  return _combo != 0;
}

void PixmapCombo::clear ()
{
  if (_combo) {
    XFreePixmap (_display, _combo);
    _combo = 0;
  }
}

Pixmap *PixmapCombo::get ()
{
  if (!_combo || _overlay->changed(_oindex) || _underlay->changed(_uindex)) {
    assert (_overlay->get(_oindex) && _underlay->get(_uindex));
    Window root;
    int x, y;
    unsigned int width, height, bwidth;
    unsigned int owidth, oheight, odepth, uwidth, uheight, udepth;

    Status status;
    status = XGetGeometry (_display, _overlay->get(_oindex), &root, &x, &y,
      &owidth, &oheight, &bwidth, &odepth);
    status = XGetGeometry (_display, _underlay->get(_uindex), &root, &x, &y,
      &uwidth, &uheight, &bwidth, &udepth);

    // overall dimensions may not match; pick largest
    width  = uwidth  > owidth  ? uwidth  : owidth ;
    height = uheight > oheight ? uheight : oheight;
    assert (odepth == 1 && udepth > 1);

    // find the border dimensions for both overlay and underlay
    int  topb =  _overlay->plotImage()->getTopBorder    ();
    int  botb =  _overlay->plotImage()->getBottomBorder ();
    int  lefb =  _overlay->plotImage()->getLeftBorder   ();
    int  rigb =  _overlay->plotImage()->getRightBorder  ();
    int utopb = _underlay->plotImage()->getTopBorder    ();
    int ubotb = _underlay->plotImage()->getBottomBorder ();
    int ulefb = _underlay->plotImage()->getLeftBorder   ();
    int urigb = _underlay->plotImage()->getRightBorder  ();

    // border dimensions must match
    assert (topb == utopb && lefb == ulefb &&
            rigb == urigb && botb == ubotb   );

    // adjust width and height so as to not to extend nonwhite pixels
    //   beyond borders
    owidth  = owidth  - rigb;
    oheight = oheight - botb;
    uwidth  = uwidth  - rigb;
    uheight = uheight - botb;

    if (_combo) XFreePixmap (_display, _combo);
    Paintset *paintset = _overlay->colorsetIs()->paintset ();
    _combo = XCreatePixmap (_display, paintset->rootWindow(), width, height,
      paintset->visualDepth());

    Pixel plane_mask = AllPlanes;
    int   format     = ZPixmap;

    XImage *oimage = XGetImage (_display, _overlay->get(_oindex), 0, 0,
      owidth, oheight, plane_mask, format);

    XImage *uimage = XGetImage (_display, _underlay->get(_uindex), 0, 0,
      uwidth, uheight, plane_mask, format);

    int num_pixels = width * height;
    unsigned int *pixels = (unsigned int *)malloc (num_pixels
      *sizeof(unsigned int));

    int indx, ix, iy;
    Pixel white = _overlay->colorsetIs()->paintset()->white ();
    Pixel black = _overlay->colorsetIs()->paintset()->black ();
    Pixel opixel, upixel;
    for (indx = 0; indx < num_pixels; indx++) {
      iy = indx / width;
      ix = indx - iy * width;
      if (!(iy < topb || ix < lefb)) {
	if (iy < oheight && ix < owidth) {
	  opixel = XGetPixel (oimage, ix, iy);
	}
	else {
	  opixel = 0;
	}
	if (iy < uheight && ix < uwidth) {
	  upixel = XGetPixel (uimage, ix, iy);
	}
	else {
	  upixel = white;
	}
      }
      else {
	opixel = 0;
	upixel = white;
      }
      if (opixel) {
	pixels[indx] = (unsigned int)black;
      }
      else {
	pixels[indx] = (unsigned int)upixel;
      }
    }
    XDestroyImage (oimage);
    XDestroyImage (uimage);

    _overlay->colorsetIs()->paintDrawable (_combo,
      (const unsigned int *)pixels, width, height, 0, 0, width, height);
    free (pixels);

    _underlay->plotImage()->drawPlotLabel (&_combo);
    
  }
  return &_combo;
}

//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//
