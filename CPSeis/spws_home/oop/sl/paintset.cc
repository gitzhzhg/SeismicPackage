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

//--------------------------- paintset.cc --------------------------------//
//--------------------------- paintset.cc --------------------------------//
//--------------------------- paintset.cc --------------------------------//

//              implementation file for the Paintset class
//                      not derived from any class
//                           subdirectory sl


       // index = index into the colormap (0 thru _colormap_size - 1).
       // icolor = index into list of colors (0 thru ncolors - 1).


//---------------------------- start -------------------------------------//
//---------------------------- start -------------------------------------//
//---------------------------- start -------------------------------------//


#include "sl/paintset.hh"
#include <Xm/Xm.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include <stdlib.h>

static double FACTOR = (double)100.0 / (double)Paintset::BRIGHTEST;


//-------------------- small static functions -----------------------------//
//-------------------- small static functions -----------------------------//
//-------------------- small static functions -----------------------------//


static int get_shift_from_mask (unsigned long mask)
{
  int shft = 0;
  while(mask && !((mask >> shft) & 1)) {shft++; }
  return shft;
}


static int get_index (Pixel pixel, Pixel mask, int shft, int colormap_size)
{
  int index;
  if(mask > 0)
      {
      index = (int)((pixel & mask) >> shft);
      }
  else
      {
      index = (int)pixel;
      }
  assert(index >= 0 && index < colormap_size);
  return index;
}


static void put_index
             (Pixel *pixel, Pixel mask, int shft, int index, int colormap_size)
{
  assert(pixel);
  assert(index >= 0 && index < colormap_size);
  if(mask > 0)
      {
      *pixel = (*pixel) | (((Pixel)index << shft) & mask);
      }
  else
      {
      *pixel = (Pixel)index;
      }
}


static const char *get_vclass_name(int vclass)
{
  static char buffer[22];
  switch(vclass)
      {
      case PseudoColor: strcpy(buffer, "PseudoColor"); break;
      case StaticColor: strcpy(buffer, "StaticColor"); break;
      case GrayScale  : strcpy(buffer, "GrayScale"  ); break;
      case StaticGray : strcpy(buffer, "StaticGray" ); break;
      case DirectColor: strcpy(buffer, "DirectColor"); break;
      case TrueColor  : strcpy(buffer, "TrueColor"  ); break;
      default         : assert(FALSE);                 break;
      }
  return buffer;
}


//----------------------- split and build pixel ---------------------------//
//----------------------- split and build pixel ---------------------------//
//----------------------- split and build pixel ---------------------------//


void Paintset::splitPixel (Pixel pixel,
                   int *red_index, int *green_index, int *blue_index)  const
{
  *red_index   = get_index (pixel,   _red_mask,   _red_shift, _colormap_size);
  *green_index = get_index (pixel, _green_mask, _green_shift, _colormap_size);
  *blue_index  = get_index (pixel,  _blue_mask,  _blue_shift, _colormap_size);
}


Pixel Paintset::buildPixel
                  (int  red_index, int  green_index, int  blue_index)  const
{
  Pixel pixel = 0;
  put_index (&pixel,   _red_mask,   _red_shift,   red_index, _colormap_size);
  put_index (&pixel, _green_mask, _green_shift, green_index, _colormap_size);
  put_index (&pixel,  _blue_mask,  _blue_shift,  blue_index, _colormap_size);
  return pixel;
}


//--------------------------- constructor --------------------------------//
//--------------------------- constructor --------------------------------//
//--------------------------- constructor --------------------------------//


Paintset::Paintset(Display *display, int iscreen, int vprivate, int vdynamic)
       : 
         _display             (display),
         _iscreen             (iscreen),
         _screen              (NULL),
         _vprivate            (vprivate),       // might get changed.
         _vdepth              (0),
         _vclass              (0),
         _red_mask            (0),
         _green_mask          (0),
         _blue_mask           (0),
         _red_shift           (0),
         _green_shift         (0),
         _blue_shift          (0),
         _colormap_size       (0),
         _bits_per_rgb        (0),
         _visual              (NULL),
         _colormap            (0),
         _black               (0),
         _white               (0),
         _clear               (0),
         _read_only           (FALSE),
	 _nplanes             (0),
         _allocated_red       (NULL),
         _allocated_green     (NULL),
         _allocated_blue      (NULL),
         _red_color           (NULL),
         _green_color         (NULL),
         _blue_color          (NULL),
         _stored_colors       (0),
	 _named_pixels        (NULL),
	 _named_colors        (NULL),
         _num_named_colors    (0),
         _alloc_named_colors  (0)
{
  assert(_display);
  _screen = ScreenOfDisplay(_display, _iscreen);
  assert(_screen);

/////////////// find out what the default visual is.

  Visual      *default_visual   = DefaultVisual  (_display, _iscreen);
  Colormap     default_colormap = DefaultColormap(_display, _iscreen);

  _visual = default_visual;

  privateGetVisualAttributes();

/////////////// print the default visual.
/*
  printf("\nScreen %d:\n", _iscreen);
  printf("Default colormap is %08lx.\n", default_colormap);
  printf("Default visual is %s depth %d.\n",
                                   get_vclass_name(_vclass), _vdepth);
*/

/////////////// choose desired visual class (with same depth as default visual).

  int default_vclass = _vclass;

  if(vdynamic)
      {
      switch(_vclass)
          {
          case PseudoColor:                                          break;
          case StaticColor: _vclass = PseudoColor; _vprivate = TRUE; break;
          case GrayScale  :                                          break;
          case StaticGray : _vclass = GrayScale  ; _vprivate = TRUE; break;
          case DirectColor:                                          break;
          case TrueColor  : _vclass = DirectColor; _vprivate = TRUE; break;
          default         : assert(FALSE);                           break;
          }
      }

  switch(_vclass)
      {
      case PseudoColor: _read_only = FALSE; break;
      case StaticColor: _read_only = TRUE;  break;
      case GrayScale  : _read_only = FALSE; break;
      case StaticGray : _read_only = TRUE;  break;
      case DirectColor: _read_only = FALSE; break;
      case TrueColor  : _read_only = TRUE;  break;
      default         : assert(FALSE);      break;
      }

/////////////// print the desired visual.

  if(_vclass == default_vclass)
      {
      if(_vprivate)
          {
/*
          printf("Creating new colormap with default visual.\n");
*/
          }
      else
          {
/*
          printf("Using default colormap with default visual.\n");
*/
          }
      }
  else
      {
/*
      printf("Creating new colormap using visual %s depth %d.\n",
                                     get_vclass_name(_vclass), _vdepth);
*/
      }

/////////////// get new visual.

  if(_vclass == default_vclass)
      {
      _visual = default_visual;
      }
  else
      {
      XVisualInfo info;
      Status status = XMatchVisualInfo
                             (_display, _iscreen, _vdepth, _vclass, &info);
      if(!status)
          {
          printf("Requested visual not available.\n");
          assert(FALSE);
          }
      _visual = info.visual;
      }

/////////////// get several attributes from visual:

  privateGetVisualAttributes();

/////////////// get new colormap.

  if(_vprivate)
      {
      Window window = RootWindow(_display, _iscreen);
      _colormap = XCreateColormap(_display, window, _visual, AllocNone);
/*
      printf("New colormap is %08lx.\n", _colormap);
*/
      }
  else
      {
      _colormap = default_colormap;
      }
/*
  printf("\n");
*/

/////////////// initialize new colormap.

  if(_vprivate && _vclass == default_vclass)
      {
      privateCopyFromOldColormap(default_colormap);
      }

  _black = BlackPixel(_display, _iscreen);        // initialize _black.
  _white = WhitePixel(_display, _iscreen);        // initialize _white.
  _black = getForegroundPixelFromName("black");   // uses _black.
  _white = getBackgroundPixelFromName("white");   // uses _white.
  _clear = _black;                                // same as _black.

/////////////// allocate and initialize copy of colors for efficiency:

  _allocated_red   = new int [_colormap_size];
  _allocated_green = new int [_colormap_size];
  _allocated_blue  = new int [_colormap_size];
  _red_color       = new int [_colormap_size];
  _green_color     = new int [_colormap_size];
  _blue_color      = new int [_colormap_size];

  for(int index = 0; index < _colormap_size; index++)
      {
      _allocated_red  [index] = 0;
      _allocated_green[index] = 0;
      _allocated_blue [index] = 0;
      _red_color      [index] = 7777;    // will be replaced.
      _green_color    [index] = 8888;    // will be replaced.
      _blue_color     [index] = 9999;    // will be replaced.
      }

  privateRememberColors();
  _colorset_naming_policy = _read_only ? NAME_AS_IS : NAMES_BY_PAINTSETS;
}


//-------------------------- destructor ---------------------------------//
//-------------------------- destructor ---------------------------------//
//-------------------------- destructor ---------------------------------//


Paintset::~Paintset()
{
  if(_vprivate)
      {
      XFreeColormap(_display, _colormap);
      }
  delete [] _allocated_red;
  delete [] _allocated_green;
  delete [] _allocated_blue;
  delete [] _red_color;
  delete [] _green_color;
  delete [] _blue_color;
}


//------------------- private remember colors -----------------------------//
//------------------- private remember colors -----------------------------//
//------------------- private remember colors -----------------------------//


void Paintset::privateRememberColors()
{
  XColor *colors = new XColor [_colormap_size];

  int index;
  for(index = 0; index < _colormap_size; index++)
      {
      colors[index].pixel = buildPixel(index, index, index);
      colors[index].flags = DoRed | DoGreen | DoBlue;
      }
  XQueryColors(_display, _colormap, colors, _colormap_size);
  for(index = 0; index < _colormap_size; index++)
      {
      _red_color  [index] = colors[index].red;
      _green_color[index] = colors[index].green;
      _blue_color [index] = colors[index].blue;
      }
  delete [] colors;
}


//-------------------- private remember color -----------------------------//
//-------------------- private remember color -----------------------------//
//-------------------- private remember color -----------------------------//


void Paintset::privateRememberColor(Pixel pixel)
{
  XColor color;
  color.pixel = pixel;
  color.flags = DoRed | DoGreen | DoBlue;
  XQueryColor(_display, _colormap, &color);
  int red_index   = getRedIndex  (pixel);
  int green_index = getGreenIndex(pixel);
  int blue_index  = getBlueIndex (pixel);
  _red_color  [red_index  ] = color.red;
  _green_color[green_index] = color.green;
  _blue_color [blue_index ] = color.blue;
}


//---------------------------- remake private ------------------------------//
//---------------------------- remake private ------------------------------//
//---------------------------- remake private ------------------------------//


void Paintset::remakePrivate()
{
  if(_vprivate) return;
  assert(_colormap);
  Colormap old_colormap = _colormap;

/*****************
  _colormap = XCopyColormapAndFree(_display, old_colormap);
*****************/

  Window window = RootWindow(_display, _iscreen);
  _colormap = XCreateColormap(_display, window, _visual, AllocNone);

  privateCopyFromOldColormap(old_colormap);

  _black = BlackPixel(_display, _iscreen);        // initialize _black.
  _white = WhitePixel(_display, _iscreen);        // initialize _white.
  _black = getForegroundPixelFromName("black");   // uses _black.
  _white = getBackgroundPixelFromName("white");   // uses _white.
  _clear = _black;                                // uses _black.

  privateRememberColors();

/*
  printf("\nScreen %d:\n", _iscreen);
  printf("Default colormap %08lx", old_colormap);
  printf(" replaced with new private colormap %08lx.\n\n", _colormap);
*/

  _vprivate = TRUE;
}


void Paintset::tryPrivateAgain (int num_copy)
{
  assert (_vprivate);
  if (num_copy >= NCOPY) return;
  assert (_colormap);
  Colormap old_colormap = _colormap;

  Window window = RootWindow (_display, _iscreen);
  _colormap = XCreateColormap (_display, window, _visual, AllocNone);

  privateCopyFromOldColormap (old_colormap, &num_copy);

  privateRememberColors ();
}


//------------------------------ add resources ----------------------------//
//------------------------------ add resources ----------------------------//
//------------------------------ add resources ----------------------------//


void Paintset::addResources(Arg *arglist, int *n)
{
  XtSetArg (arglist[*n], XmNcolormap  , _colormap   ); (*n)++;
  XtSetArg (arglist[*n], XmNvisual    , _visual     ); (*n)++;
  XtSetArg (arglist[*n], XmNdepth     , _vdepth     ); (*n)++;
  XtSetArg (arglist[*n], XmNscreen    , _screen     ); (*n)++;
/****
  XtSetArg (arglist[*n], XmNbackground, _white      ); (*n)++;
  XtSetArg (arglist[*n], XmNforeground, _black      ); (*n)++;
****/
}


//--------------------- private get visual attributes ----------------------//
//--------------------- private get visual attributes ----------------------//
//--------------------- private get visual attributes ----------------------//


void Paintset::privateGetVisualAttributes()
{
  long         vmask  = VisualScreenMask;
  int          nitems = 0;
  XVisualInfo  vtemplate;

  vtemplate.screen        = _iscreen;
  vtemplate.depth         = 0;
  vtemplate.c_class       = 0;
  vtemplate.red_mask      = 0;
  vtemplate.green_mask    = 0;
  vtemplate.blue_mask     = 0;
  vtemplate.colormap_size = 0;
  vtemplate.bits_per_rgb  = 0;

  XVisualInfo *list = XGetVisualInfo(_display, vmask, &vtemplate, &nitems);

  _vdepth        = 0;
  _vclass        = 0;
  _colormap_size = 0;

  for(int item = 0; item < nitems; item++)
      {
      if(list[item].visual == _visual)
          {
          _vdepth        = list[item].depth;
          _vclass        = list[item].c_class;
          _red_mask      = list[item].red_mask;
          _green_mask    = list[item].green_mask;
          _blue_mask     = list[item].blue_mask;
          _colormap_size = list[item].colormap_size;
          _bits_per_rgb  = list[item].bits_per_rgb;
          break;
          }
      }
  XFree(list);

  assert(_vdepth > 0);
  assert(_vclass != 0);
  assert(_colormap_size > 0);

  _red_shift   = get_shift_from_mask(  _red_mask);
  _green_shift = get_shift_from_mask(_green_mask);
  _blue_shift  = get_shift_from_mask( _blue_mask);
}


//------------------- private copy from old colormap -----------------------//
//------------------- private copy from old colormap -----------------------//
//------------------- private copy from old colormap -----------------------//


void Paintset::privateCopyFromOldColormap (Colormap old_colormap,
  int *num_copy)
{
  assert(old_colormap);
  assert(_display);
  assert(_colormap);

  XColor colors[NCOPY];
  Pixel  pixels[NCOPY];

  int ncopy;
  if (num_copy) {
    assert (*num_copy < NCOPY && *num_copy < _colormap_size);
    ncopy = *num_copy;
  }
  else {
    ncopy = MinimumValue(NCOPY, _colormap_size / 4);
  }

  for(int index = 0; index < ncopy; index++)
      {
      colors[index].pixel = buildPixel(index, index, index);
      colors[index].flags = DoRed | DoGreen | DoBlue;
      }
  XQueryColors(_display, old_colormap, colors, ncopy);

  int    contig  = FALSE;
  Pixel *planes  = NULL;
        _nplanes = 0;
  Status status  = XAllocColorCells (_display, _colormap,
                                     contig, planes, _nplanes, pixels, ncopy);
  if(status)
      {
      int may_store = TRUE;
      for(int index = 0; index < ncopy; index++)
          {
          if(colors[index].pixel != pixels[index]) may_store = FALSE;
          }
      if (may_store) {
        XStoreColors(_display, _colormap, colors, ncopy);
      }
      if (!_read_only) _stored_colors += ncopy; // from the XAllocColorCells
      }
}


//---------------------- useful convenience functions ---------------------//
//---------------------- useful convenience functions ---------------------//
//---------------------- useful convenience functions ---------------------//


Pixmap Paintset::createPixmap (int width, int height)   const
{
  Drawable drawable = (Drawable)RootWindowOfScreen(_screen);
  return XCreatePixmap(_display, drawable, width, height, _vdepth);
}


Pixmap Paintset::createBitmap (int width, int height)   const
{
  Drawable drawable = (Drawable)RootWindowOfScreen(_screen);
  return XCreatePixmap(_display, drawable, width, height, 1);
}


Pixmap Paintset::createBitmap (int width, int height,
                               const char blackwhite[])   const
{
  assert(blackwhite);
  Drawable drawable = (Drawable)RootWindowOfScreen(_screen);
  int      nbits    = sizeof(char) * CHAR_BIT;
  int      xwidth   = (width + nbits - 1) / nbits;
  int      length   = xwidth * height;
  char    *data     = new char [length];

  memset(data, 0, sizeof(char) * (unsigned int)length);

  for(int iy = 0; iy < height; iy++)
      {
      for(int ix = 0; ix < width; ix++)
          {
          int i     = ix / nbits;
          int ibit  = ix - i * nbits;
          int mask  = (1 << ibit);
          int idata = i + iy * xwidth;
          int value = (data[idata] & mask);
          int indx  = ix + iy * width;

          assert(idata >= 0 && idata < length);
          assert(indx >= 0 && indx < width * height);

          if(blackwhite[indx]) { if(!value) data[idata] |=  mask; }
          else                 { if( value) data[idata] &= ~mask; }
          }
      }

  Pixmap bitmap = XCreateBitmapFromData
                                (_display, drawable, data, width, height);
  delete [] data;
  return bitmap;
}


GC Paintset::createGC (unsigned long valuemask, XGCValues *values,
                       Drawable drawable) const
{
  assert(values);
  if(drawable == 0) drawable = (Drawable)RootWindowOfScreen(_screen);
  return XCreateGC(_display, drawable, valuemask, values);
}


GC Paintset::createGC (Drawable drawable) const
{
  XGCValues values;
  values.foreground = _black;
  values.background = _white;
  unsigned long valuemask = GCForeground | GCBackground;
  return createGC (valuemask, &values, drawable);
}


GC Paintset::createBitmapGC () const
{
  Pixmap bitmap = createBitmap(1,1);
  GC gc = createGC(bitmap);
  freeBitmap(bitmap);
  return gc;
}


void   Paintset::freePixmap   (Pixmap pixmap) const
{
  XFreePixmap(_display, pixmap);
}


void   Paintset::freeBitmap   (Pixmap bitmap) const
{
  XFreePixmap(_display, bitmap);
}


void   Paintset::freeGC       (GC gc)    const
{
  XFreeGC(_display, gc);
}


//--------------------------- print summary -------------------------------//
//--------------------------- print summary -------------------------------//
//--------------------------- print summary -------------------------------//


void Paintset::printSummary()
{
  printf("\n");
  printf("      display %s    screen %d    class %s    depth %d\n",
        DisplayString(_display), _iscreen, get_vclass_name(_vclass), _vdepth);
  printf("\n");
}


//--------------------------- print all colors ----------------------------//
//--------------------------- print all colors ----------------------------//
//--------------------------- print all colors ----------------------------//


void Paintset::printAllColors()
{
  privateRememberColors();
  printf("\n");
  printf("                            ALL COLORS IN COLORMAP\n");
  printSummary();

  for(int index = 0; index < _colormap_size; index++)
      {
      Pixel pixel = buildPixel(index, index, index);
      printf( " index = %3d   pixel = %6lx\
  %3d red = %5d  %3d green = %5d  %3d blue = %5d\n",
             index, pixel,
             _allocated_red  [index], _red_color  [index],
             _allocated_green[index], _green_color[index],
             _allocated_blue [index], _blue_color [index]);
      }
  printf("\n");
}


//-------------------- print colors of default colormaps ------------------//
//-------------------- print colors of default colormaps ------------------//
//-------------------- print colors of default colormaps ------------------//

                         // static function.


void Paintset::printColorsOfDefaultColormaps(Display *display)
{
  assert(display);
  int nscreens = ScreenCount(display);

  for(int iscreen = 0; iscreen < nscreens; iscreen++)
      {
      Paintset *paintset = new Paintset(display, iscreen, FALSE, FALSE);
      paintset->printAllColors();
      delete paintset;
      }
}


//----------------------- print available visuals --------------------------//
//----------------------- print available visuals --------------------------//
//----------------------- print available visuals --------------------------//

                         // static function.


void Paintset::printAvailableVisuals (Display *display)
{
  assert(display);
  int nscreens = ScreenCount(display);

printf("\n");
printf("          AVAILABLE VISUALS FOR DISPLAY %s\n", DisplayString(display));
printf("\n");
printf("                                              shifts          bits\n");
printf(" screen                  red   green     blue RRGGBB colormap  per\n");
printf("     depth    class     mask    mask     mask          size    rgb\n");
printf("\n");

  for(int iscreen = 0; iscreen < nscreens; iscreen++)
      {
      Visual *visual = DefaultVisual(display, iscreen);
      long    vmask  = VisualScreenMask;
      int     nitems = 0;

      XVisualInfo vtemplate;

      vtemplate.screen        = iscreen;
      vtemplate.depth         = 0;
      vtemplate.c_class       = 0;
      vtemplate.red_mask      = 0;
      vtemplate.green_mask    = 0;
      vtemplate.blue_mask     = 0;
      vtemplate.colormap_size = 0;
      vtemplate.bits_per_rgb  = 0;

      XVisualInfo *list = XGetVisualInfo(display, vmask, &vtemplate, &nitems);

      for(int indx = 0; indx < nitems; indx++)
        {
        int red_shift   = get_shift_from_mask(list[indx].red_mask  );
        int green_shift = get_shift_from_mask(list[indx].green_mask);
        int blue_shift  = get_shift_from_mask(list[indx].blue_mask );
        const char *buffer = get_vclass_name(list[indx].c_class);
        char msg[32];
        if(list[indx].visual == visual) strcpy(msg, "default");
        else                            strcpy(msg, "");
        printf("%4d %4d   %11s %4lx %7lx %8lx %2d%2d%2d %6d %5d  %s\n",
                   list[indx].screen,
                   list[indx].depth, buffer,
                   list[indx].red_mask,
                   list[indx].green_mask,
                   list[indx].blue_mask,
                   red_shift, green_shift, blue_shift,
                   list[indx].colormap_size,
                   list[indx].bits_per_rgb, msg);
        }

      XFree(list);
      printf("\n");
      }
}


//---------------------- allocate and free color cells ---------------------//
//---------------------- allocate and free color cells ---------------------//
//---------------------- allocate and free color cells ---------------------//


int Paintset::allocateColorCells (int ncolors, Pixel pixels[], Pixel *plane)
{
  assert(ncolors <= _colormap_size);
  if(plane) *plane = 0;
  if(ncolors == 0) return FALSE;               // no error.

  if(_read_only)
      {
      for(int icolor = 0; icolor < ncolors; icolor++)
          {
          pixels[icolor] = _black;     // will be reset by setColors().
          }
      }
  else
      {
      Pixel  planes[1];
            _nplanes = plane ? 1 : 0;
      Bool   contig  = FALSE;
      Status status  = XAllocColorCells (_display, _colormap, contig,
                                         planes, _nplanes, pixels, ncolors);
      if (!status) return TRUE;    // error.

      if(plane)
          {
          *plane = planes[0];
          for(int icolor = 0; icolor < ncolors; icolor++)
              {
              XColor color;
              color.pixel = pixels[icolor] | *plane;
              color.red   = 0;
              color.green = 0;
              color.blue  = 0;
              color.flags = DoRed | DoGreen | DoBlue;
              XStoreColor(_display, _colormap, &color);
              }
	  _stored_colors += ncolors;       // black for the overlay plane
          }
      }

  for(int icolor = 0; icolor < ncolors; icolor++)
      {
      int   red_index = getRedIndex  (pixels[icolor]);
      int green_index = getGreenIndex(pixels[icolor]);
      int  blue_index = getBlueIndex (pixels[icolor]);
      _allocated_red  [  red_index] += 1;
      _allocated_green[green_index] += 1;
      _allocated_blue [ blue_index] += 1;
      }

  if (!_read_only) _stored_colors += ncolors;// for the XAllocColorCells
  setColors(ncolors, pixels);             // presets shades of gray.
  return FALSE;                           // no error.
}


void Paintset::freeColorCells (int ncolors, const Pixel pixels[], Pixel plane)
{
  assert(ncolors <= _colormap_size);
  if(ncolors == 0) return;

  for(int icolor = 0; icolor < ncolors; icolor++)
      {
      Pixel pixel = pixels[icolor];
      int   red_index = getRedIndex  (pixel);
      int green_index = getGreenIndex(pixel);
      int  blue_index = getBlueIndex (pixel);

/*
      assert(_allocated_red  [  red_index] > 0);
      assert(_allocated_green[green_index] > 0);
      assert(_allocated_blue [ blue_index] > 0);
*/

      _allocated_red  [  red_index] -= 1;
      _allocated_green[green_index] -= 1;
      _allocated_blue [ blue_index] -= 1;
      }
  if(_read_only) return;

  XFreeColors(_display, _colormap, (Pixel*)pixels, ncolors, plane);
  _stored_colors -= ncolors; // free the underlay colors
  if (plane) {
    _stored_colors -= ncolors; // free the black overlay colors
  }
}


//------------------------ num available colors --------------------------//
//------------------------ num available colors --------------------------//
//------------------------ num available colors --------------------------//


int Paintset::numAvailableColors(int nplanes)  const
{
  if(_read_only) return _colormap_size;

  Pixel         *pixels   = new Pixel [_colormap_size];
  Bool           contig   = FALSE;
  unsigned long *planes   = NULL;
  for(int ncolors = _colormap_size - 1; ncolors > 0; ncolors--)
      {
      Status status = XAllocColorCells (_display, _colormap, contig,
                                        planes, nplanes, pixels, ncolors);
      if(status)
           {
           unsigned long planes = 0;
           XFreeColors(_display, _colormap, pixels, ncolors, planes);
           delete [] pixels;
           return ncolors;
           }
      }
  delete [] pixels;
  return 0;
}


//--------------------------- get values ----------------------------------//
//--------------------------- get values ----------------------------------//
//--------------------------- get values ----------------------------------//


const char *Paintset::visualClassName()  const
{
  return get_vclass_name(_vclass);
}


Window Paintset::rootWindow()  const
{
  return RootWindow(_display, _iscreen);
}

                        //////////////////


Pixel Paintset::getBackgroundPixelFromName(const char *colorname)
{
  Pixel retval;

  if (!strcmp(colorname,"white")) {
    if (!findPixelFromName(colorname,&retval)) {
      // "white" has already been allocated but not stored
      retval = _white;
      storePixelFromName (colorname, retval);
    }
    return retval;
  }
  else if (getPixelFromName(colorname,&retval)) {
    return retval;
  }
  return _white;
}


Pixel Paintset::getForegroundPixelFromName(const char *colorname)
{
  Pixel retval;

  if (!strcmp(colorname,"black")) {
    if (!findPixelFromName(colorname,&retval)) {
      // "black" has already been allocated!!
      retval = _black;
      storePixelFromName (colorname, retval);
    }
    return retval;
  }
  else if (getPixelFromName(colorname,&retval)) {
    return retval;
  }
  return _black;
}

                        //////////////////


Pixel Paintset::getForegroundPixelFromColors (int red, int green, int blue)
{
  XColor color;
  color.red   = ConstrainValue(red  , 0, BRIGHTEST);
  color.green = ConstrainValue(green, 0, BRIGHTEST);
  color.blue  = ConstrainValue(blue , 0, BRIGHTEST);
  Status status = XAllocColor (_display, _colormap, &color);
  if (status) {
    if (!_read_only) _stored_colors++;
    return color.pixel;
  }
  return _black;
}


Pixel Paintset::getBackgroundPixelFromColors (int red, int green, int blue)
{
  XColor color;
  color.red   = ConstrainValue(red  , 0, BRIGHTEST);
  color.green = ConstrainValue(green, 0, BRIGHTEST);
  color.blue  = ConstrainValue(blue , 0, BRIGHTEST);
  Status status = XAllocColor (_display, _colormap, &color);
  if (status) {
    if (!_read_only) _stored_colors++;
    return color.pixel;
  }
  return _white;
}
                        //////////////////


void Paintset::getColors (int ncolors, const Pixel pixels[],
                                       int red  [],
                                       int green[],
                                       int blue []) const
{
  if(ncolors <= 0) return;
  assert(ncolors <= _colormap_size);
  assert(pixels);
  XColor *colors = new XColor [ncolors];
  int icolor;
  for(icolor = 0; icolor < ncolors; icolor++)
      {
      colors[icolor].pixel = pixels[icolor];
      colors[icolor].flags = DoRed | DoGreen | DoBlue;
      }
  XQueryColors(_display, _colormap, colors, ncolors);
  for(icolor = 0; icolor < ncolors; icolor++)
      {
      if(red  ) red  [icolor] = colors[icolor].red;
      if(green) green[icolor] = colors[icolor].green;
      if(blue ) blue [icolor] = colors[icolor].blue;
      }
  delete [] colors;
}

void Paintset::getColors (ColorInfo *col,
                                       int **red  ,
			               int **green,
                                       int **blue  ) const
{
  if (col->cnum <= 0) return;
  assert (col->cnum <= _colormap_size && red && green && blue);
  if (!_read_only) {
    // don't yet know how overlay planes are going to be handled in
    //   _read_only 
    assert (col->numplanes == _nplanes);
  }

  XColor *colors = new XColor[col->cnum];
  int icolor;
  for (icolor = 0; icolor < col->cnum; icolor++) {
    colors[icolor].pixel = col->pix[icolor];
    colors[icolor].flags = DoRed | DoGreen | DoBlue;
  }
  XQueryColors (_display, _colormap, colors, col->cnum);

  *red   = (int *)malloc (col->cnum*sizeof(int));
  *green = (int *)malloc (col->cnum*sizeof(int));
  *blue  = (int *)malloc (col->cnum*sizeof(int));

  for (icolor = 0; icolor < col->cnum; icolor++) {
    (*red  )[icolor] = colors[icolor].red;
    (*green)[icolor] = colors[icolor].green;
    (*blue )[icolor] = colors[icolor].blue;
  }
  delete [] colors;
}


void Paintset::getColor (Pixel pixel, int *red, int *green, int *blue)  const
{
  XColor color;
  color.pixel = pixel;
  color.flags = DoRed | DoGreen | DoBlue;
  XQueryColor(_display, _colormap, &color);
  if(red  ) *red   = color.red;
  if(green) *green = color.green;
  if(blue ) *blue  = color.blue;
}


void Paintset::getXColor (Pixel pixel, XColor *color) const
{
  color->pixel = pixel;
  color->flags = DoRed | DoGreen | DoBlue;
  XQueryColor (_display, _colormap, color);
}

                        //////////////////


int Paintset::getRedColor   (Pixel pixel)  const
{
  int index = getRedIndex(pixel);
  return _red_color[index];
}


int Paintset::getGreenColor (Pixel pixel)  const
{
  int index = getGreenIndex(pixel);
  return _green_color[index];
}


int Paintset::getBlueColor  (Pixel pixel)  const
{
  int index = getBlueIndex(pixel);
  return _blue_color[index];
}

                        //////////////////


int Paintset::getRedPercent (Pixel pixel)  const
{
  return NearestInteger(FACTOR * getRedColor(pixel));
}


int Paintset::getGreenPercent (Pixel pixel)  const
{
  return NearestInteger(FACTOR * getGreenColor(pixel));
}


int Paintset::getBluePercent (Pixel pixel)  const
{
  return NearestInteger(FACTOR * getBlueColor(pixel));
}

                        //////////////////


int Paintset::getRedIndex   (Pixel pixel)  const
{
  return get_index (pixel, _red_mask, _red_shift, _colormap_size);
}


int Paintset::getGreenIndex (Pixel pixel)  const
{
  return get_index (pixel, _green_mask, _green_shift, _colormap_size);
}


int Paintset::getBlueIndex  (Pixel pixel)  const
{
  return get_index (pixel, _blue_mask, _blue_shift, _colormap_size);
}

                        //////////////////


int Paintset::redColor (int index)  const
{
  assert(index >= 0 && index < _colormap_size);
  return _red_color[index];
}


int Paintset::greenColor (int index)  const
{
  assert(index >= 0 && index < _colormap_size);
  return _green_color[index];
}


int Paintset::blueColor (int index)  const
{
  assert(index >= 0 && index < _colormap_size);
  return _blue_color[index];
}

                        //////////////////


int Paintset::redPercent (int index)  const
{
  return NearestInteger(FACTOR * redColor(index));
}


int Paintset::greenPercent (int index)  const
{
  return NearestInteger(FACTOR * greenColor(index));
}


int Paintset::bluePercent (int index)  const
{
  return NearestInteger(FACTOR * blueColor(index));
}

                        //////////////////


int Paintset::redIsAllocated (int index)  const
{
  assert(index >= 0 && index < _colormap_size);
  return _allocated_red[index];
}


int Paintset::greenIsAllocated (int index)  const
{
  assert(index >= 0 && index < _colormap_size);
  return _allocated_green[index];
}


int Paintset::blueIsAllocated (int index)  const
{
  assert(index >= 0 && index < _colormap_size);
  return _allocated_blue[index];
}


//--------------------------- set values ----------------------------------//
//--------------------------- set values ----------------------------------//
//--------------------------- set values ----------------------------------//


void Paintset::setColors (int ncolors, Pixel pixels[],
                                       const int red  [],
                                       const int green[],
                                       const int blue [])
{
  if(ncolors <= 0) return;
  assert(ncolors <= _colormap_size);
  assert(pixels);
  for(int icolor = 0; icolor < ncolors; icolor++)
      {
      int   red_color = 0;
      int green_color = 0;
      int  blue_color = 0;

      if(  red)   red_color =   red[icolor];
      if(green) green_color = green[icolor];
      if( blue)  blue_color =  blue[icolor];

      if(!red && !green && !blue)
          {
          int gray;
          if(ncolors == 1) gray = BRIGHTEST / 2;
          else gray = (icolor * BRIGHTEST) / (ncolors - 1);
            red_color = gray;
          green_color = gray;
           blue_color = gray;
          }

      setColor(&pixels[icolor], red_color, green_color, blue_color);
      }
}


void Paintset::setColor (Pixel *pixel, int red, int green, int blue)
{
  int   red_index = getRedIndex  (*pixel);
  int green_index = getGreenIndex(*pixel);
  int  blue_index = getBlueIndex (*pixel);

/*
  assert(_allocated_red  [  red_index] > 0);
  assert(_allocated_green[green_index] > 0);
  assert(_allocated_blue [ blue_index] > 0);
*/

  XColor color;
  color.red   = ConstrainValue(red  , 0, BRIGHTEST);
  color.green = ConstrainValue(green, 0, BRIGHTEST);
  color.blue  = ConstrainValue(blue , 0, BRIGHTEST);
  color.flags = DoRed | DoGreen | DoBlue;
  if(_read_only)
      {
      Status status = XAllocColor(_display, _colormap, &color);
      if(!status) return;
      //_stored_colors++;

      _allocated_red  [  red_index] -= 1;
      _allocated_green[green_index] -= 1;
      _allocated_blue [ blue_index] -= 1;

      *pixel = color.pixel;

        red_index = getRedIndex  (*pixel);
      green_index = getGreenIndex(*pixel);
       blue_index = getBlueIndex (*pixel);

      _allocated_red  [  red_index] += 1;
      _allocated_green[green_index] += 1;
      _allocated_blue [ blue_index] += 1;
      }
  else
      {
      color.pixel = *pixel;
      XStoreColor(_display, _colormap, &color);
      }
  privateRememberColor(*pixel);
}


void Paintset::setXColor (Pixel *pixel, XColor *color)
{
  setColor (pixel, color->red, color->green, color->blue);
}

                        //////////////////


void Paintset::setRedColor   (Pixel *pixel, int   red)
{
  int green = getGreenColor(*pixel);
  int blue  = getBlueColor (*pixel);
  setColor(pixel, red, green, blue);
}

void Paintset::setGreenColor (Pixel *pixel, int green)
{
  int red   = getRedColor  (*pixel);
  int blue  = getBlueColor (*pixel);
  setColor(pixel, red, green, blue);
}

void Paintset::setBlueColor  (Pixel *pixel, int  blue)
{
  int red   = getRedColor  (*pixel);
  int green = getGreenColor(*pixel);
  setColor(pixel, red, green, blue);
}

Pixel Paintset::setClearPixelFromName (const char *colorname)
{
  if (!getPixelFromName(colorname,&_clear)) {
    _clear = _black;
  }
  return _clear;
}

int Paintset::getPixelFromName (const char *colorname, Pixel *pixel)
{
  int retval;

  XColor closest, exact;

  if (!_read_only) {
    int found_it = findPixelFromName (colorname, pixel);
    if (!found_it) {
      retval = XAllocNamedColor (_display, _colormap, colorname,
        &closest, &exact);
      if (retval) {
	_stored_colors++;
	*pixel = closest.pixel;
	storePixelFromName (colorname, *pixel);
      }
      else {
	if (_num_named_colors > 0) {
	  *pixel = _named_pixels[_num_named_colors-1]; // use the last one
	}
	else {
	  printf ("could not allocate color %s for display %x,",
	    colorname, _display);
	  printf ("Paintset %x --- using black\n", this);
	  *pixel = _black; // return foreground color instead
	}
	retval = FALSE;
      }
    }
    else {
      retval = TRUE;
    }
  }
  else {
    retval = XAllocNamedColor (_display, _colormap, colorname,
      &closest, &exact);
    if (retval) {
      *pixel = closest.pixel;
    }
    else {
      assert (0);
    }
  }
  return retval;
}

void Paintset::freePixelFromName (const char *colorname, Pixel plane)
{
  if (!_read_only) {
    Pixel dummy;
    int index = findPixelFromName (colorname, &dummy) - 1;
    if (index > -1) {
      Pixel pixel = _named_pixels[index];
      int unique = uniquePixelFromName (pixel);
      // remove it from the array
      removePixelFromName (index);
      if (unique) {
	// free the color pixel from X
	XFreeColors (_display, _colormap, &pixel, 1, plane);
	_stored_colors--;
      }
    }
  }
}

int Paintset::numAllocatedPlanes ()
{
  return _nplanes;
}

void Paintset::setNumAllocatedPlanes (int nplanes)
{
  _nplanes = nplanes;
  assert (_nplanes < 2);
}

void Paintset::setColorsetNamingPolicy (int p)
{
  if (_read_only) return;

  switch (p) {
  case NAME_AS_IS         :
  case NAMES_BY_PAINTSETS :
  case NAMES_BY_PLOTS     :
    _colorset_naming_policy = p;
    break;
  default:
    assert (0);
  }
}

int Paintset::colorsetNamingPolicy ()
{
  return _colorset_naming_policy;
}

int Paintset::findPixelFromName (const char *colorname, Pixel *pixel)
{
  int retval; // index + 1 of found colorname otherwise 0

  int k2;
  for (k2 = 0, retval = 0; k2 < _num_named_colors; k2++) {
    if (!strcmp(_named_colors[k2],colorname)) {
      if (pixel) *pixel = _named_pixels[k2];
      retval = k2 + 1;
      k2 = _num_named_colors;
    }
  }
  return retval;
}

#define INCR 10

void Paintset::storePixelFromName (const char *colorname, Pixel pixel)
{
  if (!_read_only) {
    if (_num_named_colors == _alloc_named_colors) {
      _alloc_named_colors = _alloc_named_colors + INCR;
      char **named_colors = (char **)malloc (_alloc_named_colors
        *sizeof(char *));
      Pixel *named_pixels = (Pixel *)malloc (_alloc_named_colors
        *sizeof(Pixel));
      int k2;
      for (k2 = 0; k2 < _num_named_colors; k2++) {
	named_colors[k2] = _named_colors[k2];
	named_pixels[k2] = _named_pixels[k2];
      }
      free (_named_colors);
      _named_colors = named_colors;
      free (_named_pixels);
      _named_pixels = named_pixels;
    }
    _named_colors[_num_named_colors] = (char *)malloc ((strlen(colorname)+1)
      *sizeof(char));
    strcpy (_named_colors[_num_named_colors], colorname);
    _named_pixels[_num_named_colors] = pixel;
    _num_named_colors++;
  }
}

int Paintset::uniquePixelFromName (Pixel pixel)
{
  int count;

  int k2;
  for (k2 = 0, count = 0; k2 < _num_named_colors; k2++) {
    if (pixel == _named_pixels[k2]) count++;
  }
  return count == 1;
}

void Paintset::removePixelFromName (int index)
{
  assert (index > -1 && index < _num_named_colors);

  int k2;
  for (k2 = index+1; k2 < _num_named_colors; k2++) {
    _named_colors[k2] = _named_colors[k2-1];
    _named_pixels[k2] = _named_pixels[k2-1];
  }
  _num_named_colors--;
}

//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//

