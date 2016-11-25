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

//--------------------------- paintset.hh --------------------------------//
//--------------------------- paintset.hh --------------------------------//
//--------------------------- paintset.hh --------------------------------//

//                 header file for the Paintset class
//                    not derived from any class
//                         subdirectory sl

                     // this is a colormap class.


  // The visual depth will be the default visual depth of the screen.
  // The visual class will match default visual class of screen if possible.

  // If vprivate is true, the colormap will be private.
  // If vprivate is false, colormap will be default if possible, or private.
  // If vdynamic is true, visual class will have dynamic (read-write) colors.
  // If vdynamic is false, visual class will be default visual class of screen.


  // The following function must be called just before calling
  // XtAppCreateShell and (on the sun) also when creating all other
  // shells (dialog boxes, pulldown menus, and popup menus).  This
  // function adds resources XmNcolormap, XmNvisual, and XmNdepth to args.
  //
  //                     addResources(args, &i);


  // red         = brightness of the red   color (0 thru 65535).
  // green       = brightness of the green color (0 thru 65535).
  // blue        = brightness of the blue  color (0 thru 65535).
  //
  // red_index   = index of the red   color (0 thru _colormap_size - 1).
  // green_index = index of the green color (0 thru _colormap_size - 1).
  // blue_index  = index of the blue  color (0 thru _colormap_size - 1).


  // pixel = identifier of a trio of colors in the colormap.


  // for visuals with one colormap index (normally 8-bit):
  //  (1) there is one colormap index which references a trio of colors.
  //  (2) red_index = green_index = blue_index = pixel.
  //
  // for visuals with three colormap indices (normally 24-bit):
  //  (1) there are three colormap indices which reference a trio of colors.
  //  (2) pixel contains 3 indices (which may be different) packed together.


  // visual classes are the following:
  //  (1) PseudoColor  (read-write)  (1 colormap index)    (normally 8-bit)
  //  (2) StaticColor  (read-only)   (1 colormap index)    (normally 8-bit)
  //  (3) GrayScale    (read-write)  (1 colormap index)    (normally 8-bit)
  //  (4) StaticGray   (read-only)   (1 colormap index)    (normally 8-bit)
  //  (5) DirectColor  (read-write)  (3 colormap indices)  (normally 24-bit)
  //  (6) TrueColor    (read-only)   (3 colormap indices)  (normally 24-bit)


//---------------------------- start -------------------------------------//
//---------------------------- start -------------------------------------//
//---------------------------- start -------------------------------------//

#ifndef _PAINTSET_HH_
#define _PAINTSET_HH_

#include "named_constants.h"
#include "wproc.h"
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>


class Paintset
{

//---------------------------- data --------------------------------------//
//---------------------------- data --------------------------------------//
//---------------------------- data --------------------------------------//

public:

  enum { BRIGHTEST = 65535 };  // maximum intensity for red or green or blue.

  enum { NCOPY = 20 };  // number of colors allowed for graphics

  enum {                /* policy on naming Colorset's       */
    NAME_AS_IS,         /* don't alter given Colorset names  */
    NAMES_BY_PAINTSETS, /* name Colorset's by Paintset/plane */
    NAMES_BY_PLOTS      /* name Colorset's by given prefixes */
  };

private:

  Display   *_display;
  int        _iscreen;
  Screen    *_screen;
  int        _vprivate;        // true or false (might not match input arg).
  int        _vdepth;          // from _visual.
  int        _vclass;          // from _visual.
  Pixel      _red_mask;        // from _visual.
  Pixel      _green_mask;      // from _visual.
  Pixel      _blue_mask;       // from _visual.
  int        _red_shift;       // from _red_mask.
  int        _green_shift;     // from _green_mask.
  int        _blue_shift;      // from _blue_mask.
  int        _colormap_size;   // from _visual.
  int        _bits_per_rgb;    // from _visual.
  Visual    *_visual;          // default or alternative.
  Colormap   _colormap;        // default or private.
  Pixel      _black;
  Pixel      _white;
  Pixel      _clear;
  int        _read_only;       // true if visual is a static color class.
  int        _nplanes;         // number of planes used in allocation

  int       *_allocated_red;   // >= 0             [_colormap_size].
  int       *_allocated_green; // >= 0             [_colormap_size].
  int       *_allocated_blue;  // >= 0             [_colormap_size].
  int       *_red_color;       // color brightness [_colormap_size].
  int       *_green_color;     // color brightness [_colormap_size].
  int       *_blue_color;      // color brightness [_colormap_size].
  int        _stored_colors;   // number of colors stored
  Pixel     *_named_pixels;
  char      **_named_colors;
  int        _num_named_colors;
  int        _alloc_named_colors;
  int        _colorset_naming_policy; // depends on read_only

//-------------------- constructor and destructor ----------------------//
//-------------------- constructor and destructor ----------------------//
//-------------------- constructor and destructor ----------------------//

public:

  Paintset (Display *display, int iscreen, int vprivate = FALSE,
                                           int vdynamic = FALSE);

  virtual ~Paintset();

private:

  void privateCopyFromOldColormap (Colormap old_colormap,
                                   int *num_copy = NULL);
  void privateGetVisualAttributes ();
  void privateRememberColors      ();
  void privateRememberColor       (Pixel pixel);

//----------------------- misc functions -------------------------------//
//----------------------- misc functions -------------------------------//
//----------------------- misc functions -------------------------------//

public:  // copy to new private colormap since previous colormap is used up.
         // this function changes _vprivate to TRUE, and changes _colormap.
         // this function does nothing if the colormap is already private.

  void remakePrivate();
  void tryPrivateAgain (int num_copy); // try to remakePrivate again

public:  // add to the resource list for creating shell widgets.

  void addResources (Arg *arglist, int *n);

public:  // useful convenience functions.
         // blackwhite[width*height] should contain true/false values.

  Pixmap createPixmap (int width, int height)                           const;
  Pixmap createBitmap (int width, int height)                           const;
  Pixmap createBitmap (int width, int height, const char blackwhite[])  const;

  GC     createGC       (unsigned long valuemask, XGCValues *values,
                         Drawable drawable = 0)  const;
  GC     createGC       (Drawable drawable = 0)  const;
  GC     createBitmapGC ()  const;

  void   freePixmap   (Pixmap pixmap)  const;
  void   freeBitmap   (Pixmap bitmap)  const;     // same as freePixmap.
  void   freeGC       (GC gc)          const;

public:  // compose and decompose pixels.

  void  splitPixel (Pixel pixel,
                    int *red_index, int *green_index, int *blue_index)  const;
  Pixel buildPixel (int  red_index, int  green_index, int  blue_index)  const;

public:  // print information.

  void        printSummary                  ();
  void        printAllColors                ();
  static void printColorsOfDefaultColormaps (Display *display);
  static void printAvailableVisuals         (Display *display);

//---------------------- allocate and free color cells ---------------------//
//---------------------- allocate and free color cells ---------------------//
//---------------------- allocate and free color cells ---------------------//

public: // allocateColorCells allocates a set of colors in the colormap.
        // allocateColorCells sets pixels to the newly allocated cells.
        // allocateColorCells presets colors to shades of gray.
        // allocateColorCells returns error TRUE or FALSE.
        // a color plane is allocated and returned if PLANE is not NULL.

  int  allocateColorCells (int ncolors, Pixel pixels[], Pixel *plane = NULL);

  void freeColorCells     (int ncolors, const Pixel pixels[], Pixel plane = 0);

//------------------------- get values ----------------------------------//
//------------------------- get values ----------------------------------//
//------------------------- get values ----------------------------------//

public:  // red and green and blue are between 0 and BRIGHTEST.

  int         usingDefaultColormap ()  const  { return !_vprivate; }
  int         usingPrivateColormap ()  const  { return  _vprivate; }
  Display    *display              ()  const  { return  _display; }
  int         iscreen              ()  const  { return  _iscreen; }
  Screen     *screen               ()  const  { return  _screen; }
  int         numColors            ()  const  { return  _colormap_size; }
  const char *visualClassName      ()  const;
  int         visualClass          ()  const  { return  _vclass; }
  int         visualDepth          ()  const  { return  _vdepth; }
  Visual     *visual               ()  const  { return  _visual; }
  Colormap    colormap             ()  const  { return  _colormap; }
  Pixel       black                ()  const  { return  _black; }
  Pixel       white                ()  const  { return  _white; }
  Pixel       clear                ()  const  { return  _clear; }
  int         readOnly             ()  const  { return  _read_only; }
  int         numAvailableColors   (int nplanes = 0)  const;
  Window      rootWindow           ()  const;

  Pixel getBackgroundPixelFromName (const char *colorname);
  Pixel getForegroundPixelFromName (const char *colorname);

  Pixel getForegroundPixelFromColors (int red, int green, int blue);
  Pixel getBackgroundPixelFromColors (int red, int green, int blue);

  void getColors (int ncolors, const Pixel pixels[], int red  [] = NULL,
                                                     int green[] = NULL,
                                                     int blue [] = NULL) const;

  void getColors (ColorInfo *col, int **red  ,
		                  int **green,
                                  int **blue ) const;

  void getColor  (Pixel pixel, int *red, int *green, int *blue)  const;
  void getXColor (Pixel pixel, XColor *color)  const;

  int  getRedColor      (Pixel pixel)  const;
  int  getGreenColor    (Pixel pixel)  const;
  int  getBlueColor     (Pixel pixel)  const;

  int  getRedPercent    (Pixel pixel)  const;
  int  getGreenPercent  (Pixel pixel)  const;
  int  getBluePercent   (Pixel pixel)  const;

  int  getRedIndex      (Pixel pixel)  const;   // index into red colormap.
  int  getGreenIndex    (Pixel pixel)  const;   // index into green colormap.
  int  getBlueIndex     (Pixel pixel)  const;   // index into blue colormap.

  int  redColor         (int   red_index)  const;
  int  greenColor       (int green_index)  const;
  int  blueColor        (int  blue_index)  const;

  int  redPercent       (int   red_index)  const;
  int  greenPercent     (int green_index)  const;
  int  bluePercent      (int  blue_index)  const;

  int  redIsAllocated   (int   red_index)  const;
  int  greenIsAllocated (int green_index)  const;
  int  blueIsAllocated  (int  blue_index)  const;

//--------------------------- set values ----------------------------------//
//--------------------------- set values ----------------------------------//
//--------------------------- set values ----------------------------------//

public:  // red and green and blue are between 0 and BRIGHTEST.
         // pixels will change if the colormap is read-only.

  void setColors  (int ncolors, Pixel pixels[], const int red  [] = NULL,
                                                const int green[] = NULL,
                                                const int blue [] = NULL);

  void setColor  (Pixel *pixel, int red, int green, int blue);
  void setXColor (Pixel *pixel, XColor *color);

  void setRedColor   (Pixel *pixel, int   red);
  void setGreenColor (Pixel *pixel, int green);
  void setBlueColor  (Pixel *pixel, int  blue);

  Pixel setClearPixelFromName (const char *colorname);

  int    getPixelFromName (const char *colorname, Pixel *pixel);
  void  freePixelFromName (const char *colorname, Pixel  plane);

  int     numAllocatedPlanes ();
  void setNumAllocatedPlanes (int nplanes);

  void setColorsetNamingPolicy (int p);
  int     colorsetNamingPolicy ();

private:
  int    findPixelFromName (const char *colorname, Pixel *pixel);
  void  storePixelFromName (const char *colorname, Pixel  pixel);
  int  uniquePixelFromName (Pixel pixel);
  void removePixelFromName (int index);

//------------------------------ end functions ---------------------------//
//------------------------------ end functions ---------------------------//
//------------------------------ end functions ---------------------------//

};

#endif

//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//
