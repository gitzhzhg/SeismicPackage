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

//--------------------------- colorset.hh --------------------------------//
//--------------------------- colorset.hh --------------------------------//
//--------------------------- colorset.hh --------------------------------//

//                header file for the Colorset class
//                    not derived from any class
//                         subdirectory sl

                     // this is a colorbar class.


//---------------------------- start -------------------------------------//
//---------------------------- start -------------------------------------//
//---------------------------- start -------------------------------------//

#ifndef _COLORSET_HH_
#define _COLORSET_HH_

#include "named_constants.h"

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>


class Colorset
{

//---------------------------- data --------------------------------------//
//---------------------------- data --------------------------------------//
//---------------------------- data --------------------------------------//

public:

  enum { BRIGHTEST = 65535 };

private:

  class Paintset *_paintset;        // colormap to use.
  Display        *_display;         // from _paintset (convenience).
  int             _ncolors;         // number of colors in colorbar.
  Pixel          *_pixels;          // list of allocated pixels.
  Pixel           _plane;           // plane mask if allocating planes (or 0).
  Pixel           _black;           // black with plane mask.
  Pixel           _white;           // white with plane mask.
  float           _first_value;     // value for start of colorbar.
  float           _last_value;      // value for end of colorbar.
  int             _changed;         // true if colorbar has changed.
  int             _transferred_in;  // false if pixels were allocated

  int             _old_ncolors;
  unsigned short *_old_reds;
  unsigned short *_old_greens;
  unsigned short *_old_blues;

  GC              _gc_overlay;
  GC              _gc_underlay;
  GC              _gc_white;
  GC              _gc_bitmap;

  class SpecificPixmap  **_pixmaps;      // array of SpecificPixmap objects
  int                     _alloc_pixmaps;// size of _pixmaps array
  int                     _num_pixmaps;  // # of SpecificPixmap objects

  class PixmapCombo     **_combos;       // array of PixmapCombo objects
  int                     _alloc_combos; // size of _combos array
  int                     _num_combos;   // # of PixmapCombo objects

  class Colorset *_shared;
  int _shared_ncolors;
  int _shared_inited;

  // _changed is set to TRUE if ncolors or first/last values change.
  // _changed is also set to TRUE (if read-only) if colors themselves change.

//-------------------- constructor and destructor ----------------------//
//-------------------- constructor and destructor ----------------------//
//-------------------- constructor and destructor ----------------------//

public:

  Colorset (class Paintset *paintset);

  virtual ~Colorset();

  void printColorbar()  const;

public: // determine whether pixmaps or windows need to be redrawn.
        // they must be redrawn if ncolors or first/last values change.
        // they also must be redrawn (if read-only) if colors themselves change.

  int  colorbarHasChanged();

private:

  void privatePaintDrawable (Drawable drawable,
                             Pixel data[],
                             int xsize, int ysize,
                             int xlo,   int ylo,
                             int width, int height,
                             int xlo2,  int ylo2);

//------------------------ allocate and free colorbar -----------------------//
//------------------------ allocate and free colorbar -----------------------//
//------------------------ allocate and free colorbar -----------------------//

public: // allocateColorbar allocates a set of color cells in the colormap.
        // allocateColorbar frees and reallocates colorbar if already exists.
        // allocateColorbar sets pixels to the newly allocated cells.
        // allocateColorbar presets colors to shades of gray.
        // allocateColorbar returns error TRUE or FALSE.
        // PIXMAPS OR WINDOWS MUST BE REDRAWN IF NCOLORS CHANGES.
        // PIXMAPS OR WINDOWS MUST BE REDRAWN (IF READ-ONLY) IF COLORS CHANGE.
        // these do not change _first_value or _last_value.

  int  allocateColorbar (int ncolors, int allocate_plane = FALSE);
  void freeColorbar     ();
  void transferPixels   (int ncolors, Pixel *pixels,
			 int allocate_plane = FALSE);

  void shareWith        (class Colorset *shared = NULL);

private:
  int  allocateSharedColorbar (Pixel *plane);
  void freeSharedColorbar     ();

//----------------------------- get values --------------------------------//
//----------------------------- get values --------------------------------//
//----------------------------- get values --------------------------------//

public:  // icolor must be between 0 and _ncolors-1.
         // icolor can also be _ncolors for getValue.
         // red and green and blue are between 0 and BRIGHTEST.

  Paintset  *paintset           ()             const  { return _paintset; }
  int        numColors          ()             const  { return _ncolors; }
  float      getFirstValue      ()             const  { return _first_value; } 
  float      getLastValue       ()             const  { return _last_value ; } 
  Pixel      getPlane           ()             const  { return _plane ; } 
  float      getValue           (int icolor)   const;
  int        getRedColor        (int icolor)   const;
  int        getGreenColor      (int icolor)   const;
  int        getBlueColor       (int icolor)   const;
  Pixel      getPixel           (int icolor)   const;
  Pixel      getPixel           (float value)  const;
  int        getColor           (float value)  const; // -3,-2,-1 if no match
  int        getColor           (Pixel pixel)  const; // -1 if no match
  int        readOnly           ()             const; // true if static colors.
  int        numAvailableColors ()             const;

  void    getColor   (int icolor, int *red, int *green, int *blue)  const;

  void    getColors  (int red  [] = NULL,     // must be _ncolor colors.
                      int green[] = NULL,
                      int blue [] = NULL)  const;

//----------------------------- set values --------------------------------//
//----------------------------- set values --------------------------------//
//----------------------------- set values --------------------------------//

public:  // icolor must be between 0 and _ncolors-1.
         // red and green and blue are between 0 and BRIGHTEST.

  void  setFirstValue (float value) { _first_value = value; _changed = TRUE; }
  void  setLastValue  (float value) { _last_value  = value; _changed = TRUE; }

  void  setRedColor   (int icolor, int red);
  void  setGreenColor (int icolor, int green);
  void  setBlueColor  (int icolor, int blue);

  void  setColor  (int icolor, int  red, int  green, int  blue);

  void  setColors (const int red  [] = NULL,  // must be _ncolor colors.
                   const int green[] = NULL,
                   const int blue [] = NULL);

  void  setSharedColors (const int red  [] = NULL,  // must be _ncolor colors.
                         const int green[] = NULL,
                         const int blue [] = NULL);

  void  freeOldColorbar ();
  void  reallocateOldColorbar ();
  void  rememberColors ();
  void  rememberColor (int icolor);
  int   colorsChanged ();
  int   colorChanged (int icolor);
  Pixel matchPixelByColor (int *index, int red, int green, int blue);

//-------------------------- painting routines ----------------------------//
//-------------------------- painting routines ----------------------------//
//-------------------------- painting routines ----------------------------//

public: // put SOURCE or INDICES or BLACKWHITE or PIXELS to a drawable.
        // colorbar is used to convert SOURCE or INDICES to pixels.
        // SOURCE must contain floating point values (e.g. trace amplitudes).
        // there must be one value in SOURCE or INDICES or BLACKWHITE or
        //  PIXELS for each pixel location.
        // BB MUST BE CALLED AGAIN IF NCOLORS OR FIRST/LAST VALUES CHANGE.
        // BB MUST BE CALLED AGAIN (IF READ-ONLY) IF COLORS CHANGE.
        // pixmaps and windows must be same depth as root window.
        // BITMAPS must of course have depth of one.
        // width  < 0 assumes xlo and xlo2 are at the upper end of width.
        // height < 0 assumes ylo and ylo2 are at the upper end of height.
        // width  == 0 is replaced by xsize (or drawable width)  - xlo.
        // height == 0 is replaced by ysize (or drawable height) - ylo.

  void paintRectangle
             (Drawable drawable,              // pixmap or window or BITMAP.
              GC gc,
              int xlo   = 0, int ylo    = 0,  // in destination drawable.
              int width = 0, int height = 0); // in destination drawable.

  void paintBits
             (Drawable drawable,              // pixmap or window or BITMAP.
              const char bits[],              // bits 1 (black) or 0 (white).
   /* AA */   int xsize    , int ysize     ,  // number of bits in bits array.
              int xlo   = 0, int ylo    = 0,  // in bits array.
              int width = 0, int height = 0,  // in bits and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.

  void paintDrawable
             (Drawable drawable,              // pixmap or window or BITMAP.
              const char blackwhite[],        // TRUE (black) or FALSE (white).
   /* AA */   int xsize    , int ysize     ,  // size of blackwhite array.
              int xlo   = 0, int ylo    = 0,  // in blackwhite array.
              int width = 0, int height = 0,  // in blackwhite and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.

  void paintDrawable
             (Drawable drawable,              // pixmap or window to paint.
              const float source[],           // values in source data.
   /* BB */   int xsize    , int ysize     ,  // size of source array.
              int xlo   = 0, int ylo    = 0,  // in source array.
              int width = 0, int height = 0,  // in source and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.

  void paintDrawable
             (Drawable drawable,              // pixmap or window to paint.
              const int indices[],            // indices into the colorbar.
   /* BB */   int xsize    , int ysize     ,  // size of indices array.
              int xlo   = 0, int ylo    = 0,  // in indices array.
              int width = 0, int height = 0,  // in indices and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.

  void paintDrawable
             (Drawable drawable,              // pixmap or window to paint.
              const unsigned int pixels[],    // XImage pixels are UINT's
   /* CC */   int xsize    , int ysize     ,  // size of pixels array.
              int xlo   = 0, int ylo    = 0,  // in pixels array.
              int width = 0, int height = 0,  // in pixels and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.

  void copyDrawable
             (Drawable drawable,              // pixmap or window or BITMAP.
              Drawable source,                // pixmap or window or BITMAP.
              int xlo   = 0, int ylo    = 0,  // in source drawable.
              int width = 0, int height = 0,  // in source and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.

  void clearDrawable
             (Drawable drawable,              // pixmap or window or BITMAP.
              int xlo   = 0, int ylo    = 0,  // in destination drawable.
              int width = 0, int height = 0); // in destination drawable.

//------------------------- overlays and underlays ------------------------//
//------------------------- overlays and underlays ------------------------//
//------------------------- overlays and underlays ------------------------//

public: // width  < 0 assumes xlo and xlo2 are at the upper end of width.
        // height < 0 assumes ylo and ylo2 are at the upper end of height.
        // width  == 0 replaced by overlay/underlay (or drawable) width  - xlo.
        // height == 0 replaced by overlay/underlay (or drawable) height - ylo.

  void putOverlay
             (Drawable drawable,              // pixmap or window to paint.
              Pixmap overlay,                 // BITMAP to copy into drawable.
              int xlo   = 0, int ylo    = 0,  // in overlay.
              int width = 0, int height = 0,  // in overlay and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.
                                              // (overlay painted by AA)
                                              // (underlay remains in place)

  void putUnderlay
             (Drawable drawable,              // pixmap or window to paint.
              Pixmap underlay,                // pixmap to copy into drawable.
              int xlo   = 0, int ylo    = 0,  // in underlay.
              int width = 0, int height = 0,  // in underlay and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.
                                              // (underlay painted by BB or CC)
                                              // (overlay remains in place)

  void clearOverlay
             (Drawable drawable,              // pixmap or window to clear.
              int xlo   = 0, int ylo    = 0,  // in destination drawable.
              int width = 0, int height = 0); // in destination drawable.
                                              // (underlay remains in place)

  void clearUnderlay
             (Drawable drawable,              // pixmap or window to clear.
              int xlo   = 0, int ylo    = 0,  // in destination drawable.
              int width = 0, int height = 0); // in destination drawable.
                                              // (overlay remains in place)


  void storeXColor
             (int icolor,                     // color index to set
	      XColor color);                  // color containing rgb to set

  void storeXColor
             (int icolor);                    // color index to set

  XColor queryXColor
             (int icolor) const;              // color index to get

  XColor getXColor
             (int icolor) const;              // color index to get

  int synchronize
             (class Colorset *from);          // Colorset from which to sync

  int planeAllocated ();

  Pixel *getPixels                       // returns the pixels or NULL
            (Pixmap source,              // pixmap or BITMAP to get.
             int width, int height,      // size of subimage to get
	     int xlo = 0, int ylo = 0);  // in source Pixmap

  Pixel *padPixels                       // returns padded pixels or NULL
            (int xsize, int ysize,       // size of padded pixel array
	     Pixel *pixels,              // array of given pixel array
             int width, int height,      // size of pixel array
             int xlo = 0, int ylo = 0);  // in padded pixel array

  void padPixmap                          // clears overlay beyond underlay
             (class PixmapSet * overlay,  //   PixmapSet containing overlay
              class PixmapSet *underlay); //   PixmapSet containing overlay


  void paintRectangle
             (class PixmapSet *pixmap_set,    // pixmap set
              GC gc,
              int xlo   = 0, int ylo    = 0,  // in destination drawable.
              int width = 0, int height = 0); // in destination drawable.

  void paintBits
             (class PixmapSet *pixmap_set,    // pixmap set
              const char bits[],              // bits 1 (black) or 0 (white).
   /* AA */   int xsize    , int ysize     ,  // number of bits in bits array.
              int xlo   = 0, int ylo    = 0,  // in bits array.
              int width = 0, int height = 0,  // in bits and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.

  void paintDrawable
             (class PixmapSet *pixmap_set,    // pixmap set
              const char blackwhite[],        // TRUE (black) or FALSE (white).
   /* AA */   int xsize    , int ysize     ,  // size of blackwhite array.
              int xlo   = 0, int ylo    = 0,  // in blackwhite array.
              int width = 0, int height = 0,  // in blackwhite and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.

  void paintDrawable
             (class PixmapSet *pixmap_set,    // pixmap set
              const float source[],           // values in source data.
   /* BB */   int xsize    , int ysize     ,  // size of source array.
              int xlo   = 0, int ylo    = 0,  // in source array.
              int width = 0, int height = 0,  // in source and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.

  void paintDrawable
             (class PixmapSet *pixmap_set,    // pixmap set
              const int indices[],            // indices into the colorbar.
   /* BB */   int xsize    , int ysize     ,  // size of indices array.
              int xlo   = 0, int ylo    = 0,  // in indices array.
              int width = 0, int height = 0,  // in indices and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.

  void paintDrawable
             (class PixmapSet *pixmap_set,    // pixmap set
              const unsigned int pixels[],    // XImage pixels are UINT's
   /* CC */   int xsize    , int ysize     ,  // size of pixels array.
              int xlo   = 0, int ylo    = 0,  // in pixels array.
              int width = 0, int height = 0,  // in pixels and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.

  void copyDrawable
             (Drawable drawable,              // pixmap or window or BITMAP.
              class PixmapSet *pixmap_set,    // pixmap set
              int xlo   = 0, int ylo    = 0,  // in source drawable.
              int width = 0, int height = 0,  // in source and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.

  void clearDrawable
             (class PixmapSet *pixmap_set,    // pixmap set
              int xlo   = 0, int ylo    = 0,  // in destination drawable.
              int width = 0, int height = 0); // in destination drawable.

  void putOverlay
             (Drawable drawable,              // pixmap or window to paint.
              class PixmapSet *pixmap_set,    // pixmap set
              int xlo   = 0, int ylo    = 0,  // in overlay.
              int width = 0, int height = 0,  // in overlay and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.
                                              // (overlay painted by AA)
                                              // (underlay remains in place)

  void putUnderlay
             (Drawable drawable,              // pixmap or window to paint.
              class PixmapSet *pixmap_set,    // pixmap set
              int xlo   = 0, int ylo    = 0,  // in underlay.
              int width = 0, int height = 0,  // in underlay and destination.
              int xlo2  = 0, int ylo2   = 0); // in destination drawable.
                                              // (underlay painted by BB or CC)
                                              // (overlay remains in place)

  void clearOverlay
             (class PixmapSet *pixmap_set,    // pixmap set
              int xlo   = 0, int ylo    = 0,  // in destination drawable.
              int width = 0, int height = 0); // in destination drawable.
                                              // (underlay remains in place)

  void clearUnderlay
             (class PixmapSet *pixmap_set,    // pixmap set
              int xlo   = 0, int ylo    = 0,  // in destination drawable.
              int width = 0, int height = 0); // in destination drawable.
                                              // (overlay remains in place)

  void putOverlayOnPixels
                 (Drawable drawable,       // pixmap or window to paint.
                  Pixmap overlay,          // BITMAP to get and put on pixels.
                  Pixel *data,             // pixels (freed by this function).
                  int xlo,   int ylo,      // in overlay.
                  int width, int height,   // size of data, overlay, and dest.
                  int xlo2,  int ylo2);    // in destination drawable.

  void copyOverlayOnUnderlay
                 (Drawable drawable,        // pixmap or window to paint.
                  class PixmapSet *overlay, // overlay PixmapSet
                  class PixmapSet *underlay,// underlay PixmapSet
                  int xlo,   int ylo,       // in overlay.
                  int width, int height,    // size of data, overlay, and dest
                  int xlo2,  int ylo2);     // in destination drawable.

  Pixmap pixmap
             (class PixmapSet *pixmap_set,  // given PixmapSet
              int *index = NULL);           // which pixmap returned (opt)

  Pixmap existingPixmap
             (class PixmapSet *pixmap_set,  // given PixmapSet
              int *index = NULL);           // which pixmap returned (opt)

  void removePixmap
             (class PixmapSet *pixmap_set); // given PixmapSet

  int adjustHeight                          // limit height to data area
                 (int index,                //   index of PixmapCombo
                  int ylo,                  //   in destination drawable
                  int height);              //   height of data in dest

  Pixmap combinePixmaps
                 (class PixmapSet *overlay,  // overlay PixmapSet
                  class PixmapSet *underlay, // underlay PixmapSet
                  int *index = NULL);        // index of PixmapCombo

  void removePixmapCombo
                 (class PixmapSet *overlay,  // overlay PixmapSet
                  class PixmapSet *underlay);// underlay PixmapSet

  void clearCombosWith
                 (Pixmap pixmap);            // given Pixmap

  void clearRectangle
             (class PixmapSet *pixmap_set,   // given PixmapSet
	      int xlo,   int ylo,            // in pixmap.
              int width, int height);        // size of pixmap, data  and dest

  int comboExists
                 (class PixmapSet *overlay,  // overlay PixmapSet
                  class PixmapSet *underlay);// underlay PixmapSet

//------------------------------ end functions ---------------------------//
//------------------------------ end functions ---------------------------//
//------------------------------ end functions ---------------------------//

};

class SpecificPixmap
{
public:
  SpecificPixmap
    (class PixmapSet *pixmap_set);          // overlay PixmapSet

  ~SpecificPixmap ();

  int matches
    (class PixmapSet *pixmap_set);          // given PixmapSet

  Pixmap get ();

  void markChanged ();

private:
  class PixmapSet
    *_pixmap_set;
};

class PixmapCombo
{
public:
  PixmapCombo
    (Display *display,                      // XtDisplay
     class PixmapSet *overlay,              // overlay PixmapSet
     class PixmapSet *underlay);            // underlay PixmapSet

  ~PixmapCombo ();

  int matches
    (class PixmapSet *overlay,              //  overlay PixmapSet
     class PixmapSet *underlay);            // underlay PixmapSet

  int matches
    (Pixmap pixmap);                        // given Pixmap

  int exists ();

  void clear ();

  Pixmap *get ();

  int adjustHeight                          // limit height to data area
    (int ylo,                               //   in destination drawable
     int height);                           //   height of data in dest

private:
  Display
    *_display;

  class PixmapSet
    *_overlay,
    *_underlay;

  Pixmap
    _combo;

  int
    _oindex,
    _uindex;
};

#endif

//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//
