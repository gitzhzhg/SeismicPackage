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

//---------------------- paintset_collection.hh ---------------------------//
//---------------------- paintset_collection.hh ---------------------------//
//---------------------- paintset_collection.hh ---------------------------//

//           header file for the PaintsetCollection class
//                    not derived from any class
//                         subdirectory sl


// This class maintains one instance of the Paintset class for each
// display and screen.  The correct instance for a given screen is
// created (if necessary) and returned by the static function fetch.
// This class consists only of static functions and need not be instantiated.


//---------------------------- start -------------------------------------//
//---------------------------- start -------------------------------------//
//---------------------------- start -------------------------------------//

#ifndef _PAINTSET_COLLECTION_HH_
#define _PAINTSET_COLLECTION_HH_

#include "named_constants.h"
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>


class PaintsetCollection
{

//---------------------------- data --------------------------------------//
//---------------------------- data --------------------------------------//
//---------------------------- data --------------------------------------//

private:
  static int
    _colorset_naming_policy;

//------------------------ static functions ------------------------------//
//------------------------ static functions ------------------------------//
//------------------------ static functions ------------------------------//

public:  // these return the paintset for the specified display and screen.
         // these create a paintset if the requested paintset does not exist.
         // these use vprivate only when creating a paintset.

  static class Paintset *fetch
    (Display *display,
     int iscreen,
     int vprivate = FALSE,
     int vdynamic = FALSE);

  static class Paintset *fetch
    (Screen *screen,
     int vprivate = FALSE,
     int vdynamic = FALSE);

  static class Paintset *fetch
    (Widget widget,
     int vprivate = FALSE,
     int vdynamic = FALSE);


public:  // these return the paintset accomodating a required number of colors
         //   for each plane, the required number of colors doubles

  static class Paintset *fetchByNumColors
    (Display *display,
     int iscreen,
     int ncolors,
     int nplanes = 0);

  static class Paintset *fetchByNumColors
    (Screen *screen,
     int ncolors,
     int nplanes = 0);

  static class Paintset *fetchByNumColors
    (Widget widget,
     int ncolors,
     int nplanes = 0);

  static class Paintset *releaseColors
    (Colormap colormap,
     int ncolors,
     int nplanes = 0);

public:  // these are similar to the first three fetch functions.
         // these assert if the requested paintset does not exist.

  static class Paintset *fetchExisting
    (Display *display,
     int iscreen);

  static class Paintset *fetchExisting
    (Screen *screen);

  static class Paintset *fetchExisting
    (Widget widget);

public:  // this is a convenience if the colormap is already known.
         // this asserts if colormap is 0.
         // this asserts if there is no Paintset instance with this colormap.

  static class Paintset *fetchExisting
    (Colormap colormap);

  static class Paintset *fetchExistingByColormap
    (Widget widget);

public:  // various convenience functions which pass through to Paintset.
         // these functions assert if the requested paintset does not exist.

  static Pixel black
    (Display *display,
     int iscreen);

  static Pixel black
    (Screen  *screen);

  static Pixel black
    (Widget widget);

  static Pixel white
    (Display *display,
     int iscreen);

  static Pixel white
    (Screen *screen);

  static Pixel white
    (Widget widget);

  static Pixel clear
    (Display *display,
     int iscreen);

  static Pixel clear
    (Screen *screen);

  static Pixel clear
    (Widget widget);

  static void addResources
    (Arg *arglist,
     int *n,
     Display *display,
     int iscreen);

  static void addResources
    (Arg *arglist,
     int *n,
     Screen *screen);

  static void addResources
    (Arg *arglist,
     int *n,
     Widget widget);

  static int visualDepth 
    (Display *display,
     int iscreen);

  static int visualDepth
    (Screen *screen);

  static int visualDepth
    (Widget widget);

  static Visual *visual
    (Display *display,
     int iscreen);

  static Visual *visual
    (Screen *screen);

  static Visual *visual
    (Widget widget);

  static Colormap colormap
    (Display *display,
     int iscreen);

  static Colormap colormap
    (Screen *screen);

  static Colormap colormap
  (Widget widget);

  static int numColorsLeft
    (class Paintset *paintset,
     int nplanes = 0);

  static void setColorsetNamingPolicy
    (int p);

  static int colorsetNamingPolicy ();


public:  // printing routines.

  static void printSummary ();

  static void printAllColors ();

//------------------------------ end functions ---------------------------//
//------------------------------ end functions ---------------------------//
//------------------------------ end functions ---------------------------//

};

#endif

//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//
