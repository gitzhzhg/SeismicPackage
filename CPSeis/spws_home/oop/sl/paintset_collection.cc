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

//----------------------- paintset_collection.cc ----------------------------//
//----------------------- paintset_collection.cc ----------------------------//
//----------------------- paintset_collection.cc ----------------------------//

//          implementation file for the PaintsetCollection class
//                      not derived from any class
//                           subdirectory sl


//---------------------------- start -------------------------------------//
//---------------------------- start -------------------------------------//
//---------------------------- start -------------------------------------//


#include "sl/paintset_collection.hh"
#include "sl/paintset.hh"
#include <Xm/Xm.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>


//------------------------- static data ---------------------------------//
//------------------------- static data ---------------------------------//
//------------------------- static data ---------------------------------//


#define MAXSETS 20

static Paintset *PAINTSETS[MAXSETS];
static int       NUM_COLORS_LEFT[MAXSETS];
static int       NSETS = 0;

int PaintsetCollection::_colorset_naming_policy
  = Paintset::NAMES_BY_PAINTSETS;


//------------------------- printing routines ----------------------------//
//------------------------- printing routines ----------------------------//
//------------------------- printing routines ----------------------------//


void PaintsetCollection::printSummary()
{
  for(int iset = 0; iset < NSETS; iset++)
      {
      Paintset *paintset = PAINTSETS[iset];
      paintset->printSummary();
      }
}


void PaintsetCollection::printAllColors()
{
  for(int iset = 0; iset < NSETS; iset++)
      {
      Paintset *paintset = PAINTSETS[iset];
      paintset->printAllColors();
      }
}


//------------------------------ fetch -----------------------------------//
//------------------------------ fetch -----------------------------------//
//------------------------------ fetch -----------------------------------//


Paintset *PaintsetCollection::fetch (Display *display, int iscreen,
  int vprivate, int vdynamic)
{
  assert(display);
  for(int iset = 0; iset < NSETS; iset++)
      {
      Paintset *paintset = PAINTSETS[iset];
      if(paintset->display() != display) continue;
      if(paintset->iscreen() != iscreen) continue;
      return paintset;
      }
  assert(NSETS < MAXSETS);
  Paintset *paintset = new Paintset(display, iscreen, vprivate, vdynamic);
  PAINTSETS[NSETS] = paintset;
  NUM_COLORS_LEFT[NSETS] = paintset->numAvailableColors ();
  NSETS++;
  return paintset;
}


Paintset *PaintsetCollection::fetch (Screen *screen, int vprivate,
  int vdynamic)
{
  assert(screen);
  Display *display = XDisplayOfScreen(screen);
  int      iscreen = XScreenNumberOfScreen(screen);

  return fetch (display, iscreen, vprivate, vdynamic);
}


Paintset *PaintsetCollection::fetch (Widget widget, int vprivate,
  int vdynamic)
{
  assert(widget);
  Screen *screen = XtScreen(widget);

  return fetch(screen, vprivate, vdynamic);
}


Paintset *PaintsetCollection::fetchByNumColors (Display *display, int iscreen,
  int ncolors, int nplanes)
{
  assert (display);

  int k2, multiplier;
  for (k2 = 0, multiplier = 1; k2 < nplanes; k2++) {
    multiplier *= 2;
  }

  ncolors = nplanes > 0 ? ncolors * multiplier : ncolors;

  for (int iset = 0; iset < NSETS; iset++) {
    Paintset *paintset = PAINTSETS[iset];
    if (paintset->display()   != display) continue;
    if (paintset->iscreen()   != iscreen) continue;
    if (NUM_COLORS_LEFT[iset] <  ncolors) continue;
    NUM_COLORS_LEFT[iset] = NUM_COLORS_LEFT[iset] - ncolors;
    return paintset;
  }
  assert (NSETS < MAXSETS);
  Paintset *paintset = new Paintset (display, iscreen, TRUE/*, TRUE*/);
  int num = paintset->numAvailableColors ();
  int paintset_ok = FALSE;
  if (num >= ncolors) {
    paintset_ok = TRUE;
  }
  else {
    if (ncolors <= paintset->numColors()) {
      // last ditch effort to compromise the number of graphics' colors 
      //   copied in order to accomodate the requested number of colors
      int ncopy;
      for (ncopy = Paintset::NCOPY-1; ncopy > 0; ncopy--) { 
	paintset->tryPrivateAgain (ncopy);
	num = paintset->numAvailableColors ();
	if (num >= ncolors) {
	  ncopy = 0;
	  paintset_ok = TRUE;
	}
      }
    }
  }
  if (paintset_ok) {
    PAINTSETS[NSETS] = paintset;
    NUM_COLORS_LEFT[NSETS] = num - ncolors;
    NSETS++;
  }
  else {
    delete paintset;
    paintset = NULL;
  }
  return paintset;
}


Paintset *PaintsetCollection::fetchByNumColors (Screen *screen, int ncolors,
  int nplanes)
{
  assert (screen);
  Display *display = XDisplayOfScreen (screen);
  int      iscreen = XScreenNumberOfScreen (screen);

  return fetchByNumColors (display, iscreen, ncolors, nplanes);
}


Paintset *PaintsetCollection::fetchByNumColors (Widget widget, int ncolors,
  int nplanes)
{
  // try to use the colormap associated with the widget to minimize flashing
  //   ASSUME the paintset previously existed!!!
  Paintset *paintset = fetchExistingByColormap (widget);

  int k2, multiplier;
  for (k2 = 0, multiplier = 1; k2 < nplanes; k2++) {
    multiplier *= 2;
  }
  int num_colors = nplanes > 0 ? ncolors * multiplier : ncolors;
  for (int iset = 0; iset < NSETS; iset++) {
    if (paintset != PAINTSETS[iset]) continue;
    if (NUM_COLORS_LEFT[iset] <  num_colors) {
      // not enough colors are left!!!!
      iset = NSETS;
      paintset = NULL;
      continue;
    }
    NUM_COLORS_LEFT[iset] = NUM_COLORS_LEFT[iset] - num_colors;
    return paintset;
  }
  assert (NSETS < MAXSETS);
  // Based on the screen find another paintset candidate
  Screen *screen = XtScreen(widget);
  return fetchByNumColors (screen, ncolors, nplanes);
}

Paintset *PaintsetCollection::releaseColors (Colormap colormap, int ncolors,
  int nplanes)
{
  int k2, multiplier;
  for (k2 = 0, multiplier = 1; k2 < nplanes; k2++) {
    multiplier *= 2;
  }
  int num_colors = nplanes > 0 ? ncolors * multiplier : ncolors;

  Paintset *paintset;
  for (int iset = 0; iset < NSETS; iset++) {
    paintset = PAINTSETS[iset];
    if (paintset->colormap() != colormap) continue;
    NUM_COLORS_LEFT[iset] += num_colors;
    assert (NUM_COLORS_LEFT[iset] <= paintset->numColors());
    return paintset;
  }
  return NULL;
}


//---------------------------- fetch existing -----------------------------//
//---------------------------- fetch existing -----------------------------//
//---------------------------- fetch existing -----------------------------//


Paintset *PaintsetCollection::fetchExisting (Display *display, int iscreen)
{
  assert(display);
  for(int iset = 0; iset < NSETS; iset++)
      {
      Paintset *paintset = PAINTSETS[iset];
      if(paintset->display() != display) continue;
      if(paintset->iscreen() != iscreen) continue;
      return paintset;
      }
  assert(FALSE);
  return NULL;
}


Paintset *PaintsetCollection::fetchExisting (Screen *screen)
{
  assert(screen);
  Display *display = XDisplayOfScreen(screen);
  int      iscreen = XScreenNumberOfScreen(screen);

  return fetchExisting(display, iscreen);
}


Paintset *PaintsetCollection::fetchExisting (Widget widget)
{
  assert(widget);
  Screen *screen = XtScreen(widget);

  return fetchExisting(screen);
}

//---------------------------- fetch existing -----------------------------//
//---------------------------- fetch existing -----------------------------//
//---------------------------- fetch existing -----------------------------//


Paintset *PaintsetCollection::fetchExisting (Colormap colormap)
{
  assert(colormap);
  for(int iset = 0; iset < NSETS; iset++)
      {
      Paintset *paintset = PAINTSETS[iset];
      if(paintset->colormap() != colormap) continue;
      return paintset;
      }
  assert(FALSE);
  return NULL;
}


Paintset *PaintsetCollection::fetchExistingByColormap (Widget widget)
{
  Colormap colormap;
  XtVaGetValues (widget, XmNcolormap, &colormap, NULL);
  return fetchExisting (colormap);
}


//------------------------------- black ----------------------------------//
//------------------------------- black ----------------------------------//
//------------------------------- black ----------------------------------//


Pixel PaintsetCollection::black (Display *display, int iscreen)
{
  Paintset *paintset = fetchExisting(display, iscreen);
  return paintset->black();
}


Pixel PaintsetCollection::black (Screen *screen)
{
  Paintset *paintset = fetchExisting(screen);
  return paintset->black();
}


Pixel PaintsetCollection::black (Widget widget)
{
  Paintset *paintset = fetchExistingByColormap (widget);
  return paintset->black ();
}


//------------------------------- white ----------------------------------//
//------------------------------- white ----------------------------------//
//------------------------------- white ----------------------------------//


Pixel PaintsetCollection::white (Display *display, int iscreen)
{
  Paintset *paintset = fetchExisting(display, iscreen);
  return paintset->white();
}


Pixel PaintsetCollection::white (Screen *screen)
{
  Paintset *paintset = fetchExisting(screen);
  return paintset->white();
}


Pixel PaintsetCollection::white (Widget widget)
{
  Paintset *paintset = fetchExistingByColormap (widget);
  return paintset->white ();
}


//------------------------------- clear ----------------------------------//
//------------------------------- clear ----------------------------------//
//------------------------------- clear ----------------------------------//


Pixel PaintsetCollection::clear (Display *display, int iscreen)
{
  Paintset *paintset = fetchExisting (display, iscreen);
  return paintset->clear ();
}


Pixel PaintsetCollection::clear (Screen *screen)
{
  Paintset *paintset = fetchExisting (screen);
  return paintset->clear ();
}


Pixel PaintsetCollection::clear (Widget widget)
{
  Paintset *paintset = fetchExistingByColormap (widget);
  return paintset->clear ();
}


//------------------------- add resources --------------------------------//
//------------------------- add resources --------------------------------//
//------------------------- add resources --------------------------------//


void PaintsetCollection::addResources (Arg *arglist, int *n, Display *display,
  int iscreen)
{
  Paintset *paintset = fetchExisting(display, iscreen);
  paintset->addResources(arglist, n);
}


void PaintsetCollection::addResources (Arg *arglist, int *n, Screen *screen)
{
  Paintset *paintset = fetchExisting(screen);
  paintset->addResources(arglist, n);
}


void PaintsetCollection::addResources (Arg *arglist, int *n, Widget widget)
{
  Paintset *paintset = fetchExistingByColormap (widget);
  paintset->addResources (arglist, n);
}


//---------------------------- visual depth ------------------------------//
//---------------------------- visual depth ------------------------------//
//---------------------------- visual depth ------------------------------//


int PaintsetCollection::visualDepth (Display *display, int iscreen)
{
  Paintset *paintset = fetchExisting(display, iscreen);
  return paintset->visualDepth();
}


int PaintsetCollection::visualDepth (Screen *screen)
{
  Paintset *paintset = fetchExisting(screen);
  return paintset->visualDepth();
}


int PaintsetCollection::visualDepth (Widget widget)
{
  Paintset *paintset = fetchExistingByColormap (widget);
  return paintset->visualDepth ();
}


//------------------------------- visual ----------------------------------//
//------------------------------- visual ----------------------------------//
//------------------------------- visual ----------------------------------//


Visual *PaintsetCollection::visual (Display *display, int iscreen)
{
  Paintset *paintset = fetchExisting(display, iscreen);
  return paintset->visual();
}


Visual *PaintsetCollection::visual (Screen *screen)
{
  Paintset *paintset = fetchExisting(screen);
  return paintset->visual();
}


Visual *PaintsetCollection::visual (Widget widget)
{
  Paintset *paintset = fetchExistingByColormap (widget);
  return paintset->visual ();
}


//------------------------------- colormap --------------------------------//
//------------------------------- colormap --------------------------------//
//------------------------------- colormap --------------------------------//


Colormap PaintsetCollection::colormap (Display *display, int iscreen)
{
  Paintset *paintset = fetchExisting(display, iscreen);
  return paintset->colormap();
}


Colormap PaintsetCollection::colormap (Screen *screen)
{
  Paintset *paintset = fetchExisting(screen);
  return paintset->colormap();
}


Colormap PaintsetCollection::colormap (Widget widget)
{
  Paintset *paintset = fetchExistingByColormap (widget);
  return paintset->colormap ();
}


int PaintsetCollection::numColorsLeft (Paintset *paintset, int nplanes)
{
  int k2, divider;
  for (k2 = 0, divider = 1; k2 < nplanes; k2++) {
    divider *= 2;
  }

  for (k2 = 0; k2 < NSETS; k2++) {
    if (paintset != PAINTSETS[k2]) continue;
    return NUM_COLORS_LEFT[k2]/divider;
  }
  assert (0);
}

void PaintsetCollection::setColorsetNamingPolicy (int p)
{
  _colorset_naming_policy = p;
}

int PaintsetCollection::colorsetNamingPolicy ()
{
  return _colorset_naming_policy;
}

//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//

