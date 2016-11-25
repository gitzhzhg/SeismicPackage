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
#include "sl/xvisual_collection.hh"
#include "sl/xvisual.hh"

#include <Xm/Xm.h>
#include <assert.h>

#define MAXSETS 10

static Xvisual *XVISUALS[MAXSETS];
static int NSETS = 0;

Xvisual *XvisualCollection::fetch (Display *display, int iscreen)
{
  assert (display);
  for (int iset = 0; iset < NSETS; iset++) {
    Xvisual *xvisual = XVISUALS[iset];
    if (xvisual->display() != display) continue;
    if (xvisual->iscreen() != iscreen) continue;
    return xvisual;
  }
  assert (NSETS < MAXSETS);
  Xvisual *xvisual = new Xvisual (display, iscreen);
  XVISUALS[NSETS] = xvisual;
  NSETS++;
  return xvisual;
}

Xvisual *XvisualCollection::fetch (Screen *screen)
{
  assert (screen);
  Display *display = XDisplayOfScreen (screen);
  int iscreen = XScreenNumberOfScreen (screen);

  return fetch (display, iscreen);
}

Xvisual *XvisualCollection::fetch (Widget widget)
{
  assert (widget);
  Screen *screen = XtScreen (widget);

  return fetch (screen);
}

Xvisual *XvisualCollection::fetchExisting (Display *display, int iscreen)
{
  assert(display);
  for(int iset = 0; iset < NSETS; iset++) {
    Xvisual *xvisual = XVISUALS[iset];
    if (xvisual->display() != display) continue;
    if (xvisual->iscreen() != iscreen) continue;
    return xvisual;
  }
  assert (FALSE);
  return NULL;
}

Xvisual *XvisualCollection::fetchExisting (Screen *screen)
{
  assert (screen);
  Display *display = XDisplayOfScreen(screen);
  int      iscreen = XScreenNumberOfScreen(screen);

  return fetchExisting (display, iscreen);
}


Xvisual *XvisualCollection::fetchExisting (Widget widget)
{
  assert (widget);
  Screen *screen = XtScreen(widget);

  return fetchExisting(screen);
}

void XvisualCollection::addResources (Arg *arglist, int *n,
  Display *display, int iscreen)
{
  Xvisual *xvisual = fetchExisting (display, iscreen);
  xvisual->addResources (arglist, n);
}


void XvisualCollection::addResources (Arg *arglist, int *n, Screen *screen)
{
  Xvisual *xvisual = fetchExisting (screen);
  xvisual->addResources (arglist, n);
}


void XvisualCollection::addResources (Arg *arglist, int *n, Widget widget)
{
  Xvisual *xvisual = fetchExisting (widget);
  xvisual->addResources (arglist, n);
}

Boolean XvisualCollection::matches (Xvisual *xvisual, Display *display,
  int iscreen)
{
  Xvisual *check_xvisual = fetchExisting (display, iscreen);
  return check_xvisual->matches (xvisual);
}

Boolean XvisualCollection::matches (Xvisual *xvisual, Screen *screen)
{
  Xvisual *check_xvisual = fetchExisting (screen);
  return check_xvisual->matches (xvisual);
}

Boolean XvisualCollection::matches (Xvisual *xvisual, Widget widget)
{
  Xvisual *check_xvisual = fetchExisting (widget);
  return check_xvisual->matches (xvisual);
}
