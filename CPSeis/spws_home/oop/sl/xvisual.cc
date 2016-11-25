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
#include "sl/xvisual.hh"

#include <Xm/Xm.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>

Xvisual::Xvisual (Display *display, int iscreen) : 
  _display  (display),
  _screen   (NULL),
  _visual   (NULL),
  _iscreen  (iscreen),
  _vdepth   (0)
{
  assert (_display);
  _screen = ScreenOfDisplay (_display, _iscreen);
  assert (_screen);

  _visual = DefaultVisual  (_display, _iscreen);

  privateGetVisualAttributes();
}

Xvisual::~Xvisual ()
{
}

void Xvisual::addResources (Arg *arglist, int *n)
{
  XtSetArg (arglist[*n], XmNvisual    , _visual     ); (*n)++;
  XtSetArg (arglist[*n], XmNdepth     , _vdepth     ); (*n)++;
  XtSetArg (arglist[*n], XmNscreen    , _screen     ); (*n)++;
}

Boolean Xvisual::matches (Xvisual *xvisual)
{
  assert (xvisual);
  return _visual == xvisual->_visual &&
         _screen == xvisual->_screen &&
         _vdepth == xvisual->_vdepth   ;
}

Display *Xvisual::display ()
{
  return _display;
}

int Xvisual::iscreen ()
{
  return _iscreen;
}

void Xvisual::privateGetVisualAttributes ()
{
  long        vmask  = VisualScreenMask;
  int         nitems = 0;
  XVisualInfo vtemplate;

  vtemplate.screen        = _iscreen;
  vtemplate.depth         = 0;
  vtemplate.c_class       = 0;
  vtemplate.red_mask      = 0;
  vtemplate.green_mask    = 0;
  vtemplate.blue_mask     = 0;
  vtemplate.colormap_size = 0;
  vtemplate.bits_per_rgb  = 0;

  XVisualInfo *list = XGetVisualInfo (_display, vmask, &vtemplate, &nitems);

  _vdepth        = 0;

  for (int item = 0; item < nitems; item++) {
      if (list[item].visual == _visual) {
	_vdepth = list[item].depth;
	break;
      }
  }
  XFree (list);

  assert (_vdepth > 0);
}
